## Import library and data
library(tidyverse)
library(keras)
library(readr)
library(ggplot2)
library(miscset)

penguins_size <- read_csv("./Penguins/penguins_size.csv")
data <- drop_na(penguins_size)
data <- data[data$sex != '.', ]
set.seed(1234)

# EDA
# boxplot for visualization of all continuous variables
ggplotGrid(ncol = 2,
           lapply(c("culmen_length_mm", "culmen_depth_mm",
                    "flipper_length_mm", "body_mass_g"),
                  function(col) {
                    ggplot(data, aes_string(x = "species", y = col,
                                            group = "species")) +
                      geom_boxplot()
                  }))

# maybe something for label vs. sex/island?

# One-hot encoding for island, sex, species
data <- data %>%
  mutate(island = model.matrix(~0 + island),
         species = model.matrix(~0 + species),
         sex = model.matrix(~0 + sex))

#data$sex[data$sex == "MALE"] <- 0
#data$sex[data$sex == "FEMALE"] <- 1
#data$sex <- as.numeric(data$sex)

#data$species[data$species == "Adelie"]<-1
#data$species[data$species == "Chinstrap"]<-2
#data$species[data$species == "Gentoo"]<-3

#data <- mutate_if(data, is.character, as.double)

# train and test set
sample <- round(0.8 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = sample)
train_data = data[train_ind, ]
test_data = data[-train_ind, ]

# normalization
label_col = "species"  
feature_cols = c("island", "culmen_length_mm", "culmen_depth_mm", "flipper_length_mm", "body_mass_g", "sex" ) 
## should we include sex in the species prediction??
## is normalisation of categorical data (one-hot encoding) reasonable?

train_means = apply(train_data[feature_cols], 2, mean)
train_sds = apply(train_data[feature_cols], 2, sd)

train_features = scale(train_data[feature_cols], 
                       center = train_means,
                       scale = train_sds)
test_features = scale(test_data[feature_cols], 
                      center = train_means,
                      scale = train_sds)
train_labels = train_data[label_col]
test_labels = test_data[label_col]

# Species prediction

# build model
penguin_model = keras_model_sequential() %>%
  layer_dense(units = 100, activation = 'relu', input_shape = c(9)) %>%
  layer_dense(units = 3, activation = 'softmax')
summary(penguin_model)

# compile model
penguin_model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(lr = 0.001),
  metrics = c('accuracy')
)

# history
history = penguin_model %>% 
  fit(as.matrix(train_features), 
      as.matrix(train_labels),
      epochs = 20,
      verbose = 0,
      validation_split = 0.2)
history
plot(history, metrics = "accuracy", smooth = F)
plot(history, metrics = "loss", smooth = F)

# evaluation on the test set
penguin_model  %>% 
  evaluate(as.matrix(test_features),
           as.matrix(test_labels), verbose = 0)
# accuracy = 0.985, loss = 0.014

# prediction on test set
results_test <- predict(penguin_model, test_features)

# comparison of prediction with real test labels
results_test %>% apply(., 1, function(u){which(u==max(u))}) %>%
  cbind(., test_labels$species %>%
          apply(., 1, function(u){which(u==max(u))})) %>%
  View()
# misclassification of 1 obs of species 1 to species 2
