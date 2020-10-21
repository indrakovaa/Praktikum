## Import library and data
library(tidyverse)
library(keras)
library(readr)
penguins_size <- read_csv("./Penguins/penguins_size.csv")
data <- drop_na(penguins_size)

#Recode island, sex, and species
levels(as.factor(data$species))
data$island[data$island == "Biscoe"] <- 1
data$island[data$island == "Dream"]<-2
data$island[data$island == "Torgersen"]<-3

data$sex[data$sex == "MALE"]<-1
data$sex[data$sex == "FEMALE"]<-2

data$species[data$species == "Adelie"]<-1
data$species[data$species == "Chinstrap"]<-2
data$species[data$species == "Gentoo"]<-3

data<-mutate_if(data, is.character, as.double)

# boxplot for visualization of variable culmen depth
data %>%
ggplot(aes(x=species, y=culmen_depth_mm, group=species))+
  geom_boxplot()

# train and test set
sample <- round(0.8 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = sample)
train_data = data[train_ind, ]
test_data = data[-train_ind, ]

# normalization
label_col = "species"  
feature_cols = c("island", "culmen_length_mm", "culmen_depth_mm", "flipper_length_mm", "body_mass_g", "sex" ) 
## should we include sex in the species prediction??

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

## TODO
# build model
penguin_model =
summary(penguin_model)
# compile model
# history
# plot predictions on the train set
# evaluation on the test set
