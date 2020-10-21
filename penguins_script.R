library(tidyverse)
library(keras)
library(readr)
penguins_size <- read_csv("./Penguins/penguins_size.csv")
data <- drop_na(penguins_size)

#recode Island and Sex
levels(as.factor(data$island))
data$island[data$island == "Biscoe"] <- 1
data$island[data$island == "Dream"]<-2
data$island[data$island == "Torgersen"]<-3

data$island[data$island == "MALE"]<-1
data$island[data$island == "FEMALE"]<-2

# boxplot for visualization of variable culmen depth
data %>%
  ggplot(aes(x=island, y=culmen_depth_mm, group=island))+
  geom_boxplot()

# train and test set
sample <- round(0.8 * nrow(data))
train_ind <- sample(seq_len(nrow(data)), size = sample)
train_data = data[train_ind, ]
test_data = data[-train_ind, ]

