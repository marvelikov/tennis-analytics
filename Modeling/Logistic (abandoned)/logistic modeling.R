# Say we want to predict a given match... Let us do it with an example

# 0. Load packages --------------------------------------------------------

require(readr)
require(lubridate)
require(data.table)
require(readr)
require(readxl)
require(corrplot)
require(caret)

# 1. Import data ----------------------------------------------------------

source("Modeling/Logistic/DataPreparation.R")


# 2. Look at correlations -------------------------------------------------

# Faudrait revoir si ces transformations sont tous appropriées (surtout lorsque j'enlève les NAs ... faudrait peut etre plus fouillé sur pourquoi les valeurs sont vides)
test_modeling <- data_modeling[, -c(1:2)]
test_modeling$p1_score <- as.numeric(test_modeling$p1_score)
test_modeling$p2_score <- as.numeric(test_modeling$p2_score)
test_modeling <- test_modeling[-which(is.na(test_modeling$set_no)),]
test_modeling <- test_modeling[-which(test_modeling$set_no == 0),]
test_modeling <- test_modeling[-which(test_modeling$winner == 0),] # Faudrait comprendre pourquoi ce point n'a pas été gagné par aucun joueur ... pour l'instant, je l'exclue
test_modeling[which(is.na(p1_bp_perc)),] <- 0
test_modeling[which(is.na(p2_bp_perc)),] <- 0
test_modeling <- test_modeling[-which(is.na(p1_cum_ace)),]


str(test_modeling)
summary(test_modeling)

M <- cor(test_modeling[, -c("winner")])
corrplot(M)


# 3. Split the data -------------------------------------------------------

# Set the random number seed to reproduce the resuts
set.seed(1)

training_rows <- createDataPartition(test_modeling$winner,
                                    p = 0.8,
                                    list = FALSE)

train_predictors <- test_modeling[, -c("winner")][training_rows,]
train_responses <-  test_modeling[, c("winner")][training_rows,]

test_predictors <- test_modeling[, -c("winner")][-training_rows,]
test_responses <-  test_modeling[, c("winner")][-training_rows,]





