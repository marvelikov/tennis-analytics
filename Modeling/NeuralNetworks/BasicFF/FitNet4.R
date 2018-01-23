
###############
# Load packages -----------------------------------------------------------
###############

library(keras)
library(data.table)
library(tidyverse)

#####################
# Import/prepare data -----------------------------------------------------
#####################


data <- fread("Data/Cleaned/DataModeling.csv")
# data <- fread("https://raw.githubusercontent.com/samperochkin/tennis-analytics/2c4d9ca2430287ab5305e329aa5422328135a2a3/Data/Cleaned/DataModeling.csv")

# Select the response variable and remove others
data[, reponse_variable1 := p1_win * (1 - p2_retired)]
data[, reponse_variable2 := (1 - p1_win) * (1 - p1_retired)]
data[, reponse_variable3 := p1_retired]
data[, reponse_variable4 := p2_retired]
data <- data[, -c("p1_win", "p1_perc_game_win", "p1_retired", "p2_retired"), with = FALSE]

n_response <- 4

# Remove one year to avoid working with rows full of zeros
data <- data[tourney_date > "2011-01-01",]
data <- na.omit(data)

# Remove player names date and match num
data <- data[,-c(1,2,3,4)]


# save names somewhere
var_types <- sapply(data, class)[-ncol(data)]
data <- as.matrix(data)
var_names <- colnames(data[,-(ncol(data) - 0:(n_response - 1))])



# record number of variables
nb_variables <- length(var_names)

# train vs test (Randomly seperated! To be determined more carefully)
set.seed(667) # watch out not to put 666 in there!

test_pct <- .1
test_ind <- sample(x = c(0,1), size = nrow(data), prob = c(1 - test_pct, test_pct), replace = TRUE)

# Need the variable p1_wins to be defined in the data (in the appropriate script)
train_x <- subset(data, subset = as.logical(1-test_ind), select = var_names)
train_y <- subset(data, subset = as.logical(1-test_ind), select = c("reponse_variable1","reponse_variable2","reponse_variable3","reponse_variable4"))
test_x <- subset(data, subset = as.logical(test_ind), select = var_names)
test_y <- subset(data, subset = as.logical(test_ind), select = c("reponse_variable1","reponse_variable2","reponse_variable3","reponse_variable4"))
#rm(data)

# Normalize the columns we want to normalize...

# watch out here. Might not always work when we add variables...
not_to_scale <- c("grass", "clay", "hard", unlist(sapply(c("injury", "game_played_30_uncertainty", "game_played_365_uncertainty",
                                                           "game_played_clay_uncertainty", "game_played_hard_uncertainty",
                                                           "game_played_grass_uncertainty", "top_4_GS"),
                function(nn){
                  paste0(c("p1_", "p2_"), nn)
}, simplify = FALSE, USE.NAMES = FALSE)))
                  

to_scale <- setdiff(var_names, not_to_scale)

train_x[,to_scale] <- scale(train_x[,to_scale], center = TRUE)
test_x[,to_scale] <- scale(test_x[,to_scale], center = TRUE)



# Initialisation
# One input layer of 1 input layers of 15 nodes, 1 hidden layer of 30 nodes, with dropout rate 0.4 and 1 output layer[10 neurons]
#i.e number of digits from 0 to 9

model <-  keras_model_sequential() %>%
  layer_dense(units = 3 * nb_variables, input_shape = nb_variables) %>%
  layer_activation(activation = 'tanh') %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 3 * nb_variables) %>%
  layer_activation(activation = 'tanh') %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 3 * nb_variables) %>%
  layer_activation(activation = 'tanh') %>%
  layer_dropout(rate=0.4) %>%
  layer_dense(units = 4) %>%
  layer_activation(activation = 'sigmoid')


# Compile with custom metric
K <- backend()
met <- function(y_true, y_pred){
  K$mean(1 - K$abs(K$round(y_pred) - K$round(y_true)))
}

model %>% compile(
  loss = 'mean_squared_error', # We have 0-1 classification...
  optimizer = 'sgd', # To be investigated
  metrics = c("categorical_accuracy")  
)

# Fit


#batch_size <- 128 # Somewhat arbitrary 
train_y[,3:4] <- train_y[,3:4]*15

model %>% fit(x = train_x, y = train_y, batch_size = 1024 , epochs = 100, validation_data = list(test_x, test_y))
model %>% fit(x = train_x, y = train_y, batch_size = 128 , epochs = 25, validation_data = list(test_x, test_y))


pred <- keras::predict_on_batch(model, test_x)
1 - sum(abs(round(pred[,1] + pred[,4]) - (test_y[,1] + test_y[,4] > 0)))/nrow(test_y)
hist(c(pred[,3:4]), probability = TRUE)
points(pred[,3:4][test_y[,3:4] == 1],rep(0,length(pred[,3:4][test_y[,3:4] == 1])), pch = 19)
sapply(sort(pred[,3:4][test_y[,3:4] == 1]), function(a){mean(pred[,3:4] > a)})
1/length(pred[,3:4][test_y[,3:4] == 1])

plot(sort(c(pred[,3:4])), (1:length(c(pred[,3:4])))/length(c(pred[,3:4])), type = "l")
lines(sort(pred[,3:4][test_y[,3:4] == 1]), (1:length(pred[,3:4][test_y[,3:4] == 1]))/length(pred[,3:4][test_y[,3:4] == 1]))


pred <- keras::predict_on_batch(model, train_x)
1 - sum(abs(round(pred[,1] + pred[,4]) - (train_y[,1] + train_y[,4])))/nrow(train_y)
hist(c(pred[,3:4]), probability = TRUE)
points(pred[,3:4][train_y[,3:4] == 2],rep(0,length(pred[,3:4][train_y[,3:4] == 2])), pch = 19)
sapply(sort(pred[,3:4][train_y[,3:4] == 10]), function(a){mean(pred[,3:4] > a)})

