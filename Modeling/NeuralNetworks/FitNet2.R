
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

# Remove one year to avoid working with rows full of zeros
data <- data[tourney_date > "2011-01-01",]
data <- na.omit(data)

# Remove player names date and match num
data <- data[,-c(1,2,3,4)]

# save names somewhere
var_types <- sapply(data, class)[-ncol(data)]
data <- as.matrix(data)
var_names <- colnames(data[,-ncol(data)])


# record number of variables
nb_variables <- length(var_names)

# train vs test (Randomly seperated! To be determined more carefully)
set.seed(667) # watch out not to put 666 in there!

test_pct <- .05
test_ind <- sample(x = c(0,1), size = nrow(data), prob = c(1 - test_pct, test_pct), replace = TRUE)

# Need the variable p1_wins to be defined in the data (in the appropriate script)
train_x <- subset(data, subset = as.logical(1-test_ind), select = var_names)
train_y <- subset(data, subset = as.logical(1-test_ind), select = "p1_win")
test_x <- subset(data, subset = as.logical(test_ind), select = var_names)
test_y <- subset(data, subset = as.logical(test_ind), select = "p1_win")
#rm(data)

# Normalize the columns we want to normalize...

# watch out here. Might not always work when we add variables...
numeric_variables <- which(var_types == "numeric" & var_types != "integer")

train_x[,numeric_variables] <- scale(train_x[,numeric_variables], center = TRUE)
test_x[,numeric_variables] <- scale(test_x[,numeric_variables], center = TRUE)



# Initialisation
# One input layer of 1 input layers of 15 nodes, 1 hidden layer of 30 nodes, with dropout rate 0.4 and 1 output layer[10 neurons]
#i.e number of digits from 0 to 9

model <-  keras_model_sequential() %>%
  layer_dense(units = floor(nb_variables/2), input_shape = nb_variables) %>%
  layer_activation(activation = 'elu') %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 1) %>%
  layer_activation(activation = 'sigmoid')



# Compile
model %>% compile(
  loss = 'binary_crossentropy', # We have 0-1 classification...
  optimizer = 'adam', # To be investigated
  metrics = c('accuracy')  
)

# Fit

#batch_size <- 128 # Somewhat arbitrary 
model %>% fit(x = train_x, y = train_y, epochs = 30)

#Evaluating model on the cross validation dataset
model %>% evaluate(test_x, test_y)





