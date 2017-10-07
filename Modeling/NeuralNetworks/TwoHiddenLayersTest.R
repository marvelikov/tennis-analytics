
###############
# Load packages -----------------------------------------------------------
###############

library(keras)
library(data.table)

#####################
# Import/prepare data -----------------------------------------------------
#####################

data <- fread("Data/Cleaned/DataModeling.csv")
# data <- fread("https://raw.githubusercontent.com/samperochkin/tennis-analytics/2c4d9ca2430287ab5305e329aa5422328135a2a3/Data/Cleaned/DataModeling.csv")
# Remove one year to avoid working with rows full of zeros
data <- data[tourney_date > "2011-01-01",]

# Remove player names date and match num
data <- data[,-c(1,2,3,4)]
data <- na.omit(data)

# save names somewhere
var_names <- colnames(data)
var_names <- var_names[-length(var_names)]
var_types <- sapply(data,class)
var_types <- var_types[-length(var_types)]


# record number of variables
nb_variables <- length(var_names)

# train vs test (Randomly seperated! To be determined more carefully)
set.seed(667) # watch out not to put 666 in there!

test_pct <- .05
test_ind <- rep(0,nrow(data))
test_ind[sample(nrow(data), nrow(data) * test_pct)] <- 1

data <- as.matrix(data)

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
  layer_dense(units = 2 * nb_variables, input_shape = nb_variables) %>%
  layer_dropout(rate=0.4) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units = 2 * nb_variables) %>%
  layer_dropout(rate=0.4) %>%
  layer_activation(activation = 'relu') %>%
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
model %>% fit(x = train_x, y = train_y, epochs = 15)

#Evaluating model on the cross validation dataset
loss_and_metrics <- model %>% evaluate(test_x, test_y)





