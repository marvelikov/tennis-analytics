####################
# Longterm Objective ------------------------------------------------------
####################

# We try to replicate the MLP model called TimeSeries Model of Somboonphokkaphan (2009) with our data.

###################
# Present Objective -------------------------------------------------------
###################

# Feed the tennis data to a MLP with the keras package.


library(keras)
library(data.table)

###########
# Import/prepare data -----------------------------------------------------
###########

data <- fread("Data/Cleaned/DataModeling.csv")

# save names somewhere
var_names <- colnames(data)

# Remove one year to avoid working with rows full of zeros
data <- data[tourney_date > "2011-01-01",]
data <- as.matrix(data[,-c(1,2,3,4)])
var_names <- var_names[-c(1,2,3,4)]
colnames(data) <- NULL

# train vs test (Randomly seperated! To be determined more carefully)
set.seed(667) # watch out not to put 666 in there!

test_pct <- .05
test_ind <- sample(nrow(data), nrow(data) * test_pct)


# Need the variable p1_wins to be defined in the data (in the appropriate script)
train_x <- data[-test_ind,1:15]
train_y <- data[-test_ind,16]
test_x <- data[test_ind,1:15]
test_y <- data[test_ind,16]
#rm(data)

# Normalize the columns we want to normalize...

#train_x[,1:12] <- scale(train_x[,1:12], center = TRUE)
#test_x[,1:12] <- scale(test_x[,1:12], center = TRUE)

# We leave the response in numeric...
#train_y <-
#test_y <- 



# Initialisation
model <- keras_model_sequential()

# One input layer of 1 input layers of 15 nodes, 1 hidden layer of 30 nodes, with dropout rate 0.4 and 1 output layer[10 neurons]
#i.e number of digits from 0 to 9

model %>%
  layer_dense(units = 15, input_shape = 15) %>%
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
model %>% fit(x = train_x, y = train_y, epochs = 50)

#Evaluating model on the cross validation dataset
loss_and_metrics <- model %>% evaluate(test_x, test_y)





