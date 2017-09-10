####################
# Longterm Objective ------------------------------------------------------
####################

# We try to replicate the MLP model called TimeSeries Model of Somboonphokkaphan (2009) with our data.

###################
# Present Objective -------------------------------------------------------
###################

# Feed the tennis data to a MLP with the keras package.


library(keras)

###########
# Import/prepare data -----------------------------------------------------
###########

data <- fread("Data/Cleaned/DataModeling.csv")

# train vs test (Randomly seperated! To be determined more carefully)
set.seed(666)

test_pct <- .05
test_ind <- sample(nrow(data), nrow(data) * .05)

# Need the variable p1_wins to be defined in the data (in the appropriate script)
train_x <- data[-test_ind, -p1_wins]
train_y <- data[-test_ind, p1_wins]
test_x <- data[test_ind, -p1_wins]
test_y <- data[test_ind, p1_wins]
rm(data)

# Normalize the columns we want to normalize...

train_x <- scale(train_x, center = TRUE)
test_x <- scale(test_x, center = TRUE)

# We leave the response in numeric...
#train_y <-
#test_y <- 


### RENDU LA ####
# Notre model n'a le meme type de response (num vs cat) donc attention pour la suite


# Initialisation
model <- keras_model_sequential()

# One input layer of 1 input layers of 15 nodes, 1 hidden layer of 30 nodes, with dropout rate 0.4 and 1 output layer[10 neurons]
#i.e number of digits from 0 to 9

model %>%
  layer_dense(units = 30, input_shape = 15) %>%
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

batch_size <- nrow(x_train)

model %>% fit(train_x, train_y, epochs = 100, batch_size = batch_size)

#Evaluating model on the cross validation dataset
loss_and_metrics <- model %>% evaluate(test_x, test_y, batch_size = 128)



get_layer(object = model, name = "dense_2")


