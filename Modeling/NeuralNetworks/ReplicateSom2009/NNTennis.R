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

data <- fread()

# train vs test (Randomly seperated! To be determined more carefully)
set.seed(666)

test.pct <- .05
test.ind <- sample(nrow(data), nrow(data) * .05)

# Need the variable p1_wins to be defined in the data (in the appropriate script)
train_x <- data[-test.ind, -p1_wins]
train_y <- data[-test.ind, p1_wins]
test_x <- data[test.ind, -p1_wins]
test_y <- data[test.ind, p1_wins]
rm(data)

# Normalizing the columns ####****#### does that work with data.frames?
train_x <- scale(train_x)
test_x <- scale(test_x)

# We leave the response in numeric...
#train_y <-
#test_y <- 


### RENDU LA ####
# Notre model n'a le meme type de response (num vs cat) donc attention pour la suite


#defining a keras sequential model
model <- keras_model_sequential()

#defining the model with 1 input layer[784 neurons], 1 hidden layer[784 neurons] with dropout rate 0.4 and 1 output layer[10 neurons]
#i.e number of digits from 0 to 9

model %>%
  layer_dense(units = 784, input_shape = 784) %>%
  layer_dropout(rate=0.4)%>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units = 10) %>%
  layer_activation(activation = 'softmax')

#compiling the defined model with metric = accuracy and optimiser as adam.
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

#fitting the model on the training dataset
model %>% fit(train_x, train_y, epochs = 100, batch_size = 128)

#Evaluating model on the cross validation dataset
loss_and_metrics <- model %>% evaluate(test_x, test_y, batch_size = 128)



get_layer(object = model, name = "dense_2")


