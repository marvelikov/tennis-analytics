# The model
#source("Modeling/NeuralNetworks/Structure2.R")

data_input <- readRDS("Modeling/NeuralNetworks/datss2")
who <- readRDS("Modeling/NeuralNetworks/whoss2")
layer_names <- c("p1_l1","p1_l2","p1_l3","p1_l4","p1_l5","p2_l1","p2_l2","p2_l3","p2_l4","p2_l5")

model %>% compile(
  #loss = 'mean_squared_error', # We have 0-1 classification...
  loss = 'binary_crossentropy', # We have 0-1 classification...
  optimizer = 'adamax', # To be investigated
  metrics = c("binary_accuracy")  
)


model %>% fit(x = data_input, y = as.numeric(who == 1), batch_size = 128, epochs = 5, validation_split = .05)
model %>% fit(x = data_input, y = as.numeric(who == 1), batch_size = 256, epochs = 5, validation_split = .05)
model %>% fit(x = data_input, y = as.numeric(who == 1), batch_size = 1024, epochs = 15, validation_split = .05)

#sapply(1:5, function(j){
#  set_weights(model$get_layer(name = layer_names[5+j]), weights = get_weights(model$get_layer(name = layer_names[j])))
#})




