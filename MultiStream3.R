# Description: We work a bit on the data and define our network architecture.
source("Data/R/GameByGame/scripts/ConstructDataNN.R")
#source("Modeling/Lstm/Preamble.R")
data <- fread("Modeling/Lstm/DataPreamble")

# randomize winner/loser
p1_win <- sample(0:1,nrow(m_is), replace = TRUE)
#data4d <- abind( list(data3d[2*(1:nrow(m_is)) - p1_win,,], data3d[2*(1:nrow(m_info)) - 1 + p1_win,,]), along=4)
#data_p1 <- data3d[2*(1:nrow(m_is)) - p1_win,,]
#data_p2 <- data3d[2*(1:nrow(m_info)) - 1 + p1_win,,]
data_p1 <- data[2*(1:nrow(m_info)) - p1_win,]
data_p2 <- data[2*(1:nrow(m_info)) - 1 + p1_win,]
data_match <- as.matrix(m_info)


# construct responses
n_outputs_win <- 1
n_outputs_scores <- 2*5
n_outputs_scores <- 5
n_outputs_stats <- ncol(m_stats) + 2*(ncol(p_stats)-6)

#response_win <- cbind(p1_win,1-p1_win)
response_win <- as.matrix(p1_win)

response_scores <- as.matrix(cbind(p_stats[2*(1:nrow(m_info)) - p1_win, 13:17], p_stats[2*(1:nrow(m_info)) - 1 + p1_win, 13:17]))
response_scores <- response_scores/7

response_stats <- as.matrix(cbind(m_stats,p_stats[2*(1:nrow(m_info)) - p1_win, 2:12], p_stats[2*(1:nrow(m_info)) - 1 + p1_win, 2:12]))


input_p1 <- layer_input(shape=c(n_timesteps*n_inputs))
input_p2 <- layer_input(shape=c(n_timesteps*n_inputs))
#input_match <- layer_input(shape=c(ncol(m_info) + 2*ncol(p_info[,-1])))
input_match <- layer_input(shape=c(ncol(m_info)))



# Maybe should use create_layer() to wrap some of these?

reshape <- layer_reshape(target_shape = c(n_timesteps, n_inputs))
shared_ff1 <- layer_dense(units = ceiling(n_inputs), activation = "relu")
shared_ff2 <- layer_dense(units = ceiling(n_inputs/2), activation = "relu")
shared_ff3 <- layer_dense(units = ceiling(n_inputs/3), activation = "relu")
shared_ff4 <- layer_dense(units = ceiling(n_inputs/6), activation = "relu", input_shape = c(35))
shared_ff5 <- layer_dense(units = ceiling(n_inputs/8), activation = "relu", input_shape = c(35))
shared_ff6 <- layer_dense(units = ceiling(n_inputs/8), activation = "relu", input_shape = c(35))


gru1 <- layer_gru(units = ceiling(n_inputs/8), activation = "relu", return_sequences = TRUE)
#gru1 <- layer_gru(units = ceiling(n_inputs/8), activation = "relu")
gru2 <- layer_gru(units = ceiling(n_inputs/10), activation = "relu")


block1_p1 <- input_p1 %>%
  reshape %>% layer_masking(mask_value = 0) %>% layer_batch_normalization() %>%
  shared_ff1 %>% layer_dropout(.1) %>%  shared_ff2  %>% layer_dropout(.1) %>% shared_ff3 %>%
  layer_batch_normalization() %>% gru1

block1_p2 <- input_p2 %>%
  reshape %>% layer_masking(mask_value = 0) %>% layer_batch_normalization() %>%
  shared_ff1 %>% layer_dropout(.1) %>% shared_ff2  %>% layer_dropout(.1) %>% shared_ff3 %>%
  layer_batch_normalization() %>% gru1

block2_p1 <- block1_p1 %>%  layer_batch_normalization() %>%
  shared_ff4 %>% shared_ff5 %>% #shared_ff6 %>% 
  layer_batch_normalization() %>%
  gru2

block2_p2 <- block1_p2 %>%  layer_batch_normalization() %>%
  shared_ff4 %>% shared_ff5 %>% #shared_ff6 %>% 
  layer_batch_normalization() %>%
  gru2


#ff7 <- layer_concatenate(list(input_match, layer_subtract(list(block2_p1,block2_p2)))) %>%
ff7 <- layer_concatenate(list(input_match, layer_subtract(list(block2_p1,block2_p2)))) %>%
  layer_batch_normalization() %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_dropout(.1) %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_dropout(.1) %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_batch_normalization()

output_stats <- ff7 %>%
  layer_dense(ceiling(n_inputs/10), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/10), activation = "relu") %>%
  layer_dense(n_outputs_stats, activation = "relu", name = "output_stats")

output_scores <- layer_concatenate(list(ff7,output_stats)) %>%
  layer_dense(ceiling(n_inputs/15), activation = "relu") %>%
  layer_dense(n_outputs_scores, activation = "sigmoid", name = "output_scores")

output_win <- output_scores %>%
  layer_dense(ceiling(n_outputs_scores/2), activation = "relu") %>%
  #layer_dense(n_outputs_win, activation = "softmax", name = "output_win")
  layer_dense(n_outputs_win, activation = "sigmoid", name = "output_win")
  


model <- keras_model(inputs = list(input_p1,input_p2,input_match), outputs = list(output_win,output_scores,output_stats))

model %>% compile(
  loss = c("binary_crossentropy",'mae','mae'), # We have 0-1 classification...
  loss_weights = c(1,1,1),
  optimizer = 'adagrad' # To be investigated
  #metrics = c("binary_accuracy")  
  #metrics = "binary_accuracy"  
)



split <- sample(nrow(data_match),900)

#data_val <- list(data_p1[split,], data_p2[split,], data_match[split,])
#data_train <- list(data_p1[-split,], data_p2[-split,], data_match[-split,])
#data_train <- list(data_p1,data_p2,data_match)
#predict(model, list(data_p1[1:2,],data_p2[1:2,],data_match[1:2,]))

#response_val <- list(response_win[split,],response_scores[split,],response_stats[split,])
#response_train <- list(response_win[-split,],response_scores[-split,],response_stats[-split,])
#response_train <- list(response_win,response_scores,response_stats)


history <- model %>% fit(x = list(data_p1[-split,], data_p2[-split,], data_match[-split,]),
              y = list(response_win[-split,],response_scores[-split,],response_stats[-split,]),
              batch_size = 1024, epochs = 50,
              validation_data = list(list(data_p1[split,], data_p2[split,], data_match[split,]),
                                     list(response_win[split,],response_scores[split,],response_stats[split,])),
              view_metrics = FALSE,
              callbacks = callback_model_checkpoint("Modeling/Lstm/checkpoint2.hdf5", monitor = "val_output_win_loss", save_best_only = TRUE, verbose = 0, period = 1))





# preds <- predict(model, list(data_p1[1:2,],data_p2[1:2,],data_match[1:2,]))
# rbind(response_win[1,],
#   preds[[1]][1,])
# rbind(response_scores[1,],
#       preds[[2]][1,])
# rbind(response_stats[1,],
#   preds[[3]][1,])
# 
# 
# sub_model <- keras_model(inputs = list(input_p1,input_p2,input_match), outputs = list(output_win))
# pred_win <- predict(sub_model, data_val)
# mean(response_win[,1] == round(pred_win[,1]))
# 
# #cbind(response_win,response_scores,response_stats)[1,]
