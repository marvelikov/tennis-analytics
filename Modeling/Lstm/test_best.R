best_model <- load_model_hdf5("Modeling/Lstm/checkpoint2.hdf5")

preds <- predict(model, list(data_p1[split,], data_p2[split,], data_match[split,]))
rbind(response_win[split[1],],
   preds[[1]][1,])
rbind(response_scores[split[1],],
       preds[[2]][1,])
rbind(response_stats[split[1],],
   preds[[3]][1,])
 
 
sub_model <- keras_model(inputs = list(input_p1,input_p2,input_match), outputs = list(output_win))
pred_win <- predict(sub_model, list(data_p1[split,], data_p2[split,], data_match[split,]))
mean(response_win[split,1] == round(pred_win[,1]))












input_match <- layer_input(shape=c(ncol(m_info)))
input <- layer_input(shape=c(n_timesteps*n_inputs))

shared <- input %>%
  layer_reshape(target_shape = c(n_timesteps, n_inputs)) %>%
  layer_masking(mask_value = 0) %>%
  layer_batch_normalization() %>%
  layer_dense(units = ceiling(n_inputs/4), activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = ceiling(n_inputs/4), activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = ceiling(n_inputs/4), activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_gru(units = ceiling(n_inputs/8), activation = "relu", return_sequences = TRUE) %>%
  layer_batch_normalization() %>%
  layer_dense(units = ceiling(n_inputs/10), activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = ceiling(n_inputs/10), activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = ceiling(n_inputs/10), activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_gru(units = ceiling(n_inputs/12), activation = "relu")

block_p1 <- shared
block_p2 <- shared

ff <- layer_concatenate(list(input_match, layer_subtract(list(block_p1,block_p2)))) %>%
  layer_batch_normalization() %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_dropout(.1) %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_dropout(.1) %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_batch_normalization()

output_stats <- ff %>%
  layer_dense(ceiling(n_inputs/10), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/10), activation = "relu") %>%
  layer_dense(n_outputs_stats, activation = "relu", name = "output_stats")

output_scores <- layer_concatenate(list(ff,output_stats)) %>%
  layer_dense(ceiling(n_inputs/15), activation = "relu") %>%
  layer_dense(n_outputs_scores, activation = "relu", name = "output_scores")

output_win <- layer_concatenate(list(output_stats,output_scores)) %>%
  layer_dense(ceiling(n_inputs/15), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/15), activation = "relu") %>%
  layer_dense(n_outputs_win, activation = "softmax", name = "output_win")

