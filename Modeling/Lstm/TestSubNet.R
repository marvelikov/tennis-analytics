output_lstm1 <- input_lstm1 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm)) %>%
  layer_masking(mask_value = -1) %>%
  layer_gru(units = 2*n_inputs_lstm, dropout = .4, recurrent_dropout =  .05,  return_sequences = TRUE, go_backwards = TRUE) %>%
  layer_gru(units = n_inputs_lstm, dropout = .4, recurrent_dropout =  .05, return_sequences = TRUE, go_backwards = TRUE) %>%
  layer_gru(units = ceiling(n_inputs_lstm/2), dropout = .1, recurrent_dropout =  .05, return_sequences = TRUE, go_backwards = TRUE) %>%
  #layer_gru(units = n_inputs_lstm, dropout = .1, return_sequences = TRUE) %>%
  #layer_gru(units = ceiling(n_inputs_lstm/2), dropout = .1, return_sequences = TRUE) %>%
  layer_gru(units = ceiling(n_inputs_lstm/2), dropout = .1, name = "output_lstm", go_backwards = TRUE)



model_lstm1 <- keras_model(input_lstm1, output_lstm1)
preds <- predict(model_lstm1, matrix(data_val[[1]][1,], nrow = 1))

preds[,6,] == 


predict(model_lstm, matrix(data_val[1,], nrow = 1))[,1,]
predict(model, data_val[1:5,])
