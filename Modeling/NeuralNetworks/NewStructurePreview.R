# say nb_variables is the number of variables for one player


# One reccurent net per player.
names <- unique(names)

input_layers <- list()
for(i in seq_along(names)){
  input_layers[[i]] <- layer_gru(units = 2 * nb_variables, input_shape = nb_variables) %>%
    layer_gru(units = 2 * nb_variables) %>%
    layer_gru(units = 2 * nb_variables) %>%
    layer_gru(units = 2 * nb_variables)
}

predictions <- layer_concatenate(unlist(input_layers, recursive = FALSE), axis=-1) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = 'sigmoid')




