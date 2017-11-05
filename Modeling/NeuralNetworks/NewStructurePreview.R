# say nb_variables is the number of variables for one player


# One reccurent net per player.
names <- unique(names)

players_layers <- list()
for(i in seq_along(names)){
  players_layers[[i]] <- layer_gru(units = 2 * nb_variables, input_shape = nb_variables) %>%
    layer_gru(units = 2 * nb_variables) %>%
    layer_gru(units = 2 * nb_variables) %>%
    layer_gru(units = 2 * nb_variables)
}

# An input layers for the conditions in which the match is played (surface, weather, etc...)
conditions_layer <-   layer_dense(units = 10 * nb_variables, activation = "tanh", input_shape = nb_variables) %>%
  layer_dropout(rate = 0.4)

# We put all that together into one input layer for a feedforward!
input_layers <- c(players_layers, list(conditions_layer))

predictions <- layer_concatenate((input_layers, recursive = FALSE), axis=-1) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = 'sigmoid')




