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
conditions_layer <-   layer_dense(units = nb_env, activation = "identity", input_shape = nb_env)

# We put all that together into one input layer for a feedforward!
input_layers <- c(players_layers, list(conditions_layer))

nb_hidden <- 5 * (nb_variables + nb_env)

predictions <- layer_concatenate(input_layers, axis=-1) %>%
  layer_dense(units = nb_hidden, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_hidden, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_hidden, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = 'sigmoid')




