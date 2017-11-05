# say nb_p is the number of variables for one player
# and nb_m is the number of variables related to the environment of a match


# One reccurent net per player.
names <- unique(names)

players_layers <- list()
for(i in seq_along(names)){
  players_layers[[i]] <- layer_gru(units = 2 * nb_p, input_shape = nb_p) %>%
    layer_gru(units = 2 * nb_p) %>%
    layer_gru(units = 2 * nb_p) %>%
    layer_gru(units = 2 * nb_p)
}

# An input layers for the conditions in which the match is played (surface, weather, etc...)
conditions_layer <-   layer_dense(units = nb_m, activation = "identity", input_shape = nb_m)

# We put all that together into one input layer for a feedforward
input_layers <- c(players_layers, list(conditions_layer))

nb_variables <- 5 * (nb_p + nb_m)

predictions <- layer_concatenate(input_layers, axis=-1) %>%
  layer_dense(units = nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 10 * nb_variables, activation = "tanh") %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1, activation = 'sigmoid')




