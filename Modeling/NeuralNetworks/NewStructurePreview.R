# Description:  We investigate a new net structure. We begin by formating the data appropriately. Ultimately, all this should be done
#               in different files.




################
# Data structure ----------------------------------------------------------
################
############################
## Data from JackSackmann ##
############################


# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)

# Import raw data ---------------------------------------------------------

data_raw <- fread("Data/Raw/DataRawGameByGame.csv")
data_transformed <- data_raw[-grep(pattern = "Davis", x = data_raw$tourney_name),]
data_transformed <- data_transformed[-grep(pattern = "Olympics", x = data_transformed$tourney_name),]
data_transformed$tourney_date <- ymd(data_transformed$tourney_date)


# Get info of players, stats of players and match info

info_p1 <- data_transformed %>% select(starts_with("winner"))
info_p2 <- data_transformed %>% select(starts_with("loser"))

stats_p1 <- data_transformed %>% select(starts_with("w_"))
stats_p2 <- data_transformed %>% select(starts_with("l_"))


#data_match <- data_transformed %>% select(-c(starts_with("winner"),starts_with("loser"),starts_with("w_"),starts_with("l_")))
#data_match

info_match <- data_transformed %>% select(c(starts_with("tourney"), surface, draw_size, best_of, round))
stats_match <- data_transformed %>% select(minutes, score)



# Important notes:

# 1) minutes -- is in match_info, but could be duplicated in both stats_p1/p2.

# 2) info_p1/p2 -- were meant to contain fixed info known "before the game": watch out
#                   because they are not all "fixed". age, rank, entry, seed are all evolving.

# 3) In fact, for the two players, we need to create a vector with
#       c(stats_player,info_player,stats_opponent,info_opponent,stats_match,info_match)
# that we feed to the net in both the payers individual nets, along with
#       c(info_new_match)

# 4) We need to adapt some of this to identify the winner.

# ..... To be re-considered. But the questions are there....





###############
# Net structure -----------------------------------------------------------
###############

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




