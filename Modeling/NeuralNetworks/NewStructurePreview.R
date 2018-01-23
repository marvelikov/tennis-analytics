# Description:  We investigate a new net structure. We begin by formating the data appropriately. Ultimately, all this should be done
#               in different files.

library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)
library(keras)



###############
# Net structure -----------------------------------------------------------
###############

# https://github.com/keras-team/keras/issues/7403
# https://keras.rstudio.com/reference/layer_gru.html
# https://keras.io/getting-started/functional-api-guide/
# http://colah.github.io/posts/2015-08-Understanding-LSTMs/


p1_rec_input <- layer_input(shape=list(NULL,60))
p2_rec_input <- layer_input(shape=list(NULL,60))
p1_info_input <- layer_input(shape=list(10))
p2_info_input <- layer_input(shape=list(10))
match_info_input <- layer_input(shape=list(10))
inputs <- list(p1_info_input,p1_rec_input,p2_info_input,p2_rec_input,match_info_input)


p1_rec_output <- p1_rec_input %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40)

p2_rec_output <- p2_rec_input %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40)


feed_input <- layer_concatenate(list(p1_info_input,p1_rec_output,p2_info_input,p2_rec_output,match_info_input))

feed_output <- feed_input %>%
  layer_dense(110) %>%
  layer_dense(55) %>%
  layer_dense(55) %>%
  layer_dense(55) %>%
  layer_dense(1)


model <- keras_model(inputs = inputs, outputs = feed_output)



# Prepare data ------------------------------------------------------------

# We begin by train only one iteration
# Load data in the environment
source("Data/Preprocess/ConstructPartialData.R")

# Define response variable
data_history[, p1_win := 1]

# Expand surface variable
data_history[, grass := 0]
data_history[surface == "Grass", grass := 1]
data_history[, clay := 0]
data_history[surface == "Clay", clay := 1]
data_history[, hard := 0]
data_history[surface == "Hard", hard := 1]
data_history[, surface := NULL]

# Expand date variable
data_history[, day := day(tourney_date)]
data_history[, month := month(tourney_date)]
data_history[, year := year(tourney_date)]
data_history[, tourney_date := NULL]

# Use more information via
#data_history[, tourney_latitude := latitude_function(tourney_name)]
#data_history[, tourney_longitude := longitude_function(tourney_name)]
#data_history[, tourney_altitude := altitude_function(tourney_name)]
#data_history[, tourney_name := embedding_function(tourney_name)]





data_history
# There still are some winner_ , loser_ , w_ , l_ in there... Now OK !!
# Also, many values are NA.


# We need to arrange the data in order to be coherent with the feed_input
info_variables <- c("right", "left", "ht", "age", "rank")
p1_info_variables <- paste0("p1_", info_variables)
p2_info_variables <- paste0("p2_", info_variables)


# need split in two: match_stats = c("minutes",...)   and   players_stats = c("ace",...) ... Done !
match_stats_variables <- c("minutes")
player_stats_variables <- c("ace", "df", "svpt", "1stIn", "1stWon", "2ndWon", "SvGms", "bpSaved", "bpFaced", "rpt_won", "score_set_1", "score_set_2", "score_set_3", "score_set_4", "score_set_5")

# Define the variables for p1 last game (at this moment we only miss the opp rep)
p1_match_stats <- paste0("p1_", match_stats_variables) 
p1_stats_variables <- paste0("p1_", player_stats_variables) 
p1_opp_stats_variables <- paste0("p1_opp_", player_stats_variables)
p1_opp_info_variables <- paste0("p1_opp_", info_variables)

# Define the variables for p1 last game (at this moment we only miss the opp rep)
p2_match_stats <- paste0("p2_", match_stats_variables) 
p2_stats_variables <- paste0("p2_", player_stats_variables) 
p2_opp_stats_variables <- paste0("p2_opp_", player_stats_variables)
p2_opp_info_variables <- paste0("p2_opp_", info_variables)

p1_rec_variables <- c(p1_match_stats, p1_stats_variables, p1_opp_stats_variables, p1_opp_info_variables)
p2_rec_variables <- c(p2_match_stats, p2_stats_variables, p2_opp_stats_variables, p2_opp_info_variables)

match_info_variables <- c("grass", "hard", "clay", "year", "month", "day", "draw_size")


  
# Train the model --------------------------------------------------------

p1_rec_input <- layer_input(shape=list(NULL,length(p1_rec_variables)))
p2_rec_input <- layer_input(shape=list(NULL,length(p2_rec_variables)))
p1_info_input <- layer_input(shape=list(length(p1_info_variables)))
p2_info_input <- layer_input(shape=list(length(p2_info_variables)))
match_info_input <- layer_input(shape=list(length(match_info_variables)))
inputs <- list(p1_info_input,p1_rec_input,p2_info_input,p2_rec_input,match_info_input)


p1_rec_output <- p1_rec_input %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_state = TRUE)

p2_rec_output <- p2_rec_input %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_sequences = TRUE) %>%
  layer_gru(units = 40, return_state = TRUE)





# Fake opponents representations
p1_opp_rec_input <- layer_input(shape = list(NULL,length(p1_rec_variables)))
p1_opp_rec_output <- p1_opp_rec_input %>%  layer_gru(units = 40, return_state = TRUE)
p1_opp_rep_state <- get_weights(p1_opp_rec_output[[2]])

p1_opp_rec_output[[2]]

# Let us focus on one line only!
train_x <- data_history[1, (c(p1_info_variables, p1_rec_variables, p2_info_variables, p2_rec_variables, match_info_variables)), with = FALSE]
train_y <- data_history[1, p1_win]

#opp_rec_model <- keras_model(inputs = p1_opp_rec_input, outputs = p1_opp_rec_output[[1]])
#p1_opp_rep <- c(p1_opp_info_input,get_wewights(opp_rec_model))








feed_input <- layer_concatenate(list(p1_info_input,p1_rec_output,p2_info_input,p2_rec_output,match_info_input))

feed_output <- feed_input %>%
  layer_dense(100) %>%
  layer_dense(75) %>%
  layer_dense(50) %>%
  layer_dense(50) %>%
  layer_dense(1)


model <- keras_model(inputs = inputs, outputs = feed_output)

data_history

test <- model %>% fit()
