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

# We need to arrange the data in order to be coherent with the feed_input
info_variables <- c("right", "left", "ht", "age", "rank")
p1_info_variables <- paste0("p1_", info_variables)
p2_info_variables <- paste0("p2_", info_variables)

stats_variables <- c("minutes", "ace", "df", "svpt", "1stIn", "1stWon", "2ndWon", "SvGms", "bpSaved", "bpFaced", "rpt_won", "score_set_1", "score_set_2", "score_set_3", "score_set_4", "score_set_5")
p1_rec_variables <- c(paste0("p1_", stats_variables), paste0("p1_opp_", stats_variables), paste0("p1_opp_", info_variables))
p2_rec_variables <- c(paste0("p2_", stats_variables), paste0("p2_opp_", stats_variables), paste0("p2_opp_", info_variables))

match_info_variables <- c("grass", "hard", "clay", "year", "month", "day", "draw_size")

train_x <- data_history[, (c(p1_info_variables, p1_rec_variables, p2_info_variables, p2_rec_variables, match_info_variables)), with = FALSE]
train_y <- 

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


test <- model %>% fit()
