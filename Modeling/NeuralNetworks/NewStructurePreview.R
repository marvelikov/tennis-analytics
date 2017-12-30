# Description:  We investigate a new net structure. This is a toy net with only two players.

# Future tasks are written down at the end.

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




##############
# Future tasks ------------------------------------------------------------
##############

# 1. Properly define rec_input and output so that it makes sense:

#     length(rec_input) = length(stats_input) + 
#                            length(opp_stats_input) + 
#                            length(opp_info_input) + 
#                            length(rec_output) + 
#                            length(match_input)



# 2. Maybe add some layer_dropout in the dense net? Or try to keep it smaller to run on CPU? 
