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

# Our data blocks are

source("Data/Preprocess/ConstructPartialData_temp.R")
p_info
p_stats
m_info
m_stats

# Do not consider the variables id for p_info, and match_id for m_info
p_info_width <- ncol(p_info) - 1
p_stats_width <- ncol(p_stats)
m_info_width <- ncol(m_info) - 1
m_stats_width <- ncol(m_stats)

# Determine the width of the recurrent nets' output
rec_output_width <- 30

# That means the total width of the representation is
p_rep_width <- p_info_width+rec_output_width

# So that the input width of the rec nets is
rec_width <- m_info_width+m_stats_width+2*p_stats_width+p_rep_width

# And the feedfoward net's width is
feed_width <- 2*p_rep_width + m_info_width

p1_rec_input <- layer_input(shape=list(rec_width))
p2_rec_input <- layer_input(shape=list(rec_width))
p1_info_input <- layer_input(shape=list(p_info_width))
p2_info_input <- layer_input(shape=list(p_info_width))
match_info_input <- layer_input(shape=list(m_info_width))
inputs <- list(p1_info_input,p1_rec_input,p2_info_input,p2_rec_input,match_info_input)


p1_rec_output <- p1_rec_input %>%
  layer_dense(units = 80) %>%
  layer_dense(units = 60) %>%
  layer_dense(units = 40) %>%
  layer_dense(units = 30) %>%
  layer_reshape(target_shape = c(1,30)) %>%
  layer_simple_rnn(units = 20)

  #layer_embedding(input_dim = 30, output_dim = 20) %>%
  #layer_gru(units = 20)

p2_rec_output <- p2_rec_input %>%
  layer_dense(units = 80) %>%
  layer_dense(units = 60) %>%
  layer_dense(units = 40) %>%
  layer_dense(units = 30) %>%
  layer_reshape(target_shape = c(1,30)) %>%
  layer_gru(units = 20)

#layer_embedding(input_dim = 30, output_dim = 20) %>%
#layer_gru(units = 20)


feed_input <- layer_concatenate(list(p1_info_input,p1_rec_output,p2_info_input,p2_rec_output,match_info_input))

feed_output <- feed_input %>%
  layer_dense(feed_width) %>%
  layer_dense(feed_width) %>%
  layer_dense(floor(feed_width/2)) %>%
  layer_dense(floor(feed_width/2)) %>%
  layer_dense(floor(feed_width/4)) %>%
  layer_dense(floor(feed_width/4)) %>%
  layer_dense(floor(feed_width/8)) %>%
  layer_dense(1)


model <- keras_model(inputs = inputs, outputs = feed_output)



