# Description: We work a bit on the data and define our network architecture.

library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)
library(keras)



###############
# Net structure -----------------------------------------------------------
###############

# Helpful sources

# https://github.com/keras-team/keras/issues/7403
# https://keras.rstudio.com/reference/layer_gru.html
# https://keras.io/getting-started/functional-api-guide/
# http://colah.github.io/posts/2015-08-Understanding-LSTMs/



# Our data blocks are

source("Data/R/GameByGame/scripts/ConstructDataNN.R")
p_info
p_stats
m_info
m_stats





na_ind <- which(rowSums(is.na(p_info)) > 0)
p_info[na_ind]

# We still have some NAs in there...
any(is.na(m_stats))
any(is.na(p_stats))
any(is.na(p_info))

# Thanks to Matt Dowle https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
f_dowle3 = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

f_dowle3(m_stats)
f_dowle3(p_stats)


# A little more complex for p_info, we need to remove some lines (and their corresponding elsewhere).

ind_rem <- which(is.na(p_info$age) | is.na(p_info$ht) | is.na(p_info$rank) | is.na(p_info$rank_points))

ind_rem_p <- unique(c(ind_rem, ind_rem - (ind_rem %% 2 == 0) + (ind_rem %% 2 == 1)))
ind_rem_m <- unique((ind_rem + (ind_rem %% 2 == 1))/2)

m_info <- m_info[-ind_rem_m,]
m_stats <- m_stats[-ind_rem_m,]

p_info <- p_info[-ind_rem_p,]
p_stats <- p_stats[-ind_rem_p,]

m_info[, match_id := NULL]
p_info[, id := frank(id, ties.method = "dense")]


# Reverse everything now
m_info <- m_info[nrow(m_info):1,]
m_stats <- m_stats[nrow(m_stats):1,]
p_info <- p_info[nrow(p_info):1,]
p_stats <- p_stats[nrow(p_stats):1,]



cols <- colnames(m_info)[c(4,5,7:11)]
m_info[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]

cols <- colnames(m_stats)
m_stats[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]

cols <- colnames(p_info)[c(2,11,81:83)]
p_info[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]

#cols <- colnames(p_stats)[c(2:18)]
#p_stats[, (cols) := lapply(.SD, scale), .SDcols=cols]
cols <- colnames(p_stats)[2:18]
p_stats[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]




m_i_mat <- as.matrix(m_info)
p_i_mat <- as.matrix(p_info)
m_s_mat <- as.matrix(m_stats)
p_s_mat <- as.matrix(p_stats)



row_id_trails <- sapply(1:max(p_info$id), function(the_id){
  which(p_info$id == the_id)
}, simplify = FALSE)

trails_info <- sapply(1:max(p_info$id), function(the_id){
  row_id_trail <- row_id_trails[[the_id]]
  
  row_id_trail2 <- unlist(sapply(row_id_trail, function(j){
    j+1-2*(j %% 2 == 0)
  }))
  
  t(sapply(1:length(row_id_trail), function(i){
    c(m_i_mat[ceiling(row_id_trail[i]/2),],p_i_mat[row_id_trail[i],-1],p_i_mat[row_id_trail2[i],-1])
  }))
}, simplify = FALSE)

trails_stats <- sapply(1:max(p_info$id), function(the_id){
  row_id_trail <- row_id_trails[[the_id]]
  
  row_id_trail2 <- unlist(sapply(row_id_trail, function(j){
    j+1-2*(j %% 2 == 0)
  }))
  
  t(sapply(1:length(row_id_trail), function(i){
    c(m_s_mat[ceiling(row_id_trail[i]/2),],p_s_mat[row_id_trail[i],],p_s_mat[row_id_trail2[i],])
  }))
}, simplify = FALSE)



# number of timesteps for the two different kind of streams we have 

n_timesteps <- 10



# construct data
n_inputs_lstm <- ncol(trails_info[[1]]) + ncol(trails_stats[[1]])


shifted_trails <- sapply(1:length(trails_stats), function(i){
  cbind(rbind(rep(0,ncol(trails_info[[i]])),trails_info[[i]]),rbind(trails_stats[[i]],rep(0,ncol(trails_stats[[i]]))))
})



# This constructs the appropriate data sequences
my_env <- environment()
response_mat <- matrix(nrow = nrow(m_i_mat), ncol = ncol(m_s_mat)+2*ncol(p_s_mat))
safety_mat <- matrix(nrow = nrow(m_i_mat), ncol = ncol(m_s_mat)+2*ncol(p_s_mat))

p1_win <- sample(0:1, nrow(m_i_mat), replace = TRUE)



data_mat1 <- t(sapply(2* 1:nrow(m_i_mat), function(i){
  
  if(i %% 200 == 0){
    print(i)
  }
  
  the_id <- p_i_mat[i - p1_win[ceiling(i/2)],1]
  
  trail <- row_id_trails[[the_id]]
  the_rows <- which(trail >= i - p1_win[ceiling(i/2)])
  
  trail2 <- unlist(sapply(trail, function(j){
    j+1-2*(j %% 2 == 0)
  }))
  
  
  my_env$response_mat[ceiling(i/2),] <- shifted_trails[[the_id]][the_rows[1],-(1:ncol(trails_info[[1]]))]
  
  mat <- matrix(-1.0, nrow = n_timesteps, ncol = ncol(shifted_trails[[1]]))
  
  mat[n_timesteps:(n_timesteps-min(length(the_rows),n_timesteps)+1),] <- shifted_trails[[the_id]][the_rows[1:min(length(the_rows),n_timesteps)]+1,]

  my_env$safety_mat[ceiling(i/2),] <- mat[n_timesteps,-(1:ncol(trails_info[[1]]))]
  
  c(t(mat))
}))

rbind(safety_mat[10,-(1:2)][1:18],response_mat[10,-(1:2)][19:36])


data_mat2 <- t(sapply(2* 1:nrow(m_i_mat), function(i){
  
  if(i %% 200 == 0){
    print(i)
  }
  
  the_id <- p_i_mat[i + p1_win[ceiling(i/2)] - 1,1]
  
  trail <- row_id_trails[[the_id]]
  the_rows <-which(trail >= i + p1_win[ceiling(i/2)] - 1)
  
  trail2 <- unlist(sapply(trail, function(j){
    j+1-2*(j %% 2 == 0)
  }))
  
  mat <- matrix(-1.0, nrow = n_timesteps, ncol = ncol(shifted_trails[[1]]))
  
  mat[n_timesteps:(n_timesteps-min(length(the_rows),n_timesteps)+1),] <- shifted_trails[[the_id]][the_rows[1:min(length(the_rows),n_timesteps)]+1,]

  my_env$safety_mat[ceiling(i/2),] <- mat[n_timesteps,-(1:ncol(trails_info[[1]]))]

  c(t(mat))
}))

rbind(safety_mat[10,-(1:2)][1:18],response_mat[10,-(1:2)][19:36])
rbind(safety_mat[10,-(1:2)][1:18],response_mat[10,-(1:2)][1:18])

dim(data_mat1)

# construct responses

n_outputs_win <- 2
n_outputs_scores <- 2*5
n_outputs_stats <- ncol(m_s_mat) + 2*(ncol(p_s_mat)-6)

response_win <- response_mat[,c(3,21)]
response_scores <- response_mat[,c(14:18,32:36)]
response_stats <- response_mat[,-c(3,21,16:20,34:38)]


# construct input layers

input_lstm1 <- layer_input(shape=list(n_timesteps*n_inputs_lstm), name = "input_lstm1")
input_lstm2 <- layer_input(shape=list(n_timesteps*n_inputs_lstm), name = "input_lstm2")


# construct the lstms

output_lstm1 <- input_lstm1 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm)) %>%
  layer_masking(mask_value = -1.0) %>%
  #layer_gru(units = 2*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE) %>%
  #layer_gru(units = 3*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE, recurrent_regularizer = regularizer_l1_l2(l1 = .2)) %>%
  #layer_gru(units = 3*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE, recurrent_regularizer = regularizer_l1_l2(l1 = .2)) %>%
  #layer_gru(units = 5*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE, go_backwards = TRUE) %>%
  #layer_gru(units = n_inputs_lstm, dropout = .1, return_sequences = TRUE) %>%
  #layer_gru(units = ceiling(n_inputs_lstm/2), dropout = .1, return_sequences = TRUE) %>%
  layer_gru(units = ceiling(n_inputs_lstm/2), dropout = .2, name = "output_lstm1")

output_lstm2 <- input_lstm2 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm)) %>%
  layer_masking(mask_value = -1.0) %>%
  #layer_gru(units = 2*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE) %>%
  #layer_gru(units = 3*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE, recurrent_regularizer = regularizer_l1_l2(l1 = .2)) %>%
  #layer_gru(units = 3*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE, recurrent_regularizer = regularizer_l1_l2(l1 = .2)) %>%
  #layer_gru(units = 5*n_inputs_lstm, dropout = .4, recurrent_dropout =  .1, return_sequences = TRUE, go_backwards = TRUE) %>%
  #layer_gru(units = n_inputs_lstm, dropout = .1, return_sequences = TRUE) %>%
  #layer_gru(units = ceiling(n_inputs_lstm/2), dropout = .1, return_sequences = TRUE) %>%
  layer_gru(units = ceiling(n_inputs_lstm/2), dropout = .2, name = "output_lstm2")

output_ff <- layer_concatenate(list(output_lstm1,output_lstm2))  %>% 
  layer_dense(ceiling(2*n_inputs_lstm), activation = "tanh") %>%
  layer_dropout(rate = .1) %>%
  layer_dense(ceiling(n_inputs_lstm/2), activation = "tanh", name = "output_ff") 

output_win <- output_ff %>%
  #layer_dense(n_outputs_win, activation = "sigmoid") %>%
  layer_dense(n_outputs_win, activation = "softmax", name = "output_win")

output_scores <- output_ff %>%
  #layer_dense(n_outputs_scores, activation = "sigmoid") %>%
  layer_dropout(rate = .05) %>%
  layer_dense(n_outputs_scores, activation = "sigmoid", name = "output_scores")

output_stats <- output_ff %>%
  #layer_dense(n_outputs_stats, activation = "sigmoid") %>%
  layer_dropout(rate = .05) %>%
  layer_dense(n_outputs_stats, activation = "sigmoid") %>%
  layer_dense(n_outputs_stats, activation = "sigmoid", name = "output_stats")





#model <- keras_model(inputs = list(input_now,input_lstm1,input_lstm2,input_lstm3), outputs = list(output_win,output_scores,output_stats))
model <- keras_model(inputs = list(input_lstm1,input_lstm2), outputs = list(output_win,output_scores,output_stats))
model %>% compile(
  loss = c("categorical_crossentropy",'mse','mse'), # We have 0-1 classification...
  loss_weights = c(10,1,1),
  optimizer = 'adam', # To be investigated
  #metrics = c("binary_accuracy")  
  metrics = "binary_accuracy"  
)
predict(model, list(data_mat1[1:2,],data_mat2[1:2,]))
#model %>% fit(x = list(data_now, data_trail_p1, data_trail_p2, data_trail_past), y = list(response_win,response_scores,response_stats), batch_size = 512, epochs = 3, validation_split = .05)

split <- sample(nrow(data_mat1),300)
data_val <- list(data_mat1[split,],data_mat2[split,])
data_train <- list(data_mat1[-split,],data_mat2[-split,])

response_val <- list(response_win[split,],response_scores[split,],response_stats[split,])
response_train <- list(response_win[-split,],response_scores[-split,],response_stats[-split,])

model %>% fit(x = data_train, y = response_train, batch_size = 256, epochs = 10, validation_split = .05, view_metrics = FALSE)


preds <- predict(model, data_val)[[1]]
mean(round(preds[,1]) == response_val[[1]][,1])
