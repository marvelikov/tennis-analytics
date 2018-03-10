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

source("Data/Preprocess/ConstructPartialData_temp.R")
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





trails <- sapply(1:max(p_info$id), function(the_id){
  which(p_info$id == the_id)
}, simplify = FALSE)





# number of timesteps for the two different kind of streams we have 

n_timesteps <- 10
n_timesteps2 <- 100


# construct data
m_is <- cbind(as.matrix(m_info),as.matrix(m_stats))
p_is <- cbind(as.matrix(p_info[,-1]),as.matrix(p_stats))

n_inputs_lstm <- ncol(m_is) + 2*ncol(p_is)



# This constructs the appropriate data sequences
data_mat <- t(sapply(1:nrow(p_info), function(i){
  
  if(i %% 200 == 0){
    print(i)
  }
  
  trail <- trails[[p_info$id[i]]]
  trail <- trail[which(trail > i)]
  
  
  if(length(trail)==0){
    rep(c(0,rep(0,n_inputs_lstm)),n_timesteps)
  }else if(length(trail)==1){
    trail <- rev(trail[1:min(c(length(trail),n_timesteps))])
    len_pad <- n_timesteps - length(trail)
    
    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))
    
    c(rep(c(0,rep(0,n_inputs_lstm)), len_pad),1,m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,])
  }else{
    trail <- rev(trail[1:min(c(length(trail),n_timesteps))])
    len_pad <- n_timesteps - length(trail)
    
    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))
    
    c(matrix(c(0,rep(0,n_inputs_lstm)), nrow = len_pad, ncol = n_inputs_lstm + 1, byrow = TRUE),
      t(rbind(cbind(rep(1,length(trail)),m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,]))))
  }
}))


dim(data_mat)

# We don't need the id anymore
p_info[, id := NULL]

# randomize winner/loser
p1_win <- sample(0:1,nrow(m_info), replace = TRUE)


# the different streams
data_now  <- as.matrix(cbind(m_info,p_info[2*(1:nrow(m_info))-p1_win],p_info[2*(1:nrow(m_info))-1+p1_win]))
data_trail_p1 <- data_mat[2*(1:nrow(m_info)) - p1_win,]
data_trail_p2 <- data_mat[2*(1:nrow(m_info)) - 1 + p1_win,]


# experimental one
# data_trail_past <- t(sapply(1:nrow(m_is), function(i){
#   if(i %% 200 == 0){
#     print(i)
#   }
#   
#   # NEEEED begin i+1
#   # Length problems with the else.
#   
#   if(nrow(m_is) - i >= n_timesteps2){
#     c(t(cbind(m_is[i:(i+n_timesteps2),],p_is[2*(i:(i+n_timesteps2))-p1_win[i:(i+n_timesteps2)],],p_is[2*(i:(i+n_timesteps2))-1+p1_win[i:(i+n_timesteps2)],])))
#   }else{
#     c(t(cbind(m_is[i:nrow(m_is),],p_is[2*(i:nrow(m_is))-p1_win[i:nrow(m_is)],],p_is[2*(i:nrow(m_is))-1+p1_win[i:nrow(m_is)],])),rep(0,(nrow(m_is) - i)*n_inputs_lstm))
#   }
# }))





# construct responses

n_outputs_win <- 2
n_outputs_scores <- 2*5
n_outputs_stats <- ncol(m_stats) + 2*(ncol(p_stats)-6)

response_win <- cbind(p1_win,1-p1_win)
response_scores <- as.matrix(cbind(p_stats[2*(1:nrow(m_info)) - p1_win, 14:18], p_stats[2*(1:nrow(m_info)) - 1 + p1_win, 14:18]))
response_stats <- as.matrix(cbind(m_stats,p_stats[2*(1:nrow(m_info)) - p1_win, 2:13], p_stats[2*(1:nrow(m_info)) - 1 + p1_win, 2:13]))


# construct input layers

n_inputs_now <- ncol(m_info) + 2*ncol(p_info)

input_now <- layer_input(shape=list(n_inputs_now))
input_lstm1 <- layer_input(shape=list(n_timesteps*(n_inputs_lstm+1)), name = "input_lstm1")
input_lstm2 <- layer_input(shape=list(n_timesteps*(n_inputs_lstm+1)), name = "input_lstm2")
#input_lstm3 <- layer_input(shape=list(n_timesteps2*n_inputs_lstm), name = "input_lstm3")

# construct the 2 lstms

output_lstm1 <- input_lstm1 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm+1)) %>%
  #layer_gru(units = n_inputs_lstm/5, dropout = .05, return_sequences = TRUE) %>%
  #layer_gru(units = n_inputs_lstm/5, dropout = .05, return_sequences = TRUE) %>%
  layer_gru(units = 2*n_inputs_lstm, recurrent_dropout = .05, dropout = .05) %>%
  layer_dense(2*n_inputs_lstm, activation = "tanh") %>%
  layer_dense(2*n_inputs_lstm, activation = "tanh", name = "output_lstm1")
  

output_lstm2 <- input_lstm2 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm+1)) %>%
  #layer_gru(units = n_inputs_lstm/5, dropout = .05, return_sequences = TRUE) %>%
  #layer_gru(units = n_inputs_lstm/5, dropout = .05, return_sequences = TRUE) %>%
  layer_gru(units = 2*n_inputs_lstm, recurrent_dropout = .05, dropout = .05) %>%
  layer_dense(2*n_inputs_lstm, activation = "tanh") %>%
  layer_dense(2*n_inputs_lstm, activation = "tanh", name = "output_lstm2")

# output_lstm3 <- input_lstm3 %>% 
#   layer_reshape(target_shape = c(n_timesteps2, n_inputs_lstm)) %>%
#   layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
#   layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
#   layer_lstm(units = n_inputs_lstm, activation = "tanh", name = "output_lstm3")


# concatenate them in a ff

output_now <- input_now %>%
  layer_dense(n_inputs_now, activation = "sigmoid")

#output_ff <- layer_concatenate(list(output_now,output_lstm1,output_lstm2,output_lstm3)) %>%
output_ff <- layer_concatenate(list(output_now,output_lstm1,output_lstm2)) %>%
  layer_dense(n_inputs_now + 2*n_inputs_lstm/5, activation = "tanh") %>%
  layer_dense(2*n_inputs_lstm/5, activation = "tanh") %>%
  layer_dense(n_inputs_lstm/5, activation = "tanh") %>%
  layer_dense(n_inputs_lstm/5, activation = "tanh") %>%
  layer_dense(n_inputs_lstm/5, activation = "tanh") %>%
  layer_dense(n_inputs_lstm/5, activation = "tanh") %>%
  layer_dense(ceiling(n_inputs_lstm/7), activation = "sigmoid", name = "output_ff") 

output_win <- output_ff %>%
  layer_dense(n_outputs_win, activation = "softmax", name = "output_win")

output_scores <- output_ff %>%
  layer_dense(n_outputs_scores, activation = "sigmoid", name = "output_scores")

output_stats <- output_ff %>%
  layer_dense(n_outputs_stats, activation = "sigmoid", name = "output_stats")





#model <- keras_model(inputs = list(input_now,input_lstm1,input_lstm2,input_lstm3), outputs = list(output_win,output_scores,output_stats))
model <- keras_model(inputs = list(input_now,input_lstm1,input_lstm2), outputs = list(output_win,output_scores,output_stats))
model %>% compile(
  loss = c("categorical_crossentropy",'mse','mse'), # We have 0-1 classification...
  loss_weights = c(1,.01,.01),
  optimizer = 'sgd', # To be investigated
  #metrics = c("binary_accuracy")  
  metrics = "binary_accuracy"  
)
predict(model, list(matrix(data_now[1,], nrow = 1),matrix(data_trail_p1[1,], nrow = 1),matrix(data_trail_p2[1,], nrow = 1)))
#model %>% fit(x = list(data_now, data_trail_p1, data_trail_p2, data_trail_past), y = list(response_win,response_scores,response_stats), batch_size = 512, epochs = 3, validation_split = .05)

split <- sample(nrow(data_now),300)
data_val <- list(data_now[split,], data_trail_p1[split,], data_trail_p2[split,])
data_train <- list(data_now[-split,], data_trail_p1[-split,], data_trail_p2[-split,])

response_val <- list(response_win[split,],response_scores[split,],response_stats[split,])
response_train <- list(response_win[-split,],response_scores[-split,],response_stats[-split,])

model %>% fit(x = data_train, y = response_train, batch_size = 512, epochs = 20, validation_split = .05, view_metrics = FALSE)


preds <- predict(model, data_val)[[1]]
mean(round(preds[,1]) == response_val[[1]][,1])
