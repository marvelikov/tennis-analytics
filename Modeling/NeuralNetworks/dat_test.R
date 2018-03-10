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

source("Data/R/GameByGame/scripts/ConstructPartialData_temp.R")
p_info
p_stats
m_info
m_stats




f_dowle3 = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

f_dowle3(m_stats)
f_dowle3(p_stats)

ind_rem <- which(is.na(p_info$age) | is.na(p_info$ht) | is.na(p_info$rank) | is.na(p_info$rank_points))

ind_rem_p <- unique(c(ind_rem, ind_rem - (ind_rem %% 2 == 0) + (ind_rem %% 2 == 1)))
ind_rem_m <- unique((ind_rem + (ind_rem %% 2 == 1))/2)

m_info <- m_info[-ind_rem_m,]
m_stats <- m_stats[-ind_rem_m,]

p_info <- p_info[-ind_rem_p,]
p_stats <- p_stats[-ind_rem_p,]

m_info[, match_id := NULL]
p_info[, id := frank(id, ties.method = "dense")]


trails <- sapply(1:max(p_info$id), function(the_id){
  which(p_info$id == the_id)
}, simplify = FALSE)






n_timesteps <- 10
n_timesteps2 <- 100


# construct data

m_is <- cbind(as.matrix(m_info),as.matrix(m_stats))
p_is <- cbind(as.matrix(p_info[,-1]),as.matrix(p_stats))

n_inputs_lstm <- ncol(m_is) + 2*ncol(p_is)


data_mat <- t(sapply(1:nrow(p_info), function(i){

  if(i %% 200 == 0){
    print(i)
  }
  
  trail <- trails[[p_info$id[i]]]
  trail <- trail[which(trail > i)]

    
  if(length(trail)==0){
    rep(c(1,rep(0,n_inputs_lstm)),n_timesteps)
  }else if(length(trail)==1){
    trail <- trail[1:min(c(length(trail),n_timesteps))]
    len_pad <- n_timesteps - length(trail)
    
    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))

    c(0,m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,],rep(c(1,rep(0,n_inputs_lstm)), len_pad))
  }else{
    trail <- trail[1:min(c(length(trail),n_timesteps))]
    len_pad <- n_timesteps - length(trail)
    
    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))
    
    c(t(rbind(cbind(rep(0,length(trail)),m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,]),
              matrix(c(1,rep(0,n_inputs_lstm)), nrow = len_pad, ncol = n_inputs_lstm + 1, byrow = TRUE))))
  }
}))


dim(data_mat)

p_info[, id := NULL]


# construct different inputs flows

p1_win <-sample(0:1,nrow(m_info), replace = TRUE)

data_now  <- as.matrix(cbind(m_info,p_info[2*(1:nrow(m_info))-p1_win],p_info[2*(1:nrow(m_info))-1+p1_win]))
data_trail_p1 <- data_mat[2*(1:nrow(m_info)) - p1_win,]
data_trail_p2 <- data_mat[2*(1:nrow(m_info)) - 1 + p1_win,]

data_trail_past <- t(sapply(1:nrow(m_is), function(i){
  if(i %% 200 == 0){
    print(i)
  }
  
  # NEEEED begin i+1
  # Length problems with the else.
  
  if(nrow(m_is) - i >= n_timesteps2){
    c(t(cbind(m_is[i:(i+n_timesteps2),],p_is[2*(i:(i+n_timesteps2))-p1_win[i:(i+n_timesteps2)],],p_is[2*(i:(i+n_timesteps2))-1+p1_win[i:(i+n_timesteps2)],])))
  }else{
    c(t(cbind(m_is[i:nrow(m_is),],p_is[2*(i:nrow(m_is))-p1_win[i:nrow(m_is)],],p_is[2*(i:nrow(m_is))-1+p1_win[i:nrow(m_is)],])),rep(0,(nrow(m_is) - i)*n_inputs_lstm))
  }
}))





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
input_lstm3 <- layer_input(shape=list(n_timesteps2*n_inputs_lstm), name = "input_lstm3")

# construct the 2 lstms

output_lstm1 <- input_lstm1 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm+1)) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", name = "output_lstm1")

output_lstm2 <- input_lstm2 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm+1)) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", name = "output_lstm2")

output_lstm3 <- input_lstm3 %>% 
  layer_reshape(target_shape = c(n_timesteps2, n_inputs_lstm)) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", return_sequences = TRUE) %>%
  layer_lstm(units = n_inputs_lstm, activation = "tanh", name = "output_lstm3")


# concatenate them in a ff

output_now <- input_now %>%
  layer_dense(n_inputs_now, activation = "relu")

output_ff <- layer_concatenate(list(output_now,output_lstm1,output_lstm2,output_lstm3)) %>%
  layer_dense(n_inputs_now + 2*n_inputs_lstm, activation = "relu") %>%
  layer_dense(n_inputs_now + 2*n_inputs_lstm, activation = "relu") %>%
  layer_dense(2*n_inputs_lstm, activation = "relu") %>%
  layer_dense(2*n_inputs_lstm, activation = "relu") %>%
  layer_dense(n_inputs_lstm, activation = "relu") %>%
  layer_dense(ceiling(n_inputs_lstm/2), activation = "relu") %>%
  layer_dense(ceiling(n_inputs_lstm/4), activation = "relu", name = "output_ff") 
  
output_win <- output_ff %>%
  layer_dense(n_outputs_win, activation = "softmax", name = "output_win")

output_scores <- output_ff %>%
  layer_dense(n_outputs_scores, activation = "relu", name = "output_scores")

output_stats <- output_ff %>%
  layer_dense(n_outputs_stats, activation = "relu", name = "output_stats")





model <- keras_model(inputs = list(input_now,input_lstm1,input_lstm2,input_lstm3), outputs = list(output_win,output_scores,output_stats))
model %>% compile(
  loss = c("binary_crossentropy",'mse','mse'), # We have 0-1 classification...
  loss_weights = c(200,10,1),
  optimizer = 'adamax', # To be investigated
  #metrics = c("binary_accuracy")  
  metrics = list(output_win = "categorical_accuracy")  
)
predict(model, list(matrix(data_now[1,], nrow = 1),matrix(data_trail_p1[1,], nrow = 1),matrix(data_trail_p2[1,], nrow = 1)))
model %>% fit(x = list(data_now,data_trail_p1, data_trail_p2, data_trail_past), y = list(response_win,response_scores,response_stats), batch_size = 512, epochs = 3, validation_split = .05)


num <- 5
#predict(model, list(matrix(data_now[1,], nrow = 1),matrix(data_trail_p1[1,], nrow = 1),matrix(data_trail_p2[1,], nrow = 1)))
preds <- predict(model, list(data_now[1:num,],data_trail_p1[1:num,],data_trail_p2[1:num,]))

mean(round(preds[,1]) == p1_win[1:num])
