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



# cols <- colnames(m_info)[c(4,5,7:11)]
# m_info[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]
# 
# cols <- colnames(m_stats)
# m_stats[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]
# 
# cols <- colnames(p_info)[c(2,11,81:83)]
# p_info[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]

#cols <- colnames(p_stats)[c(2:18)]
#p_stats[, (cols) := lapply(.SD, scale), .SDcols=cols]
#cols <- colnames(p_stats)[2:18]
#p_stats[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]




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

n_timesteps <- 15



# construct data
n_inputs_lstm <- ncol(trails_info[[1]]) + ncol(trails_stats[[1]])


shifted_trails <- sapply(1:length(trails_stats), function(i){
  cbind(rbind(rep(0,ncol(trails_info[[i]])),trails_info[[i]]),rbind(trails_stats[[i]],rep(0,ncol(trails_stats[[i]]))))
})



# This constructs the appropriate data sequences
my_env <- environment()
response_mat <- matrix(nrow = nrow(p_info), ncol = ncol(m_s_mat)+2*ncol(p_s_mat))



data_mat <- t(sapply(1:nrow(p_i_mat), function(i){
  
  if(i %% 200 == 0){
    print(i)
  }
  
  the_id <- p_i_mat[i,1]
  
  trail <- row_id_trails[[the_id]]
  the_rows <- which(trail >= i)
  
  trail2 <- unlist(sapply(trail, function(j){
    j+1-2*(j %% 2 == 0)
  }))
  
  
  my_env$response_mat[i,] <- shifted_trails[[the_id]][the_rows[1],-(1:ncol(trails_info[[1]]))]
  
  mat <- matrix(-1.0, nrow = n_timesteps, ncol = ncol(shifted_trails[[1]]))
  
  mat[n_timesteps:(n_timesteps-min(length(the_rows),n_timesteps)+1),] <- shifted_trails[[the_id]][the_rows[1:min(length(the_rows),n_timesteps)]+1,]
  c(t(mat))
}))


dim(data_mat)

# construct responses

n_outputs_win <- 2
n_outputs_scores <- 2*5
n_outputs_stats <- ncol(m_s_mat) + 2*(ncol(p_s_mat)-6)

response_win <- response_mat[,c(3,21)]
response_scores <- response_mat[,c(14:18,32:36)]
response_stats <- response_mat[,-c(3,21,16:20,34:38)]


# construct input layers

input1 <- layer_input(shape=list((ncol(m_i_mat) + 2*ncol(p_i_mat) - 2)))
input2 <- layer_input(shape=list(n_timesteps*n_inputs_lstm))



# construct the lstms

d_rate <- .15

ff1 <- input2 %>% 
  layer_reshape(target_shape = c(n_timesteps, n_inputs_lstm)) %>%
  layer_masking(mask_value = -1.0) %>%
  layer_dense(ceiling(n_inputs_lstm/15), activation = "relu") %>%
  layer_dropout(rate = d_rate*2) %>%
  layer_dense(ceiling(n_inputs_lstm/15), activation = "relu") %>%
  layer_dropout(rate = d_rate) %>%
  layer_dense(ceiling(n_inputs_lstm/15), activation = "relu") %>%
  layer_batch_normalization()

gru1 <- ff1 %>%
  layer_gru(units = ceiling(n_inputs_lstm/15), activation = "relu", return_sequences = TRUE) %>%
  layer_batch_normalization()

ff2 <- gru1 %>%
  layer_dense(ceiling(n_inputs_lstm/20), activation = "relu") %>%
  layer_dropout(rate = d_rate/2) %>%
  layer_dense(ceiling(n_inputs_lstm/20), activation = "relu") %>%
  layer_dropout(rate = d_rate/2) %>%
  layer_dense(ceiling(n_inputs_lstm/20), activation = "relu") %>%
  layer_dropout(rate = d_rate/2) %>%
  layer_dense(ceiling(n_inputs_lstm/20), activation = "relu") %>%
  layer_batch_normalization()

gru2 <- layer_concatenate(list(ff1,ff2)) %>%
  layer_gru(units = ceiling(n_inputs_lstm/15), activation = "relu", dropout = d_rate/2, return_sequences = TRUE) %>%
  layer_batch_normalization()

ff3 <- layer_concatenate(list(gru1,gru2))  %>% 
  layer_dense(ceiling(n_inputs_lstm/15), activation = "relu") %>%
  layer_dropout(rate = d_rate/4) %>%
  layer_dense(ceiling(n_inputs_lstm/15), activation = "relu") %>%
  layer_dropout(rate = d_rate/4) %>%
  layer_dense(ceiling(n_inputs_lstm/20), activation = "relu") %>%
  layer_dropout(rate = d_rate/4) %>%
  layer_dense(ceiling(n_inputs_lstm/20), activation = "relu") %>%
  layer_batch_normalization()

gru3 <- layer_concatenate(list(ff2,ff3))  %>% 
  layer_gru(units = ceiling(n_inputs_lstm/15), activation = "relu", dropout = d_rate/4) %>%
  layer_batch_normalization()

ff4 <- layer_concatenate(list(input1,gru3)) %>%
  layer_dense(ceiling(n_inputs_lstm/5), activation = "relu") %>%
  layer_dropout(rate = d_rate/4) %>%
  layer_dense(ceiling(n_inputs_lstm/5), activation = "relu") %>%
  layer_dropout(rate = d_rate/4) %>%
  layer_dense(ceiling(n_inputs_lstm/5), activation = "relu") %>%
  layer_dropout(rate = d_rate/4) %>%
  layer_dense(ceiling(n_inputs_lstm/5), activation = "relu") %>%
  layer_batch_normalization()


output_stats <- layer_concatenate(list(ff3,ff4)) %>%
  layer_dense(ceiling(n_inputs_lstm/10), activation = "sigmoid") %>%
  layer_dense(ceiling(n_inputs_lstm/10), activation = "sigmoid") %>%
  layer_dense(n_outputs_stats, activation = "sigmoid") %>%
  layer_dense(n_outputs_stats, activation = "relu", name = "output_stats")


output_scores <- output_stats %>%
  layer_dense(n_outputs_stats, activation = "sigmoid") %>%
  layer_dense(n_outputs_scores, activation = "sigmoid") %>%
  layer_dense(n_outputs_scores, activation = "relu", name = "output_scores")


output_win <- layer_concatenate(list(ff4,output_stats,output_scores)) %>%
  layer_dense(ceiling(n_inputs_lstm/10), activation = "sigmoid") %>%
  layer_dense(n_outputs_scores, activation = "sigmoid") %>%
  layer_dense(n_outputs_win, activation = "softmax", name = "output_win")







#model <- keras_model(inputs = list(input_now,input_lstm1,input_lstm2,input_lstm3), outputs = list(output_win,output_scores,output_stats))
model <- keras_model(inputs = list(input1,input2), outputs = list(output_stats,output_scores,output_win))
model %>% compile(
  loss = c('mse','mse',"binary_crossentropy"), # We have 0-1 classification...
  loss_weights = c(1,1,1),
  optimizer = 'adam', # To be investigated
  metrics = c("binary_accuracy")  
  #metrics = c("mse") 
  #metrics = c("mse","binary_accuracy") 
)

#split <- sample(nrow(data_mat),300)
#split <- numeric(0)
#data_val <- data_mat[split,]
data1_train <- data_mat[,ncol(data_mat) - (n_inputs_lstm-1):0][,1:(ncol(m_i_mat) + 2*ncol(p_i_mat) - 2)]
data2_train <- data_mat

predict(model, list(data1_train[1:2,],data2_train[1:2,]))

#response_val <- list(response_win[split,],response_scores[split,],response_stats[split,])
#response_train <- list(response_win[-split,],response_scores[-split,],response_stats[-split,])
response_train <- list(response_stats,response_scores,response_win)

#set_weights(model, weights)

model %>% fit(x = list(data1_train,data2_train), y = response_train, batch_size = 256, epochs = 200, validation_split = .05, view_metrics = FALSE)

weights <- get_weights(model)

#preds <- predict(model, data_val)[[1]]
#mean(round(preds[,1]) == response_val[[1]][,1])



