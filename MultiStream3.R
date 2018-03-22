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


# We still have some NAs in there...
any(is.na(m_stats))
any(is.na(p_stats))
any(is.na(p_info))



## p_info
na_ind <- which(rowSums(is.na(p_info)) > 0)
p_info[na_ind]

# Let the ones with NA in rank have rank := max(rank) 
max_rank <- max(p_info$rank, na.rm = TRUE)
p_info[is.na(rank), rank := max_rank]
p_info[na_ind]
# Same idea with rank_points
p_info[is.na(rank_points), rank_points := 0]
p_info[na_ind]

na_ind <- which(rowSums(is.na(p_info)) > 0)
p_info[na_ind]
# Still many left, age
mean_age <- mean(p_info$age, na.rm = TRUE)
p_info[is.na(age), age := mean_age]

mean_age <- mean(p_info$age, na.rm = TRUE)

# Everything is in height actually...
colSums(is.na(p_info))
mean_ht <- round(mean(p_info$ht, na.rm = TRUE))
p_info[is.na(ht), ht := mean_ht]

na_ind <- which(rowSums(is.na(p_info)) > 0)
p_info[na_ind]




# For the rest, it is mostly games that were abandoned.

# Thanks to Matt Dowle https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
f_dowle3 = function(DT) {
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j,0)
}

f_dowle3(m_stats)
f_dowle3(p_stats)



m_info[, match_id := NULL]
p_info[, id := frank(id, ties.method = "dense")]


# Reverse everything now
m_info <- m_info[nrow(m_info):1,]
m_stats <- m_stats[nrow(m_stats):1,]
p_info <- p_info[nrow(p_info):1,]
p_stats <- p_stats[nrow(p_stats):1,]




#cols <- colnames(m_info)[c(4,5,7:11)]
#m_info[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]

#cols <- colnames(m_stats)
#m_stats[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]

#cols <- colnames(p_info)[c(2,11,81:83)]
#p_info[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]

##cols <- colnames(p_stats)[c(2:18)]
##p_stats[, (cols) := lapply(.SD, scale), .SDcols=cols]
#cols <- colnames(p_stats)[2:18]
#p_stats[, (cols) := lapply(.SD, function(x){(x - min(x))/(max(x) - min(x))}), .SDcols=cols]





trails <- sapply(1:max(p_info$id), function(the_id){
  which(p_info$id == the_id)
}, simplify = FALSE)





# number of timesteps for the two different kind of streams we have 

n_timesteps <- 3


# construct data
m_is <- cbind(as.matrix(m_info),as.matrix(m_stats))
p_is <- cbind(as.matrix(p_info[,-1]),as.matrix(p_stats))

n_inputs <- ncol(m_is) + 2*ncol(p_is)



# This constructs the appropriate data sequences
#mat_list <- lapply(1:nrow(p_info), function(i){
vec_list <- lapply(1:nrow(p_info), function(i){
if(i %% 2000 == 0){
    print(i)
  }
  
  trail <- trails[[p_info$id[i]]]
  trail <- trail[which(trail > i)]
  
  
  if(length(trail)==0){
    #matrix(0,n_timesteps,n_inputs)
    c(t(matrix(0,n_timesteps,n_inputs)))
  }else if(length(trail) < n_timesteps){
    trail <- rev(trail[1:min(c(length(trail),n_timesteps))])
    len_pad <- n_timesteps - length(trail)
    
    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))
    
    
    #rbind(matrix(0,len_pad,n_inputs),cbind(m_is[ceiling(trail/2),,drop = FALSE],p_is[trail,,drop = FALSE],p_is[trail2,,drop = FALSE]))
    c(t(rbind(matrix(0,len_pad,n_inputs),cbind(m_is[ceiling(trail/2),,drop = FALSE],p_is[trail,,drop = FALSE],p_is[trail2,,drop = FALSE]))))
  }else{
    trail <- rev(trail[1:min(c(length(trail),n_timesteps))])

    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))
    
    #cbind(m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,])
    c(t(cbind(m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,])))
  }
})

#library(abind)
#data3d <- abind(mat_list, along=3)
#dim(data3d)
#data3d <- aperm(data3d, c(3,1,2))
#dim(data3d)

data <- do.call(rbind,vec_list)

# randomize winner/loser
p1_win <- sample(0:1,nrow(m_is), replace = TRUE)
#data4d <- abind( list(data3d[2*(1:nrow(m_is)) - p1_win,,], data3d[2*(1:nrow(m_info)) - 1 + p1_win,,]), along=4)
#data_p1 <- data3d[2*(1:nrow(m_is)) - p1_win,,]
#data_p2 <- data3d[2*(1:nrow(m_info)) - 1 + p1_win,,]
data_p1 <- data[2*(1:nrow(m_is)) - p1_win,]
data_p2 <- data[2*(1:nrow(m_info)) - 1 + p1_win,]


# construct responses

n_outputs_win <- 2
n_outputs_scores <- 2*5
n_outputs_stats <- ncol(m_stats) + 2*(ncol(p_stats)-6)

response_win <- cbind(p1_win,1-p1_win)
response_scores <- as.matrix(cbind(p_stats[2*(1:nrow(m_info)) - p1_win, 14:18], p_stats[2*(1:nrow(m_info)) - 1 + p1_win, 14:18]))
response_stats <- as.matrix(cbind(m_stats,p_stats[2*(1:nrow(m_info)) - p1_win, 2:13], p_stats[2*(1:nrow(m_info)) - 1 + p1_win, 2:13]))


input_p1 <- layer_input(shape=c(n_timesteps*n_inputs))
input_p2 <- layer_input(shape=c(n_timesteps*n_inputs))



reshape <- layer_reshape(target_shape = c(n_timesteps, n_inputs))
shared_ff1 <- layer_dense(units = ceiling(n_inputs), activation = "relu", input_shape = c(n_timesteps*n_inputs))
shared_ff2 <- layer_dense(units = ceiling(n_inputs), activation = "relu")
shared_ff3 <- layer_dense(units = ceiling(n_inputs), activation = "relu")
shared_ff4 <- layer_dense(units = ceiling(n_inputs), activation = "relu")
shared_ff5 <- layer_dense(units = ceiling(n_inputs), activation = "relu")
shared_ff6 <- layer_dense(units = ceiling(n_inputs), activation = "relu")

#%>%
#  layer_dense(units = ceiling(n_inputs), activation = "relu") %>%
#  layer_dense(ceiling(n_inputs/2), activation = "relu") %>%
#  layer_dense(ceiling(n_inputs/3), activation = "relu") %>%
#  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
#  layer_batch_normalization()

gru1 <- layer_gru(units = ceiling(n_inputs/4), activation = "relu", return_sequences = TRUE)
gru2 <- layer_gru(units = ceiling(n_inputs/4), activation = "relu")

ff2 <- layer_input(shape=c(n_timesteps,n_inputs)) %>%
  layer_batch_normalization() %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu")
  
gru2 <- layer_input(shape=c(n_timesteps,n_inputs)) %>%
  layer_batch_normalization() %>%
  layer_gru(ceiling(n_inputs/4), activation = "relu", return_sequences = TRUE) %>%
  layer_gru(ceiling(n_inputs/4), activation = "relu", return_sequences = TRUE)



block1_p1 <- input_p1 %>% reshape %>% shared_ff1 %>% shared_ff2 %>% shared_ff3 %>% gru1
block1_p2 <- input_p2 %>% reshape %>% shared_ff1 %>% shared_ff2 %>% shared_ff3 %>% gru1

block2_p1 <- block1_p1 %>% shared_ff4 %>% shared_ff5 %>% shared_ff6 %>% gru2
block2_p2 <- block1_p2 %>% shared_ff4 %>% shared_ff5 %>% shared_ff6 %>% gru2


ff7 <- layer_subtract(list(block2_p1,block2_p2)) %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu")


output_win <- ff7 %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_dense(n_outputs_win, activation = "softmax", name = "output_win")

output_scores <- ff7 %>%
  layer_dense(ceiling(n_inputs/8), activation = "relu") %>%
  layer_dense(n_outputs_scores, activation = "relu", name = "output_scores")

output_stats <- ff7 %>%
  layer_dense(ceiling(n_inputs/4), activation = "relu") %>%
  layer_dense(n_outputs_stats, activation = "relu", name = "output_stats")


model <- keras_model(inputs = list(data_p1,data_p2), outputs = list(response_win,response_scores,response_stats))

model <- keras_model(inputs = list(data_p1,data_p2), outputs = output)
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
