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


m_info[, match_id := NULL]
p_info[, id := frank(id, ties.method = "dense")]

trails <- sapply(1:max(p_info$id), function(the_id){
  which(p_info$id == the_id)
}, simplify = FALSE)


n_timesteps <- 10

p1_win <-sample(0:1,nrow(m_info), replace = TRUE)

m_is <- cbind(as.matrix(m_info),as.matrix(m_stats))
p_is <- cbind(as.matrix(p_info),as.matrix(p_stats))

n_inputs <- ncol(m_is) + 2*ncol(p_is)


data_mat <- t(sapply(1:nrow(p_info), function(i){

  if(i %% 200 == 0){
    print(i)
  }
  
  trail <- trails[[p_info$id[i]]]
  trail <- trail[which(trail > i)]

    
  if(length(trail)==0){
    rep(c(1,rep(0,n_inputs)),n_timesteps)
  }else if(length(trail)==1){
    trail <- trail[1:min(c(length(trail),n_timesteps))]
    len_pad <- n_timesteps - length(trail)
    
    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))

    c(0,m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,],rep(c(1,rep(0,n_inputs)), len_pad))
  }else{
    trail <- trail[1:min(c(length(trail),n_timesteps))]
    len_pad <- n_timesteps - length(trail)
    
    trail2 <- unlist(sapply(trail, function(j){
      j+1-2*(j %% 2 == 0)
    }))
    
    c(t(rbind(cbind(rep(0,length(trail)),m_is[ceiling(trail/2),],p_is[trail,],p_is[trail2,]),
              matrix(c(1,rep(0,n_inputs)), nrow = len_pad, ncol = n_inputs + 1, byrow = TRUE))))
  }
}))



data_now  <- cbind(m_info,p_info[2*(1:nrow(m_info))-p1_win],p_info[2*(1:nrow(m_info))-1+p1_win])
data_trail_p1 <- 
data_trail_p2

data_response <- cbind(m_stats,p_stats[2*(1:nrow(m_stats))-p1_win],p_stats[2*(1:nrow(m_info))-1+p1_win])

data_mat <- sapply(1:nrow(match_info), function(i){
  c(match)
})


# Do not consider the variables id for p_info, and match_id for m_info
p_info_width <- ncol(p_info) - 1
p_stats_width <- ncol(p_stats) - 1
m_info_width <- ncol(m_info) - 1
m_stats_width <- ncol(m_stats)
