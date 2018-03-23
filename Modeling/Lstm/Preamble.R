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



# First remove all abandoned matches
ind <- which(p_stats$retired == 1)

p_stats <- p_stats[-c(ind,ind-1),]
p_info <- p_info[-c(ind, ind-1),]
m_stats <- m_stats[-ind/2,]
m_info <- m_info[-ind/2,]

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



any(is.na(m_stats))
any(is.na(p_stats))
any(is.na(p_info))


colSums(is.na(p_stats))
colSums(is.na(p_info))

na_ind <- which(is.na(p_stats$ace))
p_stats <- p_stats[-na_ind,]
p_info <- p_info[-na_ind,]
m_stats <- m_stats[-unique(ceiling(na_ind/2)),]
m_info <- m_info[-unique(ceiling(na_ind/2)),]





any(is.na(m_stats))
any(is.na(p_stats))
any(is.na(p_info))
any(is.na(p_stats))

colSums(is.na(m_stats))
mean_minutes <- round(mean(m_stats$minutes, na.rm = TRUE))
m_stats[which(is.na(minutes)), minutes := mean_minutes]

p_stats[, retired := NULL]

# Thanks to Matt Dowle https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
# f_dowle3 = function(DT) {
#   for (j in seq_len(ncol(DT)))
#     set(DT,which(is.na(DT[[j]])),j,0)
# }
# 
# f_dowle3(m_stats)
# f_dowle3(p_stats)



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

n_timesteps <- 20


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

#fwrite(data, "Modeling/Lstm/DataPreamble")
