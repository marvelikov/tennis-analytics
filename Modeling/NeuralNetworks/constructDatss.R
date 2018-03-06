# The model
source("Modeling/NeuralNetworks/Structure.R")


layer_names <- c("GRU1_p1", "GRU2_p1", "GRU3_p1", "GRU1_p2", "GRU2_p2", "GRU3_p2")


# data
#m_info
#m_stats
#p_info
#p_stats

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


N <- length(unique(p_info$id))

r1 <- sapply(layer_names[1:3], function(nn){
  get_weights(model$get_layer(name = nn))
})

weight_list <- replicate(N, r1)
rm("r1")


last_game_mat <- matrix(nrow = N, ncol = rec_width)



  who <- sample.int(2,nrow(m_info), replace = TRUE)
  saveRDS(who, "Modeling/NeuralNetworks/whoss")
  
  p1_ind <- 2*(1:nrow(m_info)) - (who == 1)
  p2_ind <- 2*(1:nrow(m_info)) - (who == 2)
  
  p1_i <- p_info[p1_ind,]
  p2_i <- p_info[p2_ind,]
  p1_s <- p_stats[p1_ind,]
  p2_s <- p_stats[p2_ind,]
  

  p1_i2 <- copy(p1_i)[, id := NULL]
  p2_i2 <- copy(p2_i)[, id := NULL]
  p1_s2 <- copy(p1_s)[, win := NULL]
  p2_s2 <- copy(p2_s)[, win := NULL]
  
  m_i <- m_info
  m_s <- m_stats
  
  p1_r <- matrix(nrow = nrow(m_info), ncol = 281)
  p2_r <- matrix(nrow = nrow(m_info), ncol = 281)
  
  for(i in 1:nrow(m_info)){
    
    if(i %% 200 == 0){
      print(i)
    }

    
    if(!(is.na(last_game_mat[p1_i$id[i],1]))){
      p1_r[i,] <- last_game_mat[p1_i$id[i],]
    }else{
      p1_r[i,] <- c(as.matrix(m_i[i,]),as.matrix(m_s[i,]),
                              as.matrix(p1_s2[i,]),as.matrix(p2_s2[i,]),
                              as.matrix(p2_i2[i,]),rep(0,rec_output_width))
    }

    if(!(is.na(last_game_mat[p2_i$id[i],1]))){
      p2_r[i,] <- last_game_mat[p2_i$id[i],]
    }else{
      p2_r[i,] <- c(as.matrix(m_i[i,]),as.matrix(m_s[i,]),
                              as.matrix(p2_s2[i,]),as.matrix(p1_s2[i,]),
                              as.matrix(p1_i2[i,]),rep(0,rec_output_width))
    }

    
    last_game_mat[p1_i$id[i],] <- c(as.matrix(m_i[i,]),as.matrix(m_s[i,]),
                                 as.matrix(p1_s2[i,]),as.matrix(p2_s2[i,]),
                                 as.matrix(p2_i2[i,]),rep(0,rec_output_width))
    last_game_mat[p2_i$id[i],] <- c(as.matrix(m_i[i,]),as.matrix(m_s[i,]),
                                 as.matrix(p2_s2[i,]),as.matrix(p1_s2[i,]),
                                 as.matrix(p1_i2[i,]),rep(0,rec_output_width))
    

  }
  

  
  data_input <- list(as.matrix(p1_i2), as.matrix(p1_r), as.matrix(p2_i2), as.matrix(p2_r),as.matrix(m_i))
  #saveRDS(data_input, "Modeling/NeuralNetworks/datss")




