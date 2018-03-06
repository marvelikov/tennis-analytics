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
m_info[, (cols) := lapply(.SD, function(x){x/max(x)}), .SDcols=cols]

cols <- colnames(m_stats)
m_stats[, (cols) := lapply(.SD, function(x){x/max(x)}), .SDcols=cols]

cols <- colnames(p_info)[c(2,11,81:83)]
p_info[, (cols) := lapply(.SD, function(x){x/max(x)}), .SDcols=cols]

#cols <- colnames(p_stats)[c(2:18)]
#p_stats[, (cols) := lapply(.SD, scale), .SDcols=cols]
cols <- colnames(p_stats)[2:18]
p_stats[, (cols) := lapply(.SD, function(x){x/max(x)}), .SDcols=cols]


N <- length(unique(p_info$id))

r1 <- sapply(layer_names[1:3], function(nn){
  get_weights(model$get_layer(name = nn))
})

weight_list <- replicate(N, r1)
rm("r1")


last_game_mat <- matrix(nrow = N, ncol = rec_width)



#for(i in 1:nrow(m_info)){
for(i in 1:100){
  
  print(i)

  # choose which player is player 1
  who <- sample.int(2,1)
  
  p1_ind <- 2*i - (who == 1)
  p2_ind <- 2*i - (who == 2)
  
  p1_i <- p_info[p1_ind,]
  p2_i <- p_info[p2_ind,]
  p1_s <- p_stats[p1_ind,]
  p2_s <- p_stats[p2_ind,]
  
  sapply(1:3, function(j){
    set_weights(model$get_layer(name = layer_names[j]), weights = weight_list[,j,p1_i$id])
    set_weights(model$get_layer(name = layer_names[3+j]), weights = weight_list[,j,p2_i$id])
  })
  
  p1_i2 <- copy(p1_i)[, id := NULL]
  p2_i2 <- copy(p2_i)[, id := NULL]
  p1_s2 <- copy(p1_s)[, win := NULL]
  p2_s2 <- copy(p2_s)[, win := NULL]
  
  m_i <- m_info[i,]
  m_s <- m_stats[i,]
  

  if(!(is.na(last_game_mat[p1_i$id,1]))){
    p1_rec_input <- last_game_mat[p1_i$id,]
  }else{
    p1_rec_input <- c(as.matrix(m_i),as.matrix(m_s),
                      as.matrix(p1_s2),as.matrix(p2_s2),
                      as.matrix(p2_i2),rep(0,rec_output_width))
  }
  if(!(is.na(last_game_mat[p2_i$id,1]))){
    p2_rec_input <- last_game_mat[p2_i$id,]
  }else{
    p2_rec_input <- c(as.matrix(m_i),as.matrix(m_s),
                      as.matrix(p2_s2),as.matrix(p1_s2),
                      as.matrix(p1_i2),rep(0,rec_output_width))
  }
  
  

  
  data_input <- list(as.matrix(p1_i2), matrix(p1_rec_input,nrow=1), as.matrix(p2_i2), matrix(p2_rec_input,nrow=1),matrix(m_info2,nrow=1))
  
  gru_p1_model <- keras_model(inputs = model$input, outputs = get_layer(model, "GRU3_p1")$output)
  gru_p2_model <- keras_model(inputs = model$input, outputs = get_layer(model, "GRU3_p2")$output)
  gru_p1_output <- predict(gru_p1_model, data_input)
  gru_p2_output <- predict(gru_p2_model, data_input)
  
  model %>% fit(x = data_input, y = as.numeric(who == 1), batch_size = 1 , epochs = 1)
    

  last_game_mat[p1_i$id,] <- c(as.matrix(m_i),as.matrix(m_s),
                    as.matrix(p1_s2),as.matrix(p2_s2),
                    as.matrix(p2_i2),gru_p1_output)
  last_game_mat[p2_i$id,] <- c(as.matrix(m_i),as.matrix(m_s),
                    as.matrix(p2_s2),as.matrix(p1_s2),
                    as.matrix(p1_i2),gru_p2_output)
  

  # Update representations
  weight_list[,,p1_i$id] <- sapply(layer_names[1:3], function(nn){
    get_weights(model$get_layer(name = nn))
  })
  
  weight_list[,,p2_i$id] <- sapply(layer_names[4:6], function(nn){
    get_weights(model$get_layer(name = nn))
  })
  
}



