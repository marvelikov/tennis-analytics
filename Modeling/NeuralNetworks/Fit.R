# The model
source("Modeling/NeuralNetworks/Structure.R")


layer_names <- c("GRU1_p1", "GRU2_p1", "GRU3_p1", "GRU1_p2", "GRU2_p2", "GRU3_p2")


# data
m_info
m_stats
p_info
p_stats

p_info[, id := frank(id, ties.method = "dense")]

N <- length(unique(p_info$id))

r1 <- sapply(layer_names[1:3], function(nn){
  get_weights(model$get_layer(name = nn))
})

weight_list <- replicate(N, r1)
rm("r1")

s1 <- sapply(layer_names[1:3], function(nn){
  get_config(model$get_layer(name = nn))
})



last_game_mat <- 



for(i in 1:nrow(m_info)){
  
  # choose which player is player 1
  who <- sample.int(2,1)
  
  p1_i <- p_info[2*i - (who == 1),]
  p2_i <- p_info[2*i - (who == 2),]
  p1_s <- p_stats[2*i - (who == 1),]
  p2_s <- p_stats[2*i - (who == 2),]
  
  sapply(seq_along(layer_names), function(j){
    set_weights(model$get_layer(name = layer_names[j]), weights = representation_list[,(j - 1) %% 3 + 1,p1_i$id])
  })


  
  
  
  

  # Need modif....
  model %>% fit(x = list(p1_i2,matrix(p1_rec_input, nrow = 1),p2_i2,matrix(p2_rec_input, nrow = 1),matrix(m_info2, nrow = 1)), y = 1, batch_size = 1 , epochs = 1)
  
  
  
  # Store this game's stats NEED TO GET THE OUTPUT AT THE GRU LAYER!!!
  #last_game_mat[,p1_i$id] <- c(m_i,m_s,p1_s,p2_s,p2_i,  **GRU-OUT-p2**  )
  #last_game_mat[,p2_i$id] <- c(m_i,m_s,p2_s,p1_s,p1_i,  **GRU-OUT-p1**  )
  
  
  # Update representations
  representation_list[[p1_i$id]] <- sapply(layer_names[1:3], function(nn){
    get_weights(model$get_layer(name = nn))
  })
  
  representation_list[[p2_i$id]] <- sapply(layer_names[4:6], function(nn){
    get_weights(model$get_layer(name = nn))
  })
  
  c(as.matrix(m_info[ceiling(p1_last_row/2),-1]),as.matrix(m_stats[ceiling(p1_last_row/2),]),
    as.matrix(p_stats[p1_last_row,]),as.matrix(p_stats[p1_opp_row,]),
    as.matrix(p_info[p1_opp_row,-1]),p1_opp_net_out)
  
}



