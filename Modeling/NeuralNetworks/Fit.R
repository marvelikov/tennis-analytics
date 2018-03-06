# The model
source("Modeling/NeuralNetworks/Structure.R")


layer_names <- c("GRU1_p1", "GRU2_p1", "GRU3_p1", "GRU1_p2", "GRU2_p2", "GRU3_p2")


# data
m_info
m_stats
p_info
p_stats


m_info[, match_id := NULL]
p_info[, id := frank(id, ties.method = "dense")]

N <- length(unique(p_info$id))

r1 <- sapply(layer_names[1:3], function(nn){
  get_weights(model$get_layer(name = nn))
})

weight_list <- replicate(N, r1)
rm("r1")


last_game_mat <- matrix(nrow = N, ncol = rec_width)



#for(i in 1:nrow(m_info)){
for(i in 1:5){

  # choose which player is player 1
  who <- sample.int(2,1)
  
  p1_ind <- 2*i - (who == 1)
  p2_ind <- 2*i - (who == 2)
  
  p1_i <- p_info[p1_ind,]
  p2_i <- p_info[p2_ind,]
  p1_s <- p_stats[p1_ind,]
  p2_s <- p_stats[p2_ind,]
  
  sapply(seq_along(layer_names), function(j){
    print(j)
    set_weights(model$get_layer(name = layer_names[j]), weights = weight_list[,(j - 1) %% 3 + 1,p1_i$id])
  })
  
  p1_i2 <- copy(p1_i)[, id := NULL]
  p2_i2 <- copy(p2_i)[, id := NULL]

  m_i <- m_info[i,]
  m_s <- m_stats[i,]
  

  if(!(is.na(last_game_mat[p1_i$id,1]) | is.na(last_game_mat[p1_i$id,1]))){
    p1_rec_input <- last_game_mat[p1_i$id,]
    p2_rec_input <- last_game_mat[p2_i$id,]
  }else{
    p1_rec_input <- c(as.matrix(m_i),as.matrix(m_s),
                      as.matrix(p1_s),as.matrix(p2_s),
                      as.matrix(p2_i2),rep(0,rec_output_width))
    p2_rec_input <- c(as.matrix(m_i),as.matrix(m_s),
                      as.matrix(p2_s),as.matrix(p1_s),
                      as.matrix(p1_i2),rep(0,rec_output_width))
  }
  
  data_input <- list(as.matrix(p1_i2), matrix(p1_rec_input,nrow=1), as.matrix(p2_i2), matrix(p2_rec_input,nrow=1),matrix(m_info2,nrow=1))
  model %>% fit(x = data_input, y = as.numeric(who == 1), batch_size = 1 , epochs = 1)
    


  gru_p1_model <- keras_model(inputs = model$input, outputs = get_layer(model, "GRU3_p1")$output)
  gru_p2_model <- keras_model(inputs = model$input, outputs = get_layer(model, "GRU3_p2")$output)
  gru_p1_output <- predict(gru_p1_model, data_input)
  gru_p2_output <- predict(gru_p2_model, data_input)

  
  last_game_mat[p1_i$id,] <- c(as.matrix(m_i),as.matrix(m_s),
                    as.matrix(p1_s),as.matrix(p2_s),
                    as.matrix(p2_i2),gru_p1_output)
  last_game_mat[p2_i$id,] <- c(as.matrix(m_i),as.matrix(m_s),
                    as.matrix(p2_s),as.matrix(p1_s),
                    as.matrix(p1_i2),gru_p2_output)
  

  # Update representations
  weight_list[[p1_i$id]] <- sapply(layer_names[1:3], function(nn){
    get_weights(model$get_layer(name = nn))
  })
  
  weight_list[[p2_i$id]] <- sapply(layer_names[4:6], function(nn){
    get_weights(model$get_layer(name = nn))
  })
  
}



