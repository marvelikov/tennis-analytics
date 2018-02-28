source("Modeling/NeuralNetworks/Structure.R")


# How to handle id and everything? This is not that nice...
#m_stats[, match_id := m_info[,1]]
#p_info[, match_id := rep(m_info[,1], each = 2)]
#p_stats[, match_id := rep(m_info[,1], each = 2)]


######

i <- 200

p1_wins <- rbernoulli(1)

p1_i <- p_info[2*i - p1_wins,]
p1_id <- p1_i$id
  
p2_i <- p_info[2*i - !p1_wins,]
p2_id <- p2_i$id



# need that to be more efficient
p1_last_row <- rev(which(p_info[1:(2*(i-1)),]$id == p1_id))[1]
p1_opp_row <- p1_last_row + (-1)^(p1_last_row %% 2 == 0)

p1_opp_net_out <- rnorm(20,0,1)

p2_last_row <- rev(which(p_info[1:(2*(i-1)),]$id == p2_id))[1]
p2_opp_row <- p2_last_row + (-1)^(p2_last_row %% 2 == 0)

p2_opp_net_out <- rnorm(20,0,1)


p1_rec_input <- c(as.matrix(m_info[ceiling(p1_last_row/2),-1]),as.matrix(m_stats[ceiling(p1_last_row/2),]),
                  as.matrix(p_stats[p1_last_row,]),as.matrix(p_stats[p1_opp_row,]),
                  as.matrix(p_info[p1_opp_row,-1]),p1_opp_net_out)
p2_rec_input <- c(as.matrix(m_info[ceiling(p2_last_row/2),-1]),as.matrix(m_stats[ceiling(p2_last_row/2),]),
                  as.matrix(p_stats[p2_last_row,]),as.matrix(p_stats[p2_opp_row,]),
                  as.matrix(p_info[p2_opp_row,-1]),p2_opp_net_out)


m_info2 <- m_info[, match_id := NULL]
m_info2 <- as.matrix(m_info2)[1,]
p1_i2 <- p1_i[, id := NULL]
p1_i2 <- as.matrix(p1_i2)
p2_i2 <- p2_i[, id := NULL]
p2_i2 <- as.matrix(p2_i2)


data_input <- cbind(p1_i2,matrix(p1_rec_input, nrow = 1),p2_i2,matrix(p2_rec_input, nrow = 1),matrix(m_info2, nrow = 1))


model %>% compile(
  loss = 'mean_absolute_error', # We have 0-1 classification...
  optimizer = 'adamax', # To be investigated
  metrics = c("categorical_accuracy")  
)

model %>% fit(x = list(p1_i2,matrix(p1_rec_input, nrow = 1),p2_i2,matrix(p2_rec_input, nrow = 1),matrix(m_info2, nrow = 1)), y = 1, batch_size = 1 , epochs = 1)

list(p1_i2,matrix(p1_rec_input, nrow = 1),p2_i2,matrix(p2_rec_input, nrow = 1),matrix(m_info2, nrow = 1))
