#id <- data$match_id[1]


aceMomentum.data <- sapply(unique(data$match_id), function(id){
  print(as.character(id))
  
  sub.data <- subset(data, match_id == id & !is.na(PointServer), c(match_id, P1Ace, P2Ace, PointServer))
  
  P1Serves <- which(sub.data$PointServer == 1)
  P2Serves <- which(sub.data$PointServer == 2)
  
  w <- 1/2^(1:max(c(length(P1Serves),length(P2Serves))))
  
  aceMomentumP1 <- rep(NA, nrow(sub.data))
  l1 <- sum(sub.data$PointServer == 1)
    
  aceMomentumP1[sub.data$PointServer == 1] <- sapply(1:l1, function(i){
    sum(w[i:1] * sub.data[PointServer == 1,P1Ace][1:i])
  })

  aceMomentumP2 <- rep(NA, nrow(sub.data))
  l2 <- sum(sub.data$PointServer == 2)
  aceMomentumP2[sub.data$PointServer == 2] <- sapply(1:l2, function(i){
    sum(w[i:1] * sub.data[PointServer == 2,P2Ace][1:i])
  })
    
  data.table(match_id = rep(id, nrow(sub.data)), aceMomentumP1 = aceMomentumP1, aceMomentumP2 = aceMomentumP2)
})
  
aceMomentum.data <- data.table(match_id = aceMomentum.data[1,], aceMomentumP1 = aceMomentum.data[2,], aceMomentumP2 = aceMomentum.data[3,])
saveRDS(aceMomentum.data, file="aceMomentumData.Rda")
