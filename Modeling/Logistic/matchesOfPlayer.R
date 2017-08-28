matchesOfPlayer <- function(player){
  matches <- sapply(1:nrow(data.matches), function(i){
    w <- which(c(as.character(data.matches[i,player1]) == player, as.character(data.matches[i,player2]) == player))
    if(length(w) == 0){
      return(NULL)
    }else{
      return(c(i,w))
    }
  }, simplify = FALSE)
  
  do.call(rbind, matches)
}


