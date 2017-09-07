matchesOfPlayer <- function(player){
  player.num <- 1 * data.matches[,player1 == player] + 2 * data.matches[,player2 == player]
  id <- which(player.num > 0)
  return(data.table(match_id.number = data.matches$match_id[id], player.num = player.num[id]))
}


