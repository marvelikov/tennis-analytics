# Say we want to predict a given match... Let us do it with an example

# 0. Load packages --------------------------------------------------------

require(readr)
require(lubridate)
require(data.table)
require(readr)
require(readxl)
require(tidyverse)

# 1. Import data ----------------------------------------------------------

load("Data/Raw/pts_by_pts.RData")
data <- data_pts_by_pts
rm("data_pts_by_pts")
summary(data)

load("Data/Raw/data_slam_matches.RData")
data.matches <- data_slam_matches
rm("data_slam_matches")
summary(data.matches)



# baseline computation ----------------------------------------------------

# On se concentre sur la partie suivante
the.id <- "2016-wimbledon-1601"
match.info <- data.matches[match_id == the.id,]
the.match <- data[match_id == the.id,]

head(data[match_id == the.id,])


# On veut créer une colonne indiquant qui sert (1 Player1 - 2 Player2)

summary(data$ServeIndicator)
summary(data$PointServer)


# Un baseline vraiment basic -- #ptswon/#ptsTotal * factor(serveOrReturn)

# un baseline pour un match
player <- "Roger Federer"
player.number <- which(c(as.character(match.info$player1), as.character(match.info$player2)) == player)

# new col with who won the point
P1WonPoint <- c(0,diff(the.match[,P1PointsWon]))
P2WonPoint <- c(0,diff(the.match[,P2PointsWon]))
the.match <- cbind(the.match,P1WonPoint,P2WonPoint)

PtsWon.matrix <- the.match[, .(P1WonPoint = sum(P1WonPoint), P2WonPoint = sum(P2WonPoint), P1WonPerc = sum(P1WonPoint)/(sum(P1WonPoint) + sum(P2WonPoint)), P2WonPerc = sum(P2WonPoint)/(sum(P1WonPoint) + sum(P2WonPoint))), ServeIndicator][-1,]
PtsWon.matrix <- the.match[, .(P1WonPoint = sum(P1WonPoint), P2WonPoint = sum(P2WonPoint)), ServeIndicator][-1,]
                  
PtsWon.matrix_gathered <- gather(PtsWon.matrix, key = player, value = PtsWon, -ServeIndicator) %>% 
                            mutate(
                              player = as.numeric(substr(player, 2, 2)),
                              ServeIndicator = player == ServeIndicator
                            ) %>% 
                            select(
                              player,
                              ServeIndicator,
                              PtsWon
                            ) %>% 
                            arrange(
                              player,
                              -ServeIndicator
                            )













                            

# un baseline calculé sur plusieurs matchs


# new col with who won the point
P1WonPoint <- c(0,diff(data[,P1PointsWon]))
P2WonPoint <- c(0,diff(data[,P2PointsWon]))
the.match <- cbind(data,P1WonPoint,P2WonPoint)
  
data.gathered <- data %>% 
                          filter(
                            ServeIndicator %in% c(1, 2)
                          ) %>% 
                          select(
                            match_id,
                            ServeIndicator,
                            P1WonPoint,
                            P2WonPoint
                          ) %>% 
                          gather(
                            player,
                            PtsWon,
                            -c(match_id, ServeIndicator)
                          ) %>% 
                          mutate(
                            player = as.numeric(substr(player, 2, 2)),
                            ServeIndicator = player == ServeIndicator
                          ) %>% 
                          data.table()
  
  data.sum <- data.gathered[, .(PtsWon = sum(PtsWon)), .(match_id, player, ServeIndicator)]
  data.sum[, PtsTot := ]
  
  PtsWon.matrix <- data[, .(P1WonPoint = sum(P1WonPoint), P2WonPoint = sum(P2WonPoint), P1WonPerc = sum(P1WonPoint)/(sum(P1WonPoint) + sum(P2WonPoint)), P2WonPerc = sum(P2WonPoint)/(sum(P1WonPoint) + sum(P2WonPoint))), ServeIndicator][-1,]
  PtsWon.matrix <- data[, .(P1WonPoint = sum(P1WonPoint), P2WonPoint = sum(P2WonPoint)), ServeIndicator][-1,]
  
  PtsWon.matrix_gathered <- gather(PtsWon.matrix, key = player, value = PtsWon, -ServeIndicator) %>% 
    mutate(
      player = as.numeric(substr(player, 2, 2)),
      ServeIndicator = player == ServeIndicator
    ) %>% 
    select(
      player,
      ServeIndicator,
      PtsWon
    ) %>% 
    arrange(
      player,
      -ServeIndicator
    )
  
  return(PtsWon.matrix_gathered)
})
  

























# la fonction qui calcule le baseline de player pour chaque game

i <- 1

id <- player.matches[i,]$match_id
player.number <- player.matches[i,]$player

the.match <- data[match_id == id,]

WonPoint <- c(0,diff(as.matrix(the.match[,P2PointsWon,P1PointsWon])[,player.number]))
the.match <- cbind(the.match,WonPoint)

PtsWon.matrix <- the.match[, .(WonPoint = sum(WonPoint), ServeIndicator][-1,]
PtsWon.matrix <- the.match[, .(P1WonPoint = sum(P1WonPoint), P2WonPoint = sum(P2WonPoint)), ServeIndicator][-1,]

PtsWon.matrix_gathered <- gather(PtsWon.matrix, key = player, value = PtsWon, -ServeIndicator) %>% 
  mutate(
    player = as.numeric(substr(player, 2, 2)),
    ServeIndicator = player == ServeIndicator
  ) %>% 
  select(
    player,
    ServeIndicator,
    PtsWon
  ) %>% 
  arrange(
    player,
    -ServeIndicator
  )




