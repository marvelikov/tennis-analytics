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
# summary(data)

load("Data/Raw/data_slam_matches.RData")
data.matches <- data_slam_matches
rm("data_slam_matches")
# summary(data.matches)


# 2. Create Baseline table for every match --------------------------------


# new col with who won the point
data[, P1_won_point := diff(P1PointsWon), by = match_id]
data[, P2_won_point := diff(P2PointsWon), by = match_id]

data_gathered <- data %>% 
                  filter(
                    ServeIndicator %in% c(1, 2)
                  ) %>% 
                  select(
                    match_id,
                    ServeIndicator,
                    P1_won_point,
                    P2_won_point
                  ) %>% 
                  mutate(
                    pts_tot = P1_won_point + P2_won_point
                  ) %>% 
                  gather(
                    player,
                    pts_won,
                    -c(match_id, ServeIndicator, pts_tot)
                  ) %>% 
                  mutate(
                    player = as.numeric(substr(player, 2, 2)),
                    ServeIndicator = player == ServeIndicator
                  ) %>% 
                  data.table()

data_sum <- data_gathered[, .(pts_won = sum(pts_won, na.rm = TRUE), pts_tot = sum(pts_tot, na.rm = TRUE)), .(match_id, player, ServeIndicator)]
data_sum[, perc_points_won := pts_won/pts_tot][, c("pts_won", "pts_tot") := NULL]


# 3. Function to get Baseline table for a given player --------------------


matchesOfPlayer <- function(player){
  player.num <- 1 * data.matches[,player1 == player] + 2 * data.matches[,player2 == player]
  id <- which(player.num > 0)
  return(data.table(match_id.number = data.matches$match_id[id], player.num = player.num[id]))
}


player_baseline <- function(player_name) {
  player_matches <- matchesOfPlayer(player_name)
  setkey(data_sum, match_id, player)
  setkey(player_matches, match_id.number, player.num)
  
  player_key = unique(data_sum[player_matches, which = TRUE, allow.cartesian = TRUE])
  player_baseline <- data_sum[player_key]
  player_baseline
}

# Exemple:

Fed <- player_baseline(player_name = "Roger Federer")
Nadal <- player_baseline(player_name = "Rafael Nadal")














