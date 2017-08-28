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
P1_won_point <- c(0, diff(data[, P1PointsWon]))
P2_won_point <- c(0, diff(data[, P2PointsWon]))
data_summarized <- cbind(data, P1_won_point, P2_won_point)

data_gathered <- data_summarized %>% 
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




