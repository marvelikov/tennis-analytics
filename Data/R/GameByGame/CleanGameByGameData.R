# Title: CleanGameByGameData
# Date: 5 Aout 2017
# Author: Stephane Caron
# Subject: Clean tennis game by game data


# Here, I just cleaned the data from JackSackmann in order to organise it by player. 
# We could adapt this script to keep the data by games 

# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)

# Import data -------------------------------------------------------------

data <- fread("Data/Raw/data_game_by_game.csv")


# Clean data --------------------------------------------------------------

# Remove Davis Cup
data <- subset(data[-grep(pattern = "Davis", x = data$tourney_name),], select = c("surface", "tourney_date", "match_num", "winner_name", "loser_name", "score", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_bpSaved", "w_bpFaced", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_bpSaved", "l_bpFaced"))
data$tourney_date <- ymd(data$tourney_date)

# Calculate service return stats
data$w_rpt_won <- data$l_svpt - (data$l_1stWon + data$l_2ndWon)
data$l_rpt_won <- data$w_svpt - (data$w_1stWon + data$w_2ndWon)

# Summer les variables pour la dernière année
data$w_svpt_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$w_svpt, na.rm = TRUE)})
data$l_svpt_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$l_svpt, na.rm = TRUE)})

data$w_1stIn_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$w_1stIn, na.rm = TRUE)})
data$l_1stIn_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$l_1stIn, na.rm = TRUE)})

data$w_1stWon_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$w_1stWon, na.rm = TRUE)})
data$l_1stWon_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$l_1stWon, na.rm = TRUE)})

data$w_2ndWon_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$w_2ndWon, na.rm = TRUE)})
data$l_2ndWon_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$l_2ndWon, na.rm = TRUE)})

data$w_bpSaved_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$w_bpSaved, na.rm = TRUE)})
data$l_bpSaved_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$l_bpSaved, na.rm = TRUE)})

data$w_bpFaced_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$w_bpFaced, na.rm = TRUE)})
data$l_bpFaced_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$l_bpFaced, na.rm = TRUE)})

data$w_rpt_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$l_svpt, na.rm = TRUE)})
data$l_rpt_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$w_svpt, na.rm = TRUE)})

data$w_rpt_won_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$winner_name[x] == data$winner_name | data$winner_name[x] == data$loser_name),]$w_rpt_won, na.rm = TRUE)})
data$l_rpt_won_365 <- sapply(X = 1:nrow(data), FUN = function(x) {sum(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & (data$loser_name[x] == data$winner_name | data$loser_name[x] == data$loser_name),]$l_rpt_won, na.rm = TRUE)})

data$w_wins_365 <- sapply(X = 1:nrow(data), FUN = function(x) {nrow(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & data$winner_name[x] == data$winner_name,])})
data$w_loss_365 <- sapply(X = 1:nrow(data), FUN = function(x) {nrow(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & data$winner_name[x] == data$loser_name,])})

data$l_wins_365 <- sapply(X = 1:nrow(data), FUN = function(x) {nrow(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & data$loser_name[x] == data$winner_name,])})
data$l_loss_365 <- sapply(X = 1:nrow(data), FUN = function(x) {nrow(data[difftime(data$tourney_date[x], data$tourney_date, units = "days") < 365 & (difftime(data$tourney_date[x], data$tourney_date, units = "days") > 0 | (difftime(data$tourney_date[x], data$tourney_date, units = "days") == 0 & data$match_num[x] > data$match_num)) & data$loser_name[x] == data$loser_name,])})


# Define ratios -----------------------------------------------------------

# Ratio no.1: Winning percentage on 1st serve
data[, w_perc_1st_serve_won := w_1stWon_365/w_svpt_365]
data[, l_perc_1st_serve_won := l_1stWon_365/l_svpt_365]
# Remove NaN
data[w_svpt_365 == 0, w_perc_1st_serve_won := 0]
data[l_svpt_365 == 0, l_perc_1st_serve_won := 0]

# Ratio no.2: Winning percentage on 2nd serve
data[, w_perc_2nd_serve_won := w_2ndWon_365/w_svpt_365]
data[, l_perc_2nd_serve_won := l_2ndWon_365/l_svpt_365]
# Remove NaN
data[w_svpt_365 == 0, w_perc_2nd_serve_won := 0]
data[l_svpt_365 == 0, l_perc_2nd_serve_won := 0]

# Ratio no.3: Winning percentage on return serve 
data[, w_perc_return_won := w_rpt_won_365/w_rpt_365]
data[, l_perc_return_won := l_rpt_won_365/l_rpt_365]
# Remove NaN
data[w_rpt_365 == 0, w_perc_return_won := 0]
data[l_rpt_365 == 0, l_perc_return_won := 0]

# Ratio no.4: Winning percentage on break point
data[, w_perc_bp := w_bpSaved_365/w_bpFaced_365]
data[, l_perc_bp := l_bpSaved_365/l_bpFaced_365]
# Remove NaN
data[w_bpFaced_365 == 0, w_perc_bp := 0]
data[l_bpFaced_365 == 0, l_perc_bp := 0]

# Ratio no.5: Winning percentage of match
data[, w_perc_win := w_wins_365/(w_wins_365 + w_loss_365)]
data[, l_perc_win := l_wins_365/(l_wins_365 + l_loss_365)]
# Remove NaN
data[(w_wins_365 + w_loss_365) == 0, w_perc_win := 0]
data[(l_wins_365 + l_loss_365) == 0, l_perc_win := 0]

# Ratio no.6: Average point per game
data[, w_ave_pts_game := (w_1stWon_365 + w_2ndWon_365 + w_rpt_won_365)/(w_wins_365 + w_loss_365)]
data[, l_ave_pts_game := (l_1stWon_365 + l_2ndWon_365 + l_rpt_won_365)/(l_wins_365 + l_loss_365)]
# Remove NaN
data[(w_wins_365 + w_loss_365) == 0, w_ave_pts_game := 0]
data[(l_wins_365 + l_loss_365) == 0, l_ave_pts_game := 0]

fwrite(x = data, "Data/Cleaned/DataGameByGame.csv")
data <- fread("Data/Cleaned/DataGameByGame.csv")

# Create modeling data  ----------------------------------------------

data_modeling <- data %>% 
                  select(
                    -c(score, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_bpSaved, w_bpFaced, l_svpt, l_1stIn, l_1stWon, l_2ndWon, l_bpSaved, l_bpFaced, w_rpt_won, l_rpt_won, w_svpt_365, l_svpt_365, w_1stIn_365, l_1stIn_365, w_1stWon_365, l_1stWon_365, w_2ndWon_365, l_2ndWon_365, w_bpSaved_365, l_bpSaved_365, w_bpFaced_365, l_bpFaced_365, w_rpt_365, l_rpt_365, w_rpt_won_365, l_rpt_won_365, w_wins_365, w_loss_365, l_wins_365, l_loss_365) 
                  ) %>% 
                  spread(
                    surface,
                    surface,
                    fill = 0
                  ) %>% 
                  mutate(
                    clay = ifelse(Clay != 0, 1, 0),
                    hard = ifelse(Hard != 0 ,1, 0),
                    grass = ifelse(Grass != 0, 1, 0),
                    p1_win = 1
                  ) %>% 
                  select(
                    -c(Hard, Clay, Grass)
                  ) %>% 
                  rename(
                    p1 = winner_name,
                    p2 = loser_name
                  )

new_names <- gsub("w_", "p1_", colnames(data_modeling))
new_names <- gsub("l_", "p2_", new_names)

colnames(data_modeling) <- new_names
 

                

