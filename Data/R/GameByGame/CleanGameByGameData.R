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
data <- subset(data[-grep(pattern = "Davis", x = data$tourney_name),], select = c("surface", "tourney_date", "winner_name", "loser_name", "score", "w_svpt", "w_1stIn", "w_1stWon", "w_2ndWon", "w_bpSaved", "w_bpFaced", "l_svpt", "l_1stIn", "l_1stWon", "l_2ndWon", "l_bpSaved", "l_bpFaced"))
data$tourney_date <- ymd(data$tourney_date)



# Clean data for clustering -----------------------------------------------

# 2. Select variables -----------------------------------------------------

data_reduced <- data %>% 
                  select(
                    tourney_date,
                    winner_name,
                    winner_ht,
                    loser_name,
                    loser_ht,
                    minutes,
                    w_ace : l_bpFaced
                  )

winner_sum <- data_reduced %>% 
                group_by(
                  name = winner_name
                ) %>% 
                summarize(
                  nb_games = n(),
                  hgt = max(winner_ht),
                  avg_min = mean(minutes, na.rm = TRUE),
                  nb_aces = sum(w_ace, na.rm = TRUE),
                  nb_df = sum(w_df, na.rm = TRUE),
                  nb_sv_pts = sum(w_svpt, na.rm = TRUE),
                  nb_1st_in = sum(w_1stIn, na.rm = TRUE),
                  nb_1st_won = sum(w_1stWon, na.rm = TRUE),
                  nb_2nd_won = sum(w_2ndWon, na.rm = TRUE),
                  nb_bp_faced = sum(w_bpFaced, na.rm = TRUE),
                  nb_bp_saved = sum(w_bpSaved, na.rm = TRUE)
                ) %>% 
                mutate(
                  outcome = "win"
                )

looser_sum <- data_reduced %>% 
                group_by(
                  name = loser_name
                ) %>% 
                summarize(
                  nb_games = n(),
                  hgt = max(loser_ht),
                  avg_min = mean(minutes, na.rm = TRUE),
                  nb_aces = sum(l_ace, na.rm = TRUE),
                  nb_df = sum(l_df, na.rm = TRUE),
                  nb_sv_pts = sum(l_svpt, na.rm = TRUE),
                  nb_1st_in = sum(l_1stIn, na.rm = TRUE),
                  nb_1st_won = sum(l_1stWon, na.rm = TRUE),
                  nb_2nd_won = sum(l_2ndWon, na.rm = TRUE),
                  nb_bp_faced = sum(l_bpFaced, na.rm = TRUE),
                  nb_bp_saved = sum(l_bpSaved, na.rm = TRUE)
                ) %>% 
                mutate(
                  outcome = "loss"
                )

data_tot <- rbind(winner_sum, looser_sum) %>% 
              group_by(
                name
              ) %>% 
              summarize(
                nb_wins = sum(nb_games[outcome == "win"]),
                nb_loss = sum(nb_games[outcome == "loss"]),
                hgt = max(hgt),
                avg_min = mean(avg_min, na.rm = TRUE),
                nb_aces = sum(nb_aces, na.rm = TRUE),
                nb_df = sum(nb_df, na.rm = TRUE),
                nb_sv_pts = sum(nb_sv_pts, na.rm = TRUE),
                nb_1st_in = sum(nb_1st_in, na.rm = TRUE),
                nb_1st_won = sum(nb_1st_won, na.rm = TRUE),
                nb_2nd_won = sum(nb_2nd_won, na.rm = TRUE),
                nb_bp_faced = sum(nb_bp_faced, na.rm = TRUE),
                nb_bp_saved = sum(nb_bp_saved, na.rm = TRUE)
              )

data_clustering <- data_tot %>% 
                      filter(
                        nb_wins >= 150
                      ) %>% 
                      mutate(
                        pourc_sv_win = (nb_1st_won + nb_2nd_won)/nb_sv_pts,
                        pourc_1st_win = nb_1st_won/nb_1st_in,
                        pourc_bp_saved = nb_bp_saved/nb_bp_faced
                      ) %>% 
                      select(
                        name,
                        nb_wins,
                        nb_loss,
                        hgt,
                        avg_min,
                        nb_aces,
                        nb_df,
                        pourc_sv_win,
                        pourc_1st_win,
                        pourc_bp_saved
                      )

# I replace NAs in Height by the mean of all players
data_clustering$hgt[which(is.na(data_clustering$hgt))] <- mean(data_clustering$hgt, na.rm = TRUE)


