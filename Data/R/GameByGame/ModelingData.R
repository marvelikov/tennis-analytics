# Title: ModelingData
# Date: 27 septembre 2017
# Author: Stephane Caron
# Subject: Modeling often requires to structure the data in 1 line per game


# Load packages -----------------------------------------------------------

library(data.table)
library(tidyverse)

# Import summarised data --------------------------------------------------

data_summarised <- fread("Data/Cleaned/DataSummarised.csv")


# Transform data for modeling purposes ------------------------------------

cols_modeling <- c("tourney_date", "match_num", "name", "perc_1st_serve_won", "perc_2nd_serve_won", "perc_return_won", "perc_bp", "perc_win", "ave_pts_game", "surface", "win")
data_pre_modeling <- data_summarised[, !cols_modeling, with = FALSE]

data_summarised_winner <- data_summarised[win == 1,]
new_names_win <- sapply(1:names(data_summarised_winner), function(x) {
  if (x != c("tourney_date", "match_num", "surface")) {
    paste0("w_", x)
  } else {
    x
  }
})
names(data_summarised_winner) <- new_names_win

data_summarised_looser <- data_summarised[win == 0,]
new_names_loss <- sapply(1:names(data_summarised_winner), function(x) {
  if (x != c("tourney_date", "surface")) {
    paste0("l_", x)
  } else {
    x
  }
})
names(data_summarised_looser) <- new_names_loss

# Rejoin both tables together
setkey(data_summarised_winner, tourney_date, match_num, surface)
setkey(data_summarised_looser, tourney_date, match_num, surface)
data_summarised_winner[data_summarised_looser]

data_modeling <- data_summarised %>% 
                    select(
                      -c(w_win, l_win)
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

data_modeling <- data.table(data_modeling)

new_names <- gsub("w_", "p1_", colnames(data_modeling))
new_names <- gsub("l_", "p2_", new_names)
colnames(data_modeling) <- new_names


# Swap the columns
source("Data/R/GameByGame/SwapColumns.R")
data_modeling <- swap_cols(data_modeling)

# Save the data
fwrite(data_modeling, "Data/Cleaned/DataModeling.csv")