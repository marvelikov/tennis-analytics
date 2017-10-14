# Title: ModelingData
# Date: 27 septembre 2017
# Author: Stephane Caron
# Subject: Modeling often requires to structure the data in 1 line per game


# Load packages -----------------------------------------------------------

library(data.table)
library(tidyverse)

# Import summarised data --------------------------------------------------

data_pre_modeling <- fread("Data/Cleaned/DataPreModeling.csv")


# Transform data for modeling purposes ------------------------------------

cols_modeling <- c("tourney_date", "match_num", "tourney_name", "name", "perc_1st_serve_won", "perc_2nd_serve_won", "perc_return_won", "perc_bp", "perc_clay_win", "perc_grass_win", "perc_hard_win", "ave_pts_game", "perc_1st_in", "surface", "win")
data_pre_modeling <- data_pre_modeling[, cols_modeling, with = FALSE]

data_pre_modeling_winner <- data_pre_modeling[win == 1,]
new_names_win <- sapply(1:length(data_pre_modeling_winner), function(x) {
  if (names(data_pre_modeling_winner)[x] %in% c("tourney_date", "tourney_name" , "match_num", "surface")) {
    names(data_pre_modeling_winner)[x]
  } else {
    names(data_pre_modeling_winner)[x] <- paste0("w_", names(data_pre_modeling_winner)[x])
  }
})
names(data_pre_modeling_winner) <- new_names_win

data_pre_modeling_looser <- data_pre_modeling[win == 0,]
new_names_loss <- sapply(1:length(data_pre_modeling_looser), function(x) {
  if (names(data_pre_modeling_looser)[x] %in% c("tourney_date", "tourney_name" , "match_num", "surface")) {
    names(data_pre_modeling_looser)[x]
  } else {
    names(data_pre_modeling_looser)[x] <- paste0("l_", names(data_pre_modeling_looser)[x])
  }
})
names(data_pre_modeling_looser) <- new_names_loss

# Rejoin both tables together
setkey(data_pre_modeling_winner, tourney_date, tourney_name, match_num, surface)
setkey(data_pre_modeling_looser, tourney_date, tourney_name, match_num, surface)
data_pre_modeling <- data_pre_modeling_winner[data_pre_modeling_looser]
rm(data_pre_modeling_winner, data_pre_modeling_looser)

data_modeling <- data_pre_modeling %>% 
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
                      p1 = w_name,
                      p2 = l_name
                    )

data_modeling <- data.table(data_modeling)

new_names <- gsub("w_", "p1_", colnames(data_modeling))
new_names <- gsub("l_", "p2_", new_names)
colnames(data_modeling) <- new_names

# Swap the columns
source("Data/R/GameByGame/SwapColumns.R")
data_modeling <- swap_cols(data_modeling) %>% select(-tourney_name)

# Save the data
fwrite(data_modeling, "Data/Cleaned/DataModeling.csv")
