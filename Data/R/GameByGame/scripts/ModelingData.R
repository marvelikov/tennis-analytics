# Title: ModelingData
# Date: 27 septembre 2017
# Author: Stephane Caron
# Subject: Modeling often requires to structure the data in 1 line per game


# Load packages -----------------------------------------------------------

library(data.table)
library(tidyverse)

# Import summarised data --------------------------------------------------

#data_pre_modeling <- import_splitted_data(filename = "Data/Cleaned/DataPreModeling", split_number = split_data_into)
#data_pre_modeling <- fread("Data/Cleaned/DataPreModeling.csv")



# Transform data for modeling purposes ------------------------------------

cols_modeling <- c("tourney_date", 
                   "match_num", 
                   "tourney_name", 
                   "name", 
                   "perc_1st_serve_won_30", 
                   "perc_1st_serve_won_365", 
                   "perc_2nd_serve_won_30", 
                   "perc_2nd_serve_won_365", 
                   "perc_return_won_30", 
                   "perc_return_won_365", 
                   "perc_bp_30", 
                   "perc_bp_365", 
                   "perc_win_30", 
                   "perc_win_365", 
                   "game_played_30", 
                   "perc_win_clay", 
                   "perc_win_grass", 
                   "perc_win_hard", 
                   "perc_1st_in_30", 
                   "perc_1st_in_365", 
                   "injury", 
                   "top_4", 
                   "top_4_GS", 
                   "ind_GS", 
                   "min_played", 
                   "surface",
                   "game_played_365_uncertainty", 
                   "game_played_30_uncertainty", 
                   "game_played_clay_uncertainty", 
                   "game_played_grass_uncertainty", 
                   "game_played_hard_uncertainty", 
                   "retired",
                   "score_sum",
                   "win")

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
source("Data/R/GameByGame/scripts/SwapColumns.R")
data_modeling <- swap_cols(data_modeling) %>% select(-tourney_name)


# Create other response variables -----------------------------------------------

data_modeling[, p1_perc_game_win := p1_score_sum/(p1_score_sum + p2_score_sum)]
data_modeling <- data_modeling[!is.na(p1_perc_game_win),] # On exclut les matchs qui n'ont pas du tout eu lieu ... ca ajoute probablement du noise dans le modele

data_modeling[p1_win == 1, p1_perc_game_win := pmax(0.5, p1_perc_game_win)]
data_modeling[p1_win == 0, p1_perc_game_win := pmin(0.5, p1_perc_game_win)]
data_modeling <- data_modeling[,-c("p1_score_sum", "p2_score_sum"), with = FALSE]


# Save the data
# fwrite(data_modeling, "Data/Cleaned/DataModeling.csv")
