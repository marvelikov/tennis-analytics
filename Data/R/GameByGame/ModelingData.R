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

data_modeling <- data.table(data_modeling)

new_names <- gsub("w_", "p1_", colnames(data_modeling))
new_names <- gsub("l_", "p2_", new_names)
colnames(data_modeling) <- new_names


# Swap the columns
source("Data/R/GameByGame/SwapColumns.R")
data_modeling <- swap_cols(data_modeling)

# Save the data
fwrite(data_modeling, "Data/Cleaned/DataModeling.csv")