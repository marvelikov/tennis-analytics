# Title: CreateVariablesData
# Date: 30 septembre 2017
# Author: Stephane Caron
# Subject: In this script we create the variables that will be used in modeling

library(data.table)

# Import summarised data --------------------------------------------------

split_data_into <- 4
data_summarised <- import_splitted_data(filename = "Data/Cleaned/DataSummarised", split_number = split_data_into)
# data_summarised <- fread("Data/Cleaned/DataSummarised.csv", stringsAsFactors = FALSE)


# Creates variables explicatives  -------------------------------------------------------

data_summarised[, perc_1st_serve_won_30 := `1stWon_30`/svpt_30]
data_summarised[, perc_1st_serve_won_365 := `1stWon_365`/svpt_365]
data_summarised[, perc_2nd_serve_won_30 := `2ndWon_30`/svpt_30]
data_summarised[, perc_2nd_serve_won_365 := `2ndWon_365`/svpt_365]
data_summarised[, perc_return_won_30 := rpt_won_30/rpt_30]
data_summarised[, perc_return_won_365 := rpt_won_365/rpt_365]
data_summarised[, perc_bp_30 := bpSaved_30/bpFaced_30]
data_summarised[, perc_bp_365 := bpSaved_365/bpFaced_365]
data_summarised[, game_played_clay := (win_Clay_99999 + loss_Clay_99999)]
data_summarised[, game_played_grass := (win_Grass_99999 + loss_Grass_99999)]
data_summarised[, game_played_hard := (win_Hard_99999 + loss_Hard_99999)]
data_summarised[, perc_win_clay := win_Clay_99999/game_played_clay]
data_summarised[, perc_win_grass := win_Grass_99999/game_played_grass]
data_summarised[, perc_win_hard := win_Hard_99999/game_played_hard]
data_summarised[, game_played_30 := (win_30 + loss_30)]
data_summarised[, game_played_365 := (win_365 + loss_365)]
data_summarised[, perc_win_30 := win_30/game_played_30]
data_summarised[, perc_win_365 := win_365/game_played_365]
data_summarised[, perc_1st_in_30 := `1stIn_30`/svpt_30]
data_summarised[, perc_1st_in_365 := `1stIn_365`/svpt_365]
data_summarised[, top_4 := top_4_365]
data_summarised[, top_4_GS := top_4_GS_99999]
data_summarised[, min_played := minutes_3]
data_summarised[, injury := retired_365/game_played_365]

#######################################
#### Adding Uncertainty indicators ####
#######################################
data_summarised[, game_played_30_uncertainty := 1/game_played_30]
data_summarised[, game_played_365_uncertainty := 1/game_played_365]
data_summarised[, game_played_clay_uncertainty := 1/game_played_clay]
data_summarised[, game_played_grass_uncertainty := 1/game_played_grass]
data_summarised[, game_played_hard_uncertainty := 1/game_played_hard]



# Create new response variable --------------------------------------------

data_summarised[, score_sum := unlist(lapply(.I, function(i) {
  sum(data_summarised[i, c("score_set_1", "score_set_2", "score_set_3", "score_set_4", "score_set_5"), with = FALSE], na.rm = TRUE)
}))]


# Correct null ratios -----------------------------------------------------

# Remove NaN
data_summarised[svpt_365 == 0, perc_1st_serve_won_365 := 0]
data_summarised[svpt_30 == 0, perc_1st_serve_won_30 := 0]
data_summarised[svpt_365 == 0, perc_2nd_serve_won_365 := 0]
data_summarised[svpt_30 == 0, perc_2nd_serve_won_30 := 0]
data_summarised[rpt_365 == 0, perc_return_won_365 := 0]
data_summarised[rpt_30 == 0, perc_return_won_30 := 0]
data_summarised[bpFaced_365 == 0, perc_bp_365 := 0]
data_summarised[bpFaced_30 == 0, perc_bp_30 := 0]
data_summarised[game_played_365 == 0, perc_win_365 := 0]
data_summarised[game_played_30 == 0, perc_win_30 := 0]
data_summarised[game_played_clay == 0, perc_win_clay := 0]
data_summarised[game_played_grass == 0, perc_win_grass := 0]
data_summarised[game_played_hard == 0, perc_win_hard := 0]
# data_summarised[game_played == 0, ave_pts_game := 0]
data_summarised[svpt_365 == 0, perc_1st_in_365 := 0]
data_summarised[svpt_30 == 0, perc_1st_in_30 := 0]
data_summarised[game_played_365 == 0, injury := 0]


###################################################################
#### putting 1 (extreme uncertainty) when no games were played ####
###################################################################
data_summarised[game_played_365 == 0, game_played_365_uncertainty := 1]
data_summarised[game_played_30 == 0, game_played_30_uncertainty := 1]
data_summarised[game_played_clay == 0, game_played_clay_uncertainty := 1]
data_summarised[game_played_grass == 0, game_played_grass_uncertainty := 1]
data_summarised[game_played_hard == 0, game_played_hard_uncertainty := 1]


data_pre_modeling <- data_summarised
