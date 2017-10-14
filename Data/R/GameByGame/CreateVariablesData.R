# Title: CreateVariablesData
# Date: 30 septembre 2017
# Author: Stephane Caron
# Subject: In this script we create the variables that will be used in modeling

library(data.table)

# Import summarised data --------------------------------------------------

data_summarised <- fread("Data/Cleaned/DataSummarised.csv", stringsAsFactors = FALSE)


# Creates variables -------------------------------------------------------


# Calculate tennis stats variables/ratios 

# Ratio no.1: Winning percentage on 1st serve
data_summarised[, perc_1st_serve_won := `1stWon_365`/svpt_365]

# Ratio no.2: Winning percentage on 2nd serve
data_summarised[, perc_2nd_serve_won := `2ndWon_365`/svpt_365]

# Ratio no.3: Winning percentage on return serve 
data_summarised[, perc_return_won := rpt_won_365/rpt_365]

# Ratio no.4: Winning percentage on break point
data_summarised[, perc_bp := bpSaved_365/bpFaced_365]

# Number of games played by surface in last 3 years
data_summarised[, game_played_clay := (win_Clay_1095 + loss_Clay_1095)]
data_summarised[, game_played_grass := (win_Grass_1095 + loss_Grass_1095)]
data_summarised[, game_played_hard := (win_Hard_1095 + loss_Hard_1095)]

# Ratio no.5: Winning percentage of match per surface
data_summarised[, perc_win_clay := win_Clay_1095/game_played_clay]
data_summarised[, perc_win_grass := win_Grass_1095/game_played_grass]
data_summarised[, perc_win_hard := win_Hard_1095/game_played_hard]

# Number of games last year
data_summarised[, game_played := (win_365 + loss_365)]

# Ratio no.6: Winning percentage of match all surfaces in last year
data_summarised[, perc_win := win_365/game_played]

# Ratio no.6: Average point per game
data_summarised[, ave_pts_game := (`1stWon_365` + `2ndWon_365` + rpt_won_365)/game_played]

# Ratio no.7: 1st serve In percentage
data_summarised[, perc_1st_in := `1stIn_365`/svpt_365]




# Remove NaN
data_summarised[svpt_365 == 0, perc_1st_serve_won := 0]
data_summarised[svpt_365 == 0, perc_2nd_serve_won := 0]
data_summarised[rpt_365 == 0, perc_return_won := 0]
data_summarised[bpFaced_365 == 0, perc_bp := 0]
data_summarised[game_played == 0, perc_win := 0]
data_summarised[game_played_clay == 0, perc_win_clay := 0]
data_summarised[game_played_grass == 0, perc_win_grass := 0]
data_summarised[game_played_hard == 0, perc_win_hard := 0]
data_summarised[game_played == 0, ave_pts_game := 0]
data_summarised[svpt_365 == 0, perc_1st_in := 0]

data_pre_modeling <- data_summarised
