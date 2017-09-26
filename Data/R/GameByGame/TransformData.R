############################
## Data from JackSackmann ##
############################

# Description: We clean the data so as to have 2 lines per match (which we keep track of with a match_id).


# This file is on pause as I now wonder whether we should just construct a file in the style of DataModeling.csv
# so as to have a match_id and other information like Tournament name and so on.
# Later on we will be able to split the matches on 2 lines.



# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)

# Import raw data ---------------------------------------------------------

data_raw <- fread("Data/Raw/DataRawGameByGame.csv")
data_transformed <- data_raw[-grep(pattern = "Davis", x = data_raw$tourney_name),]
data_transformed$tourney_date <- ymd(data_transformed$tourney_date)


# Rearrange the data_transformed into two lines per game ------------------------------

# Calculate service return stats
data_transformed$w_rpt_won <- data_transformed$l_svpt - (data_transformed$l_1stWon + data_transformed$l_2ndWon)
data_transformed$l_rpt_won <- data_transformed$w_svpt - (data_transformed$w_1stWon + data_transformed$w_2ndWon)

# get winner/loser variable names
winner_ind <- c(grep(pattern = "w_", x = names(data_transformed)), grep(pattern = "winner", x = names(data_transformed)))
winner_ind <- winner_ind[-1]

loser_ind <- c(grep(pattern = "l_", x = names(data_transformed)), grep(pattern = "loser", x = names(data_transformed)))

winner_variables <- names(data_transformed)[winner_ind]
loser_variables <- names(data_transformed)[loser_ind]

# put the names first...
winner_variables <- c("winner_name", winner_variables[-grep(pattern = "name", x = winner_variables)])
loser_variables <- c("loser_name", loser_variables[-grep(pattern = "name", x = loser_variables)])

# get general variables [tourney_id and match_num will be our id]
# watch out here for errors if changes are made to raww data_transformed...
general_variables <- names(data_transformed)[-c(winner_ind, loser_ind)]
rm("winner_ind", "loser_ind")

new_names <- gsub(loser_variables, pattern = "l_", replacement = "")
new_names <- gsub(new_names, pattern = "loser_", replacement = "")

winner_variables <- c(winner_variables, general_variables)
loser_variables <- c(loser_variables, general_variables)
new_names <- c(new_names, general_variables)

data_transformed_winner <- subset(data_transformed, select = winner_variables)
data_transformed_loser <- subset(data_transformed, select = loser_variables)

names(data_transformed_winner) <- new_names
names(data_transformed_loser) <- new_names

data_transformed_winner[, win := rep(1, nrow(data_transformed_winner))]
data_transformed_loser[, win := rep(0, nrow(data_transformed_winner))]

data_transformed <- rbindlist(l = list(data_transformed_winner, data_transformed_loser), use.names = TRUE, fill = TRUE)
data_transformed[, loss := 0]
data_transformed[win == 0, loss := 1]

# reorder cols for visual purposes.
new_names <- c(new_names, "win", "loss")
cbind(1:length(new_names), new_names)
ord <- c(1, 32, 33, 28, 30, 22, 26, 12:21, 27, 23:25, 29, 2:11)

#length(ord) == length(new_names)
#setdiff(ord, 1:length(new_names))

new_names <- new_names[ord]
data_transformed <- subset(data_transformed, select = new_names)

# reorder rows to have opponents beside (row-wise) one another.
data_transformed <- data_transformed[order(tourney_id ,tourney_date, match_num)]


# Clean the score variable ------------------------------------------------

# Create abondonned indicator
data_transformed[, ind_aband := FALSE]
data_transformed[grep("RET", score), ind_aband := TRUE]
data_transformed[, score := gsub("RET", "", score)]

data_transformed[, sapply(1:5, function(x) paste0("score_set_", x)) := tstrsplit(score, " ", fixed = TRUE)]
# For the moment, I keep the points made by the loser (for both winner and loser) in the tie break col ...
data_transformed[grep(")", score_set_1), tb_set_1 := substr(score_set_1, 5, 5)]
data_transformed[grep(")", score_set_2), tb_set_2 := substr(score_set_2, 5, 5)]
data_transformed[grep(")", score_set_3), tb_set_3 := substr(score_set_3, 5, 5)]
data_transformed[grep(")", score_set_4), tb_set_4 := substr(score_set_4, 5, 5)]
data_transformed[grep(")", score_set_5), tb_set_5 := substr(score_set_5, 5, 5)]

data_transformed[win == 1, score_set_1 := substr(score_set_1, 1, 1)]
data_transformed[win == 1, score_set_2 := substr(score_set_2, 1, 1)]
data_transformed[win == 1, score_set_3 := substr(score_set_3, 1, 1)]
data_transformed[win == 1, score_set_4 := substr(score_set_4, 1, 1)]
data_transformed[win == 1, score_set_5 := substr(score_set_5, 1, 1)]

data_transformed[win == 0, score_set_1 := substr(score_set_1, 3, 3)]
data_transformed[win == 0, score_set_2 := substr(score_set_2, 3, 3)]
data_transformed[win == 0, score_set_3 := substr(score_set_3, 3, 3)]
data_transformed[win == 0, score_set_4 := substr(score_set_4, 3, 3)]
data_transformed[win == 0, score_set_5 := substr(score_set_5, 3, 3)]

# Remove old score variable
data_transformed[, score := NULL]
rm(data_transformed_loser, data_transformed_winner)

