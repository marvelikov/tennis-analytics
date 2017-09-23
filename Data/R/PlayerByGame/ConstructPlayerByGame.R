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
library(lubridate)

# Import data -------------------------------------------------------------

raw <- fread("Data/Raw/data_game_by_game.csv")
data <- raw[-grep(pattern = "Davis", x = raw$tourney_name),]
data$tourney_date <- ymd(data$tourney_date)


#### Rearrange the data into two lines per game ####

# get winner/loser variable names
winner_ind <- c(grep(pattern = "w_", x = names(data)), grep(pattern = "winner", x = names(data)))
winner_ind <- winner_ind[-1]

loser_ind <- c(grep(pattern = "l_", x = names(data)), grep(pattern = "loser", x = names(data)))

winner_variables <- names(data)[winner_ind]
loser_variables <- names(data)[loser_ind]

# put the names first...
winner_variables <- c("winner_name", winner_variables[-grep(pattern = "name", x = winner_variables)])
loser_variables <- c("loser_name", loser_variables[-grep(pattern = "name", x = loser_variables)])

# get general variables [tourney_id and match_num will be our id]
# watch out here for errors if changes are made to raww data...
general_variables <- names(data)[-c(winner_ind, loser_ind)][c(1,7,(1:11)[-c(1,7)])]
rm("winner_ind", "loser_ind")

new_names <- gsub(loser_variables, pattern = "l_", replacement = "")
new_names <- gsub(new_names, pattern = "loser_", replacement = "")

winner_variables <- c(winner_variables[1], general_variables[1:2], winner_variables[-1], general_variables[-(1:2)])
loser_variables <- c(loser_variables[1], general_variables[1:2], loser_variables[-1], general_variables[-(1:2)])
new_names <- c(new_names[1], general_variables[1:2], new_names[-1], general_variables[-(1:2)])

data_winner <- subset(data, select = winner_variables)
data_loser <- subset(data, select = loser_variables)

names(data_winner) <- new_names
names(data_loser) <- new_names

data_winner[, win := rep(1, nrow(data_winner))]
data_loser[, win := rep(0, nrow(data_winner))]

data <- rbindlist(l = list(data_winner, data_loser), use.names = TRUE, fill = TRUE)

fwrite(data, "Data/Cleaned/PlayerOrientedData.csv")
