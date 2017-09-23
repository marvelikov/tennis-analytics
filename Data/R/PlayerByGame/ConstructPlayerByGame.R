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
general_variables <- names(data)[-c(winner_ind, loser_ind)]


# ********** the best would be to order them as we wish there ********** #
# create a winner_variables with both winner_variables and general_variables
# by mixing them appropriately - do the same for loser.

data_winner <- subset(data, select = c(winner_variables, general_variables))
data_loser <- subset(data, select = c(loser_variables, general_variables))

# adjust names -- if vectors were created with nice ordering of the variables, watch out
# with "draw_size" (the w_ can be erased by gsub). *OKAY* Changed it for loser instead..

new_names <- gsub(loser_variables, pattern = "l_", replacement = "")
new_names <- gsub(new_names, pattern = "loser_", replacement = "")

names(data_winner) <- c(new_names, general_variables)
names(data_loser) <- c(new_names, general_variables)

data <- rbindlist(l = list(data_winner, data_loser), use.names = TRUE, fill = TRUE)




# But should we create that from raw or data_G? If we do it from data_G, we need
# tourney_id or tourney_name to construct the more general match_id

match_id <- unique(paste(raw$tourney_id, raw$tourney_id, sep = ""))
data_P <- data.table(match_id = rep(unique(match_id), each = 2))

