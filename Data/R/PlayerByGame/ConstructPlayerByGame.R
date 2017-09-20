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
data_G <- fread("Data/Cleaned/DataModeling.csv")

# But should we create that from raw or data_G? If we do it from data_G, we need
# tourney_id or tourney_name to construct the more general match_id

match_id <- unique(paste(raw$tourney_id, raw$tourney_id, sep = ""))
data_P <- data.table(match_id = rep(unique(match_id), each = 2))

