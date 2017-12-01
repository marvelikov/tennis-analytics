# Description:  We construct the data matrix with empty spots for the players' representation.


################
# Data structure ----------------------------------------------------------
################
############################
## Data from JackSackmann ##
############################


# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)

# Import raw data ---------------------------------------------------------

data_raw <- fread("Data/Raw/DataRawGameByGame.csv")
data <- data_raw[-grep(pattern = "Davis", x = data_raw$tourney_name),]
data <- data[-grep(pattern = "Olympics", x = data$tourney_name),]
data$tourney_date <- ymd(data$tourney_date)




# Make sure we have a well-ordered data.table -----------------------------

data <- arrange(data, tourney_date, tourney_id, match_num)



# Get info of players, stats of players and match info

info_p1 <- data %>% select(starts_with("winner"))
info_p2 <- data %>% select(starts_with("loser"))

stats_p1 <- data %>% select(starts_with("w_"))
stats_p2 <- data %>% select(starts_with("l_"))


#data_match <- data %>% select(-c(starts_with("winner"),starts_with("loser"),starts_with("w_"),starts_with("l_")))
#data_match

info_match <- data %>% select(c(starts_with("tourney"), surface, draw_size, best_of, round))
stats_match <- data %>% select(minutes, score)


# ****************NOTE******************
# Might need to divide varying info_p and fixed info_p...



#######################
# Individual treatments ---------------------------------------------------
#######################

# Here we should treat each variable seperately, selecting and formatting them as wished

## SOME OF THIS COULD BE DONE ON data_one.. (Should?)
info_p1 <- as.data.table(info_p1)
info_p2 <- as.data.table(info_p2)

# winner/loser_seed -------------------------------------------------------
info_p1[is.na(winner_seed), winner_seed := 0]
info_p2[is.na(loser_seed), loser_seed := 0]


# winner/loser_entry ------------------------------------------------------

# we want the natural order of
#> info_p2[,unique(loser_entry)]
#[1] ""   "Q"  "WC" "LL" "PR" "S"  "SE"
# given by [TO BE DETERMINED!]
info_p1[, winner_entry := sapply(info_p1$winner_entry,function(en){which.max(en == c("","WC","Q","LL","PR","S","SE"))})]
info_p2[, loser_entry := sapply(info_p2$loser_entry,function(en){which.max(en == c("","WC","Q","LL","PR","S","SE"))})]



name_factors <- unique(c(info_p1$winner_name,info_p2$loser_name))
info_p1[, winner_name := factor(winner_name, levels = name_factors)]
info_p2[, loser_name := factor(loser_name, levels = name_factors)]
# Preferably, we would us data_two?


# I prefer to focus on an actual frame for constructing the data from the processed variables.
# Anyways, we need to build something that is flexible in its input, so that we can re-use it...



# At the end we have a bunch of numeric matrices

# We could think of using a two-line data.table too: easier to get overall info

data_one <- data
data_two <- fread("Data/Cleaned/DataTransformed.csv")

# Missing player_id. Here is a cheap trick...
data_two[, player_id := as.numeric(factor(name))]


###################
# Loop construction -------------------------------------------------------
###################




# a list of the players' last game

# player_id <- unique(data_two$player_id)
#last_stats <- replicate(expr = rep(0, 2*ncol(stats_p1)), n = length(player_id))


# match_id <- unique(data_one$match_id) # Should be sorted
#for(m_id in match_id){
    
  # We want the infos and store the stats in "last"  
  
  #last_stats_p1 <- c(last_stats[[p1_id]],last_stats[[p2_id]])
  #last_stats_p2 <- c(last_stats[[p2_id]],last_stats[[p1_id]])

  #last_stats[[p1_id]] <- c(stats_p1[m_id,],stats_p2[m_id,])
  #last_stats[[p2_id]] <- c(stats_p2[m_id,],stats_p1[m_id,])


  # We leave space for the representation of their last opponents
  
  #last_opp_rep <- 10 # Need to adjust!
  #new_line <- c(info_p1,last_p1, last_opp_rep,info_p2,last_p2,info_match[m_id])

#} 
  





