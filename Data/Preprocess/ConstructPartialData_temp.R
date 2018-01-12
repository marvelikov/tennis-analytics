# Description:  We construct the data matrix with empty spots for the players' representation.


################
# Data structure ----------------------------------------------------------
################
############################
## Data from JackSackmann ##
############################



###############
# Load packages -----------------------------------------------------------
###############

library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
library(dplyr)
library(mltools)


############
# 1st split ---------------------------------------------------------------
############

# Load data
data <- fread("Data/Cleaned/DataTransformed.csv")
data <- as.data.table(arrange(data, tourney_date, tourney_id, match_num))


# Define subsets of variables
p_info_var <- c("name","id","seed","entry","hand","ht","ioc","age","rank","rank_points")
m_info_var <- c("round","tourney_name","tourney_date","tourney_id","match_num","surface",
                "draw_size","tourney_level","best_of")

p_stats_var <- c("ace","df","svpt","1stIn","1stWon","2ndWon","SvGms","bpSaved","bpFaced",
             "rpt_won","rpt","ind_aband","retired","score_set_1","score_set_2","score_set_3",
             "score_set_4","score_set_5")
m_stats_var <- c("minutes","nb_sets")


p_info <- data[,p_info_var, with = FALSE]
p_stats <- data[,p_stats_var, with = FALSE]

m_info <- data[seq(1,nrow(data),2), m_info_var, with = FALSE]
m_stats <- data[seq(1,nrow(data),2), m_stats_var, with = FALSE]



###################
# Work on variables -------------------------------------------------------
###################



#### player's info ####

# we don't need the player's name
p_info[, name := NULL]


# NA in seed should be replaced by a high value
#> max(p_info$seed, na.rm = TRUE)
#[1] 33
# Let us put 100
p_info$seed[is.na(p_info$seed)] <- 100


# The entry variable
#> unique(p_info$entry)
#[1] ""   "WC" "Q"  "LL" "PR" "S"  "SE"
# has the following meaning (from Wikipedia)
# WC : Wild Card -- Player allowed to play in a tournament, even if his or her rank is not adequate
#                    or he or she does not register in time. Typically a few places in the draw are 
#                    reserved for wild cards, which may be for local players who do not gain direct 
#                    acceptance or for players who are just outside the ranking required to gain direct 
#                    acceptance. Wild cards may also be given to players whose ranking has dropped due to
#                    a long-term injury.
# Q : Qualifiers -- Player who reaches the tournament's main draw by competing in a pre-tournament 
#                    qualifying competition, rather than automatically by virtue of his/her world ranking,
#                    by being awarded a wildcard, or other exemption.
# SE : Special Exempt -- Players who are unable to appear in a tournament's qualifying draw because they 
#                        are still competing in a previous tournament can be awarded a spot in the main 
#                        draw by special exempt.
# LL : Lucky Loser -- Highest-ranked player to lose in the final round of qualifying for a tournament, 
#                      but still ends up qualifying because of a sudden withdrawal by one of the players
#                      already in the main draw. In Grand Slam events, one of the four highest-ranked losers
#                      in the final qualifying round is randomly picked as the lucky loser.
# PR : Protected Ranking -- Players injured for a minimum of six months can ask for
#                            a protected ranking, which is based on his or her average
#                            ranking during the first three months of his or her injury.
#                            The player can use his or her protected ranking to enter tournaments'
#                            main draws or qualifying competitions when coming back from injury.
# S : ???
# Anyways, we do not have enough of S. Same for SE.
# It seems we should encode the rest in different variables.
p_info[entry == "",]$entry <- "R"
p_info[, entry := as.factor(entry)]
p_info <- one_hot(p_info, "entry")
# Let us put the S and SE entry as regulars... Right?
p_info[, entry_R := entry_R + entry_S + entry_SE]
p_info[, entry_S := NULL]
p_info[, entry_SE := NULL]


# hand
p_info[,hand := as.factor(hand)]
p_info <- one_hot(p_info, "hand")

# ioc -- Here, maybe we should exaplnd this even more in selecting the latitude and longitude of city of birth!
p_info[,ioc := as.factor(ioc)]
p_info <- one_hot(p_info, "ioc")




#### player's stats ####

p_stats[,ind_aband := NULL]
p_stats[,retired := as.numeric(retired)]

p_stats[is.na(score_set_1),]$score_set_1 <- 0
p_stats[is.na(score_set_2),]$score_set_2 <- 0
p_stats[is.na(score_set_3),]$score_set_3 <- 0
p_stats[is.na(score_set_4),]$score_set_4 <- 0
p_stats[is.na(score_set_5),]$score_set_5 <- 0

# Create sub-data
p1_info <- p_info[seq(1,nrow(p_info),2),]
p2_info <- p_info[seq(2,nrow(p_info),2),]

p1_stats <- p_stats[seq(1,nrow(p_stats),2),]
p2_stats <- p_stats[seq(2,nrow(p_stats),2),]




#### matches' info 

# Keep match_id for... id purposes


# New tourney_id
tourney_id2 <- unlist(strsplit(x = m_info[, tourney_id], split = "-"))[seq(2,2*nrow(m_info),2)]
# tourney_id2 from tourney_id -- removing year
m_info[, tourney_id2 := tourney_id2]
m_info[, tourney_id := NULL]


# tourney_name removed -- should try long-lat
m_info[, tourney_name := NULL]



# Identify Round Robin ("RR") tourney
RR_tourney_ids <- unique(m_info[round == "RR",]$tourney_id2)
m_info[, RR_tourney := 0]
m_info[tourney_id2 %in% RR_tourney_ids, RR_tourney := 1]

# round, in order
ordered_char <- c("F", "SF", "R16", "R32", "R64", "R128", "RR", "QF")
for(i in seq_along(ordered_char)){
  m_info[round == ordered_char[i], round_ord := i]
}
m_info[,round := NULL]


# one_hot surface
m_info[,surface := as.factor(surface)]
m_info <- one_hot(m_info,"surface")


# tourney_level
# G: Grand Slam
# M: Masters
# F: Finals
# A: ATP Series (250 and 500)
# C: Challenger
ordered_char <- c("G", "F", "M", "A", "C")
for(i in seq_along(ordered_char)){
  m_info[tourney_level == ordered_char[i], tourney_level_ord := i]
}
m_info[,tourney_level := NULL]


# split date in three
m_info[, tourney_date := as.Date(tourney_date)]
m_info[, year := year(tourney_date)]
m_info[, month := month(tourney_date)]
m_info[, day := day(tourney_date)]
m_info[, tourney_date := NULL]
