# Title: DataPreparation
# Date: 1er septembre 2017
# Author: Stéphane Caron
# Goal: We want to prepare the data for logistic regression


# 1. Load packages --------------------------------------------------------

library(data.table)
library(tidyverse)


# 2. Load data ------------------------------------------------------------

source("Modeling/Logistic/baseline.R")


# 3. Load function --------------------------------------------------------

source("Modeling/Logistic/momentumCalculator.R")


# On commence par calculer des statistiques cumulatives dans le march
data[, p1_cum_ace := momentumCalculator(P1Ace), by = match_id]
data[, p2_cum_ace := cumsum(P2Ace), by = match_id]
data[, p1_cum_winner := cumsum(P1Winner), by = match_id]
data[, p2_cum_winner := cumsum(P2Winner), by = match_id]
data[, p1_cum_df := cumsum(P1DoubleFault), by = match_id]
data[, p2_cum_df := cumsum(P2DoubleFault), by = match_id]
data[, p1_cum_unferr := cumsum(P1UnfErr), by = match_id]
data[, p2_cum_unferr := cumsum(P2UnfErr), by = match_id]
data[, p1_cum_bp := cumsum(P1BreakPoint), by = match_id]
data[, p2_cum_bp := cumsum(P2BreakPoint), by = match_id]
data[, p1_cum_bp_won := cumsum(P1BreakPointWon), by = match_id]
data[, p2_cum_bp_won := cumsum(P2BreakPointWon), by = match_id]

# Variables en lien avec les % de reussite et % de points gagnés sur chacun des services
# Je sais pas encore si j'ai les données par rapport à cela pour toutes les années, je vais l'exclure pour l'instant
# data[, p1_cum_first_in := cumsum(P1FirstSrvIn), by = match_id]
# data[, p2_cum_first_in := cumsum(P2FirstSrvIn), by = match_id]
# data[, p1_cum_first_won := cumsum(P1FirstSrvWon), by = match_id]
# data[, p2_cum_first_won := cumsum(P2FirstSrvWon), by = match_id]
# data[, p1_cum_second_in := cumsum(P1SecondSrvIn), by = match_id]
# data[, p2_cum_second_in := cumsum(P2SecondSrvIn), by = match_id]
# data[, p1_cum_second_won := cumsum(P1SecondSrvWon), by = match_id]
# data[, p2_cum_second_won := cumsum(P2SecondSrvWon), by = match_id]
# 
# data[, p1_serve_first := 0]
# data[PointServer == 1, p1_serve_first := 1]
# data[, p1_serve_second := 0]
# data[PointServer == 1 & ServeIndicator == 2, p1_serve_second := 1]
# data[, p1_cum_first := cumsum(p1_serve_first), by = match_id]
# data[, p1_cum_second := cumsum(p1_serve_second), by = match_id]
# 
# data[, p2_serve_first := 0]
# data[PointServer == 2, p2_serve_first := 1]
# data[, p2_serve_second := 0]
# data[PointServer == 2 & ServeIndicator == 2, p2_serve_second := 1]
# data[, p2_cum_first := cumsum(p2_serve_first), by = match_id]
# data[, p2_cum_second := cumsum(p2_serve_second), by = match_id]



# Summarize
data_modeling <- data[, 
                      .(set_no = SetNo, 
                        p1_games_won = P1GamesWon, 
                        p2_games_won = P2GamesWon,
                        point_no = PointNumber,
                        server = PointServer,
                        p1_score = P1Score,
                        p2_score = P2Score,
                        p1_cum_ace = p1_cum_ace,
                        p2_cum_ace = p2_cum_ace,
                        p1_cum_df = p1_cum_df,
                        p2_cum_df = p2_cum_df,
                        p1_cum_unferr = p1_cum_unferr,
                        p2_cum_unferr = p2_cum_unferr,
                        p1_bp_perc = (p1_cum_bp_won/p1_cum_bp),
                        p2_bp_perc = (p2_cum_bp_won/p2_cum_bp),
                        # p1_1st_in_perc = (p1_cum_first_in/p1_cum_first),
                        # p1_1st_won_perc = (p1_cum_first_won/p1_cum_first),
                        # p2_1st_in_perc = (p2_cum_first_in/p2_cum_first),
                        # p2_1st_won_perc = (p2_cum_first_won/p2_cum_first),
                        # p1_2nd_in_perc = (p1_cum_second_in/p1_cum_second),
                        # p1_2nd_won_perc = (p1_cum_second_won/p1_cum_second),
                        # p2_2nd_in_perc = (p2_cum_second_in/p2_cum_second),
                        # p2_2nd_won_perc = (p2_cum_second_won/p2_cum_second),
                        winner = PointWinner), 
                      by = .(match_id, elasped_time = ElapsedTime)]

