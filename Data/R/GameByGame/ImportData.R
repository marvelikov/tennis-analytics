# Title: ImportGameByGame
# Date: 5 Aout 2017
# Author: Stephane Caron  
# Subject: Extract ATP game data from GitHub JackSackmann

# 0. Load packages --------------------------------------------------------

library(tidyverse)
library(RCurl)
library(data.table)
library(readr)


# 1. Import data ----------------------------------------------------------

# Define a function to import data from given year to another by using data stored in JackSackmann GitHub repo.

import_games <- function(year_from, year_to){
  i <- 1
  data_x <- list(NA)
  for (year in (year_from:year_to)){
    x <- getURL(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_", year, ".csv"))
    data_x[[i]] <- fread(x, header = TRUE, sep = ",")
    # data_x[[i]] <- read.csv(textConnection(x))
    i <- i + 1
  }
  data_games <- do.call(rbind, data_x)
  # data_games <- rbindlist(data_x, use.names = TRUE, fill = TRUE)
}
