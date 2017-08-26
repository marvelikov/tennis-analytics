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
    data_x[[i]] <- read.csv(textConnection(x))
    i <-i + 1
  }
  data_games <- do.call(rbind, data_x)
}

data <- import_games(year_from = 2010, year_to = 2017)



# 2. Point by point data --------------------------------------------------

tournement <- c("ausopen", "frenchopen", "wimbledon", "usopen")
year <- seq(2011, 2016, 1)

grid <- expand.grid(year, tournement)
grand_slam <- c(sapply(1:nrow(grid), function(i) {paste0(grid$Var1[i], "-", grid$Var2[i])}))

import_point_by_point <- function(x){
  i <- 1
  data_x <- list(NA)
  for (link in x){
    import <- getURL(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/", link, "-points.csv"))
    data_x[[i]] <- data.table(read.csv(textConnection(import)))
    i <- i + 1
  }
  data_point_by_point <- rbindlist(data_x, fill = TRUE, use.names = FALSE)
}

data_pts_by_pts <- import_point_by_point(x = grand_slam)

save(data_pts_by_pts, file = "Data/Raw/pts_by_pts.RData")

import_matches <- function(x){
  i <- 1
  data_x <- list(NA)
  for (link in x){
    import <- getURL(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/", link, "-matches.csv"))
    data_x[[i]] <- data.table(read.csv(textConnection(import)))
    i <-i + 1
  }
  data_point_by_point <- rbindlist(data_x)
}

data_matches <- import_point_by_point(x = grand_slam[2])


import <- getURL(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/", grand_slam[1]))
test <- read.csv(textConnection(import))
