# Title: ImportPointByPointSlam
# Date: 26 Aout 2017
# Author: Stephane Caron  
# Subject: Extract ATP point by point data from JackSackmann's GitHub repo

# 0. Load packages --------------------------------------------------------

library(tidyverse)
library(RCurl)
library(data.table)
library(readr)


# 1. Define grand slam tournement -----------------------------------------

tournement <- c("ausopen", "frenchopen", "wimbledon", "usopen")
year <- seq(2011, 2016, 1)

grid <- expand.grid(year, tournement)
grand_slam <- c(sapply(1:nrow(grid), function(i) {paste0(grid$Var1[i], "-", grid$Var2[i])}))


# 2. Define function extract data  ----------------------------------------

import_point_by_point <- function(x, file){
  i <- 1
  data_x <- list(NA)
  for (link in x){
    import <- getURL(paste0("https://raw.githubusercontent.com/JeffSackmann/tennis_slam_pointbypoint/master/", link, "-", file, ".csv"))
    data_x[[i]] <- data.table(read.csv(textConnection(import)))
    i <- i + 1
  }
  data_binded <- rbindlist(data_x, fill = TRUE, use.names = FALSE)
}


# 3. Extract and save data ------------------------------------------------

data_pts_by_pts <- import_point_by_point(x = grand_slam, file = "points")
data_slam_matches <- import_point_by_point(x = grand_slam, file = "matches")

save(data_pts_by_pts, file = "Data/Raw/pts_by_pts.RData")
save(data_slam_matches, file = "Data/Raw/data_slam_matches.RData")

