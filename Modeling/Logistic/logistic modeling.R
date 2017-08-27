# Say we want to predict a given match... Let us do it with an example

# 0. Load packages --------------------------------------------------------

require(readr)
require(lubridate)
require(data.table)
require(readr)
require(readxl)

# 1. Import data ----------------------------------------------------------

load("Data/Raw/pts_by_pts.RData")
data <- data_pts_by_pts
rm("data_pts_by_pts")
summary(data)

load("Data/Raw/data_slam_matches.RData")
data.matches <- data_slam_matches
rm("data_slam_matches")
summary(data.matches)



