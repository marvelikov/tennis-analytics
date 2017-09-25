# Title: LauchingFile
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: Lauching file to update game by game data

# I wanna build a lauching file that will be used to update/clean/structure game by game tennis data ...

# Set wanted variables ----------------------------------------------------

start_year <- 2010
end_year <- 2017
aggregated_time_frame_days <- 365 # Unit = days ...


# Run needed scripts ------------------------------------------------------

source("Data/R/GameByGame/ImportData.R")
source("Data/R/GameByGame/")
source("Data/R/GameByGame/SummariseData.R")
