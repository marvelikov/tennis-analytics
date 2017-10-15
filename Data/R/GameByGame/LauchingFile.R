# Title: LauchingFile
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: Lauching file to update game by game data

# I wanna build a launching file that will be used to update/clean/structure game by game tennis data ...

# Set wanted variables ----------------------------------------------------

start_year <- 2000
end_year <- 2016
aggregated_time_frame_days <- 365 # Unit = days ...


# Run needed scripts ------------------------------------------------------

start_time <- Sys.time()
# Import data
source("Data/R/GameByGame/ImportData.R")
data_raw <- import_games(year_from = start_year, year_to = end_year)
fwrite(data_raw, "Data/Raw/DataRawGameByGame.csv")
import_time <- Sys.time()

# Transform data
source("Data/R/GameByGame/TransformData.R")
fwrite(data_transformed, "Data/Cleaned/DataTransformed.csv")
transform_time <- Sys.time()

# Summarise data
source("Data/R/GameByGame/SummariseData.R")
fwrite(data_summarised, "Data/Cleaned/DataSummarised.csv")
summarise_time <- Sys.time()

# Create variables
source("Data/R/GameByGame/CreateVariablesData.R")
fwrite(data_pre_modeling, "Data/Cleaned/DataPreModeling.csv")
variables_time <- Sys.time()

# Structure data for modeling
source("Data/R/GameByGame/ModelingData.R")
fwrite(data_modeling, "Data/Cleaned/DataModeling.csv")
modeling_time <- Sys.time()

# Summary of execution time
int <- list(start_time, import_time, transform_time, summarise_time, variables_time, modeling_time)

lapply(1:(length(int)-1), function(i) difftime(int[[i+1]], int[[i]], units = "min"))
