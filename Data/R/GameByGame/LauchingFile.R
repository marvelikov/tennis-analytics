# Title: LauchingFile
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: Lauching file to update game by game data

# I wanna build a launching file that will be used to update/clean/structure game by game tennis data ...

# Set wanted variables ----------------------------------------------------

start_year <- 2000
end_year <- 2016
split_data_into <- 4


# Run needed scripts ------------------------------------------------------

split_data_git <- function(data, split_into, filename) {
  start_row <- 1
  step <- nrow(data)/split_into
  for (i in 1:split_into) {
    data_temp <- data[start_row:(step * i),]
    start_row <- (step * i) + 1
    fwrite(data_temp, paste0(filename, "_", i, ".csv"))
  }
}

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
split_data_git(data = data_summarised, split_into = split_data_into, filename = "Data/Cleaned/DataSummarised")
summarise_time <- Sys.time()

# Create variables
source("Data/R/GameByGame/CreateVariablesData.R")
split_data_git(data = data_pre_modeling, split_into = split_data_into, filename = "Data/Cleaned/DataPreModeling")
variables_time <- Sys.time()

# Structure data for modeling
source("Data/R/GameByGame/ModelingData.R")
split_data_git(data = data_modeling, split_into = split_data_into, filename = "Data/Cleaned/DataModeling")
modeling_time <- Sys.time()

# Summary of execution time
int <- list(start_time, import_time, transform_time, summarise_time, variables_time, modeling_time)

lapply(1:(length(int)-1), function(i) difftime(int[[i+1]], int[[i]], units = "min"))
