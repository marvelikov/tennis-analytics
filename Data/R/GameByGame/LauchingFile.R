# Title: LauchingFile
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: Lauching file to update game by game data

# I wanna build a launching file that will be used to update/clean/structure game by game tennis data ...

# Set wanted variables ----------------------------------------------------

start_year <- 2010
end_year <- 2017
aggregated_time_frame_days <- 365 # Unit = days ...


# Run needed scripts ------------------------------------------------------

# Import data
source("Data/R/GameByGame/ImportData.R")
data_raw <- import_games(year_from = start_year, year_to = end_year)
fwrite(data_raw, "Data/Raw/DataRawGameByGame.csv")

# Transform data
source("Data/R/GameByGame/TransformData.R")
fwrite(data_transformed, "Data/Cleaned/DataTransformed.csv")

# Summarise data
source("Data/R/GameByGame/SummariseData.R")
fwrite(data_summarised, "Data/Cleaned/DataSummarised.csv")

# Create variables
source("Data/R/GameByGame/CreateVariablesData.R")
fwrite(data_pre_modeling, "Data/Cleaned/DataPreModeling.csv")

# Structure data for modeling
source("Data/R/GameByGame/ModelingData.R")
fwrite(data_modeling, "Data/Cleaned/DataModeling.csv")
