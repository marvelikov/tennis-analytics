# Title: SummariseData
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: We wanna summarise data per player for each of their game


# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)


# Load transformed data ---------------------------------------------------

data_transformed <- fread("Data/Cleaned/DataTransformed.csv")
data_transformed$tourney_date <- ymd(data_transformed$tourney_date)

# Summarise data  ---------------------------------------------------------

# Here we define the columns we need to create our variables with the timeframe (to summarised) associated to each columns 
variables_to_summarise <- list(
                            serve_point = list(var = "svpt", time_frame = 365), 
                            first_serve_in = list(var = "1stIn", time_frame = 365),
                            first_serve_won = list(var = "1stWon", time_frame = 365),
                            second_serve_won = list(var = "2ndWon", time_frame = 365), 
                            bp_faced = list(var = "bpFaced", time_frame = 365), 
                            bp_saved = list(var = "bpSaved", time_frame = 365), 
                            return_point_won = list(var = "rpt_won", time_frame = 365), 
                            return_point = list(var = "rpt", time_frame = 365), 
                            win = list(var = "win", time_frame = 365), 
                            loss = list(var = "loss", time_frame = 365), 
                            min_played = list(var = "minutes", time_frame = 3)
                            )

unique_tf <- unique(unlist(lapply(variables_to_summarise, function(x) x[["time_frame"]])))


# We start by finding each rows that met the conditions setted by the parameters (time frame) and then we'll use 
# back these indices to summarise our data by different variables
sapply(X = unique_tf, FUN = function(time) {
  col_name = paste0("targeted_rows_", time)
  data_transformed[, (col_name) := sapply(X = 1:nrow(data_transformed), FUN = function(i) {which(difftime(data_transformed$tourney_date[i], data_transformed$tourney_date, units = "days") < time & (difftime(data_transformed$tourney_date[i], data_transformed$tourney_date, units = "days") > 0 | (difftime(data_transformed$tourney_date[i], data_transformed$tourney_date, units = "days") == 0 & data_transformed$match_num[i] > data_transformed$match_num)) & data_transformed$name[i] == data_transformed$name)})]
})


# Summarise data given the time frame of the variable ---------------------

lapply(X = variables_to_summarise, function(x) {
  summarised_col_name <- paste0(x[["var"]], "_", x[["time_frame"]])
  col_to_summarised <- which(names(data_transformed) == x[["var"]])

  data_transformed[, (summarised_col_name) := sapply(1:nrow(data_transformed), FUN = function(i) {
    if (length(unlist(data_transformed[i, paste0("targeted_rows_", x[["time_frame"]]), with = FALSE])) == 0) {
      NA
    } else {
      sum(data_transformed[unlist(data_transformed[i, paste0("targeted_rows_", x[["time_frame"]]), with = FALSE]), x[["var"]], with = FALSE], na.rm = TRUE)
    }
  })]
})

# Copy the transformed data
data_summarised <- copy(data_transformed)

  