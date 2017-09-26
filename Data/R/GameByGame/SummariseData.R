# Title: SummariseData
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: We wanna summarise data per player for each of their game


# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)


# Load transformed data ---------------------------------------------------

data_transformed <- fread("Data/Cleaned/DataTransformed.csv")


# Summarise data  ---------------------------------------------------------

# We start by finding each rows that met the conditions setted by the parameters (time frame) and then we'll use 
# back these indices to summarise our data by different variables
data_transformed$targeted_rows <- sapply(X = 1:nrow(data_transformed), FUN = function(x) {which(difftime(data_transformed$tourney_date[x], data_transformed$tourney_date, units = "days") < aggregated_time_frame_days & (difftime(data_transformed$tourney_date[x], data_transformed$tourney_date, units = "days") > 0 | (difftime(data_transformed$tourney_date[x], data_transformed$tourney_date, units = "days") == 0 & data_transformed$match_num[x] > data_transformed$match_num)) & data_transformed$name[x] == data_transformed$name)})

# We define here a function that loop over the different variables that we want to summarise and we crunch them 
# by the rows determined by the variable "targetet_rows"

summarise_variable <- function(data, colname, nb_days = 365, fill_missing = NA) {
  summarised_col_name <- paste0(colname, "_", nb_days)
  col_number <- which(names(data) == colname)
  data[, (summarised_col_name) := sapply(X = 1:nrow(data), FUN = function(x) {
    if (length(unlist(data$targeted_rows[x])) == 0) {
      NA
    } else {
      sum(data[unlist(data$targeted_rows[x]), col_number, with = FALSE], na.rm = TRUE)
    }
  })]
}

# Define col to summarise
col_summarise <- c("svpt", "1stIn", "1stWon", "2ndWon", "bpFaced", "bpSaved", "rpt_won", "rpt", "win", "loss")

# Copy the transformed data
data_summarised <- copy(data_transformed)

# Loop over different variables to summarise
sapply(col_summarise, function(x) summarise_variable(data_summarised, x, nb_days = aggregated_time_frame_days))



# Calculate tennis stats variables/ratios ---------------------------------

# Ratio no.1: Winning percentage on 1st serve
data_summarised[, perc_1st_serve_won := `1stWon_365`/svpt_365]

# Ratio no.2: Winning percentage on 2nd serve
data_summarised[, perc_2nd_serve_won := `2ndWon_365`/svpt_365]

# Ratio no.3: Winning percentage on return serve 
data_summarised[, perc_return_won := rpt_won_365/rpt_365]

# Ratio no.4: Winning percentage on break point
data_summarised[, perc_bp := bpSaved_365/bpFaced_365]

# Ratio no.5: Winning percentage of match
data_summarised[, perc_win := win_365/(win_365 + loss_365)]

# Ratio no.6: Average point per game
data_summarised[, ave_pts_game := (`1stWon_365` + `2ndWon_365` + rpt_won_365)/(win_365 + loss_365)]

# Remove NaN
data_summarised[is.na(svpt_365), perc_1st_serve_won := 0]
data_summarised[is.na(rpt_365), perc_2nd_serve_won := 0]
data_summarised[is.na(bpFaced_365) | bpFaced_365 == 0, perc_bp := 0]
data_summarised[is.na(win_365 + loss_365), perc_win := 0]
data_summarised[is.na(win_365 + loss_365), ave_pts_game := 0]

  