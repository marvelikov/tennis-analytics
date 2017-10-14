# Title: SummariseData
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: We wanna summarise data per player for each of their game


# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)


# Load transformed data ---------------------------------------------------

data_transformed <- fread("Data/Cleaned/DataTransformed.csv", stringsAsFactors = FALSE)
data_transformed$tourney_date <- ymd(data_transformed$tourney_date)

# Summarise data  ---------------------------------------------------------

# Here we define the columns we need to create our variables with the timeframe (to summarised) associated to each columns 
variables_to_summarise <- list(
                            serve_point = list(var = "svpt", time_frame = 365, split = NULL), 
                            first_serve_in = list(var = "1stIn", time_frame = 365, split = NULL),
                            first_serve_won = list(var = "1stWon", time_frame = 365, split = NULL),
                            second_serve_won = list(var = "2ndWon", time_frame = 365, split = NULL), 
                            bp_faced = list(var = "bpFaced", time_frame = 365, split = NULL), 
                            bp_saved = list(var = "bpSaved", time_frame = 365), 
                            return_point_won = list(var = "rpt_won", time_frame = 365, split = NULL), 
                            return_point = list(var = "rpt", time_frame = 365, split = NULL), 
                            win = list(var = "win", time_frame = 365, split = "surface"), 
                            loss = list(var = "loss", time_frame = 365, split = "surface")
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
  
  # We begin by looking if it's a variable to split
  if(is.null(x[["split"]])) {
    split_grid <- cbind(data.table(var = x[["var"]]), data.table(time_frame = x[["time_frame"]]))
  } else {
    split_grid <- cbind(data.table(var = x[["var"]]), expand.grid(sapply(x[["split"]], function(split_var) {
      unique(data_transformed[, split_var, with = FALSE])
    }, USE.NAMES = FALSE), stringsAsFactors = FALSE), data.table(time_frame = x[["time_frame"]])) 
  }
  
  # We calculate the new summarised variable of the given time frame
  sapply(1:nrow(split_grid), function(row) {
    print(paste(unlist(split_grid[row,]), collapse = "_"))
    data_transformed[, (paste(unlist(split_grid[row,]), collapse = "_")) := sapply(1:nrow(data_transformed), FUN = function(i) {
      if (length(unlist(data_transformed[i, paste0("targeted_rows_", x[["time_frame"]]), with = FALSE])) == 0) {
        0 # For the moment I fill missing values with 0 to make it easy to build variables after. In fact, it would probably be better to fill with NAs here to have a indication of real missing values
      } else {
        text_condition <- paste0(sapply(x[["split"]], function(col) {
          paste0(col, "=='", split_grid[row, col, with = FALSE], "'")
        }), collapse = "")
        text_eval <- paste0("sum(unlist(data_transformed[unlist(data_transformed[i, paste0('targeted_rows_', x[['time_frame']]), with = FALSE]), ][", text_condition, ", x[['var']], with = FALSE]), na.rm = TRUE)")
        eval(parse(text = text_eval))
      }
    })]
  })
})

# Copy the transformed data
data_summarised <- copy(data_transformed)

  