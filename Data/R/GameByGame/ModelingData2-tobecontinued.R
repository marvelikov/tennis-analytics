# Title: SurfaceSplitExploration
# Date: 10 October 2017
# Author: Samuel Perreault
# Subject: This is a first step towards splitting some variables with respect to the surface


# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)
library(lubridate)


# Load transformed data ---------------------------------------------------

data_transformed <- fread("Data/Cleaned/DataTransformed.csv")
data_transformed <- fread("Data/R/GameByGame/data_transformed2.csv")
data_transformed$tourney_date <- ymd(data_transformed$tourney_date)

# Summarise data  ---------------------------------------------------------

# Here we define the columns we need to create our variables with the timeframe (to summarised) associated to each columns 
variables_to_summarise <- list(
  serve_point = list(var = "svpt", time_frame = 365, split = NULL), 
  first_serve_in = list(var = "1stIn", time_frame = 365, split = NULL),
  first_serve_won = list(var = "1stWon", time_frame = 365, split = NULL),
  second_serve_won = list(var = "2ndWon", time_frame = 365, split = NULL), 
  bp_faced = list(var = "bpFaced", time_frame = 365, split = NULL), 
  bp_saved = list(var = "bpSaved", time_frame = 365, split = NULL), 
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
  print(x[["var"]])
  col_to_summarised <- which(names(data_transformed) == x[["var"]])
  
  if(is.null(x[["split"]])) {
    split_grid <- cbind(data.table(var = x[["var"]]), data.table(time_frame = x[["time_frame"]]))
  } else {
    split_grid <- cbind(data.table(var = x[["var"]]), expand.grid(sapply(x[["split"]], function(split_var) {
      unique(data_transformed[, split_var, with = FALSE])
    }, USE.NAMES = FALSE)), data.table(time_frame = x[["time_frame"]])) 
  }
  
  sapply(1:nrow(split_grid), function(row) {
    data_transformed[, (paste(split_grid[row,], collapse = "_")) := sapply(1:nrow(data_transformed), FUN = function(i) {
      if (length(unlist(data_transformed[i, paste0("targeted_rows_", x[["time_frame"]]), with = FALSE])) == 0) {
        NA
      } else {
        text_condition <- paste0(sapply(x[["split"]], function(col) {
          paste0("[", col, "=='", split_grid[row, col], "']")
        }), collapse = "")
        text_eval <- paste0("sum(data_transformed[unlist(data_transformed[i, paste0('targeted_rows_', x[['time_frame']]), with = FALSE]), x[['var']], with = FALSE]", text_condition, ", na.rm = TRUE)")
        print(row)
        eval(parse(text = text_eval))
      }
    }
   )]
  })
})

# Copy the transformed data
data_summarised <- copy(data_transformed)

test <- variables_to_summarise[[9]]

split_grid <- cbind(data.table(var = test[["var"]]), expand.grid(sapply(test[["split"]], function(split_var) {
  unique(data_transformed[, split_var, with = FALSE])
}, USE.NAMES = FALSE)), data.table(time_frame = test[["time_frame"]])) 



#########
#########



# need to construct the new variables





# Creates variables -------------------------------------------------------


# Calculate tennis stats variables/ratios 

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

# Ratio no.7: 1st serve In percentage
data_summarised[, perc_1st_in := `1stIn_365`/svpt_365]

# Remove NaN
data_summarised[is.na(svpt_365), perc_1st_serve_won := 0]
data_summarised[is.na(svpt_365), perc_2nd_serve_won := 0]
data_summarised[is.na(rpt_365), perc_return_won := 0]
data_summarised[is.na(bpFaced_365) | bpFaced_365 == 0, perc_bp := 0]
data_summarised[is.na(win_365 + loss_365), perc_win := 0]
data_summarised[is.na(win_365 + loss_365), ave_pts_game := 0]
data_summarised[is.na(svpt_365), perc_1st_in := 0]

data_pre_modeling <- data_summarised






######
######







# Title: ModelingData
# Date: 27 septembre 2017
# Author: Stephane Caron
# Subject: Modeling often requires to structure the data in 1 line per game


# Load packages -----------------------------------------------------------

library(data.table)
library(tidyverse)

# Import summarised data --------------------------------------------------

data_pre_modeling <- fread("Data/Cleaned/DataPreModeling.csv")


# Transform data for modeling purposes ------------------------------------

cols_modeling <- c("tourney_date", "match_num", "tourney_name", "name", "perc_1st_serve_won", "perc_2nd_serve_won", "perc_return_won", "perc_bp", "perc_win", "ave_pts_game", "perc_1st_in", "surface", "win")
data_pre_modeling <- data_pre_modeling[, cols_modeling, with = FALSE]

data_pre_modeling_winner <- data_pre_modeling[win == 1,]
new_names_win <- sapply(1:length(data_pre_modeling_winner), function(x) {
  if (names(data_pre_modeling_winner)[x] %in% c("tourney_date", "tourney_name" , "match_num", "surface")) {
    names(data_pre_modeling_winner)[x]
  } else {
    names(data_pre_modeling_winner)[x] <- paste0("w_", names(data_pre_modeling_winner)[x])
  }
})
names(data_pre_modeling_winner) <- new_names_win

data_pre_modeling_looser <- data_pre_modeling[win == 0,]
new_names_loss <- sapply(1:length(data_pre_modeling_looser), function(x) {
  if (names(data_pre_modeling_looser)[x] %in% c("tourney_date", "tourney_name" , "match_num", "surface")) {
    names(data_pre_modeling_looser)[x]
  } else {
    names(data_pre_modeling_looser)[x] <- paste0("l_", names(data_pre_modeling_looser)[x])
  }
})
names(data_pre_modeling_looser) <- new_names_loss

# Rejoin both tables together
setkey(data_pre_modeling_winner, tourney_date, tourney_name, match_num, surface)
setkey(data_pre_modeling_looser, tourney_date, tourney_name, match_num, surface)
data_pre_modeling <- data_pre_modeling_winner[data_pre_modeling_looser]
rm(data_pre_modeling_winner, data_pre_modeling_looser)

data_modeling <- data_pre_modeling %>% 
  select(
    -c(w_win, l_win)
  ) %>% 
  spread(
    surface,
    surface,
    fill = 0
  ) %>% 
  mutate(
    clay = ifelse(Clay != 0, 1, 0),
    hard = ifelse(Hard != 0 ,1, 0),
    grass = ifelse(Grass != 0, 1, 0),
    p1_win = 1
  ) %>% 
  select(
    -c(Hard, Clay, Grass)
  ) %>% 
  rename(
    p1 = w_name,
    p2 = l_name
  )

data_modeling <- data.table(data_modeling)

new_names <- gsub("w_", "p1_", colnames(data_modeling))
new_names <- gsub("l_", "p2_", new_names)
colnames(data_modeling) <- new_names

# Swap the columns
source("Data/R/GameByGame/SwapColumns.R")
data_modeling <- swap_cols(data_modeling) %>% select(-tourney_name)

# Save the data
fwrite(data_modeling, "Data/Cleaned/DataModeling.csv")
