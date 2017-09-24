# Title: SummariseData
# Date: 23 septembre 2017
# Author: Stephane Caron
# Subject: We wanna summarise data per player for each of their game


# Load packages -----------------------------------------------------------

library(tidyverse)
library(data.table)


# Compute number of days since first match (in the database)
data_summarised <- data_transformed[, tourney_date_num := as.numeric(tourney_date - min(tourney_date))]

# Test moving average just on winner -- needs an update with new data structure**
subdata <- data_summarised[name == "Milos Raonic", .(svpt_sum = sum(svpt, na.rm = TRUE), svpt_count = .N), by = tourney_date_num]


data_summarised[tourney_dat_num - lag,, by = (name, tourney_date, match_num)]

subdata_range <- range(subdata$tourney_date_num)
y <- rep(0, subdata_range[2] - subdata_range[1] + 1)
y[subdata$tourney_date_num - subdata_range[1] + 1] <- subdata$svpt_sum

# y <- stats::filter(y ,rep(1,365), sides=1)
# Suppose we wanted average per game instead
w <- rep(1, subdata_range[2] - subdata_range[1] + 1)
w[subdata$tourney_date_num - subdata_range[1] + 1] <- subdata$w_svpt_count

fun <- function(n){stats::filter(y, rep(1/n,n), sides=1)}
fun(365)


