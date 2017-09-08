# Title: CleaningFunctions
# Date: 9 Septembre 2017
# Author: Stephane Caron
# Subject: Define functions used to clean the data

# Il me reste des trucs a faire pour completer la fonction ...

sum_last_days <- function(x, nb_days) {
  
  w_stat <- data$w_1stIn[which(data$winner_name == data$winner_name[8100] & difftime(time1 = data$tourney_date[8100], time2 = data$tourney_date, units = "days") <= nb_days & difftime(time1 = data$tourney_date[8100], time2 = data$tourney_date, units = "days") > 0)]
  l_stat <- data$l_1stIn[which(data$loser_name == data$winner_name[8100] & difftime(time1 = data$tourney_date[8100], time2 = data$tourney_date, units = "days") <= nb_days & difftime(time1 = data$tourney_date[8100], time2 = data$tourney_date, units = "days") > 0)]
  
  w_stat + l_stat
  
}
