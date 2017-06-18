# Shiny application for filtering tennis ranking datset


# 0. Load packages --------------------------------------------------------

require(shiny)
require(dplyr)
require(shinydashboard)
require(data.table)
require(RCurl)

# 1. Import data ----------------------------------------------------------

url <- getURL("https://raw.githubusercontent.com/samperochkin/tennis-analytics/master/Data/DataRanking.csv")
data_ranking <- data.table(read.csv(textConnection(url)))
data_ranking$ranking_date <- as.Date(data_ranking$ranking_date)

# 2. Define filtering function --------------------------------------------

filtering <- function(age_input, ranking_input, stat_input){
  data_filtered <- copy(data_ranking)
  if (stat_input == "entry"){
    data_filtered <- data_filtered[age <= age_input & rank <= ranking_input, .(ranking_date = min(ranking_date)), id]
  } else {
    data_filtered <- data_filtered[age <= age_input & rank <= ranking_input, .(rank = min(rank)), .(id)]
    data_filtered <- data.table(left_join(data_filtered, data_ranking, by = c("id", "rank")))
    data_filtered <- data_filtered[, .(ranking_date = min(ranking_date)), id]
  }
  data_filtered <- data.table(left_join(data_filtered, data_ranking, by = c("id", "ranking_date")))
}

# 3. UI  ------------------------------------------------------------------


# 4. Server ---------------------------------------------------------------


