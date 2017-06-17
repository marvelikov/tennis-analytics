# Shiny application for filtering tennis ranking datset


# 0. Load packages --------------------------------------------------------

require(shiny)
require(shinydashboard)
require(data.table)
require(RCurl)

# 1. Import data ----------------------------------------------------------

url <- getURL("https://raw.githubusercontent.com/samperochkin/tennis-analytics/master/Data/DataRanking.csv")
DataRanking <- read.csv(textConnection(url))

# 2. UI  ------------------------------------------------------------------


# 3. Server ---------------------------------------------------------------


