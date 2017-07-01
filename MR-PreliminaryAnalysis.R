# Illustrating our 'Match Results and Betting Odds'data





# 0. Load packages --------------------------------------------------------

require(shiny)
require(dplyr)
require(DT)
require(shinydashboard)
require(data.table)
require(RCurl)
library(readxl)





# 1. Locally Import data ----------------------------------------------------------

#url <- getURL("https://raw.githubusercontent.com/samperochkin/tennis-analytics/master/Data/MR-BO-2016.xlsx")
#setwd("C:/Users/Samuel/Dropbox/Git Projects/tennis-analytics")

data_mr <- data.table(read_excel("C:/Users/Samuel/Dropbox/Git Projects/tennis-analytics/Data/MR-BO-2016.xlsx"))





# 2. Preliminary analysis -- data cleaning considerations

# There are some warnings concerning the presence of 'N/A' values
# They are found at [c(196,204,832,1387,1677,1696,1926),c(13,15)] and [1687,c(12,14)]
data_mr[c(196,204,832,1387,1677,1696,1926,1687),]

# All-in-all, the data is pretty clean... We can either try to fill the NA's



