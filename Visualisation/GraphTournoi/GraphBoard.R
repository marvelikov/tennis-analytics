# Title: GraphBoard
# Author: Stephane Caron
# Date: 10-07-2017
# Subject: Made a tennis tournament board from grid package


# 0. Load package ---------------------------------------------------------

require(grid)
require(readxl)
require(dplyr)

# 1. Load and rearrange data ----------------------------------------------

data_tennis <- read_xlsx("Data/MR-BO-2016.xlsx")

# Methodology:
# We have to start by making a function that will sort our data in an appropriate order before
# filling our graph. Why ? It's because our tournament tree is determined in advance so we 
# have to place the matchs up in each round in the correct order. 

# This part is a little tricky because the order of a given round will be determined by the
# data of the next round. In fact, the match up of the next round will tell us which match up
# have to be placed back to back.

data_1st <- data_tennis[which(data_tennis$ATP == 1),]

# Ensure data is in the right order for the Rounds
order_round <- c("1st Round", "2nd Round", "3rd Round", "4th Round", "Quarterfinals", "Semifinals", "The Final")


# 2. Make the grid depending the tournament structure ---------------------

vp_contour <- viewport(x = 0.5, y = 0.5, width = 0.9, height = 0.9)
pushViewport(vp_contour)
nb_round <- length(unique(data_1st$Round))
nb_set <- data_1st$`Best of`[1]
for (k in 0:(nb_round - 1)) {
  for (n in 1:2^k) {
    grid.rect(x = 1 - k * (1/(nb_round - 1)), y = (1 + 2 * (n - 1))/ 2^(k+1), width = 0.01, height = 0.01)
  }
}





(1+2(n-1))/2^k
k = 1
n = 1
options(scipen = 999)
sapply(X = 0:(nb_round - 1), FUN = function(x) {0.9 - x * (0.9/(nb_round - 1))})





