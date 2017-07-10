# Title: GraphBoard
# Author: Stephane Caron
# Date: 10-07-2017
# Subject: Made a tennis tournament board from grid package


# 0. Load package ---------------------------------------------------------

require(grid)
require(readxl)

# 1. Load and rearrange data ----------------------------------------------

data <- read_xlsx("Data/MR-BO-2016.xlsx")

# Methodology:
# We have to start by making a function that will sort our data in an appropriate order before
# filling our graph. Why ? It's because our tournament tree is determined in advance so we 
# have to place the matchs up in each round in the correct order. 

# This part is a little tricky because the order of a given round will be determined by the
# data of the next round. In fact, the match up of the next round will tell us which match up
# have to be placed back to back.

