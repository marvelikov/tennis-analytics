# Say we want to predict a given match... Let us do it with an example

# 0. Load packages --------------------------------------------------------

require(readr)
require(lubridate)
require(data.table)
require(readr)

# 1. Import data ----------------------------------------------------------

data <- data.table(read_excel("C:/Users/Samuel/Dropbox/Git Projects/tennis-analytics/Data/MR-BO-2016.xlsx"))




# Say we want to predict the following game

data[Round == "The Final" & Tournament == "Rogers Masters"]

# The most "basic but yet interesting" model to fit a logistic regression using the history of games...
# We already have the possibility to use shot-by-shot data, but will restrict ourselves to match-by-match for now.
# Neural Nets should be where we aim (longterm)

# Anyways, let get started.. We first want to identify the date of our match and select the ones anterior to it.
# Note that this is not necessarily required in a real life setting as there exists no matches posterior to it yet...

data.anterior <- data[Date <= data[Round == "The Final" & Tournament == "Rogers Masters", Date]]






