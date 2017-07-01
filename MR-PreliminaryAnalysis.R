# Illustrating our 'Match Results and Betting Odds'data

# Here, I suggest we put ideas about many of the aspects of the project. Je vais ecrire en francais pas d'accents.

# Il y a deux gros milestones au projet :

# 1. le premier est la prediction du resultat d'un match;
# 2. le deuxieme le resultat d'un tournoi (l'arbre du tournoi et non seulement le vaiqueur)

# Toutefois, on doit se concentrer avant tout sur la data qu'on a.
# J'ai enregistre les tournois de 2016 sur le site que Phil a partage.

# J'ai commence par essayer de reformer un arbre de tournoi, pour le plaisir.
# La tache la plus pressante est toutefois de definir un modele naif pour la prediction d'un seul match.
# Je crois qu'on devrait commencer avec un modele logistique.



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

# There are some warnings concerning the presence of 'N/A' values. Most, if not all, missing values are in the
# columns 'W3', 'L3', ...
# This is simply because the match did not require these sets to be played...

# All-in-all, the data is pretty clean... We can either try to fill the NA's





# 3. Recovering a tournament tree

# Say we are interested in the Rogers Masters, so we subset the data
data_rogers <- data_mr[Tournament == "Rogers Masters"]

# Let us build it top to bottom
# First, how many rounds are there?
tree_height <- length(unique(data_rogers$Round))

# The final game...
final_game <- data_rogers[Round == "The Final"]










# 4. Focus on a single match outcome

# The most obvious thing to start with is a basic logistic model










# 5. Focus on a tournament tree outcome

# I think the simplest thing to begin with in this case is to use our results obtained with the first model. 
# We can artificially generate many tournament trees using the probabilities returned by the logistics and
# create a forest of tournament trees.



