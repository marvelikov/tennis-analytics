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


# On pourrait ensuite utiliser ces modeles logistiques pour generer artificiellement des arbres de tournois
# Ca nous permettrait aussi de travailler on the side sur l'amelioration de la prediction d'un match
# et d'integrer les changements au fur et a mesure dans la generation d'arbre de tournoi sans avoir
# a reelement modifier cette derniere.



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

# The tournaments are encoded with 'Tournament' 'Round' 'Winner' 'Loser'
data_mr$Tournament
data_mr$Round
data_mr$Winner
data_mr$Loser


# We want a function taking 'Tournament' and ('Winner','Loser','Round')


f <- function(Name, data){
    # subset the Tournament
    sub_data <- data[Tournament == Name]
    
    all <- unique(unlist(sub_data[, Winner, Loser]))
    d <- length(all)
    id.mat <- data.table(PName = all, id = -(1:d))
    
    # unique rounds
    uRound <- unique(sub_data$Round)

    # we need to who merges with who and at which level.. Probably not going to work.
    height.vec <- NULL
    merge.mat <- matrix(, 0, 2)
    
    for(i in 1:nrow(sub_data)){
      merge.mat <- rbind(merge.mat, id.mat[PName == sub_data[i, Winner] | PName == sub_data[i, Loser]]$id)
      id.mat[PName == sub_data[i, Winner] | PName == sub_data[i, Loser]]$id <- i
      
      h <- which(uRound == sub_data[i, Round])
      height.vec <- c(height.vec,h)
    }
    
    # This is good, but the result will still be unorganized
    
    structure <- list()
        
    structure$merge <- merge.mat
    structure$height <- height.vec
    structure$order <- 1:d
    structure$labels <- all
    
    class(structure) <- "hclust"
    #class(structure) <- as.dendrogram(structure)
    plot(structure)
    
}

f('Rogers Masters', data_mr)

# fonctionne pas mais c'est un début
# On devrait peut-etre regarde dans les trucs de modeles graphique finalement...








# 4. Focus on a single match outcome

# The most obvious thing to start with is a basic logistic model
# I think this will eventually end up being a neural network.








# 5. Focus on a tournament tree outcome

# I think the simplest thing to begin with in this case is to use our results obtained with the 'single-match' model. 
# We can artificially generate many (thousands++) tournament trees using the probabilities returned by the logistics and
# create a forest of tournament trees.

help(hclust)






# 6. Clustering

# Using the full model we have, we could try to find some interesting clusterings of the players.
# That would produce nice visual 

