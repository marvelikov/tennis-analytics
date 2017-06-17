# Here it goes

# Le data -- sur mon ordi, je devrais le push sur le repository (J'ai pas juste 2010, mais commen?ons avec ?a)
library(readr)
Players <- read_csv("C:/Users/Samuel/Dropbox/Projects/Tennis-Fil/Players.txt", col_names = FALSE)
T2010 <- read_csv("C:/Users/Samuel/Dropbox/Projects/Tennis-Fil/T2010.txt", col_names = FALSE)

colnames(Players) <- c("id", "first_name", "family_name", "hand", "birth_date", "country")
colnames(T2010) <- c("ranking_date", "rank", "id", "points")

# Un peu de cleaning pour les birth_date -- pas ?l?gant mais bon... J'ai mieux ? faire
Players$birth_date <- sapply(Players$birth_date, function(d){
  #print(d)
  if(is.na(d)){return(NA)}
  d <- strsplit(as.character(d), split = "")[[1]]
  
  if(length(d) < 8){
    #print(c(d, " but not complete"))
    return(NA)
  }
  
  y <- paste0(d[1],d[2],d[3],d[4])
  m <- paste0(d[5],d[6])
  d <- paste0(d[7],d[8])
  if(m == "00" | d == "00"){
    #print(c(y, " but HEY ----------"))
    return(NA)
  }
  #print(paste0(y,"-",m,"-",d), as.Date(paste0(y,"-",m,"-",d)))
  paste0(y,"-",m,"-",d)
})


# M?me principe pour les ranking_date
T2010$ranking_date <- sapply(T2010$ranking_date, function(d){
  #print(d)
  if(is.na(d)){return(NA)}
  d <- strsplit(as.character(d), split = "")[[1]]
  
  if(length(d) < 8){
    print(c(d, " but not complete"))
    return(NA)
  }
  
  y <- paste0(d[1],d[2],d[3],d[4])
  m <- paste0(d[5],d[6])
  d <- paste0(d[7],d[8])
  if(m == "00" | d == "00"){
    print(c(y, " but HEY ----------"))
    return(NA)
  }
  #print(paste0(y,"-",m,"-",d))
  #as.Date(paste0(y,"-",m,"-",d))
  paste0(y,"-",m,"-",d)
})


# Pour l'instant je ne les transforme pas en objet `Date`

#Players$birth_date <- sapply(Players$birth_date, function(d){
#  as.Date(as.character(d),format="%Y%m%d")
#})

#T2010$date <- sapply(T2010$date, function(d){
#  as.Date(as.character(d),format="%Y%m%d")
#})


# Faque  le data c'est juste ?a
data <- merge(Players, T2010)

# Transforme ?a en data.table
library(data.table)
data <- data.table(data)
data


# On peut facilement calculer l'?ge en jours....
data$days <- as.Date(data$ranking_date) - as.Date(data$birth_date)


# L?, jusqu'ici tout va bien
data


# Mais ?a look plus avec YYYY years, MM months and DD days ....
rd <- as.Date(data$ranking_date)
bd <- as.Date(data$birth_date)

library(lubridate)
int <- interval(bd, rd)
per <- as.period(int)

data[,age := per]

# Pourquoi maintenant ?a bug?
data




# Aussi, les 0H 0M 0S sont fatiguants, je suis rendu l?... (?a ?a marche mais pas juste le call "data")
data$age[1:5]



