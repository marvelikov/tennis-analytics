# Here it goes


# 0. Load packages --------------------------------------------------------

require(readr)
require(lubridate)
require(data.table)

# 1. Import data ----------------------------------------------------------

# Le data -- sur mon ordi, je devrais le push sur le repository (J'ai pas juste 2010, mais commençons avec ça)
library(readr)
Players <- read_csv("Data/Players.txt", col_names = FALSE)
T2010 <- read_csv("Data/T2010.txt", col_names = FALSE)

colnames(Players) <- c("id", "first_name", "family_name", "hand", "birth_date", "country")
colnames(T2010) <- c("ranking_date", "rank", "id", "points")


# 2. Clean data -----------------------------------------------------------

# Un peu de cleaning pour les birth_date -- pas élégant mais bon... J'ai mieux ? faire
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

age_calc <- function(birth, now){
  birth_dt <- as.Date(birth)
  now_dt <- as.Date(now)
  
  age <- year(now_dt) - year(birth_dt)
  
  ifelse(month(now_dt) < month(birth_dt) | (month(now_dt) == month(birth_dt) & day(now_dt) <= day(birth_dt)), age - 1, age)
}


# 3. Merge players to ranking ---------------------------------------------

# Faque  le data c'est juste ca
data <- merge(Players, T2010)

# Transforme ca en data.table
data <- data.table(data)
class(data)

# On calcule l'âge
data[, age:= age_calc(birth_date, ranking_date)]



