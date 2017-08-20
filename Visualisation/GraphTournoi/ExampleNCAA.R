
require(grid)
grid.newpage()
pushViewport(viewport(width=.8,height=.8))
# partition the plot areas
pushViewport(viewport(layout = grid.layout(50,104)))
#grid.rect(gp=gpar(col="grey"))
#main title
pushViewport(viewport(layout.pos.col = 1:104, layout.pos.row = 1:2))
grid.rect(gp=gpar(col="gray", fill="lightblue"))
grid.text("NCAA Tournament Bracket A")
popViewport()

# Seeded Team Names
Teams <- paste("Team", 1:16,sep=" ")
Seeds <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
Scores <- sample(50:95,16)
PlotRound <- function(Teams,Seeds,Scores){
  ngames <- length(Teams)/2
  nround <- 4 - log2(ngames)
  offset <- c(0,3,9,21)
  skip <- c(0,6,18,0)
  col <- (nround - 1) * 26
  for(k in 1:ngames){
    for(j in 1:2){
      i <-  j +(k-1)*2
      row <-  4 + (k-1)*6 + (j-1)*2 + offset[nround]  + skip[nround]*(k-1)
      #Seed Box
      pushViewport(viewport(layout.pos.col = col + 1:3, layout.pos.row = row:(row+1)))
      grid.rect(gp=gpar(col="gray", fill="cornsilk"))
      grid.text(Seeds[i],gp=gpar(fontsize=9, col=gray(0.1)))
      popViewport()
      #Name Box
      pushViewport(viewport(layout.pos.col = col + 4:21, layout.pos.row = row:(row+1)))
      grid.rect(gp=gpar(col="gray", fill="cornsilk"))
      if(j == 1 && (Scores[i] > Scores[i+1]))
        grid.text(Teams[i],gp=gpar(fontsize=9, fontface="bold",col=gray(0.1)))
      else if(j == 2 && (Scores[i] > Scores[i-1]))
        grid.text(Teams[i],gp=gpar(fontsize=9, fontface="bold",col=gray(0.1)))
      else 
        grid.text(Teams[i],gp=gpar(fontsize=9, col=gray(0.1)))
      popViewport()
      #Score Box
      pushViewport(viewport(layout.pos.col = col + 21:23, layout.pos.row = row:(row+1)))
      grid.rect(gp=gpar(col="gray", fill="cornsilk"))
      grid.text(Scores[i],gp=gpar(fontsize=9, col=gray(0.1)))
      popViewport()
    }
  }
}
# Seeded Team Names and other initializations
Teams <- paste("Team", 1:16,sep=" ")
Seeds <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
Scores <- sample(50:95,16)
for(round in 1:4){
  PlotRound(Teams,Seeds,Scores)
  ngames <- length(Teams)/2
  odd <- (1:ngames)*2 - 1
  even  <- (1:ngames)*2 
  W <- ifelse(diff(Scores)[odd] < 0,odd,even)
  Teams <- Teams[W]
  Seeds <- Seeds[W]
  Scores <- sample(50:95,length(Teams))
}
