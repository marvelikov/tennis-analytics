# Creates model... a little bit of patience is necessary.
# It also loads keras...
source("Modeling/NeuralNetworks/ReplicateSom2009/NNTennis2.R")


# Load packages -----------------------------------------------------------
library(ggplot2)


# Interesting stuff -------------------------------------------------------
model$layers
w <- keras::get_weights(model)

# Coloring tools ----------------------------------------------------------
colfunc <- colorRampPalette(c("darkred", "darkorange", "palegoldenrod", "forestgreen", "darkblue"))


# Setup -------------------------------------------------------------------

m1 <- mean(1:nrow(w[[1]]))
m2 <- mean(1:ncol(w[[1]]))
set1 <- data.table(x1 = 1:nrow(w[[1]]) - m1, y1 = rep(1,nrow(w[[1]])))
set2 <- data.table(x2 = 1:ncol(w[[1]]) - m2, y2 = rep(2,ncol(w[[1]])))

set12 <- expand.grid(1:nrow(set1),1:nrow(set2))
set12 <- data.table(set1[set12[,1],1], set1[set12[,1],2], set2[set12[,2],1], set2[set12[,2],2])

m3 <- mean(1:ncol(w[[3]]))
set3 <- data.table(x3 = 1:ncol(w[[3]])  - m3, y3 = rep(3,ncol(w[[3]])))

set23 <- expand.grid(1:nrow(set2),1:nrow(set3))
set23 <- data.table(set2[set23[,1],1], set2[set23[,1],2], set3[set23[,2],1], set3[set23[,2],2])

# Plot --------------------------------------------------------------------

# initialize the plot
plot <- ggplot() + theme(axis.line=element_blank(),axis.text.x=element_blank(),
             axis.text.y=element_blank(),axis.ticks=element_blank(),
             axis.title.x=element_blank(),
             axis.title.y=element_blank(),legend.position="none",
             panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),plot.background=element_blank())

plot  

# put the nodes
plot <- plot +
  geom_point(data = set1, aes(x1,y1)) +
  geom_point(data = set2, aes(x2,y2)) +
  geom_point(data = set3, aes(x3,y3))

plot  
# Need to: adjust size of points with respect to sum_w abs(w)
# Need to: place point on new layer around center of the plot.
# Need to: create a group vector that tells the id of a given point and use
# geom_line(data = dt.line, aes(x = x, y = y, group = group, color = ***))
# instead of what is below.


# a cause des means ca bug .. juste a ajuster les indices.

# put lines
for(i in 1:nrow(set12)){
  dat <- data.table(x = c(set12$x1[i],set12$x2[i]), y = c(set12$y1[i],set12$y2[i]))
  ww <- w[[1]][dat$x[1], dat$x[2]]
  if(abs(ww) > .5){
    col <- colfunc(200)[median(c(1, 100 + ww * 100 ,200)) ]
    plot <- plot + geom_line(data = dat, aes(x,y), col = col)
  }
}
plot


for(i in 1:nrow(set23)){
  dat <- data.table(x = c(set23$x2[i],set23$x3[i]), y = c(set23$y2[i],set23$y3[i]))
  ww <- w[[1]][dat$x[1], dat$x[2]]
  if(abs(ww) > 0){
    col <- colfunc(200)[median(c(1, 100 + ww * 100 ,200)) ]
    plot <- plot + geom_line(data = dat, aes(x,y), col = col)
  }
}

plot
var_names[- length(var_names)]

par(mar = c(0,0,0,0))
image(t(w[[1]][nrow(w[[1]]):1,]), axes=FALSE, col=colfunc(100))

