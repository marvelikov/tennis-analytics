library(pcaPP)
par(mar = c(0,0,0,0))
image(cor.fk(p_stats[,2:12])[11:1,], zlim = c(-1,1), col = colorRampPalette(c("darkred","aliceblue","darkgreen"))(100))

