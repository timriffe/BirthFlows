
# Author: tim
###############################################################################

obsmat <- matrix(ncol = length(1966:2016), nrow = 56 - 12)
projyrs <- 2017:2072

pdf("Figures/ProjectionDiagram.pdf",width=7,height=5)
par(mai=c(.5,.6,.1,.1))
plot(NULL, type = "n", 
		xlim = c(1966, 2071), 
		ylim = c(0, 60), 
		asp = 1, 
		ann = FALSE, 
		axes = FALSE)
rect(col(obsmat) + 1965,
	 row(obsmat) + 11,
	 col(obsmat) + 1966,
	 row(obsmat) + 12,
	 lwd = .5,
	 border = gray(.3))

# diagonal
segments(2017, 0, 2017 + 56, 56)
bottoms <- c(rep(12,13),13:55)
for (i in 1:length(projyrs)){
	
	rect(projyrs[i], bottoms[i]:55, projyrs[i] + 1, (bottoms[i] + 1):56,
			lwd = .5, border = "royalblue")
}
segments(2017, 0, 2017, 60, lwd = 2)
# x axis
segments(1966, 0, 2072, 0)
segments(c(1966, 2017, 2072), 0, c(1966, 2017, 2072), -1, xpd = TRUE)
text(c(1966, 2017, 2072), -1, c(1966, 2017, 2072), pos = 1, xpd = TRUE)

# y axis
segments(1966, 0, 1966, 60)
segments(1966, c(0, 12, 56), 1965, c(0, 12, 56), xpd = TRUE)
text(1965, c(0, 12, 56), c(0, 12, 56), pos = 2, xpd = TRUE)

# labels
text(2016,-10,"Year",cex=1.2,font=2,xpd=TRUE)
text(1956,35,"Age",cex=1.2,font=2,xpd=TRUE)
text(1990,60,"Training rates (HFD)",cex=1.2,font=2,xpd=TRUE)
text(2040,60,"Projected rates",cex=1.2,font=2,xpd=TRUE)
dev.off()