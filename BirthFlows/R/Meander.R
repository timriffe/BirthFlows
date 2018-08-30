setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")

# Author: tim
###############################################################################

# for the meander description in the Appendix.

BT       <- colSums(PC5)
BC       <- colSums(P5C) 

yrs      <- 1775:rightCoh(SWE,45)
yrsc     <- as.character(yrs)

meander  <- BC[yrsc] / BT[yrsc] 
start    <- meander[1]
end      <- meander[length(meander)]

Nstart   <- min(yrs) - min(Cohs) 
Nend     <- max(Yrs) - max(yrs)
meander_extended <- c(rep(start,Nstart),meander,rep(end,Nend))

yrs_smooth       <- min(Cohs):max(Yrs)

meander_smoothed <- smooth.spline(x = yrs_smooth, y = meander_extended, lambda = .00001)$y
names(meander_smoothed) <- yrs_smooth

smoothed <- meander_smoothed[yrsc]


pdf("Figures/Meander.pdf")
plot(yrs, meander,ylab="B(c=r)/B(t=r)",xlab = "Reference Year",axes=FALSE)
abline(h=1, col = "#00000080")
lines(yrs, smoothed, lwd = 2, col = "blue")
axis(1)
axis(2,las=1)
dev.off()
locator(2)