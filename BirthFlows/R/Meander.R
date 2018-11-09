setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")


# Author: tim
###############################################################################

# for the meander description in the Appendix.


pdf("Figures/Meander.pdf")
plot(yrs, log(meander),ylab="ln(B(c=r)/B(t=r))",xlab = "Reference Year, r",axes=FALSE)
abline(h=0, col = "#00000080")
lines(yrs, meander_smoothed[yrsc], lwd = 2, col = "blue")
axis(1)
axis(2,las=1)
dev.off()
