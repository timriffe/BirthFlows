# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")
# Structural echoes
# reference years
ryrs <- 1775:1968
cind <- as.character(ryrs)
N    <- length(ryrs)
Bt   <- rowSums(PC)[cind]
Bc   <- colSums(PC)[cind]

# reference years

rBt <- diff(Bt) / Bt[-N]
rBc <- diff(Bc) / Bc[-N]

yind <- ryrs < 1891
plot(rBt,rBc,col=ifelse(yind,"magenta","blue"),	asp=1,
		main = "Fractional change in offspring versus cohort size",
		las=1)
abline(a=0,b=1)
abline(lm(rBc[yind]~rBt[yind]),col="magenta")
abline(lm(rBc[!yind]~rBt[!yind]),col="blue")

# Give rates and exposures these shocks are decomposable, also into mort, mig, etc.