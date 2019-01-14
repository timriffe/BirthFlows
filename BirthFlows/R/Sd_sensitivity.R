
# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")

# example year 1863:
# PC has final,

# smoothed sweden
SWE_sm    <- local(get(load("/home/tim/git/BirthFlows/BirthFlows/Data/SWE_sm.Rdata")))
SWE_final <- local(get(load("/home/tim/git/BirthFlows/BirthFlows/Data/SWE_final.Rdata")))


ind <- SWE_sm$Year == 1863

wsd(SWE_sm$ARDY[ind],SWE_sm$Total[ind])
wsd(SWE_sm$Cohort[ind],SWE_sm$Total[ind])

ind2 <- SWE_final$Year == 1863
wsd(SWE_final$ARDY[ind2],SWE_final$Total[ind2])
wsd(SWE_final$Cohort[ind2],SWE_final$Total[ind2])

unif <- splitUniform(groupAges(SWE_sm$Total[ind],SWE_sm$ARDY[ind]),OAG=FALSE,OAvalue=5)
wsd(names2age(unif),unif)

png("Figures/sd_sens.png")
plot(names2age(unif),unif,type="s",ylim=c(0,7500))
lines(SWE_sm$ARDY[ind],SWE_sm$Total[ind],col="blue")
lines(SWE_final$ARDY[ind2],SWE_final$Total[ind2], col = "red")
legend("bottom",lty=1,col=c("black","blue","red"),legend=c("Orig (uniform) sd = 6.704",
				"graduated smooth sd = 6.4064", "final adjusted sd = 6.3928"),bty="n")
dev.off()
