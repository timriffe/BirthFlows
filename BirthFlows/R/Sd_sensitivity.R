
# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")

# example year 1863:
# PC has final,

# smoothed sweden
SWE <- local(get(load("/home/tim/git/BirthFlows/BirthFlows/Data/SWE.Rdata")))
		
# compare w uniform

plot(1687:2016, PC[ "1863",],type='l',xlim=c(1810,1850))

head(SWE[SWE$Year == 1863,])

ind <- SWE$Year == 1863
wsd(SWE$ARDY[ind],SWE$Total[ind])
wsd(SWE$Cohort[ind],SWE$Total[ind])

wsd(1687:2016, PC[ "1863",])

sum(PC[ "1863",])
sum(SWE$Total[ind])
unif <- splitUniform(groupAges(SWE$Total[ind],SWE$ARDY[ind]),OAG=FALSE,OAvalue=5)
wsd(10:54,splitUniform(groupAges(SWE$Total[ind],SWE$ARDY[ind]),OAG=FALSE,OAvalue=5))

png("Figures/sd_sens.png")
plot(10:54,unif,type="s",ylim=c(0,7500))
lines(SWE$ARDY[ind],SWE$Total[ind],col="blue")
lines(1863-1687:2016, PC[ "1863",], col = "red")
legend("bottom",lty=1,col=c("black","blue","red"),legend=c("Orig (uniform) sd = 6.704",
				"graduated smooth sd = 6.4064", "final adjusted sd = 6.3928"),bty="n")
dev.off()
