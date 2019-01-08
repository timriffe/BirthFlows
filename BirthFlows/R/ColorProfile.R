# Author: tim
###############################################################################
setwd("/home/tim/git/BirthFlows/BirthFlows")
#source("R/DataPrep.R")

x          <- 1720:2030
xl         <- min(x)
xr         <- max(x)
breaks     <- pretty(c(Per_SD5, Coh_SD5),35)
n          <- length(breaks)
breaks[1]  <- breaks[1] - .1
breaks[n]  <- breaks[n] + .1
fillcolors <- viridis(length(breaks) - 1, option = "A")

pdf("Figures/ColorProfile.pdf",height=4.3,width=10)
par(mai=c(.6,1,.2,.2),xaxs="i",yaxs="i",xpd=TRUE)
plot(NULL, type = 'n', xlim=c(xl,xr),ylim=range(breaks),panel.first = {
			rect(xl,breaks[-n],xr,breaks[-1],border=NA,col=fillcolors)
			segments(seq(1720,2030,by=20),breaks[1],seq(1720,2030,by=20),breaks[n],col="#88888880")
			segments(xl,seq(5,6.5,by=.5),xr,seq(5,6.5,by=.5),col="#88888880")
			text(xl,seq(5,6.5,by=.5),c("5.0","5.5","6.0","6.5"),pos=2)
			text(seq(1720,2020,by=20),breaks[1],seq(1720,2020,by=20),pos=1)
			rect(xl,breaks[1],xr,breaks[n])
		},
		ann = FALSE, axes = FALSE
)
lines(Cohs,Coh_SD, lwd = 2, col = "#63C8FB",lty="42") # light
lines(Cohs,Coh_SD, lwd = 2, col = "#0049A0",lty="22") # dark
#lines(Cohs,Coh_SD, lwd = 2, col = "#DFC400",lty="22")

lines(Yrs,Per_SD, lwd = 2, col = "#FBE963",lty="42")  # light
lines(Yrs,Per_SD, lwd = 2, col = "#686419",lty="22")  # dark

text(1860,4.5,"Index Year",cex=1.5)
text(xl-25,5.7,"standard deviation (years)",cex=1.5,srt=90)

text(1900,5.7,"Cohort",cex=1.5)
text(1970,6.2,"Period",cex=1.5)
dev.off()