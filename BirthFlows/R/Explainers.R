
# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")
source("R/ColorRamp.R")
source("R/Arcs.R")
SWEl     <- melt(PC, varnames = c("Year","Cohort"),value.name = "B")
SWEl$Age <- SWEl$Year - SWEl$Cohort
SWEl     <- SWEl[SWEl$Age >= 12 & SWEl$Age <= 55, ]
SWEl     <- SWEl[SWEl$B > 0, ]
SWEl     <- data.table(SWEl)
SWEl$Age5 <- SWEl$Age - SWEl$Age %% 5
SWEl5    <- SWEl[,list(B5 = sum(B)), by = list(Year, Age5)]
SWEl5$Cohort <- SWEl5$Year - SWEl5$Age5 
# experiment with arcs



SWEl5 <- data.frame(SWEl5)
SWEl5L <- SWEl5[SWEl5$B5 > 1000, ]
SWEl5L$Cohort < SWEl5L$Year



plot(NULL, xlim = c(1750,2015), ylim = c(-5,25))
for (i in 1:nrow(SWEl5L)){
		arc <- arcpoly(
				p1=list(x= SWEl5L[i,"Cohort"],y=0),
				p2=list(x= SWEl5L[i,"Year"],y=0),
				maxthick = SWEl5L[i,"B5"]/1000)		
		polygon(arc, border = NA, col = "#00000001")
	
}

arc <- arcpoly(p1=list(x=1820,y=0),p2=list(x=1850,y=0),maxthick = 2)
polygon(arc, border = NA, col = "#00000050")

