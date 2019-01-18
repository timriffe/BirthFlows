
# Author: tim
###############################################################################


# highest / lowest this and that.
setwd("/home/tim/git/BirthFlows/BirthFlows")
#source("R/DataPrep.R")
SWE <- local(get(load("Data/SWE_final.Rdata")))

head(SWE)

SWE <- data.table(SWE)
SWE$Total[is.na(SWE$Total)] <- 0
PeriodMeasures <- SWE[,list(
				pmac = wmean(ARDY,Total), 
				psd = wsd(ARDY,Total), 
				Bt = sum(Total)), 
		by = list(Year)]
CohortMeasures <- SWE[,list(
				pmac = wmean(ARDY,Total), 
				psd = wsd(ARDY,Total), 
				Bt = sum(Total)), 
		by = list(Cohort)]

CohortMeasures <- CohortMeasures[Cohort >= 1720 & Cohort <= 2016]
PeriodMeasures <- PeriodMeasures[Year >= 1736 & Year <= 2035]

# save notes in a list
notes <- list()

# max period empirical MAC

# projected... 2035 32.7
i <- which.max(PeriodMeasures$pmac)
PeriodMeasures[i]
notes[[1]] <- list(r=2029,d=1,m=">2029 pMAC 32.5")

# in empirical series max mac in 1862 32.5
PeriodMeasures[Year <= 2016][which.max(pmac)]
#plot(PeriodMeasures$Year,PeriodMeasures$pmac)
#abline(v=1862)
notes[[2]] <- list(r=1862,d=1,m="1862 pMAC 32.5")

# min
i <- which.min(PeriodMeasures$pmac)
PeriodMeasures[i]
notes[[3]] <- list(r=1967,d=1,m="1967 pMAC 26.3")

# max period sd in 1775
i <- which.max(PeriodMeasures$psd)
PeriodMeasures[i]
notes[[4]] <- list(r=1775,d=1,m="1775 pSD 6.78")

#plot(PeriodMeasures$Year,PeriodMeasures$psd)
#abline(v=1775)
#abline(v=1975)
# lowest sd in 1975
i <- which.min(PeriodMeasures$psd)
PeriodMeasures[i]
notes[[5]] <- list(r=1975,d=1,m="1975 pSD 4.83")

# 1887 largest birth cohort 140095
i <- which.max(PeriodMeasures$Bt)
PeriodMeasures[i]
notes[[6]] <- list(r=1887,d=1,m="1887 over 140000 births")
#plot(PeriodMeasures$Year,PeriodMeasures$Bt)
#abline(v=1887)
#abline(v=1736)
# 1736 smallest cohort 50892
i <- which.min(PeriodMeasures$Bt)
PeriodMeasures[i]
notes[[7]] <- list(r=1736,d=1,m="1736 just over 50000 births")

# -----------------------------------
# projected 2015
i <- which.max(CohortMeasures$pmac)
CohortMeasures[i]
notes[[8]] <- list(r=2000,d=-1,m=">2000 cMAC > 32.6")


# 1822 cohort for non-projected cohorts
CohortMeasures[CohortMeasures$Cohort < 1990][which.max(pmac)]
notes[[9]] <- list(r=1823,d=-1,m="1823 cMAC 32.6")
CohortMeasures$pmac
#plot(CohortMeasures$Cohort,CohortMeasures$pmac)

# 1943 cohort had lowest sd of 5.4
i <- which.min(CohortMeasures$psd)
CohortMeasures[i]
notes[[10]] <- list(r=1937,d=-1,m="1937 cSD 5.07")
# highest sd 1732 cohort 6.7
i <- which.max(CohortMeasures$psd)
CohortMeasures[i]
notes[[11]] <- list(r=1732,d=-1,m="1732 cSD 6.73")
# 1990 151968 births, but mostly projected?,
# however it's a short term projection, rather likely in this ballpark
i <- which.max(CohortMeasures$Bt)
CohortMeasures[i]
notes[[12]] <- list(r=1990,d=-1,m="1990 offspring >151k")
# 1859 142115

CohortMeasures[CohortMeasures$Cohort < 1980][which.max(Bt)]
notes[[13]] <- list(r=1859,d=-1,m="1859 offspring >142k")

# 1772 58665 births
CohortMeasures[CohortMeasures$Cohort < 2016][which.min(Bt)]
notes[[14]] <- list(r=1773,d=-1,m="1773 offspring >58k")


# event timeline

notes1 <- list()
notes1[[1]] <- list(r = 1743, m = "armed conflict")
notes1[[2]] <- list(r = 1773, m = "famine")
notes1[[3]] <- list(r = 1790, m = "armed conflict")
notes1[[4]] <- list(r = 1800, m = "armed conflict", adj = list(x=.2,y=-10000))
notes1[[5]] <- list(r = 1809, m = "armed conflict")
notes1[[6]] <- list(r = 1889, m = "Russian pandemic")
# the year without summer
#segments(1816,baseline["1816"]-150e3,1816,3.2e5,lwd=.8,col="#2a70e0",lty="12")
#text(1816,3.2e5,"'the year\nwithout summer'",pos=3,cex=.7,col="#2a70e0")

notes_fork <- list()
notes_fork[[1]] <- list(xl=1831,xr=1833,m="pandemic") 

# pandemic
#draw.fork(1832,1831,1833,
#		y1=3e5,y2=1.4e5,baseline["1831"]+Bt["1831"],baseline["1833"]+Bt["1833"]
#		,lwd=.8,col="#2a70e0",lty="12") 
#text(1832,3e5,"pandemic",pos=3,cex=.7,col="#2a70e0")
# pandemic

#draw.fork(1847.5,1847,1848,
#		y1=3e5,y2=1.4e5,baseline["1847"]+Bt["1847"],baseline["1848"]+Bt["1848"]
#		,lwd=.8,col="#2a70e0",lty="12") 
#text(1847.5,3e5,"pandemic",pos=3,cex=.7,col="#2a70e0")

notes_fork[[2]] <- list(xl=1847,xr=1848,m="pandemic") 
# famine
#draw.fork(1868,1867,1869,
#		y1=3e5,y2=1.4e5,baseline["1867"]+Bt["1867"],baseline["1869"]+Bt["1869"]
#		,lwd=.8,col="#2a70e0",lty="12") 
#text(1868,3e5,"famine",pos=3,cex=.7,col="#2a70e0")
notes_fork[[3]] <- list(xl=1867,xr=1869,m="famine") 
# Russian pandemic
#segments(1889,baseline["1889"],1889,3e5,lwd=.8,col="#2a70e0",lty="12")
#text(1889,3e5,"Russian pandemic",pos=3,cex=.7,col="#2a70e0")


# Spanish flu
#draw.fork(1919.5,1919,1920.5,
#		y1=3e5,y2=1.45e5,baseline["1919"]+Bt["1919"],baseline["1920"]+Bt["1920"]
#		,lwd=.8,col="#2a70e0",lty="12") 
##segments(1918.5,1.19e5,1918.5,3e5,lwd=.8,col="#2a70e0",lty="12")
#text(1919,3e5,"Spanish influenza\nand recovery",pos=3,cex=.7,col="#2a70e0")
notes_fork[[4]] <- list(xl=1919,xr=1921,m="Spanish influenza\nand recovery") 
# -------- end annotations ----------- #

