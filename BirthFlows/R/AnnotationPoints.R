
# Author: tim
###############################################################################


# highest / lowest this and that.
setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")
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
plot(PeriodMeasures$Year,PeriodMeasures$pmac)
abline(v=1862)
notes[[2]] <- list(r=1862,d=1,m="1862 pMAC 32.5")

# min
i <- which.min(PeriodMeasures$pmac)
PeriodMeasures[i]
notes[[3]] <- list(r=1967,d=1,m="1967 pMAC 26.3")

# max period sd in 1775
i <- which.max(PeriodMeasures$psd)
PeriodMeasures[i]
notes[[4]] <- list(r=1775,d=1,m="1775 pSD 6.78")

plot(PeriodMeasures$Year,PeriodMeasures$psd)
abline(v=1775)
abline(v=1975)
# lowest sd in 1975
i <- which.min(PeriodMeasures$psd)
PeriodMeasures[i]
notes[[5]] <- list(r=1975,d=1,m="1975 pSD 4.83")

# 1887 largest birth cohort 140095
i <- which.max(PeriodMeasures$Bt)
PeriodMeasures[i]
notes[[6]] <- list(r=1887,d=1,m="1887 over 140000 births")
plot(PeriodMeasures$Year,PeriodMeasures$Bt)
abline(v=1887)
abline(v=1736)
# 1736 smallest cohort 50892
i <- which.min(PeriodMeasures$Bt)
PeriodMeasures[i]
notes[[7]] <- list(r=1887,d=1,m="1736 just over 50000 births")

# -----------------------------------
# projected 2015
i <- which.max(CohortMeasures$pmac)
CohortMeasures[i]
notes[[8]] <- list(r=2000,d=-1,m=">2000 cMAC > 32.6")


# 1822 cohort for non-projected cohorts
CohortMeasures[CohortMeasures$Cohort < 1990][which.max(pmac)]
notes[[9]] <- list(r=1823,d=-1,m="1823 cMAC 32.6")
CohortMeasures$pmac
plot(CohortMeasures$Cohort,CohortMeasures$pmac)

# 1943 cohort had lowest sd of 5.4
i <- which.min(CohortMeasures$psd)
CohortMeasures[i]
notes[[10]] <- list(r=1937,d=-1,m="1937 cSD 5.07")
# highest sd 1732 cohort 6.7
i <- which.max(CohortMeasures$psd)
CohortMeasures[i]
notes[[10]] <- list(r=1732,d=-1,m="1732 cSD 6.73")
# 1989 151968 births, but mostly projected?,
# however it's a short term projection, rather likely in this ballpark
i <- which.max(CohortMeasures$Bt)
CohortMeasures[i]

# 1858 142115
i <- which.max(CohortMeasures$Bt[CohortMeasures$Cohort < 1970])
CohortMeasures[i]
# 1772 58665 births
i <- which.min(CohortMeasures$Bt[CohortMeasures$Cohort < 2016])
CohortMeasures[i]


