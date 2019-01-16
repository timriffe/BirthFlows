
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

# max period empirical MAC

# projected...
i <- which.max(PeriodMeasures$pmac)
PeriodMeasures[i]

# in empirical series max in 1862 32.5
PeriodMeasures[Year <= 2016][which.max(pmac)]

# min
i <- which.min(PeriodMeasures$pmac)
PeriodMeasures[i]

# max period sd in 1775
i <- which.max(PeriodMeasures$psd)
PeriodMeasures[i]

# lowest sd in 1975
i <- which.min(PeriodMeasures$psd)
PeriodMeasures[i]

# 1887 largest birth cohort 140095
i <- which.max(PeriodMeasures$Bt)
PeriodMeasures[i]

# 1736 smallest cohort 50892
i <- which.min(PeriodMeasures$Bt)
PeriodMeasures[i]

# -----------------------------------
# projected 2015
i <- which.max(CohortMeasures$pmac)
CohortMeasures[i]

# 1822 cohort for non-projected cohorts
CohortMeasures[CohortMeasures$Cohort < 1990][which.max(pmac)]

# 1943 cohort had lowest sd of 5.4
i <- which.min(CohortMeasures$pmac)
CohortMeasures[i]

# highest sd 1732 cohort 6.7
i <- which.max(CohortMeasures$psd)
CohortMeasures[i]

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
