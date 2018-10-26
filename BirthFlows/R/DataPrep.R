# TODO: decide how to weight sd vs slope in lm()

setwd("/home/tim/git/BirthFlows/BirthFlows")
# download birthsVV (period-cohort Lexis shape for birth counts) from HFD. 
library(RColorBrewer)
library(HMDHFDplus)
library(ungroup)
library(reshape2)
library(data.table)
library(DemoTools)

# -------------------------------------------------

# Total births for single years 1736 to 1775
oldtotals <- read.csv("Data/HistoricalTotals1736to1775.csv")
#plot(oldtotals$Year,oldtotals$Births)

# --------------------------
# deprecated test code
# These are single-age ASFR, in 5-year groups. Graduated
# by HFC, data originally in 5x5 Lexis cells. Years 1751+
# ASFR  <- readHFCweb("SWE","ASFRst
#save(ASFR,file = "Data/ASFR_1751to1775.Rdata")and_TOT")
# ASFR  <- ASFR[ASFR$Year <= 1775,]
#lt <- readHMDweb("SWE","fltper_1x1",username = us, password = pw)
#lt <- lt[lt$Year <= 1755, ]
#Expos <- readHMDweb("SWE","Exposures_1x1",username = us, password = pw)
#Expos <- Expos[Expos$Year < 1775, ]
#save(Expos,file="Data/Expos1751to1774.Rdata")
#save(lt,file = "Data/lt_1751to1755.Rdata")
#Pop <- readHMDweb("SWE","Population",username = us, password = pw)
#devtools::load_all("/home/tim/git/IDButils/IDButils/IDButils")
#Ex   <- local(get(load("Data/Expos1751.Rdata")))$Female
#Pin  <- readIDBcurrent("SWE","pop",full.path = "/home/tim/git/BirthFlows/BirthFlows/Data/SWEpop.txt")
#Pt   <- Pin[Pin$Year == 1751 & Pin$Sex == "f", ]$Population
# --------------------------

# this object useful for years 1736-1775
ASFR <- local(get(load("Data/ASFR_1751to1775.Rdata")))

# lifetables for retrojection
lt   <- local(get(load("Data/lt_1751to1755.Rdata")))
LT   <- acast(lt, Age~Year, value.var = "lx")
# Read in year 1751 female census (July 1, proxy for exposure)
Pt   <- c(27127L, 49895L, 47264L, 89148L, 88017L, 83206L, 81877L, 75966L, 
		64870L, 55368L, 50642L, 43647L, 43411L, 33508L, 39769L, 28201L, 
		19507L, 9273L, 6352L, 2004L, 1039L)
# age and age intervals, could also come from data..
A    <- c(0, 1, 3, seq(5, 90, by = 5))
AI   <- c(diff(A), 1)

#splitMono(Pt, AgeInt = c(1,2,2,rep(5,17),1), OAG=TRUE)

# ---------------------------------------------
# adjust 1751 midyear population to use a jump-off for retrojection
# 1) smooth over 5-year age groups
smoothed5      <- c(Pt[1:3],
		            agesmth(Pt, A, ageMin = 5, ageMax = 80,
				    method = "United Nations", 
					young.tail = "Original", old.tail = "Original")[-1])
# 2) graduate to single ages
smoothed1      <- sprague(smoothed5, Age = A, OAG=TRUE)

# 3) and get back original totals for ages 0-4. Plays no role in
# retrojection, just a perfectionist detail.
smoothed1      <- rescaleAgeGroups(
		            Value1 = smoothed1, 
		            AgeInt1 = rep(1,length(smoothed1)), 
		            Value2 = smoothed5, 
		            AgeInt2 = AI, 
		            splitfun = splitUniform,
					recursive = TRUE)
# ---------------------------------------------

# parameters and container for retrojection
n        <- 1:60
hyrs     <- 1736:1750
ExRetro  <- matrix(0, nrow = 60, ncol = length(hyrs), dimnames = list(0:59, hyrs)) 
# loop back 1 == 1750, 15 = 1736
for (i in 1:15){
	# ratx() is lagged ratios
	ExRetro[, 16-i] <- ratx(rowMeans(LT), -i)[n] * shift.vector(smoothed1, -i)[n]
}

# recast fertility in AP matrix form
asfr        <- acast(ASFR, Age~Year,value.var = "ASFR")
asfr        <- asfr[as.character(15:49), ] # NAs in open ages, cut off
# average standardized ASFR over the period. Very stable age pattern.
masfr       <- colMeans(t(asfr) / colSums(asfr))
# first-pass implied totals (note masfr implied tfr of 1 each year)
B1.3        <- ExRetro[16:50,] * masfr
# ratios to inflate birth counts = implied TFR per year. 
cof.3       <- oldtotals$Births[oldtotals$Year < 1751] / colSums(B1.3)
# rescale first pass of births by age, save this object to append to historical series.
Boldold_AP  <- t(B1.3) * cof.3

# ---------------------------------
# Now adjustments for births in years 1751-1774
Expos <- local(get(load("Data/Expos1751to1774.Rdata")))

# get female exposure in AP matrix:
ExAP   <- acast(Expos[Expos$Year < 1775, ], Age~Year, value.var = "Female")[16:50, ]
asfrAP <- t(apply(asfr,1,rep,times=c(rep(5,4),4)))
BAP    <- ExAP * asfrAP
Bobs   <- oldtotals$Births[oldtotals$Year >= 1751 & oldtotals$Year < 1775] 
cof    <- Bobs / colSums(BAP)
B_1751to1774 <- t(t(BAP) * cof)
# STEPS:
# 1) given asfr, uniform over 5-year Year-intervals, what are
# predicted birth counts for each year? \hat{B(t)}
# 2) rescale to observed totals, multiplying all age groups by same factor.
# \hat{B(a,t)} * B(t) / \hat(B(t)}
# 3) B(a,t) is in ACY, so use RR2VV to split to VV and add on.

# 4) are shocks already imlied? I think so.. So what join in 1775 looks like:
# 2 estimates for 1775,1776,1777,1778,1779, do spot check

# STEP 2:
# consider age-breakdown of births for years 1736-1750. 
# lx-inflate year 1751 population backwards.



# these are the historical births
SWEh  <- read.csv("Data/SWEbirths.txt",na.strings = ".")
SWEh  <- SWEh[SWEh$Year < 1891, ]

# remove TOT, not useful
SWEh  <- SWEh[SWEh$Age != "TOT", ]
SWEh  <- data.table(SWEh)

# step 1, redistribute births of unknown maternal age
SWEh  <- SWEh[, b_unk(.SD), by = list(Year)]
cat("Graduating historical data to single ages...\n")
# step 2, graduate to single ages
SWEh1 <- SWEh[, graduatechunk(.SD), by = list(Year)]

# step 3, shift to PC (VV)
SWEh1 <- SWEh1[,RR2VV(.SD), by = list(Year)]

# tidy the data obeject to be able to join it to the
# already-in-shape HFD file for years 1891+
SWEh1         <- data.frame(SWEh1)
SWEh1$PopName <- NULL
SWEh1$Year.1  <- NULL
SWEh1$Lexis   <- NULL
SWEh1$AgeInt  <- NULL
cnames        <- colnames(SWEh1)
SWEh1$Cohort  <- SWEh1$Year - SWEh1$Age
colnames(SWEh1)[colnames(SWEh1) == "Age"]    <- "ARDY"
colnames(SWEh1)[colnames(SWEh1) == "Births"] <- "Total"

# This is from the full-HFD zip file:
SWE     <- readHFD("Data/SWEbirthsVV.txt")
SWE$OpenInterval <- NULL

# ---------------------------------
# select like columns
SWEh1   <- SWEh1[,colnames(SWE)]

# stack files
SWE     <- rbind(SWEh1, SWE)

# save intermediate data object. This one hasn't
# received the perturbation yet
save(SWE, file = "Data/SWE.Rdata")

# adjust mother cohort size based on first diffs in daughter cohort size
cat("Adjusting graduated data...\n")
span   <- optimize(minspan, interval = c(.01,.5), SWE = SWE)
PCi    <- pertspan(SWE, span = span$minimum)
(span$minimum)
Bt     <- rowSums(PCi) # same as previous
Bc     <- colSums(PCi)

cat("Creating figure data objects...\n")
PC      <- PCi

minYR   <- min(SWE$Year)
minCoh  <- min(SWE$Cohort)

Yrs     <- sort(unique(SWE$Year))
Cohs    <- sort(unique(SWE$Cohort))


# grouping Births by cohort and year
# 
# single calendar year, 5-year cohort bins
PC5 <- apply(PC,1,function(x, Cohs){
	groupN(x, Cohs, 5)
}, Cohs = Cohs)

# single cohort bin, 5-year calendar bins
P5C <- apply(PC,2,function(x, Yrs){
	groupN(x, Yrs, 5)
}, Yrs = Yrs)

#dimnames(PC5)


PC5cs <- apply(PC5,2,cumsum)
P5Ccs <- apply(P5C,2,cumsum)
PC5cs <- rbind(0,PC5cs)
P5Ccs <- rbind(0,P5Ccs)

# ---------------------
# make a meander:
# let's get a ratio (coh / period):

# ---------------------------------
# TODO:
# 2) optimize so that Bc + Bt is centered on average.

# NOTE: the object used in BirthFlows.R
# is meander_smoothed,
# and it is modified as so:
# meander_smoothed <- meander_smoothed * 7e4
# the value 7e4 can probably be optimized to achieve centering.
# ---------------------------------

BT      <- colSums(PC5)
BC      <- colSums(P5C) 

yrs     <- 1775:rightCoh(SWE,45)
yrsc    <- as.character(yrs)

meander <- BC[yrsc] / BT[yrsc] 
start   <- meander[1]
end     <- meander[length(meander)]

Nstart  <- min(yrs) - min(Cohs) 
Nend    <- max(Yrs) - max(yrs)
meander_extended <- c(rep(start,Nstart),meander,rep(end,Nend))

yrs_smooth       <- min(Cohs):max(Yrs)

meander_smoothed <- smooth.spline(x = yrs_smooth, y = meander_extended, lambda = .00001)$y
names(meander_smoothed) <- yrs_smooth

subt             <- (meander_smoothed[1] + meander_smoothed[length(meander_smoothed)]) / 2
meander_smoothed <- (meander_smoothed - subt) * 2 + subt

# plot(yrs_smooth, meander_smoothed, ylim = c(.2,3))
# lines(yrs_smooth,(meander_smoothed - subt) * 2 + subt)

cat("DataPrep.R all done!\n")

#PC5[,"1907"]
#plot(PC["1850",])
#lines(PCout["1850",])