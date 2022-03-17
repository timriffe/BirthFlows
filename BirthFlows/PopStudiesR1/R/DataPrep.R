# TODO: decide how to weight sd vs slope in lm()
# planned solution: weight such that series is on average centered.

# download birthsVV (period-cohort Lexis shape for birth counts) from HFD. 
library(viridis)
library(HMDHFDplus)
library(ungroup)
library(reshape2)
library(data.table)
library(DemoTools)
library(minpack.lm)
library(here)
library(tidyverse)

source(here("BirthFlows","PopStudiesR1","R","Functions.R"))
source(here("BirthFlows","PopStudiesR1","R","Method13_deBeer1985and1989-swe.R"))
set.seed(1)
# -------------------------------------------------

# Total births for single years 1736 to 1775
oldtotals <- read.csv(here("BirthFlows","Data","HistoricalTotals1736to1775.csv"))

# oldtotals %>% 
#   ggplot(aes(x=Year,y=Births)) +
#   geom_line() +
#   labs(title = "Total births by year, used as constraint",
#        subtitle ="The '1773 in Sweden' wikipedia entry includes:
#        'The population death rate doubles, due to famine and
#        dysentery caused by crop failures in the previous years.'\nThe '1743 in Sweden' entry lists internal conflicts")

# --------------------------------------------------------
# this object useful for years 1736-1775

ASFR <- readRDS(here("BirthFlows","Data","ASFR_1751to1775.rds"))
# ASFR %>% 
#   ggplot(aes(x=Age,y=ASFR, color = Year, group = Year)) +
#   geom_line()

# lifetables for retrojection
lt   <- readRDS(here("BirthFlows","Data","lt_1751to1755.rds"))
LT   <- acast(lt, Age~Year, value.var = "lx")

# Read in year 1751 female census (July 1, proxy for exposure) HMD
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
		            smooth_age_5(Pt, A, 
		                    ageMin = 5, 
		                    ageMax = 80,
				                method = "United Nations", 
					              young.tail = "Original", 
					              old.tail = "Original")[-1])
	
# 2) graduate to single ages
smoothed1      <- graduate(smoothed5, Age = A, OAG = TRUE, method = "Sprague")

# 3) and get back original totals for ages 0-4. Plays no role in
# retrojection, just a perfectionist detail.
smoothed1      <- rescaleAgeGroups(
		                Value1 = smoothed1, 
		                AgeInt1 = rep(1,length(smoothed1)), 
		                Value2 = smoothed5, 
		                AgeInt2 = AI, 
		                splitfun = graduate_uniform,
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

# average standardized ASFR over the period. Very stable age pattern.
masfr <- ASFR %>% 
  select(Year, Age, ASFR) %>% 
  dplyr::filter(Age >= 15 & Age < 50) %>% 
  group_by(Year) %>% 
  mutate(sasfr = ASFR / sum(ASFR, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Age) %>% 
  summarize(masfr = mean(sasfr), 
            .groups = 
              "drop") 
#
# ggplot(masfr,aes(x = Age, y = masfr)) + 
#   geom_line() + 
#   labs(title = "standard age distribution of fertility rates",
#        subtitle = "this is rescales to estimated TFR\nfor years 1736-1750")

# Now reconstruct births
B_1736to1750  <- 
  ExRetro %>% 
  as_tibble() %>% 
  tibble::rownames_to_column("Age") %>% 
  mutate(Age = as.integer(Age)) %>% 
  pivot_longer(2:ncol(.), names_to = "Year", values_to = "Exposure") %>% 
  mutate(Year = as.integer(Year)) %>% 
  inner_join(masfr, by = "Age") %>%      # created above
  left_join(oldtotals, by = "Year") %>%  # as read in31 
  # the exercise:
  mutate(B1.3 = Exposure * masfr,
         B1.3Tot = sum(B1.3, na.rm = TRUE),
         cof.3 = Births / B1.3Tot,
         Births = B1.3 * cof.3) %>% 
  select(Year, Age, Births) %>% 
  arrange(Year, Age) %>% 
  group_by(Year) %>% 
  RR2VV() # split to PC shape (VV)
# 
# B_1736to1750 %>% 
#   ggplot(aes(x = Age, y = Births, group = Year)) + 
#   geom_line()

# ----------------------------------------------#
# Now adjustments for births in years 1751-1774 #
# ----------------------------------------------#

ASFR <-
 ASFR %>% 
 mutate(Y5 = Year - Year %% 5) %>% 
 select(Y5, Age, ASFR)

B_1751to1774 <-
  readRDS(here("BirthFlows","Data","Expos1751to1774.rds")) %>% 
  filter(Year < 1775,
         Age >= 15,
         Age <= 49) %>% 
  mutate(Y5 = Year - Year %% 5) %>% 
  left_join(ASFR, by = c("Y5","Age")) %>% 
  select(Year, Age, Female, ASFR) %>% 
  replace_na(list(ASFR = 0)) %>% 
  left_join(oldtotals, by = "Year") %>% 
  group_by(Year) %>% 
  mutate(BAP = Female * ASFR,
         BAPT = sum(BAP, na.rm = TRUE),
         cof = Births / BAPT,
         Births = BAP * cof) %>% 
  select(Year, Age, Births) %>% 
  group_by(Year) %>% 
  RR2VV() %>% 
  ungroup() 

# -----------------------------------------#
# append and standardize further           #
# -----------------------------------------#
B_oldold <- 
  B_1736to1750 %>% 
  bind_rows(B_1751to1774) %>% 
  mutate(Cohort = Year - Age,
         Lexis = NULL) %>% 
  rename(ARDY = Age, 
         Total = Births)
# -------------------------------------------
# these are the historical births from SK, 1775-1890
# TR: verify this still runs, and make it work with following code.

SWEh1  <-  
  suppressMessages(
  read_csv(here("BirthFlows","Data","SWEbirths.txt"), 
           na = ".")) %>% 
  dplyr::filter(Year < 1891,
         Age != "TOT",
         is.na(Note2)) %>% 
  group_by(Year) %>% 
  b_unk() %>% 
  do(graduate_chunk(chunk = .data)) %>% 
  RR2VV() %>% 
  ungroup() %>% 
  rename(ARDY = Age, 
         Total = Births) %>% 
  select(Year, ARDY, Total) %>% 
  mutate(Cohort = Year - ARDY) 


#---------------------------------------------------#
# Append all historical data                        #
#---------------------------------------------------#

SWEh1 <- bind_rows(B_oldold, SWEh1)
# SWEh1 is the entire pre-HFD series.
#---------------------------------------------------#

# This is from the full-HFD zip file:
SWE     <- readHFD(here("BirthFlows","Data","SWEbirthsVV.txt"))
SWE$OpenInterval <- NULL

# ---------------------------------
# sort columns too
SWEh1   <- SWEh1 %>% 
           select(Year, ARDY, Cohort, Total)

# stack files
SWE     <- bind_rows(SWEh1, SWE)
SWE     <- as.data.frame(SWE)

#head(SWE[is.na(SWE$Total),],20)

# save intermediate data object. This one hasn't
# received the perturbation yet
saveRDS(SWE, file = here("BirthFlows","Data","SWE_sm.rds"))

# TODO 
# troubleshoot pertspan and friends

# adjust mother cohort size based on first diffs in daughter cohort size
cat("Adjusting graduated data...\n")
(span  <- optimize(minspan, interval = c(.01,.8), SWE = SWE, maxit = 500)) # 0.09068448
                                                                           # 0.5838217
                                                                           # 0.3101669
PCi    <- pertspan(SWE, span = span$minimum, maxit = 5000)

# return to long format for purposes of joining:
SWE <- melt(PCi, varnames = c("Year","Cohort"), value.name = "Total")
SWE <- SWE[order(SWE$Year,SWE$Cohort),]

# -------------------------------------------- #
# insert forecast by CB                        #
# -------------------------------------------- #

# read in data objects as required.
#data <- read.table("Data/forecast/asfrRR.txt",skip=2,as.is=T, header=TRUE)
#data <- data[data$Code == "SWE", ]
#data <- write.table(data,file="Data/forecast/SWEasfrRR.txt",row.names=FALSE#)
asfr          <- 
  read.table(
    here("Data","forecast","SWEasfrRR.txt"), 
    stringsAsFactors = FALSE, 
    header = TRUE)
			
# recode age from HFR standard
i12           <- asfr$Age == "12-"
i55           <- asfr$Age == "55+"
asfr$Age[i12] <- "12"
asfr$Age[i55] <- "55"
asfr$Age      <- as.integer(asfr$Age)

# reshape to AP matrix
ASFR          <- acast(asfr, Age~Year, value.var = "ASFR")

swe_base_period_50_a <- Method13_deBeer1985and1989.R(ASFR,
		joy = 2016, 
		obs = 50,
		age1 = 12, 
		age2 = 55,
		parameter = c(1,0,0,1,0,0),
		len = 100,
		pop="SWE")

swe_obs_pred_ASFR_base_period_50_a  <- as.matrix(cbind(swe_base_period_50_a$obsASFR[,as.character(1891:2016)],swe_base_period_50_a$predASFR[,as.character(2017:2116)]))
swe_obs_pred_CASFR_base_period_50_a <- asfr_period_to_cohort(as.matrix(swe_obs_pred_ASFR_base_period_50_a))

# --------------------------------- #
# read in SCB popualtion projection #
# --------------------------------- #

PProj           <- 
  read.table(
    here(,"Data","forecast","scb-females-projected-swe.csv"),
	  skip = 0,
	  as.is = TRUE, 
	  header = TRUE)
PProj           <- as.vector(PProj)[, 4:106]
rownames(PProj) <- 0:105
colnames(PProj) <- 2018:2120

# get 2017 "observed" population from SCB:
#P2017 <- read.table("Data/forecast/scb-females-observed-swe.csv",  
#		skip = 0,
#		as.is = TRUE, 
#		header = TRUE)
#head(data)
#data$age
#
#P2017           <- as.vector(P2017)[,3:(length(1860:2017)+2)]
#rownames(P2017) <- 0:100
#colnames(P2017) <- 1860:2017
#P2017           <- P2017[, ncol(P2017)]
#
#dput(P2017)
P2017 <- c(56715L, 59444L, 58123L, 59233L, 58684L, 59279L, 58916L, 61077L, 
		59410L, 58612L, 57936L, 57688L, 55943L, 55436L, 54893L, 53557L, 
		51353L, 51508L, 51083L, 51275L, 52151L, 54920L, 58872L, 64263L, 
		67178L, 70320L, 72030L, 73732L, 70759L, 69794L, 67193L, 66138L, 
		64686L, 62531L, 61410L, 61495L, 61178L, 63145L, 61249L, 59582L, 
		60163L, 60791L, 63208L, 65995L, 64867L, 65858L, 65698L, 64129L, 
		62766L, 65367L, 67947L, 68266L, 68353L, 68215L, 62984L, 60368L, 
		58228L, 57689L, 57304L, 57120L, 57803L, 57102L, 56556L, 55026L, 
		55930L, 55289L, 54520L, 56251L, 58049L, 59699L, 59444L, 59301L, 
		57810L, 55893L, 51746L, 46580L, 40689L, 38033L, 37459L, 35393L, 
		32489L, 30728L, 27919L, 26285L, 24448L, 23638L, 22099L, 20605L, 
		17953L, 16422L, 14240L, 12151L, 10369L, 8452L, 6580L, 4947L, 
		4073L, 3039L, 1688L, 1043L, 1732L)
names(P2017) <- 0:100

# -----------------------------------
# stick denoms together
asel                <- as.character(12:55)
DenomProj           <- cbind(P2017[asel], as.matrix(PProj[asel, ]))
colnames(DenomProj) <- 2017:2120
# derive birth counts

yrsel         <- sort(intersect(
				    colnames(DenomProj), 
				    colnames(swe_obs_pred_ASFR_base_period_50_a)))
Bproj         <- swe_obs_pred_ASFR_base_period_50_a[, yrsel] * DenomProj[, yrsel]

# reshape to long format
Bproj         <- melt(Bproj, varnames = c("Age", "Year"), value.name = "Births")

# shift AP into VV
Bproj         <- data.table(Bproj)
Bproj         <- Bproj[,RR2VV(.SD), by = list(Year)]
Bproj$Cohort  <- Bproj$Year - Bproj$Age
colnames(Bproj)[colnames(Bproj) == "Births"] <- "Total"
Bproj         <- Bproj[,c("Year","Cohort","Total")]

# combine data objects
SWE           <- rbind(SWE, Bproj)

# throw away forecast beyond 2016 cohort
SWE           <- SWE[SWE$Cohort <= 2016, ]
SWE$ARDY      <- SWE$Year - SWE$Cohort
SWE           <- SWE[SWE$ARDY > 10 & SWE$ARDY < 60, ]
# end forecast chunk
# -------------------------------------------- #

PC         <- acast(SWE, Year~Cohort, value.var = "Total", fill = 0)

# save out in long format:
SWE        <- melt(PC,varnames=c("Year","Cohort"),value.name="Total")
SWE$ARDY <- SWE$Year - SWE$Cohort 
saveRDS(SWE, file = here("Data","SWE_final.Rdata"))

# -------------------------------------------- #

Bt     <- rowSums(PC) # same as previous
Bc     <- colSums(PC)

#plot(1736:2016,Bt,type='l',xlim=c(1687,2016),ylim=c(-150000,150000))
#lines(1687:2004,-Bc)


cat("Creating figure data objects...\n")

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

BT      <- colSums(PC5)
BC      <- colSums(P5C) 

yrs     <- leftYear(SWE):max(SWE$Cohort)
yrsc    <- as.character(yrs)

# ratio of offspring to original cohort size
# defines meander
meander           <- BC[yrsc] / BT[yrsc] 
which(abs(meander)>2)
# pad left w average crude replacement of first 10 cohorts
start             <- mean(meander[1:10])

# pad right w average crude replacement of last 5 cohorts
end               <- mean(meander[(length(meander)-5):length(meander)])

# how far left and right do we need to pad?
Nstart            <- min(Yrs) - min(Cohs) 
Nend              <- max(Yrs) - max(Cohs)

# concatenate the raw meander
meander_extended  <- c(rep(start,Nstart),meander,rep(end,Nend))

# get x variable
yrs_smooth        <- min(Cohs):max(Yrs)

# smooth the meander
meander_smoothed  <- smooth.spline(x = yrs_smooth, y = log(meander_extended), lambda = 1/1e5)$y
names(meander_smoothed) <- yrs_smooth

#plot(yrs_smooth, log(meander_extended))
#lines(yrs_smooth,meander_smoothed)
#abline(h=0)
# -------------------------------
# the meander is just relative now, so it needs
# to be scaled.
multiply         <- 1e5
baseline         <- meander_smoothed * multiply
# -------------------
# shift flow
PC5cs2 <- t(t(PC5cs) + baseline[colnames(PC5cs)])
P5Ccs2 <- t(t(P5Ccs) - baseline[colnames(P5Ccs)])


# --------------------------------------------------
# get family coords, maybe swap out this family
Lineage                      <- read.csv("Data/Swedishfamilybranch.csv")
Lineage$year.daughters.birth <- c(Lineage$date.of.birth[-1],NA)
Lineage$year.mothers.birth   <- c(1757,Lineage$date.of.birth[-5])
Lineage$age.of.mother        <- c(30,Lineage$age.at.daughters.birth[-5])
colnames(Lineage) <- c("first","last","C","AB","DC","MC","AM")
# Now. assign mother age quantile to birth?
# i <- 1
Lineage$ytop    <- NA
Lineage$ybottom <- NA
for (i in 1:5){
	ybase <- baseline[as.character(Lineage$C[i])]
	ind1 <- SWE$Year == Lineage$C[i]
	B <- sum(SWE$Total[ind1])
	Bx <- cumsum(SWE$Total[ind1])
	ages <- SWE$ARDY[ind1]
	By <- splinefun(Bx ~ I(ages))(Lineage$AM[i])
	Lineage$ytop[i] <- ybase + B - By
	
	# now bottm y coord
	ind2 <- SWE$Cohort == Lineage$C[i]
	Bx   <- cumsum(SWE$Total[ind2])
	ages <- SWE$ARDY[ind2]
	
	By <- splinefun(Bx ~ I(ages))(Lineage$AB[i])
	Lineage$ybottom[i] <- ybase - By
}
L1 <- Lineage
# another one to try
Lineage                      <- 
  read.csv(here("Data","Swedishfamilybranch2.csv"))
Lineage$year.daughters.birth <- c(Lineage$date.of.birth[-1],NA)
Lineage$year.mothers.birth   <- c(1800,Lineage$date.of.birth[-6])
Lineage$age.of.mother        <- c(29,Lineage$age.at.daughters.birth[-6])
colnames(Lineage)            <- c("first","last","C","AB","DC","MC","AM")
# Now. assign mother age quantile to birth?
# i <- 1
Lineage$ytop    <- NA
Lineage$ybottom <- NA
for (i in 1:5){
	ybase <- baseline[as.character(Lineage$C[i])]
	ind1  <- SWE$Year == Lineage$C[i]
	B     <- sum(SWE$Total[ind1])
	Bx    <- cumsum(SWE$Total[ind1])
	ages  <- SWE$ARDY[ind1]
	By    <- splinefun(Bx ~ I(ages))(Lineage$AM[i])
	#Lineage$ytop[i] <- ybase + B - By
	Lineage$ytop[i] <- ybase + By
	# now bottm y coord
	ind2  <- SWE$Cohort == Lineage$C[i]
	Bx    <- cumsum(SWE$Total[ind2])
	ages  <- SWE$ARDY[ind2]
	
	By    <- splinefun(Bx ~ I(ages))(Lineage$AB[i])
	Lineage$ybottom[i] <- ybase - By
}

L2 <- Lineage

# coloring properties (SD)
cohNA          <- Cohs < 1720 
perNA          <- Yrs > 2030
Coh_SD         <- apply(PC, 2, wsd, x = Yrs + .5)
Coh_SD[cohNA]  <- NA
Per_SD         <- apply(PC, 1, wsd, x = Cohs + .5)
Per_SD[perNA]  <- NA

Per_SD5   <- groupN(Per_SD, y = Yrs, n = 5, fun = mean)
Coh_SD5   <- groupN(Coh_SD, y = Cohs, n = 5, fun = mean)


cat("DataPrep.R all done!\n")

