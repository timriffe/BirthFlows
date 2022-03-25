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

source(here("R","Functions.R"))
source(here("R","Method13_deBeer1985and1989-swe.R"))
set.seed(1)
# -------------------------------------------------

# Total births for single years 1736 to 1775
oldtotals <- read.csv(here("Data","HistoricalTotals1736to1775.csv"))

# oldtotals %>% 
#   ggplot(aes(x=Year,y=Births)) +
#   geom_line() +
#   labs(title = "Total births by year, used as constraint",
#        subtitle ="The '1773 in Sweden' wikipedia entry includes:
#        'The population death rate doubles, due to famine and
#        dysentery caused by crop failures in the previous years.'\nThe '1743 in Sweden' entry lists internal conflicts")

# --------------------------------------------------------
# this object useful for years 1736-1775

ASFR <- readRDS(here("Data","ASFR_1751to1775.rds"))
# ASFR %>% 
#   ggplot(aes(x=Age,y=ASFR, color = Year, group = Year)) +
#   geom_line()

# lifetables for retrojection
lt   <- readRDS(here("Data","lt_1751to1755.rds"))
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
  group_by(Year) %>% 
  # the exercise:
  mutate(B1.3 = Exposure * masfr,
         B1.3Tot = sum(B1.3, na.rm = TRUE),
         cof.3 = Births / B1.3Tot,
         Births = B1.3 * cof.3) %>% 
  ungroup() %>% 
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
  readRDS(here("Data","Expos1751to1774.rds")) %>% 
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
  read_csv(here("Data","SWEbirths.txt"), 
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
SWE_HFD     <- readHFD(here("Data","SWEbirthsVV.txt"))
SWE_HFD$OpenInterval <- NULL

# ---------------------------------
# sort columns too
SWEh1   <- SWEh1 %>% 
           select(Year, ARDY, Cohort, Total)

# stack files
SWE     <- bind_rows(SWEh1, SWE_HFD)
SWE     <- as.data.frame(SWE)
# PCmask[yrs >= maxYr, ]   <- 0
# PCmask[yrs < maxYr, ]    <- 1
# PCmask[, cohs < maxCoh]  <- 0
# PCmask                   <- !PCmask

SWE <-
  SWE %>% 
  mutate(mask = Year < 1891 & Cohort >=  leftYear(SWE))
        
#head(SWE[is.na(SWE$Total),],20)

# save intermediate data object. This one hasn't
# received the perturbation yet
saveRDS(SWE, file = here("Data","SWE_sm.rds"))


# SWE <- readRDS(here::here("Data","SWE_sm.rds"))
# TODO 
# double check date-range selection and time reference matching

# adjust mother cohort size based on first diffs in daughter cohort size
cat("Adjusting graduated data...\n")
(span  <- optimize(minspan, interval = c(.01,.8), SWE = SWE, maxit = 500)) # 0.09068448
                                                                           # 0.5838217
                                                                           # 0.6248677

SWE    <- pertspan(SWE, span = span$minimum, maxit = 5000) %>% 
  arrange(Year, Cohort) %>% 
  select(Year, Age = ARDY, Cohort, Births = Bhat)

# return to long format for purposes of joining:


# -------------------------------------------- #
# insert forecast by CB                        #
# -------------------------------------------- #

# read in data objects as required.
#data <- read.table("Data/forecast/asfrRR.txt",skip=2,as.is=T, header=TRUE)
#data <- data[data$Code == "SWE", ]
#data <- write.table(data,file="Data/forecast/SWEasfrRR.txt",row.names=FALSE#)
ASFRlong          <- 
  read.table(
    here("Data","forecast","SWEasfrRR.txt"), 
    stringsAsFactors = FALSE, 
    header = TRUE,
    skip=2) %>% 
  # recode Age from HFD standard
  mutate(Age = case_when(Age == "12-" ~ "12",
                         Age == "55+" ~ "55",
                         TRUE ~ Age),
         Age = as.integer(Age)) 

p16 <- make_asfr_proj(ASFRlong, jump_off_year = 2016) %>% 
  rename(`2016` = ASFR)
p17 <- make_asfr_proj(ASFRlong, jump_off_year = 2017)%>% 
  rename(`2017` = ASFR)
p18 <- make_asfr_proj(ASFRlong, jump_off_year = 2018)%>% 
  rename(`2018` = ASFR)
p19 <- make_asfr_proj(ASFRlong, jump_off_year = 2019)%>% 
  rename(`2019` = ASFR)
p20 <- make_asfr_proj(ASFRlong, jump_off_year = 2020)%>% 
  rename(`2020` = ASFR)

left_join(p16,p17,by =c("Age","Year")) %>% 
  left_join(p18,by =c("Age","Year")) %>% 
  left_join(p19,by =c("Age","Year")) %>% 
  left_join(p20,by =c("Age","Year")) %>% 
  dplyr::filter(Year > 2016) %>% 
  pivot_longer(`2016`:`2020`, names_to = "variant", values_to = "ASFR") %>% 
  group_by(Year, variant) %>% 
  summarize(TFR = sum(ASFR)) %>% 
  ggplot(aes(x = Year, y = TFR, color = variant)) +
  geom_line()

# --------------------------------- #
# read in SCB population projection #
# --------------------------------- #

PProj           <- 
  readr::read_csv(here::here("Data","forecast","scb-females-projected-swe.csv"),
                  show_col_types = FALSE) %>% 
  pivot_longer(`2021`:`2120`, names_to = "Year", values_to = "Pop") %>% 
  select(Year, Age = age, Pop) %>% 
  mutate(Age = substr(Age,1,2) %>% as.integer(),
         Year = as.integer(Year))

SWE_proj <-
  left_join(PProj,
            p18, 
            by = c("Year","Age")) %>% 
  arrange(Year, Age) %>% 
  rename(ASFR = `2018`) %>% 
  mutate(Births = ASFR * Pop) %>% 
  group_by(Year) %>% 
  RR2VV() %>% 
  mutate(Cohort = Year - Age)%>% 
  select(Year, Age, Cohort, Births) %>% 
  dplyr::filter(Cohort <= 2020)
# end forecast chunk
# -------------------------------------------- #


# combine data objects
SWE           <- bind_rows(SWE, 
                           SWE_proj)


PC         <- acast(SWE, Year~Cohort, value.var = "Births", fill = 0)

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

