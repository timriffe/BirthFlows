
setwd("/home/tim/git/BirthFlows/BirthFlows")
# download birthsVV (period-cohort Lexis shape for birth counts) from HFD. 
library(RColorBrewer)
library(HMDHFDplus)
library(pclm)
library(reshape2)
library(data.table)

groupN <- function(x,y,n){
	tapply(x, y - y %% n, sum)
}
#devtools::install_github("mpascariu/pclm")
# step 1, redistribute UNK
b_unk <- function(Chunk){
	if (! "UNK" %in% Chunk$Age){
		return(Chunk)
	}
	UNK <- Chunk$Births[Chunk$Age == "UNK"]
	Chunk <- Chunk[Chunk$Age != "UNK", ]
	w <- Chunk$Births / sum(Chunk$Births)
	Chunk$Births <- Chunk$Births + w * UNK
	Chunk
}


graduatechunk <- function(chunk){
	if (length(unique(chunk$Vital))>1){
		chunk <- chunk[chunk$Vital == min(chunk$Vital),]
	}
	a <- as.integer(chunk$Age)
	if (a[1] == 19){
		a[1] <- 15
	}
	minA    <- min(a)
	add     <- rev(diff(a))[1]
	a[length(a)+1] <- a[length(a)] + add
	
	B       <- chunk$Births
	breaks  <- a - minA
	bout    <- pclm(dta = B, breaks)
	Age 	<- minA:(max(a)-1)
	N 		<- length(Age)
	out <- data.frame(PopName=rep("SWE",N),
					  Year=rep(unique(chunk$Year),N),
					  Age = Age,
					  AgeInt = rep(1,length(Age)),
					  Lexis = rep("RR",N),
					  Births = bout$fitted.values[,1])
	
	out
}

# functions for first-difference matching sort of.
makeMask <- function(PC, maxYr = 1891, maxCoh = 1775){
	PCmask  <- PC
	cohs <- as.integer(colnames(PC))
	yrs  <- as.integer(rownames(PC))
	PCmask[yrs >= maxYr, ]  <- 0
	PCmask[yrs < maxYr, ]   <- 1
	PCmask[, cohs < maxCoh]  <- 0
	PCmask   <- !PCmask
	PCmask
}

constrainBlocks <- function(SWE, PCpert, maxit = 100, tol = .1){
	
	SWE$Age5                  <- SWE$ARDY - SWE$ARDY %% 5
	SWE$Age5[SWE$Age5  == 10] <- 12 
	
	PCin     <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
	yrs      <- as.integer(rownames(PCin))
	cohs     <- as.integer(colnames(PCin))
	PCmask   <- makeMask(PCin)
	PChat    <- melt(PCpert, varnames = c("Year","Cohort"), value.name = "Bhapert")
	SWEmerge <- merge(SWE, PChat, by = c("Year","Cohort"), all.x = TRUE, all.y = FALSE)
	
	SWEmerge <- data.table(SWEmerge)
	SWEmerge[, GroupTot:=sum(Total),by=list(Year,Age5)]
	SWEmerge[, Grouphat:=sum(Bhapert),by=list(Year,Age5)]
	SWEmerge[, Bhat := (Bhapert / Grouphat) * GroupTot, ]
	
	nanor0                <- SWEmerge$Bhat == 0 | is.nan(SWEmerge$Bhat)
	SWEmerge$Bhat[nanor0] <- SWEmerge$Total[nanor0]
	
	PCout         <- acast(SWEmerge, Year ~ Cohort, sum, value.var = "Bhat")
	PCout[PCmask] <- PCin[PCmask]
	
	Bknown <- rowSums(PCin)
	for (i in 1:maxit){
		Bi     <- rowSums(PCout) 
		if (sum(abs(Bi-Bknown))<tol){
			break
		}
		PCout  <- PCout * (Bknown / Bi)
		PCout[PCmask] <- PCin[PCmask]
	}
	
	PCout
}

perturbDiffs <- function(par=.5,SWE){
	PCin   <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
	PC     <- PCin
	PCmask <- makeMask(PCin)
	Bt     <- rowSums(PCin)
	ny     <- length(Bt)
	yrs    <- as.integer(names(Bt))
	yrsi   <- yrs >= 1775 & yrs < 1891
	dBt    <- diff(Bt)
	
	# approach this first difference trend
	rdBt   <- dBt/Bt[-ny]
	# -----------------
	Bc     <- colSums(PCin)
	nc     <- length(Bc)
	cohs   <- as.integer(names(Bc))
	
	cohsi  <- cohs >= 1775 & cohs < 1891
	pertc  <- (1 - (rdBt[yrsi] * par))
	
	PC[, cohsi]     <- t(t(PC[, cohsi]) * pertc)
	PC[PCmask]      <- PCin[PCmask]
	
	# now constrain
	PC <- constrainBlocks(SWE, PCpert = PC)
	PC
}

minpert <- function(par, SWE){
	PC     <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
	
	PC2 <- perturbDiffs(par, SWE)
	Bt  <- rowSums(PC)
	Bc  <- colSums(PC2)
	ny  <- length(Bt)
	nc  <- length(Bc)
	rdt <- diff(Bt) / Bt[-ny]
	rdc <- diff(Bc) / Bc[-nc]
	
	# canonical values
	compgen <- as.character(1891:1959)
	cr      <- cor(rdt[compgen], rdc[compgen])
	slp     <- lm(rdt[compgen]~ rdc[compgen])$coef[2]
	
	# perturbed values
	pertgen <- as.character(1776:1890)
	cr_p      <- cor(rdt[pertgen], rdc[pertgen])
	slp_p     <- lm(rdt[pertgen]~ rdc[pertgen])$coef[2]
	
	# let's see how close we can get
	(cr - cr_p)^2 + (slp - slp_p)^2
}
# -------------------------------------------------


SWEh  <- read.csv("Data/SWEbirths.txt",na.strings = ".")
SWEh  <- SWEh[SWEh$Year < 1891, ]
# remove TOT, not useful
SWEh  <- SWEh[SWEh$Age != "TOT", ]
SWEh  <- data.table(SWEh)
SWEh  <- SWEh[, b_unk(.SD), by = list(Year)]
SWEh1 <- SWEh[, graduatechunk(.SD), by = list(Year)]
SWEh1 <- data.frame(SWEh1)
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


# mean(abs(BCrel3-MCrel3))
SWEh1   <- SWEh1[,colnames(SWE)]

SWE     <- rbind(SWEh1,SWE)

save(SWE, file = "Data/SWE.Rdata")

# adjust mother cohort size based on first diffs in daughter cohort size
# 

mod    <- optimize(minpert, interval = c(.3,3), SWE = SWE)
PC     <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
PCout  <- perturbDiffs(par = mod$min, SWE)
Bt     <- rowSums(PC)
Bc     <- colSums(PCout)

# rdt <- diff(Bt) / Bt[-ny]
# rdc <- diff(Bc) / Bc[-nc]
# pertgen <- as.character(1776:1890)
# plot(rdt[pertgen],rdc[pertgen])
# abline(a=0,b=1)

# plot(yrs, rowSums(PCout), type = 'l', ylim = c(-2e5,2e5), xlim = c(1720,2015))
# lines(cohs, - colSums(PCout))
# lines(cohs, - colSums(PC))
# plot(cohs,colSums(PC),type='l')
# lines(cohs, colSums(PCout))
# abline(v=1775)
# abline(v=1891)
# 
PC <- PCout

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
BT <- colSums(PC5)
BC <- colSums(P5C) 

yrs  <- 1775:1968
yrsc <- as.character(yrs)

meander <- BC[yrsc] / BT[yrsc] 
start   <- meander[1]
end     <- meander[length(meander)]

Nstart <- min(yrs) - min(Cohs) 
Nend   <- max(Yrs) - max(yrs)
meander_extended <- c(rep(start,Nstart),meander,rep(end,Nend))

yrs_smooth       <- min(Cohs):max(Yrs)

meander_smoothed <- smooth.spline(x = yrs_smooth, y =meander_extended, lambda = .00001)$y
names(meander_smoothed) <- yrs_smooth

subt <- (meander_smoothed[1] + meander_smoothed[length(meander_smoothed)]) / 2
meander_smoothed <- (meander_smoothed - subt) * 2 + subt
# plot(yrs_smooth, meander_smoothed, ylim = c(.2,3))
# lines(yrs_smooth,(meander_smoothed - subt) * 2 + subt)


#PC5[,"1907"]
