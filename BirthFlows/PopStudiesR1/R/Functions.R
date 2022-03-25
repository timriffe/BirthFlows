
# Author: tim
###############################################################################
library(HMDHFDplus)
library(ungroup)
library(reshape2)
library(data.table)
library(plotrix)

wmean <- function(x,w){
	sum(x*w) / sum(w)
}
wsd   <- function(x,w){
	mn <- wmean(x,w)
	sqrt(sum((mn-x)^2*(w/sum(w))))
}

IQR <- function(x,w){
	Fx <- cumsum(w) / sum(w)
	pts <- try(splinefun(x~Fx,method="monoH.FC")(c(.25,.75)),silent=TRUE)
	if (class(pts) == "try-error"){
		return(NA)
	}
	diff(pts)
}
draw.fork <- function(x1,x2,x3,y1,y2,y3,y4,...){
	segments(x1,y1,x1,y2,...)
	segments(x2,y2,x3,y2,...)
	segments(x2,y2,x2,y3,...)
	segments(x3,y2,x3,y4,...)
}




# group single ages (useful for rescaling)
groupN <- function(x,y,n,fun=sum){
	tapply(x, y - y %% n, fun)
}


# (adjustment step 1)
# redistribute births of unknown mother age (UNK)
b_unk <- function(Chunk){
	if (! "UNK" %in% Chunk$Age){
		return(Chunk)
	}
  UNK          <- Chunk$Births[Chunk$Age == "UNK"][1]
	Chunk        <- Chunk[Chunk$Age != "UNK", ]
	w            <- Chunk$Births / sum(Chunk$Births)
	Chunk$Births <- Chunk$Births + w * UNK
	Chunk
}
# b_unk <- function(Chunk){
#   if (! "UNK" %in% Chunk$Age){
#     return(Chunk)
#   }
#   UNK <- 
#     Chunk %>% 
#     filter(Age == "UNK") %>% 
#     pull(Births)
#   
#   Chunk <- 
#     Chunk %>% 
#     filter(Age != "UNK") %>% 
#     mutate(w = Births / sum(Births),
#            Births = Births + w * UNK,
#            Age = as.integer(Age)) 
#   return(Chunk)
# }
# # use early version of pclm algorithm to graduate
# re write to use ungroup package
#graduatechunk <- function(chunk){
#	if (length(unique(chunk$Vital))>1){
#		chunk <- chunk[chunk$Vital == min(chunk$Vital),]
#	}
#	# some years have lower age cat <= 19, but we want to extend
#	# lower bound to 15-19. Assume sorted age (they are)
#	a <- as.integer(chunk$Age)
#	if (a[1] == 19){
#		a[1] <- 15
#	}
#	minA    <- min(a)
#	add     <- rev(diff(a))[1]
#	# upper bound of highest age
#	a[length(a)+1] <- a[length(a)] + add
#	
#	B       <- chunk$Births
#	
#	# early implementation of pclm assumed start at age 0...
#	breaks  <- a - minA
#	# this gets the result already (assumes single age output)
#	# also only returned a single vector back then
#	bout    <- pclm(dta = B, breaks)
#	# then shift ages back up
#	Age 	<- minA:(max(a)-1)
#	N 		<- length(Age)
#	out     <- data.frame(
#			          PopName = rep("SWE", N),
#					  Year = rep(unique(chunk$Year), N),
#					  Age = Age,
#					  AgeInt = rep(1, length(Age)),
#					  Lexis = rep("RR", N),
#					  Births = bout$fitted.values[,1])
#	
#	out
#}

bin2age <- function(binchar = "[15,16)"){
	matches <- regmatches(binchar, gregexpr("[[:digit:]]+", binchar))
	as.numeric(unlist(matches))[1]
}
# adjustment step 2
# now uses most recent ungroup package
graduate_chunk <- function(chunk){
	if (length(unique(chunk$Vital))>1){
		chunk <- chunk[chunk$Vital == min(chunk$Vital),]
	}
	#message(chunk$Year[1])
	# some years have lower age cat <= 19, but we want to extend
	# lower bound to 15-19. Assume sorted age (they are)
	
	x       <- as.integer(chunk$Age)
	y       <- chunk$Births
	
	# add small amount to 0s
	y0      <- y == 0
	scaler  <- rep(y,times=diff(c(x,max(x)+5)))
	out0    <- scaler == 0
	y[y0]   <- .1
	
	if (x[1] == 19){
		x[1] <- 15
	}
	
	# how far should we distribute the open age group. This
	# will turn out to be 5 ages every time (50-54)
	nlast   <- 55 - max(x)
	stopifnot(nlast >= 0)
	if (nlast < 1){
		nlast <- 1
	}
	# data now in single ages, not strictly constrained,
	# but we take care of this later.
	
	bout    <- ungroup::pclm(x = x, y = y, nlast = nlast)$fitted
	bout[out0] <- 0
	Age     <- sapply(names(bout),bin2age)
	# then shift ages back up
	
	N 		  <- length(Age)
	out     <- data.frame(
			#Year = rep(chunk$Year[1], N), # not needed in dplyr
			Age = Age,
			Births = bout,
			Lexis = rep("RR", N))
	
	out
}

# Shift to PC parallelograms (adjustment step 3)
RR2VV <- function(chunk){
	n             <- nrow(chunk)
	chunk1        <- chunk
	chunk1$Births <- chunk1$Births / 2
	chunk2        <- chunk1
	chunk2$Births[-1] <- chunk2$Births[-1] + chunk2$Births[-n]
	chunk2$Lexis  <- "VV"
	chunk2
}

# functions for first-difference matching sort of.
makeMask <- function(PC, maxYr = 1891, maxCoh = 1736){
	PCmask                   <- PC
	cohs                     <- as.integer(colnames(PC))
	yrs                      <- as.integer(rownames(PC))
	PCmask[yrs >= maxYr, ]   <- 0
	PCmask[yrs < maxYr, ]    <- 1
	PCmask[, cohs < maxCoh]  <- 0
	PCmask                   <- !PCmask
	PCmask
}

# reverse engineer this to
constrainBlocks <- function(SWE, PCpert, maxit = 100, tol = .1){
	
	# SWE is a file whose totals in 5-year age groups we wish
	# to treat as canonical. It needs to be reshaped to a matrix.
	# PCpert is said matrix, with years < 1891 perturbed to 
	# approximate an echo strength from first order differences
	# in the period birth series. We use SWE to constrain it back,
	# keeping perturbations in single ages.

	SWE <- SWE %>% 
	  mutate(Age5 = ARDY - ARDY %% 5,
	         Age5 = if_else(Age5 == 10, 12, Age5))
	# 1) shape SWE to same dims as PCpert
	PCin   <-
	  SWE %>% 
	  select(-ARDY) %>% 
	  pivot_wider(names_from = "Cohort", values_from = "Total", values_fill = 0) %>% 
	  column_to_rownames("Year") %>% 
	  as.matrix()
	
	# 2) get period and cohort vectors
	yrs      <- as.integer(rownames(PCin))
	cohs     <- as.integer(colnames(PCin))
	
	# 3) create logical mask to delimit cells for rescaling,
	# to be used later (matrix, needed?)
	# PCmask   <- makeMask(PCin, maxYr = 1891, maxCoh = leftYear(SWE))
	PCmask <- SWE %>% 
	  mutate(mask = Year > 1891 | Cohort < leftYear(SWE) )
	# 4) reshape PCpert to long format to merge w SWE

	PChat    <- melt(PCpert, varnames = c("Year","Cohort"), value.name = "Bhapert")
	
	# 5) merge them together
	SWEmerge <- merge(SWE, PChat, by = c("Year","Cohort"), all.x = TRUE, all.y = FALSE)
	
	# 6) change class for easy group operations
	SWEmerge <- data.table(SWEmerge)
	
	# 7) get totals in 5 year age groups 
	# before and after single age perturbation
	SWEmerge[, GroupTot:=sum(Total),by=list(Year,Age5)]    # canonical
	SWEmerge[, Grouphat:=sum(Bhapert),by=list(Year,Age5)]  # to rescale
	
	# 8) rescale to sum properly
	SWEmerge[, Bhat := (Bhapert / Grouphat) * GroupTot, ]
	
	# 9) might induce NaN, etc. Keep orig values if so.
	nanor0                <- SWEmerge$Bhat == 0 | is.nan(SWEmerge$Bhat)
	SWEmerge$Bhat[nanor0] <- SWEmerge$Total[nanor0]
	
	# 10) reshape back to PC matrix
	PCout         <- acast(SWEmerge, Year ~ Cohort, sum, value.var = "Bhat")
	
	# 11) impute original, unperturbed values outside 
	# Lexis bounds of adjustment
	PCout[PCmask] <- PCin[PCmask]
	
	# 12) Now constrain annual totals
	# create vector of canonical period totals
	Bknown <- rowSums(PCin)
	for (i in 1:maxit){
		# annual totals as of now
		Bi     <- rowSums(PCout) 
		
		# break condition
		if (sum(abs(Bi-Bknown))<tol){
			break
		}
		# now rescale to sum to canonical year total
		# this affects some cells we wish to hold as canonical,
		# so wash, wrinse, repeat
		PCout  <- PCout * (Bknown / Bi)
		
		# put back in canonical values
		PCout[PCmask] <- PCin[PCmask]
	}
	cat(i," iterations\n")
	# return result.
	PCout
}

# This perturbation method is based on transfering differences from period
# to offspring. Differences are proportional differences from observerd
# period totals and a loess smooth of the same series, where the single
# parameter span determines the smoothness of the reference. This is meant
# to mimick the smoothing that offspring reference cohorts will have undergone
# from pclm. SWE is the master file, post graduation.
pertspan <- function(SWE, span = .05, maxit = 100){
	SWE %>% 
	  group_by(Year) %>% 
	  summarize(Births = sum(Total, na.rm = TRUE), 
	            .groups = "drop") %>% 
	  ungroup() %>% 
	  mutate(fitted = base::suppressWarnings(loess(Births ~ Year, 
	                                               span = span, 
	                                               data = .)) %>% 
	                  '[['("fitted"),
	         ratio = Births / fitted) %>% 
	  rename(Cohort = Year) %>% 
	  select(Cohort, ratio) %>% 
	  right_join(SWE, by = "Cohort") %>% 
    mutate(ratio = if_else(is.na(ratio),1,ratio),
           Bhat = Total * ratio,
	         Age5 = ARDY - ARDY %% 5,
	         Age5 = if_else(Age5 == 10, 12, Age5)) %>% 
	  group_by(Year, Age5) %>% 
	  mutate(B5 = sum(Total),
	         B5hat = sum(Bhat)) %>% 
	  ungroup() %>% 
	  mutate(Bhat = if_else(mask, (Bhat / B5hat) * B5, Total),
	         Bhat = if_else(is.nan(Bhat),0,Bhat)) %>% 
    select(Year, ARDY, Cohort, Total, Bhat, mask) %>% 
    arrange(Year, ARDY) # %>% 
     # group_by(Cohort) %>% 
     # summarize(B_orig = sum(Total),
     #           Bhat = sum(Bhat)) %>% 
     # # pivot_longer(B_orig:Bhat, names_to = "variant", values_to = "Births") %>% 
     # ggplot(aes(x = Cohort, y = Bhat - B_orig)) +
     # geom_line()
	  
  # Note: edge cases ca 1891 might need iterating to fully constrain?
}
minspan <- function(SWE, span = .05, maxAge = 45, maxit = 100){
  PCpert    <- pertspan(SWE, span = span, maxit = maxit)
 
  Bc <- PCpert %>% 
    group_by(Cohort) %>% 
    summarize(Bc = sum(Bhat),.groups= "drop") %>% 
    rename(refYear = Cohort)
  
  Bt <- PCpert %>% 
    group_by(Year) %>% 
    summarize(Bt = sum(Bhat),.groups= "drop")%>% 
    rename(refYear = Year)
  
  B <- inner_join(Bc, Bt, by = "refYear") %>% 
    dplyr::filter(refYear >= leftYear(SWE) - 15,
                  refYear <= rightCoh(SWE, Age = maxAge)) %>% 
    arrange(refYear) %>% 
    mutate(compgen = between(refYear, 1891 - 15, rightCoh(SWE, Age = maxAge)),
           pertgen = between(refYear, leftYear(SWE),1875),
           rdt = (lead(Bt) - Bt) / Bt,
           rdc = (lead(Bc) - Bc) / Bc)
   mod1 <- 
    B %>% 
    dplyr::filter(compgen) %>% 
    lm(rdt ~ rdc, data = .)
   mod2 <-
     B %>% 
     dplyr::filter(pertgen) %>% 
     lm(rdt ~ rdc, data = .)
# B %>% 
#   pivot_longer(Bc:Bt, names_to = "variant", values_to = "B") %>% 
#   ggplot(aes(x = refYear, y = B, color = variant)) +
#   geom_line()
# B %>% 
#   ggplot(aes(x = refYear, y = Bc / Bt)) + 
#   geom_line()
  # Bc        <- colSums(PCpert)
  # Bt        <- rowSums(PCpert)
  # # 4) rel first differences in annual births
  # rdt       <- diff(Bt) / Bt[-length(Bt)]
  # # 5) rel first differences in offspring size
  # rdc       <- diff(Bc) / Bc[-length(Bc)]
  
  # 6) canonical values. How well do rel first differences correlate,
  # and what is the slope in their size relationship?
  # TODO adjust default date ranges, now need to push leftward
  # compgen   <- as.character(1876:rightCoh(SWE, Age = maxAge))
  # # correlation
  # # cr        <- cor(rdt[compgen], rdc[compgen])
  # # slope parameter
  # mod1      <- lm(rdt[compgen]~ rdc[compgen])
  slp       <- mod1$coef[2]
  se        <- sd(mod1$residuals)
  # 7) perturbed values
  # pertgen   <- as.character(leftYear(SWE):1875)
  # # correlation in rel first diffs in adjustment area
  # # cr_p      <- cor(rdt[pertgen], rdc[pertgen])
  # # slope in rel first diffs in adjustment area
  # mod2      <- lm(rdt[pertgen]~ rdc[pertgen])
  slp_p     <- mod2$coef[2]
  se_p      <- sd(mod2$residuals)
  
  2*(se - se_p)^2 + 8*(slp - slp_p)^2
}

# TODO: decide how to weight sd vs slope in lm()
# optimize best span given slope and sd of relative first differences
# in period vs offspring series pre and post adjustment break.
# minspan <- function(SWE, span = .05, maxAge = 45, maxit = 100){
# 	PCpert    <- pertspan(SWE, span = span, maxit = maxit)
# 	
# 	Bc        <- colSums(PCpert)
# 	Bt        <- rowSums(PCpert)
# 	# 4) rel first differences in annual births
# 	rdt       <- diff(Bt) / Bt[-length(Bt)]
# 	# 5) rel first differences in offspring size
# 	rdc       <- diff(Bc) / Bc[-length(Bc)]
# 	
# 	# 6) canonical values. How well do rel first differences correlate,
# 	# and what is the slope in their size relationship?
# 	# TODO adjust default date ranges, now need to push leftward
# 	compgen   <- as.character(1876:rightCoh(SWE, Age = maxAge))
# 	# correlation
# 	# cr        <- cor(rdt[compgen], rdc[compgen])
# 	# slope parameter
# 	mod1      <- lm(rdt[compgen]~ rdc[compgen])
# 	slp       <- mod1$coef[2]
# 	se        <- sd(mod1$residuals)
# 	# 7) perturbed values
# 	pertgen   <- as.character(leftYear(SWE):1875)
# 	# correlation in rel first diffs in adjustment area
# 	# cr_p      <- cor(rdt[pertgen], rdc[pertgen])
# 	# slope in rel first diffs in adjustment area
# 	mod2      <- lm(rdt[pertgen]~ rdc[pertgen])
# 	slp_p     <- mod2$coef[2]
# 	se_p      <- sd(mod2$residuals)
# 	2*(se - se_p)^2 + 8*(slp - slp_p)^2
# }

# which is the youngest 'complete' cohort?
rightCoh <- function(SWE, Age = 45){
	ind <- SWE$Year == max(SWE$Year) &
			SWE$ARDY == Age
	SWE$Cohort[ind]
	
}
leftYear <- function(SWE){
	min(SWE$Year)
}
# ---------------------------------------------
# deprecated perturbation method
#perturbDiffs <- function(par=.5,SWE){
#	# a 2 step operation 
#	# First perturb offspring diffs to keep a fraction 
#	# of first diffs of birth series (i.e. structural echo),
#	# then rescale to keep totals as desired.
#	
#	# 1) take input SWE file, i.e. where years pre 1891 are smooth
#	# from graduation exercise, reshape to PC matrix
#	PCin            <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
#	# 2) make a copy
#	PC              <- PCin
#	# 3) create mask delimiting Lexis bounds of perturbation
#	PCmask          <- makeMask(PCin)
#	# 4) annual birth totals, to preserve.
#	Bt              <- rowSums(PCin)
#	ny              <- length(Bt)
#	yrs             <- as.integer(names(Bt))
#	# year identifier TRUE for years within the perturbation range
#	yrsi            <- yrs >= 1775 & yrs < 1876
#	# 5) first differences we expect to echo some
#	dBt             <- diff(Bt)
#	
#	# 6) change relative to last year's births
#	rdBt            <- dBt/Bt[-ny]
#	# -----------------
#	# 7) total offspring from mother cohort (by cohort)
#	Bc              <- colSums(PCin)
#	nc              <- length(Bc)
#	cohs            <- as.integer(names(Bc))
#	# we're allowed to perturb cohorts 1775 through 1891
#	cohsi           <- cohs >= 1775 & cohs < 1876
#	# 8) perturbation factor. par is a prior, to come from separate optimization.
#	# par is positive, ca .4, so perturbation is:
#	# 1 - (.4 of relative change in size of mother cohort).
#	pertc           <- (1 - (rdBt[yrsi] * par))
#	
#	PC[, cohsi]     <- t(t(PC[, cohsi]) * pertc)
#	PC[PCmask]      <- PCin[PCmask]
#	
#	# 9) constrain and return
#	PC              <- constrainBlocks(SWE, PCpert = PC)
#	PC
#}
#
#minpert <- function(par, SWE){
#	# now find the optimal echo factor
#	
#	# 1) get PC matrix from original SWE file, where births from
#	# mothers born in years < 1891 are overly smooth
#	PC     <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
#	
#	# 2) for the given perturbation factor, give back the martix,
#	# with values only for cohorts 1775-1890 before year 1891 perturbed
#	PC2       <- perturbDiffs(par, SWE)
#	# 3) get marginals from which to calculate differences
#	Bt        <- rowSums(PC)
#	Bc        <- colSums(PC2)
#	ny        <- length(Bt)
#	nc        <- length(Bc)
#	# 4) rel first differences in annual births
#	rdt       <- diff(Bt) / Bt[-ny]
#	# 5) rel first differences in offspring size
#	rdc       <- diff(Bc) / Bc[-nc]
#	
#	# 6) canonical values. How well do rel first differences correlate,
#	# and what is the slope in their size relationship?
#	compgen   <- as.character(1876:1959)
#	# correlation
#	# cr        <- cor(rdt[compgen], rdc[compgen])
#	# slope parameter
#	mod1     <- lm(rdt[compgen]~ rdc[compgen])
#	slp      <- mod1$coef[2]
#	se       <- sd(mod1$residuals)
#	# 7) perturbed values
#	pertgen   <- as.character(1776:1875)
#	# correlation in rel first diffs in adjustment area
#	# cr_p      <- cor(rdt[pertgen], rdc[pertgen])
#	# slope in rel first diffs in adjustment area
#	mod2      <- lm(rdt[pertgen]~ rdc[pertgen])
#	slp_p     <- mod2$coef[2]
#	se_p      <- sd(mod2$residuals)
#
#	# 8) how close do we get to correlation and slope?
#	# calc residual:
#	# we want both slope and correlation to be
#	# as close as possible. This is the quantity we try to 
#	# minimize by changing the input 'par'
#	3*(se - se_p)^2 + 7*(slp - slp_p)^2
#}

# ---------------------------------------------
# deprecated perturbation method
#perturbDiffs <- function(par=.5,SWE){
#	# a 2 step operation 
#	# First perturb offspring diffs to keep a fraction 
#	# of first diffs of birth series (i.e. structural echo),
#	# then rescale to keep totals as desired.
#	
#	# 1) take input SWE file, i.e. where years pre 1891 are smooth
#	# from graduation exercise, reshape to PC matrix
#	PCin            <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
#	# 2) make a copy
#	PC              <- PCin
#	# 3) create mask delimiting Lexis bounds of perturbation
#	PCmask          <- makeMask(PCin)
#	# 4) annual birth totals, to preserve.
#	Bt              <- rowSums(PCin)
#	ny              <- length(Bt)
#	yrs             <- as.integer(names(Bt))
#	# year identifier TRUE for years within the perturbation range
#	yrsi            <- yrs >= 1775 & yrs < 1876
#	# 5) first differences we expect to echo some
#	dBt             <- diff(Bt)
#	
#	# 6) change relative to last year's births
#	rdBt            <- dBt/Bt[-ny]
#	# -----------------
#	# 7) total offspring from mother cohort (by cohort)
#	Bc              <- colSums(PCin)
#	nc              <- length(Bc)
#	cohs            <- as.integer(names(Bc))
#	# we're allowed to perturb cohorts 1775 through 1891
#	cohsi           <- cohs >= 1775 & cohs < 1876
#	# 8) perturbation factor. par is a prior, to come from separate optimization.
#	# par is positive, ca .4, so perturbation is:
#	# 1 - (.4 of relative change in size of mother cohort).
#	pertc           <- (1 - (rdBt[yrsi] * par))
#	
#	PC[, cohsi]     <- t(t(PC[, cohsi]) * pertc)
#	PC[PCmask]      <- PCin[PCmask]
#	
#	# 9) constrain and return
#	PC              <- constrainBlocks(SWE, PCpert = PC)
#	PC
#}
#
#minpert <- function(par, SWE){
#	# now find the optimal echo factor
#	
#	# 1) get PC matrix from original SWE file, where births from
#	# mothers born in years < 1891 are overly smooth
#	PC     <- acast(SWE, Year ~ Cohort, sum, value.var = "Total")
#	
#	# 2) for the given perturbation factor, give back the martix,
#	# with values only for cohorts 1775-1890 before year 1891 perturbed
#	PC2       <- perturbDiffs(par, SWE)
#	# 3) get marginals from which to calculate differences
#	Bt        <- rowSums(PC)
#	Bc        <- colSums(PC2)
#	ny        <- length(Bt)
#	nc        <- length(Bc)
#	# 4) rel first differences in annual births
#	rdt       <- diff(Bt) / Bt[-ny]
#	# 5) rel first differences in offspring size
#	rdc       <- diff(Bc) / Bc[-nc]
#	
#	# 6) canonical values. How well do rel first differences correlate,
#	# and what is the slope in their size relationship?
#	compgen   <- as.character(1876:1959)
#	# correlation
#	# cr        <- cor(rdt[compgen], rdc[compgen])
#	# slope parameter
#	mod1     <- lm(rdt[compgen]~ rdc[compgen])
#	slp      <- mod1$coef[2]
#	se       <- sd(mod1$residuals)
#	# 7) perturbed values
#	pertgen   <- as.character(1776:1875)
#	# correlation in rel first diffs in adjustment area
#	# cr_p      <- cor(rdt[pertgen], rdc[pertgen])
#	# slope in rel first diffs in adjustment area
#	mod2      <- lm(rdt[pertgen]~ rdc[pertgen])
#	slp_p     <- mod2$coef[2]
#	se_p      <- sd(mod2$residuals)
#
#	# 8) how close do we get to correlation and slope?
#	# calc residual:
#	# we want both slope and correlation to be
#	# as close as possible. This is the quantity we try to 
#	# minimize by changing the input 'par'
#	3*(se - se_p)^2 + 7*(slp - slp_p)^2
#}

# crappy buggy functions for drawing arcs between points. See hackish application

Arc <- function(a,b,nvert,rad1=0,rad2=pi){
	pivec <- seq(from = rad1 , to = rad2, length.out=nvert)
	x <- a * cos(pivec) + a
	y <- b * sin(pivec)
	out <- matrix(c(x,y),nrow=nvert,ncol=2)
	return(out)
}

# how many radians does the standard arc need to be rotated, 
radians <- function(p1,p2){
	rise <- p2$y-p1$y
	run <- p2$x-p1$x
	add <- ifelse(p1$x<p2$x,0,pi)
	add+atan(rise/run)
}

# distance formula, used to calculate a (major radius)
pythag <- function(p1,p2){
	((p2$y-p1$y)^2 + (p2$x-p1$x)^2)^.5
}
# we use matrx multiplication to perform the rotation, since it's more efficient in R
rotationMatrix <- function(theta){
	matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),ncol=2,nrow=2)
}

rotatearc <- function(arc,Rmat){
	rotatevec <- function(arc,Rmat){
		Rmat%*%c(arc)
	}
	t(apply(arc,1,rotatevec,Rmat=Rmat))
}

arcpoints <- function(p1,p2,nvert=200,rad1,rad2,brel=.4){
	a          <- pythag(p1,p2)*.5
	b          <- a * brel
	theta      <- radians(p1,p2)+pi
	Rmat       <- rotationMatrix(theta)
	arc0       <- Arc(a,b,nvert=nvert,rad1=rad1,rad2=rad2)
	arc1       <- rotatearc(arc0,Rmat)
	arc1[,1]   <- arc1[,1] - arc1[1,1] 
	arc1[,2]   <- arc1[,2] - arc1[1,2] 
	hyp2       <- pythag(
			p1=list(x=arc1[1,1],y=arc1[1,2]),
			p2=list(x=arc1[nvert,1],y=arc1[nvert,2]))
	arcout     <- arc1 * (a * 2) / hyp2
	arcout[,1] <- arcout[,1] + p1$x
	arcout[,2] <- arcout[,2] + p1$y
	return(arcout)
}

draw.arc <- function(x1,y1,x2,y2,brel=.5,nvert=100,rad=1/5,...){
	p1   <- list(x=x1,y=y1)
	p2   <- list(x=x2,y=y2)
	sgn  <- sign(rad)
	rad2 <- sgn * (pi - abs(rad))
	xy   <- arcpoints(p1=p1,p2=p2,rad1=rad,rad2=rad2,brel=brel,nvert=nvert)
	lines(xy,...)
}

draw_block_poly <- function(x,y,...){
  x  <- c(x,max(x)+1)
  xx <- rep(x,each=2)
  yy <- c(0,rep(y,each=2),0)
  polygon(xx,yy,...)
}

draw_block_poly_grid <- function(x,y,N=5,...){
  draw_block_poly(x = x, y = y, ...)
  xNi <- x %% N == 0
  segments(x[xNi],0,x[xNi],y[xNi],col = "white")
  draw_block_poly(x = x, y = y)
}

make_asfr_proj <- function(ASFRlong, jump_off_year){
  ASFR <-
    ASFRlong %>% 
    pivot_wider(names_from = Year, values_from = "ASFR") %>% 
    column_to_rownames("Age") %>% 
    as.matrix()
  
  swe_base_period_50 <- Method13_deBeer1985and1989.R(ASFR,
                                                     joy = jump_off_year, 
                                                     obs = 50,
                                                     age1 = 12, 
                                                     age2 = 55,
                                                     parameter = c(1,0,0,1,0,0),
                                                     len = 100,
                                                     pop = "SWE")
  asfr <- as.matrix(cbind(swe_base_period_50$obsASFR[,as.character(1891:jump_off_year)],swe_base_period_50$predASFR[,as.character((jump_off_year + 1):2116)]))
  
  asfr %>% 
    as_tibble() %>% 
    rownames_to_column("Age") %>% 
    pivot_longer(2:ncol(.), names_to = "Year", values_to = "ASFR") %>% 
    mutate(Age = as.integer(Age),
           Age = Age + 11,
           Year = as.integer(Year)) %>% 
    arrange(Year, Age)
}
