# Author: tim
###############################################################################
setwd("/home/tim/git/BirthFlows/BirthFlows")
# For years 1876:1968 we have observed both total cohort and offspring size
# For years 1775:1875 we have total cohort size, OK, but only smoothed total offspring
# size, an artifact of smooth graduation (denom offsets wouldn't help, since
# these were also in abridged ages).

# Testing ground for new perturbation method:
# 1) Fit a smoother (*) to observed period total births
# 2) get fractional residuals for years 1775-1875
# 3) multiply into (smooth) offspring (1775-1875)
# 4) constrain so that 5-year age groups sum, and only
#    period 1775-1890 affected
# (*) here we use loess(), where parameter 'span' is optimized
#     based on minimizing residuals of sd and slope of lm() between 
#     fractional differences in period vs offspring sizes 

SWE    <- local(get(load("Data/SWE.Rdata")))

PCin   <- acast(SWE, Year~Cohort, sum, value.var = "Total")
per    <- rowSums(PCin)
coh    <- colSums(PCin)
cohs   <- as.integer(names(coh))
yrs    <- as.integer(names(per))

lo     <- loess(per~yrs,span=.05)
plot(yrs,per,type='l',xlim=c(1775,1968))
lines(cohs,coh,col = "red")
lines(yrs,lo$fitted,col=gray(.5),lty="82")
ovlp <- as.character(1775:1875)
lines(1775:1875,coh[ovlp] + lo$residuals[ovlp],col=gray(.5),lty="82")
abline(v=1876)

yrsi <- yrs < 1876
# and relativized:
lines(1775:1875,coh[ovlp] * (lo$y[yrsi] / lo$fitted[yrsi]),col=gray(.5),lty="82")

# iterate to best span?
span   <- .05
per    <- rowSums(PCin)
coh    <- colSums(PCin)
yrs    <- as.integer(names(per))
yrsi   <- yrs < 1876
cohs   <- as.integer(colnames(PCin))
cohsi  <- cohs >= 1775 & cohs < 1876
# fit new loess
lo     <- loess(per~yrs, span = span)
ratio  <- (lo$y[yrsi] / lo$fitted[yrsi])
PCi    <- PCin
PCi[, cohsi] <- t(t(PCi[, cohsi]) * ratio)
PCi    <- constrainBlocks(SWE, PCi, maxit = 100, tol = .1)

cohi   <- colSums(PCi)

plot(cohs,cohi,xlim=c(1775,1968),type='l')
lines(yrs, per)

# here method designed

pertspan <- function(SWE, span = .05){
	PCin   <- acast(SWE, Year~Cohort, sum, value.var = "Total")
	per    <- rowSums(PCin)
	coh    <- colSums(PCin)
	yrs    <- as.integer(names(per))
	yrsi   <- yrs < 1876
	cohs   <- as.integer(colnames(PCin))
	cohsi  <- cohs >= 1775 & cohs < 1876
# fit new loess
	lo     <- loess(per~yrs, span = span)
	ratio  <- (lo$y[yrsi] / lo$fitted[yrsi])
	PCi    <- PCin
	PCi[, cohsi] <- t(t(PCi[, cohsi]) * ratio)
	PCi    <- constrainBlocks(SWE, PCi, maxit = 100, tol = .1)
	PCi
}

minspan <- function(SWE, span = .05){
	PCpert    <- pertspan(SWE, span = span)
	
	BC        <- colSums(PCpert)
	Bt        <- rowSums(PCpert)
	# 4) rel first differences in annual births
	rdt       <- diff(Bt) / Bt[-length(Bt)]
	# 5) rel first differences in offspring size
	rdc       <- diff(Bc) / Bc[-length(Bc)]
	
	# 6) canonical values. How well do rel first differences correlate,
	# and what is the slope in their size relationship?
	compgen   <- as.character(1876:1959)
	# correlation
	# cr        <- cor(rdt[compgen], rdc[compgen])
	# slope parameter
	mod1     <- lm(rdt[compgen]~ rdc[compgen])
	slp      <- mod1$coef[2]
	se       <- sd(mod1$residuals)
	# 7) perturbed values
	pertgen   <- as.character(1776:1875)
	# correlation in rel first diffs in adjustment area
	# cr_p      <- cor(rdt[pertgen], rdc[pertgen])
	# slope in rel first diffs in adjustment area
	mod2      <- lm(rdt[pertgen]~ rdc[pertgen])
	slp_p     <- mod2$coef[2]
	se_p      <- sd(mod2$residuals)
	3*(se - se_p)^2 + 7*(slp - slp_p)^2
}

span     <- optimize(minspan, interval = c(.01,.5), SWE = SWE)

PCi      <- pertspan(SWE, span = span$minimum)
cohi     <- colSums(PCi)

rdc      <- diff(cohi)/cohi[-length(cohi)]
rdp      <- diff(per)/per[-length(per)]

yrsc <- as.character(1775:1875)
yrsp <- as.character(1876:1968)


# Looks pretty good
par(mfrow=c(1,2))
# 1775- 1875
plot(rdp[yrsc], rdc[yrsc], asp=1, 
		xlab = "frac diff in period", 
		ylab = "frac diff in offspring size",
		ylim = c(-.12,.25),
		xlim = c(-.12,.25),
		main = "Reference years 1775 - 1875")
abline(a=0,b=1)
abline(lm(rdc[yrsc]~rdp[yrsc]),col = "red")
text(rdp[yrsc[c(35,36, 75, 93, 95)]],rdc[yrsc[c(35,36, 75, 93, 95)]], yrsc[c(35,36, 75, 93, 95)],xpd=TRUE,pos=1)
#identify(rdp[yrsc], rdc[yrsc],labels=yrsc,n=4)

# 1876 - 1968
plot(rdp[yrsp], rdc[yrsp], asp=1, 
		xlab = "frac diff in period", 
		ylab = "frac diff in offspring size",
		ylim = c(-.12,.25),
		xlim = c(-.12,.25),
		main = "Reference years 1876 - 1968")
abline(a=0,b=1)
abline(lm(rdc[yrsp]~rdp[yrsp]),col = "red")
#identify(rdp[yrsp], rdc[yrsp],labels=yrsp,n=5)
text(rdp[yrsp[c(4,5,45,46,47,67)]],rdc[yrsp[c(4,5,45,46,47,67)]], yrsp[c(4,5,45,46,47,67)],
		pos=c(3,1,1,2,1,1))