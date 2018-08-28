
# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")



offspring <- PC[,"1800"]  # children born to 1800 mothers
grandmas  <- PC["1800", ] # births in 1800 by mother cohorts

yrs       <- as.integer(names(offspring))
cohs      <- as.integer(names(grandmas))

plot(yrs, offspring, xlim = c(1750,1850), type= 'l')
lines(cohs, grandmas)



wmean <- function(x,w){
	sum((x+.5)*w)/sum(w)
}

g2dist <- function(PC,year = 1800){
	yrs       <- as.integer(rownames(PC))
	cohs      <- as.integer(colnames(PC))
	yrc       <- as.character(year)
	offspring <- PC[, yrc]
	grandmas  <- PC[yrc, ]
	wmean(yrs,offspring) - wmean(cohs,grandmas)
}


g2sd <- function(PC,year = 1800){
	yrc       <- as.character(year)
	yrs       <- as.integer(rownames(PC))
	cohs      <- as.integer(colnames(PC))
	offspring <- PC[, yrc]
	grandmas  <- PC[yrc, ]
	yri       <- offspring > 0
	coi       <- grandmas > 0
	# and standard dev needs outer diff?
	yrs       <- yrs[yri]
	cohs      <- cohs[coi]
	offspring <- offspring[yri]
	grandmas  <- grandmas[coi]
	
	og        <- outer(offspring,grandmas,"*")
	diffs     <- outer(yrs,cohs,"-")
	mn        <- sum(diffs * og) / sum(og)
	sqrt(sum(((diffs - mn)^2) * og) / sum(og))
}

# mean distance by reference year.
refyrs    <- 1775:1968
gdists    <- sapply(refyrs,g2dist,PC=PC)

plot(refyrs,gdists)
g2dist(PC,1800)
g2sd(PC,1800)
gsds <- sapply(refyrs,g2sd,PC=PC)


plot(refyrs,gsds)
plot(refyrs,gdists, type = 'l', ylim=c(45,75))
polygon(c(refyrs,rev(refyrs)), 
		c(gdists - gsds,rev(gdists+gsds)),col = "#00000050")


# and quantiles?

g2dist <- function(PC, year = 1800){
	yrc       <- as.character(year)
	yrs       <- as.integer(rownames(PC))
	cohs      <- as.integer(colnames(PC))
	offspring <- PC[, yrc]
	grandmas  <- PC[yrc, ]
	yri       <- offspring > 0
	coi       <- grandmas > 0
	# and standard dev needs outer diff?
	yrs       <- yrs[yri]
	cohs      <- cohs[coi]
	offspring <- offspring[yri]
	grandmas  <- grandmas[coi]
	
	og        <- outer(offspring,grandmas,"*")
	diffs     <- outer(yrs,cohs,"-")
	
	dt        <- data.table(og=c(og),diffs=c(diffs))
	dt        <- dt[,sum(og),by=list(diffs)]
	dt$V1     <- dt$V1 / sum(dt$V1)

	setnames(dt,old="V1",new="p")
	dt        <- as.data.frame(dt)
	dt[order(dt$diffs), ]
}

g2quant <- function(PC, year = 1800, probs = c(.01,.025,.25,.5,.75,.975,.99)){
	dt <- g2dist(PC = PC, year = year)
	dt$pc <- cumsum(dt$p)
	splinefun(dt$diffs~dt$pc,method="monoH.FC")(probs)
}

gQuants <- sapply(refyrs, g2quant, PC=PC)
rownames(gQuants) <- c(.01,.025,.25,.5,.75,.975,.99)
colnames(gQuants) <- refyrs
plot(refyrs,gQuants[4,], type = 'l', ylim=c(40,85))
polygon(c(refyrs,rev(refyrs)), 
		c(gQuants[3,],rev(gQuants[5,])),col = "#00000020")
polygon(c(refyrs,rev(refyrs)), 
		c(gQuants[2,],rev(gQuants[6,])),col = "#00000020")
polygon(c(refyrs,rev(refyrs)), 
		c(gQuants[1,],rev(gQuants[7,])),col = "#00000010")
lines(refyrs, sapply(refyrs,g2dist,PC=PC),col="blue",lwd=2)
# what about standard deviations? hmmm.


