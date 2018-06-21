
# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")

draw_block_poly <- function(x,y,...){
	x <- c(x,max(x)+1)
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

#source("R/ColorRamp.R")
#source("R/Arcs.R")
SWEl     <- melt(PC, varnames = c("Year","Cohort"),value.name = "B")
SWEl$Age <- SWEl$Year - SWEl$Cohort
SWEl     <- SWEl[SWEl$Age >= 12 & SWEl$Age <= 55, ]
SWEl     <- SWEl[SWEl$B > 0, ]
SWEl     <- data.table(SWEl)
SWEl$Age5 <- SWEl$Age - SWEl$Age %% 5
SWEl5    <- SWEl[,list(B5 = sum(B)), by = list(Year, Age5)]
SWEl5$Cohort <- SWEl5$Year - SWEl5$Age5 
# experiment with arcs

cohs <- as.integer(colnames(PC))
yrs  <- as.integer(rownames(PC))

times <- sort(union(cohs,yrs))

head(SWEl)
plot()

refyr <- 1900

BornIn    <- SWEl[Year == refyr]
BabiesHad <- SWEl[Cohort == refyr]


pdf("Figures/Fig11900MotherAge.pdf")
plot(BornIn$Age, BornIn$B, type = 'n',xlab = "Mother's age", ylab = "Count",axes=FALSE,ylim=c(0,7000))
draw_block_poly_grid(BornIn$Age, BornIn$B, border = NA, col = gray(.8))
axis(2,las=1)
text(seq(15,50,by=5),0,seq(15,50,by=5),pos=1,xpd=TRUE)
dev.off()

pdf("Figures/Fig11900MotherCohort.pdf")
plot(BornIn$Cohort, BornIn$B, type = 'n',xlab = "Mother's Year of birth", ylab = "Count",axes=FALSE,ylim=c(0,7000))
draw_block_poly_grid(BornIn$Cohort, BornIn$B, border = NA, col = gray(.8))
axis(2,las=1)
text(seq(1850,1885,by=5),0,seq(1850,1885,by=5),pos=1,xpd=TRUE)
dev.off()

pdf("Figures/Fig11900IDAge.pdf")
plot(BabiesHad$Age, BabiesHad$B, type = 'n',xlab = "ID age at birth", ylab = "Count",axes=FALSE,ylim=c(0,7000))
draw_block_poly_grid(BabiesHad$Age, BabiesHad$B, border = NA, col = gray(.8))
axis(2,las=1)
text(seq(15,50,by=5),0,seq(15,50,by=5),pos=1,xpd=TRUE)
dev.off()

pdf("Figures/Fig11900IDYear.pdf")
plot(BabiesHad$Year, BabiesHad$B, type = 'n',xlab = "ID's births by year", ylab = "Count",axes=FALSE,ylim=c(0,7000))
draw_block_poly_grid(BabiesHad$Year, BabiesHad$B, border = NA, col = gray(.8))
axis(2,las=1)
text(seq(1915,1950,by=5),0,seq(1915,1950,by=5),pos=1,xpd=TRUE)
dev.off()


pdf("Figures/Fig31900juxt.pdf",width=14)
plot(c(BornIn$Cohort,BabiesHad$Year), c(BornIn$B,BabiesHad$B), type = 'n',xlab = "Calendar time", ylab = "Count",axes=FALSE,ylim=c(0,7000))
segments(1887,0,1914,0)
segments(seq(1890,1910,by=5),0,seq(1890,1910,by=5),100)
draw_block_poly_grid(BabiesHad$Year, BabiesHad$B, border = NA, col = gray(.8))
draw_block_poly_grid(BornIn$Cohort, BornIn$B, border = NA, col = gray(.8))
axis(2,las=1)
text(seq(1850,1950,by=5),0,seq(1850,1950,by=5),pos=1,xpd=TRUE)
segments(1900,0,1900,4000,lwd=2)
text(1900,4100,"Reference year\n1900",pos=3,cex=1.5)
text(1877,BornIn$B[BornIn$Cohort==1876],"Year 1900 births\nby mothers' birth cohort",pos=4,cex=1)
text(1927,BabiesHad$B[BabiesHad$Year==1926],"Offspring over time of\nmothers born in 1900",pos=4,cex=1)
dev.off()

# BornIn5
BornIn$C5 <-BornIn$Cohort - BornIn$Cohort%%5
BabiesHad$Y5 <- BabiesHad$Year - BabiesHad$Year%%5



library(animation)
# save this then speed up the gif online
saveGIF({
for (i in 1:length(times)){
	ic <- as.character(times[i])
	plot(NULL, xlim = range(c(cohs,yrs)),ylim=c(-10000,10000),axes=FALSE,xlab = "",ylab = "")
	axis(1);axis(2,las=1)
	if (any(cohs==times[i])){
		matplot(yrs,PC[,1:which(cohs==times[i]),drop=FALSE], type = 'l', col = "#00000050", lty = 1, add =TRUE)
	} else {
		matplot(yrs,PC, type = 'l', col = "#00000050", lty = 1, add =TRUE)
	}
	if (any(yrs == times[i])){
		matplot(cohs,-t(PC[1:which(yrs==times[i]),,drop=FALSE]), type = 'l', col = "#00000050", lty = 1, add =TRUE)
	}
	abline(v=times[i])
	if(any(cohs == times[i])){
		ind1 <- is.na(PC[,ic])
		yrsi <- yrs[!ind1]
		if (length(yrsi) > 0){
		yrsi <- c(yrsi[1],yrsi,yrsi[length(yrsi)])
		y <- PC[!ind1,ic]
		y <- c(0,y,0)
		polygon(yrsi,y,border="red",col = "#FF000050")}
	}
#lines(yrs,PC[,ic],col="red")
				if(any(yrs == times[i])){
					ind2 <- is.na(PC[ic,])
					yrs2 <- cohs[!ind2]
					yrs2 <- c(yrs2[1],yrs2,yrs2[length(yrs2)])
					y2 <- PC[ic,!ind2]
					y2 <- c(0,y2,0)
					polygon(yrs2,-y2,border="red",col = "#FF000050")
				}
}
},ani.height = 300,ani.width = 1200,movie.name = "Figures/Anim1/anim1.gif")

png("Figures/Header.png",height=500,width=1500)
par(mai=c(0,0,0,0),xaxs=i,yaxs="i")
plot(NULL, xlim = range(1785,1965),ylim=c(-10000,10000), axes = FALSE, xlab = "", ylab = "")
matplot(yrs,PC, type = 'l', col = "#00000050", lty = 1, add =TRUE)
matplot(cohs,-t(PC), type = 'l', col = "#00000050", lty = 1, add =TRUE)
#axis(1)
#axis(2,las=1)
dev.off()

plot(NULL, xlim = range(c(cohs,yrs)),ylim=c(-10000,10000))




matplot(yrs,PC[,"1850"], type = 'l', col = "#00000050", lty = 1, add =TRUE)
matplot(cohs,-PC["1850",], type = 'l', col = "#00000050", lty = 1, add =TRUE)




mask0 <- PC == 0
PCc <- t(apply(PC,1,cumsum))
PCc[mask0] <- NA
PCcc <- t(apply(PC,2,cumsum))
PCc[mask0] <- NA
plot(NULL, xlim = range(c(cohs,yrs)),ylim=c(-150000,150000))
matplot(yrs,PCc, type = 'l', col = "#00000050", lty = 1, add =TRUE)
matplot(cohs,-PCcc, type = 'l', col = "#00000050", lty = 1, add =TRUE)
#SWEl5 <- data.frame(SWEl5)
#SWEl5L <- SWEl5[SWEl5$B5 > 1000, ]
#SWEl5L$Cohort < SWEl5L$Year
#
#
#
#plot(NULL, xlim = c(1750,2015), ylim = c(-5,25))
#for (i in 1:nrow(SWEl5L)){
#		arc <- arcpoly(
#				p1=list(x= SWEl5L[i,"Cohort"],y=0),
#				p2=list(x= SWEl5L[i,"Year"],y=0),
#				maxthick = SWEl5L[i,"B5"]/1000)		
#		polygon(arc, border = NA, col = "#00000001")
#	
#}
#
#arc <- arcpoly(p1=list(x=1820,y=0),p2=list(x=1850,y=0),maxthick = 2)
#polygon(arc, border = NA, col = "#00000050")
#
