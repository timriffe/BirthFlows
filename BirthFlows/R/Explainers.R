
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

BI5 <- BornIn[,list(B5 = sum(B)),by=list(C5)]
BH5 <- BabiesHad[,list(B5 = sum(B)),by=list(Y5)]
gap <- 3000
Top  <- as.matrix(BI5$B5)
Bot  <- as.matrix(BH5$B5)
Topc <- cumsum(c(0,Top))
Botc <- cumsum(c(0,-Bot))-gap

ytix <- seq(0,125000,by=25000)
ylabs <- c("0","50,000","100,000")

pdf("Figures/FigReflection.pdf",width=5,height=9)
par(mai=c(1,.6,1.,.6),xpd=TRUE)
plot(NULL, type = "n", axes=FALSE,ylim = c(-100000,130000),xlim=c(-2,3),xlab="",ylab="")
rect(0,0,1,Topc[10],col = gray(.8), border=NA)
rect(0,-gap,1,Botc[10],col = gray(.8), border=NA)
segments(0,ytix,-.15,ytix)
segments(0,-ytix[-6]-gap,-.15,-ytix[-6]-gap)
text(-.1,c(2000,50000,100000),ylabs,pos=2)
text(-.1,-c(5000,50000,100000)-gap,ylabs,pos=2)
rect(0,Topc[-10],1,Topc[-1],border=gray(.2))
rect(0,Botc[-10],1,Botc[-1],border=gray(.2))

# TODO: continue with alignment here
ytop <- Topc + c(2000,8000,13000,7000,0,0,0,-3000,0,4000)
text(1.7,ytop[-length(ytop)],BI5$C5,pos=4,xpd=TRUE)
segments(1.1,Topc[-c(5:7,length(Topc))],1.8,ytop[-c(5:7,length(ytop))])

ybot <- Botc - c(3000,9000,12000,0,0,0,0,1000,7000) 
text(1.7,ybot[-length(ybot)],BH5$Y5,pos=4,xpd=TRUE)
segments(1.1,Botc[-c(4:6,10)],1.8,ybot[-c(4:6,10)])

text(0,160000,"Births\nin 1900",pos=2,cex=1.2)
text(1,160000,"Mother's\ncohort",pos=4,cex=1.2)
text(0,-132000,"Children of\n1900 cohort",pos=2,cex=1.2)
text(1,-132000,"Ocurrence\nyear",pos=4,cex=1.2)
dev.off()

# for percentages cited in text
#sum(BI5$B5[BI5$C5 %in% 1860:1875]) / sum(BI5$B5)
#sum(BH5$B5[BH5$Y5 %in% 1920:1935]) / sum(BH5$B5)
#sum(BH5$B5)/sum(BI5$B5)




blues   <- colorRampPalette(RColorBrewer::brewer.pal(9,"Blues")[-c(1:3)],space="Lab")
greens  <- colorRampPalette(RColorBrewer::brewer.pal(9,"Greens")[-c(1:3)],space="Lab")
nblues  <- sum(cohs%%20==0)
ngreens <- sum(yrs%%20==0)
PC2     <- PC
PC2[PC2==0] <- NA
ytix    <- seq(-10000,10000,by=2000)

pdf("Figures/FxFlowReflect.pdf",height=3.7,width=10)
# fresh plane
par(mai=c(.5,1,.5,.6),xaxs="i",yaxs="i",xpd=TRUE)
plot(NULL, xlim = range(1720,2015),ylim=c(-10000,10000), axes = FALSE, xlab = "", ylab = "")

# grid lines
segments(seq(1720,2010,by=10),-10000,seq(1720,2010,by=10),10000,col = gray(.8))
segments(1720,ytix,2015,ytix,col = gray(.8))

# axis labels
text(seq(1720,2000,by=20),-10000,seq(1720,2000,by=20),pos=1)
text(1720,ytix,abs(ytix),pos=2,cex=.8)
#text(2015,ytix,abs(ytix),pos=4,cex=.8)

# Fx lines
matplot(yrs,PC2, type = 'l', col = "#00000040", lty = 1, add =TRUE,lwd=.7)
matplot(cohs,-t(PC2), type = 'l', col = "#00000040", lty = 1, add =TRUE,lwd=.7)

# highlight every 20th
matplot(yrs,PC2[,cohs%%20==0], type = 'l', col = paste0(blues(nblues),"A1"), lty = 1, add =TRUE,lwd=seq(2,1,length=nblues))
matplot(cohs,-t(PC2)[,yrs%%20==0], type = 'l', col = paste0(greens(ngreens),"A1"), lty = 1, add =TRUE,lwd=seq(2,1,length=ngreens))

# highlight 1900
matplot(yrs,PC2[,"1900"], type = 'l', col = "#000000", lty = 1, add =TRUE,lwd=2)
matplot(cohs,-PC2["1900",], type = 'l', col = "#000000", lty = 1, add =TRUE,lwd=2)
arrows(1900,-10000,1900,-max(PC[,"1900"]),length=.05)
arrows(1900,10000,1900,max(PC["1900",]),length=.05)

# centerline
segments(1720,0,2015,0,lwd=.5)

# formalize labels of 1900
text(1871,-8485,"A",font=2,cex=1.5)
text(1927,6760,"B",font=2,cex=1.5)
text(1899,-10000,"b",font=2,cex=1.5,pos=4)
text(1899,10000,"a",font=2,cex=1.5,pos=4)

# axis names
text(1880,-12500,"Index Year",cex=1.5)
text(1720,-12500,"Mothers' cohort",cex=1.3)
text(1720,12500,"Ocurrence year",cex=1.3)
arrows(c(1750,1750),c(-12500,12500),c(1765,1765),c(-12500,12500),length=.1)
text(1700,0,"Count",cex=1.5,srt=90,pos=3)
dev.off()





## once-off gif and twitter header
#library(animation)
## save this then speed up the gif online
#saveGIF({
#for (i in 1:length(times)){
#	ic <- as.character(times[i])
#	plot(NULL, xlim = range(c(cohs,yrs)),ylim=c(-10000,10000),axes=FALSE,xlab = "",ylab = "")
#	axis(1);axis(2,las=1)
#	if (any(cohs==times[i])){
#		matplot(yrs,PC[,1:which(cohs==times[i]),drop=FALSE], type = 'l', col = "#00000050", lty = 1, add =TRUE)
#	} else {
#		matplot(yrs,PC, type = 'l', col = "#00000050", lty = 1, add =TRUE)
#	}
#	if (any(yrs == times[i])){
#		matplot(cohs,-t(PC[1:which(yrs==times[i]),,drop=FALSE]), type = 'l', col = "#00000050", lty = 1, add =TRUE)
#	}
#	abline(v=times[i])
#	if(any(cohs == times[i])){
#		ind1 <- is.na(PC[,ic])
#		yrsi <- yrs[!ind1]
#		if (length(yrsi) > 0){
#		yrsi <- c(yrsi[1],yrsi,yrsi[length(yrsi)])
#		y <- PC[!ind1,ic]
#		y <- c(0,y,0)
#		polygon(yrsi,y,border="red",col = "#FF000050")}
#	}
##lines(yrs,PC[,ic],col="red")
#				if(any(yrs == times[i])){
#					ind2 <- is.na(PC[ic,])
#					yrs2 <- cohs[!ind2]
#					yrs2 <- c(yrs2[1],yrs2,yrs2[length(yrs2)])
#					y2 <- PC[ic,!ind2]
#					y2 <- c(0,y2,0)
#					polygon(yrs2,-y2,border="red",col = "#FF000050")
#				}
#}
#},ani.height = 300,ani.width = 1200,movie.name = "Figures/Anim1/anim1.gif")
#
#png("Figures/Header.png",height=500,width=1500)
#par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
#plot(NULL, xlim = range(1785,1965),ylim=c(-10000,10000), axes = FALSE, xlab = "", ylab = "")
#matplot(yrs,PC, type = 'l', col = "#00000050", lty = 1, add =TRUE)
#matplot(cohs,-t(PC), type = 'l', col = "#00000050", lty = 1, add =TRUE)
##axis(1)
##axis(2,las=1)
#dev.off()
# once-off tweet
#mask0 <- PC == 0
#PCc <- t(apply(PC,1,cumsum))
#PCc[mask0] <- NA
#PCcc <- t(apply(PC,2,cumsum))
#PCcc[t(mask0)] <- NA
#
#png("Figures/TweetIt.png",width=1000,height=300)
#par(mai=c(0,0,0,0))
#plot(NULL, xlim = range(c(cohs,yrs)),ylim=c(-150000,150000),axes=FALSE,xlab="",ylab="")
#matplot(yrs,PCc, type = 'l', col = "#00000050", lty = 1, add =TRUE)
#matplot(cohs,-PCcc, type = 'l', col = "#00000050", lty = 1, add =TRUE)
#dev.off()
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
