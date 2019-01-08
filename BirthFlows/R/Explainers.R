
# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
#source("R/DataPrep.R")

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

#source("R/ColorRamp.R")
#source("R/Arcs.R")
SWEl      <- melt(PC, varnames = c("Year","Cohort"),value.name = "B")
SWEl$Age  <- SWEl$Year - SWEl$Cohort
SWEl      <- SWEl[SWEl$Age >= 12 & SWEl$Age <= 55, ]
SWEl      <- SWEl[SWEl$B > 0, ]
SWEl      <- data.table(SWEl)
SWEl$Age5 <- SWEl$Age - SWEl$Age %% 5
SWEl5     <- SWEl[,list(B5 = sum(B)), by = list(Year, Age5)]
SWEl5$Cohort <- SWEl5$Year - SWEl5$Age5 

cohs      <- as.integer(colnames(PC))
yrs       <- as.integer(rownames(PC))

times     <- sort(union(cohs,yrs))



refyr     <- 1900

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


pdf("Figures/Fig31900juxt.pdf",width=10,height=5)
plot(c(BornIn$Cohort,BabiesHad$Year), c(BornIn$B,BabiesHad$B), type = 'n',xlab = "Calendar time", ylab = "Count",axes=FALSE,ylim=c(0,7000))
segments(1887,0,1914,0)
segments(seq(1890,1910,by=5),0,seq(1890,1910,by=5),100)
draw_block_poly_grid(BabiesHad$Year, BabiesHad$B, border = NA, col = gray(.8))
draw_block_poly_grid(BornIn$Cohort, BornIn$B, border = NA, col = gray(.8))
axis(2,las=1)
text(seq(1850,1950,by=5),0,seq(1850,1950,by=5),pos=1,xpd=TRUE,cex=.8)
segments(1900,0,1900,4000,lwd=2)
text(1900,4100,"Reference year\n1900",pos=3,cex=1.5)
text(1877,BornIn$B[BornIn$Cohort==1876],"Year 1900 births\nby mothers' birth cohort",pos=4,cex=1)
text(1927,BabiesHad$B[BabiesHad$Year==1926],"Offspring over time of\nmothers born in 1900",pos=4,cex=1)
text(1877,BornIn$B[BornIn$Cohort==1876]+800,"A",pos=4,cex=1.2,font=2)
text(1927,BabiesHad$B[BabiesHad$Year==1926]+800,"B",pos=4,cex=1.2,font=2)

# young & old labels
text(c(1855,1885,1920,1940),3000,c("older","younger","younger","older"),cex=2,font=2,col="#11111180")
dev.off()

# BornIn5
BornIn$C5 <-BornIn$Cohort - BornIn$Cohort%%5
BabiesHad$Y5 <- BabiesHad$Year - BabiesHad$Year%%5

BI5   <- BornIn[,list(B5 = sum(B)),by=list(C5)]
BH5   <- BabiesHad[,list(B5 = sum(B)),by=list(Y5)]
gap   <- 3000
Top   <- as.matrix(BI5$B5)
Bot   <- as.matrix(BH5$B5)
Topc  <- cumsum(c(0,Top))
Botc  <- cumsum(c(0,-Bot))-gap

ytix  <- seq(0,125000,by=25000)
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
text(.5,-110000,"B,b",cex=1.4,font=2)
text(.5,148000,"A,a",cex=1.4,font=2)
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
cohs      <- as.integer(colnames(PC))
yrs       <- as.integer(rownames(PC))
pdf("Figures/FxFlowReflect.pdf",height=4.3,width=12)
# fresh plane
par(mai=c(.6,1,.6,.6),xaxs="i",yaxs="i",xpd=TRUE)
plot(NULL, xlim = range(1680,2071),ylim=c(-10000,10000), axes = FALSE, xlab = "", ylab = "")

# grid lines
segments(seq(1680,2060,by=10),-10000,seq(1680,2060,by=10),10000,col = gray(.8))
segments(1680,ytix,2067,ytix,col = gray(.8))

# axis labels
text(seq(1680,2060,by=20),-10000,seq(1680,2060,by=20),pos=1)
text(1680,ytix,abs(ytix),pos=2,cex=.8)
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
segments(1720,0,2017,0,lwd=.5)

# formalize labels of 1900
text(1871,-8485,"A",font=2,cex=1.5)
text(1927,6760,"B",font=2,cex=1.5)
text(1899,-10000,"b",font=2,cex=1.5,pos=4)
text(1899,10000,"a",font=2,cex=1.5,pos=4)

# axis names
text(1880,-12500,"Index Year",cex=1.5)
text(1680,-12500,"Mothers' cohort",cex=1.3)
text(1680,12500,"Ocurrence year",cex=1.3)
arrows(c(1715,1715),c(-12500,12500),c(1730,1730),c(-12500,12500),length=.1)
text(1660,0,"Count",cex=1.5,srt=90,pos=3)

dev.off()


# Appendix Lexis explainer:
graphics.off()

pdf("Figures/App_split1.pdf",height=3.4,width=1.4)
par(mai=c(.2,.2,.2,.2))
plot(NULL, type = "n",xlim = c(0,1),ylim=c(0,3),asp=1,ann=FALSE,axes=FALSE)
rect(0,0:2,1,1:3,border = "black",lwd=4)
text(0,0:3,0:3,pos=2,xpd=TRUE,cex=1.3)
dev.off()

pdf("Figures/App_split2.pdf",height=3.4,width=1.4)
par(mai=c(.2,.2,.2,.2))
plot(NULL, type = "n",xlim = c(0,1),ylim=c(0,3),asp=1,ann=FALSE,axes=FALSE)
rect(0,0:2,1,1:3,border = "black",lwd=4)
text(0,0:3,0:3,pos=2,xpd=TRUE,cex=1.3)
segments(0,0:2,1,1:3,lwd=4)
dev.off()

pdf("Figures/App_split3.pdf",height=3.4,width=1.4)
par(mai=c(.2,.2,.2,.2))
plot(NULL, type = "n",xlim = c(0,1),ylim=c(0,3),asp=1,ann=FALSE,axes=FALSE)
text(0,0:3,0:3,pos=2,xpd=TRUE,cex=1.3)

x <- c(0,1,1,0)
y <- c(-1,0,1,0)
yshift <- 0:2
for (i in 1:length(yshift)){
	yi <- y + yshift[i]
	yi[yi < 0] <- 0
	yi[yi > 3] <- 3
	polygon(x,yi,lwd=4)
}
segments(0,2,0,3,lwd=4)
segments(0,1:3,1,1:3,lty="57")
dev.off()


# represent reflection before perturbation:

SWE <- local(get(load("Data/SWE.Rdata")))
Ph <- tapply(SWE$Total,SWE$Year,sum)
Ch <- tapply(SWE$Cohort,SWE$Year,sum)

PCin   <- acast(SWE, Year~Cohort, sum, value.var = "Total")
per  <- rowSums(PCin)
coh  <- colSums(PCin)
cohs <- as.integer(names(coh))
yrs  <- as.integer(names(per))
rc <- function(x){
	rev(cumsum(rev(x)))
}
PCc <- apply(PCin,2,cumsum)
fyrs  <- 1836:1876
frac <- PCc["1890",as.character(fyrs)]


pdf("Figures/App_preAdjustment.pdf",height=5,width=10)
plot(NULL,type="n", ann = FALSE, axes = FALSE, xlim = c(1680, 2017), ylim = c(-150000,150000))
polygon(x=c(yrs,rev(yrs)),y=c(per,rep(0,length(yrs))),col="#AADDDD", border = NA)
polygon(x=c(cohs,rev(cohs)),y=c(-coh,rep(0,length(cohs))),col="#AADDAA", border = NA)
grid(lty=1,col="white")
axis(2,las=1,at =c(150000,100000,50000,0,-50000,-100000,-150000), 
		labels=c("150000","100000","50000","0","50000","100000","150000"))
axis(1)
text(1860,-240000,"Reference year", xpd = TRUE,cex=1.5)
text(1640,200000,"Births in Year", cex = 1.5,xpd=TRUE,pos=4)
text(1640,-200000,"Offspring", cex = 1.5,xpd=TRUE,pos=4)

#rect(1775,-150000,1875,0)
polygon(c(1736,1736,1836,fyrs),c(0,-150000,-150000,-frac))

text(1736,-127000,"Too smooth\n(artifact of pclm\nand indirect methods)",pos=4)
text(1736,-130000,"B",font=2,cex=1.5,xpd=TRUE,pos=2)
rect(1736,150000,1890,0)
text(1736,170000,"Ages graduated smoothly",pos=4,xpd=TRUE)
text(1736,170000,"A",font=2,cex=1.5,xpd=TRUE,pos=2)

rect(1876,-155000,1968,155000,border=gray(.2),lty="82")
text(1876,175000,"Structural echo preserved",pos=4,xpd=TRUE)
text(1876,175000,"C",font=2,cex=1.5,xpd=TRUE,pos=2)

dev.off()


per  <- rowSums(PCi)
yrs  <- as.integer(names(per))
coh  <- colSums(PCi)
cohs <- as.integer(names(coh)) 
PCc  <- apply(PCi,2,cumsum)
frac <- PCc["1890",as.character(fyrs)]
pdf("Figures/App_postAdjustment.pdf",height=5,width=10)
plot(NULL,type="n", ann = FALSE, axes = FALSE, xlim = c(1680, 2017), ylim = c(-150000,150000))
polygon(x=c(yrs,rev(yrs)),y=c(per,rep(0,length(yrs))),col="#AADDDD", border = NA)
polygon(x=c(cohs,rev(cohs)),y=c(-coh,rep(0,length(cohs))),col="#AADDAA", border = NA)
grid(lty=1,col="white")
axis(2,las=1,at =c(150000,100000,50000,0,-50000,-100000,-150000), 
		labels=c("150000","100000","50000","0","50000","100000","150000"))
axis(1)
polygon(c(1736,1736,1836,fyrs),c(0,-150000,-150000,-frac))
text(1736,-130000,"Adjusted B(c)'",pos=4)
text(1860,-240000,"Reference year", xpd = TRUE,cex=1.5)
text(1640,200000,"Births in Year", cex = 1.5,xpd=TRUE,pos=4)
text(1640,-200000,"Offspring", cex = 1.5,xpd=TRUE,pos=4)
dev.off()

# -----------------------------
# gray bins joined:
pdf("Figures/JoinBins.pdf",height=5,width=10)
plot(NULL, xlim = range(1680,2071),ylim=c(-170000,170000), axes = FALSE, xlab = "", ylab = "")
par(mai = c(1,1,.4,0))
for (i in 1:(nrow(PC5cs) - 1)){
	polygon(c(Yrs, rev(Yrs)), 
			c(PC5cs[i, ], rev(PC5cs[i + 1, ])), 
			col = gray(.8), 
			border = "white", 
			lwd = 1)
}
# bottom
for (i in 1:(nrow(P5Ccs) - 1)){
	polygon(c(Cohs, rev(Cohs)), 
			-c(P5Ccs[i, ], rev(P5Ccs[i + 1, ])), 
			col = gray(.8), 
			border = "white", 
			lwd = 1)
	
}
axis(2,las=1,at =c(150000,100000,50000,0,-50000,-100000,-150000), 
		labels=c("150000","100000","50000","0","50000","100000","150000"))
axis(1,at=seq(1700,2050,by=50))
text(1630,200000,"Births in Year", cex = 1.5,xpd=TRUE,pos=4)
text(1630,-210000,"Offspring", cex = 1.5,xpd=TRUE,pos=4)
text(1860,-250000,"Reference year", xpd = TRUE,cex=1.5)
dev.off()

# ---------------------------
# color key:

#plot(NULL, xlim = c(1700,2030), ylim = c(4,7), ann = FALSE, axes=FALSE)
#lines(Cohs, Coh_SD, col = "red")
#lines(yrs, Per_SD, col = "blue")
#axis(1)
#axis(2)
#range(Per_SD,na.rm=TRUE)
#range(Coh_SD,na.rm=TRUE)
#which.max(Per_SD)
#which.min(Per_SD)
#
#which.max(Coh_SD)
#which.min(Coh_SD)


# ---------------------------
#compareplot <- function(PCin, PCi, year = 1800){
#	yrc <- as.character(year)
#	ind <- PCin[yrc, ] > 0
#	
#	plot(PCin[yrc, ind])
#	lines(PCi[yrc, ind])
#}
#for (i in 1810:1895){
#	compareplot(PCin, PCi, year = i)
#	title(i, cex = 2)
#	locator(1)
#}
#compareplot(PCin, PCi, year = 1885)
#
#
#plot(1775:1968,coh[yrsc]/per[yrsc],type='l')
#
#rper <- diff(per)/per[-length(per)]
#rcoh <- diff(coh)/coh[-length(coh)]
#yrsm <- 1775:1968
#plot(rper[yrsc],rcoh[yrsc],asp=1,type="n")
#abline(a=0,b=1)
#points(rper[yrsc],rcoh[yrsc],col=ifelse(yrsm<1876,"red",gray(.5)))
#yrsm <- 1775:1968
#abline()
#
#afteri <- as.character(1876:1968)
#plot(rper[afteri],rcoh[afteri],asp=1,type="n")
#abline(a=0,b=1)
#points(rper[afteri],rcoh[afteri])
#abline(lm(rcoh[afteri]~rper[afteri]),col="red")
#
#beforei <- as.character(1775:1875)
#plot(rper[beforei],rcoh[beforei],asp=1,type="n")
#abline(a=0,b=1)
#points(rper[beforei],rcoh[beforei])
#abline(lm(rcoh[beforei]~rper[beforei]),col="red")
#
#plot(1775:1968,rper[yrsc],type='l', ylim=c(-.2,.2))
#lines(1775:1968,rcoh[yrsc],col="red")
#abline(v=1876)
#
#plot(1775:1968,rper[yrsc]-rcoh[yrsc])
#abline(v=1876)
#abline(h=0)
#
## what if the adjustment were not to send a difference shock,
## but rather hmmm
#
#plot(1775:1968,(rper[yrsc]-rcoh[yrsc])/rper[yrsc])
#abline(v=1876)
#abline(h=0)

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

# for email to Pop Studies, 24-Aug-2018
#w <- 210
#h <- 297
#x <- seq(0,w * 4,length=5)
#getwd()
#png("Figures/a4demonstration.png",width=840,height=297)
#par(mai = c(.1,.1,.1,.1))
#plot(NULL, type = "n", ann = FALSE, axes = FALSE, asp = 1, xlim = c(0,840),ylim=c(0,297))
#rect(x[-5],0,x[-1],h,lwd=2)
#dev.off()
#
#228.60/
#685.80
#
#297 840
#297/840


plot(Yrs,PC[,"1920"],xlim=c(1935,1965))
LexisUtils::PC2AP(PC)[,as.character(1945:1949)]
1945-1919
dim(PC)
sum(PC["1920",]) # mother cohort contrib to1920 cohort
sum(PC[,"1920"]) # offspring of 1920 cohort

yi <- as.character(1910:1930)
plot(Yrs, PC[,"1920"] / rowSums(PC), xlim=c(1935,1960), typ = "l")
matplot(Yrs, PC[,yi] / rowSums(PC), type = 'l',lty=1,col = gray(seq(0,.8,length=length(yi))),add=TRUE)
lines(Yrs, PC[,"1919"] / rowSums(PC), col = "blue")
lines(Yrs, PC[,"1921"] / rowSums(PC), col = "red")
lines(Yrs, PC[,"1920"] / rowSums(PC), col = "magenta",lwd=2)
lines(Yrs, adj * PC[,"1920"] / rowSums(PC), col = "magenta",lwd=2, lty=2)


yii <- as.character(Cohs)
rle(Cohs[apply(PC / rowSums(PC),1,which.max)])$values[(rle(Cohs[apply(PC / rowSums(PC),1,which.max)])$lengths)>7]

# counterfactual 1920 cohort size
C1920_counterfactual <- (Bt["1919"] + Bt["1921"]) / 2
adj <- C1920_counterfactual / Bt["1920"] 

Bc1920_counterfactual <- (Bc["1919"] + Bc["1921"]) / 2
adj2 <- Bc1920_counterfactual / Bc["1920"] 

PC2 <- PC
PC2[,"1920"] <- adj * PC2[,"1920"]
PC3 <- PC
PC3[,"1920"] <- adj2 * PC3[,"1920"]
plot(Yrs, PC[,"1920"] / rowSums(PC), xlim=c(1935,1960), typ = "l")
matplot(Yrs, PC / rowSums(PC), type = 'l',lty=1,col = gray(seq(0,.8,length=length(Cohs))),add=TRUE)
lines(Yrs, PC[,"1919"] / rowSums(PC), col = "blue")
lines(Yrs, PC[,"1921"] / rowSums(PC), col = "red")
lines(Yrs, PC[,"1920"] / rowSums(PC), col = "magenta",lwd=2)
lines(Yrs, PC2[,"1920"] / rowSums(PC2), col = "magenta",lwd=2, lty=2)
lines(Yrs, PC3[,"1920"] / rowSums(PC3), col = "magenta",lwd="2", lty="82")

Bt / rowSums(PC2) 

Bc1920_counterfactual <- (Bc["1919"] + Bc["1921"]) / 2
adj2 <- Bc1920_counterfactual / Bc["1920"] 
(1-adj2) * Bc["1920"] 

plot(Yrs,Bt,xlim=c(1920,1965))
abline(v=c(1939,1951))


# and how much excess in boom, bounded by 1940-1951
x1 <- 1940
x2 <- 1951
y1 <- Bt["1940"]
y2 <- Bt["1951"]
m  <- (y2 - y1) / (x2 - x1)
yt <- 1:10
y1 + yt * m
# and of the 'excess' that is the baby boom?
Bt2  <- Bt
w1 <- as.character(1941:1950)
Bt2[w1] <- y1 + yt * m

plot(Yrs,Bt,xlim=c(1920,1965))
abline(v=c(1939,1951))
lines(Yrs,Bt2)

Bt - Bt2

# excess 1920 births
((Bt - rowSums(PC2)) / 
		# excess baby boomers
		(Bt - Bt2))[w1]

# 1% of 1st wavers
sum(Bt[w1])/
sum(rowSums(PC2)[w1])
# similar
sum(Bt[w1])/
		sum(rowSums(PC3)[w1])
# 4-5 % of excess boomers
sum((Bt - rowSums(PC2))[w1]) / 
		sum((Bt - Bt2)[w1])
