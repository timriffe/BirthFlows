setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")
source("R/ColorRamp.R")
# -------------------------------
# an explanation of where the meander comes from
# pdf("/home/tim/workspace/Other/IDEMVIZ/Figures/Meander.pdf")
# plot(yrs,meander,pch=16,col = "#00000050",ylab = "Child/Mother", xlab = "Year/Cohort", axes = FALSE)
# axis(2);axis(1,seq(1780,1960,by=20))
# lines(yrs,meander_smoothed[yrsc])
# dev.off()
# -------------------------------
PeriodColors <- autumn2
CohortColors <- autumn1
# -------------------------------
# a simple flat-line undecomposed version of the graphic,
# as a cleaner reference
yp           <- as.integer(names(BT)) 
yc           <- as.integer(names(BC))
# pdf("/home/tim/workspace/Other/IDEMVIZ/Figures/flatline.pdf",width=9,height=5)
# plot(yp,BT, xlim=range(yrs_smooth),ylim=c(-15e4,15e4),type = 'n', 
# 	 axes = FALSE, xlab = "", ylab = "")
# polygon(c(yp,rev(yp)),
# 		c(BT,rep(0,length(yp))),
# 		col = "#88CD7F",
# 		border=NA)
# polygon(c(yc,rev(yc)),
# 		c(-BC,rep(0,length(yc))),
# 		col = "#58BCC1",
# 		border=NA)
# abline(h=seq(-150000,150000,by=50000),col="white",lwd=.5)
# abline(v=seq(1720,2000,by=20),col="white",lwd=.5)
# axis(2,seq(-150000,150000,by=50000),c(150,100,50,"",50,100,150),las=2)
# axis(1,seq(1780,1960,by=20))
# dev.off()
# -------------------------------

multiply <- 7e4
meander_smoothed <- meander_smoothed * multiply
# -------------------
# shift flow
PC5cs2 <- t(t(PC5cs) + meander_smoothed[colnames(PC5cs)])
P5Ccs2 <- t(t(P5Ccs) - meander_smoothed[colnames(P5Ccs)])




# get family coords
Lineage <- read.csv("Data/Swedishfamilybranch.csv")
Lineage$year.daughters.birth <- c(Lineage$date.of.birth[-1],NA)
Lineage$year.mothers.birth   <- c(1757,Lineage$date.of.birth[-5])
Lineage$age.of.mother        <- c(30,Lineage$age.at.daughters.birth[-5])
colnames(Lineage) <- c("first","last","C","AB","DC","MC","AM")
# Now. assign mother age quantile to birth?
# i <- 1
Lineage$ytop    <- NA
Lineage$ybottom <- NA
for (i in 1:5){
	ybase <- meander_smoothed[as.character(Lineage$C[i])]
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
# colnames(PC5)
# plot(1775:2014, PC5[,"1907"], xlim=c(1980,2015),type='l')

# PC5cs[, as.character(Lineage$date.of.birth)]

# ------------

# color based on area in bar
# PeriodColors <- colorRampPalette(brewer.pal(9,"YlGn"),space="Lab")(14)
# CohortColors <- colorRampPalette(brewer.pal(9,"YlGnBu"),space="Lab")(14)

#show.pal(vangoghwheat)

# length(rownames(PC5cs))
# length(ColsC)
# length(totals)
totals <- rowSums(PC5)
qts    <- quantile(totals[totals > 3e5], seq(0,1,length=(length(PeriodColors) + 1)))
ColsC  <- c(rep(gray(.8),6),as.character(
	cut(totals[-c(1:6,(length(totals)-5):length(totals))], 
		breaks = qts,
		labels = PeriodColors, include.lowest = TRUE)),rep(gray(.8),5))

totalsC <- rowSums(P5C)/5
qts2    <- quantile(totalsC, seq(0,1,length=(length(CohortColors) + 1)))
ColsP   <- as.character(
	cut(rowSums(P5C)/rep(5,nrow(P5C)), 
		breaks = qts2,
		labels = CohortColors, include.lowest = TRUE))

# -------------------------------
# a slice through the 1900 birth cohort, showing
# composition by birth cohort of mother.
#bt    <- PC5[as.character(seq(1845, 1885, by = 5)), "1900"]
#btcol <- ColsC[as.character(seq(1845, 1885, by = 5)), "1900"]

# pdf("/home/tim/workspace/Other/IDEMVIZ/Figures/CrossSection.pdf")
# plot(NULL, type = "n", axes = FALSE, xlim = c(1845,1890), ylim = c(0,max(bt)), xlab = "", ylab = "")
# rect(seq(1845,1885,by=5),0,seq(1850,1890,by=5),bt,col = ColsC[PC5[,"1900"]>0])
# text(seq(1845,1890,by=5),0,seq(1845,1890,by=5),pos=1 )
# axis(2,seq(0,35000,by=5000),seq(0,35,by=5),las=1)
# dev.off()

# start making the descendant bar chart (blues) corresponding to the above
#bd        <- P5C[,"1900"]
#btcol     <- ColsC[as.character(seq(1845,1885,by=5)),"1900"]
# -------------------------------


#setwd("/home/triffe/Desktop")
yticks    <- c(-15e4,-10e4,-5e4,5e4,10e4,15e4)
xticks    <- seq(1730,2010,by=10)
xticksc   <- as.character(xticks)
xticks20  <- seq(1740,2000,by=20)
xticks20c <- as.character(xticks20)

graphics.off()

# 64cm x 138cm is final dim
pdf("Figures/SwedenBirthFlowsR4.pdf", height = 9, width = 27)
#dev.new(height = 9, width= 27)
par(mai = c(1.2,1.5,1.2,.5),
	xpd = TRUE,
	xaxs = "i",
	yaxs = "i")
plot(NULL, 
	 type = "n", 
	 xlim = c(1710,2020), 
	 ylim = c(-25e4,25e4), 
	 axes = FALSE, 
	 xlab = "", 
	 ylab = "")
polygon(c(1710:1760,1760:1710),
		c(meander_smoothed[as.character(1710:1760)],
		  meander_smoothed[as.character(1760:1710)] - 15e4),
		col=gray(.8),border=gray(.8))
polygon(c(1975:2014,2014:1975),
		c(meander_smoothed[as.character(1975:2014)],
		  meander_smoothed[as.character(2014:1975)] - 15e4),
		col=gray(.8),border=gray(.8))
# y-guides
segments(xticks20,
		 meander_smoothed[xticks20c]+15e4,
		 xticks20,
		 meander_smoothed[xticks20c]-15e4,
		 col=gray(.7))

# y axis reference lines
for (i in 1:length(yticks)){
	lines(yrs_smooth, yticks[i] + meander_smoothed, col = gray(.8),lwd=.6)
}
push <- + meander_smoothed[1]
segments(1721,
		 -15e4 + push, 
		 1721, 
		 15e4 + push)
+ meander_smoothed[1]
segments(1721, 
		 yticks + push, 
		 1720, 
		 yticks + push)

text(1720,c(-15e4,-10e4,-5e4,5e4,10e4,15e4)+ push,c(150,100,50,50,100,150),pos=2,cex=.8)
text(1724,17e4+push,"Births in year (1000s)",pos=4,cex=.9)
text(1724,-18e4+push,"Births from cohort",pos=4,cex=.9)
#matplot(t(PC5[c("1720","1730","1740","1750"),]),type='l')
# draw the stacked area
for (i in 1:(nrow(PC5cs) - 1)){
	polygon(c(Yrs, rev(Yrs)), c(PC5cs2[i, ], rev(PC5cs2[i + 1, ])), col = ColsC[i], border = "#FFFFFF", lwd = .2)
    # label correctly
	# PC5["1995",]
	# PC5cs["1720",]
	# PC5cs["1725",]
    # dimnames(PC5cs) == dimnames(PC5cs2)
	}
for (i in 1:(nrow(P5Ccs) - 1)){
	polygon(c(Cohs, rev(Cohs)), -c(P5Ccs2[i, ], rev(P5Ccs2[i + 1, ])), col = ColsP[i], border = "#FFFFFF", lwd = .2)
}

lines(yrs_smooth, meander_smoothed, col = "white")
segments(xticks, meander_smoothed[xticksc] - 5000, xticks, meander_smoothed[xticksc] + 5000, col = "white")
text(xticks20, meander_smoothed[xticks20c] - 15e4, xticks20, pos = 1, cex = .7, xpd = TRUE)

# curvetickn <- 1
# for (ii in 1:length(yticks)){
# 	for (i in 1:length(xticks)){
# 		# +/- 3 years
# 		yrsi <- (xticks[i]-curvetickn):(xticks[i]+curvetickn)
# 		lines(yrsi, yticks[ii] + meander_smoothed[as.character(yrsi)], 
# 			  col = gray(1),lwd=.3)
# 	}
# }
for (i in 1:length(yticks)){
	lines(yrs_smooth, yticks[i] + meander_smoothed, col = "#FFFFFF50",lwd=.3)
}
#lines(yrs_smooth, yticks[i] + meander_smoothed, col = gray(.8),lwd=.4)
#text(1890,-24e4,"top bands indicate 5-year cohort contributions to births, while x axis is ocurrence year\nlower bands indicate 5-calendar-year groups, while x axis is birth cohort\ndarker shades indicate greater total births within group",pos=4,cex=.8)
#text(1920,23e4,"Sweden, total birth counts by periods and cohorts of mother (HFD)")
#
#rect(rep(1835,9), -qts[1:9],rep(1840,9),-qts[2:10],col=PeriodColors,border=gray(.8))
segments(1775,multiply,1975,multiply,col="white",lwd=.1)
text(1820, multiply + 4000, "crude generation growth",pos=4,col = "white",cex=.7)
text(1895, multiply - 5000, "crude generation contraction",pos=4,col = "white",cex=.7)

points(Lineage$C,Lineage$ytop,pch=16,col = "white",cex=1)
points(Lineage$C,Lineage$ybottom,pch=16,col = "white",cex=1)
text(Lineage$C,Lineage$ytop,Lineage$first,col ="white",cex=.7,pos=4)
text(Lineage$C[-5],Lineage$ybottom[-5],Lineage$first[-1],col ="white",cex=.7,pos=4)
#locator(1)
dev.off()



