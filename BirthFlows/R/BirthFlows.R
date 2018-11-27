setwd("/home/tim/git/BirthFlows/BirthFlows")
#source("R/DataPrep.R")
#source("R/ColorRamp.R")


# -------------------------------
#PeriodColors <- autumn2
#CohortColors <- autumn1
# -------------------------------
# a simple flat-line undecomposed version of the graphic,
# as a cleaner reference
yp           <- as.integer(names(BT)) 
yc           <- as.integer(names(BC))
#length(intersect(yp,yc))
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

# ------------

# color based on area in bar
# PeriodColors <- colorRampPalette(brewer.pal(9,"YlGn"),space="Lab")(14)
# CohortColors <- colorRampPalette(brewer.pal(9,"YlGnBu"),space="Lab")(14)

#show.pal(vangoghwheat)

# length(rownames(PC5cs))
# length(ColsC)
# length(totals)
#totals <- rowSums(PC5)
#qts    <- quantile(totals[totals > 3e5], seq(0,1,length=(length(PeriodColors) + 1)))
#ColsC  <- c(rep(gray(.8),6),as.character(
#	cut(totals[-c(1:6,(length(totals)-5):length(totals))], 
#		breaks = qts,
#		labels = PeriodColors, include.lowest = TRUE)),rep(gray(.8),5))
#
#
#totalsC <- rowSums(P5C)/5
#qts2    <- quantile(totalsC, seq(0,1,length=(length(CohortColors) + 1)))
#ColsP   <- as.character(
#	cut(rowSums(P5C)/rep(5,nrow(P5C)), 
#		breaks = qts2,
#		labels = CohortColors, include.lowest = TRUE))

## Colors based on CV
#breaks <- seq(.16,.23,by=.005)
#ColsC <- as.character(cut(Per_CV5,breaks=breaks,labels=colsRamp(length(breaks)-1)))
#ColsP <- as.character(cut(Coh_CV5,breaks=breaks,labels=colsRamp(length(breaks)-1)))
#ColsP[is.na(ColsP)] <- gray(.8)
#
## Colors based on SD
#breaks <- seq(4.5,7,by=.1)
#ColsC <- as.character(cut(Per_SD5,breaks=breaks,labels=colsRamp(length(breaks)-1)))
#ColsP <- as.character(cut(Coh_SD5,breaks=breaks,labels=colsRamp(length(breaks)-1)))
#ColsP[is.na(ColsP)] <- gray(.8)
#
## Colors based on IQR
#breaks <- seq(6,11,by=.1)
#ColsC <- as.character(cut(Per_IQR5,breaks=breaks,labels=colsRamp(length(breaks)-1)))
#ColsP <- as.character(cut(Coh_IQR5,breaks=breaks,labels=colsRamp(length(breaks)-1)))
#ColsP[is.na(ColsP)] <- gray(.8)
#
## try our viridis on IQR
#breaks <- seq(6,11,by=.1)
#ColsC <- as.character(cut(Per_IQR5,breaks=breaks,labels=viridis(length(breaks)-1, option = "A")))
#ColsP <- as.character(cut(Coh_IQR5,breaks=breaks,labels=viridis(length(breaks)-1, option = "A")))
#ColsP[is.na(ColsP)] <- gray(.8)

# try our viridis on SD

# TODO: can add color 1 polygon earlier to ColsP. Later ones too after projection.
breaks <- pretty(c(Per_SD5,Coh_SD5),35)
ColsC <- as.character(cut(Per_SD5,breaks=breaks,labels=viridis(length(breaks)-1, option = "A")))
ColsP <- as.character(cut(Coh_SD5,breaks=breaks,labels=viridis(length(breaks)-1, option = "A")))
ColsP[is.na(ColsP)] <- gray(.8)
ColsC[is.na(ColsC)] <- gray(.8)
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
xticks    <- seq(1680,2070, by = 10)
xticksc   <- as.character(xticks)
xticks20  <- seq(1680, 2060, by = 20)
xticks20c <- as.character(xticks20)

graphics.off()

# 4 a4 sheets = 840mm wide, 297mm tall.
#wnew <- 33.0709
#hnew <- 11.6929
## curves calibrated to particular xlim, ylim, asp. Need to back them out...
## device size = 9h, 27w
#mai   <- c(1.2,1.5,1.2,.5)
#xr    <- 2020 - 1680
#yr    <- 50e4
#dh    <- 9
#dw       <- 27
#plotw    <- dw - mai[2] - mai[3]
#ploth    <- dh - mai[1] - mai[4]
#
## years per inch
#ypi      <- xr / plotw
#
## height to add to margins:
#hadd     <- hnew - 9
#mainew   <- mai + c(hadd/2,0,hadd / 2,0)
#dput(mainew)
## years to add to x
#wadd     <- wnew - 27
#yearsadd <- wadd * ypi

pdf("Figures/SwedenBirthFlowsR5.pdf", height = 11.6929, width = 33.0709)
#dev.new(height = 9, width= 27)
par(mai = c(2.54645, 1.5, 2.54645, 0.5),
	xpd = TRUE,
	xaxs = "i",
	yaxs = "i")
# empty device, pre-specified ranges. Very careful adjusting, 
# as it will throw off curved text!!
plot(NULL, 
	 type = "n", 
	 xlim = c(1660, 2084.94263374486), # odd nr, due to recalculations
	 ylim = c(-25e4, 25e4),            # that preserve aspect ratio after   
	 axes = FALSE,                     # a device resize
	 xlab = "", 
	 ylab = "")

# y-guides
segments(xticks20,
		baseline[xticks20c] + 15e4,
		 xticks20,
		 baseline[xticks20c] - 15e4,
		 col = gray(.7))

# y axis reference lines
for (i in 1:length(yticks)){
	lines(yrs_smooth, 
			yticks[i] + baseline, 
			col = gray(.8), 
			lwd = .6)
}


# shaded polygons
# top
for (i in 1:(nrow(PC5cs) - 1)){
	polygon(c(Yrs, rev(Yrs)), 
			c(PC5cs2[i, ], rev(PC5cs2[i + 1, ])), 
			col = ColsP[i], 
			border = "#FFFFFF80", 
			lwd = .2)
}
# bottom
for (i in 1:(nrow(P5Ccs) - 1)){
	polygon(c(Cohs, rev(Cohs)), 
			-c(P5Ccs2[i, ], rev(P5Ccs2[i + 1, ])), 
			col = ColsC[i], 
			border = "#FFFFFF80", 
			lwd = .2)

}

# labelling every second polygon (every 10 years)
# hackish code
# Year of ocurrence labels pertain to bottom polygons
Y5 <- Yrs[Yrs%%5 == 0]
for (i in seq(2, nrow(P5Ccs)-2, by = 2)){
	if (Y5[i - 1] != 1750){
	ind <- which(P5C[as.character(Y5[i]), ] > 1000)[1]
	text(Cohs[ind], 
			-P5Ccs2[i, ind], 
			Y5[i - 1], 
			cex = .5, 
			col = ifelse(Y5[i] < 1960,"#33333380","#FFFFFF80"))
}
}
# mother cohort labels pertain to top polygons
C5 <- Cohs[Cohs%%5 == 0]
for (i in seq(2,nrow(PC5cs)-2,by=2)){
	if (C5[i] >= 1700 & C5[i-1] != 1720){
	ind <- which(PC5[as.character(C5[i]),] > 500)[1]
	text(Yrs[ind], 
			PC5cs2[i, ind],
			C5[i - 1], 
			cex = .5, 
			col = ifelse(C5[i] < 1910,"#33333380","#FFFFFF80"))
}
}

# centerline
lines(yrs_smooth, baseline, col = "white")
# centerline axis ticks
segments(xticks, 
		baseline[xticksc] - 5000, 
		xticks, 
		baseline[xticksc] + 5000, 
		col = "white")
# outer x axis labels
# bottom
text(xticks20, 
		baseline[xticks20c] - 15e4, 
		xticks20, 
		pos = 1, 
		cex = .7, 
		xpd = TRUE)
# top
text(xticks20, 
		baseline[xticks20c] + 15e4, 
		xticks20, 
		pos = 3, 
		cex = .7, 
		xpd = TRUE)

# grid lines over polygons
for (i in 1:length(yticks)){
	lines(yrs_smooth, 
			yticks[i] + baseline, 
			col = "#B3B3B3A0", 
			lwd = .5)
}

# crude cohort replacement line
segments(1736,
		0,
		2016,
		0,
		col = "white",
		lwd = .5)

# ----------------------------------------------------
# lineage of Alva Myrdal
# arcs: 
# arcs connecting diagonally for some reason require a hack
for (i in 1:5){
	vt <- L2$ytop[i] - L2$ybottom[i]
	draw.arc(L2$C[i],
			L2$ybottom[i],
			L2$C[i],
			L2$ybottom[i]-vt,
			col = "#7B6A93",
			lwd = .5,
			lty = "74",
			brel = .00015) # use to adjust 'curviness'
}
# arcs between top and bottom uncomplicated
for (i in 1:5){
	draw.arc(L2$C[i],
			L2$ybottom[i],
			L2$C[i+1],
			L2$ytop[i+1],
			col="#7B6A93",
			lwd=.5,
			lty="74",
			brel=.00015)
}
# points and text atop arcs
# top points
points(L2$C, 
		L2$ytop, 
		pch = 16, 
		col = "#21114C",
		cex = .8)
# bottom points
points(L2$C[-6], 
		L2$ybottom[-6], 
		pch = c(rep(16,4),1), 
		col = "#21114C",
		cex = .8)
# top name labels
text(L2$C,
		L2$ytop,
		L2$first,
		col = "#21114C",
		cex = .7,
		pos = 4)
# bottom name labels
text(L2$C[-6],
		L2$ybottom[-6],
		L2$first[-1],
		col = "#21114C",
		cex = .7,
		pos = 4)
# ----------------------------------------------------

# NOTE: arctext positioning is very sensitive to xlim, ylim, asp. Positioning
# is done via specifying a circle centroid, radius, and radian at which to render the text.
# smaller radius = more curved. But things don't seem to scale equally with regular x,y shifts
# so repositioning takes trial and error. For this reason, only adjust when rendering to the
# output pdf device, not in an interactive graphics device.
arctext("gave birth to", 
		center = c(1822.3, 45000), 
		middle = .2, 
		radius = 12,
		col = "#21114C",
		cex = .6)

arctext("appeared in the birth series in 1856", 
		center = c(1803, 425000), 
		middle = -.335 * pi, 
		radius = 81,
		col = "#21114C",
		cex = .6,
		clockwise = FALSE)

# polygon annotation
arctext("child born in year 1750", 
		center = c(1712, 64700), 
		middle = -pi / 2, 
		radius = 15,
		col = gray(.2),
		cex = .6,
		clockwise = FALSE)

arctext("mother born in year 1720", 
		center = c(1790, 430000), 
		middle = 1.34 * pi, 
		radius = 80.5,
		col = gray(.2),
		cex = .6,
		clockwise = FALSE)

# to make sure text on top
text(1810, 6000, "crude generation growth", col = "#FFFFFFA0",cex=1)
text(1904, -8000, "crude generation contraction", col = "#FFFFFFA0",cex=1)

##################################################
# custom axes
# left y axis
push <- baseline[1] # everything needs to get 
                    # shifted to deal w meandering baseline
segments(min(Cohs),
		-15e4 + push, 
		min(Cohs), 
		15e4 + push)

segments(min(Cohs), 
		yticks + push, 
		min(Cohs) + 1, 
		yticks + push)
text(min(Cohs), 
		yticks + push,
		c(150,100,50,50,100,150),
		pos = 2,
		cex = .8)

# right y axis
push2 <- baseline[length(baseline)]
segments(max(Yrs),
		-15e4 + push2, 
		max(Yrs), 
		15e4 + push2)
segments(max(Yrs), 
		yticks + push2, 
		max(Yrs) - 1, 
		yticks + push2)
text(max(Yrs), 
	 yticks + push2, 
	 c(150,100,50,50,100,150), 
	 pos = 4, 
	 cex = .8)

# large axis labels
text(min(Cohs), 
		18e4 + push, 
		"Births in year (1000s)", 
		pos = 4, 
		cex = 1.1, 
		font = 2)
text(min(Cohs), 
		-18e4 + push, 
		"The children they had", 
		pos = 4, 
		cex = 1.1, 
		font = 2)
# ------------------------------------------------------------------------
# end
dev.off()


sum(SWE$Total[SWE$Cohort == 1971 & SWE$Year <= 2016]) / sum(SWE$Total[SWE$Cohort == 1971])
