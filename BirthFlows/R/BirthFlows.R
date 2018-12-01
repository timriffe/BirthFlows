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

# try our viridis on SD

# determine colors
# TODO: can add color 1 polygon earlier to ColsP. Later ones too after projection.
breaks     <- pretty(c(Per_SD5,Coh_SD5),35)
fillcolors <- viridis(length(breaks) - 1, option = "A")

ColsC               <- as.character(cut(Per_SD5, breaks = breaks, labels = fillcolors))
ColsP               <- as.character(cut(Coh_SD5, breaks = breaks, labels = fillcolors))
ColsP[is.na(ColsP)] <- gray(.8)
ColsC[is.na(ColsC)] <- gray(.8)

# some plotting indices
yticks    <- c(-15e4,-10e4,-5e4,5e4,10e4,15e4)
xticks    <- seq(1680,2070, by = 10)
xticksc   <- as.character(xticks)
xticks20  <- seq(1680, 2060, by = 20)
xticks20c <- as.character(xticks20)

# clear devices
graphics.off()

# ----------------------------------------
# save code used to recalibrate size and aspect ratio
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
draw.fork <- function(x1,x2,x3,y1,y2,y3,y4,...){
	segments(x1,y1,x1,y2,...)
	segments(x2,y2,x3,y2,...)
	segments(x2,y2,x2,y3,...)
	segments(x3,y2,x3,y4,...)
}
# ----------------------------------------

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

# ------------------------------------------------------------------------
segments(1743,baseline["1743"],1743,3e5,lwd=.8,col="#2a70e0",lty="12")
text(1743,3e5,"armed conflict",pos=3,cex=.7,col="#2a70e0")
# famine "#9fdaf9"
segments(1773,baseline["1773"],1773,3e5,lwd=.8,col="#2a70e0",lty="12")
text(1773,3e5,"famine",pos=3,cex=.7,col="#2a70e0")
# famine or war
segments(1790,baseline["1790"],1790,3e5,lwd=.8,col="#2a70e0",lty="12")
text(1790,3e5,"armed conflict",pos=3,cex=.7,col="#2a70e0")
# famine or war (make this one bend in...)
segments(1800.2,3.2e5,1800.2,baseline["1800"],lwd=.8,col="#2a70e0",lty="12")
text(1800,3.2e5,"armed conflict",pos=3,cex=.7,col="#2a70e0")

# conflict and upheaval
segments(1809,baseline["1809"],1809,3e5,lwd=.8,col="#2a70e0",lty="12")
text(1809,3e5,"armed conflict",pos=3,cex=.7,col="#2a70e0")

# the year without summer
#segments(1816,baseline["1816"]-150e3,1816,3.2e5,lwd=.8,col="#2a70e0",lty="12")
#text(1816,3.2e5,"'the year\nwithout summer'",pos=3,cex=.7,col="#2a70e0")
# pandemic
draw.fork(1832,1831,1833,
		y1=3e5,y2=1.4e5,baseline["1831"]+Bt["1831"],baseline["1833"]+Bt["1833"]
		,lwd=.8,col="#2a70e0",lty="12") 
text(1832,3e5,"pandemic",pos=3,cex=.7,col="#2a70e0")

# pandemic

draw.fork(1847.5,1847,1848,
		y1=3e5,y2=1.4e5,baseline["1847"]+Bt["1847"],baseline["1848"]+Bt["1848"]
		,lwd=.8,col="#2a70e0",lty="12") 
text(1847.5,3e5,"pandemic",pos=3,cex=.7,col="#2a70e0")

# famine
draw.fork(1868,1867,1869,
		y1=3e5,y2=1.4e5,baseline["1867"]+Bt["1867"],baseline["1869"]+Bt["1869"]
		,lwd=.8,col="#2a70e0",lty="12") 
text(1868,3e5,"famine",pos=3,cex=.7,col="#2a70e0")

# Russian pandemic
segments(1889,baseline["1889"],1889,3e5,lwd=.8,col="#2a70e0",lty="12")
text(1889,3e5,"Russian pandemic",pos=3,cex=.7,col="#2a70e0")

# Spanish flu
draw.fork(1919.5,1919,1920.5,
		y1=3e5,y2=1.45e5,baseline["1919"]+Bt["1919"],baseline["1920"]+Bt["1920"]
		,lwd=.8,col="#2a70e0",lty="12") 
#segments(1918.5,1.19e5,1918.5,3e5,lwd=.8,col="#2a70e0",lty="12")
text(1919,3e5,"Spanish influenza\nand recovery",pos=3,cex=.7,col="#2a70e0")
# -------- end annotations ----------- #

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
			PC5cs2[i, ind]+3e3,
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
		20e4 + push, 
		"Births in year (1000s)", 
		pos = 4, 
		cex = 2, 
		font = 2)
text(min(Cohs), 
		-20e4 + push, 
		"The children they had", 
		pos = 4, 
		cex = 2, 
		font = 2)

# caption about here box
bottom <- -40e4
top    <- -25e4
rect(1660,top, 1740,bottom)
text( 1675,top, "Caption about here",pos=1)
# color strip legend

xl <- 1745
w  <- 5
h <- top - bottom
n <- length(breaks)
#ind <- 1:n %% 2 == 0
#yt <- seq(bottom,top,length=sum(ind))
#nn <- length(yt)
#rect(xl, yt[-nn],xl+w,yt[-1],border="white",col=fillcolors[ind],lwd=.4)
#text(xl+w,yt,breaks[ind],pos=4)

yt <- seq(bottom,top,length=n)
nn <- length(yt)
rect(xl, yt[-nn],xl+w,yt[-1],border="white",col=fillcolors,lwd=.4)

ind <- abs(breaks %% .25) < 1e-6
text(xl+w,yt[ind],sprintf("%.2f",breaks[ind]),pos=4,cex=.8)
segments(xl+w,yt[ind],xl+w+1,yt[ind])
# title legend
text(xl-w,top+2e4,"standard deviation of\nage at childbearing (years)",pos=4)

# end
dev.off()




