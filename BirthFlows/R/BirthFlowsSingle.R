setwd("/home/tim/git/BirthFlows/BirthFlows")
#source("R/DataPrep.R")
#source("R/ColorRamp.R")
 source("R/AnnotationPoints.R")
#PC5cs <- apply(PC5,2,cumsum) # single years in columns
PCcs  <- apply(PC,1,cumsum)
CPcs  <- apply(PC,2,cumsum)
#P5Ccs <- apply(P5C,2,cumsum) # single cohorts in columns
PCcs <- rbind(0,PCcs)
CPcs <- rbind(0,CPcs)

# shift flow
PCcs2 <- t(t(PCcs) + baseline[colnames(PCcs)])
CPcs2 <- t(t(CPcs) - baseline[colnames(CPcs)])

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
breaks     <- pretty(c(Per_SD,Coh_SD),35)
fillcolors <- viridis(length(breaks) - 1, option = "A")

ColsC               <- as.character(cut(Per_SD, breaks = breaks, labels = fillcolors))
ColsP               <- as.character(cut(Coh_SD, breaks = breaks, labels = fillcolors))
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
range(Per_SD,na.rm=TRUE)
range(Coh_SD,na.rm=TRUE)
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

# ----------------------------------------

pdf("Figures/BirthFlowsSingleFoldout.pdf", height = 11.6929, width = 33.0709)
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
for (i in 1:(nrow(PCcs) - 1)){
	polygon(c(Yrs, rev(Yrs)), 
			c(PCcs2[i, ], rev(PCcs2[i + 1, ])), 
			col = ColsP[i], 
			border = ColsP[i], 
			lwd = .1)
}
# bottom
for (i in 1:(nrow(CPcs) - 1)){
	polygon(c(Cohs, rev(Cohs)), 
			-c(CPcs2[i, ], rev(CPcs2[i + 1, ])), 
			col = ColsC[i], 
			border = ColsC[i], 
			lwd = .1)

}
projLine <- -CPcs2["2016",]
projLine[Cohs < 1962 | Cohs > 2004] <- NA
lines(Cohs,projLine,col="white",lty="84",lwd=.4)
text(1985,projLine["1985"],"2017",col="white",srt=53,cex=.6,pos=4)
segments(2016,baseline["2016"],2016,Bt["2016"]+baseline["2016"],col="white",lty="84",lwd=.4)
# 5-year borders
for (i in 1:(nrow(PC5cs) - 1)){
	polygon(c(Yrs, rev(Yrs)), 
			c(PC5cs2[i, ], rev(PC5cs2[i + 1, ])), 
			col = NA, 
			border = "#FFFFFF", 
			lwd = .2)
}
# bottom
for (i in 1:(nrow(P5Ccs) - 1)){
	polygon(c(Cohs, rev(Cohs)), 
			-c(P5Ccs2[i, ], rev(P5Ccs2[i + 1, ])), 
			col = NA, 
			border = "#FFFFFF", 
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



# other annotation points added, so be 

# simple timeline pointers
lapply(notes1, function(X, baseline, Bt){
			x <- X$r
			xc <- as.character(x)
			y1 <- baseline[xc] + Bt[xc]
			y2 <- 3e5
			
			if ("adj" %in% names(X)){
				x <- x + X$adj$x
				y2 <- y2 + X$adj$y
			}
			
			segments(x,y1,x,y2, lwd=.5,col = "#9AD0D8")
			text(x,y2,X$m,pos=3,cex=.7,col = "#74A2A9")
		}, baseline = baseline, Bt = Bt)
# fork timeline pointers:
lapply(notes_fork, function(X, baseline, Bt){
			x2 <- X$xl
			x3 <- X$xr
			x2c <- as.character(x2)
			x3c <- as.character(x3)
			x1 <- (x2+x3)/2
			y1 <- 3e5
			y3 <- baseline[x2c] + Bt[x2c]
			y4 <- baseline[x3c] + Bt[x3c]
			y2 <- max(c(y3,y4)) + 20000
			draw.fork(x1,x2,x3,y1,y2,y3,y4,lwd=.5,col="#9AD0D8") 
			text(x1, y1, X$m, cex=.7,col="#74A2A9"  ,pos=3)
		}, baseline = baseline, Bt = Bt)

# these ones also need referse labels... do in inkscape
lapply(notes, function(X, baseline, Bt, Bc){
			x <- X$r
			xc <- as.character(x)
			y1 <- baseline[xc] + X$d * ifelse(X$d == 1, Bt[xc],Bc[xc])
			y2 <- y1 + X$d * 1.1e5
			segments(x,y1,x,y2, lwd=.5,col = gray(.3))
			text(x,y2,X$m,pos=ifelse(X$d == 1,3,1),cex=.7,col = gray(.3))
		}, baseline = baseline, Bt = Bt, Bc = Bc)



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
text(min(Cohs)-15, 
		20e4 + push, 
		"Births in year (1000s)", 
		pos = 4, 
		cex = 2, 
		font = 2)
text(min(Cohs)-15, 
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




