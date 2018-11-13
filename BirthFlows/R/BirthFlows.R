setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")
#source("R/ColorRamp.R")


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
breaks <- pretty(c(Per_SD5,Coh_SD5),35)
ColsC <- as.character(cut(Per_SD5,breaks=breaks,labels=viridis(length(breaks)-1, option = "A")))
ColsP <- as.character(cut(Coh_SD5,breaks=breaks,labels=viridis(length(breaks)-1, option = "A")))
ColsP[is.na(ColsP)] <- gray(.8)
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
xticks    <- seq(1680,2010,by=10)
xticksc   <- as.character(xticks)
xticks20  <- seq(1680,2000,by=20)
xticks20c <- as.character(xticks20)

graphics.off()

for (measure in c("IQR","CV","MAB","SD")){
	for (rampi in c("mine",LETTERS[1:5],"blend")){
		
		# select ramp
		if (rampi == "mine"){
			this.ramp <- colsRamp
		} else {
			if (rampi == "blend"){
				this.ramp <- viridisblend
			} else {
				this.ramp <- function(n,option = rampi){
					viridis(n,option=option)
				}
			}
		}
		
		if (measure == "IQR"){
			valueP <- Per_IQR5; valueC <- Coh_IQR5
		}
		if (measure == "SD"){
			valueP <- Per_SD5; valueC <- Coh_SD5
		}
		if (measure == "MAB"){
			valueP <- Per_MAB5; valueC <- Coh_MAB5
		}
		if (measure == "SD"){
			valueP <- Per_SD5; valueC <- Coh_SD5
		}
		
		
		breaks <- pretty(c(valueP,valueC),n=25)
		
		ColsC <- as.character(cut(valueP,breaks=breaks,labels=this.ramp(length(breaks)-1)))
		ColsP <- as.character(cut(valueC,breaks=breaks,labels=this.ramp(length(breaks)-1)))
		ColsP[is.na(ColsP)] <- gray(.8)
		
		
		path <- file.path("Figures/ColorRamps",measure,paste0(rampi,".pdf"))
		pdf(path, height = 8, width = 27)
		par(mai = c(0,0,0,0),
				xpd = TRUE,
				xaxs = "i",
				yaxs = "i")
		plot(NULL, 
				type = "n", 
				xlim = c(1670,2020), 
				ylim = c(-25e4,25e4), 
				axes = FALSE, 
				xlab = "", 
				ylab = "")
		
# y-guides
		segments(xticks20,
				baseline[xticks20c]+15e4,
				xticks20,
				baseline[xticks20c]-15e4,
				col=gray(.7))
		
# y axis reference lines
		for (i in 1:length(yticks)){
			lines(yrs_smooth, yticks[i] + baseline, col = gray(.8),lwd=.6)
		}
		push <- baseline[1]
		segments(min(Cohs),
				-15e4 + push, 
				min(Cohs), 
				15e4 + push)
		
		segments(min(Cohs), 
				yticks + push, 
				min(Cohs)+1, 
				yticks + push)
		
		text(min(Cohs),c(-15e4,-10e4,-5e4,5e4,10e4,15e4)+ push,c(150,100,50,50,100,150),pos=2,cex=.8)
		text(min(Cohs)+3,17e4+push,"Births in year (1000s)",pos=4,cex=.9)
		text(min(Cohs)+3,-18e4+push,"Births from cohort",pos=4,cex=.9)
		
# polygons
		for (i in 1:(nrow(PC5cs) - 1)){
			polygon(c(Yrs, rev(Yrs)), c(PC5cs2[i, ], rev(PC5cs2[i + 1, ])), col = ColsP[i], border = "#FFFFFF", lwd = .2)
		}
		for (i in 1:(nrow(P5Ccs) - 1)){
			polygon(c(Cohs, rev(Cohs)), -c(P5Ccs2[i, ], rev(P5Ccs2[i + 1, ])), col = ColsC[i], border = "#FFFFFF", lwd = .2)
		}
		
		do.this <- FALSE
		if (do.this){
		# start experimental age bands
		matplot(Yrs,t(APc[age_bands,]) + 
						baseline[colnames(AP)],type='l',col="white",lwd=.2,lty=1,add=TRUE)
		
		lines(Yrs, apply(AP,2,function(Bx){
									y <- cumsum(Bx)
									x <- names2age(Bx)
									MAB <- wmean(w=Bx,x=x)
									splinefun(y~x, method = "monoH.FC")(MAB)
								})+ 
						baseline[colnames(AP)], col = "white")
		
		matplot(Cohs,t(-ACc[age_bands,]) + 
						baseline[colnames(AC)],type='l',col="white",lwd=.2,lty=1,add=TRUE)
		lines(Cohs, -apply(AC,2,function(Bx){
									y <- cumsum(Bx)
									x <- names2age(Bx)
									MAB <- wmean(w=Bx,x=x)
									splinefun(y~x, method = "monoH.FC")(MAB)
								})+ 
						baseline[colnames(AC)], col = "white")
	}
		# end experiemental age bands
		lines(yrs_smooth, baseline, col = "white")
		segments(xticks, baseline[xticksc] - 5000, xticks, baseline[xticksc] + 5000, col = "white")
		text(xticks20, baseline[xticks20c] - 15e4, xticks20, pos = 1, cex = .7, xpd = TRUE)
		
		for (i in 1:length(yticks)){
			lines(yrs_smooth, yticks[i] + baseline, col = "#FFFFFF50",lwd=.3)
		}
		
		segments(1736,0,rightCoh(SWE),0,col="white",lwd=.5)
		text(1820, 3000, "crude generation growth",col = "white",cex=.7)
		text(1900, -3000, "crude generation contraction",col = "white",cex=.7)
		
		points(Lineage$C,Lineage$ytop,pch=16,col = "white",cex=1)
		points(Lineage$C,Lineage$ybottom,pch=16,col = "white",cex=1)
		text(Lineage$C,Lineage$ytop,Lineage$first,col ="white",cex=.7,pos=4)
		text(Lineage$C[-5],Lineage$ybottom[-5],Lineage$first[-1],col ="white",cex=.7,pos=4)
		
		dev.off()
		
	}
}



# 64cm x 138cm is final dim
pdf("Figures/SwedenBirthFlowsR5.pdf", height = 9, width = 27)

#dev.new(height = 9, width= 27)
par(mai = c(1.2,1.5,1.2,.5),
	xpd = TRUE,
	xaxs = "i",
	yaxs = "i")
plot(NULL, 
	 type = "n", 
	 xlim = c(1680,2020), 
	 ylim = c(-25e4,25e4), 
	 axes = FALSE, 
	 xlab = "", 
	 ylab = "")

# y-guides
segments(xticks20,
		baseline[xticks20c]+15e4,
		 xticks20,
		 baseline[xticks20c]-15e4,
		 col=gray(.7))

# y axis reference lines
for (i in 1:length(yticks)){
	lines(yrs_smooth, yticks[i] + baseline, col = gray(.8),lwd=.6)
}
# left y axis
push <- baseline[1]
segments(min(Cohs),
		 -15e4 + push, 
		 min(Cohs), 
		 15e4 + push)

segments(min(Cohs), 
		 yticks + push, 
		 min(Cohs)+1, 
		 yticks + push)
text(min(Cohs),c(-15e4,-10e4,-5e4,5e4,10e4,15e4)+ push,c(150,100,50,50,100,150),pos=2,cex=.8)
text(min(Cohs),18e4+push,"Births in year (1000s)",pos=2,cex=.9)
text(min(Cohs),-18e4+push,"Births from cohort",pos=2,cex=.9)

# right y axis
push2 <- baseline[length(baseline)]
segments(max(Yrs),
		-15e4 + push2, 
		max(Yrs), 
		15e4 + push2)

segments(max(Yrs), 
		yticks + push2, 
		max(Yrs)-1, 
		yticks + push2)
text(max(Yrs),c(-15e4,-10e4,-5e4,5e4,10e4,15e4)+ push2,c(150,100,50,50,100,150),pos=4,cex=.8)


# polygons
for (i in 1:(nrow(PC5cs) - 1)){
	polygon(c(Yrs, rev(Yrs)), c(PC5cs2[i, ], rev(PC5cs2[i + 1, ])), col = ColsP[i], border = "#FFFFFF", lwd = .2)
	}
for (i in 1:(nrow(P5Ccs) - 1)){
	polygon(c(Cohs, rev(Cohs)), -c(P5Ccs2[i, ], rev(P5Ccs2[i + 1, ])), col = ColsC[i], border = "#FFFFFF", lwd = .2)
}

lines(yrs_smooth, baseline, col = "white")
segments(xticks, baseline[xticksc] - 5000, xticks, baseline[xticksc] + 5000, col = "white")
text(xticks20, baseline[xticks20c] - 15e4, xticks20, pos = 1, cex = .7, xpd = TRUE)
text(xticks20, baseline[xticks20c] + 15e4, xticks20, pos = 3, cex = .7, xpd = TRUE)

for (i in 1:length(yticks)){
	lines(yrs_smooth, yticks[i] + baseline, col = "#FFFFFF50",lwd=.3)
}

segments(1736,0,rightCoh(SWE),0,col="white",lwd=.5)
text(1820, 10000, "crude generation growth",col = "white",cex=.7)
text(1900, -10000, "crude generation contraction",col = "white",cex=.7)

# first lineage
points(L1$C,L1$ytop,pch=16,col = gray(.3), cex=.8)
points(L1$C,L1$ybottom,pch=16,col = gray(.3), cex=.8)
text(L1$C,L1$ytop,L1$first,col = gray(.3), cex=.7,pos=4)
text(L1$C[-5],L1$ybottom[-5],L1$first[-1], col = gray(.3),cex=.7,pos=4)
for (i in 1:4){
	vt <- L1$ytop[i] - L1$ybottom[i]
	draw.arc(L1$C[i],L1$ybottom[i],L1$C[i],L1$ybottom[i]-vt,col=gray(.3),lwd=.5,lty="83",brel=.00015)
}
for (i in 1:4){
	draw.arc(L1$C[i],L1$ybottom[i],L1$C[i+1],L1$ytop[i+1],col=gray(.3),lwd=.5,lty="83",brel=.00015)
}


# second lineage
points(L2$C,L2$ytop,pch=16,col = "#21114C",cex=.8)
points(L2$C,L2$ybottom,pch=16,col = "#21114C",cex=.8)
text(L2$C,L2$ytop,L2$first,col ="#21114C",cex=.7,pos=4)
text(L2$C,L2$ybottom,L2$first,col ="#21114C",cex=.7,pos=4)
for (i in 1:5){
	vt <- L2$ytop[i] - L2$ybottom[i]
	draw.arc(L2$C[i],L2$ybottom[i],L2$C[i],L2$ybottom[i]-vt,col="#21114C",lwd=.5,lty="74",brel=-.00015)
}
for (i in 1:5){
	draw.arc(L2$C[i],L2$ybottom[i],L2$C[i+1],L2$ytop[i+1],col="#21114C",lwd=.5,lty="74",brel=-.00015)
}


dev.off()






