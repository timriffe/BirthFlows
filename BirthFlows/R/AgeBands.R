# Author: tim
###############################################################################



APC   <- melt(PC, varnames = c("P","C"), value.name = "Births")
APC   <- APC[APC$Births > 0,]
APC$A <- APC$P - APC$C

AP <- acast(APC, A~P, value.var = "Births", fill = 0)
AC <- acast(APC, A~C, value.var = "Births", fill = 0)

A <- as.integer(rownames(AP))


APc <- apply(AP,2,cumsum)
ACc <- apply(AC,2,cumsum)

plot(Cohs, MAB_C)
age_bands <- c("20","25","30","35","40")
matplot(Yrs,t(APc[age_bands,]) + 
		meander_smoothed[colnames(AP)],type='l',col="white",lwd=.2,lty=1,add=TRUE)

lines(Yrs, apply(AP,2,function(Bx){
					y <- cumsum(Bx)
					x <- names2age(Bx)
					MAB <- wmean(w=Bx,x=x)
					splinefun(y~x, method = "monoH.FC")(MAB)
				})+ 
		meander_smoothed[colnames(AP)], col = "white")

matplot(Cohs,t(-ACc[age_bands,]) + 
				meander_smoothed[colnames(AC)],type='l',col="white",lwd=.2,lty=1,add=TRUE)
lines(Cohs, -apply(AC,2,function(Bx){
							y <- cumsum(Bx)
							x <- names2age(Bx)
							MAB <- wmean(w=Bx,x=x)
							splinefun(y~x, method = "monoH.FC")(MAB)
						})+ 
				meander_smoothed[colnames(AC)], col = "white")



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

