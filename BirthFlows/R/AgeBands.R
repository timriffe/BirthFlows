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