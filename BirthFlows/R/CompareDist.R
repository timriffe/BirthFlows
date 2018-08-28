
# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")



offspring <- PC[,"1800"]  # children born to 1800 mothers
grandmas  <- PC["1800", ] # births in 1800 by mother cohorts

yrs       <- as.integer(names(offspring))
cohs      <- as.integer(names(grandmas))

plot(yrs, offspring, xlim = c(1750,1850), type= 'l')
lines(cohs, grandmas)



wmean <- function(x,w){
	sum((x+.5)*w)/sum(w)
}

g2dist <- function(PC,year = 1800){
	yrs       <- as.integer(rownames(PC))
	cohs      <- as.integer(colnames(PC))
	yrc       <- as.character(year)
	offspring <- PC[, yrc]
	grandmas  <- PC[yrc, ]
	wmean(yrs,offspring) - wmean(cohs,grandmas)
}


g2sd <- function(PC,year = 1800){
	yrc       <- as.character(year)
	yrs       <- as.integer(rownames(PC))
	cohs      <- as.integer(colnames(PC))
	offspring <- PC[, yrc]
	grandmas  <- PC[yrc, ]
	yri       <- offspring > 0
	coi       <- grandmas > 0
	# and standard dev needs outer diff?
	yrs       <- yrs[yri]
	cohs      <- cohs[coi]
	offspring <- offspring[yri]
	grandmas  <- grandmas[coi]
	
	og        <- outer(offspring,grandmas,"*")
	diffs     <- outer(yrs,cohs,"-")
	mn        <- sum(diffs * og) / sum(og)
	sqrt(sum(((diffs - mn)^2) * og) / sum(og))
}

# mean distance by reference year.
refyrs    <- 1775:1968
gdists    <- sapply(refyrs,g2dist,PC=PC)

plot(refyrs,gdists)
g2dist(PC,1800)
g2sd(PC,1800)
gsds <- sapply(refyrs,g2sd,PC=PC)


plot(refyrs,gsds)
plot(refyrs,gdists, type = 'l', ylim=c(45,75))
polygon(c(refyrs,rev(refyrs)), 
		c(gdists - gsds,rev(gdists+gsds)),col = "#00000050")


# and quantiles?

g2dist <- function(PC, year = 1800){
	yrc       <- as.character(year)
	yrs       <- as.integer(rownames(PC))
	cohs      <- as.integer(colnames(PC))
	offspring <- PC[, yrc]
	grandmas  <- PC[yrc, ]
	yri       <- offspring > 0
	coi       <- grandmas > 0
	# and standard dev needs outer diff?
	yrs       <- yrs[yri]
	cohs      <- cohs[coi]
	offspring <- offspring[yri]
	grandmas  <- grandmas[coi]
	
	og        <- outer(offspring,grandmas,"*")
	diffs     <- outer(yrs,cohs,"-")
	
	dt        <- data.table(og=c(og),diffs=c(diffs))
	dt        <- dt[,sum(og),by=list(diffs)]
	dt$V1     <- dt$V1 / sum(dt$V1)

	setnames(dt,old="V1",new="p")
	dt        <- as.data.frame(dt)
	dt[order(dt$diffs), ]
}

g2quant <- function(PC, year = 1800, probs = c(.01,.025,.25,.5,.75,.975,.99)){
	dt <- g2dist(PC = PC, year = year)
	dt$pc <- cumsum(dt$p)
	splinefun(dt$diffs~dt$pc,method="monoH.FC")(probs)
}

gQuants           <- sapply(refyrs, g2quant, PC=PC)
rownames(gQuants) <- c(.01,.025,.25,.5,.75,.975,.99)
colnames(gQuants) <- refyrs

gm <- sapply(refyrs,g2dist,PC=PC)
names(gm) <- refyrs

# plot intergenerational maternal distances
pdf("Figures/GMLag.pdf",height=6,width=10)
par(mai=c(1,1,.2,.2))
plot(refyrs, gQuants[4,], type = 'n', ylim = c(38, 85), las = 1,
		axes = FALSE, ann = FALSE)
polygon(c(refyrs, rev(refyrs)), 
		c(gQuants[3, ],rev(gQuants[5, ])),col = "#00000020",border=NA)
polygon(c(refyrs, rev(refyrs)), 
		c(gQuants[2, ],rev(gQuants[6, ])),col = "#00000020",border=NA)
polygon(c(refyrs,rev(refyrs)), 
		c(gQuants[1, ],rev(gQuants[7, ])),col = "#00000010",border=NA)
grid(lty=1,col="#FFFFFF80")
polygon(c(refyrs, rev(refyrs)), 
		c(gQuants[3, ],rev(gQuants[5, ])))
polygon(c(refyrs, rev(refyrs)), 
		c(gQuants[2, ],rev(gQuants[6, ])))
polygon(c(refyrs,rev(refyrs)), 
		c(gQuants[1, ],rev(gQuants[7, ])))
lines(refyrs, gm, col = "blue", lwd = 2, lty = "82")
lines(refyrs, gQuants[4, ],col = "red", lwd = 2)
xl <- seq(1800, 1830, by = 5)
text(xl, diag(gQuants[, as.character(xl)]), rownames(gQuants), pos = c(1, 3, 1, 1, 3, 1, 3))
text(1850, gm["1850"], "mean", pos = 3)
axis(1)
axis(2, las = 1)
mtext(side = 2, "grandmaternal lag", line = 3, cex = 1.5)
mtext(side = 1, "reference year", line = 3, cex = 1.5)
dev.off()

gQuants[7,c("1850","1950")] - gQuants[1,c("1850","1950")]
gQuants[6,c("1850","1950")] - gQuants[2,c("1850","1950")]
gQuants[5,c("1850","1950")] - gQuants[3,c("1850","1950")]

gQuants[7,c("1950","1968")] - gQuants[1,c("1950","1968")]
gQuants[6,c("1950","1968")] - gQuants[2,c("1950","1968")]
gQuants[5,c("1950","1968")] - gQuants[3,c("1950","1968")]
# what about standard deviations? hmmm.

# if PC is taken as an adjacency matrix then time is vertices,
# and counts are edge weights. Yes, folks. We have an undirected graph
# because a vertex is linked both forward and backward in time, or a 
# directed graph because renewal only propagates forward.

# what then is the meaning of vertex centrality? It's how quickly 
# you can get to another year via reproduction, i.e. the years

#library(igraph)
#yrs       <- as.integer(rownames(PC))
#cohs      <- as.integer(colnames(PC))
#allrefyrs <- min(cohs):max(yrs)
#N         <- length(allrefyrs)
#PCsquare  <- matrix(0,ncol=N,nrow=N,dimnames=list(allrefyrs,allrefyrs))
#PCsquare[rownames(PC),colnames(PC)] <- PC
#image(allrefyrs,allrefyrs,PCsquare,asp=1)
#ig <- graph.adjacency(PCsquare, mode="undirected", weighted=TRUE)
#E(ig)$width <- E(ig)$weight + min(E(ig)$weight) + 1
#plot(allrefyrs,estimate_betweenness(ig,cutoff=100))
#plot(allrefyrs,closeness(ig, mode="all",weights=E(ig)$weight))
#
#par(mfrow=c(2,1))
#plot(allrefyrs,
#		estimate_closeness(
#				ig, 
#				mode="all",
#				weights=E(ig)$weight,
#				cutoff=0),
#		type='l',
#		xlim=c(1775,1968),
#		log = 'y',las=1)
#
#E(ig)[edge_betweenness(ig)>0]
#Bt <- rowSums(PC)
#assortativity(ig,directed=FALSE,types1="1900")
#centr_clo(ig, mode="all")$centralization
#centr_eigen(ig, directed=FALSE)$centralization
#centr_degree(ig)$centralization
#cohesion(ig)
#coreness(ig)
#ego(ig, order=1, nodes = V(ig), mode = "all",
#		mindist = 0)
#igl <- make_ego_graph(ig, order=1, nodes = V(ig), mode = "all",
#		mindist = 0)
#for (i in 1:length(igl)){
#	plot(igl[[i]])
#	locator(1)
#}

#closenesses <- unlist(lapply(igl,function(X){
#			estimate_closeness(
#					X, 
#					mode="all",
#					weights=E(X)$weight,
#					cutoff=0)[1]
#		}))
#rs <- as.integer(names(closenesses))
#plot(rs,unlist(closenesses),log='y',type='l',xlim=c(1775,1960))
#length(cohs)
# not interesting: basically tracks to changes in min and max
# ages: non standaard in our case
#plot(diff(Bt)/Bt[-length(Bt)],type='l')
#cor(diff(Bt)/Bt[-length(Bt)],estimate_closeness(
#				ig, 
#				mode="all",
#				weights=E(ig)$weight,
#				cutoff=0))
#plot(degree_distribution(ig,cumulative=TRUE))
#########################
#hm <- svd(PC)
#str(hm)
#plot(hm$d,log='y')
#for(i in 1:240){
#	plot(hm$u[,i],type='l')
#	locator(1)
#}
#plot(hm$u[,12],type='l')
#for(i in 1:240){
#	plot(hm$v[,i],type='l')
#	locator(1)
#}
#matplot(hm$u,type = 'l',lty=1,col="#00000005")
#matplot(t(hm$u),type = 'l',lty=1,col="#00000005")
#matplot(hm$v,type = 'l',lty=1,col="#00000005")
#matplot(t(hm$v),type = 'l',lty=1,col="#00000005")
#
#hm$d %*% hm$u  %*% hm$v
#D <- diag(hm$d)
#hm$u %*% D %*% t(hm$v) 
#
#plot(hm$d)
#######################