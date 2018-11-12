
# Author: tim
###############################################################################
setwd("/home/tim/git/BirthFlows/BirthFlows")
# download birthsVV (period-cohort Lexis shape for birth counts) from HFD. 
library(RColorBrewer)
library(HMDHFDplus)
library(ungroup)
library(reshape2)
library(data.table)
library(DemoTools)
source("R/Functions.R")

oldtotals <- read.csv("Data/HistoricalTotals1736to1775.csv")
ASFR      <- local(get(load("Data/ASFR_1751to1775.Rdata")))
SWEh      <- read.csv("Data/SWEbirths.txt", na.strings = ".", stringsAsFactors = FALSE)
SWEh      <- SWEh[SWEh$Age != "TOT" & SWEh$Year < 1891, ]
SWEh$Age  <- as.integer(SWEh$Age)
SWEh      <- SWEh[!is.na(SWEh$Age),]
table(SWEh$Age,SWEh$Year)
mins      <- tapply(SWEh$Age,SWEh$Year,min)
unlist(lapply(split(SWEh,SWEh$Year),function(X){
			X$AgeInt[which.min(X$Age)]
		}))
maxs      <- tapply(SWEh$Age,SWEh$Year,max)
unlist(lapply(split(SWEh,SWEh$Year),function(X){
					X$AgeInt[which.max(X$Age)]
				}))

HFDm <- matrix(0,nrow=length(12:55),ncol=5)
x2 <- x1 <- col(HFDm)+1890
x3 <- x4 <- x1 + 1

y1 <- y3 <- row(HFDm) + 11
y2 <- y1 - 1
y4 <- y1 + 1

x <- rbind(c(x1),c(x2),c(x3),c(x4),NA)
y <- rbind(c(y1),c(y2),c(y3),c(y4),NA)

SWEh[is.na(SWEh$Age),]
pdf("Figures/HistoricalDimensions.pdf",height=5.5,width=10)
par(mai=c(.7,.7,.5,.4))
plot(NULL, type="n",xlim=c(1736, 1895), ylim = c(0,60), asp = 1, axes = FALSE, ann = FALSE)
rect(1736,14,1751,51,border=NA,col = "#fff89b")
rect(1751,14,1775,51,border=NA,col = "#c0ffbf")

rect(1775,20,1891,50,border=NA,col = "#5ab4f4")
rect(1775,20,1861,15,border=NA,col = "#b3defc")
rect(1775,50,1891,55,border=NA,col = "#b3defc")
rect(1861,14,1891,20,border=NA,col = "#1d9ff9")
rect(1736,0,1775,1,border=NA,col="#ff7e6d")

polygon(c(x),c(y),lwd=.5,col=gray(.6))
text(1895,9,"HFD",cex=1.2)

# green AP rates (HFC)
text(1762,51,"ASFR (HFC)",cex=1.2,pos=3)
segments(1751,14:51,1775,14:51,lwd=.5)
segments(seq(1751,1771,5),14,seq(1751,1771,5),51,lwd=.5)
segments(1775,14,1775,51,lwd=.5)

# blue AP SGF (1775-1890)
segments(1775,15,1861,15,lwd=.5)
segments(1861,14:19,1891,14:19,lwd=.5)
segments(1775,seq(20,55,5),1891,seq(20,55,5),lwd=.5)
segments(1775:1860,15,1775:1860,55,lwd=.5)
segments(1861:1891,14,1861:1891,55,lwd=.5)
text(1790,55,"Counts (SGF)",pos=3,cex=1.2)
text(1835,59,"Open age 50+",cex=1.2,pos=4)
text(1835,10,"Open age <20",cex=1.2,pos=4)
# pointers
segments(1835,59,1831,52.5)
segments(1835,10,1831,17.5)
# yellow reconstruction (1736-1750)
segments(1736,14,1736,51,lwd=.5)
segments(1736,c(14,51),1751,c(14,51),lwd=.5)
text(1736,25,"Reconstruction",srt=90,pos=4,cex=1.2)

# red totals (1736-1774)
rect(1736:1774,0,1737:1775,1)
text(1745,1,"Totals",pos=3,cex=1.2)

# HFD arrows
polygon(c(1896,1898,1898,1900,1898,1898,1896)+1,c(30,30,28,32,36,34,34))
polygon(c(1896,1898,1898,1900,1898,1898,1896)+1,c(30,30,28,32,36,34,34)-10)
polygon(c(1896,1898,1898,1900,1898,1898,1896)+1,c(30,30,28,32,36,34,34)+10)
polygon(c(1896,1898,1898,1900,1898,1898,1896)+1,c(30,30,28,32,36,34,34)+20)

arrows(1736,70,1898,70,length=.1,xpd=TRUE)
segments(c(1736,1751,1775,1891),69,c(1736,1751,1775,1891),71,xpd=TRUE)
text(c(1736,1751,1775,1891),71,c(1736,1751,1775,1891),pos=3,xpd=TRUE)

axis(2,las=1)
text(1722,65,"Age",xpd=TRUE,cex=1.2)

segments(1735,0,1900,0,xpd=TRUE)
segments(seq(1735,1900,5),0,seq(1735,1900,5),-1.5,xpd=TRUE)
text(seq(1740,1900,20),-2,seq(1740,1900,20),pos=1,xpd=TRUE)
text(1820,-10,"Year",cex=1.2,xpd=TRUE)
dev.off()