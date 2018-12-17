# Author: tim
###############################################################################

setwd("/home/tim/git/BirthFlows/BirthFlows")
source("R/DataPrep.R")
# Structural echoes
# reference years
ryrs <- 1775:1968
cind <- as.character(ryrs)
N    <- length(ryrs)
Bt   <- rowSums(PC)[cind]
Bc   <- colSums(PC)[cind]

# reference years

rBt <- diff(Bt) / Bt[-N]
rBc <- diff(Bc) / Bc[-N]

yind <- ryrs < 1891

rBc1 <- rBc[yind]
rBc2 <- rBc[!yind]
rBt1 <- rBt[yind]
rBt2 <- rBt[!yind]
lm1 <- lm(rBc1~rBt1)
lm2 <- lm(rBc2~rBt2)
x <- seq(-.2,.3,length=200)
newdata=data.frame('rBt1'=x )
p1 <- predict(lm1,newdata=newdata,level=.95,se.fit=TRUE)
newdata=data.frame('rBt2'=x )
p2 <- predict(lm2,newdata=newdata,level=.95,se.fit=TRUE)
pdf("Figures/rBcrBt.pdf")
plot(rBt,rBc,col=ifelse(yind,"magenta","blue"),	asp=1,
		main = "Fractional change in offspring versus cohort size",
		las=1)
abline(v=0,col="#AAAAAA80")
abline(h=0,col="#AAAAAA80")
abline(a=0,b=1)
polygon(c(x,rev(x)),c(p1$fit - 2*p1$se.fit,rev(p1$fit + 2*p1$se.fit)),col="#FF00FF20",border=NA)
polygon(c(x,rev(x)),c(p2$fit - 2*p2$se.fit,rev(p2$fit + 2*p2$se.fit)),col="#0000FF20",border=NA)
abline(lm1,col="magenta")
abline(lm2,col="blue")
text(.1,c(-.04,-.06),c("1775-1890",paste0("1891-", rightCoh(SWE,45))),col=c("magenta","blue"))
text(.15,.15,"unity",srt=45,pos=3)

dev.off()



rBt["1920"]
rBc["1920"]

findfrequency(as.ts(Bc/Bt))
auto.arima(as.ts(Bc/Bt))
plot(auto.arima(as.ts(rBt),max.order=40,max.D=40,max.d=40,seasonal=TRUE))
?auto.arima

s_beer = ts(timeserie_beer, frequency = 4)
stl_beer = stl(ts_beer, "periodic")
seasonal_stl_beer   <- stl_beer$time.series[,1]
trend_stl_beer     <- stl_beer$time.series[,2]
random_stl_beer  <- stl_beer$time.series[,3]
par(mfrow=c())
for (i in 20:40){
detrend_Bt = timeseries_Bt - ma(timeseries_Bt, order = i, centre = T)
plot(as.ts(detrend_Bt,start=1775),main=i)
locator(1)
}
# Give rates and exposures these shocks are decomposable, also into mort, mig, etc.


# compare time series of SD
compare <- intersect(names(Coh_SD),names(Per_SD))
plot(Per_SD[compare],Coh_SD[compare], type = 'o',asp=1)
points(Per_SD[compare],Coh_SD[compare])

plot(1736:2016,Per_SD[compare]/Coh_SD[compare], type = 'l')
