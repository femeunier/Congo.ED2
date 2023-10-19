library(randomForest)

# Read data
dat<-read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Sullivan/Data package/MultiCensusPlots.csv",header=T,as.is=T)


# Convert from biomass to carbon
#Using 0.456 from Martin et al. 2018 Nat Geosciences
dat$AGB<-dat$AGB*0.456
dat$AGWP<-dat$AGWP*0.456
#Calculate carbon residence time
dat$BRT<-dat$AGB/dat$AGWP

# Correct temperature variables for warming trends
dat$bio5<-dat$bio5+(dat$Warm.rate2*(dat$Midpoint-1985))
dat$Min.temp<-dat$Min.temp+(dat$Warm.rate2*(dat$Midpoint-1985))

# Create weights
dat$WtAGB<-dat$Area^(1/3)
dat$WtAGWP<-dat$MonitorLength^(1/7)
dat$WtBRT<-dat$Area^(1/9)+dat$MonitorLength^(1/12)-1

# Log transform CEC
dat$CEC<-log(dat$CEC)

# Random forest on continuous variables
rf<-randomForest(log(AGB)~Min.temp+bio5+wind+bio17+Cloud+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
                 data=dat,
                 # weights=WtAGB,
                 na.action=na.fail,
                 importance = TRUE)

rf<-randomForest(log(AGB)~bio5+bio17+MEM1+MEM2,
                 data=dat,
                 # weights=WtAGB,
                 na.action=na.fail,
                 importance = TRUE)

as.data.frame(importance(rf, type = 1)) %>%
  tibble::rownames_to_column() %>%
  arrange(desc(`%IncMSE`)) # bio17 (precip in driest quarter) and bio5 (maxT in warmest month) most important (excluding MEMs)


a<-partialPlot(rf,dat,bio5)
a2<-partialPlot(rf,dat,bio17)

par(cex=1)
par(mar=c(5,4,1,1))
par(mfrow=c(1,2))
par(mgp=c(2,0.5,0))
plot(a$y~a$x,type="l",main="",xlab="",ylab=expression(paste("ln (Carbon) (Mg ha"^-1,")")),lwd=2)
mtext(side=1,line=3.5,expression(atop("Mean daily maximum temperature",paste("in the warmest month ("^o,"C)"))))
plot(a2$y~a2$x,type="l",main="",xlab="Precipitation, driest quarter (mm)",ylab=expression(paste("ln (Carbon) (Mg ha"^-1,")")),lwd=2)
