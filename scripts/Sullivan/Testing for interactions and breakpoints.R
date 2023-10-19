# Interactions and non-linearity

library(segmented)

# Read data
dat<-read.csv("MultiCensusPlots.csv",header=T,as.is=T)

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

# Dataframe to store scaled variables
dat.scale<-dat

# Scale explanitory variables
dat.scale$Min.temp<-c(scale(dat.scale$Min.temp))
dat.scale$bio5<-c(scale(dat.scale$bio5))
dat.scale$wind<-c(scale(dat.scale$wind))
dat.scale$bio17<-c(scale(dat.scale$bio17))
dat.scale$Cloud<-c(scale(dat.scale$Cloud))
dat.scale$Clay<-c(scale(dat.scale$Clay))
dat.scale$CEC<-c(scale(log(dat.scale$CEC)))

# Carbon stocks
m1<-lm(log(AGB)~Min.temp+bio5+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtAGB,na.action=na.fail)
m2<-lm(log(AGB)~Min.temp+bio5+wind+bio17+bio5:bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtAGB,na.action=na.fail)
seg2<-segmented(m2,seg.Z=~bio5)

# Interaction vs no interaction
aics<-AIC(m1,m2)
diff(aics$AIC)

# Breakpoint vs no breakpoint
aics<-AIC(m2,seg2)
diff(aics$AIC)

# Repeat with unscaled variables (to see breakpoint)
m2<-lm(log(AGB)~Min.temp+bio5+wind+bio17+bio5:bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat,weights=WtAGB,na.action=na.fail)
seg2<-segmented(m2,seg.Z=~bio5)
seg2$psi # Estimated breakpoint

# Carbon gains
m1<-lm(log(AGWP)~Min.temp+bio5+wind+bio17+Cloud+Continent+Sand+Clay+pH+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtAGWP,na.action=na.fail)
m2<-lm(log(AGWP)~Min.temp+bio5+wind+bio17+bio5:bio17+Cloud+Continent+Sand+Clay+pH+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtAGWP,na.action=na.fail)
seg2<-segmented(m2,seg.Z=~bio5)

# Interaction vs no interaction
aics<-AIC(m1,m2)
diff(aics$AIC)

# Breakpoint vs no breakpoint
aics<-AIC(m2,seg2)
diff(aics$AIC)



# Carbon residence time

m1<-lm(log(BRT)~Min.temp+bio5+wind+bio17+Cloud+Continent+Sand+Clay+pH+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtBRT,na.action=na.fail)
m2<-lm(log(BRT)~Min.temp+bio5+wind+bio17+bio5:bio17+Cloud+Continent+Sand+Clay+pH+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtBRT,na.action=na.fail)
seg2<-segmented(m2,seg.Z=~bio5)

# Interaction vs no interaction
aics<-AIC(m1,m2)
diff(aics$AIC)

# Breakpoint vs no breakpoint
aics<-AIC(m2,seg2)
diff(aics$AIC)



