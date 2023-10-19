#Load required R packages
library(rworldmap)
library(RColorBrewer)
library(raster)
library(multcomp)

# Read in data
# setwd to set working directory to data package folder
#Multi-census
dat<-read.csv("/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/MultiCensusPlots.csv",header=T,as.is=T)
# Plus single census
all.dat<-read.csv("/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/AllPlots.csv",header=T,as.is=T)
sc.dat<-all.dat[all.dat$PlotCode%in%setdiff(all.dat$PlotCode,dat$PlotCode),]

# User option - restrict.vars.
# Choose set of environmental variables to account for when looking at differences in carbon stocks, gains and residence time among continents
# Set as FALSE to replicate Fig. 1, using a wide set of climate and soil variables to capture environmental variation
# Set as TRUE to use the subset of variables we hypothsise to have mechanistic controls on spatial variation in carbon (see Table S1).
# Results are similar regardless of option picked (e.g. main differences between continents the same)
# Values for Australia most sensitive to environmental explanitory variable choice
restrict.vars<-FALSE

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

# Scale explanitory variables
dat$Min.temp<-c(scale(dat$Min.temp))
dat$bio5<-c(scale(dat$bio5))
dat$wind<-c(scale(dat$wind))
dat$bio17<-c(scale(dat$bio17))
dat$Cloud<-c(scale(dat$Cloud))
dat$Clay<-c(scale(dat$Clay))
dat$CEC<-c(scale(dat$CEC))
dat$Sand<-c(scale(dat$Sand))
dat$pH<-c(scale(dat$pH))
dat$Srad<-c(scale(dat$Srad))


# Get world map
world<-getMap(resolution="less islands")
# Crop to tropics - don't worry about warning message about different proj4 strings
trop<-crop(world,extent(-86,150,-23,23))


#Figure 1 start
pdf("/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/Fig1.pdf",width=7.25,height=4.5)
layout(t(matrix(c(1,1,1,2,3,4),ncol=2)),height=c(1,2))

# Set colour palette
cols<-brewer.pal(4,"Dark2")

# Plot map
cex.factor<-1
par(mar=c(0,3,0.5,0))
plot(trop,lwd=0.5,border="dark gray")
with(dat[dat$Continent=="SA",],points(Long,Lat,pch=16,col=cols[1],cex=cex.factor))
with(sc.dat[sc.dat$Continent=="SA",],points(Long,Lat,pch=1,col=cols[1],cex=cex.factor))
with(dat[dat$Continent=="AF",],points(Long,Lat,pch=16,col=cols[2],cex=cex.factor))
with(sc.dat[sc.dat$Continent=="AF",],points(Long,Lat,pch=1,col=cols[2],cex=cex.factor))
with(dat[dat$Continent=="AS",],points(Long,Lat,pch=16,col=cols[3],cex=cex.factor))
with(sc.dat[sc.dat$Continent=="AS",],points(Long,Lat,pch=1,col=cols[3],cex=cex.factor))
with(dat[dat$Continent=="AU",],points(Long,Lat,pch=16,col=cols[4],cex=cex.factor))

lines(c(-86,150),c(-23,-23))
lines(c(-86,150),c(23,23))
lines(c(-86,-86),c(-23,23))
lines(c(150,150),c(-23,23))
mtext("A",at=-103,line=-1)


# Silly approach of reordering factors so South America first!
dat$Cont2<-as.factor(sub("SA","AASA",dat$Continent))
par(mar=c(5.6,5,3,1))
par(mgp=c(3.2,0.7,0))
par(cex.lab=1.4)
par(cex.axis=1.1)


par(bty="l")
boxplot(dat$AGB~dat$Cont2,names=c("S America","Africa","Asia","Australia"),las=2,col=cols,ylab=expression(paste("Carbon stocks (Mg C ha"^-1,")")))
# Fit model with continent and environmental variables [set choice depending on restrict.vars
if(restrict.vars==TRUE){
	m1<-lm(log(AGB)~0+Cont2+bio5+Min.temp+bio17+wind+Cloud+CEC+Clay,data=dat,weights=WtAGB)
}else{
	m1<-lm(log(AGB)~0+Cont2+bio5+Min.temp+bio17+Srad+wind+Cloud+CEC+Sand+Clay+pH,data=dat,weights=WtAGB)
}
# Extract continent coefficients
ests<-summary(m1)$coefficients[1:4,1:2]
points(exp(ests[,1]),pch=16,col="blue",cex=1.2)
arrows(seq(1,4),exp(ests[,1]+ests[,2]),seq(1,4),exp(ests[,1]-ests[,2]),length=0,col="blue",lwd=2.5)
# Post-hoc test for differences between continents
let1<-cld(glht(m1,linfct=mcp(Cont2="Tukey")))
# Repeat but without controlling for environmental variables
m2<-lm(log(AGB)~Cont2,data=dat)
let2<-cld(glht(m2,linfct=mcp(Cont2="Tukey")))
# Add post-hoc test results to graph
mtext(let2$mcletters$Letters,at=c(1,2,3,4),line=-0.3,cex=0.8)
mtext(paste("[",let1$mcletters$Letters,"]",sep=""),at=c(1,2,3,4)+0.4,line=-0.3,cex=0.8,col="blue")
mtext("B",at=-1,line=2)


boxplot(dat$AGWP~dat$Cont2,names=c("S America","Africa","Asia","Australia"),las=2,col=cols,ylab=expression(paste("Carbon gains (Mg C ha"^-1," yr"^-1,")")),xlab="")
if(restrict.vars==TRUE){
	m1<-lm(log(AGWP)~0+Cont2+bio5+Min.temp+bio17+wind+Cloud+CEC+Clay,data=dat,weights=WtAGWP)
}else{
	m1<-lm(log(AGWP)~0+Cont2+bio5+Min.temp+bio17+Srad+wind+Cloud+CEC+Sand+Clay+pH,data=dat,weights=WtAGWP)
}
ests<-summary(m1)$coefficients[1:4,1:2]
points(exp(ests[,1]),pch=16,col="blue",cex=1.2)
arrows(seq(1,4),exp(ests[,1]+ests[,2]),seq(1,4),exp(ests[,1]-ests[,2]),length=0,col="blue",lwd=2.5)
let1<-cld(glht(m1,linfct=mcp(Cont2="Tukey")))
m2<-lm(log(AGWP)~Cont2,data=dat)
let2<-cld(glht(m2,linfct=mcp(Cont2="Tukey")))
mtext(let2$mcletters$Letters,at=c(1,2,3,4),line=-0.3,cex=0.8)
mtext(paste("[",let1$mcletters$Letters,"]",sep=""),at=c(1,2,3,4)+0.4,line=-0.3,cex=0.8,col="blue")

boxplot(dat$BRT~dat$Cont2,names=c("S America","Africa","Asia","Australia"),las=2,col=cols,ylab="Carbon residence time (years)")
if(restrict.vars==TRUE){
	m1<-lm(log(BRT)~0+Cont2+bio5+Min.temp+bio17+wind+Cloud+CEC+Clay,data=dat,weights=WtBRT)
}else{
	m1<-lm(log(BRT)~0+Cont2+bio5+Min.temp+bio17+Srad+wind+Cloud+CEC+Sand+Clay+pH,data=dat,weights=WtBRT)
}
ests<-summary(m1)$coefficients[1:4,1:2]
points(exp(ests[,1]),pch=16,col="blue",cex=1.2)
arrows(seq(1,4),exp(ests[,1]+ests[,2]),seq(1,4),exp(ests[,1]-ests[,2]),length=0,col="blue",lwd=2.5)
let1<-cld(glht(m1,linfct=mcp(Cont2="Tukey")))
m2<-lm(log(BRT)~Cont2,data=dat)
let2<-cld(glht(m2,linfct=mcp(Cont2="Tukey")))
mtext(let2$mcletters$Letters,at=c(1,2,3,4),line=-0.3,cex=0.8)
mtext(paste("[",let1$mcletters$Letters,"]",sep=""),at=c(1,2,3,4)+0.4,line=-0.3,cex=0.8,col="blue")
dev.off()
