library(adespatial)
library(SoDA)
library(MuMIn)

# Read in data
# setwd to set working directory to data package folder
dat<-read.csv("MultiCensusPlots.csv",header=T,as.is=T)
all.dat<-read.csv("AllPlots.csv",header=T,as.is=T)

# Convert from biomass to carbon
#Using 0.456 from Martin et al. 2018 Nat Geosciences
dat$AGB<-dat$AGB*0.456
all.dat$AGB<-all.dat$AGB*0.456


# Correct temperature variables for warming trends
dat$bio5<-dat$bio5+(dat$Warm.rate2*(dat$Midpoint-1985))
dat$Min.temp<-dat$Min.temp+(dat$Warm.rate2*(dat$Midpoint-1985))

all.dat$bio5<-all.dat$bio5+(all.dat$Warm.rate2*(all.dat$Midpoint-1985))
all.dat$Min.temp<-all.dat$Min.temp+(all.dat$Warm.rate2*(all.dat$Midpoint-1985))

# Create weights
dat$WtAGB<-dat$Area^(1/3)
all.dat$WtAGB<-all.dat$Area^(1/3)

# Dataframe to store scaled variables
dat.scale<-dat
all.dat.scale<-all.dat

# Scale explanitory variables
dat.scale$Min.temp<-c(scale(dat.scale$Min.temp))
dat.scale$bio5<-c(scale(dat.scale$bio5))
dat.scale$wind<-c(scale(dat.scale$wind))
dat.scale$bio17<-c(scale(dat.scale$bio17))
dat.scale$Cloud<-c(scale(dat.scale$Cloud))
dat.scale$Clay<-c(scale(dat.scale$Clay))
dat.scale$CEC<-c(scale(log(dat.scale$CEC)))


all.dat.scale$Min.temp<-c(scale(all.dat.scale$Min.temp))
all.dat.scale$bio5<-c(scale(all.dat.scale$bio5))
all.dat.scale$wind<-c(scale(all.dat.scale$wind))
all.dat.scale$bio17<-c(scale(all.dat.scale$bio17))
all.dat.scale$Cloud<-c(scale(all.dat.scale$Cloud))
all.dat.scale$Clay<-c(scale(all.dat.scale$Clay))
all.dat.scale$CEC<-c(scale(log(all.dat.scale$CEC)))

# Subset to single-census plots
sc.dat<-all.dat.scale[!all.dat.scale$PlotCode%in%dat.scale$PlotCode,]

# Calculate MEMs for combined and single-census data
xy2<-geoXY(all.dat.scale$Lat,all.dat.scale$Long)
mems<-dbmem(xy2)
all.dat.scale<-cbind(all.dat.scale,mems)

xy2<-geoXY(sc.dat$Lat,sc.dat$Long)
mems<-dbmem(xy2)
sc.dat<-cbind(sc.dat,mems)

# NOTE: MEMs slightly different due to rounded coordinates, but results similar to presented in paper

mf<-lm(log(AGB)~Min.temp+bio5+wind+bio17+Cloud+Continent+CEC+Clay+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtAGB,na.action=na.fail)
dd<-dredge(mf,fixed=c("MEM1","MEM2","MEM3","MEM4","MEM5","MEM6","MEM7","MEM8"))
a<-model.avg(dd,delta<4)

mf<-lm(log(AGB)~Min.temp+bio5+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8+MEM9,data=all.dat.scale,weights=WtAGB,na.action=na.fail)
dd<-dredge(mf,fixed=c("MEM1","MEM2","MEM3","MEM4","MEM5","MEM6","MEM7","MEM8","MEM9"))
a2<-model.avg(dd,delta<4)


mf<-lm(log(AGB)~Min.temp+bio5+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4,data=sc.dat,weights=WtAGB,na.action=na.fail)
dd<-dredge(mf,fixed=c("MEM1","MEM2","MEM3","MEM4"))
a3<-model.avg(dd,delta<4)

# Graph start
cols<-c("red","red","blue","dark gray","dark gray","goldenrod4","goldenrod4")
par(mar=c(4,1,3,1))
par(mfrow=c(1,4))
plot(0,type="n",axes=F,ylab="",xlab="")
vars<-c("Min.temp","bio5","bio17","Cloud","wind","Clay","CEC")
coefs<-summary(a)$coefmat.full[,1]
ses<-summary(a)$coefmat.full[,2]
imp<-importance(a)
cis<-confint(a,full=T)
res<-data.frame("Vars"=vars,"Beta"=coefs[match(vars,names(coefs))],
"SE"=ses[match(vars,names(ses))],
"Importance"=imp[match(vars,names(imp))],
"LCL"=cis[match(vars,rownames(cis)),1],
"UCL"=cis[match(vars,rownames(cis)),2])
var.neat<-c("Minimum temperature","Maximum temperature","Precipitation, driest quarter","Cloud frequency","Mean wind speed","Soil texture","Soil fertility")
yseq<-c(7,6,5,4,3,2,1)
plot(yseq~res$Beta,xlim=c(-0.2,0.2),axes=FALSE,pch=16,cex=1.5,ylab="",xlab="Coefficient",type="n")
lines(c(0,0),c(-10,10),col="gray")
arrows(res$Beta-res$SE,yseq,res$Beta+res$SE,yseq,length=0,lwd=3,col=cols)
arrows(res$LCL,yseq,res$UCL,yseq,length=0,col=cols)
points(yseq~res$Beta,pch=16,col="black")
axis(2,at=yseq,labels=var.neat,las=2)
axis(1)
mtext("Multi-census plots")

coefs<-summary(a2)$coefmat.full[,1]
ses<-summary(a2)$coefmat.full[,2]
imp<-importance(a2)
cis<-confint(a2,full=T)
res<-data.frame("Vars"=vars,"Beta"=coefs[match(vars,names(coefs))],
"SE"=ses[match(vars,names(ses))],
"Importance"=imp[match(vars,names(imp))],
"LCL"=cis[match(vars,rownames(cis)),1],
"UCL"=cis[match(vars,rownames(cis)),2])
yseq<-c(7,6,5,4,3,2,1)
plot(yseq~res$Beta,xlim=c(-0.2,0.2),axes=FALSE,pch=16,cex=1.5,ylab="",xlab="Coefficient",type="n")
lines(c(0,0),c(-10,10),col="gray")
arrows(res$Beta-res$SE,yseq,res$Beta+res$SE,yseq,length=0,lwd=3,col=cols)
arrows(res$LCL,yseq,res$UCL,yseq,length=0,col=cols)
points(yseq~res$Beta,pch=16)
axis(2,at=yseq,labels=NA,las=2)
axis(1)
mtext("All plots")


coefs<-summary(a3)$coefmat.full[,1]
ses<-summary(a3)$coefmat.full[,2]
imp<-importance(a3)
cis<-confint(a3,full=T)
res<-data.frame("Vars"=vars,"Beta"=coefs[match(vars,names(coefs))],
"SE"=ses[match(vars,names(ses))],
"Importance"=imp[match(vars,names(imp))],
"LCL"=cis[match(vars,rownames(cis)),1],
"UCL"=cis[match(vars,rownames(cis)),2])
yseq<-c(7,6,5,4,3,2,1)
plot(yseq~res$Beta,xlim=c(-0.2,0.2),axes=FALSE,pch=16,cex=1.5,ylab="",xlab="Coefficient",type="n")
lines(c(0,0),c(-10,10),col="gray")
arrows(res$Beta-res$SE,yseq,res$Beta+res$SE,yseq,length=0,lwd=3,col=cols)
arrows(res$LCL,yseq,res$UCL,yseq,length=0,col=cols)
points(yseq~res$Beta,pch=16)
axis(2,at=yseq,labels=NA,las=2)
axis(1)
mtext("Single-census plots")
