# Load required packages
library(MuMIn)

# Read in data
# setwd to set working directory to data package folder
dat<-read.csv("/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/MultiCensusPlots.csv",header=T,as.is=T)

# Concert from biomass to carbon
#Using 0.456 from Martin et al. 2018 Nat Geosciences
dat$AGB<-dat$AGB*0.456
dat$AGWP<-dat$AGWP*0.456
#Calculate carbon residence time
dat$BRT<-dat$AGB/dat$AGWP

# Correct temperature variables for warming trends
dat$bio5<-dat$bio5+(dat$Warm.rate2*(dat$Midpoint-1985))
dat$Min.temp<-dat$Min.temp+(dat$Warm.rate2*(dat$Midpoint-1985))

# Log transform CEC
dat$CEC<-log(dat$CEC)

# Scale explanitory variables
dat$Min.temp<-scale(dat$Min.temp)
dat$bio5<-scale(dat$bio5)
dat$wind<-scale(dat$wind)
dat$bio17<-scale(dat$bio17)
dat$Cloud<-scale(dat$Cloud)
dat$Clay<-scale(dat$Clay)
dat$CEC<-scale(dat$CEC)

# Create weights
dat$WtAGB<-dat$Area^(1/3)
dat$WtAGWP<-dat$MonitorLength^(1/7)
dat$WtBRT<-dat$Area^(1/9)+dat$MonitorLength^(1/12)-1

# Fit linear models for each explanitory variable
# Then use all-subsets regression and model averaging
mf<-lm(log(AGB)~Min.temp+bio5+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat,weights=WtAGB,na.action=na.fail)
dd<-dredge(mf,fixed=c("MEM1","MEM2","MEM3","MEM4","MEM5","MEM6","MEM7","MEM8"))
a<-model.avg(dd,delta<4)

mf<-lm(log(AGWP)~Min.temp+bio5+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
       data=dat,
       weights=WtAGWP,
       na.action=na.fail)


mf<-lm(log(AGWP)~bio5+bio17+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
       data=dat,
       weights=WtAGWP,
       na.action=na.fail)

dd<-dredge(mf,fixed=c("MEM1","MEM2","MEM3","MEM4","MEM5","MEM6","MEM7","MEM8"))
a2<-model.avg(dd,delta<4)

mf<-lm(log(BRT)~Min.temp+bio5+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat,weights=WtBRT,na.action=na.fail)
dd<-dredge(mf,fixed=c("MEM1","MEM2","MEM3","MEM4","MEM5","MEM6","MEM7","MEM8"))
a3<-model.avg(dd,delta<4)


# Start of figure
pdf("/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/Fig2.pdf",width=7.25,height=3)
layout(t(matrix(c(0,0,0,0,1,2,3,4),ncol=2)),height=c(0.01,1),width=c(0.3,1,1,1))
par(mar=c(3,3.5,3,1))
par(mgp=c(2,0.7,0))
par(lend="square")
par(ljoin="mitre")
cols<-c("red","red","blue","dark gray","dark gray","goldenrod4","goldenrod4")
plot(0,type="n",axes=F,ylab="",xlab="")
vars<-c("Min.temp","bio5","bio17","Cloud","wind","Clay","CEC")

# Panel 1 - carbon stocks
coefs<-summary(a)$coefmat.full[,1]
ses<-summary(a)$coefmat.full[,2]
imp<-importance(a)
cis<-confint(a,full=T)
res<-data.frame("Vars"=vars,"Beta"=coefs[match(vars,names(coefs))],
"SE"=ses[match(vars,names(ses))],
"Importance"=imp[match(vars,names(imp))],
"LCL"=cis[match(vars,rownames(cis)),1],
"UCL"=cis[match(vars,rownames(cis)),2])
var.neat<-c("Minimum    \n temperature","Maximum   \n temperature","Precipitation,\n driest quarter","Cloud cover","Wind speed","Soil texture","Soil fertility")
yseq<-c(7,6,5,4,3,2,1)
plot(yseq~res$Beta,xlim=c(-0.2,0.2),axes=FALSE,pch=16,cex=1.5,ylab="",xlab="Coefficient",type="n")
lines(c(0,0),c(-10,10),col="gray")
arrows(res$Beta-res$SE,yseq,res$Beta+res$SE,yseq,length=0,lwd=3,col=cols)
arrows(res$LCL,yseq,res$UCL,yseq,length=0,col=cols)
points(yseq~res$Beta,xlim=c(-0.2,0.2),pch=16,cex=1.5,ylab="",xlab="Coefficient")
axis(2,at=yseq,labels=var.neat,las=2,cex.axis=1.2)
axis(1)
mtext("Carbon stocks",line=2)
arrows(-0.1,7.5,0.1,7.5,xpd=TRUE,length=0.1)
text(0.15,7.5,"Greater",xpd=TRUE)
box(bty="l")

# Panel 2 - carbon gains
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
plot(yseq~res$Beta,xlim=c(-0.2,0.2),axes=FALSE,pch=16,cex=1.5,ylab="",xlab="Coefficient",col=cols,type="n")
lines(c(0,0),c(-10,10),col="gray")
arrows(res$Beta-res$SE,yseq,res$Beta+res$SE,yseq,length=0,lwd=3,col=cols)
arrows(res$LCL,yseq,res$UCL,yseq,length=0,col=cols)
points(yseq~res$Beta,xlim=c(-0.2,0.2),pch=16,cex=1.5,ylab="",xlab="Coefficient")
axis(1)
mtext("Carbon gains",line=2)
arrows(-0.1,7.5,0.1,7.5,xpd=TRUE,length=0.1)
text(0.15,7.5,"Higher",xpd=TRUE)
box(bty="l")

# Panel 3 - carbon residence time
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
plot(yseq~res$Beta,xlim=c(-0.2,0.2),axes=FALSE,pch=16,cex=1.5,ylab="",xlab="Coefficient",col=cols,type="n")
lines(c(0,0),c(-10,10),col="gray")
arrows(res$Beta-res$SE,yseq,res$Beta+res$SE,yseq,length=0,lwd=3,col=cols)
arrows(res$LCL,yseq,res$UCL,yseq,length=0,col=cols)
points(yseq~res$Beta,xlim=c(-0.2,0.2),pch=16,cex=1.5,ylab="",xlab="Coefficient")
axis(1)
mtext("Carbon residence time",line=2)
arrows(-0.1,7.5,0.1,7.5,xpd=TRUE,length=0.1)
text(0.15,7.5,"Longer",xpd=TRUE)
box(bty="l")
dev.off()
