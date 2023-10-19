rm(list = ls())

# Load required packages
library(segmented)
library(RColorBrewer)

# Read in data
# setwd to set working directory to data package folder
dat<-read.csv("/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/MultiCensusPlots.csv",header=T,as.is=T)

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


# Dataframe to store unscaled variables
dat2<-dat

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

# Fit linear models
m2<-lm(log(AGB)~Min.temp+bio5+bio5:bio17+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
       data=dat.scale,weights=WtAGB,na.action=na.fail)
# Breakpoint regression model for carbon stocks
seg<-segmented(m2,seg.Z=~bio5,psi=mean(dat.scale$bio5))
m3<-lm(log(AGWP)~Min.temp+bio5+bio5:bio17+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
       data=dat.scale,weights=WtAGWP,na.action=na.fail)
summary(m3)
m3bis<-lm(log(AGWP)~Min.temp+bio5+bio5:bio17+bio17 + MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
       data=dat.scale,weights=WtAGWP,na.action=na.fail)
summary(m3bis)

m4<-lm(log(BRT)~Min.temp+bio5+bio5:bio17+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,data=dat.scale,weights=WtBRT,na.action=na.fail)

# Make new dataframes to predict values for
cont<-"SA"
pred.dat<-data.frame(
"Min.temp"=0,
"bio5"=seq(min(dat.scale$bio5),max(dat.scale$bio5),length.out=100),
"wind"=0,
"bio17"=0,
"Cloud"=0,
"Srad"=0,
"Continent"=factor("SA",levels=c("SA","AF","AS","AU")),
"MEM1"=mean(dat2$MEM1[dat2$Continent==cont]),
"MEM2"=mean(dat2$MEM2[dat2$Continent==cont]),
"MEM3"=mean(dat2$MEM3[dat2$Continent==cont]),
"MEM4"=mean(dat2$MEM4[dat2$Continent==cont]),
"MEM5"=mean(dat2$MEM5[dat2$Continent==cont]),
"MEM6"=mean(dat2$MEM6[dat2$Continent==cont]),
"MEM7"=mean(dat2$MEM7[dat2$Continent==cont]),
"MEM8"=mean(dat2$MEM8[dat2$Continent==cont]),
"Sand"=0,
"Clay"=0,
"pH"=0,
"CEC"=0)

pred.dat.SA <- pred.dat

# Predict values
preds.sa<-predict(seg,newdata=pred.dat,se.fit=T)
preds2.sa<-predict(m2,newdata=pred.dat,se.fit=T)
preds.agwp.sa<-predict(m3,newdata=pred.dat,se.fit=T)
preds.brt2.sa<-predict(m4,newdata=pred.dat,se.fit=T)


#Repeat for each continent
cont<-"AF"
pred.dat<-data.frame(
"Min.temp"=0,
"bio5"=seq(min(dat.scale$bio5),max(dat.scale$bio5),length.out=100),
"wind"=0,
"bio17"=0,
"Cloud"=0,
"Srad"=0,
"Continent"=factor("AF",levels=c("SA","AF","AS","AU")),
"MEM1"=mean(dat2$MEM1[dat2$Continent==cont]),
"MEM2"=mean(dat2$MEM2[dat2$Continent==cont]),
"MEM3"=mean(dat2$MEM3[dat2$Continent==cont]),
"MEM4"=mean(dat2$MEM4[dat2$Continent==cont]),
"MEM5"=mean(dat2$MEM5[dat2$Continent==cont]),
"MEM6"=mean(dat2$MEM6[dat2$Continent==cont]),
"MEM7"=mean(dat2$MEM7[dat2$Continent==cont]),
"MEM8"=mean(dat2$MEM8[dat2$Continent==cont]),
"Sand"=0,
"Clay"=0,
"pH"=0,
"CEC"=0)
preds.af<-predict(seg,newdata=pred.dat,se.fit=T)
preds2.af<-predict(m2,newdata=pred.dat,se.fit=T)
preds.agwp.af<-predict(m3,newdata=pred.dat,se.fit=T)
preds.brt2.af<-predict(m4,newdata=pred.dat,se.fit=T)

pred.dat.AF <- pred.dat

cont<-"AS"
pred.dat<-data.frame(
"Min.temp"=0,
"bio5"=seq(min(dat.scale$bio5),max(dat.scale$bio5),length.out=100),
"wind"=0,
"bio17"=0,
"Cloud"=0,
"Srad"=0,
"Continent"=factor("AS",levels=c("SA","AF","AS","AU")),
"MEM1"=mean(dat2$MEM1[dat2$Continent==cont]),
"MEM2"=mean(dat2$MEM2[dat2$Continent==cont]),
"MEM3"=mean(dat2$MEM3[dat2$Continent==cont]),
"MEM4"=mean(dat2$MEM4[dat2$Continent==cont]),
"MEM5"=mean(dat2$MEM5[dat2$Continent==cont]),
"MEM6"=mean(dat2$MEM6[dat2$Continent==cont]),
"MEM7"=mean(dat2$MEM7[dat2$Continent==cont]),
"MEM8"=mean(dat2$MEM8[dat2$Continent==cont]),
"Sand"=0,
"Clay"=0,
"pH"=0,
"CEC"=0)
preds.as<-predict(seg,newdata=pred.dat,se.fit=T)
preds2.as<-predict(m2,newdata=pred.dat,se.fit=T)
preds.agwp.as<-predict(m3,newdata=pred.dat,se.fit=T)
preds.brt2.as<-predict(m4,newdata=pred.dat,se.fit=T)

pred.dat.AS <- pred.dat


cont<-"AU"
pred.dat<-data.frame(
"Min.temp"=0,
"bio5"=seq(min(dat.scale$bio5),max(dat.scale$bio5),length.out=100),
"wind"=0,
"bio17"=0,
"Cloud"=0,
"Srad"=0,
"Continent"=factor("AU",levels=c("SA","AF","AS","AU")),
"MEM1"=mean(dat2$MEM1[dat2$Continent==cont]),
"MEM2"=mean(dat2$MEM2[dat2$Continent==cont]),
"MEM3"=mean(dat2$MEM3[dat2$Continent==cont]),
"MEM4"=mean(dat2$MEM4[dat2$Continent==cont]),
"MEM5"=mean(dat2$MEM5[dat2$Continent==cont]),
"MEM6"=mean(dat2$MEM6[dat2$Continent==cont]),
"MEM7"=mean(dat2$MEM7[dat2$Continent==cont]),
"MEM8"=mean(dat2$MEM8[dat2$Continent==cont]),
"Sand"=0,
"Clay"=0,
"pH"=0,
"CEC"=0)
preds.au<-predict(seg,newdata=pred.dat,se.fit=T)
preds2.au<-predict(m2,newdata=pred.dat,se.fit=T)
preds.agwp.au<-predict(m3,newdata=pred.dat,se.fit=T)
preds.brt2.au<-predict(m4,newdata=pred.dat,se.fit=T)

pred.dat.AU <- pred.dat


# Collate predictions across continents
pred.mat<-cbind(preds.sa$fit,preds.af$fit,preds.as$fit,preds.au$fit)
pred2.mat<-cbind(preds2.sa$fit,preds2.af$fit,preds2.as$fit,preds2.au$fit)
pred.agwp.mat<-cbind(preds.agwp.sa$fit,preds.agwp.af$fit,preds.agwp.as$fit,preds.agwp.au$fit)
pred.brt2.mat<-cbind(preds.brt2.sa$fit,preds.brt2.af$fit,preds.brt2.as$fit,preds.brt2.au$fit)

# Make weighted mean predictions for pan-tropical fit based on number of plots in each continent
wts<-c(0.46192893,0.40439932,0.10490694, 0.02876481)
preds<-apply(pred.mat,1,function(x)weighted.mean(x,wts))
preds2<-apply(pred2.mat,1,function(x)weighted.mean(x,wts))
preds.agwp<-apply(pred.agwp.mat,1,function(x)weighted.mean(x,wts))
preds.brt2<-apply(pred.brt2.mat,1,function(x)weighted.mean(x,wts))

# Make bio5 variable on original temperature scale (not scaled)
pred.dat$bio5.2<-(seq(min(dat.scale$bio5),max(dat.scale$bio5),length.out=100)*sd(dat2$bio5))+mean(dat2$bio5)


#######
#Functions for plotting
cols<-brewer.pal(4,"Dark2")
darken <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col/factor
    col <- rgb(t(col), maxColorValue=255)
    col
}
lighten <- function(color, factor=1.4){
    col <- col2rgb(color)
    col <- col*factor
    col <- rgb(t(col), maxColorValue=255)
    col
}
makeTransparent = function(..., alpha=0.5) {

  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")

  alpha = floor(255*alpha)
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)

  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }

  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)

  return(newColor)

}

make.line<-function(var,data,weight,color,cont=FALSE,resp="AGB",dash=1){
if(cont==FALSE){
mf<-lm(as.formula(paste("log(",resp,")~",var)),data=data,weights=data$WtAGB)
}else{
mf<-lm(as.formula(paste("log(",resp,")~",var,"+ Continent")),data=data,weights=data$WtAGB)
}
sig<-summary(mf)$coefficients[2,4]
pred.dat<-data.frame(var=seq(min(data[,var]),max(data[,var]),length.out=100),"Continent"=factor("AF",levels=c("AF","SA")))
names(pred.dat)<-c(var,"Continent")
preds<-predict(mf,newdata=pred.dat,se.fit=T)
#if(sig==1){
if(cont==TRUE){
points(preds$fit~pred.dat[,var],type="l",lwd=weight,col=color,lty=dash)
}else{
points(preds$fit~pred.dat[,var],type="l",lwd=weight,col=color,lty=dash)
}
sig2<-ifelse(sig>0.05,"ns",ifelse(sig>0.01,"*",ifelse(sig>0.001,"**","***")))
sig2
}

#####################################
#Plot start
cols2<-makeTransparent(cols,0.5)

pdf("/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/Fig3.pdf",width=10.87,height=3.7)

par(mfrow=c(1,3))
par(mar=c(4.7,3.3,1,1))
par(cex.lab=1.1)
par(mgp=c(3,0.5,0))
plot(log(AGB)~bio5,data=dat2[dat2$Continent=="SA",],type="n",ylim=c(3.8,6.2),xlim=c(25.8,35.5),pch=16,col=cols2[1],ylab="",xlab="",cex=WtAGB,yaxt="n")
mtext(side=1,line=3.9,expression(atop("Mean daily maximum temperature",paste("in the warmest month ("^o,"C)"))))

mtext(expression(paste("Carbon stocks (Mg C ha"^-1,")")),side=2,line=1.5,cex=1)
axis(2,at=c(4,4.5,5,5.5,6),labels=round(exp(c(4,4.5,5,5.5,6)),0))
points(log(AGB)~bio5,data=dat2[dat2$Continent=="AS",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[3],ylab=expression(paste("ln (Carbon) / Mg ha"^-1)),xlab=expression(paste("Mean daily maximum temperature / "^o, "C")),cex=WtAGB)
points(log(AGB)~bio5,data=dat2[dat2$Continent=="AU",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[4],cex=WtAGB)
points(log(AGB)~bio5,data=dat2[dat2$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB)
points(log(AGB)~bio5,data=dat2[dat2$Continent=="AF",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[2],ylab=expression(paste("ln (Carbon) / Mg ha"^-1)),xlab=expression(paste("Mean daily maximum temperature / "^o, "C")),cex=WtAGB)



a2<-make.line("bio5",dat2[dat2$Continent=="AU",],weight=3,col=darken(cols[4],1.1))
a3<-make.line("bio5",dat2[dat2$Continent=="SA",],weight=3,col=darken(cols[1],1.1))
a4<-make.line("bio5",dat2[dat2$Continent=="AF",],weight=3,col=darken(cols[2],1.1))
a5<-make.line("bio5",dat2[dat2$Continent=="AS",],weight=3,col=darken(cols[3],1.1),dash=2)
points(preds2~pred.dat$bio5.2,type="l",lwd=3,col="gray")
points(preds~pred.dat$bio5.2,type="l",lwd=3)

legend("bottomleft",lwd=1,pch=16,col=cols,legend=c(paste("S America",a3),paste("Africa",a4),paste("Asia",a5),paste("Australia",a2)),bty="n",cex=0.9)


#AGWP-bio5
plot(log(AGWP)~bio5,data=dat2,type="n",pch=16,col=cols2[1],ylab="",xlab="",cex=WtAGB,yaxt="n",xlim=c(25.8,35.5))
mtext(expression(paste("Carbon gains (Mg C ha"^-1," yr"^-1,")")),side=2,line=1.5,cex=1)
axis(2,at=c(0,0.36,0.72,1.08,1.44,1.8),labels=round(exp(c(0,0.36,0.72,1.08,1.44,1.8)),1))
mtext(side=1,line=3.9,expression(atop("Mean daily maximum temperature",paste("in the warmest month ("^o,"C)"))))
points(log(AGWP)~bio5,data=dat2[dat2$Continent=="AS",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[3],ylab=expression(paste("ln (Carbon) / Mg ha"^-1)),xlab=expression(paste("Mean daily maximum temperature / "^o, "C")),cex=WtAGWP)
points(log(AGWP)~bio5,data=dat2[dat2$Continent=="AU",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[4],cex=WtAGB)
points(log(AGWP)~bio5,data=dat2[dat2$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB)
points(log(AGWP)~bio5,data=dat2[dat2$Continent=="AF",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[2],ylab=expression(paste("ln (Carbon) / Mg ha"^-1)),xlab=expression(paste("Mean daily maximum temperature / "^o, "C")),cex=WtAGWP)

a2<-make.line("bio5",dat2[dat2$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP",dash=2)
a3<-make.line("bio5",dat2[dat2$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
a4<-make.line("bio5",dat2[dat2$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
a5<-make.line("bio5",dat2[dat2$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP",dash=2)
points(preds.agwp~pred.dat$bio5.2,type="l",lwd=3)
legend("bottomleft",lwd=1,pch=16,col=cols,legend=c(paste("S America",a3),paste("Africa",a4),paste("Asia",a5),paste("Australia",a2)),bty="n",cex=0.9)

#BRT-bio5
plot(log(BRT)~bio5,data=dat2,type="n",pch=16,col=cols2[1],ylab="",xlab="",cex=WtAGB,yaxt="n",xlim=c(25.8,35.5))
mtext("Carbon residence time (years)",side=2,line=1.5,cex=1)
axis(2,at=c(3,3.5,4,4.5,5),labels=round(exp(c(3,3.5,4,4.5,5)),0))
mtext(side=1,line=3.9,expression(atop("Mean daily maximum temperature",paste("in the warmest month ("^o,"C)"))))
points(log(BRT)~bio5,data=dat2[dat2$Continent=="AS",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[3],ylab=expression(paste("ln (Carbon) / Mg ha"^-1)),xlab=expression(paste("Mean daily maximum temperature / "^o, "C")),cex=WtAGWP)
points(log(BRT)~bio5,data=dat2[dat2$Continent=="AU",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[4],cex=WtAGB)
points(log(BRT)~bio5,data=dat2[dat2$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB)
points(log(BRT)~bio5,data=dat2[dat2$Continent=="AF",],ylim=c(3.8,6.2),xlim=c(23,34),pch=16,col=cols2[2],ylab=expression(paste("ln (Carbon) / Mg ha"^-1)),xlab=expression(paste("Mean daily maximum temperature / "^o, "C")),cex=WtAGWP)

a2<-make.line("bio5",dat2[dat2$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT",dash=2)
a3<-make.line("bio5",dat2[dat2$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT",dash=2)
a4<-make.line("bio5",dat2[dat2$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT",dash=2)
a5<-make.line("bio5",dat2[dat2$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT",dash=2)
points(preds.brt2~pred.dat$bio5.2,type="l",lwd=3,lty=2)
legend("bottomleft",lwd=1,pch=16,col=cols,legend=c(paste("S America",a3),paste("Africa",a4),paste("Asia",a5),paste("Australia",a2)),bty="n",cex=0.9)

dev.off()


save.image(file='/home/femeunier/Documents/projects/Congo.ED2/scripts/Sullivan/Fig3.Env.R')
