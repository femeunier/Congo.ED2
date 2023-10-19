library(RColorBrewer)

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

# Create weights
dat$WtAGB<-dat$Area^(1/3)
dat$WtAGWP<-dat$MonitorLength^(1/7)
dat$WtBRT<-dat$Area^(1/9)+dat$MonitorLength^(1/12)-1



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


make.line2<-function(var,data,weight,color,cont=FALSE,resp="AGB"){
if(cont==FALSE){
mf<-lm(as.formula(paste("log(",resp,")~",var)),data=data,weights=data$WtAGB)
}else{
mf<-lm(as.formula(paste("log(",resp,")~",var,"+ Continent")),data=data,weights=data$WtAGB)
}
sig<-summary(mf)$coefficients[2,4]
pred.dat<-data.frame(var=seq(min(data[,var]),max(data[,var]),length.out=100),"Continent"=factor("AF",levels=c("AF","SA")))
names(pred.dat)<-c(var,"Continent")
preds<-predict(mf,newdata=pred.dat,se.fit=T)
if(sig<0.05){
if(cont==TRUE){
points(preds$fit~pred.dat[,var],type="l",lwd=weight,col=color)
#points(I(preds$fit-preds$se.fit)~pred.dat[,var],type="l",lty=2,lwd=weight/3,col=color)
#points(I(preds$fit+preds$se.fit)~pred.dat[,var],type="l",lty=2,lwd=weight/3,col=color)
}else{
points(preds$fit~pred.dat[,var],type="l",lwd=weight,col=color)
}
}
}

#Plot start
cols2<-makeTransparent(cols,0.5)

par(mgp=c(2,0.5,0))
par(mar=c(4,4,1,1))
par(mfrow=c(4,2))
cex.factor<-0.8

#MaxT
plot(log(AGB)~bio5,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon stocks) (Mg ha"^-1,")")),xlab=expression(paste("Maximum temperature, warmest month ("^o,"C)")),cex=WtAGB*cex.factor)
points(log(AGB)~bio5,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGB*cex.factor)
points(log(AGB)~bio5,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGB*cex.factor)
points(log(AGB)~bio5,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB*cex.factor)
points(log(AGB)~bio5,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGB*cex.factor)
make.line2("bio5",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGB")
make.line2("bio5",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGB")
make.line2("bio5",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGB")
make.line2("bio5",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGB")
make.line2("bio5",dat,weight=3,col="black",cont=TRUE,resp="AGB")

#MinT
plot(log(AGB)~Min.temp,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon stocks) (Mg ha"^-1,")")),xlab=expression(paste("Minimum temperature ("^o,"C)")),cex=WtAGB*cex.factor)
points(log(AGB)~Min.temp,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGB*cex.factor)
points(log(AGB)~Min.temp,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGB*cex.factor)
points(log(AGB)~Min.temp,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB*cex.factor)
points(log(AGB)~Min.temp,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGB*cex.factor)
make.line2("Min.temp",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGB")
make.line2("Min.temp",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGB")
make.line2("Min.temp",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGB")
make.line2("Min.temp",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGB")
make.line2("Min.temp",dat,weight=3,col="black",cont=TRUE,resp="AGB")

#PrecipDQ
plot(log(AGB)~bio17,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon stocks) (Mg ha"^-1,")")),xlab="Precipitation, driest quarter (mm)",cex=WtAGB*cex.factor)
points(log(AGB)~bio17,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGB*cex.factor)
points(log(AGB)~bio17,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGB*cex.factor)
points(log(AGB)~bio17,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB*cex.factor)
points(log(AGB)~bio17,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGB*cex.factor)
make.line2("bio17",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGB")
make.line2("bio17",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGB")
make.line2("bio17",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGB")
make.line2("bio17",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGB")
make.line2("bio17",dat,weight=3,col="black",cont=TRUE,resp="AGB")

#Cloud
plot(log(AGB)~Cloud,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon stocks) (Mg ha"^-1,")")),xlab="Cloud cover (%)",cex=WtAGB*cex.factor)
points(log(AGB)~Cloud,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGB*cex.factor)
points(log(AGB)~Cloud,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGB*cex.factor)
points(log(AGB)~Cloud,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB*cex.factor)
points(log(AGB)~Cloud,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGB*cex.factor)
make.line2("Cloud",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGB")
make.line2("Cloud",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGB")
make.line2("Cloud",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGB")
make.line2("Cloud",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGB")
make.line2("Cloud",dat,weight=3,col="black",cont=TRUE,resp="AGB")

#Wind
plot(log(AGB)~wind,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon stocks) (Mg ha"^-1,")")),xlab=expression(paste("Mean wind speed (m s"^-1,")")),cex=WtAGB*cex.factor)
points(log(AGB)~wind,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGB*cex.factor)
points(log(AGB)~wind,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGB*cex.factor)
points(log(AGB)~wind,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB*cex.factor)
points(log(AGB)~wind,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGB*cex.factor)
make.line2("wind",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGB")
make.line2("wind",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGB")
make.line2("wind",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGB")
make.line2("wind",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGB")
make.line2("wind",dat,weight=3,col="black",cont=TRUE,resp="AGB")

#CEC
plot(log(AGB)~CEC,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon stocks) (Mg ha"^-1,")")),xlab=expression(paste("ln (CEC) (cmol kg"^-1,")")),cex=WtAGB*cex.factor)
points(log(AGB)~CEC,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGB*cex.factor)
points(log(AGB)~CEC,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGB*cex.factor)
points(log(AGB)~CEC,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB*cex.factor)
points(log(AGB)~CEC,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGB*cex.factor)
make.line2("CEC",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGB")
make.line2("CEC",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGB")
make.line2("CEC",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGB")
make.line2("CEC",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGB")
make.line2("CEC",dat,weight=3,col="black",cont=TRUE,resp="AGB")

#Clay
plot(log(AGB)~Clay,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon stocks) (Mg ha"^-1,")")),xlab="Clay (%)",cex=WtAGB*cex.factor)
points(log(AGB)~Clay,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGB*cex.factor)
points(log(AGB)~Clay,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGB*cex.factor)
points(log(AGB)~Clay,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGB*cex.factor)
points(log(AGB)~Clay,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGB*cex.factor)
make.line2("Clay",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGB")
make.line2("Clay",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGB")
make.line2("Clay",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGB")
make.line2("Clay",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGB")
make.line2("Clay",dat,weight=3,col="black",cont=TRUE,resp="AGB")

plot(1,type="n",axes=FALSE,ylab="",xlab="")
legend("center",legend=c("South America","Africa","Asia","Australia","Pantropical"),col=c(cols,"black"),pch=c(16,16,16,16,-1),lwd=2,bty="n")

###############
#AGWP
par(mgp=c(2,0.5,0))
par(mar=c(4,4,2,2))
par(mfrow=c(4,2))
#MaxT
plot(log(AGWP)~bio5,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon gains) (Mg ha"^-1,"year"^-1,")")),xlab=expression(paste("Maximum temperature, warmest month ("^o,"C)")),cex=WtAGWP*cex.factor)
points(log(AGWP)~bio5,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGWP*cex.factor)
points(log(AGWP)~bio5,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGWP*cex.factor)
points(log(AGWP)~bio5,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGWP*cex.factor)
points(log(AGWP)~bio5,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGWP*cex.factor)
make.line2("bio5",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP")
make.line2("bio5",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP")
make.line2("bio5",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
make.line2("bio5",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
make.line2("bio5",dat,weight=3,col="black",cont=TRUE,resp="AGWP")

#MinT
plot(log(AGWP)~Min.temp,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon gains) (Mg ha"^-1,"year"^-1,")")),xlab=expression(paste("Minimum temperature ("^o,"C)")),cex=WtAGWP*cex.factor)
points(log(AGWP)~Min.temp,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGWP*cex.factor)
points(log(AGWP)~Min.temp,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGWP*cex.factor)
points(log(AGWP)~Min.temp,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGWP*cex.factor)
points(log(AGWP)~Min.temp,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGWP*cex.factor)
make.line2("Min.temp",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP")
make.line2("Min.temp",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP")
make.line2("Min.temp",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
make.line2("Min.temp",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
make.line2("Min.temp",dat,weight=3,col="black",cont=TRUE,resp="AGWP")

#PrecipDQ
plot(log(AGWP)~bio17,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon gains) (Mg ha"^-1,"year"^-1,")")),xlab="Precipitation, driest quarter (mm)",cex=WtAGWP*cex.factor)
points(log(AGWP)~bio17,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGWP*cex.factor)
points(log(AGWP)~bio17,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGWP*cex.factor)
points(log(AGWP)~bio17,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGWP*cex.factor)
points(log(AGWP)~bio17,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGWP*cex.factor)
make.line2("bio17",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP")
make.line2("bio17",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP")
make.line2("bio17",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
make.line2("bio17",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
make.line2("bio17",dat,weight=3,col="black",cont=TRUE,resp="AGWP")

#Cloud
plot(log(AGWP)~Cloud,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon gains) (Mg ha"^-1,"year"^-1,")")),xlab="Cloud cover (%)",cex=WtAGWP*cex.factor)
points(log(AGWP)~Cloud,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGWP*cex.factor)
points(log(AGWP)~Cloud,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGWP*cex.factor)
points(log(AGWP)~Cloud,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGWP*cex.factor)
points(log(AGWP)~Cloud,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGWP*cex.factor)
make.line2("Cloud",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP")
make.line2("Cloud",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP")
make.line2("Cloud",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
make.line2("Cloud",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
make.line2("Cloud",dat,weight=3,col="black",cont=TRUE,resp="AGWP")

#Wind
plot(log(AGWP)~wind,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon gains) (Mg ha"^-1,"year"^-1,")")),xlab=expression(paste("Mean wind speed (m s"^-1,")")),cex=WtAGWP*cex.factor)
points(log(AGWP)~wind,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGWP*cex.factor)
points(log(AGWP)~wind,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGWP*cex.factor)
points(log(AGWP)~wind,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGWP*cex.factor)
points(log(AGWP)~wind,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGWP*cex.factor)
make.line2("wind",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP")
make.line2("wind",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP")
make.line2("wind",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
make.line2("wind",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
make.line2("wind",dat,weight=3,col="black",cont=TRUE,resp="AGWP")

#CEC
plot(log(AGWP)~CEC,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon gains) (Mg ha"^-1,"year"^-1,")")),xlab=expression(paste("ln (CEC) (cmol kg"^-1,")")),cex=WtAGWP*cex.factor)
points(log(AGWP)~CEC,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGWP*cex.factor)
points(log(AGWP)~CEC,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGWP*cex.factor)
points(log(AGWP)~CEC,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGWP*cex.factor)
points(log(AGWP)~CEC,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGWP*cex.factor)
make.line2("CEC",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP")
make.line2("CEC",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP")
make.line2("CEC",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
make.line2("CEC",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
make.line2("CEC",dat,weight=3,col="black",cont=TRUE,resp="AGWP")

#Clay
plot(log(AGWP)~Clay,data=dat,type="n",pch=16,col=cols2[1],ylab=expression(paste("ln (Carbon gains) (Mg ha"^-1,"year"^-1,")")),xlab="Clay (%)",cex=WtAGWP*cex.factor)
points(log(AGWP)~Clay,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtAGWP*cex.factor)
points(log(AGWP)~Clay,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtAGWP*cex.factor)
points(log(AGWP)~Clay,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtAGWP*cex.factor)
points(log(AGWP)~Clay,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtAGWP*cex.factor)
make.line2("Clay",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="AGWP")
make.line2("Clay",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="AGWP")
make.line2("Clay",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="AGWP")
make.line2("Clay",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="AGWP")
make.line2("Clay",dat,weight=3,col="black",cont=TRUE,resp="AGWP")

plot(1,type="n",axes=FALSE,ylab="",xlab="")
legend("center",legend=c("South America","Africa","Asia","Australia","Pantropical"),col=c(cols,"black"),pch=c(16,16,16,16,-1),lwd=2,bty="n")



###############
#BRT
par(mgp=c(2,0.5,0))
par(mar=c(4,4,2,2))
par(mfrow=c(4,2))
#MaxT
plot(log(BRT)~bio5,data=dat,type="n",pch=16,col=cols2[1],ylab="ln (Carbon residence time) (years)",xlab=expression(paste("Maximum temperature, warmest month ("^o,"C)")),cex=WtBRT*cex.factor)
points(log(BRT)~bio5,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtBRT*cex.factor)
points(log(BRT)~bio5,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtBRT*cex.factor)
points(log(BRT)~bio5,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtBRT*cex.factor)
points(log(BRT)~bio5,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtBRT*cex.factor)
make.line2("bio5",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT")
make.line2("bio5",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT")
make.line2("bio5",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT")
make.line2("bio5",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT")
make.line2("bio5",dat,weight=3,col="black",cont=TRUE,resp="BRT")

#MinT
plot(log(BRT)~Min.temp,data=dat,type="n",pch=16,col=cols2[1],ylab="ln (Carbon residence time) (years)",xlab=expression(paste("Minimum temperature ("^o,"C)")),cex=WtBRT*cex.factor)
points(log(BRT)~Min.temp,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtBRT*cex.factor)
points(log(BRT)~Min.temp,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtBRT*cex.factor)
points(log(BRT)~Min.temp,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtBRT*cex.factor)
points(log(BRT)~Min.temp,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtBRT*cex.factor)
make.line2("Min.temp",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT")
make.line2("Min.temp",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT")
make.line2("Min.temp",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT")
make.line2("Min.temp",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT")
make.line2("Min.temp",dat,weight=3,col="black",cont=TRUE,resp="BRT")

#PrecipDQ
plot(log(BRT)~bio17,data=dat,type="n",pch=16,col=cols2[1],ylab="ln (Carbon residence time) (years)",xlab="Precipitation, driest quarter (mm)",cex=WtBRT*cex.factor)
points(log(BRT)~bio17,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtBRT*cex.factor)
points(log(BRT)~bio17,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtBRT*cex.factor)
points(log(BRT)~bio17,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtBRT*cex.factor)
points(log(BRT)~bio17,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtBRT*cex.factor)
make.line2("bio17",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT")
make.line2("bio17",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT")
make.line2("bio17",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT")
make.line2("bio17",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT")
make.line2("bio17",dat,weight=3,col="black",cont=TRUE,resp="BRT")

#Cloud
plot(log(BRT)~Cloud,data=dat,type="n",pch=16,col=cols2[1],ylab="ln (Carbon residence time) (years)",xlab="Cloud cover (%)",cex=WtBRT*cex.factor)
points(log(BRT)~Cloud,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtBRT*cex.factor)
points(log(BRT)~Cloud,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtBRT*cex.factor)
points(log(BRT)~Cloud,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtBRT*cex.factor)
points(log(BRT)~Cloud,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtBRT*cex.factor)
make.line2("Cloud",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT")
make.line2("Cloud",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT")
make.line2("Cloud",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT")
make.line2("Cloud",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT")
make.line2("Cloud",dat,weight=3,col="black",cont=TRUE,resp="BRT")

#Wind
plot(log(BRT)~wind,data=dat,type="n",pch=16,col=cols2[1],ylab="ln (Carbon residence time) (years)",xlab=expression(paste("Mean wind speed (m s"^-1,")")),cex=WtBRT*cex.factor)
points(log(BRT)~wind,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtBRT*cex.factor)
points(log(BRT)~wind,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtBRT*cex.factor)
points(log(BRT)~wind,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtBRT*cex.factor)
points(log(BRT)~wind,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtBRT*cex.factor)
make.line2("wind",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT")
make.line2("wind",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT")
make.line2("wind",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT")
make.line2("wind",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT")
make.line2("wind",dat,weight=3,col="black",cont=TRUE,resp="BRT")

#CEC
plot(log(BRT)~CEC,data=dat,type="n",pch=16,col=cols2[1],ylab="ln (Carbon residence time) (years)",xlab=expression(paste("ln (CEC) (cmol kg"^-1,")")),cex=WtBRT*cex.factor)
points(log(BRT)~CEC,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtBRT*cex.factor)
points(log(BRT)~CEC,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtBRT*cex.factor)
points(log(BRT)~CEC,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtBRT*cex.factor)
points(log(BRT)~CEC,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtBRT*cex.factor)
make.line2("CEC",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT")
make.line2("CEC",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT")
make.line2("CEC",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT")
make.line2("CEC",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT")
make.line2("CEC",dat,weight=3,col="black",cont=TRUE,resp="BRT")

#Clay
plot(log(BRT)~Clay,data=dat,type="n",pch=16,col=cols2[1],ylab="ln (Carbon residence time) (years)",xlab="Clay (%)",cex=WtBRT*cex.factor)
points(log(BRT)~Clay,data=dat[dat$Continent=="AS",],pch=16,col=cols2[3],cex=WtBRT*cex.factor)
points(log(BRT)~Clay,data=dat[dat$Continent=="AU",],pch=16,col=cols2[4],cex=WtBRT*cex.factor)
points(log(BRT)~Clay,data=dat[dat$Continent=="SA",],pch=16,col=cols2[1],cex=WtBRT*cex.factor)
points(log(BRT)~Clay,data=dat[dat$Continent=="AF",],pch=16,col=cols2[2],cex=WtBRT*cex.factor)
make.line2("Clay",dat[dat$Continent=="AU",],weight=3,col=darken(cols[4],1.1),resp="BRT")
make.line2("Clay",dat[dat$Continent=="AS",],weight=3,col=darken(cols[3],1.1),resp="BRT")
make.line2("Clay",dat[dat$Continent=="AF",],weight=3,col=darken(cols[2],1.1),resp="BRT")
make.line2("Clay",dat[dat$Continent=="SA",],weight=3,col=darken(cols[1],1.1),resp="BRT")
make.line2("Clay",dat,weight=3,col="black",cont=TRUE,resp="BRT")


plot(1,type="n",axes=FALSE,ylab="",xlab="")
legend("center",legend=c("South America","Africa","Asia","Australia","Pantropical"),col=c(cols,"black"),pch=c(16,16,16,16,-1),lwd=2,bty="n")


###############
#Values in each continent

par(mfrow=c(2,3))

#Histogram
a<-density(dat$AGB[dat$Continent=="SA"])
plot(a,main="",xlab=expression(paste("Carbon stocks (Mg ha"^-1,")")),type="n",xlim=c(0,500),yaxt="n",ylab="Probability density")
polygon(a,col=cols2[1])
a<-density(dat$AGB[dat$Continent=="AF"])
polygon(a,col=cols2[2])
a<-density(dat$AGB[dat$Continent=="AS"])
polygon(a,col=cols2[3])
a<-density(dat$AGB[dat$Continent=="AU"])
polygon(a,col=cols2[4])
legend("topright",fill=cols2,legend=c("SA","AF","AS","AU"))

#Histogram
a<-density(dat$AGWP[dat$Continent=="AU"])
plot(a,main="",xlab=expression(paste("Carbon gains (Mg ha"^-1," yr"^-1,")")),type="n",xlim=c(0,6),yaxt="n",ylab="")
polygon(a,col=cols2[4])
a<-density(dat$AGWP[dat$Continent=="AF"])
polygon(a,col=cols2[2])
a<-density(dat$AGWP[dat$Continent=="AS"])
polygon(a,col=cols2[3])
a<-density(dat$AGWP[dat$Continent=="SA"])
polygon(a,col=cols2[1])
legend("topright",fill=cols2,legend=c("SA","AF","AS","AU"))

#Histogram
a<-density(dat$BRT[dat$Continent=="AU"])
plot(a,main="",xlab="Carbon residence time (years)",type="n",xlim=c(0,250),ylim=c(0,0.025),yaxt="n",ylab="")
polygon(a,col=cols2[4])
a<-density(dat$BRT[dat$Continent=="AF"])
polygon(a,col=cols2[2])
a<-density(dat$BRT[dat$Continent=="AS"])
polygon(a,col=cols2[3])
a<-density(dat$BRT[dat$Continent=="SA"])
polygon(a,col=cols2[1])
legend("topright",fill=cols2,legend=c("SA","AF","AS","AU"))

par(las=1)
plot(1,type="l",axes=FALSE,ylim=c(0.5,5),xlim=c(0,500),xlab=expression(paste("Carbon stocks (Mg ha"^-1,")")),ylab="Continent")
axis(1)
axis(2,at=c(1,2,3,4),lab=c("AU","AS","AF","SA"))
v1<-dat$AGB[dat$Continent=="SA"]
j1<-rnorm(length(v1),4,0.1)
points(v1,j1,pch=16,col=cols2[1])

v1<-dat$AGB[dat$Continent=="AF"]
j1<-rnorm(length(v1),3,0.1)
points(v1,j1,pch=16,col=cols2[2])

v1<-dat$AGB[dat$Continent=="AS"]
j1<-rnorm(length(v1),2,0.1)
points(v1,j1,pch=16,col=cols2[3])

v1<-dat$AGB[dat$Continent=="AU"]
j1<-rnorm(length(v1),1,0.1)
points(v1,j1,pch=16,col=cols2[4])
box(bty="l")



plot(1,type="l",axes=FALSE,ylim=c(0.5,5),xlim=c(0,6),xlab=expression(paste("Carbon gains (Mg ha"^-1," yr"^-1,")")),ylab="Continent")
axis(1)
axis(2,at=c(1,2,3,4),lab=c("AU","AS","AF","SA"))
v1<-dat$AGWP[dat$Continent=="SA"]
j1<-rnorm(length(v1),4,0.1)
points(v1,j1,pch=16,col=cols2[1])

v1<-dat$AGWP[dat$Continent=="AF"]
j1<-rnorm(length(v1),3,0.1)
points(v1,j1,pch=16,col=cols2[2])

v1<-dat$AGWP[dat$Continent=="AS"]
j1<-rnorm(length(v1),2,0.1)
points(v1,j1,pch=16,col=cols2[3])

v1<-dat$AGWP[dat$Continent=="AU"]
j1<-rnorm(length(v1),1,0.1)
points(v1,j1,pch=16,col=cols2[4])
box(bty="l")



plot(1,type="l",axes=FALSE,ylim=c(0.5,5),xlim=c(0,250),xlab="Carbon residence time (years)",ylab="Continent")
axis(1)
axis(2,at=c(1,2,3,4),lab=c("AU","AS","AF","SA"))
v1<-dat$BRT[dat$Continent=="SA"]
j1<-rnorm(length(v1),4,0.1)
points(v1,j1,pch=16,col=cols2[1])

v1<-dat$BRT[dat$Continent=="AF"]
j1<-rnorm(length(v1),3,0.1)
points(v1,j1,pch=16,col=cols2[2])

v1<-dat$BRT[dat$Continent=="AS"]
j1<-rnorm(length(v1),2,0.1)
points(v1,j1,pch=16,col=cols2[3])

v1<-dat$BRT[dat$Continent=="AU"]
j1<-rnorm(length(v1),1,0.1)
points(v1,j1,pch=16,col=cols2[4])
box(bty="l")
