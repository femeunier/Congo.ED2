# Load R packages
library(raster)
library(rasterVis)
library(rworldmap)

# Read data files
# Delta carbon and percentage carbon rasters
RCP45<-raster("Delta carbon RCP 45.grd")
RCP45_perc<-raster("Delta carbon_percentage RCP 45.grd")


# Crop rasters to tropics
RCP45<-crop(RCP45,extent(-86,150,-23,23))
RCP45_perc<-crop(RCP45_perc,extent(-86,150,-23,23))
# Change percentages to negative (as percentage loss)
RCP45_perc<--RCP45_perc


# Get world map and crop to tropics 
world<-getMap(resolution="less islands")
trop<-crop(world,extent(-86,150,-23,23))

# Mask predictions by land extent
RCP45_perc<-mask(RCP45_perc,trop)
RCP45<-mask(RCP45,trop)

# Make raster stack for plotting
rcp.stack<-stack(RCP45_perc,RCP45)

# Define theme settings for plot
myTheme<-rasterTheme(region="dark gray")
myTheme2<-rasterTheme(region=rev(brewer.pal(9,"Oranges")))
par(mar=c(0,0,0,0))
my.at<-c(-80,seq(-60,0,length.out=9))
my.at2<-seq(-45,0,length.out=9)

# Start plot
p<-levelplot(RCP45,at=my.at,margin=FALSE,ylab="",xlab="",par.settings=myTheme2,main=expression(paste(Delta," Carbon stocks (Mg ha"^-1,")")))+
layer(sp.lines(world))

p2<-levelplot(RCP45_perc,at=my.at2,margin=FALSE,ylab="",xlab="",par.settings= myTheme2,main=expression(paste(Delta," Carbon stocks (%)")))+
layer(sp.lines(world))


print(p, split=c(1, 1, 1, 2), more=TRUE)
  print(p2, split=c(1, 2, 1, 2))

