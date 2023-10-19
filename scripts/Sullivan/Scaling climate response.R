# Load required libraries
library(raster)
library(rgdal)


# Read in input data files
# Current bio5 from Worldclim
bio5<-raster("wc2.0_bio_10m_05.tif")
# Baseline AGB map (Avitable)
agb<-raster("AGB_map.gri")
# Forest biome extent from WWF ecoregions
forest<-readOGR(dsn="E:\\Leeds\\",layer="TNC_broad_MF")
# Percentage forest cover from Hansen et al dataset
forest.perc<-raster("Forest_perc_2014.gri")
# File paths to rasters of predicted change to bio5 under different climate scenarios
paths<-c("RCP26_2050_Delta_bio5.grd",
"RCP45_2050_Delta_bio5.grd")

# Convert AGB from relative to absolute value, then convert to carbon
agb2<-agb*633.16
agb2<-agb2*0.456

# Crop climate baseline by forest extent
bio5<-crop(bio5,agb2)
# Resample AGB data to same resolution as climate data
agb3<-resample(agb2,bio5)

# Restrict AGB baseline map to tropical forest biome
agb3<-mask(agb3,forest)

# Get the area of each grid cell
rast.area<-area(agb3)
rast.area<-rast.area*100

# Resample forest area data to same resolution as climate data
forest.perc2<-resample(forest.perc,rast.area)



#Predict future climate response
#model coefficients
thresh.mod<-32.188
thresh.se<-0.227
t0.mod<--3.860e-02
t0.se<-2.173e-02
t1.mod<--1.588e-01
t1.se<-3.830e-02
max.obs<-35.24963


#Current climate
maxT<-bio5
#Create future climate grid

delta.carbon<-c()
non.anog.clim<-c()
mn.diff<-c()
delta.carbon2<-c()

##########################
# Part 1 - Get raster maps for Fig. 4

#Current above thresh
above.thresh<-calc(maxT,function(x)ifelse(x>thresh,1,NA))
above.thresh<-crop(above.thresh,agb3)
above.thresh<-mask(above.thresh,agb3)
cellStats(above.thresh,sum)/51525

#Current non anog
above.thresh<-calc(maxT,function(x)ifelse(x>max.obs,1,NA))
above.thresh<-crop(above.thresh,agb3)
above.thresh<-mask(above.thresh,agb3)
cellStats(above.thresh,sum)/51525


# Get RCP2.6 data
rcp<-raster(paths[1])
MaxT.fut<-bio5+rcp

# Assign model parameters to objects 
thresh<-thresh.mod
t0<-t0.mod
t1<-t1.mod
# Current and future climate above and below threshold
tpres.above<-calc(maxT,function(x)ifelse(x>thresh,x-thresh,0))
tfut.above<-calc(MaxT.fut,function(x)ifelse(x>thresh,x-thresh,0))
tpres.below<-calc(maxT,function(x)ifelse(x>thresh,thresh,x))
tfut.below<-calc(MaxT.fut,function(x)ifelse(x>thresh,thresh,x))
# How much change in temperature has been above threshold or below threhold
tdiff.above<-tfut.above-tpres.above
tdiff.below<-tfut.below-tpres.below
# Calculate predicted change in carbon stocks given model and temperature change
delta.agb<-(t0*tdiff.below)+(t1*tdiff.above)
delta.agb<-crop(delta.agb,agb3)

# Apply predicted change to log (AGB)
ln.agb<-log(agb3)
agb.poly<-ln.agb+delta.agb
delta.poly<-exp(agb.poly)-agb3
perc.poly<-(agb3-exp(agb.poly))/agb3
perc.poly<-perc.poly*100

# Get area above threshold
above.thresh<-calc(MaxT.fut,function(x)ifelse(x>thresh,1,NA))
above.thresh<-crop(above.thresh,agb3)
above.thresh<-mask(above.thresh,agb3)
cellStats(above.thresh,sum)/51525

# Write rasters giving absolute and percentage change
writeRaster(delta.poly,"Delta carbon RCP 26.grd")
writeRaster(perc.poly,"Delta carbon_percentage RCP 26.grd")


# Do same for RCP 4.5 scenario
#RCP 4.5
rcp<-raster(paths[2])
MaxT.fut<-bio5+rcp


thresh<-thresh.mod
t0<-t0.mod
t1<-t1.mod
tpres.above<-calc(maxT,function(x)ifelse(x>thresh,x-thresh,0))
tfut.above<-calc(MaxT.fut,function(x)ifelse(x>thresh,x-thresh,0))
tpres.below<-calc(maxT,function(x)ifelse(x>thresh,thresh,x))
tfut.below<-calc(MaxT.fut,function(x)ifelse(x>thresh,thresh,x))
tdiff.above<-tfut.above-tpres.above
tdiff.below<-tfut.below-tpres.below
delta.agb<-(t0*tdiff.below)+(t1*tdiff.above)
delta.agb<-crop(delta.agb,agb3)


agb.poly<-ln.agb+delta.agb
delta.poly<-exp(agb.poly)-agb3
perc.poly<-(agb3-exp(agb.poly))/agb3
perc.poly<-perc.poly*100

above.thresh<-calc(MaxT.fut,function(x)ifelse(x>thresh,1,NA))
above.thresh<-crop(above.thresh,agb3)
above.thresh<-mask(above.thresh,agb3)
cellStats(above.thresh,sum)/51525

writeRaster(delta.poly,"Delta carbon RCP 45.grd")
writeRaster(perc.poly,"Delta carbon_percentage RCP 45.grd")

#######################################################################
# Part 2 - change in carbon stocks with uncertanty
# Like above but resamples model parameters from mean and SD
#######1. Effect of 1 C
n<-1000 # Number of resamples to run

MaxT.fut<-bio5+1

for(i in 1:n){
thresh<-rnorm(1,thresh.mod,thresh.se)
t0<-rnorm(1,t0.mod,t0.se)
t1<-rnorm(1,t1.mod,t1.se)
tpres.above<-calc(maxT,function(x)ifelse(x>thresh,x-thresh,0))
tfut.above<-calc(MaxT.fut,function(x)ifelse(x>thresh,x-thresh,0))
tpres.below<-calc(maxT,function(x)ifelse(x>thresh,thresh,x))
tfut.below<-calc(MaxT.fut,function(x)ifelse(x>thresh,thresh,x))
tdiff.above<-tfut.above-tpres.above
tdiff.below<-tfut.below-tpres.below
delta.agb<-(t0*tdiff.below)+(t1*tdiff.above)
delta.agb.lin<-lin.cof*(MaxT.fut-maxT)
delta.agb<-crop(delta.agb,agb3)

ln.agb<-log(agb3)
agb.poly<-ln.agb+delta.agb
agb.ln<-ln.agb+delta.agb.lin
delta.poly<-exp(agb.poly)-agb3
delta.lin<-exp(agb.ln)-agb3
perc.poly<-(agb3-exp(agb.poly))/agb3
perc.poly<-perc.poly*100
non.anog<-calc(MaxT.fut,function(x)ifelse(x>max.obs,1,0))
non.anog<-crop(non.anog,agb3)
non.anog<-mask(non.anog,agb3)
new.clim<-cellStats(non.anog,sum)/length(non.anog)


tot<-delta.poly*rast.area*forest.perc2
tot2<-delta.lin*rast.area*forest.perc2
delta.carbon[i]<-cellStats(tot,sum)/1e9
if(i ==1){
delta.carbon2<-cellStats(tot2,sum)/1e9
non.anog.clim<-cellStats(non.anog,sum)/51525 #51525
mn.diff<-cellStats(MaxT.fut-maxT,mean)
}
print(i)
}

poly.1deg<-quantile(delta.carbon,c(0.025,0.5,0.975))
lin.1deg<-delta.carbon2
non.anog.1deg<-non.anog.clim
trop.warm.1deg<-mn.diff


#######2. RCP 2.6, 2050. ~ 1.5 C warming
rcp<-raster(paths[1])
MaxT.fut<-bio5+rcp

for(i in 1:n){
thresh<-rnorm(1,thresh.mod,thresh.se)
t0<-rnorm(1,t0.mod,t0.se)
t1<-rnorm(1,t1.mod,t1.se)
tpres.above<-calc(maxT,function(x)ifelse(x>thresh,x-thresh,0))
tfut.above<-calc(MaxT.fut,function(x)ifelse(x>thresh,x-thresh,0))
tpres.below<-calc(maxT,function(x)ifelse(x>thresh,thresh,x))
tfut.below<-calc(MaxT.fut,function(x)ifelse(x>thresh,thresh,x))
tdiff.above<-tfut.above-tpres.above
tdiff.below<-tfut.below-tpres.below
delta.agb<-(t0*tdiff.below)+(t1*tdiff.above)
delta.agb.lin<-lin.cof*(MaxT.fut-maxT)
delta.agb<-crop(delta.agb,agb3)

ln.agb<-log(agb3)
agb.poly<-ln.agb+delta.agb
agb.ln<-ln.agb+delta.agb.lin
delta.poly<-exp(agb.poly)-agb3
delta.lin<-exp(agb.ln)-agb3
perc.poly<-(agb3-exp(agb.poly))/agb3
perc.poly<-perc.poly*100
non.anog<-calc(MaxT.fut,function(x)ifelse(x>max.obs,1,0))
non.anog<-crop(non.anog,agb3)
non.anog<-mask(non.anog,agb3)
new.clim<-cellStats(non.anog,sum)/length(non.anog)


tot<-delta.poly*rast.area*forest.perc2
tot2<-delta.lin*rast.area*forest.perc2
delta.carbon[i]<-cellStats(tot,sum)/1e9
if(i ==1){
delta.carbon2<-cellStats(tot2,sum)/1e9
non.anog.clim<-cellStats(non.anog,sum)/51525 #51525
mn.diff<-cellStats(MaxT.fut-maxT,mean)
}
print(i)
}

poly.RCP26<-quantile(delta.carbon,c(0.025,0.5,0.975))
poly.RCP26.store<-delta.carbon
lin.RCP26<-delta.carbon2
non.anog.RCP26<-non.anog.clim
trop.warm.RCP26<-mn.diff

#######3. RCP 4.5, 2050. ~ 2 C warming
rcp<-raster(paths[2])
MaxT.fut<-bio5+rcp

for(i in 1:n){
thresh<-rnorm(1,thresh.mod,thresh.se)
t0<-rnorm(1,t0.mod,t0.se)
t1<-rnorm(1,t1.mod,t1.se)
tpres.above<-calc(maxT,function(x)ifelse(x>thresh,x-thresh,0))
tfut.above<-calc(MaxT.fut,function(x)ifelse(x>thresh,x-thresh,0))
tpres.below<-calc(maxT,function(x)ifelse(x>thresh,thresh,x))
tfut.below<-calc(MaxT.fut,function(x)ifelse(x>thresh,thresh,x))
tdiff.above<-tfut.above-tpres.above
tdiff.below<-tfut.below-tpres.below
delta.agb<-(t0*tdiff.below)+(t1*tdiff.above)
delta.agb.lin<-lin.cof*(MaxT.fut-maxT)
delta.agb<-crop(delta.agb,agb3)

ln.agb<-log(agb3)
agb.poly<-ln.agb+delta.agb
agb.ln<-ln.agb+delta.agb.lin
delta.poly<-exp(agb.poly)-agb3
delta.lin<-exp(agb.ln)-agb3
perc.poly<-(agb3-exp(agb.poly))/agb3
perc.poly<-perc.poly*100
non.anog<-calc(MaxT.fut,function(x)ifelse(x>max.obs,1,0))
#non.anog2<-calc(MaxT.fut,function(x)ifelse(x>max.obs,1,NA))
non.anog<-crop(non.anog,agb3)
non.anog<-mask(non.anog,agb3)
new.clim<-cellStats(non.anog,sum)/length(non.anog)


tot<-delta.poly*rast.area*forest.perc2
tot2<-delta.lin*rast.area*forest.perc2
delta.carbon[i]<-cellStats(tot,sum)/1e9
if(i ==1){
delta.carbon2<-cellStats(tot2,sum)/1e9
non.anog.clim<-cellStats(non.anog,sum)/51525 #51525
mn.diff<-cellStats(MaxT.fut-maxT,mean)
}
print(i)
}

poly.RCP45<-quantile(delta.carbon,c(0.025,0.5,0.975))
lin.RCP45<-delta.carbon2
poly.RCP45.store<-delta.carbon
non.anog.RCP45<-non.anog.clim
trop.warm.RCP45<-mn.diff


