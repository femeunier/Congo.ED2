rm(list = ls())

dat <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Sullivan/MultiCensusPlots.csv",header=T,as.is=T) %>%
  mutate(AGB = AGB*0.456,
         AGWP = AGWP*0.456) %>%
  mutate(BRT = AGB/AGWP) %>%
  mutate(bio5 = bio5+(Warm.rate2*(Midpoint-1985))) %>%
  mutate(Min.temp = Min.temp+(Warm.rate2*(Midpoint-1985))) %>%
  mutate(WtAGB = Area^(1/3)) %>%
  mutate(WtAGWP = MonitorLength^(1/7)) %>%
  mutate(WtBRT = Area^(1/9)+MonitorLength^(1/12)-1) %>%
  mutate(CEC = log(CEC))


rf<-randomForest(log(AGB/mean(AGB))~Min.temp+bio5+wind+bio17+Cloud+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
                 data=dat,
                 weights=WtAGB,
                 na.action=na.fail)

rf<-randomForest(log(AGB)~bio5+bio17+MEM1+MEM2,
                 data=dat,
                 weights=WtAGB,
                 na.action=na.fail)

randomForest::importance(rf)

partialPlot(rf,dat,"bio5")
partialPlot(rf,dat,"bio17")


networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan.RDS")

networks2plot <- networks %>%
  group_by(Continent) %>%
  rename(lat = Lat,lon = Lon) %>%
  dplyr::select(PlotCode,lat,lon,int_ini,int_fin,Continent,AGBnetchange.ha.yr,AGB_ini.ha) %>%
  rename(sink = AGBnetchange.ha.yr,
         AGB_ini = AGB_ini.ha) %>%
  mutate(yr_ini = floor(int_ini),
         yr_fin = floor(int_fin)) %>%
  dplyr::select(-c(int_ini,int_fin))

library(adespatial)
library(SoDA)
library(MuMIn)

xy2<-geoXY(networks2plot$lat,networks2plot$lon)
mems<-dbmem(xy2)


# Subset to single-census plots
sc.dat<-all.dat.scale[!all.dat.scale$PlotCode%in%dat.scale$PlotCode,]

# Calculate MEMs for combined and single-census data
xy2<-geoXY(all.dat.scale$Lat,all.dat.scale$Long)
mems<-dbmem(xy2)
all.dat.scale<-cbind(all.dat.scale,mems)

xy2<-geoXY(sc.dat$Lat,sc.dat$Long)
mems<-dbmem(xy2)
sc.dat<-cbind(sc.dat,mems)
