rm(list = ls())

library(raster)

file <- "/data/gent/vo/000/gvo00074/felicien/ELIE/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1.tif"
r <- raster(file)
e <- extent(-82,-30,-20,15)
e <- extent(-10,52,-15,10)

r.crop <- crop(r,e)

writeRaster(r.crop,"/data/gent/vo/000/gvo00074/felicien/ELIE/C3S-LC-L4-LCCS-Map-300m-P1Y-2020-v2.1.1_Congo.tif",options=c('TFW=YES'),
            overwrite = TRUE)
