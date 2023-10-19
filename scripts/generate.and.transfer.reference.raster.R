rm(list = ls())

library(raster)

temp <- merge(rasterFromXYZ(readRDS(file = paste0("/home/femeunier/Documents/projects/Congo.ED2/data/LC_","Congo",".RDS")) %>%
                select(lon,lat,LC)),
              rasterFromXYZ(readRDS(file = paste0("/home/femeunier/Documents/projects/Congo.ED2/data/LC_","Amazon",".RDS")) %>%
                select(lon,lat,LC)))

biovars <- aggregate(
  crop(brick("/home/femeunier/Documents/projects/CongoVOD/outputs/Biovars_2.5m_Pantropical.grd"),
       e <- extent(-85,52,-20,15)),
  c(6,6),method = "mean")

r.ref <- biovars[[1]]**0
r.ref[is.na(biovars[[1]])] <- NA
outfile <- writeRaster(r.ref,
                       filename=file.path("/home/femeunier/Documents/projects/Congo.ED2/data/raster.ref_Pantropical.grd"),
                       format="raster",
                       overwrite=TRUE,
                       options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

plot(raster("/home/femeunier/Documents/projects/Congo.ED2/data/raster.ref_Pantropical.grd"))

system2("scp",paste("/home/femeunier/Documents/projects/Congo.ED2/data/raster.ref_Pantropical.gr*",
                    "hpc:/data/gent/vo/000/gvo00074/felicien/R"))
