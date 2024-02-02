rm(list = ls())

library(rgeos)
library(raster)
library(ggplot2)
library(TrENDY.analyses)
library(tidyr)
library(dplyr)
library(sf)
library(ggthemes)

ncfile <- "./data/ERA5_Amazon_2020.nc"

nc <- nc_open(ncfile)
lons <- ncvar_get(nc,"longitude")
lats <- ncvar_get(nc,"latitude")
ERA5.grid <- expand.grid(lat = lats,
                         lon = lons) %>%
  mutate(daily.GPP = 1)
nc_close(nc)

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

ILF.df  <- readRDS("./outputs/ILF2020.df")

grid <- rasterFromXYZ((ERA5.grid %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","daily.GPP")))[,c("lon","lat","daily.GPP")])

test <- ERA5.grid %>%
  filter(lon <= -30, lon >= -90,
         lat <= 10, lat >= -25)

sp <- SpatialPoints(test[,c("lon","lat")])
out <- as.data.frame(rgeos::gIntersection(sp,Amazon))

cILF <- resample.df.all.col(bigdf = ILF.df %>%
                              mutate(is.undisturbed = case_when(is.na(is.undisturbed) ~ 0,
                                                                TRUE ~ is.undisturbed)) %>%
                              dplyr::select(lat,lon,is.undisturbed) %>%
                              mutate(model = "ILF"),

                            raster2resample = grid,
                            var.names = c("is.undisturbed"),
                            res = 0.00001) %>%
  dplyr::select(lon,lat,is.undisturbed) %>%
  mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
                                           is.undisturbed < 0.5 ~ 0,
                                           is.undisturbed >= 0.5 ~1))


all.coord <- data.frame(lon = out$x,
                        lat = out$y) %>%
  mutate(lon.lat =
           paste0(lon,".",lat))


all.coord.ILF <- data.frame(lon = round(out$x,digits = 2),
                            lat = round(out$y,digits = 2)) %>%
  left_join(cILF %>%
              mutate(lon = round(lon,digits = 2),
                     lat = round(lat,digits = 2)),
            by = c("lat","lon")) %>%
  filter(is.undisturbed.factor == 1) %>%
  mutate(lon.lat =
           paste0(lon,".",lat))

saveRDS(all.coord.ILF,
        "./outputs/Coord.ILF.ERA5.RDS")


system2("scp",
        c("./outputs/Coord.ILF.ERA5.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
