rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(rgdal)
library(YGB)
library(TrENDY.analyses)
library(stringr)
library(randomForest)
library(ggpointdensity)

models <- TrENDY.analyses::get.model.names.TRENDY()

Tropics.sum <- readRDS("./outputs/monthly.climate.global.all.RDS")
CN <- colnames(Tropics.sum)
Var.names <- CN[!(CN %in% c("lat","lon","year","month"))]

all.grids <- data.frame()

overwrite = TRUE
for (cmodel in models){

  model.file <- paste0("./outputs/Trendy.",cmodel,".S2.global.v11.RDS")
  print(cmodel)

  if (!file.exists(model.file)){
    model.file <- paste0("./outputs/Trendy.",cmodel,".S2.CC.global.v11.RDS")
  }

  if (!file.exists(model.file)) next()

  OPfile <- paste0("./data/grid.",cmodel,".all.years_global.RDS")

  if (file.exists(OPfile) & !overwrite){next()}

  cdata <- readRDS(model.file) %>%
    dplyr::select(
      where(
        ~!all(is.na(.x))))

  CN <- colnames(cdata)
  cn.exists <- which(c("gpp","npp","rh") %in% CN)
  if (length(cn.exists) == 0) next()

  cvar <- c("gpp","npp","rh")[cn.exists[1]]

  Biomass.Trendy <- cdata %>%
    mutate(model = cmodel) %>%
    na.omit() %>%
    mutate(Continent = Congo.ED2::coord2continent(lon,lon)) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

    cdf <- Biomass.Trendy


    craster <- tryCatch(rasterFromXYZ((cdf %>%
                                         ungroup() %>%
                                         filter(year == year[1],
                                                month == month[1]) %>%
                                         dplyr::select(c("lat","lon",cvar)))[,c("lon","lat",cvar)]),
                                         error = function(e) NULL)

    if (is.null(craster)){

      craster <- tryCatch(raster(suppressWarnings(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                                                     data = cdf[cvar],
                                                                     tolerance = 0.0001))),
                      error = function(e) NULL)

    }

    test <- resample.df.all.col(bigdf = Tropics.sum %>%
                                  mutate(model = "CRUJRA"),

                                raster2resample = craster,
                                var.names = Var.names,
                                NULL)
  cgrid <- test %>%
    mutate(model = cmodel)

  saveRDS(cgrid,
          OPfile)
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/generate.all.grids.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
