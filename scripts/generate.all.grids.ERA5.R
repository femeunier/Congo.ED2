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

# options(warn = 0)

Tropics.sum <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS")
CN <- colnames(Tropics.sum)
Var.names <- CN[!(CN %in% c("lat","lon","year","month"))]

all.grids <- data.frame()

overwrite = TRUE
for (cmodel in models){

  model.file <- paste0("./outputs/Trendy.",cmodel,".S2.CC.pantropical.v11.RDS")

  if (!file.exists(model.file)){
    model.file <- paste0("./outputs/Trendy.",cmodel,".S3.CC.pantropical.v11.RDS")
  }

  if (!file.exists(model.file)) next()

  OPfile <- paste0("./data/grid.",cmodel,".ERA5.RDS")

  if (file.exists(OPfile) & !overwrite){next()}

  cdata <- readRDS(model.file) %>%
    dplyr::select(
      where(
        ~!all(is.na(.x))))

  CN <- colnames(cdata)
  cn.exists <- which(c("gpp","npp","nep") %in% CN)
  if (length(cn.exists) == 0) next()

  cvar <- as.character(c("gpp","npp","nep")[cn.exists[1]])

  Biomass.Trendy <- cdata %>%
    mutate(model = cmodel) %>%
    na.omit() %>%
    mutate(Continent = Congo.ED2::coord2continent(lon,lon)) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  cdf <- Biomass.Trendy

  cgrid <- data.frame()

  for (cyear in sort(unique(Tropics.sum$year))){

    print(paste0(cmodel," - ",cyear))

    test <- resample.df.all.col(bigdf = Tropics.sum %>%
                                  mutate(model = "CRUJRA") %>%
                                  filter(year == cyear),

                                raster2resample = rasterFromXYZ((cdf %>%
                                                                   ungroup() %>%
                                                                   filter(year == year[1],
                                                                          month == month[1]) %>%
                                                                   dplyr::select(c("lat","lon",cvar)))[,c("lon","lat",cvar)]),
                                var.names = Var.names,
                                NULL)

    cgrid <- bind_rows(cgrid,
                       test %>%
      mutate(model = cmodel))
  }


  saveRDS(cgrid,
          OPfile)
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/generate.all.grids.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
