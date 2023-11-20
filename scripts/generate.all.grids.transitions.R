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

transitions <- readRDS("/data/gent/vo/000/gvo00074/ED_common_data/land_use/LUH/transitions_tropics_20.RDS")
transitions <- readRDS("/data/gent/vo/000/gvo00074/ED_common_data/land_use/LUH/transitions_tropics_20_reclass.RDS")


CN <- colnames(transitions)
Var.names <- CN[!(CN %in% c("lat","lon","time"))]

all.grids <- data.frame()

overwrite = TRUE
for (cmodel in models){

  model.file <- paste0("./outputs/Trendy.",cmodel,".S2.CC.pantropical.v11.RDS")
  print(cmodel)

  if (!file.exists(model.file)){
    model.file <- paste0("./outputs/Trendy.",cmodel,".S3.CC.pantropical.v11.RDS")
  }

  if (!file.exists(model.file)) next()

  OPfile <- paste0("./data/grid.",cmodel,".transitions_reclass.RDS")

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

  test <- resample.df.all.col(bigdf = transitions %>%
                                mutate(model = "LUH"),

                              raster2resample = rasterFromXYZ((cdf %>%
                                                                 ungroup() %>%
                                                                 filter(year == year[1],
                                                                        month == month[1]) %>%
                                                                 dplyr::select(c("lat","lon",cvar)))[,c("lon","lat",cvar)]),
                              var.names = Var.names,
                              NULL)
  cgrid <- test %>%
    mutate(model = cmodel)

  saveRDS(cgrid,
          OPfile)
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/generate.all.grids.transitions.R hpc:/data/gent/vo/000/gvo00074/felicien/R/



