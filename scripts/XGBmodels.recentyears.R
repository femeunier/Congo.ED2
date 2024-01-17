rm(list = ls())

library(dplyr)
library(stringr)
library(zoo)
library(raster)
library(TrENDY.analyses)

models <- TrENDY.analyses::get.model.names.TRENDY()
models <- "CABLE-POP"

all.vars <- c("gpp","npp","nep")
years <- 2017:2023
Continents <- c("America","Africa","Australasia")
Biomes <- c("Humid_large","Humid_low","Humid_seasonal","Dry_subhumid")

# CO2
dataC02 <- read.table("./data/global_co2_ann_1700_2022.txt",
                      stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)

dataC02.all <- data.frame(year = sort(unique(c(dataC02$year,2020:2023)))) %>%
  arrange(year) %>%
  left_join(dataC02,
            by = c("year")) %>%
  mutate(CO2 = na.spline(CO2,method = "natural"))

plot(dataC02.all$year,
     dataC02.all$CO2,xlim = c(2010,2022))

biomes <- readRDS("./outputs/biome.CRUJRA.1901.2022.AI.RDS") %>%
  dplyr::select(lon,lat,biome,model)

# options(warn = 2)

# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot(data = biomes %>%
#          filter(model == "ORCHIDEE")) +
#   geom_raster(aes(x=lon,
#                   y = lat, fill = biome),alpha = 0.3) +
#   geom_sf(data = world,fill = NA) +
#   coord_sf(xlim = c(-150, 180), ylim = c(-25, 25), expand = FALSE) +
#   # facet_wrap(~ source) +
#   labs(x = "",y = "")

df.all <- data.frame()

for (cmodel in models){

  print(cmodel)

  file <- paste0("./data/grid.",cmodel,".recentyears.RDS")

  if (!file.exists(file)){
    next()
  }

  climate <- readRDS(file)

  biome <- biomes %>%
    filter(model == cmodel) %>%
    dplyr::select(-model)

  inputs <- climate %>%
    filter(year %in% years) %>%
    left_join(biome,
              by = c("lat","lon")) %>%
    mutate(continent = Congo.ED2::coord2continent(lon,lat)) %>%
    mutate(xgb.model = paste0("XGB.fit.reclass.",model,".S2.",continent,".",biome)) %>%
    filter(continent %in% Continents,
           biome %in% Biomes)  %>%
    left_join(dataC02.all,
              by = c("year")) %>%
    mutate(model.lat.lon = paste0(model,".",round(lat,digits = 2),".",round(lon,digits = 2)))

  xgb.models <- expand.grid(model = unique(inputs$xgb.model),
                            var = all.vars) %>%
    mutate(xgb.model = paste0(model,".",var,".RDS"))

  for (i in seq(1,nrow(xgb.models))){

    cxgb.model <- as.character(xgb.models[["xgb.model"]][i]) ; cvar <- as.character(xgb.models[["var"]][i]) ; cxgb.model.novar <- xgb.models[["model"]][i]

    print(paste0("- ",cxgb.model.novar," - ",cvar))

    cfile <- file.path("/data/gent/vo/000/gvo00074/felicien/R/outputs/",
                       paste0(cxgb.model))

    if (!file.exists(cfile)) next()

    cXGBmodel <- readRDS(cfile)

    all.data <- bind_rows(
      bind_cols(cXGBmodel$test.data,
                !!as.character(cvar) := cXGBmodel$test.labels),

      bind_cols(cXGBmodel$validation.data,
                !!as.character(cvar) := cXGBmodel$validation.labels),

      bind_cols(cXGBmodel$training.data,
                !!as.character(cvar) := cXGBmodel$labels)) %>%
      filter(year %in% years) %>%
      mutate(model.lat.lon = paste0(cmodel,".",round(lat,digits = 2),".",round(lon,digits = 2))) %>%
      filter(model.lat.lon %in% inputs[["model.lat.lon"]])

    cdf <- inputs %>%
      filter(xgb.model == cxgb.model.novar) %>%
      filter(model.lat.lon %in% all.data[["model.lat.lon"]]) %>%
      dplyr::select(-c("biome","model","continent","xgb.model","model.lat.lon"))


    predicted <- predict(cXGBmodel,
                         cdf[,cXGBmodel$finalModel$feature_names],)


    all.predicted <- inputs %>%
      filter(xgb.model == cxgb.model.novar) %>%
      filter(model.lat.lon %in% all.data[["model.lat.lon"]]) %>%
      dplyr::select(c("biome","model","continent","xgb.model","year",
                      "model.lat.lon",
                      "month","lat","lon")) %>%
      mutate(var = cvar,
             pred = predicted)

    cpredicted <- all.predicted %>%
      left_join(all.data %>%
                  dplyr::select(model.lat.lon,year,month,cvar) %>%
                  rename(obs := !!cvar),
                by = c("model.lat.lon","year","month"))

    df.all <- bind_rows(df.all,
                        cpredicted)
  }
}

saveRDS(df.all,
        paste0("./outputs/predictions.XGB.recent.years.reclass.RDS"))


# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/XGBmodels.recentyears.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

