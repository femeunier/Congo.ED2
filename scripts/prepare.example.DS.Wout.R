rm(list = ls())

library(dplyr)
library(stringr)
library(zoo)

biome.file <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2019.AI.RDS"
model <- "ORCHIDEE"
scenario <- "S2"
vars <- c('nep')
biome.names <- "Humid_low"
continents <- "America"

biomes <- readRDS(biome.file) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

grid.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",model,".RDS")
model.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",model,".",scenario,".CC.pantropical.v11.RDS")

if (scenario == "S3"){
  grid.file.transition <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",model,".",transition.suffix,".RDS")
  all.files <- c(model.file,grid.file,grid.file.transition)
} else {
  all.files <- c(model.file,grid.file)
}

if (!all(file.exists(all.files))){
  stop(paste("Not all files exist, check:",
             all.files[which(!file.exists(all.files))]))
}

all.models <- readRDS(model.file) %>%
  dplyr::select(-starts_with("time.unit"))

all.grids <- readRDS(grid.file)

if (scenario == "S3"){
  all.grids.transitions <- readRDS(grid.file.transition) %>%
    rename(year = time)
}

CC.Trendy <- all.models %>%
  mutate(continent = Congo.ED2::coord2continent(lon,lon)) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
  filter(continent %in% continents)

# Merge

modelled.sink <- CC.Trendy %>%
  ungroup() %>%
  mutate(lat = round(lat,digits = 2),
         lon = round(lon,digits = 2))

# CO2
dataC02 <- read.table("/data/gent/vo/000/gvo00074/felicien/R/data/CO2_1700_2019_TRENDYv2020.txt",
                      stringsAsFactors = FALSE) %>%
  mutate(year = as.numeric(str_sub(V1,7,10)),
         CO2 = as.numeric(str_sub(V1,12,17))) %>%
  dplyr::select(year,CO2) %>%
  mutate(month = 1)

dataC02.all <- data.frame(year = rep(sort(unique(dataC02$year)),12)) %>%
  group_by(year) %>%
  mutate(month = 1:12) %>%
  arrange(year) %>%
  left_join(dataC02,
            by = c("year","month")) %>%
  mutate(CO2 = na.approx(CO2))


if (scenario == "S2"){

  sink.vs.climate <- modelled.sink %>%
    left_join(all.grids %>%
                dplyr::select(-starts_with("model")) %>%
                mutate(lat = round(lat,digits = 2),
                       lon = round(lon,digits = 2)),
              by = c("year","lat","lon","month")) %>%
    left_join(dataC02.all,
              by = c("year","month")) %>%
    ungroup()
} else if (scenario == "S3"){

  sink.vs.climate <- modelled.sink %>%
    left_join(all.grids %>%
                dplyr::select(-starts_with("model")) %>%
                mutate(lat = round(lat,digits = 2),
                       lon = round(lon,digits = 2)),
              by = c("year","lat","lon","month")) %>%

    left_join(all.grids.transitions %>%
                dplyr::select(-starts_with("model")) %>%
                mutate(lat = round(lat,digits = 2),
                       lon = round(lon,digits = 2)),
              by = c("year","lat","lon")) %>%

    left_join(dataC02.all,
              by = c("year","month")) %>%
    ungroup()

}

TF <- biomes %>%
  mutate(lat = round(lat,digits = 2),
         lon = round(lon,digits = 2)) %>%
  filter(biome %in% biome.names) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))


sink.vs.climate <- sink.vs.climate %>%
  mutate(gpp = gpp*86400*365,
         ra = ra*86400*365,
         rh = rh*86400*365,
         npp = npp*86400*365,
         nep = nep*86400*365)

final <- sink.vs.climate %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
  filter(model.lat.lon %in% TF[["model.lat.lon"]]) %>%
  dplyr::select(-c(time,continent,model.lat.lon,
                   npp,gpp,ra,rh))

saveRDS(final,
        "./outputs/Wout.Good.RDS")
