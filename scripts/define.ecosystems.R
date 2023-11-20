rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(lattice)
library(rgdal)
library(YGB)
library(caret)
library(TrENDY.analyses)
library(stringr)
library(randomForest)
library(ggpointdensity)
library(lubridate)
library(xgboost)
library(zoo)

overwrite = FALSE
models <- TrENDY.analyses::get.model.names.TRENDY()
all.grids <- data.frame()

existing.models <- c()
for (cmodel in models){

  grid.file <- paste0("./data/grid.",cmodel,".RDS")

  if (!all(file.exists(grid.file))){
    next()
  }

  print(cmodel)
  cgrid <- readRDS(grid.file)

  cgrid.mean <- cgrid %>%
    group_by(lat,lon,year) %>%
    filter(!any(is.na(pre))) %>%
    ungroup() %>%
    mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
    mutate(pre = pre*4*N) %>%
    group_by(lon,lat,month) %>%
    summarise(Pmm = mean(pre),
              tmean = mean(tmp),
              N = mean(N),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(diff = Pmm - 3.33*N) %>%
    group_by(lon,lat) %>%
    mutate(wettest.month = which.max(diff)) %>%
    mutate(month.ord = 1 + (month - wettest.month)) %>%
    mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                                 TRUE ~ month.ord)) %>%
    arrange(lat,lon,month.ord) %>%
    mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                           TRUE ~ NA_real_)) %>%
    mutate(CWD = calc.CWD(diff,CWD[1])) %>%
    arrange(lat,lon,month) %>%
    summarise(MCWD = min(CWD),
              MAP = sum(Pmm),
              MAT = weighted.mean(tmean,N),
              .groups = "keep") %>%
    dplyr::select(lon,lat,MCWD,MAP,MAT)

  all.grids <- bind_rows(all.grids,
                         cgrid.mean %>%
                           mutate(model = cmodel))

  existing.models <- c(existing.models,
                       cmodel)
}

models <- existing.models

coords <- cbind(id = 1:nrow(all.grids),
                MAT = all.grids$MAT - 273.15,
                MAP = all.grids$MAP/10)

biome.id <- (plotbiomes::Whittaker_biomes) %>%
  dplyr::select(biome_id,biome) %>%
  distinct() %>%
  mutate(id = 1:9)
polys <- geometry(plotbiomes::Whittaker_biomes_poly)

sp <- SpatialPoints(coords[,c(2,3)])
e <- as.data.frame(raster::extract(polys,sp)) %>%
  rename(point.id = id.y,
         id = id.x) %>%
  left_join(biome.id %>%
              dplyr::select(-biome_id),
            by = "id")

# transform to sf objects
psf   <- sf::st_as_sf(sp) %>%
  dplyr::mutate(ID_point = 1:dim(.)[1])
polsf <- sf::st_as_sf(plotbiomes::Whittaker_biomes_poly)

# remove points inside polygons
in_points  <- lengths(sf::st_within(psf,polsf))
out_points <- psf[in_points == 0, ]

# find nearest poly
nearest <- polsf[sf::st_nearest_feature(out_points, polsf) ,]  %>%
  dplyr::mutate(id_point = out_points$ID)

all.grids["biome"] <- e$biome
all.grids["type"] <- "Interpolation"

all.grids[["biome"]][nearest$id_point] <- nearest$biome
all.grids[["type"]][nearest$id_point] <- "Extrapolation"

all.grids %>%
  group_by(model,type,biome) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  arrange(desc(N))

saveRDS(all.grids,
        "./outputs/biome.CRUJRA.1901.2019.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/define.ecosystems.R hpc:/data/gent/vo/000/gvo00074/felicien/R
