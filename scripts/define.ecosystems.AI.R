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
models <- c("CABLE-POP")
all.grids <- data.frame()

existing.models <- c()
for (cmodel in models){

  grid.file <- paste0("./data/grid.",cmodel,".JRA.historical.RDS")

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
    mutate(tmin = tmin - 273.15,
           tmax = tmax - 273.15,
           tmp = tmp - 273.15) %>%
    group_by(lat,lon) %>%
    mutate(ET0 = N/30*SPEI::hargreaves(tmin,tmax,lat = unique(lat),Pre = pre,na.rm = TRUE,
                                      verbose = FALSE)) %>%
    group_by(lon,lat,month) %>%
    summarise(Pmm = mean(pre),
              ET0 = mean(ET0),
              tmean = mean(tmp),
              N = mean(N),
              .groups = "keep") %>%
    mutate(diff = Pmm - ET0) %>%
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
              ET0 = mean(ET0),
              MAP = sum(Pmm),
              MAT = weighted.mean(tmean,N),
              .groups = "keep") %>%
    dplyr::select(lon,lat,MCWD,MAP,MAT,ET0)

  # cgrid.mean2 <- cgrid %>%
  #   group_by(lat,lon,year) %>%
  #   filter(!any(is.na(pre))) %>%
  #   ungroup() %>%
  #   mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  #   mutate(pre = pre*4*N) %>%
  #   group_by(lon,lat,month) %>%
  #   summarise(Pmm = mean(pre),
  #             tmean = mean(tmp),
  #             N = mean(N),
  #             .groups = "keep") %>%
  #   ungroup() %>%
  #   mutate(diff = Pmm - 3.33*N) %>%
  #   group_by(lon,lat) %>%
  #   mutate(wettest.month = which.max(diff)) %>%
  #   mutate(month.ord = 1 + (month - wettest.month)) %>%
  #   mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
  #                                TRUE ~ month.ord)) %>%
  #   arrange(lat,lon,month.ord) %>%
  #   mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
  #                          TRUE ~ NA_real_)) %>%
  #   mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  #   arrange(lat,lon,month) %>%
  #   summarise(MCWD = min(CWD),
  #             MAP = sum(Pmm),
  #             MAT = weighted.mean(tmean,N),
  #             .groups = "keep") %>%
  #   dplyr::select(lon,lat,MCWD,MAP,MAT)

  all.grids <- bind_rows(all.grids,
                         cgrid.mean %>%
                           mutate(model = cmodel))

  existing.models <- c(existing.models,
                       cmodel)
}

all.grids <- all.grids %>%
  mutate(AI = MAP/pmin(-1e-6,MCWD)) %>%
  mutate(biome = case_when(MAP > 1700 & AI < -3.8 ~ "Humid_large",
                           AI < -3.8 ~ "Humid_low",
                           AI < -1.8 & AI >= -3.8 ~ "Humid_seasonal",
                           AI < -1 & AI >= -1.8 ~ "Dry_subhumid",
                           AI < -0.25 & AI >= -1 ~ "Semiarid",
                           AI < -0.05 & AI >= -0.25 ~ "Arid",
                           AI < 0 & AI >= -0.05 ~ "Hyperarid",
                           TRUE ~ NA_character_))


saveRDS(all.grids,
        "./outputs/biome.JRA.1901.2023.AI.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/define.ecosystems.AI.R hpc:/data/gent/vo/000/gvo00074/felicien/R
