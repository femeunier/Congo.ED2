rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)
library(raster)
library(TrENDY.analyses)
library(Congo.ED2)
library(ggplot2)
library(tidyr)

df.all <- bind_rows(list(readRDS("./data/GPP/monthly/all.df.GPP2.RDS") %>%
                           mutate(model = "Madani"),

                         readRDS("./data/GPP/monthly/all.df.GPP.RDS") %>%
                           mutate(model = "Zhang"),

                         readRDS("./data/GPP/monthly/df.all.GPP3.RDS") %>%
                           mutate(model = "Fluxcom"),

                         readRDS("./data/GPP/monthly/all.df.GPP.MODIS.RDS") %>%
                           mutate(model = "MODIS") ,

                         readRDS("./data/GPP/monthly/SIF.GPP.RDS") %>%
                           mutate(model = "SIF") %>%
                           rename(daily.GPP = value),

                         readRDS("./data/GPP/monthly/NIR.GPP.RDS") %>%
                           mutate(model = "NIR") %>%
                           rename(daily.GPP = value),

                         readRDS("./data/GPP/monthly/SIF.GPP2.RDS") %>%
                           mutate(model = "SIF2") %>%
                           rename(daily.GPP = value),

                         readRDS("./data/GPP/monthly/VOD.GPP.RDS") %>%
                           mutate(model = "VOD") %>%
                           rename(daily.GPP = value))) %>%
mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
  dplyr::select(lon,lat,year,month,model,daily.GPP) %>%
  filter(year >= 1958)


Amazon.coord <- readRDS("./outputs/Amazon.coord.GPP.products.RDS")
Congo.coord <- readRDS("./outputs/Congo.coord.GPP.products.RDS")
Amazon.coord.ILF <- readRDS("./outputs/Amazon.coord.GPP.products.ILF.RDS")
Congo.coord.ILF <- readRDS("./outputs/Congo.coord.GPP.products.ILF.RDS")


biomes <- readRDS("./outputs/biome.CRUJRA.1901.2022.AI.RDS") %>%
  filter(model == unique(model)[14]) %>%
  mutate(continent = coord2continent(lon,lat))
craster <- rasterFromXYZ(biomes %>%
                           dplyr::select(c(lon,lat,MAP)))


biomes.continents <- biomes %>%
  group_by(biome,continent) %>%
  summarise(N = n(),
            .groups = "keep")

df.all.rspld <-
  GPP.product.Amazon <- GPP.product.Congo <-
  GPP.product.Amazon.sum <- GPP.product.Congo.sum <-
  GPP.product.Amazon.ILF <- GPP.product.Congo.ILF <-
  GPP.product.Amazon.ILF.sum <- GPP.product.Congo.ILF.sum <-
  data.frame()

for (cyear in sort(unique(df.all$year))){
  print(cyear)
  cdf.all.rspld <- resample.df.all.col(bigdf = df.all %>%
                                         filter(year == cyear) %>%
                                         mutate(daily.GPP = daily.GPP/1000*365),
                                       raster2resample = craster,
                                       var.names = c("daily.GPP"),
                                       NULL)


  df.all.biomes <- cdf.all.rspld %>%
    mutate(value = daily.GPP) %>%
    dplyr::select(-daily.GPP) %>%
    ungroup() %>%
    mutate(continent = coord2continent(lon,lat)) %>%
    left_join(biomes %>%
                dplyr::select(lat,lon,biome),
              by = c("lat","lon")) %>%
    mutate(biome.continent = paste0(biome,".",continent))

  cdf.ts.year <- df.all.biomes %>%
    filter(lat <= 25) %>%
    group_by(biome.continent,model,continent,year,month,biome) %>%
    summarise(value.m = mean(value,na.rm = TRUE),
              .groups = "keep")

  cdf.Amazon <- df.all.biomes %>%
    mutate(model.lon.lat = paste0(model,".",lon,".",lat)) %>%
    filter(model.lon.lat %in% Amazon.coord[["model.lon.lat"]])

  cdf.Amazon.sum <- cdf.Amazon %>%
    group_by(model,year,month) %>%
    summarise(value.m = mean(value,na.rm = TRUE),
              .groups = "keep")

  cdf.Congo <- df.all.biomes %>%
    mutate(model.lon.lat = paste0(model,".",lon,".",lat)) %>%
    filter(model.lon.lat %in% Congo.coord[["model.lon.lat"]])

  cdf.Congo.sum <- cdf.Congo %>%
    group_by(model,year,month) %>%
    summarise(value.m = mean(value,na.rm = TRUE),
              .groups = "keep")


  cdf.Amazon.ILF <- df.all.biomes %>%
    mutate(model.lon.lat = paste0(model,".",lon,".",lat)) %>%
    filter(model.lon.lat %in% Amazon.coord.ILF[["model.lon.lat"]])

  cdf.Amazon.ILF.sum <- cdf.Amazon.ILF %>%
    group_by(model,year,month) %>%
    summarise(value.m = mean(value,na.rm = TRUE),
              .groups = "keep")

  cdf.Congo.ILF <- df.all.biomes %>%
    mutate(model.lon.lat = paste0(model,".",lon,".",lat)) %>%
    filter(model.lon.lat %in% Congo.coord.ILF[["model.lon.lat"]])

  cdf.Congo.ILF.sum <- cdf.Congo.ILF %>%
    group_by(model,year,month) %>%
    summarise(value.m = mean(value,na.rm = TRUE),
              .groups = "keep")

  GPP.product.Amazon <- bind_rows(GPP.product.Amazon,
                                  cdf.Amazon)
  GPP.product.Congo <- bind_rows(GPP.product.Congo,
                                 cdf.Congo)
  GPP.product.Amazon.sum <- bind_rows(GPP.product.Amazon.sum,
                                  cdf.Amazon.sum)
  GPP.product.Congo.sum <- bind_rows(GPP.product.Congo.sum,
                                 cdf.Congo.sum)

  GPP.product.Amazon.ILF <- bind_rows(GPP.product.Amazon.ILF,
                                      cdf.Amazon.ILF)
  GPP.product.Congo.ILF <- bind_rows(GPP.product.Congo.ILF,
                                     cdf.Congo.ILF)

  GPP.product.Amazon.ILF.sum <- bind_rows(GPP.product.Amazon.ILF.sum,
                                          cdf.Amazon.ILF.sum)
  GPP.product.Congo.ILF.sum <- bind_rows(GPP.product.Congo.ILF.sum,
                                         cdf.Congo.ILF.sum)

  df.all.rspld <- bind_rows(df.all.rspld,
                            cdf.ts.year)

}

saveRDS(df.all.rspld,
        "./outputs/GPP.products.RDS")

saveRDS(GPP.product.Amazon,
        "./outputs/GPP.products.Amazon.RDS")
saveRDS(GPP.product.Congo,
        "./outputs/GPP.products.Congo.RDS")
saveRDS(GPP.product.Amazon.sum,
        "./outputs/GPP.products.Amazon.sum.RDS")
saveRDS(GPP.product.Congo.sum,
        "./outputs/GPP.products.Congo.sum.RDS")

saveRDS(GPP.product.Amazon.ILF,
        "./outputs/GPP.products.Amazon.ILF.RDS")
saveRDS(GPP.product.Congo.ILF,
        "./outputs/GPP.products.Congo.ILF.RDS")
saveRDS(GPP.product.Amazon.ILF.sum,
        "./outputs/GPP.products.Amazon.ILF.sum.RDS")
saveRDS(GPP.product.Congo.ILF.sum,
        "./outputs/GPP.products.Congo.ILF.sum.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/resample.GPP.products.R hpc:/data/gent/vo/000/gvo00074/felicien/R/



