rm(list = ls())

library(rgeos)

df.all <- bind_rows(list(readRDS("./data/GPP/monthly/all.df.GPP2.RDS") %>%
                           mutate(model = "Madani") %>%
                           ungroup() %>%
                           filter(year == min(year)),
                         readRDS("./data/GPP/monthly/all.df.GPP.RDS") %>%
                           mutate(model = "Zhang") %>%
                           ungroup() %>%
                           filter(year == min(year)) %>%
                           filter(month == min(month)),
                         readRDS("./data/GPP/monthly/df.all.GPP3.RDS") %>%
                           mutate(model = "Fluxcom") %>%
                           ungroup() %>%
                           filter(year == min(year)) %>%
                           filter(month == min(month)),
                         readRDS("./data/GPP/monthly/all.df.GPP.MODIS.RDS") %>%
                           mutate(model = "MODIS") %>%
                           ungroup() %>%
                           filter(year == min(year))  %>%
                           filter(month == min(month)),
                         readRDS("./data/GPP/monthly/SIF.GPP.RDS") %>%
                           mutate(model = "SIF") %>%
                           rename(daily.GPP = value) %>%
                           ungroup() %>%
                           filter(year == min(year))  %>%
                           filter(month == min(month)),
                         readRDS("./data/GPP/monthly/VOD.GPP.RDS") %>%
                           mutate(model = "VOD") %>%
                           rename(daily.GPP = value) %>%
                           ungroup() %>%
                           filter(year == min(year))  %>%
                           filter(month == min(month))))

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
Congo <- as_Spatial(Congo.shp)

all.coord <- all.coord2 <-
  data.frame()

for (cmodel in unique(df.all$model)){

  print(cmodel)
  test <- df.all %>%
    filter(model == cmodel) %>%
    filter(lon <= -30, lon >= -90,
           lat <= 10, lat >= -25)

  test2 <-df.all %>%
    filter(model == cmodel) %>%
    filter(lon >= 4, lon <= 40,
           lat <= 12, lat >= -15)

  sp <- SpatialPoints(test[,c("lon","lat")])
  sp2 <- SpatialPoints(test2[,c("lon","lat")])
  # e <- as.data.frame(raster::extract(Amazon,sp)) %>%
  #   filter(!is.na(area))
  #

  out = as.data.frame(gIntersection(sp,Amazon))
  out2 = as.data.frame(gIntersection(sp2,Congo))

  all.coord <- bind_rows(all.coord,
                         data.frame(model.lon.lat =
                                      paste0(cmodel,".",out$x,".",out$y)))
  all.coord2 <- bind_rows(all.coord2,
                          data.frame(model.lon.lat =
                                      paste0(cmodel,".",out2$x,".",out2$y)))
}


saveRDS(all.coord,
        "./outputs/Amazon.coord.GPP.products.RDS")
saveRDS(all.coord2,
        "./outputs/Congo.coord.GPP.products.RDS")


system2("scp",
        c("./outputs/Amazon.coord.GPP.products.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

system2("scp",
        c("./outputs/Congo.coord.GPP.products.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

################################################################################

rm(list = ls())

library(rgeos)

climate <- readRDS("./outputs/biome.JRA.1901.2023.AI.RDS")
models <- sort(unique(climate$model))

Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
Congo <- as_Spatial(Congo.shp)

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

all.coord <- all.coord2 <- data.frame()
for (cmodel in models){

  cclimate <- climate %>%
    filter(model == cmodel)

  test <- cclimate %>%
    filter(lon >= 4, lon <= 40,
           lat <= 12, lat >= -15)
  test2 <- cclimate %>%
    filter(lon <= -30, lon >= -90,
           lat <= 10, lat >= -25)

  sp <- SpatialPoints(test[,c("lon","lat")])
  sp2 <- SpatialPoints(test2[,c("lon","lat")])

  out <- as.data.frame(gIntersection(sp,Congo))
  out2 <- as.data.frame(gIntersection(sp2,Amazon))

  all.coord <- bind_rows(all.coord,
                         data.frame(model.lon.lat =
                                      paste0(cmodel,".",out$x,".",out$y)))

  all.coord2 <- bind_rows(all.coord2,
                         data.frame(model.lon.lat =
                                      paste0(cmodel,".",out2$x,".",out2$y)))


}

saveRDS(all.coord,
        "./outputs/Congo.coord.RDS")
saveRDS(all.coord2,
        "./outputs/Amazon.coord.RDS")

system2("scp",
        c("./outputs/Congo.coord.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

system2("scp",
        c("./outputs/Amazon.coord.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
