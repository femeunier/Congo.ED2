rm(list = ls())

library(rgeos)
library(raster)
library(ggplot2)
library(TrENDY.analyses)
library(tidyr)
library(dplyr)
library(sf)
library(ggthemes)


df.all <- bind_rows(list(readRDS("./data/GPP/monthly/all.df.GPP2.RDS") %>%
                           mutate(model = "Madani") %>%
                           ungroup() %>%
                           filter(year == min(year)),

                         readRDS("./data/GPP/monthly/NIR.GPP.RDS") %>%
                           mutate(model = "NIR") %>%
                           rename(daily.GPP = value) %>%
                           ungroup() %>%
                           filter(year == min(year),
                                  month == min(month)),

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

                         readRDS("./data/GPP/monthly/SIF.GPP2.RDS") %>%
                           mutate(model = "SIF2") %>%
                           rename(daily.GPP = value) %>%
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

ggplot(data = df.all %>%
         filter(model %in% c("SIF","NIR"))) +
  geom_density(aes(x = daily.GPP,
                   fill = model),
               alpha = 0.5) +
  theme_bw()

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
Congo <- as_Spatial(Congo.shp)

all.coord <- all.coord2 <-
  all.coord.ILF2 <- all.coord.ILF <-
  data.frame()

ILF.df  <- readRDS("./outputs/ILF2020.df")


for (cmodel in unique(df.all$model)){

  print(cmodel)

  cdf <- df.all %>%
    filter(model == cmodel)

  grid <- rasterFromXYZ((cdf %>%
                           ungroup() %>%
                           dplyr::select(c("lat","lon","daily.GPP")))[,c("lon","lat","daily.GPP")])

  test <- cdf %>%
    filter(lon <= -30, lon >= -90,
           lat <= 10, lat >= -25)

  test2 <-cdf %>%
    filter(model == cmodel) %>%
    filter(lon >= 4, lon <= 40,
           lat <= 12, lat >= -15)

  sp <- SpatialPoints(test[,c("lon","lat")])
  sp2 <- SpatialPoints(test2[,c("lon","lat")])

  out = as.data.frame(gIntersection(sp,Amazon))
  out2 = as.data.frame(gIntersection(sp2,Congo))

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


  all.coord <- bind_rows(all.coord,
                         data.frame(model = cmodel,
                                    lon = out$x,
                                    lat = out$y) %>%
                           mutate(model.lon.lat =
                                    paste0(cmodel,".",lon,".",lat)))

  all.coord2 <- bind_rows(all.coord2,
                          data.frame(model = cmodel,
                                     lon = out2$x,
                                     lat = out2$y) %>%
                            mutate(model.lon.lat =
                                     paste0(cmodel,".",lon,".",lat)))

  all.coord.ILF <- bind_rows(all.coord.ILF,
                             data.frame(model = cmodel,
                                        lon = round(out$x,digits = 2),
                                        lat = round(out$y,digits = 2)) %>%
                               left_join(cILF %>%
                                           mutate(lon = round(lon,digits = 2),
                                                  lat = round(lat,digits = 2)),
                                         by = c("lat","lon")) %>%
                               filter(is.undisturbed.factor == 1) %>%
                               mutate(model.lon.lat =
                                        paste0(cmodel,".",lon,".",lat)))

  all.coord.ILF2 <- bind_rows(all.coord.ILF2,
                              data.frame(model = cmodel,
                                         lon = round(out2$x,digits = 2),
                                         lat = round(out2$y,digits = 2)) %>%
                                left_join(cILF %>%
                                            mutate(lon = round(lon,digits = 2),
                                                   lat = round(lat,digits = 2)),
                                          by = c("lat","lon")) %>%
                                filter(is.undisturbed.factor == 1) %>%
                                mutate(model.lon.lat =
                                         paste0(cmodel,".",lon,".",lat)))


}


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = ILF.df,
              aes(x = lon, y = lat,
                  fill = as.factor(is.undisturbed.factor)), alpha = 1,
              linewidth = 0.5,
              show.legend = FALSE) +
  geom_point(data = all.coord.ILF2 %>%
               filter(model == "SIF2"),
             aes(x = lon, y = lat), shape = "+") +

  geom_point(data = all.coord.ILF %>%
               filter(model == "Madani"),
             aes(x = lon, y = lat), shape = "+", color = "red") +

  geom_sf(data = world,
          fill = NA) +
  geom_sf(data = Amazon.shp,fill = NA, color = "red", fill = NA) +
  geom_sf(data = Congo.shp,fill = NA, color = "red", fill = NA) +

  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-100, 150),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  # facet_wrap(~ model) +
  theme_map() +
  labs(fill = "") +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme(text = element_text(size = 20),
        legend.position = "top")


saveRDS(all.coord,
        "./outputs/Amazon.coord.GPP.products.RDS")
saveRDS(all.coord2,
        "./outputs/Congo.coord.GPP.products.RDS")

saveRDS(all.coord.ILF,
        "./outputs/Amazon.coord.GPP.products.ILF.RDS")
saveRDS(all.coord.ILF2,
        "./outputs/Congo.coord.GPP.products.ILF.RDS")


system2("scp",
        c("./outputs/Amazon.coord.GPP.products.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

system2("scp",
        c("./outputs/Congo.coord.GPP.products.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))


system2("scp",
        c("./outputs/Amazon.coord.GPP.products.ILF.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

system2("scp",
        c("./outputs/Congo.coord.GPP.products.ILF.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

################################################################################
# rm(list = ls())
#
# library(rgeos)
#
# climate <- readRDS("./outputs/biome.JRA.1901.2023.AI.RDS")
# models <- sort(unique(climate$model))
#
# Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
#                      layer = "CongoBasin")
# Congo <- as_Spatial(Congo.shp)
#
# Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
#                       layer = "amazon_sensulatissimo_gmm_v1")
# Amazon <- as_Spatial(Amazon.shp)
#
# all.coord <- all.coord2 <- data.frame()
# for (cmodel in models){
#
#   cclimate <- climate %>%
#     filter(model == cmodel)
#
#   test <- cclimate %>%
#     filter(lon >= 4, lon <= 40,
#            lat <= 12, lat >= -15)
#   test2 <- cclimate %>%
#     filter(lon <= -30, lon >= -90,
#            lat <= 10, lat >= -25)
#
#   sp <- SpatialPoints(test[,c("lon","lat")])
#   sp2 <- SpatialPoints(test2[,c("lon","lat")])
#
#   out <- as.data.frame(gIntersection(sp,Congo))
#   out2 <- as.data.frame(gIntersection(sp2,Amazon))
#
#   all.coord <- bind_rows(all.coord,
#                          data.frame(model.lon.lat =
#                                       paste0(cmodel,".",out$x,".",out$y)))
#
#   all.coord2 <- bind_rows(all.coord2,
#                          data.frame(model.lon.lat =
#                                       paste0(cmodel,".",out2$x,".",out2$y)))
#
#
# }
#
# saveRDS(all.coord,
#         "./outputs/Congo.coord.RDS")
# saveRDS(all.coord2,
#         "./outputs/Amazon.coord.RDS")
#
# system2("scp",
#         c("./outputs/Congo.coord.RDS",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
#
# system2("scp",
#         c("./outputs/Amazon.coord.RDS",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
