rm(list = ls())

# IFL <- read_sf(dsn = "/home/femeunier/Documents/projects/Congo.ED2/data/IFL/",
#                       layer = "ifl_2020")
# IFL$type = 1
# N = 1
# r <- raster(ncol=720*N, nrow=360*N)
# extent(r) <- extent(-180,
#                     180,
#                     -90,90)
#
# rp <- rasterize(IFL, r,
#                 'type',
#                 fun = mean)
# plot(rp)
#
# rp.df <- as.data.frame(rp,xy = TRUE) %>%
#   rename(lon = x,
#          lat = y,
#          is.undisturbed = layer) %>%
#   mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
#                                            is.undisturbed < 0.5 ~ 0,
#                                            is.undisturbed >= 0.5 ~1))
#
# saveRDS(rp.df,
#         "./outputs/ILF2020.df")


ILF.df  <- readRDS("./outputs/ILF2020.df")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
Congo <- as_Spatial(Congo.shp)
ggplot() +

  geom_raster(data = ILF.df,
              aes(x = lon, y = lat,
                  fill = as.factor(is.undisturbed.factor)), alpha = 1,
              linewidth = 0.5,
              show.legend = FALSE) +

  geom_sf(data = world,
          fill = NA) +
  geom_sf(data = Amazon.shp,fill = NA, color = "red", fill = NA) +

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

################################################################################

climate <- readRDS("./outputs/biome.JRA.1901.2023.AI.RDS")
models <- sort(unique(climate$model))

all.coord <- all.coord2 <-
  data.frame()


for (cmodel in models){

  print(cmodel)

  cclimate <- climate %>%
    filter(model == cmodel)

  grid <- rasterFromXYZ((cclimate %>%
                   ungroup() %>%
                   dplyr::select(c("lat","lon","MAP")))[,c("lon","lat","MAP")])

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

  test <- cclimate %>%
    filter(lon <= -30, lon >= -90,
           lat <= 10, lat >= -25)

  test2 <-cclimate %>%
    filter(model == cmodel) %>%
    filter(lon >= 4, lon <= 40,
           lat <= 12, lat >= -15)

  sp <- SpatialPoints(test[,c("lon","lat")])
  sp2 <- SpatialPoints(test2[,c("lon","lat")])

  out = as.data.frame(gIntersection(sp,Amazon))
  out2 = as.data.frame(gIntersection(sp2,Congo))

  all.coord <- bind_rows(all.coord,
                         data.frame(model = cmodel,
                                    lon = out$x,
                                    lat = out$y) %>%
    left_join(cILF,
              by = c("lat","lon")) %>%
      filter(is.undisturbed.factor == 1) %>%
      mutate(model.lon.lat =
               paste0(cmodel,".",lon,".",lat)))

  all.coord2 <- bind_rows(all.coord2,
                          data.frame(model = cmodel,
                                     lon = out2$x,
                                     lat = out2$y) %>%
                            left_join(cILF,
                                      by = c("lat","lon")) %>%
                            filter(is.undisturbed.factor == 1) %>%
                            mutate(model.lon.lat =
                                     paste0(cmodel,".",lon,".",lat)))
}

ggplot() +

  geom_raster(data = ILF.df,
              aes(x = lon, y = lat,
                  fill = as.factor(is.undisturbed.factor)), alpha = 1,
              linewidth = 0.5,
              show.legend = FALSE) +
  geom_point(data = all.coord2 %>%
               filter(model == "ORCHIDEE"),
             aes(x = lon, y = lat), shape = "+") +

  geom_point(data = all.coord %>%
               filter(model == "ORCHIDEE"),
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
        "./outputs/Amazon.coord.ILF.RDS")
saveRDS(all.coord2,
        "./outputs/Congo.coord.ILF.RDS")


system2("scp",
        c("./outputs/Amazon.coord.ILF.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

system2("scp",
        c("./outputs/Congo.coord.ILF.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))

