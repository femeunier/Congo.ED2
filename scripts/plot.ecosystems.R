rm(list = ls())

library(ggplot2)
library(dplyr)

system2("rsync",
       paste("-avz",
             "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2019.RDS",
             "/home/femeunier/Documents/projects/Congo.ED2/outputs/"))

system2("rsync",
        paste("-avz",
              "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2019.AI.RDS",
              "/home/femeunier/Documents/projects/Congo.ED2/outputs/"))

df.coord <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/biome.CRUJRA.1901.2019.AI.RDS")

ggplot() +

  geom_raster(data = df.coord %>%
                filter(model == "CABLE-POP"),
              aes(x = lon, y = lat,
                  fill = biome), alpha = 0.5,linewidth = 0.5) +
  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(-100, 175),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()


ggplot() +

  geom_raster(data = df.coord %>%
                filter(model == "CABLE-POP",
                       biome %in% c("Humid_large",
                                    "Humid_low",
                                    "Humid_seasonal",
                                    "Dry_subhumid")),
              aes(x = lon, y = lat,
                  fill = biome), alpha = 0.5,linewidth = 0.5) +
  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(-100, 175),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()
