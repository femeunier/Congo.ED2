rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(TrENDY.analyses)
library(raster)
library(Congo.ED2)

#ERA5 (3h)
 # dlwrf J/m2 and dswrf J/m2

#CRUJRA (6h)
# dlwrf W/m2, dswrf = J/m²

# system2("rsync",
#         c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/monthly.climate.pantropical.RDS",
#           "./outputs/"))
#
# system2("rsync",
#         c("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/monthly.climate.pantropical.ERA5.rspld.RDS",
#           "./outputs/"))

CRUJRA <- readRDS("./outputs/monthly.climate.pantropical.RDS")
ERA5.rspld <- readRDS("./outputs/monthly.climate.pantropical.ERA5.rspld.RDS")

# ERA5 <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS")
# #
# # ###############################################################################
# # # Resample
# #
# cdf <- CRUJRA %>%
#   ungroup() %>%
#   filter(month == 1, year == year[1]) %>%
#   dplyr::select(lat,lon,pre)
#
# ERA5.rspld <- resample.df.all.col(bigdf = ERA5 %>%
#                                     filter(year == 2000),
#
#                             raster2resample = rasterFromXYZ( (cdf)[,c("lon","lat","pre")]),
#                             var.names = c("tmp","tmin","tmax","pre","dswrf",
#                                           "dlwrf","spfh","VPD"),
#                             NULL)
#
# saveRDS(ERA5.rspld,
#         "./outputs/monthly.climate.pantropical.ERA5.rspld.RDS")

biomes <- readRDS("./outputs/biome.CRUJRA.1901.2022.AI.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  dplyr::select(lon,lat,biome)

years <- intersect(unique(CRUJRA$year),
                   unique(ERA5.rspld$year))
years <- 1960:1989

all.df <- bind_rows(CRUJRA %>%
                        filter(year %in% years) %>%
                        mutate(source = "CRU"),
                      ERA5.rspld %>%
                        filter(year %in% years) %>%
                        mutate(source = "ERA5")) %>%
  left_join(biomes,
            by = c("lon","lat")) %>%
  mutate(continent = coord2continent(lon,lat)) %>%
  filter(continent == "America",
         biome == "Humid_large")


all.df.long <- all.df %>%
  pivot_longer(cols = -c(lon,lat,year,month,source,
                         biome,continent),
               names_to = "variable",
               values_to = "value")

# all.df.long %>%
#   group_by(source,variable) %>%
#   summarise(m = mean(value,na.rm = TRUE),
#             med = median(value,na.rm = TRUE)) %>%
#   arrange(variable)

ggplot(data = all.df.long) +
  geom_density_ridges(aes(y = source, x = value,
                          fill = source)) +
  facet_wrap(~ variable,scales = "free") +
  theme_bw()

df.joint.map <- all.df %>%
  group_by(source,lat,lon) %>%
  summarise(pre = mean(pre)*4*30*12,
            dlwrf = mean(dlwrf),
            tmp = mean(tmp),
            .groups = "keep")

ggplot(data = df.joint.map) +
  geom_density_ridges(aes(y = source, x = pre,
                          fill = source)) +
  theme_bw()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df.joint.map) +
  geom_raster(aes(x=lon,
                  y = lat, fill = pre),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, 50), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(low =  "white",high = "darkblue",
                      limits = c(0,3000),oob = scales::squish,
                      na.value = NA) +
  facet_wrap(~ source) +
  labs(x = "",y = "")

ggplot(data = df.joint.map) +
  geom_raster(aes(x=lon,
                  y = lat, fill = dlwrf),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, 50), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(low =  "white",high = "red",
                      na.value = NA) +
  facet_wrap(~ source) +
  labs(x = "",y = "")

all.df.wide <- all.df.long %>%
  pivot_wider(names_from = source,
              values_from = value)



ggplot(all.df.wide,
       aes(x = CRU,
           y = ERA5)) +
  geom_hex() +

  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  scale_fill_viridis_c(trans = "log10") +
  # scale_fill_brewer() +
  facet_wrap(~ variable, scales = "free",
             nrow = 2) +
  stat_smooth(method = "lm", se = FALSE,
              color = "red", linetype = 2) +
  theme_bw() +
  guides(fill = "none")


# scp hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/monthly.climate.pantropical.ERA5.RDS ./outputs/

