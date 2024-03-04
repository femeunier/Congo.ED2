rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)


coord.list <-  readRDS("./outputs/Coord.ILF.ERA5.RDS") %>%
  # filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat,digits = 2)))

climate <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
  filter(year >= 1994) %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat,digits = 2))) %>%
  filter(lon.lat %in% coord.list[["lon.lat"]])

climate.long <- climate %>%
  dplyr::select(year,month,lat,lon,pre,tmp) %>%
  pivot_longer(cols = c(pre,tmp),
               values_to = "value",
               names_to = "variable") %>%
  mutate(time = year + (month - 1/2)/12)


climate.select <- climate.long %>%
  group_by(variable,lon,lat) %>%
  mutate(slope = coef(lm(value ~ time))[2],
         intercept = coef(lm(value ~ time))[1]) %>%

  mutate(mean.obs = mean(value,na.rm = TRUE)) %>%

  mutate(detrended = value - mean.obs) %>%
  group_by(variable,month,lat,lon) %>%
  mutate(mean.month = mean(detrended,na.rm = TRUE)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(variable,month,lat,lon) %>%
  mutate(anomaly.m = anomaly/sd(anomaly,na.rm = TRUE))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")


ggplot(data = climate.select %>%
         filter(variable == "pre",
                year == 2023,
                month == 10)) +
  geom_raster(aes(x=lon,y = lat,
                  fill = anomaly.m),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, -30), ylim = c(-25, 10), expand = FALSE) +
  scale_fill_gradient2(limits = c(-3,3),
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkblue",mid = "grey",high = "darkred") +
  labs(x = "",y = "") +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


ggplot(data = climate.select %>%
         filter(variable == "tmp",
                year == 2023,
                month == 10)) +
  geom_raster(aes(x=lon,y = lat,
                  fill = anomaly.m),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, -30), ylim = c(-25, 10), expand = FALSE) +
  scale_fill_gradient2(limits = c(-3,3),
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkblue",mid = "grey",high = "darkred") +
  labs(x = "",y = "") +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())



ggplot(data = climate.select %>%
         filter(year == 2023,
                month == 10),
       aes(x = anomaly.m,
           y = interaction(variable),
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 1,
                               scale = 1.5) +
  scale_fill_gradient2(limits = c(-10,5)*1,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkblue",mid = "grey",high = "darkred") +
  geom_vline(xintercept = 0,linetype = 2) +
  facet_wrap(~ year, nrow = 1) +
  scale_x_continuous(limits = c(-3,3)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid = element_blank())
