rm(list = ls())

library(sf)
library(ggthemes)


coord <- readRDS("./outputs/Amazon.coord.GPP.products.ILF.RDS") %>%
  filter(model %in% c("SIF","SIF2","VOD","NIR")) %>%
  mutate(lat.lon = paste0(lat,".",lon))

SIF.data <- readRDS("./outputs/GPP.products.Amazon.ILF.RDS") %>%
  mutate(basin = "Amazon") %>%
  mutate(lat.lon = paste0(lat,".",lon))

SIF.data.sum <- SIF.data %>%
  filter(model %in% c("SIF","SIF2","VOD","NIR"),
         year %in% c(2001:2019)) %>%
  filter(lat.lon %in% coord[["lat.lon"]]) %>%
  group_by(lat,lon) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            .groups = "keep")


Trendy <- readRDS("./outputs/Trendy.data.rspld.pred.RDS") %>%
  filter(year >= 1994,
         year %in% c(2001:2019)) %>%
  mutate(lat.lon = paste0(lat,".",lon)) %>%
  filter(lat.lon %in% coord[["lat.lon"]])

Trendy.sum <- Trendy %>%
  group_by(lat,lon) %>%
  summarise(value.m = mean(pred,na.rm = TRUE),
            .groups = "keep")


ILF.df  <- readRDS("./outputs/Amazon.coord.ILF.RDS") %>%
  filter(model == "ORCHIDEE")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

Congo.shp <- read_sf(dsn = "/home/femeunier/Desktop/FWO/",
                     layer = "CongoBasin")
Congo <- as_Spatial(Congo.shp)


all <- bind_rows(Trendy.sum %>%
                   mutate(source = "Trendy"),
                 SIF.data.sum %>%
                   mutate(source = "RS"))
ggplot() +

  # geom_tile(data = all,
  #           aes(x = lon, y = lat), alpha = 0.5, fill = NA,
  #           linewidth = 0.1, color = "black",
  #           show.legend = FALSE) +

  geom_tile(data = all,
            aes(x = lon, y = lat,
                fill = value.m), alpha = 1, color = "black",
            linewidth = 0.1,na.rm = TRUE,
            show.legend = TRUE) +

  geom_sf(data = world,
          fill = NA, color = "black") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-90, -30),
           ylim = c(-25, 12)) +
  labs(x = "",y = "",fill = "") +
  facet_wrap(~ source) +
  theme_map() +
  scale_fill_gradient(limits = c(2.5,3.5),oob = scales::squish,
                      low = "white", high = "darkgreen",
                      breaks = c(2.5,3,3.5)) +
  theme(text = element_text(size = 20),
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = "none")

hist(SIF.data.sum$value.m)

ggplot(data = all,
       aes(y = source, x = value.m,
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 0.5,
                               scale = 0.9,
                               rel_min_height = 0.01) +
  scale_fill_gradient(limits = c(2.5,3.5),oob = scales::squish,
                       low = "white",high = "darkgreen") +
  scale_x_continuous(limits = c(2.5,3.5),
                     breaks = c(2.5,3,3.5)) +
  # scale_y_continuous(breaks = c()) +
  # facet_wrap(~ year) +
  # facet_wrap(~ source) +
  labs(x = "",y = "") +
  theme_minimal() +
  theme(text = element_text(size = 20),panel.grid = element_blank())

