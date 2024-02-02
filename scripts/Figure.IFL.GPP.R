rm(list = ls())

library(sf)
library(ggthemes)
library(ggridges)

coord <- readRDS("./outputs/Amazon.coord.GPP.products.ILF.RDS") %>%
  filter(model %in% c("SIF","SIF2","VOD")) %>%
  mutate(lat.lon = paste0(lat,".",lon))

################################################################################
# RS

SIF.data <- readRDS("./outputs/GPP.products.Amazon.ILF.RDS") %>%
  mutate(basin = "Amazon") %>%
  mutate(lat.lon = paste0(lat,".",lon))

SIF.data.sum <- SIF.data %>%
  filter(year %in% (2001:2019)) %>%
  filter(model %in% c("SIF","SIF2","VOD")) %>%
  filter(lat.lon %in% coord[["lat.lon"]]) %>%
  group_by(lat,lon,model) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(lat,lon) %>%
  summarise(value.MEM = mean(value.m,na.rm = TRUE),
            .groups = "keep")

################################################################################
# Trendy

ILF.df  <- readRDS("./outputs/Amazon.coord.ILF.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),
                          ".",
                          round(lat,digits = 2)))

Trendy <- readRDS("./outputs/Trendy.data.rspld.RDS") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),
                                ".",
                                round(lat,digits = 2))) %>%
  filter(lon.lat %in% ILF.df[["lon.lat"]])

unique(Trendy$model)

Trendy.sum <- Trendy %>%
  filter(year %in% (2001:2019)) %>%
  group_by(lat,lon,model) %>%
  summarise(value.m = mean(obs,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(lat,lon) %>%
  summarise(value.MEM = mean(value.m,na.rm = TRUE),
            .groups = "keep")


combined <- bind_rows(SIF.data.sum %>% mutate(source = "RS"),
                      Trendy.sum %>% mutate(source = "Trendy"))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

ggplot() +

  geom_tile(data = ILF.df,
            aes(x = lon, y = lat), alpha = 0.5, fill = NA,
            linewidth = 0.5, color = "black",
            show.legend = FALSE) +

  geom_tile(data = combined,
            aes(x = lon, y = lat,
                fill = value.MEM), alpha = 1,
            show.legend = TRUE) +

  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-85, -30),
           ylim = c(-25, 10)) +
  labs(x = "",y = "",fill = "") +
  facet_wrap(~ source) +
  theme_map() +
  guides(fill = "none") +
  scale_fill_gradient(limits = c(2.5,3.5),oob = scales::squish,
                      low = "white", high = "darkgreen",
                      breaks = c(2.5,3.5)) +
  theme(text = element_text(size = 20),
        strip.text = element_blank(),
        legend.position = "top")


ggplot(data = combined,
       aes(y = source,
           x = value.MEM,
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 0.5,
                               scale = 0.9) +
  scale_fill_gradient(limits = c(2.5,3.5),oob = scales::squish,
                      low = "white", high = "darkgreen",
                      breaks = c(2.5,3.5)) +
  scale_x_continuous(limits = c(2.5,3.5),
                     breaks = c(2.5,3,3.5)) +
  # scale_y_discrete(limits = factor(c(0,6)) +
  # facet_wrap(~ year) +
  labs(x = "",y = "") +
  theme_minimal() +
  theme(text = element_text(size = 20),panel.grid = element_blank())


