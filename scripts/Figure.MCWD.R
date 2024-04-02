rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dismo)
library(ggthemes)
library(tie)
library(YGB)
library(sf)

coord <- readRDS("./outputs/Coord.ILF.ERA5.RDS") %>%
  # filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

climate <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
  filter(year >= 1994)

climate.select <- climate %>%
  dplyr::select(year,month,lat,lon,pre) %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
  dplyr::filter(lon.lat %in% coord[["lon.lat"]]) %>%
  dplyr::select(-lon.lat)


climate.select.MCWD <- climate.select %>%
  filter(year <= 2023) %>%
  mutate(Ndays = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  mutate(Pmm = pre*Ndays*8,na.rm = TRUE) %>%
  group_by(year,lon,lat) %>%
  mutate(E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(year,month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(year,month) %>%
  mutate(MCWD = min(CWD)) %>%
  dplyr::select(year,lon,lat,MCWD) %>%
  distinct()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")

ggplot(data = climate.select.MCWD %>%
         filter(year == 2023)) +
  geom_raster(aes(x=lon, y = lat,
                  fill = MCWD),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, -30), ylim = c(-25, 10), expand = FALSE) +
  scale_fill_gradient(limits = c(-500,0),
                      oob = scales::squish,
                      low = "darkred",high = "grey") +
  labs(x = "",y = "") +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

climate.select.MCWD.Zscore <- climate.select.MCWD %>%
  group_by(lon,lat) %>%
  mutate(zscore = (MCWD - mean(MCWD))/sd(MCWD))


ggplot(data = climate.select.MCWD.Zscore %>%
         filter(year == 2023)) +
  geom_raster(aes(x=lon, y = lat,
                  fill = zscore),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, -30), ylim = c(-25, 10), expand = FALSE) +
  scale_fill_gradient(limits = c(-4,4),
                      oob = scales::squish,
                      low = "darkred",high = "darkgreen") +
  labs(x = "",y = "") +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())



