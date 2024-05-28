rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(zoo)
library(ggthemes)

# era5 <- readRDS("~/Documents/data/monthly.climate.global.ERA5.RDS")
# era5.select <- era5 %>%
#   filter(lon <= 50,
#          lon >= -15,
#          abs(lat) <= 25) %>%
#   dplyr::select(lon,lat,month,year,pre,tmp)
#
# saveRDS(era5.select,
#         "~/Documents/data/monthly.climate.global.ERA5.Congo.RDS")

era5.select <- readRDS("~/Documents/data/monthly.climate.global.ERA5.Congo.RDS")


################################################################################
# Yangambi only

era5.select.filt <- era5.select %>%
  ungroup() %>%
  filter(year %in% 1941:2024,
         lon == 25,
         lat == 0.5) %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  group_by(year,month) %>%
  summarise(pre = mean(pre*N*8),
            tmp = mean(tmp - 273.15),
            .groups = "keep") %>%
  pivot_longer(cols = c(pre,tmp),
               names_to = "var")


era5.sum <- era5.select.filt %>%
  filter(year %in% 1970:2000) %>%
  group_by(var,month) %>%
  summarise(value = mean(value),
            .groups = "keep")

Window <- 12
era5.select.filt.rm <- era5.select.filt %>%
  group_by(var) %>%
  mutate(value.rm = case_when(var == "tmp" ~
           rollapply(value, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=FALSE, fill=NA, align="right"),
           var == "pre" ~
             rollapply(value, width=Window,
                       FUN=function(x) sum(x, na.rm=TRUE),
                       partial=FALSE, fill=NA, align="right")))

ggplot() +

  geom_line(data = era5.select.filt.rm,
            aes(x = year + (month - 1/2)/12,
                y = value)) +
  # scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +

  # geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~ var,scales = "free") +

  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(limits = c(1960,2025)) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

ggplot() +

  geom_line(data = era5.select.filt,
            aes(x = month,
                y = value,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = era5.sum,
            aes(x = month,
                y = value),
            color = "black", size = 0.8) +

  geom_line(data = era5.select.filt %>%
              filter(year == 2024),
            aes(x = month,
                y = value),
            color = "red", size = 0.8) +

  facet_wrap(~ var,scales = "free") +

  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank()) +

  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


era5.sum.late <- era5.select.filt %>%
  filter(year %in% 2015:2020) %>%
  group_by(var,month) %>%
  summarise(value = mean(value),
            .groups = "keep")


ggplot() +

  geom_line(data = era5.select.filt %>%
              filter(year %in% 2015:2020),
            aes(x = month,
                y = value,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = era5.sum.late,
            aes(x = month,
                y = value),
            color = "black", size = 0.8) +

  geom_line(data = era5.select.filt %>%
              filter(year == 2024),
            aes(x = month,
                y = value),
            color = "red", size = 0.8) +

  facet_wrap(~ var,scales = "free") +

  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank()) +

  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

################################################################################
# Whole region

era5.select.filt <- era5.select %>%
  ungroup() %>%
  filter(year %in% 1941:2024) %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  mutate(pre = (pre*N*8),
         tmp = (tmp - 273.15)) %>%
  pivot_longer(cols = c(pre,tmp),
               names_to = "var")

era5.select.filt.anomaly <- era5.select.filt %>%
  filter(year >= 1960) %>%
  group_by(var,month,lon,lat) %>%
  mutate(mean.month = mean(value[year %in% 1970:1999])) %>%
  mutate(anomaly = value - mean.month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly[year %in% 1970:1999])) %>%
  ungroup()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = era5.select.filt.anomaly %>%
         filter(year == 2024,
                month == 5,
                var == "pre")) +
  geom_raster(aes(x=lon,y = lat,
                  fill = anomaly),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "black") +

  geom_point(data = data.frame(y = -4 -19/60-20.8092/3600,
                               x = 15 + 18/60+25.3620/3600),
             aes(x = x, y = y)) +

  coord_sf(xlim = c(-15, 50), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*200,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "white",high = "darkblue") +
  labs(x = "",y = "") +
  # facet_wrap(~ source) +
  theme_map() +
  # guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "right")


ggplot(data = era5.select.filt.anomaly %>%
         filter(year == 2024,
                month == 5,
                var == "tmp")) +
  geom_raster(aes(x=lon,y = lat,
                  fill = anomaly),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "black") +

  coord_sf(xlim = c(-15, 50), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(limits = c(0,1)*5,
                       oob = scales::squish,
                       low = "white",high = "darkred") +
  labs(x = "",y = "") +
  # facet_wrap(~ source) +
  theme_map() +
  # guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank(),
        legend.position = "right")



