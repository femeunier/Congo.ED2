rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dismo)
library(tie)

climate <- readRDS("./outputs/monthly.climate.pantropical.JRA.historical.RDS")

climate.select <- climate %>%
  mutate(timing = case_when(year %in% c(1961:1990) ~ "original",
                            year == 2010 ~ "2010",
                            year == 2023 ~ "2023",
                            TRUE ~ NA_character_)) %>%
  filter(!is.na(timing))

# Only MAP first
monthly.precip <- climate.select %>%
  ungroup() %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  mutate(pre = pre*4*N)

MAP <- monthly.precip %>%
  group_by(lat,lon,timing,year) %>%
  summarise(MAP = sum(pre),
            .groups = "keep") %>%
  group_by(lat,lon,timing) %>%
  summarise(MAP = mean(MAP),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = MAP) +
  geom_raster(aes(x=lon,y = lat,
                  fill = MAP),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ timing) +
  scale_fill_gradient(limits = c(0,3000),
                      oob = scales::squish,
                      low =  "white",high = "darkblue") +
  theme_bw()

MAP.diff <- MAP %>%
  pivot_wider(names_from = timing,
              values_from = MAP) %>%
  mutate(diff_2010 = original - `2010`,
         diff_2023 = original - `2023`) %>%
  dplyr::select(-c(original,`2010`,`2023`)) %>%
  pivot_longer(cols = c(diff_2010,diff_2023),
               names_to = "year",
               values_to = "diff")

ggplot(data = MAP.diff) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ year) +
  scale_fill_gradient2(limits = c(-500,500)*2,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "white",high = "darkgreen") +
  theme_bw()

ggplot(data = MAP %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35)) +
  geom_density(aes(x = MAP,
                   fill = timing),
               alpha = 0.3, color = NA) +
  theme_bw()

ggplot(data = monthly.precip  %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35) %>%
         group_by(month,timing) %>%
         summarise(precip = mean(pre),
                   .groups = "keep")) +
  geom_line(aes(x = month, color = timing, y = precip)) +
  theme_bw()

################################################################################
# All biovars

climate.biovars <- monthly.precip %>%
  group_by(timing,month,lat,lon) %>%
  summarise(pre = mean(pre),
            tmin = mean(tmin),
            tmax = mean(tmax),
            .groups = "keep") %>%
  group_by(timing,lat,lon) %>%
  bow(tie(bio1, bio2, bio3, bio4,
          bio5, bio6, bio7, bio8,
          bio9, bio10, bio11, bio12,
          bio13, bio14, bio15, bio16,
          bio17, bio18, bio19) := c(biovars(pre,
                                            (tmin - 273.15)*10,
                                            (tmax - 273.15)*10)[c(1:19)]))

climate.biovars.long <- climate.biovars %>%
  pivot_longer(cols = -c(timing,lon,lat),
               names_to = "biovar",
               values_to = "value") %>%
  mutate(biovar = factor(biovar,
                         levels = paste0("bio",1:19)))

ggplot(data = climate.biovars.long %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35)) +
  geom_density(aes(x = value,
                   fill = timing),
               alpha = 0.3, color = NA) +
  facet_wrap(~ biovar, scales = "free") +
  theme_bw()

climate.biovars.long.wide <- climate.biovars.long %>%
  pivot_wider(names_from = timing,
              values_from = value) %>%
  mutate(diff_2010 = original - `2010`,
         diff_2023 = original - `2023`) %>%
  dplyr::select(-c(original,`2010`,`2023`)) %>%
  pivot_longer(cols = c(diff_2010,diff_2023),
               names_to = "year",
               values_to = "diff")

ggplot(data = climate.biovars.long.wide %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35)) +
  geom_density(aes(x = diff,
                   fill = year),
               alpha = 0.3, color = NA) +
  facet_wrap(~ biovar, scales = "free") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  theme_bw()


ggplot(data = climate.biovars.long.wide %>%
         filter(biovar %in% paste0("bio",
                                   12:18))) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  labs(x = "",y = "") +
  facet_grid(biovar ~ year) +
  scale_fill_gradient2(limits = c(-1000,1000),
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "white",high = "darkgreen") +
  theme_bw()
