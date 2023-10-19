rm(list = ls())

library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
library(data.table)
library(scales)
library(TrENDY.analyses)
library(raster)
library(stringr)
library(ggridges)
library(Congo.ED2)

carbon_content 	<- 0.456			# Martin et al. 2018 Nature Geoscience

Biomass.Trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.cAGB.v11.RDS") %>%
  filter(variable == "cVeg")

nep.trendy <- Biomass.Trendy %>%
  rename(cVeg = value) %>%
  group_by(lon,lat,model) %>%
  mutate(nep = c(NA,diff(cVeg))/365/86400,
            .groups = "keep") %>%
  mutate(Continent = case_when(lon <= -20 ~ "Amazon",
                               lon <= 55 ~ "Africa",
                               lon > 90 & lat > - 10 ~ "Asia",
                               lon > 100 & lat <= -10 ~ "Australia",
                               TRUE ~ "Other")) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia","Other")))

gpp.npp.trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.Pantropical.GPP.NPP.v11.RDS") %>%
  ungroup() %>%
  distinct() %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
  mutate(Continent = case_when(lon <= -20 ~ "Amazon",
                               lon <= 55 ~ "Africa",
                               lon > 90 & lat > - 10 ~ "Asia",
                               lon > 100 & lat <= -10 ~ "Australia",
                               TRUE ~ "Other")) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia","Other"))) %>%
  filter(gpp != 0)


ggplot(data = gpp.npp.trendy) +
  geom_boxplot(aes(x = model, fill = model,
                   y = npp/gpp),outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw()

ggplot(data = gpp.npp.trendy) +
  geom_boxplot(aes(x = model, fill = model,
                   y = gpp*1000*86400),outlier.shape = NA) +    #gC/m²/day
  scale_y_continuous() +
  theme_bw()

gpp.npp.sum <- gpp.npp.trendy %>%
  group_by(Continent,model,year) %>%
  summarise(gpp.m = mean(gpp,na.rm = TRUE),
            npp.m = mean(npp,na.rm = TRUE),
            .groups = "keep")

ggplot(data = gpp.npp.sum %>%
         filter(Continent != "Other")) +
  geom_line(aes(x = year, color = model,
                y = gpp.m*86400*1000)) +
  geom_line(aes(x = year, color = model,
                y = npp.m*86400*1000),linetype = 2) +
  scale_y_continuous() +
  facet_wrap(~Continent) +
  theme_bw()


nep.trendy.sum <- nep.trendy %>%
  group_by(Continent,model,year) %>%
  summarise(nep.m = mean(nep,na.rm = TRUE),
            .groups = "keep")

ggplot(data = nep.trendy.sum %>%
         filter(Continent != "Other")) +
  geom_line(aes(x = year, color = model,
                y = nep.m*86400*1000)) +
  scale_y_continuous() +
  facet_wrap(~Continent) +
  theme_bw()

gpp.npp.nep.trendy <- gpp.npp.trendy %>%
  left_join(nep.trendy,
            by = c("lon","lat","year","model"))

gpp.npp.nep.sum.lat <- gpp.npp.nep.trendy %>%
  mutate(lat = round(lat,digits = 0)) %>%
  group_by(model,Continent,lat) %>%
  summarise(gpp.m = mean(gpp,na.rm = TRUE),
            npp.m = mean(npp,na.rm = TRUE),      # kgC/m2/yr
            nep.m = mean(nep,na.rm = TRUE),   # MgC/m²/yr
            .groups = "keep")

gpp.npp.nep.sum.lat.long <- gpp.npp.nep.sum.lat %>%
  pivot_longer(cols = c(gpp.m,npp.m,nep.m))


# https://essd.copernicus.org/articles/14/1063/2022/#section6
ggplot(data = gpp.npp.nep.sum.lat.long %>%
         filter(Continent %in% c("Amazon","Africa"))) +
  geom_line(aes(x = lat, color = model,
                y = value*86400*365)) +
  scale_y_continuous() +
  facet_wrap(Continent ~ name, scales = "free",ncol = 3) +
  theme_bw()


R <- raster("/home/femeunier/Documents/projects/SoilSensitivity/data/SAM_modis_yearly.grd")


# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot() +
#
#   geom_raster(data = nep.trendy %>%
#                 filter(model == model[1]),
#               aes(x = lon, y = lat,
#                   fill = nep*86400*365), na.rm = TRUE, alpha = 1) +
#
#   geom_sf(data = world,
#           fill = NA) +
#
#   coord_sf(xlim = c(-80, 150),
#            ylim = c(-20, 12)) +
#
#   scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",limits = c(-1,1)) +
#   labs(x = "",y = "") +
#
#   facet_wrap(~ model) +
#
#   theme_bw()

