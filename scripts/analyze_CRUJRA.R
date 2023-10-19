rm(list = ls())

library(lubridate)
library(ggthemes)
library(YGB)
library(zoo)
library(sf)
library(dplyr)
library(ggplot2)

climate <- readRDS("~/Documents/projects/TrENDY.analyses/outputs/monthly.climate.pantropical.RDS")
Tropics <- climate %>%
  filter(lon >= -85, lon <= 50,
         lat >= -20, lat <= 15) %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  mutate(pre = pre*4*N,
         tmp = tmp - 273.15) %>%
  dplyr::select(c(lon,lat,year,month,pre,tmp,N,tmin,tmax,dswrf)) %>%
  group_by(lon,lat,year) %>%
  mutate(diff = pre - 3.33*N) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(year,lat,lon,month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(year,lat,lon,month) %>%
  mutate(MCWD = min(CWD)) %>%
  dplyr::select(-c(wettest.month,month.ord,CWD,diff))


Tropics.yearly <- Tropics %>%
  group_by(lon,lat,year) %>%
  summarise(pre.sd = sd(pre),
            tmax.sd = sd(tmax),
            tmin.sd = sd(tmin),
            dswrf.sd = sd(dswrf),
            tmp.sd = sd(tmp),

            MAP = sum(pre),
            tmax = max(tmax),
            tmin = min(tmin),
            dswrf = mean(dswrf),
            MCWD = MCWD[1],
            MAT = mean(tmp),

            .groups = "keep")

Tropics.sum <- Tropics.yearly %>%
  group_by(lon,lat) %>%
  summarise(MAP.m = mean(MAP),
            MCWD.m = mean(MCWD),
            MAT.m = mean(MAT),
            tmax.m = mean(tmax),
            tmin.m = mean(tmin),
            dswrf.m = mean(dswrf),

            MAP.sd = sd(MAP),
            MCWD.sd = sd(MCWD),
            MAT.sd = sd(MAT),
            tmax.sd = sd(tmax),
            tmin.sd = sd(tmin),
            dswrf.sd = sd(dswrf),

            pre.m.sd = mean(pre.sd),
            tmp.m.sd = mean(tmp.sd),
            tmax.m.sd = mean(tmax.sd),
            tmin.m.sd = mean(tmin.sd),
            dswrf.m.sd = mean(dswrf.sd),

            slope.MAP = coef(lm(formula = MAP ~ year))[2],
            pval.MAP = (summary(lm(formula = MAP ~ year)))[["coefficients"]][2,4],
            slope.MCWD = coef(lm(formula = MCWD ~ year))[2],
            pval.MCWD = (summary(lm(formula = MCWD ~ year)))[["coefficients"]][2,4],
            slope.MAT = coef(lm(formula = MAT ~ year))[2],
            pval.MAT = (summary(lm(formula = MAT ~ year)))[["coefficients"]][2,4],

            .groups = "keep")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan_raw.RDS")

networks2plot <- networks %>%
  mutate(Size = int_c_length*PlotArea) %>%
  group_by(ClusterCode) %>%
  summarise(lat = mean(Lat),
            lon = mean(Lon),
            totsize = sum(Size),
            .groups = "keep")

# IFL <- readOGR(dsn = "~/Downloads/IFL/", layer = "ifl_2020")
IFL <- readRDS("./outputs/ILF2013.df")
################################################################################
# MAP

ggplot() +

  geom_raster(data = Tropics.sum,
              aes(x = lon, y = lat,
                  fill = MAP.m), na.rm = TRUE, alpha = 0.5) +

  geom_point(data = networks2plot,
             aes(x = lon, y = lat, size = totsize), alpha = 1,
             shape = 16) +

  geom_sf(data = world,
          fill = NA) +
  # geom_sf(data = st_as_sf(IFL),
  #         fill = NA) +
  geom_tile(data = IFL %>%
                filter(is.undisturbed == 1),
              aes(x = lon, y = lat),
                  fill = NA,
                  color = "black",
            linewidth = 0.5) +

  coord_sf(xlim = c(-85, 50),
           ylim = c(-20, 15)) +

  labs(x = "",y = "") +

  scale_fill_gradient(low = "white",high = "darkblue",
                      limits = c(1500,2000), oob = scales::squish) +

  theme_bw()



ggplot() +

  geom_raster(data = Tropics.sum,
              aes(x = lon, y = lat,
                  fill = slope.MAP*10), na.rm = TRUE, alpha = 1) +

  geom_point(data = Tropics.sum %>%
               filter(pval.MAP < 0.01),
              aes(x = lon, y = lat), size = 0.1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-85, 50),
           ylim = c(-20, 15)) +

  labs(x = "",y = "") +

  scale_fill_gradient2(low = "red",high = "darkgreen", mid = "white",
                      limits = c(-50,50), oob = scales::squish) +

  theme_bw()

################################################################################
# MCWD

ggplot() +

  geom_raster(data = Tropics.sum,
              aes(x = lon, y = lat,
                  fill = MCWD.m), na.rm = TRUE, alpha = 1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-85, 50),
           ylim = c(-20, 15)) +

  labs(x = "",y = "") +

  scale_fill_gradient(low = "red",high = "white",
                      limits = c(-400,0), oob = scales::squish) +

  theme_bw()


ggplot() +

  geom_raster(data = Tropics.sum,
              aes(x = lon, y = lat,
                  fill = slope.MCWD*10), na.rm = TRUE, alpha = 1) +

  geom_point(data = Tropics.sum %>%
               filter(pval.MCWD < 0.01),
             aes(x = lon, y = lat), size = 0.1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-85, 50),
           ylim = c(-20, 15)) +

  labs(x = "",y = "") +

  scale_fill_gradient2(low = "red",high = "darkgreen", mid = "white",
                       limits = c(-20,20), oob = scales::squish) +

  theme_bw()

################################################################################
# MAT

ggplot() +

  geom_raster(data = Tropics.sum,
              aes(x = lon, y = lat,
                  fill = MAT.m), na.rm = TRUE, alpha = 1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-85, 50),
           ylim = c(-20, 15)) +

  labs(x = "",y = "") +

  scale_fill_gradient(low = "white",high = "red",
                      limits = c(15,30), oob = scales::squish) +

  theme_bw()


ggplot() +

  geom_raster(data = Tropics.sum,
              aes(x = lon, y = lat,
                  fill = slope.MAT*10), na.rm = TRUE, alpha = 1) +

  geom_point(data = Tropics.sum %>%
               filter(pval.MAT < 0.01),
             aes(x = lon, y = lat), size = 0.1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-85, 50),
           ylim = c(-20, 15)) +

  labs(x = "",y = "") +

  scale_fill_gradient(low = "white",high = "red",
                       limits = c(0,0.3), oob = scales::squish) +

  theme_bw()

saveRDS(Tropics.sum %>%
          dplyr::select(lat,lon,
                        MAP.m,MCWD.m,MAT.m,tmax.m,tmin.m,dswrf.m,
                        MAP.sd,MCWD.sd,MAT.sd,tmax.sd,tmin.sd,dswrf.sd,
                        pre.m.sd,tmp.m.sd,tmax.m.sd,tmin.m.sd,dswrf.m.sd),
        "/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRUJRA.climate.pantropical.RDS")

saveRDS(Tropics.yearly %>%
          dplyr::select(lat,lon,year,
                        MAP,MCWD,MAT,tmax,tmin,dswrf,
                        pre.sd,tmp.sd,tmax.sd,tmin.sd,dswrf.sd),
        "/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRUJRA.climate.pantropical.year.RDS")
