rm(list = ls())

library(tidyr)
library(ggplot2)
library(dplyr)
library(raster)
library(ggthemes)
library(TrENDY.analyses)
library(Congo.ED2)
library(sf)
library(zoo)

files <- c("Congo.mean.JRA.historical.RDS",
           "Amazon.mean.JRA.historical.RDS")

biomes <- readRDS("./outputs/biome.CRUJRA.1901.2022.AI.RDS") %>%
  filter(model == unique(model)[14]) %>%
  mutate(continent = coord2continent(lon,lat))
craster <- rasterFromXYZ(biomes %>%
                           dplyr::select(c(lon,lat,MAP)))

biomes.continents <- biomes %>%
  group_by(biome,continent) %>%
  summarise(N = n(),
            .groups = "keep")

for (cfile in files){
  system2("rsync",
          c("-avz",paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
            "./outputs/"))
}

all <- bind_rows(readRDS("./outputs/Congo.mean.JRA.historical.RDS") %>%
                   mutate(basin = "Congo"),
                 readRDS("./outputs/Amazon.mean.JRA.historical.RDS") %>%
                   mutate(basin = "Amazon"))

ggplot(data = all %>%
         filter(var == "gpp")) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred,
                color = model)) +
  scale_x_continuous(limits = c(2020,2024)) +
  facet_wrap(~ basin) +
  theme_bw()

MEM <- all %>%
  group_by(var,year,month,basin) %>%
  summarise(pred.MEM = mean(pred,na.rm = TRUE),
            obs.MEM = mean(obs,na.rm = TRUE),
            .groups = "keep")

ggplot(data = MEM) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.MEM,
                color = basin)) +
  scale_x_continuous(limits = c(2000,2024)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

MEM.diff <- MEM %>%
  ungroup() %>%
  pivot_wider(names_from = basin,
              values_from = c(pred.MEM,obs.MEM)) %>%
  mutate(diff.pred = pred.MEM_Congo - pred.MEM_Amazon,
         diff.obs = obs.MEM_Congo - obs.MEM_Amazon)

ggplot(data = MEM.diff,
       aes(x = year + (month - 1/2)/12)) +
  geom_line(aes(y = diff.pred), color = "grey") +
  scale_x_continuous(limits = c(1958,2024)) +
  geom_line(aes(y=rollmean(diff.pred, 12, na.pad=TRUE)),
            color = "red") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ var,scales = "free") +
  theme_bw()
