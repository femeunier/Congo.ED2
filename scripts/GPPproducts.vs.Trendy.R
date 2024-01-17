rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)
library(raster)
library(TrENDY.analyses)
library(Congo.ED2)
library(ggplot2)
library(tidyr)


biomes <- readRDS("./outputs/biome.CRUJRA.1901.2022.AI.RDS") %>%
  filter(model == unique(model)[14]) %>%
  mutate(continent = coord2continent(lon,lat))
craster <- rasterFromXYZ(biomes %>%
                           dplyr::select(c(lon,lat,MAP)))


biomes.continents <- biomes %>%
  group_by(biome,continent) %>%
  summarise(N = n(),
            .groups = "keep")

system2("scp",
        c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/GPP.products.RDS",
          "./outputs/"))

df.all.rspld <- readRDS("./outputs/GPP.products.RDS")

Trendy <- readRDS("./outputs/Trendy.MEM.CC.RDS") %>%
  ungroup() %>%
  dplyr::filter(var == "gpp") %>%
  dplyr::filter(year %in% unique(c(df.all.rspld$year))) %>%
  mutate(biome.continent = paste0(biome,".",continent))

biome.continents <- intersect(unique(Trendy$biome.continent),
                              unique(df.all.rspld$biome.continent))

all <- df.all.rspld %>%
  filter(biome.continent %in% biome.continents) %>%
  ungroup() %>%
  dplyr::select(-biome.continent) %>%
  left_join(Trendy %>%
              filter(model == "MEM") %>%
              dplyr::select(continent,year,month,biome,value.m.MEM),
            by = c("year","month","continent","biome")) %>%
  group_by(continent,biome) %>%
  mutate(anomaly = value.m - mean(value.m,na.rm = TRUE),
         anomaly.MEM = value.m.MEM - mean(value.m.MEM,na.rm = TRUE))


################################################################################

droughts <- data.frame(x1 = c(1982,1997,2004,2009,2015,2023),
                       x2 = c(1983,1998,2005,2010,2016,2023) +
                         11.5/12)

ggplot(all %>%
         filter(continent == "America")) +
  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = model),
            linetype = 1) +
  # geom_line(aes(x = year + (month - 1/2)/12,
  #               y = value.m.MEM,
  #               color = biome),
  #           linetype = 2) +
  facet_grid( ~ biome) +
  scale_x_continuous(limits = c(1980,2022)) +
  theme_bw()

ggplot(data = all,
       aes(x = value.m, y = value.m.MEM)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", color = "black",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()


ggplot(data = all,
       aes(x = value.m, y = value.m.MEM,
           color = biome,
           group = interaction(continent,
                               biome))) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_grid(continent ~ model) +
  theme_bw()

all %>%
  group_by(model, biome, continent) %>%
  summarise(r2 = summary(lm(formula = value.m.MEM ~ value.m))[["r.squared"]],
            .groups = "keep") %>%
  filter(continent == "America")


ggplot(data = all,
       aes(x = anomaly, y = anomaly.MEM)) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()


ggplot(data = all ,
       aes(x = anomaly, y = anomaly.MEM,
           color = biome,
           group = interaction(continent,
                               biome))) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_grid(continent ~ model) +
  theme_bw()

all %>%
  group_by(model, biome, continent) %>%
  summarise(r2 = summary(lm(formula = anomaly.MEM ~ anomaly))[["r.squared"]],
            .groups = "keep")

################################################################################
all.continent <- all %>%
  left_join(biomes.continents,
            by = c('biome','continent')) %>%
  group_by(model, continent, year,month) %>%
  summarise(value.m.w = weighted.mean(value.m,N),
            value.m.MEM.w = weighted.mean(value.m.MEM,N),
            anomaly.w = weighted.mean(anomaly,N),
            anomaly.MEM.w = weighted.mean(anomaly.MEM,N),
            .groups = "keep")

ggplot(all.continent) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m.w,
                color = continent),
            linetype = 1) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m.MEM.w,
                color = continent),
            linetype = 2) +
  facet_grid(~ model) +
  theme_bw()

ggplot(data = all.continent,
       aes(x = value.m.w, y = value.m.MEM.w)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm", color = "black",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()


ggplot(data = all.continent,
       aes(x = value.m.w, y = value.m.MEM.w,
           color = continent,
           group = interaction(continent))) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_grid( ~ model) +
  theme_bw()

all.continent %>%
  group_by(model, continent) %>%
  summarise(r2 = summary(lm(formula = value.m.MEM.w ~ value.m.w))[["r.squared"]],
            .groups = "keep")


ggplot(data = all.continent,
       aes(x = anomaly.w, y = anomaly.MEM.w)) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()


ggplot(data = all.continent ,
       aes(x = anomaly.w, y = anomaly.MEM.w,
           color = continent)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_grid( ~ model) +
  theme_bw()

all.continent %>%
  group_by(model, continent) %>%
  summarise(r2 = summary(lm(formula = anomaly.MEM.w ~ anomaly.w))[["r.squared"]],
            .groups = "keep") %>%
  filter(continent == "America")


ggplot(data = all.continent %>%
         filter(continent == "America"),
       aes(x = anomaly.w, y = anomaly.MEM.w,
           color = model)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 1) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  # facet_grid( ~ model) +
  theme_bw()


