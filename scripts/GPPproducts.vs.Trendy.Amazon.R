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
        c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/GPP.products.Amazon.RDS",
          "./outputs/"))

system2("scp",
        c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/GPP.products.Congo.RDS",
          "./outputs/"))

df.all.rspld <- bind_rows(readRDS("./outputs/GPP.products.Amazon.RDS") %>%
                            mutate(basin = "Amazon"),
                          readRDS("./outputs/GPP.products.Congo.RDS") %>%
                            mutate(basin = "Congo"))

Trendy.data <-  bind_rows(readRDS("./outputs/Amazon.mean.JRA.historical.RDS") %>%
                            mutate(basin = "Amazon"),
                          readRDS("./outputs/Congo.mean.JRA.historical.RDS") %>%
                            mutate(basin = "Congo"))
Trendy <- Trendy.data %>%
  group_by(basin,year,month,var) %>%
  summarise(value.MEM = mean(pred),
            .groups = "keep") %>%
  ungroup() %>%
  dplyr::filter(var == "gpp") %>%
  dplyr::filter(year %in% unique(c(df.all.rspld$year)))

all <- df.all.rspld %>%
  ungroup() %>%
  left_join(Trendy %>%
              dplyr::select(basin,year,month,value.MEM),
            by = c("year","month","basin")) %>%
  mutate(anomaly = value.m - mean(value.m,na.rm = TRUE),
         anomaly.MEM = value.MEM - mean(value.MEM,na.rm = TRUE))


################################################################################

droughts <- data.frame(x1 = c(1982,1997,2004,2009,2015,2023),
                       x2 = c(1983,1998,2005,2010,2016,2023) +
                         11.5/12)

ggplot(all) +
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
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1980,2022)) +
  theme_bw()

ggplot(data = all,
       aes(x = value.m, y = value.MEM, color = basin)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()

all %>%
  group_by(model,basin) %>%
  summarise(r2 = summary(lm(formula = value.MEM ~ value.m))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = value.MEM ~ value.m))[2],
            .groups = "keep")

ggplot(data = all,
       aes(x = anomaly, y = anomaly.MEM, color = basin)) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()


all %>%
  group_by(model, basin) %>%
  summarise(r2 = summary(lm(formula = anomaly.MEM ~ anomaly))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = anomaly.MEM ~ anomaly))[2],
            .groups = "keep")


################################################################################
# Individual models

models <- unique(Trendy.data$model)

df.r2 <- data.frame()
for (cmodel in models){
  print(cmodel)

  Trendy <- Trendy.data %>%
    filter(model == cmodel) %>%
    group_by(basin,year,month,var) %>%
    summarise(value.MEM = mean(pred),
              .groups = "keep") %>%
    ungroup() %>%
    dplyr::filter(var == "gpp") %>%
    dplyr::filter(year %in% unique(c(df.all.rspld$year)))

  all <- df.all.rspld %>%
    ungroup() %>%
    left_join(Trendy %>%
                dplyr::select(basin,year,month,value.MEM),
              by = c("year","month","basin")) %>%
    mutate(anomaly = value.m - mean(value.m,na.rm = TRUE),
           anomaly.MEM = value.MEM - mean(value.MEM,na.rm = TRUE))

  df.r2 <- bind_rows(df.r2,
                     all %>%
    group_by(basin,model) %>%
    summarise(r2 = summary(lm(formula = value.MEM ~ value.m))[["r.squared"]],
              r = sqrt(r2),
              slope = coef(lm(formula = value.MEM ~ value.m))[2],
              .groups = "keep") %>%
      mutate(trendy.model = cmodel))

}

df.r2 %>%
  # filter(model == "MODIS") %>%
  pull(r2) %>% hist()

df.r2 %>%
  arrange(desc(r2))
