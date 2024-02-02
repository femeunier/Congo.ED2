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

GPPproducts2keep <- c("SIF","SIF2")

coord2keep <- bind_rows(readRDS("./outputs/Amazon.coord.GPP.products.ILF.RDS"),
                        readRDS("./outputs/Congo.coord.GPP.products.ILF.RDS"))

df.all.rspld <- bind_rows(readRDS("./outputs/GPP.products.Amazon.ILF.RDS") %>%
                            mutate(basin = "Amazon"),
                          readRDS("./outputs/GPP.products.Congo.ILF.RDS") %>%
                            mutate(basin = "Congo")) %>%
  filter(model.lon.lat %in% coord2keep[["model.lon.lat"]]) %>%
  ungroup() %>%
  dplyr::filter(model %in% GPPproducts2keep) %>%
  dplyr::select(-c(continent,biome,biome.continent,model.lon.lat))


df.all.rspld.sum <- df.all.rspld %>%
  group_by(year,month,model) %>%
  mutate(diff = value - mean(value,na.rm = TRUE)) %>%
  group_by(lat,lon,model) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            cv = 100*value.sd/value.m,
            diff.m = 100*mean(abs(diff),na.rm = TRUE)/value.m,
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df.all.rspld.sum) +
  geom_raster(aes(x=lon,y = lat,
                  fill = cv),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen") +
  facet_grid(~ model) +
  theme_minimal() +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) +
  theme(text = element_text(size = 20),
        legend.position = "bottom")

ggplot(data = df.all.rspld.sum) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff.m),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(limits = c(0,100),
                       oob = scales::squish,
                       low =  "white",high = "darkgreen") +
  facet_grid(~ model) +
  theme_minimal() +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) +
  theme(text = element_text(size = 20),
        legend.position = "bottom")


grid <- readRDS("./data/GPP/monthly/SIF.GPP.RDS") %>%
  rename(daily.GPP = value) %>%
  filter(year == year[1],
         month == month[1]) %>%
  dplyr::select(lon,lat,daily.GPP)

rast <- rasterFromXYZ((grid %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","daily.GPP")))[,c("lon","lat","daily.GPP")])

years <- sort(unique(df.all.rspld$year))

################################################################################

# Trendy.data <-  bind_rows(readRDS("./outputs/Amazon.mean.JRA.historical.IFL.RDS") %>%
#                             mutate(basin = "Amazon"),
#                           readRDS("./outputs/Congo.mean.JRA.historical.IFL.RDS") %>%
#                             mutate(basin = "Congo")) %>%
#   dplyr::select(-c(xgb.model,model.lon.lat)) %>%
#   filter(year %in% years,
#          var == "gpp")
#
# models <- sort(unique(Trendy.data$model))
#
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# Trendy.data.rspld <- data.frame()
# for (cmodel in models){
#   print(cmodel)
#
#   ctrendy.data <- Trendy.data %>%
#     filter(model == cmodel)
#
#   ctrendy.data.rspld <- resample.df.all.col(bigdf = ctrendy.data %>%
#                                               dplyr::select(lat,lon,year,month,model,obs),
#
#                                             raster2resample = rast,
#                                             var.names = c("obs"),
#                                             res = 0.00001) %>%
#     filter(!is.na(obs))
#
#   Trendy.data.rspld <- bind_rows(Trendy.data.rspld,
#                                  ctrendy.data.rspld)
#
# }
#
# saveRDS(Trendy.data.rspld,
#         "./outputs/Trendy.data.rspld.RDS")

coord2keep2 <- bind_rows(readRDS("./outputs/Amazon.coord.ILF.RDS"),
                         readRDS("./outputs/Congo.coord.ILF.RDS")) %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat, digits = 2)))

Trendy.data.rspld <- readRDS("./outputs/Trendy.data.rspld.RDS") %>%
  mutate(basin = case_when(lon <= -30 ~ "Amazon",
                           TRUE ~ "Congo")) %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat, digits = 2))) %>%
  filter(lon.lat %in% coord2keep2[["lon.lat"]])

# MEM
Trendy.sum <- Trendy.data.rspld %>%
  group_by(basin,year,month,lat,lon) %>%
  summarise(value.MEM = mean(obs),
            .groups = "keep") %>%
  ungroup()

#################################################################################
# Spatial mean

all <- df.all.rspld %>%
  ungroup() %>%
  left_join(Trendy.sum %>%
              dplyr::select(basin,year,month,lat,lon,value.MEM),
            by = c("year","month","lat","lon","basin")) %>%
  group_by(lat,lon) %>%
  mutate(anomaly = value - mean(value,na.rm = TRUE),
         anomaly.MEM = value.MEM - mean(value.MEM,na.rm = TRUE))

################################################################################
# Monthly means

ggplot(all,
       aes(x = value,
           y = value.MEM)) +
  geom_hex() +

  geom_abline(slope = 1, intercept = 0, linetype = 2) +

  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(10,10000),
    oob = scales::squish,
    trans = "log10") +

  facet_grid(model ~ basin) +
  stat_smooth(method = "lm", se = FALSE,
              color = "black", linetype = 1) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")

all %>%
  group_by(model,basin) %>%
  summarise(r2 = summary(lm(formula = value.MEM ~ value))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = value.MEM ~ value))[2],
            .groups = "keep")

# yearly means

ggplot(all %>%
         group_by(year,lat,lon,model,basin) %>%
         summarise(value.m = mean(value),
                   value.MEM = mean(value.MEM),
                   .groups = "keep"),
       aes(x = value.m,
           y = value.MEM)) +
  geom_hex() +

  geom_abline(slope = 1, intercept = 0, linetype = 2) +

  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(10,1000),
    oob = scales::squish,
    trans = "log10") +

  facet_grid(model ~ basin) +
  stat_smooth(method = "lm", se = FALSE,
              color = "black", linetype = 1) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")

all %>%
  group_by(year,lat,lon,model,basin) %>%
  summarise(value.m = mean(value),
            value.MEM = mean(value.MEM)) %>%
  group_by(model,basin) %>%
  summarise(r2 = summary(lm(formula = value.MEM ~ value.m))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = value.MEM ~ value.m))[2],
            .groups = "keep")

################################################################################
# Individual models

models <- unique(Trendy.data.rspld$model)

df.r2 <- all.models <- data.frame()

for (cmodel in models){
  print(cmodel)

  cTrendy <- Trendy.data.rspld %>%
    filter(model == cmodel) %>%
    ungroup()

  all <- df.all.rspld %>%
    ungroup() %>%
    left_join(cTrendy %>%
                dplyr::select(basin,year,month,lat,lon,obs),
              by = c("year","month","basin","lat","lon")) %>%
    group_by(lat,lon) %>%
    mutate(anomaly = value - mean(value,na.rm = TRUE),
           anomaly.MEM = obs - mean(obs,na.rm = TRUE))

  df.r2 <- bind_rows(df.r2,
                     all %>%
                       group_by(basin,model) %>%
                       summarise(r2 = summary(lm(formula = obs ~ value))[["r.squared"]],
                                 r = sqrt(r2),
                                 slope = coef(lm(formula = obs ~ value))[2],
                                 .groups = "keep") %>%
                       mutate(trendy.model = cmodel))

  all.models <- bind_rows(all.models,
                          all %>%
                            mutate(trendy.model = cmodel))

}

df.r2 %>%
  filter(basin == "Amazon",
         slope > 0) %>%
  arrange(desc(r2))


ggplot(all.models %>%
         filter(trendy.model == "ORCHIDEE"),
       aes(x = value,
           y = obs)) +
  geom_hex() +

  geom_abline(slope = 1, intercept = 0, linetype = 2) +

  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(10,10000),
    oob = scales::squish,
    trans = "log10") +

  facet_grid(model ~ basin) +
  stat_smooth(method = "lm", se = FALSE,
              color = "black", linetype = 1) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")
