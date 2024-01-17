rm(list = ls())

library(tidyr)
library(ggplot2)
library(dplyr)
library(raster)
library(ggthemes)
library(TrENDY.analyses)
library(Congo.ED2)
library(sf)

files <- c("df.all.ts.JRA.historical.RDS",
           "df.all.year.JRA.historical.RDS",
           "df.all.ts.MEM.JRA.historical.RDS",
           "df.all.year.MEM.JRA.historical.RDS",
           "Amazon.mean.JRA.historical.RDS",
           "Amazon.JRA.historical.RDS")

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

################################################################################
# Timeseries

predictions.XGB.sum <- readRDS("./outputs/df.all.ts.JRA.historical.RDS")

ggplot(predictions.XGB.sum %>%
         filter(model == "CABLE-POP")) +

  geom_line(aes(x = year + (month-1/2)/12, y = obs.m, group = var),
            color = "black") +

  geom_line(aes(x = year + (month-1/2)/12, y = pred.m, color = biome,
                group = interaction(biome,var)),
            linetype = 2) +

#   geom_line(aes(x = year + (month-1/2)/12, y = pred.JRA.m, color = biome,
#                 group = interaction(biome,var)),
#             linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  scale_x_continuous(limits = c(2020,2024)) +

  facet_grid(biome ~ continent) +
  theme_bw()

MEM <- readRDS("./outputs/df.all.ts.MEM.JRA.historical.RDS")

ggplot(MEM %>%
         filter(var %in% c("gpp","npp","nep"))) +

  geom_line(aes(x = year + (month-1/2)/12, y = obs.MEM.m, group = var),
            color = "black") +

  geom_line(aes(x = year + (month-1/2)/12, y = pred.MEM.m, group = interaction(biome,var),color = biome),
            linetype = 2) +

#   geom_line(aes(x =  year + (month-1/2)/12, y = pred.JRA.MEM.m, group = interaction(biome,var),color = biome),
#             linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  scale_x_continuous(limits = c(2020,2024)) +

  facet_grid(biome ~ continent) +
  theme_bw()

################################################################################
# yearly timeseries

predictions.XGB.sum.year <- readRDS("./outputs/df.all.year.JRA.historical.RDS")

ggplot(predictions.XGB.sum.year %>%
         filter(model == "CABLE-POP")) +

  geom_line(aes(x = year, y = obs.m, group = var),
            color = "black") +

  geom_line(aes(x = (year), y = pred.m, color = biome,
                group = interaction(biome,var)),
            linetype = 1) +

#   geom_line(aes(x = (year), y = pred.JRA.m, color = biome,
#                 group = interaction(biome,var)),
#             linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  # scale_x_continuous(limits = c(2017,2023)) +

  facet_grid(biome ~ continent) +
  theme_bw()

MEM.year <-  readRDS("./outputs/df.all.year.MEM.JRA.historical.RDS")

ggplot(MEM.year) +
  # geom_ribbon(aes(x = (month), y = pred.m, fill = biome,
  #                 ymin = pred.m - pred.sd, ymax = pred.m + pred.sd),
  #             color = NA,alpha = 0.5) +
  # geom_ribbon(aes(x = (month), y = pred.JRA.m, fill = biome,
  #                 ymin = pred.JRA.m - pred.JRA.sd, ymax = pred.JRA.m + pred.JRA.sd),
  #             color = NA,alpha = 0.5) +

  geom_line(aes(x = year, y = obs.MEM.m, group = var),
            color = "black") +

  geom_line(aes(x = (year), y = pred.MEM.m, group = interaction(biome,var),color = biome),
            linetype = 2) +

  # geom_line(aes(x = year, y = pred.JRA.MEM.m, group = interaction(biome,var),
  #               color = biome),
  #           linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  scale_x_continuous(limits = c(2000,2024)) +

  facet_grid(biome ~ continent) +
  theme_bw()


MEM.year.diff <- MEM.year %>%
  filter(continent %in% c("America","Africa")) %>%
  pivot_wider(values_from = c(obs.MEM.m,pred.MEM.m),
              names_from = continent) %>%
  mutate(diff.pred = pred.MEM.m_Africa - pred.MEM.m_America,
         diff.obs = obs.MEM.m_Africa - obs.MEM.m_America)

ggplot(MEM.year.diff) +

  geom_line(aes(x = year, y = diff.obs, group = var),
            color = "black") +

  geom_line(aes(x = (year), y = diff.pred, group = interaction(biome,var),color = biome),
            linetype = 2) +

  # geom_line(aes(x = year, y = pred.JRA.MEM.m, group = interaction(biome,var),
  #               color = biome),
  #           linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  scale_x_continuous(limits = c(2000,2024)) +

  facet_grid(var ~ biome) +
  theme_bw()

################################################################################

all.Amazon <- readRDS("./outputs/Amazon.mean.JRA.historical.RDS")

MEM.area <- all.Amazon %>%
  group_by(var,year,month) %>%
  # MEM
  summarise(obs.sd = sd(obs,na.rm = TRUE),
            obs.m = mean(obs,na.rm = TRUE),

            pred.sd = sd(pred,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")


ggplot(data = MEM.area %>%
         filter(var %in% c("nep"))) +

  geom_ribbon(aes(x = year + (month - 1/2)/12,
                y = pred.m, ymin = pred.m-pred.sd, ymax = pred.m + pred.sd,
                group = var), alpha = 0.5,
            linetype = 2, fill = "grey", color = NA) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = obs.m,
                group = var),
            color = "black") +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m,
                group = var), color = 'darkblue',
            linetype = 3) +
  scale_x_continuous(limits = c(1980,2024), expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  guides(color = "none")


MEM.Amazon <- all.Amazon %>%
  group_by(var,model,year,month) %>%
  # Regional average
  summarise(obs.m = mean(obs,na.rm = TRUE),
            obs.sd = sd(obs,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(var,year,month) %>%
  # MEM
  summarise(obs.sd = sd(obs.m,na.rm = TRUE),
            obs.m = mean(obs.m,na.rm = TRUE),

            # pred.JRA.sd = sd(pred.JRA.m,na.rm = TRUE),
            # pred.JRA.m = mean(pred.JRA.m,na.rm = TRUE),

            pred.sd = sd(pred.m,na.rm = TRUE),
            pred.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

MEM.Amazon.cycle <- MEM.Amazon %>%
  group_by(var,month) %>%
  summarise(obs.m = mean(obs.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = MEM.Amazon.cycle) +
  geom_line(aes(x = month, y = obs.m)) +
  facet_wrap(~ var,nrow = 3, scales = "free") +
  theme_bw()

MEM.area.anomalies <- MEM.Amazon %>%
  group_by(var,month) %>%
  mutate(mean.obs = mean(obs.m,na.rm = TRUE)) %>%
  mutate(anomaly.obs = obs.m - mean.obs,
         anomaly.pred = pred.m - mean.obs)

ggplot(data = MEM.area.anomalies %>%
         filter(var %in% c("nep","gpp"))) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.obs,
                group = var),
            color = "black") +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.pred,
                group = var), color = 'darkblue',
            linetype = 2) +
  # geom_line(aes(x = year + (month - 1/2)/12,
  #               y = anomaly.pred.JRA,
  #               group = var),
  #           color = "red") +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ var) +
  scale_x_continuous(limits = c(1980, 2024), expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  guides(color = "none")

all <- readRDS("./outputs/Amazon.JRA.historical.RDS") %>%
  ungroup() %>%
  filter(month == 10)

all.year <- all %>%
  group_by(model,lat,lon,year,var) %>%
  summarise(obs.m = mean(obs,na.rm = TRUE),
            # pred.JRA.m = mean(pred.JRA,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")

all.period <- all.year %>%
  ungroup() %>%
  mutate(period = case_when(year == 2023 ~ "drougth",
                            # year %in% c(2010,2015) ~ "old",
                            TRUE ~ "other")) %>%
  group_by(lat,lon,model,var,period) %>%
  summarise(obs.m = mean(obs.m,na.rm = TRUE),
            pred.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

all.period.wide <- all.period %>%
  pivot_wider(names_from = period,
              values_from = c(obs.m,pred.m)) %>%
  mutate(ratio = pred.m_drougth/obs.m_other,
         diff = pred.m_drougth - obs.m_other)

# View(all.period.wide %>%
#   mutate(model.lat.lon = paste0(model,".",round(lat,digits = 2),".",round(lon,digits = 2))) %>%
#   filter(model.lat.lon %in% selected[["model.lat.lon"]]) %>%
#   filter(var == "gpp") )

df2 <- readRDS("./outputs/selected.RDS") %>%
  filter(year == year[1],
         month == month[1]) %>%
  dplyr::select(lat,lon,tmp)

grid <- rasterFromXYZ((df2 %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","tmp")))[,c("lon","lat","tmp")])

df.rspld <- resample.df.all.col(bigdf = all.period.wide %>%
                                  dplyr::select(lat,lon,model,var,
                                                ratio,
                                                diff),

                                raster2resample = grid,
                                var.names = c("diff",
                                              "ratio"),
                                res = 0.0009) %>%
  filter(!is.na(ratio))

MEM.map <- df.rspld %>%
  group_by(lat,lon,var) %>%
  summarise(diff.m = mean(diff,na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# coord <- all %>%
#   filter(model == "CABLE-POP",
#          year == year[1],month == month[1]) %>%
#   dplyr::select(lat,lon,biome)
#
# test <- readRDS("./outputs/biome.CRUJRA.1901.2019.AI.RDS")
# ggplot(data = test %>%
#          filter(model == "CABLE-POP")) +
#   geom_raster(aes(x=lon,y = lat,
#                   fill = biome),alpha = 0.3) +
#   geom_sf(data = world,fill = NA) +
#   coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
#   labs(x = "",y = "") +
#   theme_bw()

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")

ggplot(data = MEM.map %>%
         mutate(var = factor(var,
                             levels = c("gpp","npp","nep"))) %>%
         filter(var %in% c("gpp","nep"))) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff.m),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  geom_sf(data = Amazon.shp,fill = NA, color = "red", fill = NA) +
  # geom_rect(data = data.frame(xmin = -55, xmax = -45,
  #                             ymin = -15, ymax = -5),
  #           aes(xmin = xmin,xmax = xmax,
  #               ymin = ymin,ymax = ymax), fill = NA, color = "black", size = 1) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient2(limits = c(-0.1,0.1)*5,breaks = c(-0.5,0.5),
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "white",high = "darkgreen") +
  facet_grid(~ var) +
  labs(x = "",y = "", fill = "Cflux difference") +
  theme_minimal() +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) +
  theme(text = element_text(size = 20),
        legend.position = "bottom")


diff2plot <- all.period.wide %>%
  ungroup() %>%
  dplyr::select(model,var,lat,lon,diff) %>%
  pivot_longer(cols = c(diff))

ggplot(data = diff2plot %>%
         filter(model %in%
                  c("ORCHIDEE","JULES","CLM5.0"))) +
  geom_density(aes(x= value,
                   fill = model),color = NA,
               alpha = 0.3) +
  # geom_density(aes(x= value),color = NA,
  #              alpha = 0.3, fill = "black") +
  facet_grid(var ~ name,scales = "free") +
  geom_vline(xintercept = 0) +
  theme_bw()

MEM.mean.diff <-
  diff2plot %>%
  group_by(var,model) %>%
  summarise(m = mean(value,na.rm = TRUE),
            .groups = "keep")

MEM.mean.diff %>%
  group_by(var) %>%
  summarise(m = mean(m,na.rm = TRUE))

ggplot(data = MEM.mean.diff) +
  geom_boxplot(aes(x = var, y = m)) +
  theme_bw()
