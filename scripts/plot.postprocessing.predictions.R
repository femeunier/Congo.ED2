rm(list = ls())

library(tidyr)
library(ggplot2)
library(dplyr)

files <- c("df.all.ts.RDS",
           "df.all.year.RDS",
           "df.all.ts.MEM.RDS",
           "df.all.year.MEM.RDS",
           "df.r2.predictions.RDS")

for (cfile in files){
  system2("rsync",
          c("-avz",paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
            "./outputs/"))
}

################################################################################
# Timeseries

predictions.XGB.sum <- readRDS("./outputs/df.all.ts.RDS")

ggplot(predictions.XGB.sum %>%
         filter(model == "CABLE-POP")) +

  geom_line(aes(x = year + (month-1/2)/12, y = obs.m, group = var),
            color = "black") +

  geom_line(aes(x = year + (month-1/2)/12, y = pred.m, color = biome,
                group = interaction(biome,var)),
            linetype = 2) +

  geom_line(aes(x = year + (month-1/2)/12, y = pred.ERA5.m, color = biome,
                group = interaction(biome,var)),
            linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  scale_x_continuous(limits = c(2017,2023)) +

  facet_grid(biome ~ continent) +
  theme_bw()


MEM <- readRDS("./outputs/df.all.ts.MEM.RDS")

ggplot(MEM %>%
         filter(var == "nep")) +

  geom_line(aes(x = year + (month-1/2)/12, y = obs.MEM.m, group = var),
            color = "black") +

  geom_line(aes(x = year + (month-1/2)/12, y = pred.MEM.m, group = interaction(biome,var),color = biome),
            linetype = 2) +

  geom_line(aes(x =  year + (month-1/2)/12, y = pred.ERA5.MEM.m, group = interaction(biome,var),color = biome),
            linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  scale_x_continuous(limits = c(2017,2023)) +

  facet_grid(biome ~ continent) +
  theme_bw()


################################################################################
# yearly timeseries

predictions.XGB.sum.year <- readRDS("./outputs/df.all.year.RDS")

ggplot(predictions.XGB.sum.year %>%
         filter(model == "CABLE-POP")) +

  geom_line(aes(x = year, y = obs.m, group = var),
            color = "black") +

  geom_line(aes(x = (year), y = pred.m, color = biome,
                group = interaction(biome,var)),
            linetype = 1) +

  geom_line(aes(x = (year), y = pred.ERA5.m, color = biome,
                group = interaction(biome,var)),
            linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  # scale_x_continuous(limits = c(2017,2023)) +

  facet_grid(biome ~ continent) +
  theme_bw()

MEM.year <-  readRDS("./outputs/df.all.year.MEM.RDS")

ggplot(MEM.year %>%
         filter(var == "nep",
                continent %in% c("America","Africa"))) +
  # geom_ribbon(aes(x = (month), y = pred.m, fill = biome,
  #                 ymin = pred.m - pred.sd, ymax = pred.m + pred.sd),
  #             color = NA,alpha = 0.5) +
  # geom_ribbon(aes(x = (month), y = pred.ERA5.m, fill = biome,
  #                 ymin = pred.ERA5.m - pred.ERA5.sd, ymax = pred.ERA5.m + pred.ERA5.sd),
  #             color = NA,alpha = 0.5) +

  geom_line(aes(x = year, y = obs.MEM.m, group = var),
            color = "black") +

  geom_line(aes(x = (year), y = pred.MEM.m, group = interaction(biome,var),color = biome),
            linetype = 2) +

  geom_line(aes(x = year, y = pred.ERA5.MEM.m, group = interaction(biome,var),
                color = biome),
            linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +

  # scale_x_continuous(limits = c(2010,2023)) +

  facet_grid(biome ~ continent) +
  theme_bw()



################################################################################
# r2

df.r2 <- readRDS("./outputs/df.r2.predictions.RDS")


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df.r2 %>%
         filter(model == "CLASSIC")) +
  geom_raster(aes(x=lon,y = lat,
                  fill = r2.ERA5),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-150, 180), ylim = c(-25, 25), expand = FALSE) +
  facet_wrap(~ var) +
  labs(x = "",y = "")

df.r2.long <- df.r2 %>%
  pivot_longer(cols = c(r2,r2.ERA5))

ggplot(data = df.r2.long) +
  geom_density(aes(x = value,fill = var), alpha = 0.5) +
  facet_grid(model ~ name,scales = "free") +
  theme_bw()
