rm(list = ls())

library(dplyr)
library(dismo)
library(tie)

Ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)

all.clim <- bind_rows(list(
  readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1901.1910.RDS") %>% mutate(timing = "past"),
  readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1961.1990.RDS") %>% mutate(timing = "norm"),
  readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRU.JRA.1991.2020.RDS") %>% mutate(timing = "present")))

clim <- all.clim %>%
  dplyr::filter(lat <= 10,lat >= -15,
                lon >= -10,lon <= 45) %>%
  mutate(Ndays.per.month = Ndays[month]) %>%
  ungroup()

clim.month <- clim %>%
  group_by(timing,lon,lat,month) %>%
  summarise(pre = mean(pre)*4*Ndays.per.month,
            tmp = mean(tmp),
            tmin = mean(tmin),
            tmax = mean(tmax),
            .groups = "keep") %>%
  distinct()

clim.month.bio <-
  clim.month %>%
  group_by(timing,lat,lon) %>%
  bow(tie(bio1, bio2, bio3, bio4,
          bio5, bio6, bio7, bio8,
          bio9, bio10, bio11, bio12,
          bio13, bio14, bio15, bio16,
          bio17, bio18, bio19,MCWD) := c(biovars(pre,
                                                 (tmin - 273.15)*10,
                                                 (tmax - 273.15)*10)[c(1:19)])) %>%
  mutate(timing = factor(timing,
                          levels = c("past","norm","present")))

Bio.long <- clim.month.bio %>%
  pivot_longer(cols = -c("lat","lon","timing"),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  mutate(variable = factor(variable,
                           levels = paste0("bio",seq(1,19))))

Bio.long.sum <- Bio.long %>%
  group_by(lat,lon,variable,timing) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(variable) %>%
  mutate(value.rel = (value.m - min(value.m,na.rm = TRUE))/
           (max(value.m,na.rm = TRUE) - min(value.m,na.rm = TRUE)))

ggplot() +
  geom_density(data = Bio.long.sum,
              aes(x = value.m,fill = timing),na.rm = TRUE, alpha = 0.4) +
  labs(x = "",y = "") +
  facet_wrap(~ variable,scales = "free") +
  theme_bw()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = Bio.long.sum,
              aes(x = lon, y = lat,fill = value.rel),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15)) +
  labs(x = "",y = "") +
  facet_grid(timing ~ variable) +
  theme_bw()


