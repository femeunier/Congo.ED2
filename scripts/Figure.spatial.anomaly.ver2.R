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
library(ggridges)
library(wesanderson)

coord.list <- readRDS("./outputs/Amazon.coord.ILF.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat,digits = 2)))

A <- readRDS("./outputs/Trendy.data.rspld.pred.RDS") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat,digits = 2))) %>%
  filter(lon.lat %in% coord.list[["lon.lat"]])


A.MEM <- A %>%
  group_by(year, month, lat, lon) %>%
  summarise(pred = mean(pred, na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")

A.MEM.ts <- A.MEM %>%
  filter(year >= 1994) %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")


A.MEM.ts.group <- A.MEM.ts %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(10:12) ~ "2015",
                            year == 1997 & month %in% 10:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",
                            TRUE ~ NA_character_)) %>%
  mutate(pred.m = case_when(!is.na(groups) ~ pred.m,
                            TRUE ~ NA_real_)) %>%
  mutate(pred.m = case_when((year == 1998 & month == 5) |
                              (year == 2016 & month == 4) ~ NA,
                            TRUE ~ pred.m)) %>%
  arrange(year,month)

A.MEM.dt <- A.MEM %>%
  filter(year >= 1994) %>%
  group_by(lat,lon) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred ~ time))[2],
         intercept = coef(lm(pred ~ time))[1]) %>%
  mutate(detrended = pred - (slope*(time) + intercept)) %>%
  group_by(lat,lon,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  ungroup() %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))



A.MEM.dt.sum <- A.MEM.dt %>%
  group_by(year,month) %>%
  summarise(m = mean(anomaly.m),
            .groups = "keep")


################################################################################

B <- readRDS("./outputs/all.predictions.SIF.ILF.RDS") %>%
  filter(year >= 1994) %>%
  filter(product %in% c("SIF","SIF2","VOD","NIR"))

B.MEM <- B %>%
  group_by(year, month, lat, lon) %>%
  summarise(pred = mean(pred, na.rm = TRUE),
            .groups = "keep")

B.MEM.ts <- B.MEM %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")



B.MEM.dt <- B.MEM %>%
  filter(year >= 1994) %>%
  group_by(lat,lon) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred ~ time))[2],
         intercept = coef(lm(pred ~ time))[1]) %>%
  mutate(detrended = pred - (slope*(time) + intercept)) %>%
  group_by(lat,lon,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  ungroup() %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))

###############################################################################
# Combined

all.MEM.dt <- bind_rows(A.MEM.dt %>% mutate(source = "Trendy"),
                        B.MEM.dt %>% mutate(source = "RS"))

map2plot <- all.MEM.dt %>%
  filter(year == 2023,
         month == 10)
ggplot(data = map2plot) +
  geom_tile(aes(x=lon,y = lat,
                fill = anomaly.m),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, -30), ylim = c(-25, 10), expand = FALSE) +
  scale_fill_gradient2(limits = c(-2,1)*2.5,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  facet_wrap(~ source) +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

map2plot.wide <- map2plot %>%
  dplyr::select(lat,lon,source,anomaly.m) %>%
  pivot_wider(names_from = source,
              values_from = anomaly.m)

scatter <- ggplot(data = map2plot.wide,
       aes(x = Trendy, y = RS)) +
  geom_point(size = 0.5, color = "grey") +
  stat_smooth(method = "lm", color = "black",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  labs(x = "",y = "") +
  theme(text = element_text(size = 20))

map2plot.wide %>%
  ungroup() %>%
  summarise(r2 = summary(lm(RS ~ Trendy))[["r.squared"]],
            slope = coef(lm(RS ~ Trendy))[2],
            intercept = coef(lm(RS ~ Trendy))[1])

ggExtra::ggMarginal(scatter,
                    type = "density",
                    fill = "grey", color = NA)


################################################################################


ggplot(data = all.MEM.dt %>%
         filter((year == 2023 & month == 10) |
                  (year == 2016 & month == 1) |
                  (year == 1998 & month == 3)),
       aes(x = anomaly.m,
           y = interaction(source),
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 1,
                               scale = 1.5) +
  scale_fill_gradient2(limits = c(-10,5)*1,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  geom_vline(xintercept = 0,linetype = 2) +
  facet_wrap(~ year, nrow = 1) +
  scale_x_continuous(limits = c(-30,10)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid = element_blank())

ggplot(data = map2plot,
       aes(x = anomaly.m,
           y = as.factor(source),
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 0.5,
                               scale = 1) +
  scale_fill_gradient2(limits = c(-2,1)*2.5,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  geom_vline(xintercept = 0,linetype = 2) +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-10,5)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid = element_blank())

all.MEM.dt %>%
  filter((year == 2023 & month == 10) |
           (year == 2016 & month == 1) |
           (year == 1998 & month == 3)) %>%
  group_by(year,source) %>%
  summarise(frac = sum(anomaly.m < -0)/length(anomaly.m)*100,
            .groups = "keep")

all.MEM.dt %>%
  group_by(source, year, month) %>%
  summarise(frac = sum(anomaly.m < 0)/length(anomaly.m)*100,
            .groups = "keep") %>%
  arrange(desc(frac)) %>%
  group_by(source) %>%
  slice_head(n = 10)


all.MEM.dt %>%
  filter(year == 2023,
         month == 10) %>%
  group_by(source) %>%
  summarise(frac = sum(anomaly.m < -3)/length(anomaly.m)*100,
            .groups = "keep") %>%
  arrange(desc(frac)) %>%
  group_by(source) %>%
  slice_head(n = 10)



################################################################################
# Per drought

all.MEM.dt.groups <- all.MEM.dt %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2016 & month %in% c(1:3) ~ "2015",
                            year == 2015 & month %in% c(10:12) ~ "2015",
                            year == 1997 & month %in% 10:12 ~ "1997",
                            year == 1998 & month %in% 1:4 ~ "1997",
                            TRUE ~ NA_character_)) %>%
  filter(!is.na(groups))

all.MEM.dt.groups.sum <- all.MEM.dt.groups %>%
  group_by(source,lon,lat,groups) %>%
  summarise(anomaly.m = mean(anomaly.m),
            .groups = "keep")

ggplot(data = all.MEM.dt.groups.sum %>%
         filter(groups == "2023")) +
  geom_tile(aes(x=lon,y = lat,
                fill = anomaly.m),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-85, -30), ylim = c(-25, 10), expand = FALSE) +
  scale_fill_gradient2(limits = c(-2,1)*2.5,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  facet_wrap(~ source) +
  theme_map() +
  guides(fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

ggplot(data = all.MEM.dt.groups.sum,
       aes(x = anomaly.m,
           y = interaction(source),
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 1,
                               scale = .9) +
  scale_fill_gradient2(limits = c(-2,1)*2.5,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  geom_vline(xintercept = 0,linetype = 2) +
  facet_wrap(~ groups, nrow = 1) +
  scale_x_continuous(limits = c(-10,5)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid = element_blank())

ggplot(data = all.MEM.dt.groups.sum %>%
         filter(groups == "2023"),
       aes(x = anomaly.m,
           y = as.factor(source),
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 0.5,
                               scale = 1) +
  scale_fill_gradient2(limits = c(-2,1)*2.5,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  geom_vline(xintercept = 0,linetype = 2) +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-10,5)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid = element_blank())

all.MEM.dt.groups.sum %>%
  group_by(source,groups) %>%
  summarise(frac = mean(anomaly.m),
            .groups = "keep")


all.MEM.dt.groups.sum %>%
  group_by(source,groups) %>%
  summarise(frac = sum(anomaly.m < -0)/length(anomaly.m)*100,
            .groups = "keep")

