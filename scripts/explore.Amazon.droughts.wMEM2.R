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

w <- readRDS("./outputs/weights.Trendy.RDS") %>%
  filter(slope > 0,
         basin == "Amazon",
         model == "SIF2") %>%
  dplyr::select(trendy.model,r) %>%
  rename(model = trendy.model)

A.MEM <- A %>%
  left_join(w,
            by = c("model")) %>%
  filter(!is.na(r)) %>%
  group_by(year, month, lat, lon) %>%
  summarise(pred = weighted.mean(pred,r, na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

A.MEM.sum <- A.MEM %>%
  filter(year > 1990) %>%
  group_by(lat,lon) %>%
  summarise(pred.m = mean(pred),
            .groups = "keep")

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
ggplot(data = A.MEM.sum) +
  geom_raster(aes(x=lon,y = lat,
                  fill = pred.m),alpha = 0.3) +
  geom_sf(data = world,fill = NA,
          color = "grey") +

  # geom_point(data = coord.list,
  #            aes(x = lon, y = lat),
  #            shape = "+") +

  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(limits = c(2.5,3.5),
                      oob = scales::squish,
                      low = "white",high = "darkgreen") +

  labs(x = "",y = "") +
  theme_bw()

hist(A.MEM.sum$pred.m)

################################################################################
A.diff <-  A.MEM %>%
  filter(year == 2023,
         month == 9) %>%
  group_by(lon,lat) %>%
  summarise(pred.recent = mean(pred),
            .groups = "keep") %>%
  left_join(A.MEM.sum,
            by = c("lon","lat")) %>%
  mutate(diff = pred.recent - pred.m)

ggplot(data = A.diff) +
  geom_tile(aes(x=lon,y = lat,
                fill = diff),alpha = 0.3) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*0.5,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +

  labs(x = "",y = "") +
  theme_bw()

hist(A.diff$diff)
summary(A.diff$diff)


A.MEM.ts <- A.MEM %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")

ggplot(data = A.MEM.ts) +
  geom_rect(aes(xmin = 2023+ 1/12,
                xmax = 2024,
                ymin = -Inf,
                ymax = Inf), fill = "grey", color = NA, alpha = 0.3) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m)) +
  theme_bw()

Seasonal.Cycle.SIF <- readRDS("./outputs/Seasonal.Cycle.SIF.RDS")


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


ggplot() +

  geom_line(data = A.MEM.ts %>%
              filter(year %in% 1990:2023),
            aes(x = month,
                y = pred.m,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = A.MEM.ts %>%
              filter(year %in% 1992:2020) %>%
              group_by(month) %>%
              summarise(pred.m = mean(pred.m),
                        .groups = "keep"),
            aes(x = month,
                y = pred.m), color = "black") +

  geom_line(data = Seasonal.Cycle.SIF %>%
              filter(basin == "Amazon",
                     model == "SIF2"),
            aes(x = month,
                y = value.m),
            color = "black",
            linetype = 2) +

  geom_line(data = A.MEM.ts.group %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = pred.m,
                color = as.factor(groups)),
            show.legend = TRUE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 20))

A.MEM.ts.dt <- A.MEM.ts %>%
  ungroup() %>%
  filter(year >= 1990) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  mutate(mean.pred = slope*(time) + intercept) %>%
  mutate(detrended = pred.m - mean.pred) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended)) %>%
  ungroup() %>%
  mutate(anomaly = detrended - mean.month) %>%
  ungroup() %>%
  mutate(anomaly.m = anomaly/sd(anomaly))

A.MEM.ts.dt.groups <- A.MEM.ts.dt %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(10:12) ~ "2015",
                            year == 1997 & month %in% 10:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",
                            TRUE ~ NA_character_)) %>%
  mutate(anomaly.m = case_when(!is.na(groups) ~ anomaly.m,
                            TRUE ~ NA_real_)) %>%
  mutate(anomaly.m = case_when((year == 1998 & month == 5) |
                              (year == 2016 & month == 4) ~ NA,
                            TRUE ~ anomaly.m)) %>%
  arrange(year,month)


ggplot() +

  geom_line(data = A.MEM.ts.dt %>%
              filter(year %in% 1990:2023),
            aes(x = month,
                y = anomaly.m,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = A.MEM.ts.dt.groups %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = anomaly.m,
                color = as.factor(groups)),
            show.legend = FALSE) +

  geom_hline(yintercept = 0,
             color = "black", linetype = 2) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  theme_bw() +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 20))


################################################################################

slope <- unique(A.MEM.ts.dt$slope) ; intercept <- unique(A.MEM.ts.dt$intercept)
mean.months <- A.MEM.ts.dt %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred.m),
            .groups = "keep") %>%
  mutate(time = year + (month -1/2)/12) %>%

  group_by(month) %>%
  summarise(mean.month = mean(pred.m - (slope*(time) +intercept)),
            .groups = "keep")

A.MEM.dt <- A.MEM %>%
  filter(year > 1990) %>%
  left_join(mean.months,
            by = "month") %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(detrended = pred - (slope*(time) + intercept)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  ungroup() %>%
  mutate(anomaly.m = anomaly/sd(anomaly))


ggplot(data = A.MEM.dt %>%
         filter(year == 1997,
                month == 11)) +
  geom_tile(aes(x=lon,y = lat,
                fill = anomaly),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  coord_sf(xlim = c(-90, -30), ylim = c(-25, 12), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*1,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  ggtitle("November 1997") +
  theme_map()

ggplot(data = A.MEM.dt %>%
         filter(year == 2016,
                month == 1)) +
  geom_tile(aes(x=lon,y = lat,
                fill = anomaly),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  coord_sf(xlim = c(-90, -30), ylim = c(-25, 12), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1)*1,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  ggtitle("January 2016") +
  theme_map()

ggplot(data = A.MEM.dt %>%
         filter(year == 2023,
                month == 10)) +
  geom_tile(aes(x=lon,y = lat,
                fill = anomaly.m),alpha = 1) +
  geom_sf(data = world,fill = NA, color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black", fill = NA) +

  geom_point(data = A.MEM.dt %>%
               filter(anomaly.m <= -2,
                      year == 2023,
                      month == 10),
             aes(x = lon, y = lat), shape = "+") +

  coord_sf(xlim = c(-90, -30), ylim = c(-25, 12), expand = FALSE) +
  scale_fill_gradient2(limits = c(-2,2)*1,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  labs(x = "",y = "") +
  ggtitle("October 2023") +
  theme_map() +
  theme(text = element_text(size = 20),
        legend.position = "none")

ggplot(data = A.MEM.dt %>%
         filter((year == 2023 & month == 10) |
                (year == 2016 & month == 1) |
                (year == 1997 & month == 11)),
       aes(x = anomaly.m,
           y = as.factor(year),
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 0.5,
                               scale = 0.9) +
  scale_fill_gradient2(limits = c(-1,1)*1,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  geom_vline(xintercept = 0,linetype = 2) +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-6,2)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid = element_blank())

A.MEM.dt %>%
  filter((year == 2023 & month == 10) |
           (year == 2016 & month == 1) |
           (year == 1997 & month == 11)) %>%
  group_by(year) %>%
  summarise(frac = sum(anomaly.m < -1)/length(anomaly.m)*100,
            .groups = "keep")



ggplot(data = A.MEM.dt %>%
         filter((year == 2023 & month == 10)),
       aes(x = anomaly.m,
           y = 0,
           fill = stat(x))) +
  geom_density_ridges_gradient(show.legend = FALSE, alpha = 0.5) +
  scale_fill_gradient2(limits = c(-2,2)*1,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "grey",high = "darkgreen") +
  geom_vline(xintercept = 0,linetype = 2) +
  labs(x = "",y = "") +
  scale_x_continuous(limits = c(-6,2)) +
  scale_y_continuous(breaks = c()) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        panel.grid = element_blank())



