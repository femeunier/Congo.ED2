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

coord.list <- readRDS("./outputs/Coord.ILF.ERA5.RDS") %>%
  # filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",
                          round(lat,digits = 2)))

A <- readRDS("./outputs/Trendy.data.rspld.ERA5.pred.RDS") %>%
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


A.MEM.ts <- bind_rows(A.MEM %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep"),
  data.frame(year = 2024,
             month = 3,
             pred.m = NA))

ggplot(data = A.MEM.ts) +
  geom_rect(aes(xmin = 2023+ 1/12,
                xmax = 2024 + 3/12,
                ymin = -Inf,
                ymax = Inf), fill = "grey", color = NA, alpha = 0.3) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m)) +
  theme_bw()

A.MEM.ts.group <- A.MEM.ts %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:3) ~ "2023",

                            # year == 2009 & month %in% c(8:12) ~ "2010",
                            # year == 2010 & month %in% c(1:5) ~ "2010",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA_character_)) %>%
  mutate(pred.m = case_when(!is.na(groups) ~ pred.m,
                            TRUE ~ NA_real_)) %>%
  mutate(pred.m = case_when((year == 1998 & month == 5) |
                              (year == 2024 & month == 3) |
                              (year == 2016 & month == 4) ~ NA,
                            TRUE ~ pred.m)) %>%
  arrange(year,month)


A.MEM.ts.dt <- A.MEM.ts %>%
  ungroup() %>%
  filter(year >= 1994) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  mutate(mean.pred = slope*(time) + intercept) %>%
  mutate(detrended = pred.m - mean.pred) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly,na.rm = TRUE))

A.MEM.ts.dt.groups <- A.MEM.ts.dt %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:3) ~ "2023",

                            # year == 2009 & month %in% c(8:12) ~ "2010",
                            # year == 2010 & month %in% c(1:5) ~ "2010",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA_character_)) %>%
  mutate(anomaly.m = case_when(!is.na(groups) ~ anomaly.m,
                               TRUE ~ NA_real_)) %>%
  mutate(anomaly.m = case_when((year == 1998 & month == 5) |
                                 (year == 2024 & month == 3) |
                                 (year == 2016 & month == 4) ~ NA,
                               TRUE ~ anomaly.m)) %>%
  arrange(year,month)


################################################################################
# Now RS

B <- readRDS("./outputs/all.predictions.SIF.ILF.ERA5.RDS") %>%
  filter(year >= 1994) %>%
  filter(product %in% c("SIF","SIF2","VOD","NIR"))

B.MEM <- B %>%
  group_by(year, month, lat, lon) %>%
  summarise(pred = mean(pred, na.rm = TRUE),
            .groups = "keep")


B.MEM.ts <- bind_rows(B.MEM %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep"),
  data.frame(year = 2024,
             month = 3,pred.m = NA))

ggplot(data = B.MEM.ts) +
  geom_rect(aes(xmin = 2023+ 1/12,
                xmax = 2024,
                ymin = -Inf,
                ymax = Inf), fill = "grey", color = NA, alpha = 0.3) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m)) +
  theme_bw()

B.MEM.ts.group <- B.MEM.ts %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:3) ~ "2023",

                            # year == 2009 & month %in% c(8:12) ~ "2010",
                            # year == 2010 & month %in% c(1:5) ~ "2010",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA_character_)) %>%
  mutate(pred.m = case_when(!is.na(groups) ~ pred.m,
                            TRUE ~ NA_real_)) %>%
  mutate(pred.m = case_when((year == 1998 & month == 5) |
                              (year == 2023 & month == 3) |
                              (year == 2016 & month == 4) ~ NA,
                            TRUE ~ pred.m)) %>%
  arrange(year,month)


all.MEM.ts <- bind_rows(A.MEM.ts %>% mutate(source = "Trendy"),
                        B.MEM.ts %>% mutate(source = "RS"))

all.MEM.ts.group <- bind_rows(A.MEM.ts.group %>% mutate(source = "Trendy"),
                              B.MEM.ts.group %>% mutate(source = "RS"))


B.MEM.ts.dt <- B.MEM.ts %>%
  ungroup() %>%
  filter(year >= 1994) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  mutate(mean.pred = slope*(time) + intercept) %>%
  mutate(detrended = pred.m - mean.pred) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended,na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly,na.rm = TRUE))

B.MEM.ts.dt.groups <- B.MEM.ts.dt %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:3) ~ "2023",

                            # year == 2009 & month %in% c(8:12) ~ "2010",
                            # year == 2010 & month %in% c(1:5) ~ "2010",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA_character_)) %>%
  mutate(anomaly.m = case_when(!is.na(groups) ~ anomaly.m,
                               TRUE ~ NA_real_)) %>%
  mutate(anomaly.m = case_when((year == 1998 & month == 5) |
                                 (year == 2024 & month == 3) |
                                 (year == 2016 & month == 4) ~ NA,
                               TRUE ~ anomaly.m)) %>%
  arrange(year,month)


all.MEM.ts.dt <- bind_rows(A.MEM.ts.dt %>% mutate(source = "Trendy"),
                           B.MEM.ts.dt %>% mutate(source = "RS"))

all.MEM.ts.dt.groups <- bind_rows(A.MEM.ts.dt.groups %>% mutate(source = "Trendy"),
                                  B.MEM.ts.dt.groups %>% mutate(source = "RS"))


SC.av <- all.MEM.ts %>%
  filter(year %in% 1994:2023) %>%
  group_by(source,month) %>%
  summarise(pred.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep") %>%
  left_join(all.MEM.ts.dt %>%
              mutate(pred.sd = anomaly/anomaly.m)  %>%
              group_by(source,month) %>%
              summarise(pred.sd = mean(pred.sd,na.rm = TRUE),
                        .groups = "keep") %>%
              dplyr::select(source, month,pred.sd),
            by = c("source","month"))

ggplot() +
  geom_line(data = all.MEM.ts %>%
              filter(year %in% 1994:2023),
            aes(x = month,
                y = pred.m,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = SC.av,
            aes(x = month,
                y = pred.m), color = "black") +
  geom_line(data = all.MEM.ts.group %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = pred.m,
                color = as.factor(groups)),
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  theme_bw() +
  labs(x = "", y = "") +
  facet_wrap(~ source) +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

ggplot(data = SC.av,
       aes(x = month,
           y = pred.m)) +
  geom_ribbon(aes(ymin = pred.m - pred.sd,
                  ymax = pred.m + pred.sd),
              color = NA, fill = "grey",
              alpha = 0.5) +
  geom_line(color = "black") +
  theme_bw() +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "", y = "") +
  facet_wrap(~ source) +
  theme(panel.grid = element_blank(),
        text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

SC.av %>%
  group_by(source) %>%
  summarise(m = mean(pred.m,na.rm = TRUE),
            m2 = mean(pred.sd,na.rm = TRUE))



ggplot() +

  geom_line(data = all.MEM.ts.dt %>%
              filter(year %in% 1994:2023),
            aes(x = month,
                y = anomaly.m,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = all.MEM.ts.dt.groups %>%
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
  facet_wrap(~ source) +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


all.MEM.ts.dt.groups %>%
  filter(!is.na(groups)) %>%
  group_by(source, groups) %>%
  summarise(m = mean(anomaly.m,
                     na.rm = TRUE))

