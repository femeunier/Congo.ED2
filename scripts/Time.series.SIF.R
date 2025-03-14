rm(list = ls())

library(ggplot2)
library(zoo)
library(dplyr)

Amazon.coord <- readRDS("./outputs//Amazon.coord.ILF.RDS") %>%
  filter(model == "DLEM") %>%
  mutate(lon.lat = paste0(lon,".",lat))

all.df <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/data/GPP/monthly/SIF.GPP.2023.RDS") %>%
  mutate(lon = case_when(abs(lon - floor(lon)-0.275) < 1e-6  ~ round(floor(lon) + 0.25,digits = 2),
                         abs(lon - floor(lon) - 0.775) < 1e-6  ~ round(floor(lon) + 0.75,digits = 2),
                         abs(lon - floor(lon)- 0.775) < 1e-6  ~ round(floor(lon) + 0.25,digits = 2),
                         abs(lon - floor(lon)- 0.725) < 1e-6  ~ round(floor(lon) + 0.75,digits = 2),
                         TRUE ~ lon)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

all.df %>%
  filter(year == 2021)

all.df.filtered <- all.df %>%
  filter(lon.lat %in% Amazon.coord[["lon.lat"]])

all.df.filtered %>%
  filter(year == 2021)

df.sum <- all.df.filtered %>%
  group_by(year,month) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            N = n(),
            .groups = "keep")

plot(df.sum$N,
     type = "l")

Window = 6
df.sum.anomaly <- df.sum %>%
  filter(year >= 2014) %>%
  ungroup() %>%
  mutate(time = year + (month - 1/2)/12) %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%

  mutate(mean.obs =  intercept + slope *time) %>%
  # mutate(mean.obs = mean(value.m,
  #                        na.rm = TRUE)) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  ungroup() %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         value.m.rm = rollapply(value.m, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="right"))

ggplot(data = df.sum.anomaly) +
  geom_rect(data = data.frame(xmin = 2023 + (6.5)/12, xmax = 2024 + (5.5)/12,
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = NA, fill = "lightgrey",alpha = 0.5) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = mean.obs),
            color = "grey") +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = reconstructed),
            color = "grey",
            linetype = 2) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m)) +
  scale_x_continuous(limits = c(2015,2025)) +
  theme_bw()

ggplot(data = df.sum.anomaly) +
  geom_rect(data = data.frame(xmin = 2023 + (6.5)/12, xmax = 2024 + (5.5)/12,
                              ymin = -Inf, ymax = Inf),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            color = NA, fill = "lightgrey",alpha = 0.5) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.m)) +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw()
