rm(list = ls())

library(dplyr)
library(caret)
library(xgboost)
library(ggthemes)
library(sf)

# system2("rsync",
#         c("-avz",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/*.predictions.SIF.ILF.ERA5.RDS",
#           "./outputs"))

Window = 6

df.all.pred <- readRDS("./outputs/all.predictions.SIF.ILF.ERA5.RDS") %>%
  filter(year >= 1994) %>%
  filter(product %in% c("SIF","SIF2",
                        "VOD","NIR"))


df.all.pred.sum <- df.all.pred %>%
  group_by(product,year,month) %>%
  summarise(gpp.m = mean(pred,na.rm = TRUE),
            .groups = "keep") %>%
  # filter(year >= year.min) %>%
  group_by(product) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(gpp.m ~ time))[2],
         intercept = coef(lm(gpp.m ~ time))[1]) %>%
  mutate(mean.pred = slope*(time) + intercept) %>%
  mutate(detrended = gpp.m - mean.pred) %>%
  group_by(product,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  group_by(product) %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(product) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))

df.all.pred.sum %>%
  group_by(product) %>%
  summarise(m = min(year[!is.na(gpp.m)]),
            M = max(year[!is.na(gpp.m)]))


droughts <- data.frame(x1 = c(1997,2015,2023) + 0.5/12,
                       x2 = c(1998,2016,2023) + 11.5/12)

ggplot() +
  geom_line(data = df.all.pred.sum,
            aes(x = year + (month - 1/2)/12,
                y = gpp.m,
                color = product)) +
  theme_bw()

ggplot() +
  geom_line(data = df.all.pred.sum,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.m,
                color = product)) +
  scale_x_continuous(limits = c(1995,2024)) +
  theme_bw()

df.all.pred.MEM <- df.all.pred.sum %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(gpp.m,na.rm = TRUE),
            pred.anomaly.m = mean(anomaly.m,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_line(data = df.all.pred.MEM,
            aes(x = year + (month - 1/2)/12,
                y = pred.m)) +

  scale_x_continuous(limits = c(1992,2024)) +
  theme_bw()


ggplot() +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_hline(linetype = 2, yintercept = 0, color = "black") +
  geom_line(data = df.all.pred.MEM,
            aes(x = year + (month - 1/2)/12,
                y = pred.anomaly.m)) +
  scale_x_continuous(limits = c(1992,2024)) +
  theme_bw()


df.all.pred.MEM.anomaly <- df.all.pred.MEM %>%
  ungroup() %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  mutate(mean.pred = slope*(time) + intercept) %>%
  # mutate(mean.pred =mean(pred.m)) %>%
  mutate(detrended = pred.m - mean.pred) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended)) %>%
  ungroup() %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))


ggplot() +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +
  geom_hline(linetype = 2, yintercept = 0, color = "black") +
  geom_line(data = df.all.pred.MEM.anomaly,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.m)) +
  scale_x_continuous(limits = c(1992,2024)) +
  theme_bw()

saveRDS(df.all.pred.MEM.anomaly,
        "./outputs/RSanomalies.ERA5.RDS")

saveRDS(df.all.pred.sum,
        "./outputs/RSanomalies.ERA5.product.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/plot.predictions.SIF.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
