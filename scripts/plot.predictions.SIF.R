rm(list = ls())

library(dplyr)
library(caret)
library(xgboost)
library(ggthemes)
library(sf)

Window = 6

df.all.pred.sum <- readRDS("./outputs/all.predictions.SIF.sum.ILF.RDS") %>%
  filter(year >= 1994) %>%
  filter(product %in% c("SIF","SIF2",
                        "VOD","NIR"))

df.all.pred.sum %>%
  group_by(product) %>%
  summarise(m = min(year[!is.na(obs)]),
            M = max(year[!is.na(obs)]))


droughts <- data.frame(x1 = c(1997,2009,2004,2015,2023) + 0.5/12,
                       x2 = c(1998,2010,2005,2016,2023) + 11.5/12)

ggplot() +
  geom_line(data = df.all.pred.sum,
            aes(x = year + (month - 1/2)/12,
                y = pred,
                color = product)) +
  theme_bw()

ggplot() +
  geom_line(data = df.all.pred.sum,
            aes(x = year + (month - 1/2)/12,
                y = pred.anomaly,
                color = product)) +
  scale_x_continuous(limits = c(1995,2024)) +
  theme_bw()

df.all.pred.MEM <- df.all.pred.sum %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred,na.rm = TRUE),
            pred.anomaly.m = mean(pred.anomaly,na.rm = TRUE),
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
        "./outputs/RSanomalies.RDS")
