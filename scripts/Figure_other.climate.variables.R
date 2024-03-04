rm(list = ls())

library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(corrplot)

Window = 6
climate.sum.anomaly <- readRDS("./outputs/climate.anomalies.ERA5.RDS") %>%
  group_by(variable) %>%
  mutate(anomaly.month = value - mean.month - mean(value)) %>%
  dplyr::select(-c(time,slope,intercept,mean.obs,
                   detrended,mean.month,reconstructed)) %>%
  filter(year >= 1994) %>%
  group_by(variable) %>%
  mutate(value.rm = rollapply(value, width=Window,
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE, fill=NA, align="center"),
         anomaly.month.rm = rollapply(anomaly.month, width=Window,
                                      FUN=function(x) mean(x, na.rm=TRUE),
                                      partial=TRUE, fill=NA, align="center"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"))

ggplot(data = climate.sum.anomaly) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.m.rm)) +
  facet_wrap(~variable) +
  scale_x_continuous(limits = c(1994,2024)) +
  theme_bw()


a <- climate.sum.anomaly %>%
  filter(year == 2023,
         month > 6) %>%
  group_by(variable) %>%
  summarise(m = mean(value),
            m2 = mean(anomaly.m),
            .groups = "keep")

b <- climate.sum.anomaly %>%
  filter(month > 6) %>%
  group_by(variable) %>%
  summarise(m = mean(value),
            m2 = mean(anomaly.m),
            .groups = "keep")

a %>%
  left_join(b,
            by = "variable") %>%
  mutate(diff = m.y - m.x)


A <- climate.sum.anomaly %>%
  dplyr::select(year,month,variable,anomaly.m) %>%
  pivot_wider(names_from = variable,
              values_from = c(anomaly.m)) %>%
  dplyr::select(-c(year,month))

M <- cor(A)
corrplot(M,
         type="lower")


B <- climate.sum.anomaly %>%
  dplyr::select(year,month,variable,value) %>%
  pivot_wider(names_from = variable,
              values_from = c(value)) %>%
  dplyr::select(-c(year,month))

M2 <- cor(B)
corrplot(M2,
         type="lower")
