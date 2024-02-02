rm(list = ls())

library(dplyr)
library(ggplot2)

climate.sum.anomaly <- readRDS("./outputs/climate.anomalies.RDS") %>%
  dplyr::select(-c(value,time,slope,intercept,mean.obs,
                   detrended,mean.month,reconstructed)) %>%
  filter(year >= 1990) %>%
  pivot_wider(names_from = variable,
              values_from = c(anomaly,anomaly.m))

CC.anomaly <- readRDS("./outputs/CC.anomaly.RDS")

combined <- climate.sum.anomaly %>%
  left_join(CC.anomaly,
            by = c("year","month")) %>%
  mutate(timing = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2016 & month %in% c(1:3) ~ "2015",
                            year == 2015 & month %in% c(10:12) ~ "2015",
                            year == 1997 & month %in% 10:12 ~ "1997",
                            year == 1998 & month %in% 1:4 ~ "1997",
                            TRUE ~ NA_character_))

ggplot(data = combined ,
       aes(x = anomaly.m_tmp,
           y = anomaly.m_gpp)) +
  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_point() +
  geom_point(data = combined %>%
               filter(!is.na(timing)),
             aes(color = as.factor(timing))) +


  geom_smooth(method = "lm", formula= y ~ x,
              se = FALSE,
              color = "red") +
  theme_bw()

