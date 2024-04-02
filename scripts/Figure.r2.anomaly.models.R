rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)

climate.anomalies <- readRDS("./outputs/climate.anomalies.ERA5.RDS") %>%
  group_by(variable) %>%
  mutate(anomaly.month = value - mean.month - mean(value)) %>%
  dplyr::select(-c(time,slope,intercept,mean.obs,
                   detrended,mean.month,reconstructed)) %>%
  filter(year >= 1994)

RS <- readRDS("./outputs/RSanomalies.ERA5.RDS") %>%
  dplyr::select(year,month,pred.m,anomaly,anomaly.m,mean.pred) %>%
  mutate(source = "RS")

Trendy <- readRDS("./outputs/Trendy.data.rspld.ERA5.pred.RDS") %>%
  filter(year >= 1994) %>%
  group_by(model,year,month) %>%
  summarise(pred.m = mean(pred),
            .groups = "keep") %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred.m),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  mutate(mean.obs = slope*(time) + intercept) %>%
  # mutate(mean.obs = mean(pred.m)) %>%
  mutate(detrended = pred.m - mean.obs) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  mutate(mean.pred = slope*time + intercept) %>%
  ungroup() %>%
  dplyr::select(year,month,pred.m,anomaly,anomaly.m,mean.pred) %>%
  mutate(source = "Trendy")



Windows = 1:12
df.r2 <- data.frame()

for (X in 1:2){

  for (Window in Windows){


    climate.sum.anomaly <- climate.anomalies %>%
      group_by(variable) %>%
      mutate(value.rm = rollapply(value, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"),
             anomaly.month.rm = rollapply(anomaly.month, width=Window,
                                          FUN=function(x) mean(x, na.rm=TRUE),
                                          partial=TRUE, fill=NA, align="center"),
             anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                      FUN=function(x) mean(x, na.rm=TRUE),
                                      partial=TRUE, fill=NA, align="center")) %>%
      pivot_wider(names_from = variable,
                  values_from = c(value,value.rm,anomaly,anomaly.m,anomaly.m.rm,
                                  anomaly.month,anomaly.month.rm))

    GPPanomalies <- bind_rows(RS,
                              Trendy) %>%
      group_by(source) %>%
      mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                    FUN=function(x) mean(x, na.rm=TRUE),
                                    partial=TRUE, fill=NA, align="center"),
             anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                      FUN=function(x) mean(x, na.rm=TRUE),
                                      partial=TRUE, fill=NA, align="center"),
             pred.m.rm = rollapply(pred.m, width=Window,
                                   FUN=function(x) mean(x, na.rm=TRUE),
                                   partial=TRUE, fill=NA, align="center"))


    combined <- climate.sum.anomaly %>%
      left_join(GPPanomalies %>%
                  dplyr::select(year,month,source,anomaly.m,anomaly.m.rm),
                by = c("year","month")) %>%
      mutate(timing = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                                year == 2016 & month %in% c(1:3) ~ "2015",
                                year == 2015 & month %in% c(10:12) ~ "2015",
                                year == 1997 & month %in% 10:12 ~ "1997",
                                year == 1998 & month %in% 1:4 ~ "1997",
                                TRUE ~ NA_character_))

    df.r2 <- bind_rows(df.r2,
                       combined %>%
                         group_by(source) %>%
                         summarise(r2 = summary(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[["adj.r.squared"]],
                                   slope2 = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[3],
                                   slope = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[2],
                                   intercept = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[1],
                                   .groups = "keep") %>%
                         mutate(Win = Window,
                                X))


  }
}
ggplot(data = df.r2) +
  geom_line(aes(x = Win,
                y = r2,
                color = source,
                linetype = as.factor(X))) +
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(1,12),
                     breaks = 1:12) +
  scale_color_manual(values = c("black","darkblue")) +
  labs(x = "",y = "", linetype = "Model order") +
  theme(text = element_text(size = 20),
        legend.position = c(0.1,0.8)) +
  guides(color = "none")

df.r2 %>% filter(Win == 6)


################################################################################
rm(list = ls())

Windows = 1:12
df.r2 <- data.frame()

RS <- readRDS("./outputs/RSanomalies.ERA5.RDS") %>%
  dplyr::select(year,month,pred.m,anomaly,anomaly.m,mean.pred) %>%
  mutate(source = "RS")

Trendy <- readRDS("./outputs/Trendy.data.rspld.ERA5.pred.RDS") %>%
  filter(year >= 1994) %>%
  group_by(model,year,month) %>%
  summarise(pred.m = mean(pred),
            .groups = "keep") %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred.m),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  mutate(mean.obs = slope*(time) + intercept) %>%
  # mutate(mean.obs = mean(pred.m)) %>%
  mutate(detrended = pred.m - mean.obs) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  mutate(mean.pred = slope*time + intercept) %>%
  ungroup() %>%
  dplyr::select(year,month,pred.m,anomaly,anomaly.m,mean.pred) %>%
  mutate(source = "Trendy")

all <- bind_rows(RS,
                 Trendy)

for (Window in Windows){

  print(Window)

  cdf <- all %>%
    group_by(source) %>%
    mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"),
           anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                    FUN=function(x) mean(x, na.rm=TRUE),
                                    partial=TRUE, fill=NA, align="center"),
           pred.m.rm = rollapply(pred.m, width=Window,
                                 FUN=function(x) mean(x, na.rm=TRUE),
                                 partial=TRUE, fill=NA, align="center"))


  all.wide <- cdf %>%
    pivot_wider(names_from = source,
                values_from = c(anomaly,
                                anomaly.rm,
                                anomaly.m,
                                mean.pred,
                                pred.m,
                                anomaly.m.rm,
                                pred.m.rm))


  df.r2 <- bind_rows(df.r2,
                     data.frame(r2.pred = summary(lm(data = all.wide,
                                                     formula = pred.m.rm_Trendy ~ pred.m.rm_RS))[["r.squared"]],
                                r2.anomaly = summary(lm(data = all.wide,
                                                        formula = anomaly.m.rm_Trendy ~ anomaly.m.rm_RS))[["r.squared"]],
                                Window))

}



ggplot(data = df.r2) +
  geom_line(aes(x = Window,
                y = r2.pred)) +
  geom_line(aes(x = Window,
                y = r2.anomaly), color = "darkblue") +
  theme_bw() +

  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(1,12),
                     breaks = 1:12) +
  scale_color_manual(values = c("black","darkblue")) +
  labs(x = "",y = "", linetype = "Model order") +
  theme(text = element_text(size = 20),
        legend.position = c(0.1,0.8)) +
  guides(color = "none")
