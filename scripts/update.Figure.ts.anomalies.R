rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)

Window = 6

files <- c("RSanomalies.ERA5.RDS",
           "Trendy.data.rspld.ERA5.pred.RDS")

# for (cfile in files){
#   system2("scp",
#           c(paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }

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

all %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(source) %>%
  summarise(r2 = summary(lm(formula = pred.m ~ time))[["r.squared"]],
            intercept = coef(lm(formula = pred.m ~ time))[1],
            slope = coef(lm(formula = pred.m ~ time))[2],
            pval = summary(lm(formula = pred.m ~ time))[["coefficients"]][2,4]) %>%
  mutate(Delta_t = 2020 - 1980,
         Delta_GPP =  (2020 - 1980)*slope,
         GPP = intercept + 2000*slope) %>%
  mutate(Delta = 100*(Delta_GPP/GPP/(Delta_t/10)))


all %>%
  filter(year <= 2010,year >= 2000) %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(source) %>%
  summarise(r2 = summary(lm(formula = pred.m ~ time))[["r.squared"]],
            intercept = coef(lm(formula = pred.m ~ time))[1],
            slope = coef(lm(formula = pred.m ~ time))[2],
            pval = summary(lm(formula = pred.m ~ time))[["coefficients"]][2,4]) %>%
  mutate(Delta_t = 2020 - 1980,
         Delta_GPP =  (2020 - 1980)*slope,
         GPP = intercept + 2000*slope) %>%
  mutate(Delta = 100*(Delta_GPP/GPP/(Delta_t/10)))



droughts <- data.frame(x1 = c(1997 + 9/12,
                              # 2010 + 7/12,
                              2015 + 8/12,
                              2023 + 7/12) + 0.5/12,
                       x2 = c(1998 + 4/12 ,
                              # 2010 + 10/12,
                              2016 + 3/12,
                              2024 + 4/12) + 0.5/12)

ggplot() +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_point(data = all,
             aes(x = year + (month - 1/2)/12,
                 y = pred.m*10,
                 color = source),
             size = 0.2) +

  # geom_point(data = all %>%
  #              filter(year == 2023, month == 10),
  #            aes(x = year + (month - 1/2)/12,
  #                y = pred.m), color = "red",
  #            size = 1) +

  geom_line(data = all,
            aes(x = year + (month - 1/2)/12,
                y = mean.pred*10,
                color = source), linetype = 2) +

  geom_line(data = all,
            aes(x = year + (month - 1/2)/12,
                y = pred.m.rm*10,
                color = source)) +
  scale_color_manual(values = c("#5ab4ac","#d8b365")) +
  labs(x = "",y = "", color = "") +
  scale_y_continuous(limits = c(2.5,3.5)*10) +
  scale_x_continuous(limits = c(1994,2024.5)) +
  guides(color = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 20))




A <- all %>%
  filter(year == 2023,
         month == 10) %>%
  group_by(source,month) %>%
  summarise(mean2023 = mean(pred.m),
            .groups = "keep")

B <- all %>%
  filter(year >= 1994,
         month ==10) %>%
  group_by(source, month) %>%
  summarise(meanall = mean(pred.m),
            .groups = "keep")


B %>%
  left_join(A,
            by = c("source","month")) %>%
  mutate(diff = mean2023 - meanall,
         rel.diff = (mean2023 - meanall)/meanall*100) %>%
  group_by(source) %>%
  summarise(diff.m = mean(diff),
            rel.diff.m = mean(rel.diff),
            m = mean(meanall),
            diff.min = min(diff),
            rel.diff.min = min(rel.diff))


(all %>%
    filter(source == "RS",
           pred.m <= (A %>%
                        filter(source == "RS",
                               month == 10) %>%
                        pull(mean2023) %>% min()))) %>%
  arrange(desc(year),desc(month))

all %>%
  group_by(source) %>%
  arrange(anomaly.m) %>%
  slice_head(n = 3)

(all %>%
    filter(source == "Trendy",
           pred.m <= (A %>%
                        filter(source == "Trendy") %>%
                        pull(mean2023) %>% min())))


ggplot() +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_point(data = all,
             aes(x = year + (month - 1/2)/12,
                 y = anomaly.m,
                 color = source),
             size = 0.2) +


  # geom_point(data = all %>%
  #              filter(year == 2023, month == 10),
  #            aes(x = year + (month - 1/2)/12,
  #                y = anomaly.m), color = "red",
  #            size = 1) +

  geom_line(data = all,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.m.rm,
                color = source)) +
  theme_bw() +
  geom_hline(linetype = 2, color = "black",
             yintercept = 0) +
  scale_color_manual(values = c("#5ab4ac","#d8b365")) +
  labs(x = "",y = "", color = "") +
  scale_y_continuous(limits = c(-5,2.5)) +
  scale_x_continuous(limits = c(1994,2024.5)) +
  guides(color = FALSE) +
  theme(text = element_text(size = 20))

all %>%
  group_by(source) %>%
  filter(year == 2023,
         month %in% c(10)) %>%
  summarise(m = mean(anomaly.m))

all %>%
  filter(source == "RS") %>%
  arrange(anomaly.m) %>%
  filter(year == 2023,
         month %in% c(10)) %>%
  summarise(m = mean(anomaly.m))

all %>%
  group_by(source) %>%
  filter((year == 2023 & month %in% c(7:12)) |
           (year == 2024)) %>%
  summarise(gpp = 10*mean(pred.m),
            anomaly = 10*mean(anomaly),
            anomaly.m = mean(anomaly.m))

all %>%
  group_by(source) %>%
  filter(year == 2023 & month == 10) %>%
  summarise(gpp = 10*mean(pred.m),
            anomaly = 10*mean(anomaly),
            anomaly.m = mean(anomaly.m))

all %>%
  filter(month == 10) %>%
  group_by(source) %>%
  filter(source == "RS") %>%
  arrange(pred.m)

all %>%
  group_by(source) %>%
  filter(year %in% (1994:2023),
         month %in% c(1,2,7:12)) %>%
  summarise(gpp = mean(pred.m),
            anomaly = mean(anomaly),
            anomaly.m = mean(anomaly.m))

all %>%
  filter(source == "Trendy",
         anomaly.m <= -3.8)

all %>%
  filter(source == "RS", month == 10) %>%
  arrange(pred.m)


all.wide <- all %>%
  pivot_wider(names_from = source,
              values_from = c(anomaly,
                              anomaly.rm,
                              anomaly.m,
                              mean.pred,
                              pred.m,
                              anomaly.m.rm,
                              pred.m.rm))

ggplot(data = all.wide,
       aes(x = anomaly.m.rm_RS,
           y = anomaly.m.rm_Trendy)) +
  geom_point(color = "darkgrey",
             size = 0.5) +
  geom_abline(slope = 1,
              intercept = 0, color = "black", linetype = 2) +
  stat_smooth(method = "lm",
              se = FALSE,
              color = "black") +
  theme_bw() +
  labs(x = "",y = "") +
  theme(text = element_text(size = 24))

summary(lm(data = all.wide,
           formula = anomaly.m.rm_Trendy ~ anomaly.m.rm_RS))

ggplot(data = all.wide,
       aes(x = pred.m.rm_RS*10,
           y = pred.m.rm_Trendy*10)) +
  geom_point(color = "darkgrey",
             size = 0.5) +
  geom_abline(slope = 1,
              intercept = 0, color = "black", linetype = 2) +
  stat_smooth(method = "lm",
              se = FALSE,
              color = "black") +
  theme_bw() +
  labs(x = "",y = "") +
  # scale_x_continuous(breaks = c(3,3.1,3.2)) +
  # scale_y_continuous(breaks = c(2.9,3,3.1,3.2)) +
  theme(text = element_text(size = 24))

summary(lm(data = all.wide,
           formula = pred.m.rm_Trendy ~ pred.m.rm_RS))


all.sum <- all %>%
  group_by(year,month) %>%
  summarise(pred.m = mean(pred.m),
            anomaly.m = mean(anomaly.m),
            pred.m.rm = mean(pred.m.rm),
            anomaly.m.rm = mean(anomaly.m.rm),
            .groups = "keep")

ggplot(data = all.sum) +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_line(aes(x = year + (month - 1/2)/12,
                 y = anomaly.m),
             size = 0.2) +

  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.m.rm),
            color = "black") +
  theme_bw()



saveRDS(all,
        "./outputs/GPP.anomalies.ERA5.RDS")
