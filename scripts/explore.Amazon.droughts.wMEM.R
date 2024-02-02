rm(list = ls())

library(tidyr)
library(ggplot2)
library(dplyr)
library(raster)
library(ggthemes)
library(TrENDY.analyses)
library(Congo.ED2)
library(sf)
library(Hmisc)

################################################################################

all.Amazon <- readRDS("./outputs/Amazon.mean.JRA.historical.IFL.sum.RDS")
Weights <- readRDS("./outputs/weights.Trendy.RDS") %>%
  filter(basin == "Amazon",
         model == "SIF2",
         slope > 0) %>%
  dplyr::select(trendy.model,r) %>%
  rename(model = trendy.model)

all.amazon.weights <- all.Amazon %>%
  left_join(Weights,
            by = "model") %>%
  filter(!is.na(r))

MEM.area <- all.amazon.weights  %>%
  group_by(var,year,month) %>%
  # MEM
  summarise(obs.sd = sd(obs,na.rm = TRUE),
            obs.m = weighted.mean(obs,r,na.rm = TRUE),

            N = n(),
            pred.sd = sqrt(wtd.var(pred,r,na.rm = TRUE)),
            pred.m = weighted.mean(pred,r,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(pred.se = pred.sd/sqrt(N))


df.ts.SIF <- readRDS("./outputs/Amazon.SIF.RDS") %>%
  filter(basin == "Amazon",
         model == "SIF2")

ds <- MEM.area %>%
  filter(var %in% c("gpp")) %>%
  filter(year >= 1990) %>%
  mutate(time = year + (month -1/2)/12)

summary(lm(data = ds,
           formula = pred.m ~ time))


droughts <- data.frame(x1 = c(1997,2009,2004,2015,2023) + 0.5/12,
                       x2 = c(1998,2010,2005,2016,2023) +
                         11.5/12)


cycles <- df.ts.SIF %>%
  mutate(time = year + (month -1/2)/12) %>%
  ungroup() %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%
  mutate(mean.obs = slope*(time) + intercept) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = intercept + time*slope + mean.month) %>%
  group_by(month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))

cycles.expanded <- expand.grid(year = 1990:2023,
                               month = 1:12) %>%
  arrange(year,month) %>%
  left_join(cycles %>%
              filter(year == year[1]) %>%
              dplyr::select(month,mean.month, intercept, slope),
            by = c("month")) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(reconstructed = intercept + time*slope + mean.month)


ggplot() +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_line(data = cycles.expanded,
            aes(x = time, y = intercept + time*slope ),
            color = "red", size = 0.5, linetype = 2) +

  geom_line(data = cycles.expanded,
            aes(x = time, y = reconstructed),
            color = "red", size = 0.5) +

  geom_ribbon(data = MEM.area %>%
                filter(var %in% c("gpp")),
              aes(x = year + (month - 1/2)/12,
                  y = pred.m,ymin = pred.m-1.96*pred.se, ymax = pred.m + 1.96*pred.se,
                  group = var), alpha = 0.7,
              linetype = 2, fill = "darkgrey", color = NA) +

  geom_line(data = df.ts.SIF,
            aes(x = year + (month - 1/2)/12,
                y = value.m),
            color = "darkblue") +

  geom_line(data = MEM.area %>%
              filter(var %in% c("gpp")),
            aes(x = year + (month - 1/2)/12,
                y = pred.m),
            color = 'black',
            linetype = 1) +

  scale_x_continuous(limits = c(1990,2024), expand = c(0,0)) +
  theme_bw() +

  theme(text = element_text(size = 20)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  guides(color = "none")


ggplot() +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_line(data = cycles.expanded,
            aes(x = time, y = intercept + time*slope ),
            color = "red", size = 0.5, linetype = 2) +

  geom_line(data = cycles.expanded,
            aes(x = time, y = reconstructed),
            color = "red", size = 0.5) +

  # geom_ribbon(data = MEM.area %>%
  #               filter(var %in% c("gpp")),
  #             aes(x = year + (month - 1/2)/12,
  #                 y = pred.m,ymin = pred.m-1.96*pred.se, ymax = pred.m + 1.96*pred.se,
  #                 group = var), alpha = 0.7,
  #             linetype = 2, fill = "darkgrey", color = NA) +

  geom_line(data = df.ts.SIF,
            aes(x = year + (month - 1/2)/12,
                y = value.m),
            color = "darkblue") +

  geom_line(data = MEM.area %>%
              filter(var %in% c("gpp")),
            aes(x = year + (month - 1/2)/12,
                y = pred.m),
            color = 'black',
            linetype = 1) +

  scale_x_continuous(limits = c(2014,2024), expand = c(0,0)) +
  scale_y_continuous(limits = c(3,4), expand = c(0,0)) +
  theme_bw() +

  theme(text = element_text(size = 20)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  guides(color = "none")


MEM.Amazon <- all.Amazon %>%
  left_join(Weights ,
            by = "model") %>%
  filter(!is.na(r)) %>%
  group_by(var,model,year,month) %>%
  # Regional average
  summarise(obs.m = mean(obs,na.rm = TRUE),
            obs.sd = sd(obs,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            r = mean(r),
            .groups = "keep") %>%
  group_by(var,year,month) %>%
  # MEM
  summarise(obs.sd = sd(obs.m,na.rm = TRUE),
            obs.m = weighted.mean(obs.m,r,na.rm = TRUE),

            N = n(),
            pred.sd = sqrt(wtd.var(pred.m, r, na.rm = TRUE)),
            pred.m = weighted.mean(pred.m,r,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(pred.se = pred.sd/sqrt(N))

ggplot(data = all.amazon.weights %>%
         filter(var == "gpp")) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred,
                color = r,
                group = model)) +
  scale_x_continuous(limits = c(1990,1995)) +
  theme_bw()

MEM.Amazon.cycle <- MEM.Amazon %>%
  group_by(var,month) %>%
  summarise(obs.m = mean(obs.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = MEM.Amazon.cycle) +
  geom_line(aes(x = month, y = obs.m)) +
  facet_wrap(~ var,nrow = 3, scales = "free") +
  theme_bw()

MEM.area.anomalies <- MEM.Amazon %>%
  filter(year >= 1994) %>%
  group_by(var) %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(pred.m ~ time))[2],
         intercept = coef(lm(pred.m ~ time))[1]) %>%
  mutate(mean.obs = slope*(time) + intercept) %>%
  mutate(detrended = pred.m - mean.obs) %>%
  group_by(var,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(var,month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))


df.ts.SIF.dt <- df.ts.SIF %>%
  ungroup() %>%
  mutate(time = year + (month -1/2)/12) %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%
  mutate(mean.obs = slope*(time) + intercept) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))


ggplot(data = MEM.area.anomalies %>%
         filter(var %in% c("gpp"))) +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +

  geom_line(aes(x = time,
                y = anomaly.m),
            color = "black") +

  # geom_line(data = df.ts.SIF.dt,
  #           aes(x = time,
  #               y = detrended),
  #           color = "darkblue") +
  # geom_vline(xintercept = c(2023+9.5/12,
  #                           2016+0.5/12,
  #                           1997+10.5/12),
  #            linetype = 2) +

  geom_hline(yintercept = 0, linetype = 2) +
  # facet_wrap(~ var) +
  scale_x_continuous(limits = c(1995, 2024), expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  guides(color = "none")


MEM.area.anomalies.wide <- MEM.area.anomalies %>%
  dplyr::select(year,month,anomaly,anomaly.m,pred.m,var) %>%
  pivot_wider(names_from = var,
              values_from = c(anomaly.m,anomaly,pred.m))

ggplot(data = MEM.area.anomalies.wide %>%
         filter(year <= 2022),
       aes(x = anomaly.m_gpp,
           y = anomaly.m_nep)) +
  geom_point() +
  # geom_abline(slope = 1,
  #             intercept = 0, linetype = 1) +
  stat_smooth(method = "lm",
              color = "red", fill = "red", alpha = 0.5) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  theme_bw()


saveRDS(MEM.area.anomalies,"./outputs/CC.anomalies.RDS")
saveRDS(MEM.area.anomalies.wide,"./outputs/CC.anomaly.RDS")
