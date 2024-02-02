rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)
library(raster)
library(TrENDY.analyses)
library(Congo.ED2)
library(ggplot2)
library(tidyr)

################################################################################

GPPproducts2keep <- c("SIF")

df.all.rspld.sum <- bind_rows(readRDS("./outputs/GPP.products.Amazon.ILF.sum.RDS") %>%
                                mutate(basin = "Amazon"),
                              readRDS("./outputs/GPP.products.Congo.ILF.sum.RDS") %>%
                                mutate(basin = "Congo"))

years <- sort(unique(df.all.rspld.sum$year))

################################################################################

Trendy.data.sum <- bind_rows(readRDS("./outputs/Amazon.mean.JRA.historical.IFL.sum.RDS") %>%
                               mutate(basin = "Amazon"),
                             readRDS("./outputs/Congo.mean.JRA.historical.IFL.sum.RDS") %>%
                               mutate(basin = "Congo")) %>%
  filter(var == "gpp")

Weights <- readRDS("./outputs/weights.Trendy.RDS") %>%
  filter(model %in% GPPproducts2keep) %>%
  rename(gpp.product = model,
         model = trendy.model)

# MEM
Trendy.sum <- Trendy.data.sum %>%
  left_join(Weights %>%
              filter(slope > 0) %>%
              dplyr::select(-c(r2,slope,gpp.product)),
            by = c("model","basin")) %>%
  filter(!is.na(r)) %>%
  group_by(var,basin,year,month) %>%
  summarise(value.MEM = weighted.mean(pred,r, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup()

#################################################################################
# Basin-level mean

all <- df.all.rspld.sum %>%
  ungroup() %>%
  left_join(Trendy.sum %>%
              dplyr::select(basin,year,month,value.MEM),
            by = c("year","month","basin")) %>%
  mutate(anomaly = value.m - mean(value.m,na.rm = TRUE),
         anomaly.MEM = value.MEM - mean(value.MEM,na.rm = TRUE))

ggplot() +
  geom_line(data = Trendy.sum %>%
              filter(year >= 2000) %>%
              group_by(month,basin) %>%
              summarise(v = mean(value.MEM),
                        .groups = "keep"),
            aes(x = month, y = v), color = "black") +
  geom_line(data = Trendy.data.sum %>%
              filter(year >= 2000) %>%
              group_by(var,model,month,basin) %>%
              summarise(v = mean(obs,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = month, y = v, color = model)) +
  geom_line(data = df.all.rspld.sum %>%
              filter(model == GPPproducts2keep) %>%
              group_by(month,basin) %>%
              summarise(v = mean(value.m),
                        .groups = "keep"),
            aes(x = month, y = v), color = "black",linetype = 2) +
  facet_wrap(~ basin) +
  theme_bw()

ggplot() +
  geom_line(data = Trendy.sum %>%
              group_by(year,basin) %>%
              summarise(v = mean(value.MEM),
                        .groups = "keep"),
            aes(x = year, y = v), color = "black") +
  geom_line(data = Trendy.data.sum %>%
              group_by(var,model,year,basin) %>%
              summarise(v = mean(obs,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = year, y = v, color = model)) +
  geom_line(data = df.all.rspld.sum %>%
              filter(model == GPPproducts2keep) %>%
              group_by(year,basin) %>%
              summarise(v = mean(value.m),
                        .groups = "keep"),
            aes(x = year, y = v), color = "black",linetype = 2) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1992,2022)) +
  theme_bw()


ggplot() +
  geom_line(data = Trendy.sum,
            aes(x = year + (month - 1/2)/12, y = value.MEM), color = "black") +
  # geom_line(data = Trendy.data.sum,
  #           aes(x = year + (month - 1/2)/12, y = obs, color = model)) +
  geom_line(data = df.all.rspld.sum %>%
              filter(model == GPPproducts2keep),
            aes(x = year + (month - 1/2)/12, y = value.m), color = "red",linetype = 2) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(1992,2022)) +
  theme_bw()

################################################################################

# monthly means
ggplot(data = all %>%
         filter(model %in% GPPproducts2keep,
                basin == "Amazon"),
       aes(x = value.m, y = value.MEM, color = basin)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()

all %>%
  group_by(model,basin) %>%
  summarise(r2 = summary(lm(formula = value.MEM ~ value.m))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = value.MEM ~ value.m))[2],
            .groups = "keep") %>%
  filter(model %in% GPPproducts2keep)


# yearly means
ggplot(data = all %>%
         group_by(year,model,basin) %>%
         summarise(value.m = mean(value.m),
                   value.MEM = mean(value.MEM),
                   .groups = "keep"),
       aes(x = value.m, y = value.MEM, color = basin)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()


ggplot(data = all %>%
         filter(model %in% GPPproducts2keep,
                basin == "Amazon") %>%
         group_by(year,model,basin) %>%
         summarise(value.m = mean(value.m),
                   value.MEM = mean(value.MEM),
                   .groups = "keep"),
       aes(x = value.m, y = value.MEM, color = basin)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()

all %>%
  group_by(year,model,basin) %>%
  summarise(value.m = mean(value.m),
            value.MEM = mean(value.MEM)) %>%
  group_by(model,basin) %>%
  summarise(r2 = summary(lm(formula = value.MEM ~ value.m))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = value.MEM ~ value.m))[2],
            .groups = "keep") %>%
  filter(model %in% GPPproducts2keep)
