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

files <- c("Congo.mean.JRA.historical.IFL.sum.RDS",
           "Amazon.mean.JRA.historical.IFL.sum.RDS")

for (cfile in files){
  system2("rsync",
          c("-avz",paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
            "./outputs/"))
}

all <- bind_rows(readRDS("./outputs/Congo.mean.JRA.historical.IFL.sum.RDS") %>%
                   mutate(basin = "Congo"),
                 readRDS("./outputs/Amazon.mean.JRA.historical.IFL.sum.RDS") %>%
                   mutate(basin = "Amazon"))


Weights <- readRDS("./outputs/weights.Trendy.RDS") %>%
  filter(model %in% "SIF2") %>%
  rename(gpp.product = model,
         model = trendy.model)

all <- bind_rows(all,
                 all %>%
                   left_join(Weights %>%
                               filter(slope > 0) %>%
                               dplyr::select(-c(r2,slope,gpp.product)),
                             by = c("model","basin")) %>%
                   filter(!is.na(r)) %>%
                   group_by(basin, year, month, var) %>%
                   summarise(pred = weighted.mean(pred,r, na.rm = TRUE),
                             obs = weighted.mean(obs, r,na.rm = TRUE),
                             .groups = "keep") %>%
                   mutate(model = "MEM"))

ggplot(data = all %>%
         filter(var == "gpp",
                model != "MEM")) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred,
                color = model)) +
  geom_line(data = all %>%
              filter(var == "gpp",
                     model == "MEM"),
            aes(x = year + (month - 1/2)/12,
                y = pred),
            color = "black") +
  scale_x_continuous(limits = c(2000,2024)) +
  facet_wrap(~ basin) +
  theme_bw()

MEM <- all %>%
  filter(model == "MEM") %>%
  rename(pred.MEM = pred,
         obs.MEM = obs)

ggplot(data = MEM) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.MEM,
                color = basin)) +
  scale_x_continuous(limits = c(2000,2024)) +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

MEM.diff <- MEM %>%
  ungroup() %>%
  pivot_wider(names_from = basin,
              values_from = c(pred.MEM,obs.MEM)) %>%
  mutate(diff.pred = pred.MEM_Congo - pred.MEM_Amazon,
         diff.obs = obs.MEM_Congo - obs.MEM_Amazon) %>%
  group_by(var) %>%
  mutate(diff.pred.rmean = rollmean(diff.pred, 6, na.pad=TRUE, align = "center"))

ggplot(data = MEM.diff %>%
         ungroup( ),
       aes(x = year + (month - 1/2)/12)) +
  geom_line(aes(y = diff.pred), color = "grey") +
  geom_line(aes(y = diff.pred.rmean), color = "red") +
  scale_x_continuous(limits = c(1958,2024)) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ var,scales = "free") +
  theme_bw()

MEM.diff.year <- MEM.diff %>%
  group_by(var,year) %>%
  summarise(diff.pred = mean(diff.pred),
            .groups = "keep") %>%
  group_by(var) %>%
  mutate(diff.pred.rmean = rollmean(diff.pred, 5, na.pad=TRUE, align = "center"))

ggplot(data = MEM.diff.year %>%
         group_by(var),
       aes(x = year)) +
  geom_line(aes(y = diff.pred), color = "grey") +
  scale_x_continuous(limits = c(1958,2024)) +
  geom_line(aes(y=diff.pred.rmean),
            color = "red") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ var,scales = "free") +
  theme_bw()
