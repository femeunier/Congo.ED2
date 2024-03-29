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

# files <- c("GPP.products.Amazon.ILF.sum.RDS",
#            "GPP.products.Congo.ILF.sum.RDS",
#            "Amazon.mean.JRA.historical.IFL.sum.RDS",
#            "Congo.mean.JRA.historical.IFL.sum.RDS")
#
# for (cfile in files){
#   system2("scp",
#           c(paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }

################################################################################

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

# MEM
Trendy.sum <- Trendy.data.sum %>%
  group_by(var,basin,year,month) %>%
  summarise(value.MEM = mean(pred),
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
              filter(model == "SIF") %>%
              group_by(month,basin) %>%
              summarise(v = mean(value.m),
                        .groups = "keep"),
            aes(x = month, y = v), color = "black",linetype = 2) +
  facet_wrap(~ basin) +
  theme_bw()

################################################################################

ggplot(all %>%
         filter(model == "SIF")) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = model),
            linetype = 1) +
  geom_line(data = Trendy.sum,
            aes(x = year + (month - 1/2)/12,
                y = value.MEM),
            linetype = 2) +
  facet_wrap(~ basin) +
  scale_x_continuous(limits = c(2000,2024)) +
  theme_bw()

# monthly means
ggplot(data = all,
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
            .groups = "keep")

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

all %>%
  group_by(year,model,basin) %>%
  summarise(value.m = mean(value.m),
            value.MEM = mean(value.MEM)) %>%
  group_by(model,basin) %>%
  summarise(r2 = summary(lm(formula = value.MEM ~ value.m))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = value.MEM ~ value.m))[2],
            .groups = "keep")


################################################################################
# Individual models

models <- unique(Trendy.data.sum$model)

df.r2 <- all.models <- data.frame()

for (cmodel in models){
  print(cmodel)

  cTrendy <- Trendy.data.sum %>%
    filter(model == cmodel) %>%
    group_by(basin,year,month) %>%
    summarise(value.MEM = mean(pred),
              .groups = "keep") %>%
    ungroup() %>%
    dplyr::filter(year %in% unique(c(df.all.rspld.sum$year)))

  all <- df.all.rspld.sum %>%
    ungroup() %>%
    left_join(cTrendy %>%
                dplyr::select(basin,year,month,value.MEM),
              by = c("year","month","basin")) %>%
    mutate(anomaly = value.m - mean(value.m,na.rm = TRUE),
           anomaly.MEM = value.MEM - mean(value.MEM,na.rm = TRUE))

  df.r2 <- bind_rows(df.r2,
                     all %>%
                       group_by(basin,model) %>%
                       summarise(r2 = summary(lm(formula = value.MEM ~ value.m))[["r.squared"]],
                                 r = sqrt(r2),
                                 slope = coef(lm(formula = value.MEM ~ value.m))[2],
                                 .groups = "keep") %>%
                       mutate(trendy.model = cmodel))

  all.models <- bind_rows(all.models,
                          all %>%
                            mutate(trendy.model = cmodel))

}

ggplot(data = all.models %>%
         filter(trendy.model == "ISAM"),
       aes(x = value.m, y = value.MEM, color = basin)) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  facet_wrap(~ model) +
  theme_bw()


RMSE.models <- all.models %>%
  group_by(basin,trendy.model,model) %>%
  summarise(RMSE = 1/(length(model) - 1) *sqrt(sum((value.m - value.MEM),na.rm = TRUE)**2),
            .groups = "keep")

saveRDS(df.r2 %>%
          left_join(RMSE.models,
                    by = c("basin","model","trendy.model")) %>%
          mutate(r = 1,
                 slope = 1,
                 r2 = 1),
        "./outputs/weights.Trendy.RDS")

RMSE.models %>%
  filter(model == "SIF") %>%
  arrange((RMSE))


df.r2 %>%
  filter(r > 0) %>%
  arrange(desc(r)) %>%
  filter(model == "SIF")
