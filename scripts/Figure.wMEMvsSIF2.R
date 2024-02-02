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
library(pals)

################################################################################

GPPproducts2keep <- c("VOD")

df.all.rspld.sum <- bind_rows(readRDS("./outputs/GPP.products.Amazon.ILF.sum.RDS")) %>%
  filter(model == GPPproducts2keep) %>%
  dplyr::select(-model)

years <- sort(unique(df.all.rspld.sum %>%
                       group_by(year) %>%
                       summarise(N = n()) %>%
                       filter(N == 12) %>% pull(year)))

df.all.rspld.sum <- df.all.rspld.sum %>%
  filter(year %in% years)

################################################################################

Trendy.data.sum <- bind_rows(readRDS("./outputs/Amazon.mean.JRA.historical.IFL.sum.RDS")) %>%
  filter(var == "gpp",
         year %in% years)

# MEM
Trendy.sum <- Trendy.data.sum %>%
  group_by(var,year,month) %>%
  summarise(value.MEM = mean(pred),
            .groups = "keep") %>%
  ungroup()


models <- sort(unique(Trendy.data.sum$model))

Weights <- readRDS("./outputs/weights.Trendy.RDS") %>%
  filter(model %in% GPPproducts2keep) %>%
  rename(gpp.product = model,
         model = trendy.model)

# MEM
Trendy.sum.w <- Trendy.data.sum %>%
  left_join(Weights %>%
              filter(basin == "Amazon") %>%
              filter(slope > 0) %>%
              dplyr::select(-c(r2,slope,gpp.product)),
            by = c("model")) %>%
  filter(!is.na(r)) %>%
  group_by(var,year,month) %>%
  summarise(value.MEM = weighted.mean(pred,r, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup()

#################################################################################
# Individual models

df.all <- data.frame()

for (cmodel in models){

  df.all <- bind_rows(df.all,
                      df.all.rspld.sum %>%
    ungroup() %>%
    left_join(Trendy.data.sum %>%
                filter(model == cmodel) %>%
                ungroup() %>%
                dplyr::select(model,year,month,obs),
              by = c("year","month")) )
}


palette <- c(kelly(n=17)[2:17])

# monthly means
ggplot(data = df.all,
       aes(x = value.m, y = obs, color = model)) +
  geom_point(size = 0.1, alpha = 0.5,
             show.legend = FALSE) +
  stat_smooth(method = "lm", alpha = 0.5, size = 0.5,
              se = FALSE) +
  scale_color_manual(values = palette) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  scale_x_continuous(breaks = c(2,3,4),
                     limits = c(2,4)) +
  # coord_equal() +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))


df.all  %>%
  group_by(model) %>%
  summarise(r2 = summary(lm(formula = obs ~ value.m))[["r.squared"]],
            rmse = 1/length(value.m)*sqrt(sum((obs-value.m)**2,na.rm = TRUE)))


df.monthly <- df.all %>%
  group_by(month,model) %>%
  summarise(value.m = mean(value.m),
            obs.m = mean(obs),
            .groups = "keep")

df.year <- df.all %>%
  group_by(year,model) %>%
  summarise(value.m = mean(value.m),
            obs.m = mean(obs),
            .groups = "keep")


ggplot(data = df.monthly) +
  geom_line(aes(x = month,
                y = obs.m,
                color = model)) +
  geom_line(aes(x = month,
                y = value.m), color = "black",linetype = 2) +
  scale_color_manual(values = palette) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggplot(data = df.year) +
  geom_line(aes(x = year,
                y = obs.m,
                color = model)) +
  geom_line(aes(x = year,
                y = value.m), color = "black",linetype = 2) +
  scale_color_manual(values = palette) +
  scale_x_continuous(breaks = seq(1990,2020,10),
                     limits = c(1990,2020)) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))

#################################################################################
# Model ensemble means

df.all2 <- bind_rows( df.all.rspld.sum %>%
                        ungroup() %>%
                        left_join(Trendy.sum %>%
                                    dplyr::select(year,month,value.MEM),
                                  by = c("year","month")) %>%
                        rename(obs = value.MEM) %>%
                        mutate(model = "MEM"),

                      df.all.rspld.sum %>%
                        ungroup() %>%
                        left_join(Trendy.sum.w %>%
                                    dplyr::select(year,month,value.MEM),
                                  by = c("year","month")) %>%
                        rename(obs = value.MEM) %>%
                        mutate(model = "wMEM"))

ggplot(data = df.all2,
       aes(x = value.m, y = obs, color = model)) +
  geom_point(size = 0.1, alpha = 0.5,
             show.legend = FALSE) +
  stat_smooth(method = "lm", alpha = 0.5, size = 0.5,
              se = FALSE) +
  # scale_color_manual(values = palette) +
  geom_abline(slope = 1, intercept = 0,
              color = "black", linetype = 2) +
  coord_equal() +
  labs(x = "",y = "") +
  guides(color = "none") +
  scale_color_manual(values = c("grey","black")) +
  theme_bw() +
  theme(text = element_text(size = 20))

df.all2 %>%
  group_by(model) %>%
  summarise(r2 = summary(lm(formula = obs ~ value.m))[["r.squared"]],
            rmse = 1/length(value.m)*sqrt(sum((obs-value.m)**2,na.rm = TRUE)))



df.monthly2 <- df.all2 %>%
  group_by(month,model) %>%
  summarise(value.m = mean(value.m),
            obs.m = mean(obs),
            .groups = "keep")

df.year2 <- df.all2 %>%
  group_by(year,model) %>%
  summarise(value.m = mean(value.m),
            obs.m = mean(obs),
            .groups = "keep")


ggplot(data = df.monthly2) +
  geom_line(aes(x = month,
                y = obs.m,
                color = model)) +
  geom_line(aes(x = month,
                y = value.m), color = "black",linetype = 2) +
  scale_color_manual(values = c("grey","black")) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggplot(data = df.year2) +
  geom_line(aes(x = year,
                y = obs.m,
                color = model)) +
  geom_line(aes(x = year,
                y = value.m), color = "black",linetype = 2) +
  scale_color_manual(values = c("grey","black")) +
  scale_x_continuous(breaks = seq(1990,2020,10),
                     limits = c(1990,2020)) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggplot(data = df.year2,
       aes(x = obs.m,
           y = value.m,
           color = model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0,
              linetype = 2) +
  stat_smooth(method = "lm") +
  scale_color_manual(values = c("grey","black")) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))

