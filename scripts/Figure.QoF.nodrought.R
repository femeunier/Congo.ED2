rm(list = ls())

library(dplyr)
library(pals)
library(ggplot2)


################################################################################
# RS

RS.data <- readRDS("./outputs/all.predictions.SIF.ILF_nodrought.RDS")

df.drought <- RS.data %>%
  filter(year %in% c(2015:2016))
  # mutate(timing = case_when(year == 2016 & month %in% c(1:4) ~ "2015",
  #                           year == 2015 & month %in% c(10:12) ~ "2015",
  #                           TRUE ~ "other")) %>%
  # filter(timing != "other") %>%
  # dplyr::select(-c(timing))

palette <- kelly(n=17)[2:17]

ggscater <- ggplot(df.drought ,
                   aes(x = pred,
                       y = obs,
                       color = product)) +
  geom_point(shape = NA) +
  geom_hex(color = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = 1, color = "black") +
  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(10,1000), oob = scales::squish,
    trans = "log10") +
  scale_color_manual(values = palette) +
  coord_equal() +
  stat_smooth(method = "lm",
              se = FALSE) +
  # facet_wrap(~ basin) +
  theme_bw() +
  labs(x = "", y = "", color = "", fill = "") +
  theme(legend.position = c(0.2,0.75)) +
  guides(color = "none", fill = "none") +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(limits = c(0,8)) +
  scale_y_continuous(limits = c(0,8))

ggExtra::ggMarginal(ggscater,
                    type = "density",
                    groupColour = TRUE)

df.r2 <- df.drought %>%
  group_by(product) %>%
  summarise(r2 = summary(lm(formula = obs ~ pred))[["r.squared"]],
            obs.m = mean(obs,na.rm = TRUE),
            rmse = sqrt(1/length(obs)*(sum((obs-pred)**2,na.rm = TRUE))),
            .groups = "keep") %>%
  mutate(rel.RMSE = rmse/obs.m)

df.r2

hist(df.r2 %>% pull(r2))

df.drought.sum <- df.drought %>%
  group_by(product,year,month) %>%
  summarise(obs.m = mean(obs,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")

df.drought.sum.sum <- df.drought.sum %>%
  group_by(year,month) %>%
  summarise(obs.m = mean(obs.m,na.rm = TRUE),
            pred.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

bind_rows(df.drought.sum,
          df.drought.sum.sum %>%
            mutate(product = "Total")) %>%
  group_by(product) %>%
  summarise(r2 = summary(lm(obs.m ~ pred.m))[["r.squared"]])

ggplot() +
  geom_line(data = df.drought.sum.sum,
            aes(x = year + (month - 1/2)/12,
                y = obs.m),
            color = "black",
            linetype = 1) +
  geom_point(data = df.drought.sum.sum,
            aes(x = year + (month - 1/2)/12,
                y = pred.m),
            color = "black",
            linetype = 1) +
  geom_line(data = df.drought.sum,
            aes(x = year + (month - 1/2)/12,
                y = obs.m,
                color = product),
            linetype = 1) +
  geom_point(data = df.drought.sum,
             aes(x = year + (month - 1/2)/12,
                 y = pred.m,
                 color = product)) +
  # facet_wrap(~ var, scales = "free") +
  theme_bw()

################################################################################
# Trendy

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Amazon.mean.JRA.historical.IFL_nodrought.RDS",
          "./outputs/"))

Amazon.mean.JRA.historical.IFL_nodrought <-
  readRDS("./outputs/Amazon.mean.JRA.historical.IFL_nodrought.RDS")


df.drought <- Amazon.mean.JRA.historical.IFL_nodrought %>%
  mutate(obs = obs*365*86400) %>%
  filter(year %in% c(2015:2016))
  # mutate(timing = case_when(year == 2016 & month %in% c(1:4) ~ "2015",
  #                           year == 2015 & month %in% c(10:12) ~ "2015",
  #                           TRUE ~ "other")) %>%
  # filter(timing != "other") %>%
  # dplyr::select(-c(timing))


palette <- kelly(n=17)[2:17]

ggscater <- ggplot(df.drought %>%
                     filter(var == "gpp"),
                   aes(x = pred,
                       y = obs,
                       color = model)) +
  geom_point(shape = NA) +
  geom_hex(color = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = 1, color = "black") +
  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(10,1000), oob = scales::squish,
    trans = "log10") +
  scale_color_manual(values = palette) +
  coord_equal() +
  stat_smooth(method = "lm",
              se = FALSE) +
  # facet_wrap(~ basin) +
  theme_bw() +
  labs(x = "", y = "", color = "", fill = "") +
  theme(legend.position = c(0.2,0.75)) +
  guides(color = "none") +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(limits = c(0,8)) +
  scale_y_continuous(limits = c(0,8))

ggExtra::ggMarginal(ggscater,
                    type = "density",
                    groupColour = TRUE)

df.r2 <- df.drought %>%
  group_by(model,var) %>%
  summarise(r2 = summary(lm(formula = obs ~ pred))[["r.squared"]],
            obs.m = mean(obs,na.rm = TRUE),
            rmse = sqrt(1/length(obs)*(sum((obs-pred)**2,na.rm = TRUE))),
            .groups = "keep") %>%
  mutate(rel.RMSE = rmse/obs.m)

df.r2 %>%
  filter(var == "gpp")
hist(df.r2 %>%
       filter(var == "gpp") %>%
       pull(r2))

df.drought.sum <- df.drought %>%
  group_by(model,var,year,month) %>%
  summarise(obs.m = mean(obs,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")

df.drought.sum.sum <- df.drought.sum %>%
  group_by(var,year,month) %>%
  summarise(obs.m = mean(obs.m,na.rm = TRUE),
            pred.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_line(data = df.drought.sum %>%
              filter(var == "gpp"),
            aes(x = year + (month - 1/2)/12,
                y = obs.m,
                color = model),
            linetype = 1) +
  geom_point(data = df.drought.sum %>%
               filter(var == "gpp"),
             aes(x = year + (month - 1/2)/12,
                y = pred.m,
                color = model)) +

  geom_line(data = df.drought.sum.sum %>%
              filter(var == "gpp"),
            aes(x = year + (month - 1/2)/12,
                y = obs.m),
            color = "black",
            linetype = 1) +
  geom_point(data = df.drought.sum.sum %>%
               filter(var == "gpp"),
             aes(x = year + (month - 1/2)/12,
                 y = pred.m),
             color = "black") +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
