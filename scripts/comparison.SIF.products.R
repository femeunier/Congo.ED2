rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)

GPPproducts2keep <- c("SIF","SIF2")

coord <- bind_rows(readRDS("./outputs/Amazon.coord.GPP.products.ILF.RDS"),
                   readRDS("./outputs/Congo.coord.GPP.products.ILF.RDS"))

df.all.rspld <- bind_rows(readRDS("./outputs/GPP.products.Amazon.ILF.RDS") %>%
                            mutate(basin = "Amazon"),
                          readRDS("./outputs/GPP.products.Congo.ILF.RDS") %>%
                            mutate(basin = "Congo")) %>%
  ungroup() %>%
  dplyr::filter(model %in% GPPproducts2keep) %>%
  filter(model.lon.lat %in% coord[["model.lon.lat"]]) %>%
  dplyr::select(-c(continent,biome,biome.continent,model.lon.lat))

df.wide <- df.all.rspld %>%
  pivot_wider(names_from = model,
              values_from = value) %>%
  filter(!is.na(SIF),!is.na(SIF2))

df.wide %>%
  group_by(basin) %>%
  summarise(r2 = summary(lm(formula = SIF2 ~ SIF))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = SIF2 ~ SIF))[2],
            .groups = "keep")

ggplot(df.wide,
       aes(x = SIF,
           y = SIF2)) +
  geom_hex() +

  geom_abline(slope = 1, intercept = 0, linetype = 2) +

  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(10,10000),
    oob = scales::squish,
    trans = "log10") +

  facet_grid( ~ basin) +
  stat_smooth(method = "lm", se = FALSE,
              color = "black", linetype = 1) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")

df.wide.sum <- df.wide %>%
  group_by(basin,year,month) %>%
  summarise(SIF2 = mean(SIF2),
            SIF = mean(SIF),
            .groups = "keep")

ggplot(df.wide.sum,
       aes(x = SIF, y = SIF2)) +
  geom_point(size = 0.05) +
  stat_smooth(method = "lm", color = "black",
              se = FALSE) +
  geom_abline(slope = 1, intercept = 0,
              color = "black",linetype = 2) +
  facet_wrap(~ basin) +
  coord_equal() +
  theme_bw()


df.wide.sum %>%
  group_by(basin) %>%
  summarise(r2 = summary(lm(formula = SIF2 ~ SIF))[["r.squared"]],
            r = sqrt(r2),
            slope = coef(lm(formula = SIF2 ~ SIF))[2],
            .groups = "keep")

df.ts <- df.all.rspld %>%
  group_by(model,basin,year,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(df.ts) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = model)) +
  facet_wrap(~ basin) +
  theme_bw()

df.ts.year <- df.ts %>%
  group_by(model,basin,year) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            N = n(),
            .groups = "keep") %>%
  filter(N == 12)

ggplot(df.ts.year,
       aes(x = year,
           y = value.m,
           color = model)) +
  geom_line() +
  stat_smooth(method = "lm") +
  facet_wrap(~ basin) +
  theme_bw()

df.ts.month <- df.ts %>%
  group_by(model,basin,month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot(df.ts.month,
       aes(x = month,
           y = value.m,
           color = model)) +
  geom_line() +
  facet_wrap(~ basin) +
  theme_bw()

saveRDS(df.ts.month,
        "./outputs/Seasonal.Cycle.SIF.RDS")
saveRDS(df.ts,
        "./outputs/Amazon.SIF.RDS")
