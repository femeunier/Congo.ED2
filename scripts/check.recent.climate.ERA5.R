rm(list = ls())

library(dplyr)
library(tidyr)

climate <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS")

climate.select <- climate %>%
  filter(year %in% 2010:2023) %>%
  pivot_longer(cols = -c(year,month),
               names_to = "variable",
               values_to = "value")
climate.select.sum <- climate.select %>%
  group_by(variable,
           year,
           month) %>%
  summarise(value.m = mean(value),
            .groups = "keep") %>%
  group_by(variable) %>%
  mutate(value.m.rel = value.m/mean(value.m[year == min(year)]))

# View(climate.select.sum %>%
#   filter(variable %in% c("dlwrf","dswrf","pre"),
#          year == 2023))

ggplot(data = climate.select.sum %>%
         filter(variable %in% c("tmp","tmin","tmax","pre","VPD"))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m.rel,
                color = variable)) +
  theme_bw()


ggplot(data = climate.select.sum %>%
         filter(variable %in% c("tmp","tmin","tmax"))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m - 273.15,
                color = variable)) +
  theme_bw()

ggplot(data = climate.select.sum %>%
         filter(variable %in% c("pre"))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m,
                color = variable)) +
  geom_point(data = climate.select.sum %>%
              filter(variable %in% c("pre"),
                     year == 2023),
            aes(x = year + (month - 1/2)/12,
                y = value.m),
            color = "black") +
  theme_bw()


climate.select.sum %>%
  filter(variable == "pre") %>%
  group_by(year) %>%
  summarise(m = mean(value.m)*8*365)

climate.select.sum.wide <- climate.select.sum %>%
  pivot_wider(names_from = variable,
              values_from = value.m)
