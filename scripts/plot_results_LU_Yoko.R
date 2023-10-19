rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/compare.lu.RDS",
                    "./outputs/"))

# Data
compare.lu <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/compare.lu.RDS") %>%
  mutate(time = year + month/12)

compare.lu.wide <- compare.lu %>%
  dplyr::select(sim,time,AGB.tree) %>%
  pivot_wider(names_from = sim,
              values_from = AGB.tree) %>%
  mutate(diff = Yoko_lu - Yoko) %>%
  filter(!is.na(diff)) %>%
  mutate(diff.rel = diff/Yoko)

compare.lu.wide <- compare.lu.wide %>% filter(time > (1550 + 1/12))

plot(compare.lu.wide$time,
     compare.lu.wide$Yoko,
     xlim = c(1550,1600),
     ylim = c(0,20),type = "l")
lines(compare.lu.wide$time,
      compare.lu.wide$Yoko_lu,col = "red")

plot(compare.lu.wide$diff.rel*100,type = "l")

ggplot(data = compare.lu %>% filter(time > (1550 + 1/12))) +
  geom_line(aes(x = time, y = AGB , color = sim, linetype = as.factor(sim))) +
  # scale_y_log10() +
  theme_bw()
