rm(list = ls())

library(tidyr)

system2("rsync",
        c("-avz","hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/predictions.XGB.2000.RDS",
          "./outputs"))

predictions.XGB.2000 <- readRDS("./outputs/predictions.XGB.2000.RDS")


ggplot(predictions.XGB.2000,
       aes(x = pred.ERA5,
           y = obs)) +
  geom_hex() +

  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  scale_fill_gradient(
  low = NA,
  high = "black",
  limits = c(100,10000), oob = scales::squish,
  trans = "log10") +

  facet_grid(model ~ var) +
  stat_smooth(method = "lm", se = FALSE,
              color = "red", linetype = 2) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")

predictions.XGB.2000.sum <- predictions.XGB.2000 %>%
  group_by(continent,model,month,var,biome) %>%
  summarise(obs.m = mean(obs),
            pred.m = mean(pred),
            pred.sd = sd(pred),
            pred.ERA5.m = mean(pred.ERA5),
            pred.ERA5.sd = sd(pred.ERA5),
            .groups = "keep")

ggplot(predictions.XGB.2000.sum %>%
         filter(model == "YIBs")) +
  # geom_ribbon(aes(x = (month), y = pred.m, fill = biome,
  #                 ymin = pred.m - pred.sd, ymax = pred.m + pred.sd),
  #             color = NA,alpha = 0.5) +
  # geom_ribbon(aes(x = (month), y = pred.ERA5.m, fill = biome,
  #                 ymin = pred.ERA5.m - pred.ERA5.sd, ymax = pred.ERA5.m + pred.ERA5.sd),
  #             color = NA,alpha = 0.5) +

  geom_line(aes(x = (month), y = obs.m, group = var),
            color = "black") +

  geom_line(aes(x = (month), y = pred.m, color = biome, group = interaction(biome,var))) +

  geom_line(aes(x = (month), y = pred.ERA5.m, color = biome,
                group = interaction(biome,var)),
            linetype = 2) +

  geom_hline(yintercept = 0, linetype = 2) +

  facet_grid(biome ~ continent) +
  theme_bw()



MEM <- predictions.XGB.2000.sum %>%
  group_by(continent,month,var,biome) %>%
  summarise(obs.MEM.m = mean(obs.m),
            pred.MEM.m = mean(pred.m),
            pred.ERA5.MEM.m = mean(pred.MEM.m),
            .groups = "keep")

ggplot(MEM) +
  # geom_ribbon(aes(x = (month), y = pred.m, fill = biome,
  #                 ymin = pred.m - pred.sd, ymax = pred.m + pred.sd),
  #             color = NA,alpha = 0.5) +
  # geom_ribbon(aes(x = (month), y = pred.ERA5.m, fill = biome,
  #                 ymin = pred.ERA5.m - pred.ERA5.sd, ymax = pred.ERA5.m + pred.ERA5.sd),
  #             color = NA,alpha = 0.5) +

  geom_line(aes(x = (month), y = obs.MEM.m, group = var),
            color = "black") +

  # geom_line(aes(x = (month), y = pred.MEM.m, group = interaction(biome,var),color = biome)) +

  geom_line(aes(x = (month), y = pred.ERA5.MEM.m, group = interaction(biome,var),color = biome),
            linetype = 2) +

  geom_hline(yintercept = 0, linetype = 2) +

  facet_grid(biome ~ continent) +
  theme_bw()
