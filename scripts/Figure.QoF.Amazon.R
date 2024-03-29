rm(list = ls())

library(dplyr)
library(ggplot2)
library(pals)
library(cowplot)

# system2("rsync",
#         c("-avz",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Amazon.mean.test.ERA5.IFL.RDS",
#           "./outputs/"))

################################################################################

Trendy.data <- readRDS("./outputs/Amazon.mean.test.ERA5.IFL.RDS")

palette <- kelly(n=17)[2:17]

ggscater <- ggplot(Trendy.data,
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
    limits = c(100,100000), oob = scales::squish,
    trans = "log10") +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = palette) +
  # stat_smooth(method = "lm", se = FALSE, linetype = 1) +
  coord_equal() +
  # facet_wrap(~)
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

Trendy.data %>%
  group_by(model) %>%
  summarise(r2 = summary(lm(formula = obs ~ pred))[["r.squared"]],
            rmse = 1/length(obs)*sqrt(sum((obs-pred)**2,na.rm = TRUE)))

################################################################################

RS.data <- readRDS("./outputs/test.predictions.SIF.ILF.ERA5.RDS") %>%
  filter(product %in% c("SIF","SIF2","VOD","NIR"))

palette <- kelly(n=17)[2:17]

ggscater2 <- ggplot(RS.data,
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
    limits = c(100,100000), oob = scales::squish,
    trans = "log10") +
  # scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = palette) +
  # stat_smooth(method = "lm", se = FALSE, linetype = 1) +
  coord_equal() +
  # facet_wrap(~)
  theme_bw() +
  labs(x = "", y = "", color = "", fill = "") +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0,8)) +
  scale_y_continuous(limits = c(0,8)) +
  guides(color = "none",
         fill = "none") +
  theme(text = element_text(size = 20))

ggExtra::ggMarginal(ggscater2,
                    type = "density",
                    groupColour = TRUE)

RS.data %>%
  group_by(product) %>%
  summarise(r2 = summary(lm(formula = obs ~ pred))[["r.squared"]],
            rmse = 1/length(obs)*sqrt(sum((obs-pred)**2,na.rm = TRUE)))

