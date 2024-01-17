rm(list = ls())

library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)
library(sf)

suffix <- "JRA_historical"
files <- c(paste0("df.r2.",suffix,"*.RDS"),
           paste0("df.importance.",suffix,"*.RDS"),
           paste0("df.predict.",suffix,"*.RDS"),
           paste0("df.seasonal.",suffix,"*.RDS"),
           paste0("df.ts.",suffix,"*.RDS"),
           paste0("df.coord.",suffix,"*.RDS"),
           paste0("all.extract.",suffix,"*.RDS"),
           paste0("all.examples.",suffix,"*.RDS"),
           paste0("all.shap_long.",suffix,"*.RDS"),
           paste0("df.shape.scores.",suffix,"*.RDS"))

# files <- c(paste0("df.r2.",suffix,"*.RDS"))
# files <- c(paste0("df.r2.",suffix,".RDS"),
#            paste0("df.importance.",suffix,".RDS"),
#            paste0("df.predict.",suffix,".RDS"),
#            paste0("df.coord.",suffix,".RDS"),
#            paste0("all.extract.",suffix,".RDS"),
#            paste0("all.examples.",suffix,".RDS"),
#            paste0("all.shap_long.",suffix,".RDS"),
#            paste0("df.shape.scores.",suffix,".RDS"))

# for (i in seq(1,length(files))){
#   system2("rsync",
#           paste("-avz",
#                 paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",files[i]),
#                 "./outputs/"))
# }

vars <- c("gpp","npp","nep")
vars <- c("gpp")

df.r2 <- df.coord <- df.importance <- df.predict <-
  df.extract <- df.example <- df.seasonal <- df.ts <- data.frame()
for (cvar in vars) {

  print(cvar)

  df.r2 <- bind_rows(df.r2,
                     readRDS(paste0(
                       "./outputs/df.r2.", suffix, ".", cvar, ".RDS"
                     )))
  df.coord <- bind_rows(df.coord,
                     readRDS(paste0(
                       "./outputs/df.coord.", suffix, ".", cvar, ".RDS"
                     )))

  # df.predict <- bind_rows(df.predict,
  #                         readRDS(paste0(
  #                           "./outputs/df.predict.", suffix, ".", cvar, ".RDS"
  #                         )))


  # df.ts <- bind_rows(df.ts,
  #                         readRDS(paste0(
  #                           "./outputs/df.ts.", suffix, ".", cvar, ".RDS"
  #                         )))


  df.seasonal <- bind_rows(df.seasonal,
                          readRDS(paste0(
                            "./outputs/df.seasonal.", suffix, ".", cvar, ".RDS"
                          )))

  df.importance <- bind_rows(df.importance,
                     readRDS(paste0(
                       "./outputs/df.importance.", suffix, ".", cvar, ".RDS"
                     )))

  # df.extract <- bind_rows(df.extract,
  #                         readRDS(paste0(
  #                           "./outputs/all.extract.", suffix, ".", cvar, ".RDS"
  #                         )))
  #
  # df.example <- bind_rows(df.example,
  #                            readRDS(paste0(
  #                              "./outputs/all.examples.", suffix, ".", cvar, ".RDS"
  #                            )))

}

# all.shap_long <- readRDS(paste0("./outputs/all.shap_long.",suffix,".RDS"))
# df.shape.scores <- readRDS(paste0("./outputs/df.shape.scores.",suffix,".RDS"))

Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = df.coord %>%
                filter(model == "CABLE-POP"),
              aes(x = lon, y = lat,
                  fill = biome), alpha = 0.5,linewidth = 0.5) +

  geom_sf(data = world,
          fill = NA) +
  geom_sf(data = Amazon.shp,fill = NA, color = "red", fill = NA) +

  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-100, 150),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  # facet_wrap(~ model) +
  theme_map() +
  labs(fill = "") +
  scale_fill_manual(values = c("darkgreen","#72a83d","darkolivegreen3","darkgoldenrod")) +
  theme(text = element_text(size = 20),
        legend.position = "top")


# df.predict.sample <- df.predict %>%
#   group_by(model,biome,var,continent) %>%
#   slice_sample(prop = 0.01)
#
# ggplot(df.predict.sample,
#        aes(x = pred,
#            y = obs)) +
#   geom_hex() +
#
#   geom_abline(slope = 1, intercept = 0, linetype = 1) +
#   scale_fill_gradient(
#     low = NA,
#     high = "black",
#     limits = c(100,10000), oob = scales::squish,
#     trans = "log10") +
#
#   facet_grid(biome ~ var) +
#   stat_smooth(method = "lm", se = FALSE,
#               color = "red", linetype = 2) +
#   coord_equal() +
#   theme_bw() +
#   guides(fill = "none")
#
#
# ggscater <- ggplot(df.predict.sample %>%
#                      filter(var == "nep",
#                             model %in% c("CABLE-POP","CLM5.0",
#                                           "CLM5.0","JULES","ORCHIDEE",
#                                           "LPJ"),
#                             biome == "Humid_large"),
#                    aes(x = pred,
#                        y = obs,
#                        color = model)) +
#   geom_point(shape = NA) +
#   geom_hex(color = NA) +
#   geom_abline(slope = 1, intercept = 0, linetype = 2) +
#   geom_hline(yintercept = 0, linetype = 1, color = "black") +
#   geom_vline(xintercept = 0, linetype = 1, color = "black") +
#   scale_fill_gradient(
#     low = NA,
#     high = "black",
#     limits = c(100,10000), oob = scales::squish,
#     trans = "log10") +
#   scale_color_brewer(palette = "Set2") +
#   stat_smooth(method = "lm", se = FALSE, linetype = 1) +
#   # coord_equal() +
#   # facet_wrap(~)
#   theme_bw() +
#   theme(legend.position = c(0.1,0.8)) +
#   guides(color = "none")
#
# ggExtra::ggMarginal(ggscater, type = "density",
#                     groupColour = TRUE)


ggplot(data = df.r2) +
  geom_density(aes(x = r2,
                   fill = var),alpha = 0.5) +
  facet_wrap(~ model,
             scales = "free_y") +
  theme_bw()

ggplot(data = df.r2) +
  geom_density(aes(x = r2,
                   fill = var),
               alpha = 0.5) +
  facet_grid(continent ~ biome) +
  theme_bw()

ggplot(data = df.r2) +
  geom_density(aes(x = r2,
                   fill = var),alpha = 0.5) +
  facet_grid(~ continent) +
  theme_bw()

ggplot(data = df.r2) +
  geom_density(aes(x = r2,
                   fill = var),alpha = 0.5) +
  facet_grid(~ biome) +
  theme_bw()


vars2plot <- c("gpp","npp","nep")

ggplot(mapping =
         aes(x = year + (month -1/2)/12,y = value,
             group = var)) +
  geom_line(data = df.example  %>%
              filter(origin == "data",
                     model %in% c("CABLE-POP"),
                     biome == "Humid_large",
                     var %in% vars2plot) ,
            color = "black") +
  geom_point(data = df.example  %>%
              filter(origin == "model",
                     type == 'test',
                     model %in% c("CABLE-POP"),
                     biome == "Humid_large",
                     var %in% vars2plot) %>%
              filter(!is.na(value)),
            color = "darkblue") +
  scale_x_continuous(limits = c(2000,2020)) +
  facet_grid(continent ~ model) +
  theme_bw()


level.ord <- df.importance %>%
  group_by(Feature) %>%
  summarise(Gain.m = median(Gain)) %>%
  arrange((Gain.m)) %>% pull(Feature)

df.importance.sum <- df.importance %>%
  group_by(continent,biome,var,Feature) %>%
  summarise(Gain.m = mean(Gain),
            Gain.sd = sd(Gain),
            .groups = "keep") %>%
  group_by(continent,biome,var) %>%
  mutate(minimp = sort(Gain.m,decreasing = TRUE)[10]) %>%
  ungroup() %>%
  filter(Feature %in% unique(Feature[Gain.m >= minimp])) %>%
  ungroup() %>%
  mutate(Feature = factor(Feature,
                          levels = level.ord[level.ord %in% Feature]))

ggplot(data = df.importance.sum,
       aes(x = Feature, y = Gain.m,
           fill = continent)) +
  geom_errorbar(aes(ymin = Gain.m*0.9,
                    ymax = Gain.m + Gain.sd),
                position = position_dodge(0.9),
                width = 0.5) +
  geom_bar(alpha = 1,
           stat = "identity",
           position = position_dodge(0.9)) +
  facet_grid(biome ~ var) +
  coord_flip() +
  theme_bw()

ggplot(data = df.importance.sum,
       aes(x = Feature, y = Gain.m,
           fill = var)) +
  geom_bar(alpha = 1,
           stat = "identity",
           position = position_dodge(0.9)) +
  facet_grid(biome ~ continent) +
  coord_flip() +
  theme_bw()


df.seasonal.MEM <- df.seasonal %>%
  group_by(month, origin, biome, continent,var) %>%
  summarise(value.m.MEM = mean(value.m,na.rm = TRUE),
            value.sd.MEM = sd(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.seasonal.MEM %>%
         filter(var %in% c("nep")),
       aes(x = month, y = value.m.MEM,
           color = origin, fill = origin,
           ymin = value.m.MEM - value.sd.MEM,
           ymax = value.m.MEM + value.sd.MEM,
           group = interaction(var,origin))) +
  geom_ribbon(color = NA,alpha = 0.5) +
  geom_line() +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(biome ~ continent)



df.ts.MEM <- df.ts %>%
  # filter(model == "CABLE-POP") %>%
  group_by(year, origin, biome, continent,var) %>%
  summarise(value.m.MEM = mean(value.m,na.rm = TRUE),
            value.sd.MEM = sd(value.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df.ts.MEM %>%
         filter(var %in% c("nep")),
       aes(x = year, y = value.m.MEM,
           color = origin, fill = origin,
           ymin = value.m.MEM - value.sd.MEM,
           ymax = value.m.MEM + value.sd.MEM,
           group = interaction(var,origin))) +
  geom_ribbon(color = NA,alpha = 0.5) +
  geom_line() +
  theme_bw() +
  scale_x_continuous(limits = c(1980,2020)) +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(biome ~ continent)


# df.ts <- readRDS("./outputs/df.all.ts.JRA.historical.RDS")
#
# df.ts.MEM <- bind_rows(df.ts %>%
#   # filter(origin == "data") %>%
#   group_by(year, month, biome, continent,var) %>%
#   summarise(value.m.MEM = mean(obs.m,na.rm = TRUE),
#             value.sd.MEM = sd(obs.m,na.rm = TRUE),
#             .groups = "keep") %>%
#     mutate(model = "MEM"),
#   df.ts %>%
#     # filter(origin == "data") %>%
#     group_by(year, month, biome, model,continent,var) %>%
#     summarise(value.m.MEM = mean(obs.m,na.rm = TRUE),
#               value.sd.MEM = sd(obs.m,na.rm = TRUE),
#               .groups = "keep"))
#
# saveRDS(df.ts.MEM,
#         "./outputs/Trendy.MEM.CC.RDS")


# ggplot(data = all.shap_long %>%
#          filter(var == "gpp",
#                 biome == "Humid_large",
#                 continent == "Africa"),
#        aes(x = rfvalue,y = value,
#            color = model)) +
#   # geom_point() +
#   geom_smooth(method = "loess",
#               size = 0.4, se = FALSE) +
#   facet_grid(var ~ variable, scales = "free_x") +
#   theme_bw()
