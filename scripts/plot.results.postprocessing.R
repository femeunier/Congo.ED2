rm(list = ls())

library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)

suffix <- "all.models.S2"
files <- c(paste0("df.r2.",suffix,"*.RDS"),
           paste0("df.importance.",suffix,"*.RDS"),
           paste0("df.predict.",suffix,"*.RDS"),
           paste0("df.coord.",suffix,"*.RDS"),
           paste0("all.extract.",suffix,"*.RDS"),
           paste0("all.examples.",suffix,"*.RDS"),
           paste0("all.shap_long.",suffix,"*.RDS"),
           paste0("df.shape.scores.",suffix,"*.RDS"))

# files <- c(paste0("df.r2.",suffix,".RDS"),
#            paste0("df.importance.",suffix,".RDS"),
#            paste0("df.predict.",suffix,".RDS"),
#            paste0("df.coord.",suffix,".RDS"),
#            paste0("all.extract.",suffix,".RDS"),
#            paste0("all.examples.",suffix,".RDS"),
#            paste0("all.shap_long.",suffix,".RDS"),
#            paste0("df.shape.scores.",suffix,".RDS"))

for (i in seq(1,length(files))){
  system2("rsync",
          paste("-avz",
                paste0("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",files[i]),
                "./outputs/"))
}

df.coord <- readRDS(paste0("./outputs/df.coord.",suffix,".RDS"))
df.predict <- readRDS(paste0("./outputs/df.predict.",suffix,".RDS"))
df.r2 <- readRDS(paste0("./outputs/df.r2.",suffix,".RDS"))
df.importance <- readRDS(paste0("./outputs/df.importance.",suffix,".RDS"))
df.extract <- readRDS(paste0("./outputs/all.extract.",suffix,".RDS"))
df.example <- readRDS(paste0("./outputs/all.examples.",suffix,".RDS"))
all.shap_long <- readRDS(paste0("./outputs/all.shap_long.",suffix,".RDS"))
df.shape.scores <- readRDS(paste0("./outputs/df.shape.scores.",suffix,".RDS"))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = df.coord %>%
                filter(model == "DLEM"),
              aes(x = lon, y = lat,
                  fill = biome), alpha = 0.5,linewidth = 0.5) +

  geom_sf(data = world,
          fill = NA) +
  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-100, 50),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  # facet_wrap(~ model) +
  theme_map() +
  labs(fill = "") +
  scale_fill_manual(values = c("darkgreen","#72a83d","darkolivegreen3","darkgoldenrod")) +
  theme(text = element_text(size = 20))

df.predict.sample <- df.predict %>%
  group_by(model,biome,var,continent) %>%
  slice_sample(prop = 0.05)

ggplot(df.predict,
       aes(x = pred,
           y = obs)) +
  geom_hex() +

  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(100,10000), oob = scales::squish,
    trans = "log10") +

  facet_grid(biome ~ var) +
  stat_smooth(method = "lm", se = FALSE,
              color = "red", linetype = 2) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")


ggscater <- ggplot(df.predict %>%
                     filter(var == "gpp",
                            model %in% c("CLASSIC","CLM5.0",
                                          "CLM5.0","JULES","ORCHIDEE",
                                          "LPJ"),
                            biome == "Humid_large"),
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
    limits = c(100,10000), oob = scales::squish,
    trans = "log10") +
  scale_color_brewer(palette = "Set2") +
  stat_smooth(method = "lm", se = FALSE, linetype = 1) +
  # coord_equal() +
  # facet_wrap(~)
  theme_bw() +
  theme(legend.position = c(0.1,0.8)) +
  guides(color = "none")

ggExtra::ggMarginal(ggscater, type = "density",
                    groupColour = TRUE)


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

df.r2 %>% filter(model == "ORCHIDEE") %>%
  filter(r2 %in% c(min(r2),max(r2)))

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



selected.dataset.comp <- df.example %>%
  complete(model.lat.lon = unique(df.example$model.lat.lon),
           origin = "model",
           continent = unique(df.example),
           model = unique(df.example$model),
           type = "test",
           year = unique(df.example$year),
           var = unique(df.example$var),
           month = 1:12)

vars2plot <- c("nep")
ggplot(mapping =
         aes(x = year + (month -1/2)/12,y = value,
             group = var)) +
  geom_line(data = df.example  %>%
              filter(origin == "data",
                     model %in% c("CLASSIC"),
                     biome == "Humid_large",
                     var %in% vars2plot) ,
            color = "black") +
  geom_line(data = df.example  %>%
              filter(origin == "model",
                     type == 'test',
                     model %in% c("CLASSIC"),
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



ggplot(data = all.shap_long %>%
         filter(var == "gpp",
                biome == "Humid_large",
                continent == "Africa"),
       aes(x = rfvalue,y = value,
           color = model)) +
  # geom_point() +
  geom_smooth(method = "loess",
              size = 0.4, se = FALSE) +
  facet_grid(var ~ variable, scales = "free_x") +
  theme_bw()
