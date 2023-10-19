rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)

# system2("scp",paste("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.selected.sites.CB.phn.RDS",
#                     "./outputs/"))

# Data
Climate_AGB_data <- readRDS("/home/femeunier/Documents/projects/YGB/outputs/Climate_AGB_data.RDS") %>%
  mutate(AGB = AGB/20) %>%
  filter(!is.na(value))

df_OP_ensemble_Yoko <- readRDS("./outputs/df.selected.sites.CB.phn.RDS")

df_OP_ensemble_Yoko_final <- df_OP_ensemble_Yoko %>%
  group_by(lat,lon,phen) %>%
  # filter(year == 1800)
  filter(year == max(year))

ggplot(data = df_OP_ensemble_Yoko) +
  geom_line(aes(x = year - min(year),y = AGB, group = interaction(lat,lon))) +
  facet_wrap(~ as.factor(phen)) +
  # scale_y_log10() +
  theme_bw()

select.sites <- readRDS("./outputs/select.sites.RDS") %>% dplyr::select(-AGB)

df_OP_ensemble_Yoko_final_var <- bind_rows(list(

  select.sites %>%
    left_join(df_OP_ensemble_Yoko_final %>% filter(phen == -1),
              by = c("lat","lon")) %>%
    mutate(phen = -1),

  select.sites %>%
    left_join(df_OP_ensemble_Yoko_final %>% filter(phen == 0),
            by = c("lat","lon")) %>%
    mutate(phen = 0),

  select.sites %>%
    left_join(df_OP_ensemble_Yoko_final %>% filter(phen == 2),
              by = c("lat","lon")) %>%
    mutate(phen = 2)))


df_OP_ensemble_Yoko_final %>% mutate(AGB.cat = case_when(AGB < 1 ~ 0,
                                                         AGB < 5 ~ 1,
                                                         AGB < 10 ~ 2,
                                                         AGB < 15 ~ 3,
                                                         AGB < Inf ~ 4)) %>%
  group_by(phen,AGB.cat) %>%
  summarise(N = length(AGB))


climate_AGB <- bind_rows(list(
  Climate_AGB_data %>% mutate(phen = -1,
                              source = "data"),
  Climate_AGB_data %>% mutate(phen = 0,
                              source = "data"),
  Climate_AGB_data %>% mutate(phen = 2,
                              source = "data"),
  df_OP_ensemble_Yoko_final_var %>%
    dplyr::select(lon,lat,var,AGB.tree,var,value,phen) %>%
    rename(AGB = AGB.tree) %>%
    mutate(source = "model")))


df_OP_ensemble_Yoko_final_var.wide <- df_OP_ensemble_Yoko_final_var %>%
  ungroup() %>%
  distinct() %>%
  pivot_wider(names_from = var,
              values_from = value)

summary(lm(data = df_OP_ensemble_Yoko_final_var.wide,
   formula = AGB ~ MAP.cat))


ggplot(data = climate_AGB) +
  geom_boxplot(aes(x = as.factor(value), y = AGB, fill = source),outlier.shape = NA) +
  facet_wrap(as.factor(phen) ~ var) +
  theme_bw()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df_OP_ensemble_Yoko_final %>% filter(phen == 0),
              aes(x = lon, y = lat, fill = AGB.tree),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15)) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()
