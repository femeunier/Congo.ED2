rm(list = ls())

library(rnaturalearth)

Gabon <- (ne_countries(country = "gabon", type = "countries"))

all <- readRDS("./outputs/df.all.predictions.JRA.RDS") %>%
  filter(lon >= -15, lon <= 45,
         lat >= -25, lat <= 25) %>%
  mutate(model.lat.lon = paste0(model,".",round(lat,digits = 2),".",round(lon,digits = 2)))


search <- all %>%
  filter(year == year[1],month == 1,var == var[1])
models <- unique(all$model)
all.coord <- data.frame()
for (cmodel in models){

  print(cmodel)
  test <- search %>%
    filter(model == cmodel)

  sp <- SpatialPoints(test[,c("lon","lat")])
  e <- as.data.frame(raster::extract(Gabon,sp)) %>%
    filter(sovereignt %in% Gabon[["sovereignt"]])

  all.coord <- bind_rows(all.coord,
                         data.frame(model.lat.lon = test %>%
                           ungroup() %>%
                           mutate(id = 1:n()) %>%
                           filter(id %in% e$id.y) %>%
                           pull(model.lat.lon) ))

}

gabon.df <- all %>%
  filter(model.lat.lon %in% all.coord[["model.lat.lon"]])

gabon.df.sum <- gabon.df %>%
  group_by(lat,lon,model,var) %>%
  summarise(obs.m = mean(obs,na.rm = TRUE),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = gabon.df %>%
         filter(model == "DLEM") %>%
         mutate(var = factor(var,
                             levels = c("gpp","npp","nep"))) %>%
         filter(var %in% c("gpp"))) +
  geom_raster(aes(x=lon,y = lat,
                  fill = as.factor(biome)),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(5, 20), ylim = c(-5, 5), expand = FALSE) +
  facet_wrap(~ model) +
  theme_bw()


gabon.area <- gabon.df %>%
  group_by(var,model,biome,year,month) %>%
  summarise(obs.m = mean(obs,na.rm = TRUE),
            obs.sd = sd(obs,na.rm = TRUE),
            pred.JRA.m = mean(pred.JRA,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")



ggplot(data = gabon.area) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = obs.m,
                group = model),
            color = "black") +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m,
                color = model),
            linetype = 2) +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.JRA.m,
                color = model),
            linetype = 3) +

  scale_x_continuous(limits = c(2020,2024)) +
  facet_grid(var ~ biome,scales = "free") +
  theme_bw() +
  guides(color = "none")


gabon.area.ts <- gabon.area %>%
  group_by(var,model,biome,year) %>%
  summarise(obs.m = mean(obs.m,na.rm = TRUE),
            pred.JRA.m = mean(pred.JRA.m,na.rm = TRUE),
            pred.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

ggplot(data = gabon.area.ts) +

  geom_line(aes(x = year,
                y = obs.m,
                group = model),
            color = "black") +

  geom_line(aes(x = year,
                y = pred.m,
                color = model),
            linetype = 2) +

  geom_line(aes(x = year,
                y = pred.JRA.m,
                color = model),
            linetype = 3) +

  scale_x_continuous(limits = c(2000,2024)) +
  facet_grid(var ~ biome,scales = "free") +
  theme_bw() +
  guides(color = "none")


gabon.area.seasonal <- gabon.area %>%
  group_by(var,model,biome,month) %>%
  summarise(obs.m = mean(obs.m,na.rm = TRUE),
            pred.JRA.m = mean(pred.JRA.m,na.rm = TRUE),
            pred.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")


ggplot(data = gabon.area.seasonal) +

  geom_line(aes(x = month,
                y = obs.m,
                group = model),
            color = "black") +

  geom_line(aes(x = month,
                y = pred.m,
                color = model),
            linetype = 2) +

  # scale_x_continuous(limits = c(2000,2024)) +
  facet_grid(var ~ biome, scales = "free") +
  theme_bw() +
  guides(color = "none")

MEM.area <- gabon.df  %>%
  group_by(var,year,month, biome) %>%
  # MEM
  summarise(obs.sd = sd(obs,na.rm = TRUE),
            obs.m = mean(obs,na.rm = TRUE),

            pred.JRA.sd = sd(pred.JRA,na.rm = TRUE),
            pred.JRA.m = mean(pred.JRA,na.rm = TRUE),

            pred.sd = sd(pred,na.rm = TRUE),
            pred.m = mean(pred,na.rm = TRUE),
            .groups = "keep")


ggplot(data = MEM.area %>%
         filter(var %in% c("nep","gpp")),
       aes(x = year + (month - 1/2)/12,
           y = obs.m,
           group = var),
       color = "black") +

  geom_ribbon(aes(x = year + (month - 1/2)/12,
                  y = pred.m, ymin = pred.m-pred.sd, ymax = pred.m + pred.sd,
                  group = var), alpha = 0.5,
              linetype = 2, fill = "grey", color = NA) +


  # geom_ribbon(aes(x = year + (month - 1/2)/12,
  #                 y = pred.JRA.m, ymin = pred.JRA.m-pred.JRA.sd, ymax = pred.JRA.m + pred.JRA.sd,
  #                 group = var), alpha = 0.5,
  #             linetype = 2, fill = "red", color = NA) +

  geom_line() +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m,
                group = var), color = 'darkblue',
            linetype = 3) +
  geom_hline(yintercept = 0, linetype = 2) +

  # stat_smooth(method = "lm", se = FALSE) +

  facet_wrap(~ biome) +
  scale_x_continuous(limits = c(2000,2024), expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  guides(color = "none")



MEM.area.anomalies <- MEM.area %>%
  group_by(var,biome) %>%
  mutate(bias = mean(pred.JRA.m[!is.na(pred.m)],
                     na.rm = TRUE) - mean(pred.m[!is.na(pred.JRA.m)],
                                          na.rm = TRUE)) %>%
  group_by(var,month,biome) %>%
  mutate(mean.obs = mean(obs.m,na.rm = TRUE)) %>%
  mutate(anomaly.obs = obs.m - mean.obs,
         anomaly.pred = pred.m - mean.obs,
         anomaly.pred.JRA = pred.JRA.m -  bias - mean.obs)

ggplot(data = MEM.area.anomalies %>%
         filter(var %in% c("nep","npp","gpp")),
       aes(x = year + (month - 1/2)/12,
           y = anomaly.obs,
           group = var)) +

  geom_line(color = "black") +

  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.pred,
                group = var), color = 'darkblue',
            linetype = 3) +

  geom_hline(yintercept = 0, linetype = 2) +
  facet_grid(var ~ biome) +
  stat_smooth(method = "lm",
              color = "black",
              se = FALSE) +
  scale_x_continuous(limits = c(2000,2024), expand = c(0,0)) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  labs(x = "",y = "Carbon flux (kgC/m²/yr)") +
  guides(color = "none")



