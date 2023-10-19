rm(list = ls())

library(RCMIP5)

A <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.global.npp.v11.RDS") %>%
  dplyr::select(lon,lat,model) %>%
  distinct()

all.grids <- data.frame()
for (cmodel in unique(A$model)){

  print(cmodel)

  temp <- A %>%
    filter(model == cmodel)

  lons <- sort(unique(temp$lon)) ; lats <- sort(unique(temp$lat))
  all.lons <- seq(min(lons),max(lons),min(diff(lons)))
  all.lats <- seq(min(lats),max(lats),min(diff(lats)))

  Gridarea <- RCMIP5:::calcGridArea(lon = all.lons,
                                    lat = all.lats) %>%
    melt() %>% mutate(Var1 = all.lons[Var1],
                      Var2 = all.lats[Var2]) %>%
    rename(lon = Var1,
           lat = Var2)

  all.grids <- bind_rows(all.grids,
                         Gridarea %>%
                           mutate(model = cmodel))
}

ggplot(data = all.grids) +
  geom_density(aes(x = value, fill = model)) +
  theme_bw()


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = all.grids,
              aes(x = lon, y = lat,
                  fill = value), na.rm = TRUE, alpha = 1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-80, 150),
           ylim = c(-20, 12)) +

  labs(x = "",y = "") +

  facet_wrap(~ model) +

  theme_bw()

saveRDS(all.grids,
        "./outputs/All.grids.RDS")
