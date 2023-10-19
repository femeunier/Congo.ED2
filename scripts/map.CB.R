rm(list = ls())

library(ncdf4)
library(ggplot2)
library(dplyr)
library(reshape2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(raster)
library(tidyr)
library(plotbiomes)

world <- ne_countries(scale = "medium", returnclass = "sf")
mask <- readRDS("/home/femeunier/Documents/projects/YGB/data/LandSeaMask.RDS")

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_CB.RDS",
                      "./outputs/"))

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_CB_Efiles.RDS",
                      "./outputs/"))

df_CB <- readRDS(file.path("/home/femeunier/Documents/projects/Congo.ED2/outputs/","df_CB.RDS"))
df_CB_Efiles <- readRDS(file.path("/home/femeunier/Documents/projects/Congo.ED2/outputs/","df_CB_Efiles.RDS"))
df.av.E <- df_CB_Efiles %>%
  group_by(lat,lon) %>%
  summarise(AGB = mean(AGB),
            AGB.trees = mean(AGB.trees),
            AGB.trees = mean(AGB.trees),
            LAI = mean(LAI),
            LAI.trees = mean(LAI.trees),
            LAI.grass = mean(LAI.grass),
            evap = mean(evap),
            transp = mean(transp),
            et = mean(et),
            precip = mean(precip),
            sw = mean(sw),
            gpp = mean(gpp),
            nep = mean(nep),
            .groups = "keep")


df <- df_CB %>% left_join(mask,
                          by = c("lat","lon")) %>%
  filter(mask == 1) %>%
  group_by(lat,lon) %>%
  mutate(max.yr = max(yr))

ggplot(data = df %>% dplyr::filter(max.yr < 1850,
                                   yr == max.yr)) +
  geom_bar(aes(x = max.yr)) +
  theme_bw()

table(df$max.yr)


df.final <- df %>%
  group_by(lat,lon) %>%
  filter(yr == max.yr) %>%
  mutate(AGB.bnd = case_when(AGB >= 30 ~ 30,
                             TRUE ~ AGB))

df.final %>% nrow()

summary(unique(df.final$AGB))
# hist(df.final$yr)

ggplot(data = world) +
  geom_tile(data = df_CB_Efiles,
            aes(x = lon, y = lat,fill = AGB),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = df_CB_Efiles) +
  geom_density(aes(x = nep)) +
  theme_bw()

summary(df_CB_Efiles$nep)

ggplot(data = df.final) +
  geom_density(aes(x = LAI)) +
  theme_bw()


# Compare with data

new.raster <- rasterFromXYZ(expand.grid(sort(unique(df.final$lon)),
                                        sort(unique(df.final$lat))) %>% rename(lon = Var1,
                                                                              lat = Var2) %>%
                              mutate(v = NA))

# Data
Avi.AFR <- stack("/home/femeunier/Documents/projects/SoilSensitivity/data/AGBavit_AFR.gri")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Avi.AFR.crop <- crop(Avi.AFR, e)
Avi.AFR.crop.rspld <- resample(Avi.AFR.crop/20,new.raster)


df.final.data <- bind_rows(list(df.final %>%
  dplyr::select(lat,lon,AGB) %>%
  mutate(type = "Model") ,
  as.data.frame(Avi.AFR.crop.rspld,xy = TRUE) %>%
    rename(lon = x,
           lat = y,
           AGB = Avitabile_AGB_Map) %>%
    mutate(type = "Data")))

ggplot(data = df.final.data) +
  geom_density(aes(x = AGB, linetype = as.factor(type)),
               fill = NA,
               show.legend = FALSE) +
  theme_bw()


df.sum <- df %>%
  group_by(yr) %>%
  summarise(AGB.m = mean(AGB,na.rm = TRUE),
            AGB.sd = sd(AGB,na.rm = TRUE),
            LAI.m = mean(LAI.trees ,na.rm = TRUE),
            LAI.sd = sd(LAI.trees,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df) +
  geom_line(aes(x = yr, y = AGB, group = interaction(lat,lon)),
            size = 0.1, color = "darkgrey") +
  geom_ribbon(data = df.sum,
              aes(x = yr, y = AGB.m,
                  ymin = AGB.m - AGB.sd,
                  ymax = AGB.m + AGB.sd), color = NA, fill = "black",alpha = 0.4) +
  geom_line(data = df.sum,
            aes(x = yr, y = AGB.m), color = "red") +
  theme_bw()

ggplot(data = df) +
  geom_line(aes(x = yr, y = LAI, group = interaction(lat,lon)),
            size = 0.1, color = "darkgrey") +
  geom_ribbon(data = df.sum,
              aes(x = yr, y = LAI.m,
                  ymin = LAI.m - LAI.sd,
                  ymax = LAI.m + LAI.sd), color = NA, fill = "black",alpha = 0.4) +
  geom_line(data = df.sum,
            aes(x = yr, y = LAI.m), color = "red") +
  theme_bw()



system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/climate_BC_ERA5.RDS",
                      "./outputs/"))

df_climate <- readRDS(file.path("/home/femeunier/Documents/projects/Congo.ED2/outputs/","climate_BC_ERA5.RDS"))

