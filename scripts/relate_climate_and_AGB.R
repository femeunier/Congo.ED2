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
library(YGB)
library(tidyr)


world <- ne_countries(scale = "medium", returnclass = "sf")
mask <- readRDS("/home/femeunier/Documents/projects/YGB/data/LandSeaMask.RDS")

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_CB.RDS",
#                       "./outputs/"))
# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/df_CB_Efiles.RDS",
#                       "./outputs/"))

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


df_CB <- readRDS(file.path("/home/femeunier/Documents/projects/Congo.ED2/outputs/","df_CB.RDS"))


df <- df_CB %>% left_join(mask,
                          by = c("lat","lon")) %>%
  filter(mask == 1) %>%
  group_by(lat,lon) %>%
  mutate(max.yr = max(yr)) %>%
  filter(yr == max.yr) %>%
  mutate(AGB.bnd = case_when(AGB >= 30 ~ 30,
                             TRUE ~ AGB)) %>%
  left_join(df.av.E %>% dplyr::select(lat,lon,gpp,nep,et),
            by = c("lat","lon"))

ggplot(data = world) +
  geom_tile(data = df,
            aes(x = lon, y = lat,fill = AGB*20),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()

# system2("rsync",paste("-avz",
#                       "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/climate_BC_ERA5.RDS",
#                       "./outputs/"))

climate_CB <- readRDS("./outputs//climate_BC_ERA5.RDS") %>%
  mutate(month.n = as.numeric(factor(month,
                                     levels = c("JAN","FEB","MAR","APR","MAY","JUN",
                                                "JUL","AUG","SEP","OCT","NOV","DEC")))) %>%
  arrange(year,month.n) %>%
  mutate(Ndays = lubridate::days_in_month(paste0(year,"/",sprintf("%02d",month.n),"/01"))) %>%
  mutate(Pmm = prate*Ndays*86400)


climate_av <- climate_CB %>%
  filter(!is.na(Pmm)) %>%
  dplyr::select(lon,lat,Pmm,sw,year,month.n) %>%
  rename(month = month.n) %>%
  group_by(month,lon,lat) %>%
  summarise(Pmm = mean(Pmm),
            sw = mean(sw),
            .groups = "keep") %>%
  group_by(lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD)) %>%
  summarise(MAP = sum(Pmm),
            SW = mean(sw),
            MCWD = unique(MCWD),
            .groups = "keep")

# climate_av %>% arrange(desc(MAP))
# 34    -9 6902.  244. -130.
# 29     0 5848.  203.    0
# 27    -9 4687.  220. -249.
# df.select <- climate_CB %>% filter(lon == 26,lat == -9)
# plot(df.select$Pmm,type = "l")


ggplot(data = world) +
  geom_tile(data = climate_av,
            aes(x = lon, y = lat,fill = MAP),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "blue",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()

climate_vs_agb <- df %>%
  left_join(climate_av,
            by = c("lat","lon"))

plot(climate_vs_agb$MCWD,climate_vs_agb$AGB)

ggplot(data = climate_vs_agb) +
  geom_point(aes(x = MCWD,y = MAP, color = AGB)) +
  geom_hline(yintercept = 1000) +
  geom_vline(xintercept = -250) +
  theme_bw()


climate_vs_agb.cat <- climate_vs_agb %>%
  mutate(cat = case_when(MCWD > -250 ~ 2,
                         MAP < 1000 ~ 1,
                         TRUE ~ 3))

ggplot(data = climate_vs_agb.cat) +
  geom_boxplot(aes(x = as.factor(cat), y = AGB*20, fill = as.factor(cat))) +
  scale_y_log10() +
  theme_bw()

ggplot(data = climate_vs_agb.cat) +
  geom_boxplot(aes(x = as.factor(cat), y = gpp, fill = as.factor(cat))) +
  theme_bw()



delta.quant = 0.1
MAP.quant <- as.numeric(quantile(climate_vs_agb.cat$MAP,seq(delta.quant,1-delta.quant,delta.quant)))
MCWD.quant <- as.numeric(quantile(climate_vs_agb.cat$MCWD,seq(delta.quant,1-delta.quant,delta.quant)))
SW.quant <- as.numeric(quantile(climate_vs_agb.cat$SW,seq(delta.quant,1-delta.quant,delta.quant)))


MAP.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
                   MAP.quant) +
  diff(c(climate_vs_agb.cat %>% filter(!is.na(MAP)) %>% pull(MAP) %>% min(),
         MAP.quant,
         climate_vs_agb.cat %>% filter(!is.na(MAP)) %>% pull(MAP) %>% max()))/2

SW.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(SW)) %>% pull(SW) %>% min(),
                  SW.quant) +
  diff(c(climate_vs_agb.cat %>% filter(!is.na(SW)) %>% pull(SW) %>% min(),
         SW.quant,
         climate_vs_agb.cat %>% filter(!is.na(SW)) %>% pull(SW) %>% max()))/2

MCWD.quant.all <- c(climate_vs_agb.cat %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
                    MCWD.quant) +
  diff(c(climate_vs_agb.cat %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% min(),
         MCWD.quant,
         climate_vs_agb.cat %>% filter(!is.na(MCWD)) %>% pull(MCWD) %>% max()))/2

df.sum <- climate_vs_agb.cat %>%
  mutate(MAP.cat = classify.quant(MAP,MAP.quant),
         MCWD.cat = classify.quant(MCWD,MCWD.quant),
         SW.cat = classify.quant(SW,SW.quant)) %>%
  mutate(MAP.cat.abs = MAP.quant.all[MAP.cat],
         SW.cat.abs = SW.quant.all[SW.cat],
         MCWD.cat.abs = MCWD.quant.all[MCWD.cat])

df.long <- df.sum %>%
  dplyr::select(lon,lat,AGB,gpp,nep,
                MAP.cat,SW.cat,MCWD.cat) %>%
  pivot_longer(cols = c(MAP.cat,SW.cat,MCWD.cat),
               names_to = "var",
               values_to = "value")

new.raster <- rasterFromXYZ(expand.grid(sort(unique(df.long$lon)),
            sort(unique(df.long$lat))) %>% rename(lon = Var1,
                                                  lat = Var2) %>%
  mutate(v = NA))

# Data
Avi.AFR <- stack("/home/femeunier/Documents/projects/SoilSensitivity/data/AGBavit_AFR.gri")

e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
Avi.AFR.crop <- crop(Avi.AFR, e)
Avi.AFR.crop.rspld <- resample(Avi.AFR.crop,new.raster)

AGB.data.map <- as.data.frame(Avi.AFR.crop.rspld,
                              xy = TRUE) %>%
  rename(lon = x,
         lat = y)

# Climate_AGB_data <- readRDS("/home/femeunier/Documents/projects/YGB/outputs/Climate_AGB_data.RDS")
#
# ggplot(data = df.long) +
#   geom_boxplot(aes(x = value,y = AGB*20, group = value),outlier.shape = NA) +
#   facet_wrap(~var, scales = "free") +
#   theme_bw()

Climate_AGB <- df.long %>%
  left_join(AGB.data.map,
            by = c("lon","lat")) %>%
  mutate(AGB = AGB*20) %>%
  rename(model = AGB,
         data = Avitabile_AGB_Map) %>%
  ungroup() %>%
  pivot_longer(cols = c("model","data"),
               names_to = "type",
               values_to = "AGB")



delta = 0.

Climate_AGB_sum <- Climate_AGB %>%
  group_by(type,var,value) %>%
  summarise(AGB.med = median(AGB,na.rm = TRUE),
            AGB.mean = mean(AGB,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(value = case_when(type == "data" ~ (value - delta),
                           type == "model" ~ (value + delta)))

ggplot(data = Climate_AGB) +

  geom_boxplot(aes(x = (value),y = AGB,
                   fill = type,
                   group = interaction(type,value)),
               outlier.shape = NA,
               alpha = 0.25) +

  geom_line(data = Climate_AGB_sum,
            aes(x = value,y = AGB.med,
                color = type),
            alpha = 1, size = 1) +

  facet_grid(~var, scales = "free_x") +

  scale_x_continuous(breaks = seq(1,10)) +

  theme_bw()



ggplot(data = Climate_AGB %>% dplyr::filter(type == "model")) +

  geom_boxplot(aes(x = (value),y = nep,
                   fill = type,
                   group = interaction(type,value)),
               outlier.shape = NA,
               alpha = 0.25) +
  facet_grid(~var, scales = "free_x") +
  geom_hline(yintercept = 0) +
  theme_bw()






