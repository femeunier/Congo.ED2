rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(rgdal)
library(YGB)

Biomass.Trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.cAGB.v11.RDS") %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  mutate(cAGB = case_when(!is.na(cRoot) ~ cVeg - cRoot,
                          TRUE ~ cVeg*0.7)) %>%                   # Assuming 70% of the vegetation is AGB
  dplyr::filter(!is.na(cAGB)) %>%
  mutate(Continent = case_when(lon <= -20 & lon >= -85 ~ "Amazon",
                               lon <= 55 ~ "Africa",
                               lon > 90 & lat > - 10 ~ "Asia",
                               lon > 100 & lat <= -10 ~ "Australia")) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia"))) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
  filter(Continent %in% c("Amazon","Africa","Asia"))



################################################################################
# ILF
ILF <- readRDS("./outputs/ILF2013.df")

lons <- sort(unique(ILF$lon)) ; lats <- sort(unique(ILF$lat))
all.lons <- seq(min(lons),max(lons),min(diff(lons)))
all.lats <- seq(min(lats),max(lats),min(diff(lats)))

ILF.grid <- ILF %>% left_join(RCMIP5:::calcGridArea(lon = all.lons,
                                                    lat = all.lats) %>%
                                melt() %>% mutate(Var1 = all.lons[Var1],
                                                  Var2 = all.lats[Var2]) %>%
                                rename(lon = Var1,
                                       lat = Var2),
                              by = c("lat","lon"))

dfr <- rasterFromXYZ(ILF.grid[,c("lon","lat","is.undisturbed")])

ILF.grid.sum <- ILF.grid %>%
  filter(abs(lat) < 25,
         is.undisturbed == 1) %>%
  mutate(Continent = case_when(lon <= -20 & lon >= -85 ~ "Amazon",
                               lon <= 55 ~ "Africa",
                               lon > 90 & lat > - 10 ~ "Asia",
                               lon > 100 & lat <= -10 ~ "Australia")) %>%
  group_by(Continent) %>%
  summarise(Area = sum(value)/1e6/1e6,
            .groups = "keep")

all.ILF.grids <- data.frame()
for (cmodel in unique(Biomass.Trendy$model)){

  print(cmodel)
  cdf <- Biomass.Trendy %>% filter(model == cmodel)


  test <- resample.df(ILF.grid,"is.undisturbed",
                      rasterFromXYZ((cdf %>%
                                       ungroup() %>%
                                       filter(year == year[1]) %>%
                                      dplyr::select(lat,lon,cVeg))[,c("lon","lat","cVeg")]))

  all.ILF.grids <- bind_rows(all.ILF.grids,
                             test %>%
                               mutate(model = cmodel))
}

forest.mask2 <- Biomass.Trendy %>%
  group_by(model) %>%
  mutate(is.forest = (cAGB > 10)) %>%
  group_by(model,lat,lon) %>%
  mutate(always.forest = ((sum(is.forest)/length(is.forest)) >= 0.9)) %>%
  dplyr::select(model,lat,lon,always.forest) %>%
  ungroup() %>%
  distinct()

forest.mask <- all.ILF.grids %>%
  mutate(always.forest = case_when(is.undisturbed > 0.5 ~ TRUE,
                                   TRUE ~ FALSE)) %>%
  dplyr::select(model,lat,lon,always.forest) %>%
  ungroup() %>%
  distinct()

raw.grids <- readRDS("./outputs/All.grids.RDS")

################################################################################
all.grids <-raw.grids %>%
  mutate(model.lat.lon = paste(model,lat,lon,sep = ".")) %>%
  filter(model.lat.lon %in% (forest.mask %>%
                               filter(always.forest) %>%
                               mutate(model.lat.lon = paste(model,lat,lon,sep = ".")) %>%
                               pull(model.lat.lon))) %>%
  mutate(Continent = case_when(lon <= -20 & lon >= -85 ~ "Amazon",
                               lon <= 55 ~ "Africa",
                               lon > 90 & lat > - 10 ~ "Asia",
                               lon > 100 & lat <= -10 ~ "Australia")) %>%
  group_by(model,Continent) %>%
  summarise(tot.Area = sum(value)/1e6/1e6,
            .groups = "keep")

all.grids2 <-raw.grids %>%
  mutate(model.lat.lon = paste(model,lat,lon,sep = ".")) %>%
  filter(model.lat.lon %in% (forest.mask2 %>%
                               filter(always.forest) %>%
                               mutate(model.lat.lon = paste(model,lat,lon,sep = ".")) %>%
                               pull(model.lat.lon))) %>%
  mutate(Continent = case_when(lon <= -20 & lon >= -85 ~ "Amazon",
                               lon <= 55 ~ "Africa",
                               lon > 90 & lat > - 10 ~ "Asia",
                               lon > 100 & lat <= -10 ~ "Australia")) %>%
  group_by(model,Continent) %>%
  summarise(tot.Area = sum(value)/1e6/1e6,
            .groups = "keep")
ggplot(data = all.grids) +
  geom_density(aes(x = tot.Area, fill = Continent), alpha = 0.5) +
  theme_bw()


################################################################################

networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan_raw.RDS")

networks2plot <- networks %>%
  group_by(ClusterCode) %>%
  slice_head(n = 1) %>%
  mutate(model = "CABLE-POP")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = forest.mask,
              aes(x = lon, y = lat,
                  fill = always.forest), na.rm = TRUE, alpha = 1) +

  geom_point(data = networks2plot,
             aes(x = Lon, y = Lat), alpha = 0.5, size = 1, shape = 16) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-80, 150),
           ylim = c(-20, 12)) +

  labs(x = "",y = "") +

  scale_shape_manual(values = c(1)) +

  facet_wrap(~ model) +

  theme_bw()

saveRDS(forest.mask,
        "./outputs/forest.mask.RDS")


modelled.sink <- Biomass.Trendy %>%
  filter(model.lat.lon %in% (forest.mask %>%
                               filter(always.forest) %>%
                               mutate(model.lat.lon = paste(model,lat,lon,sep = ".")) %>%
                               pull(model.lat.lon))) %>%
  group_by(lat,lon,model,Continent) %>%
  mutate(sink = c(NA,diff(cAGB))) %>%
  filter(!is.na(sink))

modelled.sink.m <- modelled.sink %>%
  group_by(Continent,model,year) %>%
  summarise(sink.m = mean(sink),
            .groups = "keep")

MEM <- modelled.sink.m %>%
  group_by(Continent,year) %>%
  summarise(sink.m.ens = mean(sink.m),
            .groups = "keep")


data.observed <- readRDS("./outputs/Afritron+Rainfor+Asia+Sullivan_raw_long.RDS") %>%
  filter(Continent != "Australia")

model.inventories <- readRDS("./outputs/Model.ensemble.inventories.RDS") %>%
  filter(Continent != "Australia")

ggplot() +
  # geom_line(data = modelled.sink.m,
  #           aes(x = year, y = sink.m, color = model)) +
  geom_line(data = MEM,
            aes(x = year, y = sink.m.ens), color = "black") +
  geom_line(data = data.observed %>%
              filter(variable %in% c("sink")),
            aes(x = year,
                y = value/10), color = "black",linetype = 2) +

  geom_line(data = model.inventories,
            aes(x = year,
                y = value/10), color = "red",linetype = 1) +

  geom_hline(yintercept = 0, linetype = 2) +
  scale_x_continuous(limits = c(1980,2015)) +
  facet_wrap(~ Continent) +
  theme_bw()

bias <- MEM %>%
  left_join(data.observed %>%
              filter(variable %in% c("sink")) %>%
              dplyr::select(Continent,year,value) %>%
              mutate(value = value/10) %>%
              rename(obs.sink = value),
            by = c("Continent","year")) %>%
  filter(!is.na(sink.m.ens),!is.na(obs.sink)) %>%
  mutate(bias = obs.sink - sink.m.ens) %>%
  group_by(Continent) %>%
  mutate(cumBias = cumsum(bias))

ggplot(data = bias,
       aes(x = year, y = bias, color = Continent, fill = Continent),
       alpha = 0.5) +
  geom_line() +
  stat_smooth(method = "lm") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2)

all.grid.sum <- all.grids %>%
  # filter(!(model %in% c("CABLE-POP","CLASSIC","LPJ-GUESS"))) %>%
  group_by(Continent) %>%
  summarise(tot.Area.m = mean(tot.Area),   # 1e6 km²
            .groups = "keep")

all.grid.sum2 <- all.grids2 %>%
  # filter(!(model %in% c("CABLE-POP","CLASSIC","LPJ-GUESS"))) %>%
  group_by(Continent) %>%
  summarise(tot.Area.m = mean(tot.Area),   # 1e6 km²
            .groups = "keep")


MEM.bias <- bias %>%
  # filter(year > 1995) %>%
  # group_by(Continent) %>%
  # summarise(bias.m = mean(bias,na.rm = TRUE)) %>%       # kgC/m²
  left_join(all.grid.sum,
            by = "Continent") %>%
  mutate(tot.sink = bias * tot.Area.m *1e6* 1e6 * 1e3 / 1e15) %>%  # PgC/yr
  mutate(cumSink = cumsum(tot.sink))


ggplot() +
  geom_line(data = MEM.bias,
            aes(x = year, y = cumSink, color = Continent)) +
  geom_line(data = data.frame(MEM.bias %>%
                         group_by(year) %>%
                         summarise(cumSink = sum(cumSink),
                                   .groups = "keep")),
            aes(x = year, y = cumSink),
            color = "black") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2)

