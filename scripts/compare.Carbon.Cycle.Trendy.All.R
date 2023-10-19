rm(list = ls())

library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
library(data.table)
library(scales)
library(TrENDY.analyses)
library(raster)
library(stringr)
library(ggridges)
library(Congo.ED2)

carbon_content 	<- 0.456			# Martin et al. 2018 Nature Geoscience
default.woody.ratio.of.npp <- 0.3   # Malhi et al. 2011, https://royalsocietypublishing.org/doi/10.1098/rstb.2011.0062 (From Koch Suppl. data Table S3), varies between 0.3 and 0.5

#####################################################################################################
# Data

networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan.RDS")
networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan_raw.RDS")

networks2plot <- networks %>%
  group_by(ClusterCode) %>%
  slice_head(n = 1)

sink.data.m <- networks %>%
  group_by(PlotCode) %>%
  summarise(sink.obs = mean(AGBnetchange.ha.yr)*carbon_content) %>%
  rename(plot = PlotCode)

#####################################################################################################
# Trendy Models

Biomass.Trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.cAGB.v11.RDS") %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  mutate(cAGB = case_when(!is.na(cRoot) ~ cVeg - cRoot,
                          TRUE ~ cVeg*0.7)) %>%                   # Assuming 70% of the vegetation is AGB
  dplyr::filter(!is.na(cAGB)) %>%
  mutate(Continent = case_when(lon <= -20 ~ "Amazon",
                               lon <= 55 ~ "Africa",
                               lon > 90 & lat > - 10 ~ "Asia",
                               lon > 100 & lat <= -10 ~ "Australia")) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia"))) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

types <- Biomass.Trendy %>%
  group_by(model) %>%
  summarise(no.Root = all(cAGB == cVeg))

ggplot(data = Biomass.Trendy) +
  geom_boxplot(aes(x = model, y = 1 - cRoot/cVeg, fill = model), outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1)) +
  labs(x = "", fill = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme_bw()

Biomass.Trendy.search.Amazon <- Biomass.Trendy %>%
  ungroup() %>%
  dplyr::filter(lon >= -80,lon <= -35) %>%
  dplyr::filter(lat >= (min(networks %>% filter(Continent == "Amazon") %>% pull(Lat),na.rm = TRUE) - 1),
                lat <= (max(networks %>% filter(Continent == "Amazon") %>% pull(Lat),na.rm = TRUE) + 1),
                lon >= (min(networks %>% filter(Continent == "Amazon") %>% pull(Lon),na.rm = TRUE) - 1),
                lon <= (max(networks %>% filter(Continent == "Amazon") %>% pull(Lon),na.rm = TRUE) + 1)) %>%
  mutate(year = floor(year))

Biomass.Trendy.search.Africa <- Biomass.Trendy %>%
  ungroup() %>%
  dplyr::filter(lon >= -12,lon <= 45) %>%
  dplyr::filter(lat >= (min(networks %>% filter(Continent == "Africa") %>% pull(Lat),na.rm = TRUE) - 1),
                lat <= (max(networks %>% filter(Continent == "Africa") %>% pull(Lat),na.rm = TRUE) + 1),
                lon >= (min(networks %>% filter(Continent == "Africa") %>% pull(Lon),na.rm = TRUE) - 1),
                lon <= (max(networks %>% filter(Continent == "Africa") %>% pull(Lon),na.rm = TRUE) + 1)) %>%
  mutate(year = floor(year))

Biomass.Trendy.search.Asia <- Biomass.Trendy %>%
  ungroup() %>%
  dplyr::filter(lon >= 75) %>%
  dplyr::filter(lat >= (min(networks %>% filter(Continent %in% c("Asia","Australia")) %>% pull(Lat),na.rm = TRUE) - 1),
                lat <= (max(networks %>% filter(Continent %in% c("Asia","Australia")) %>% pull(Lat),na.rm = TRUE) + 1),
                lon >= (min(networks %>% filter(Continent %in% c("Asia","Australia")) %>% pull(Lon),na.rm = TRUE) - 1),
                lon <= (max(networks %>% filter(Continent %in% c("Asia","Australia")) %>% pull(Lon),na.rm = TRUE) + 1)) %>%
  mutate(year = floor(year))

Biomass.Trendy.mean.sink <- Biomass.Trendy %>%
  ungroup() %>%
  mutate(year.int = floor(year)) %>%
  group_by(Continent,lon,lat,model,scenario) %>%
  summarise(AGB_init = cAGB[year.int == min(year.int)],
            AGB_end = cAGB[year.int == max(year.int)],
            year_init = min(year.int),
            year_end = max(year.int),
            .groups = "keep") %>%
  mutate(delta_AGB = AGB_end - AGB_init,
         delta_t = year_end - year_init) %>%
  mutate(sink = 10*delta_AGB/delta_t)

gpp.npp.trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.Pantropical.GPP.NPP.v11.RDS") %>%
  ungroup() %>%
  distinct() %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

# npp.trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.npp_v11.RDS") %>%
#   rename(npp = value) %>%
#   dplyr::select(-c(scenario,variable)) %>%
#   dplyr::filter(!is.na(npp)) %>%
#   ungroup() %>%
#   distinct() %>%
#   mutate(model.lat.lon = paste0(model,".",lat,".",lon))

gpp.npp.trendy.Amazon <- gpp.npp.trendy %>%
  ungroup() %>%
  dplyr::filter(lon >= -80,lon <= -35)

gpp.npp.trendy.Africa <- gpp.npp.trendy %>%
  ungroup() %>%
  dplyr::filter(lon >= -12,lon <= 45)

gpp.npp.trendy.Asia <- gpp.npp.trendy %>%
  ungroup() %>%
  dplyr::filter(lon >= 75)

gpp.npp.trendy.mean <- gpp.npp.trendy %>%
  group_by(lon,lat,model) %>%
  summarise(npp.m = mean(npp),
            gpp.m = mean(gpp),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = Biomass.Trendy.mean.sink,
              aes(x = lon, y = lat,
                  fill = sink), na.rm = TRUE, alpha = 1) +

  geom_point(data = networks2plot,
             aes(x = Lon, y = Lat), alpha = 0.5) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-80, 150),
           ylim = c(-20, 12)) +

  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +

  scale_shape_manual(values = c(1)) +

  facet_wrap(~ model) +

  theme_bw()


# Regrid
biome <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/biome1961.1990.RDS")
biome.rst <- raster::rasterFromXYZ(biome[,c("lon","lat","tmp")])
res.lat <- res(biome.rst)[2]; res.lon <- res(biome.rst)[1]


e <- as(extent(-150, 180, -25, 25), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
biome.rst.crop <- crop(biome.rst, e)

df.sink.regrid <- df.npp.regrid <- df.gpp.regrid <- df.AGB.regrid <- data.frame()
for (cmodel in unique(Biomass.Trendy.mean.sink[["model"]])){
  print(cmodel)
  Biomass.Trendy.mean.sink.regrid <- TrENDY.analyses::resample.df.all.col(bigdf = Biomass.Trendy.mean.sink %>%
                                                                            ungroup() %>%
                                                                            filter(model == cmodel) %>%
                                                                            dplyr::select(lon,lat,model,sink),
                                                                          raster2resample = biome.rst.crop,
                                                                          var.names = "sink",
                                                                          res = 0.888889)

  npp.regrid <- resample.df.all.col(bigdf = gpp.npp.trendy.mean %>%
                                      ungroup() %>%
                                      filter(model == cmodel) %>%
                                      dplyr::select(lon,lat,model,npp.m),
                                    raster2resample = biome.rst.crop,
                                    var.names = "npp.m",
                                    res = 0.888889)

  gpp.regrid <- resample.df.all.col(bigdf = gpp.npp.trendy.mean %>%
                                      ungroup() %>%
                                      filter(model == cmodel) %>%
                                      dplyr::select(lon,lat,model,gpp.m),
                                    raster2resample = biome.rst.crop,
                                    var.names = "gpp.m",
                                    res = 0.888889)

  AGB.regrid <- resample.df.all.col(bigdf = Biomass.Trendy.mean.sink %>%
                                      ungroup() %>%
                                      filter(model == cmodel) %>%
                                      dplyr::select(lon,lat,model,AGB_end),
                                    raster2resample = biome.rst.crop,
                                    var.names = "AGB_end",
                                    res = 0.888889)

  df.sink.regrid <- bind_rows(list(df.sink.regrid,
                                   Biomass.Trendy.mean.sink.regrid %>% mutate(model = cmodel)))

  df.npp.regrid <- bind_rows(list(df.npp.regrid,
                                  npp.regrid %>% mutate(model = cmodel)))

  df.gpp.regrid <- bind_rows(list(df.gpp.regrid,
                                  gpp.regrid %>% mutate(model = cmodel)))

  df.AGB.regrid <- bind_rows(list(df.AGB.regrid,
                                  AGB.regrid %>% mutate(model = cmodel)))
}

df.sink.regrid.mean <- df.sink.regrid %>%
  group_by(lat,lon) %>%
  summarise(sink.m = mean(sink,na.rm = TRUE),
            Nmodels = sum(!is.na(sink)),
            .groups = "keep")

df.npp.regrid.mean <- df.npp.regrid %>%
  group_by(lat,lon) %>%
  summarise(Nmodels = sum(!is.na(npp.m)),
            npp.m = mean(npp.m,na.rm = TRUE),
            .groups = "keep")

df.gpp.regrid.mean <- df.gpp.regrid %>%
  group_by(lat,lon) %>%
  summarise(Nmodels = sum(!is.na(gpp.m)),
            gpp.m = mean(gpp.m,na.rm = TRUE),
            .groups = "keep")

df.AGB.regrid.mean <- df.AGB.regrid %>%
  group_by(lat,lon) %>%
  summarise(AGB = mean(AGB_end,na.rm = TRUE),
            Nmodels = sum(!is.na(AGB_end)),
            .groups = "keep")

ggplot() +

  geom_raster(data = df.sink.regrid.mean %>%
                filter(!is.na(sink.m)) %>%
                filter(Nmodels == 16),
              aes(x = lon, y = lat,
                  fill = sink.m), na.rm = TRUE, alpha = 1) +

  geom_point(data = networks2plot,
             aes(x = Lon, y = Lat), shape = 1, alpha = 1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-80, 150),
           ylim = c(-20, 12)) +

  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "transparent",
                       limits = c(-0.25,0.5),
                       oob = scales::squish) +
  labs(x = "",y = "", fill = "Sink") +

  # scale_shape_manual(values = c(2,10)) +

  theme_bw() +
  theme(legend.position="bottom")


ggplot(data = df.sink.regrid.mean %>%
         mutate(lat = round(lat)) %>%
         group_by(lat) %>%
         summarise(sink.m = mean(sink.m,na.rm = TRUE))) +
  geom_line(aes(x = lat, y = sink.m)) +

  geom_line(data = df.sink.regrid.mean %>%
              mutate(Continent = case_when(lon <= -20 ~ "Amazon",
                                           lon <= 55 ~ "Africa",
                                           lon > 90 & lat > - 10 ~ "Asia",
                                           lon > 100 & lat <= -10 ~ "Australia")) %>%
              filter(!is.na(Continent)) %>%
              mutate(lat = round(lat)) %>%
              group_by(Continent,lat) %>%
              summarise(sink.m = mean(sink.m,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = lat, y = sink.m, color = Continent)) +

  coord_flip() +
  theme_bw()

# ggplot(data = df.sink.regrid.mean %>%
#          filter(abs(lat) < 23.25) %>%
#          mutate(lon = round(lon)) %>%
#          group_by(lon) %>%
#          summarise(sink.m = mean(sink.m,na.rm = TRUE))) +
#   geom_line(aes(x = lon, y = sink.m)) +
#   theme_bw()

ggplot() +

  geom_raster(data = df.AGB.regrid.mean %>%
                filter(!is.na(AGB)) %>%
                filter(Nmodels == 16),
              aes(x = lon, y = lat,
                  fill = AGB*10/carbon_content), na.rm = TRUE, alpha = 1) +

  geom_point(data = networks2plot,
             aes(x = Lon, y = Lat), shape = 1, alpha = 1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-80, 150),
           ylim = c(-1, 1)*20) +


  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "", fill = "AGB") +

  theme_bw()


ggplot() +

  geom_raster(data = df.npp.regrid.mean %>%
                filter(!is.na(npp.m)) %>%
                filter(Nmodels == 16),
              aes(x = lon, y = lat,
                  fill = 86400*365*npp.m), na.rm = TRUE, alpha = 1) +

  geom_point(data = networks2plot,
             aes(x = Lon, y = Lat), shape = 1, alpha = 1) +

  geom_sf(data = world,
          fill = NA) +

  coord_sf(xlim = c(-80, 150),
           ylim = c(-20, 12)) +

  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "", fill = "NPP") +

  theme_bw()


Biomass.Trendy.mean.sink %>%
  group_by(Continent, model) %>%
  summarise(med = median(AGB_init,na.rm = TRUE),
            m = mean(AGB_init,na.rm = TRUE),
            max = max(AGB_init,na.rm = TRUE)) %>%
  arrange(desc(max))


Biomass.Trendy.search.mean <- Biomass.Trendy  %>%
  filter(cVeg > 0) %>%
  ungroup() %>%
  group_by(year,model,Continent) %>%
  summarise(cVeg = mean(cVeg,na.rm = TRUE),
            .groups = "keep")

observed.Biomass <- readRDS("observed.Biomass.RDS")

ggplot(data = Biomass.Trendy.search.mean %>%
         filter(!is.na(Continent))) +
  geom_line(aes(x = year,y = cVeg,color = model)) +
  geom_line(data = observed.Biomass %>%
              filter(year >= 1983,year <= 2010),
            aes(x = year, y = AGB/10*carbon_content), color = "black") +
  facet_wrap(~ Continent,nrow = 1) +
  theme_bw()

observed.GPP <- bind_rows(list(readRDS("./data/GPP/monthly/all.df.GPP2.RDS") %>%
                                 mutate(model = "Madani"),
                               readRDS("./data/GPP/monthly/all.df.GPP.RDS") %>%
                                 mutate(model = "Zhang"),
                               readRDS("./data/GPP/df.all.GPP3.RDS") %>%
                                 mutate(model = "Fluxcom"),
                               readRDS("./data/GPP/monthly/all.df.GPP.MODIS.RDS") %>%
                                 mutate(model = "MODIS"))) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

####################################################################################
# Merge boths
# Method #1, per census (also possible per plot)
df.model <- df.model.long <- data.frame()
Ngridcells = 1

# # For testing purposes only
# networks <- networks %>%
#   group_by(Continent) %>%
#   slice_head(n = 10)

all.plots <- unique(networks$PlotCode)
observed.GPP.filt <- observed.GPP %>%
  filter(month == 1, year == 2010)

for (iplot in seq(1,length(all.plots))){

  print(iplot/length(all.plots))

  cplot <- as.character(all.plots[iplot])
  cdf <- networks %>%
    dplyr::filter(PlotCode %in% c(cplot,paste(cplot,"tf")))

  cContinent <- unique(cdf$Continent)
  clat <- unique(cdf$Lat)
  clon <- unique(cdf$Lon)

  if (is.na(clat)) {
    warning(paste("Missing site:",iplot))
    next()
  } else if (length(clat) > 1 | length(clon) > 1){
    stop("Error with coordinates")
  }

  if (cContinent == "Amazon"){
    cBiomass.Trendy.search <- Biomass.Trendy.search.Amazon
    cgpp.npp.trendy <- gpp.npp.trendy.Amazon
  } else if (cContinent == "Africa"){
    cBiomass.Trendy.search <- Biomass.Trendy.search.Africa
    cgpp.npp.trendy <- gpp.npp.trendy.Africa
  } else {
    cBiomass.Trendy.search <- Biomass.Trendy.search.Asia
    cgpp.npp.trendy <- gpp.npp.trendy.Asia
  }

  lat.limits <- c((clat + (Ngridcells + 1)*res.lat),
                  (clat - (Ngridcells + 1)*res.lat))

  lon.limits <- c((clon + (Ngridcells + 1)*res.lon),
                  (clon - (Ngridcells + 1)*res.lon))

  ccBiomass.Trendy.search <- cBiomass.Trendy.search %>%
    filter(lat <=  max(lat.limits) &
             lat >=  min(lat.limits) &
             lon <= max(lon.limits) &
             lon >= min(lon.limits))

  ccgpp.npp.trendy <- cgpp.npp.trendy  %>%
    filter(lat <=  max(lat.limits) &
             lat >=  min(lat.limits) &
             lon <= max(lon.limits) &
             lon >= min(lon.limits))

  model.lat.lons <- find.coord.Trendy(Trendy.grid = ccBiomass.Trendy.search,
                                      target = c(clon,clat),
                                      Ngridcells) %>% unique()
  GPP.lat.lons <- find.coord.raster(observed.GPP.filt,
                                    target = c(clon,clat),
                                    Ngridcells = 1) %>%
    unique()

  cccBiomass.Trendy.search <- ccBiomass.Trendy.search %>%
    filter(model.lat.lon %in% model.lat.lons)
  cccgpp.npp.trendy <- ccgpp.npp.trendy %>%
    filter(model.lat.lon %in% model.lat.lons)
  cobserved.GPP <- observed.GPP %>%
    filter(model.lat.lon %in% c(GPP.lat.lons))

  for (isubplot in seq(1,nrow(cdf))){

    ccdf <- cdf[isubplot,]
    cplot.size <- ccdf %>% pull(PlotArea)

    start.date <- ccdf %>% pull(int_ini) ; start.date.int <- round(start.date)
    end.date <- ccdf %>% pull(int_fin) ; end.date.int <- round(end.date)

    end.date.int <- ifelse(end.date.int == start.date.int,
                           end.date.int + 1,
                           end.date.int)

    delta_t_yr <- end.date - start.date
    delta_t_yr_close <- end.date.int - start.date.int

    # All years
    # cdf.flux <- process.Trendy(Biomass.Trendy = cccBiomass.Trendy.search,
    #                            NPP.trendy = cccnpp.trendy,
    #                            target = c(start.date.int,
    #                                       end.date.int),
    #                            params = c(woody.ratio.of.npp)) %>%
    #   mutate(Continent = cContinent,
    #          plot = cplot,
    #          int_ini = start.date,
    #          int_fin = end.date,
    #          delta_t_yr = delta_t_yr,
    #          actual_delta_t = end.date - start.date,
    #          plot.size = cplot.size,
    #          obsAGB_init = ccdf %>% pull(AGB_ini.ha)*carbon_content,
    #          obsSink = ccdf %>% pull(AGBnetchange.ha.yr)*carbon_content,
    #          obsGrowth = ccdf %>% pull(AGBGain_tot.ha.yr)*carbon_content,
    #          obsMort = ccdf %>% pull(AGBMor_tot.ha.yr)*carbon_content) %>%
    #   mutate(dist = sqrt((clat - lat)**2 + (clon - lon)**2))
    #
    # df.model.long <- bind_rows(df.model.long,
    #                            cdf.flux)

    # Final - initial, and mean NPP
    # if (end.date > 2000 & start.date < 2017){
    #
    #   obs.gpp <- process.GPP.sum(GPP.observed = cobserved.GPP %>%
    #                                     mutate(time = year + (month-1/2)/12),
    #                                   target = c(start.date,
    #                                              end.date)) %>%
    #     ungroup() %>%
    #     summarise(obs.GPP = mean(GPP)) %>% pull(obs.GPP)
    # } else {
    #   obs.gpp = NA
    # }

    obs.gpp <- cobserved.GPP %>%
      mutate(time = year + (month-1/2)/12) %>%
      filter(time >= start.date & time <= end.date) %>%
      group_by(model) %>%
      summarise(obs.GPP = mean(daily.GPP)*365*10/1000,
                .groups = "keep") %>%
      mutate(obs.GPP  = case_when( (start.date < 2000 | end.date > 2016) & model == "Zhang" ~ NA_real_,
                                   (start.date < 1982 | end.date > 2016) & model == "Madani" ~ NA_real_,
                                   (start.date < 1950 | end.date > 2017) & model == "Fluxcom" ~ NA_real_,
                                   (start.date < 2000 | end.date > 2022) & model == "MODIS" ~ NA_real_,
                                   TRUE ~ obs.GPP))


    cdf.flux.sum <- process.Trendy.sum2(Biomass.Trendy = cccBiomass.Trendy.search,
                                        GPP.NPP.trendy = cccgpp.npp.trendy,
                                        target = c(start.date.int,
                                                   end.date.int)) %>%
      mutate(Continent = cContinent,
             plot = cplot,
             int_ini = start.date,
             int_fin = end.date,
             delta_t = delta_t_yr_close,
             actual_delta_t = end.date - start.date,
             plot.size = cplot.size,
             obsAGB_init = ccdf %>% pull(AGB_ini.ha)*carbon_content,
             obsGPP = mean(obs.gpp$obs.GPP,na.rm = TRUE),
             obsSink = ccdf %>% pull(AGBnetchange.ha.yr)*carbon_content,
             obsNPP = ccdf %>% pull(AGBGain_tot.ha.yr)*carbon_content/default.woody.ratio.of.npp) %>%
      mutate(dist = sqrt((clat - lat)**2 + (clon - lon)**2))

    df.model <- bind_rows(list(df.model,
                               cdf.flux.sum))


    years <- floor(cdf.flux.sum %>% pull(year_init))[1] : ceiling(cdf.flux.sum %>% pull(year_end))[1]
    Nyears <- length(years)
    df2replicate <- cdf.flux.sum


    df.model.long <- bind_rows(df.model.long,
                               df2replicate[rep(seq_len(nrow(df2replicate)), Nyears), ] %>%
                                 ungroup() %>%
                                 mutate(year = sort(rep(years,nrow(cdf.flux.sum)))) %>%
                                 group_by(model,lat,lon) %>%
                                 mutate(cAGB =
                                          approx(c(year[1],max(year)),
                                                 c(cAGB_init[1],cAGB_end[1]), xout=years)[["y"]])
    )


  }
}


threshold.area = -Inf  # ha
init.year = -Inf       # year

df.model.filtered <- df.model %>%
  dplyr::filter(plot.size >= threshold.area,
                year_init >= init.year) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia")))

df.model.long.filtered <- df.model.long %>%
  dplyr::filter(plot.size >= threshold.area,
                year >= init.year) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia")))

df2plot <- df.model.filtered %>%
  dplyr::select(model,Continent,
                obsSink,obsGPP,obsNPP,
                sink.corrected,GPP.m,NPP.m) %>%
  rename(Obs_Sink = obsSink,
         Obs_GPP = obsGPP,
         Obs_NPP = obsNPP,
         Mod_Sink = sink.corrected,
         Mod_GPP = GPP.m,
         Mod_NPP = NPP.m) %>%
  pivot_longer(cols = -c(model,Continent),
               values_to = "value",
               names_to = "variable") %>%
  mutate(type.var = sub("\\_.*", "", variable),
         type.obs = sub(".*\\_", "", variable))

df2plot.sum <- df2plot %>%
  group_by(Continent,type.var,type.obs) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = df2plot.sum,
       aes(x = type.obs , y = value.m, fill = type.var)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  facet_wrap(~ Continent,nrow = 1) +
  labs(x = "",
       y = "Carbon flux (MgC/ha/yr)",
       fill = "") +
  theme_bw()


ggplot(data = df2plot.sum,
       aes(x = type.obs , y = value.m, fill = Continent)) +
  geom_bar(stat="identity",
           position=position_dodge()) +
  facet_wrap(~ type.var) +
  labs(x = "",
       y = "Carbon flux (MgC/ha/yr)",
       fill = "") +
  theme_bw()


sink.data.filtered <- networks %>% dplyr::filter(PlotArea >= threshold.area,
                                                 int_ini >= init.year) %>%
  mutate(mid_int_date = (int_ini + int_fin)/2)

df.model.time <- df.model.filtered %>%
  group_by(plot,model,year_init) %>%
  summarise(Continent = unique(Continent),
            year_end = unique(year_end),
            cAGB_init = weighted.mean(cAGB_init,1/dist),
            int_ini = unique(int_ini),
            int_fin = unique(int_fin),
            delta_t = unique(delta_t),
            actual_delta_t = unique(actual_delta_t),
            sink.corrected.m = weighted.mean(sink.corrected,1/dist),
            obscAGB_init = unique(obsAGB_init),
            .groups = "keep")

ggplot(data = df.model.time,
       aes(x = obscAGB_init, y = cAGB_init, color = as.factor(model))) +
  geom_point() +
  stat_smooth(method = "lm",
              se = FALSE) +
  facet_wrap(~ Continent) +
  theme_bw()


df.model.time.all <- bind_rows(list(df.model.time,
                                    df.model.time %>%
                                      group_by(Continent,plot,year_init) %>%
                                      summarise(cAGB_init = mean(cAGB_init),
                                                .groups = "keep") %>% mutate(model = "Model Ensemble Mean")))

ggplot() +
  geom_density(data = df.model.time,
               aes(x = cAGB_init, fill = model, color = model), linewidth = 0.5,
               alpha = 0.5) +

  geom_density(data = df.model.time %>%
                 dplyr::filter(model == model[1]),
               aes(x = obscAGB_init), color = "black",
               fill = NA,
               alpha = 0.5) +
  facet_wrap(~Continent,scales = "free_y") +

  theme_bw()

ggplot() +
  geom_density(data = df.model.time.all %>%
                 dplyr::filter(model == "Model Ensemble Mean"),
               aes(x = cAGB_init, fill = model), color = NA,
               alpha = 0.5) +

  geom_density(data = df.model.time %>%
                 dplyr::filter(model == model[1]),
               aes(x = obscAGB_init), color = "black",
               fill = NA,
               alpha = 0.5) +

  facet_wrap(~ Continent) +

  theme_bw()


ggplot(data = df.model.time,
       aes(x = year_init, y = int_ini)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ Continent, scales = "free") +
  theme_bw()

ggplot(data = df.model.time,
       aes(x = delta_t-actual_delta_t)) +
  geom_histogram() +
  theme_bw()


ggplot() +

  geom_point(data = sink.data.filtered %>% dplyr::filter(Continent == "Africa"),
             aes(x = mid_int_date,
                 y = AGBnetchange.ha.yr*carbon_content),
             size = 0.5,
             color = "darkgrey") +
  stat_smooth(data = sink.data.filtered %>% dplyr::filter(Continent == "Africa"),
              aes(x = mid_int_date,
                  y = AGBnetchange.ha.yr*carbon_content),
              se = FALSE, method = "lm", color = "black") +

  geom_point(data = df.model.time %>% dplyr::filter(Continent == "Africa"),
             aes(x = year_init + delta_t/2,sink.corrected.m, color = as.factor(model)),
             size = 0.5) +
  stat_smooth(data = df.model.time %>% dplyr::filter(Continent == "Africa"),
              aes(x = year_init + delta_t/2,sink.corrected.m, color = as.factor(model)),
              se = FALSE, method = "lm") +
  scale_y_continuous(limits = c(-2,2)) +
  facet_wrap(~ model) +
  theme_bw()


df.model.sum <- df.model.long.filtered %>%
  group_by(Continent,year_init,year,plot,model) %>%
  summarise(cAGB.m = weighted.mean(cAGB,1/dist,
                                   na.rm = TRUE),
            sink.corrected.m = weighted.mean(sink.corrected,1/dist,
                                             na.rm = TRUE),
            GPP.m = weighted.mean(GPP.m,1/dist,
                                  na.rm = TRUE),
            NPP.m = weighted.mean(NPP.m,1/dist,
                                  na.rm = TRUE),
            plot.size = unique(plot.size),
            AGBnetchange.ha.yr = unique(obsSink),
            AGBGain_tot.ha.yr = unique(obsNPP),
            obs_GPP = unique(obsGPP),
            .groups = "keep") %>%
  ungroup() %>%
  rename(modeled.sink = sink.corrected.m,
         modeled.GPP = GPP.m,
         modeled.NPP = NPP.m,
         observed.sink = AGBnetchange.ha.yr) %>%
  mutate(observed.NPP = AGBGain_tot.ha.yr/default.woody.ratio.of.npp,
         observed.GPP = obs_GPP) %>%
  ungroup() %>%
  complete(Continent = factor(c("Amazon","Africa","Asia","Australia")),
           year = min(df.model.filtered$year_init):max(df.model.filtered$year_end),
           model = unique(df.model.filtered$model),
           fill = list(value = NA))

Group.year <- 1
df.model.sum.time <- df.model.sum %>%
  mutate(decade = round(year/Group.year)) %>%
  group_by(Continent,year_init,plot,model) %>%
  mutate(weight = (max(year) - min(year))**(1/3) + (plot.size)**(1/4) - 1) %>%
  group_by(decade,Continent,model) %>%
  summarise(modeled.sink = weighted.mean(modeled.sink,weight,na.rm = TRUE),
            observed.sink = weighted.mean(observed.sink,weight,na.rm = TRUE),

            modeled.GPP = weighted.mean(modeled.GPP,weight,na.rm = TRUE),
            observed.GPP = weighted.mean(observed.GPP,weight,na.rm = TRUE),

            modeled.NPP = weighted.mean(modeled.NPP,weight,na.rm = TRUE),
            observed.NPP = weighted.mean(observed.NPP,weight,na.rm = TRUE),

            cAGB = weighted.mean(cAGB.m,weight,na.rm = TRUE),

            .groups = "keep") %>%
  mutate(year = decade*Group.year)

df.model.sum.time.all <- bind_rows(list(df.model.sum.time,
                                        df.model.sum.time %>% group_by(Continent, year,decade) %>%
                                          summarise(modeled.sink = mean(modeled.sink,na.rm = TRUE),
                                                    modeled.NPP = mean(modeled.NPP,na.rm = TRUE),
                                                    modeled.GPP = mean(modeled.GPP,na.rm = TRUE),
                                                    cAGB = mean(cAGB,na.rm = TRUE),
                                                    .groups = "keep") %>% mutate(model = "Model Ensemble Mean")))

# ggplot(data = df.model.sum.time) +
#   geom_line(aes(x = year, y = cAGB, color = model)) +
#   facet_wrap(~ Continent)

df.model.sum.time.all.long <- df.model.sum.time.all %>%
  dplyr::select(-c(cAGB)) %>%
  pivot_longer(cols = c(modeled.sink,observed.sink,modeled.GPP,modeled.NPP,observed.NPP),
               names_to = "variable",
               values_to = "value") %>%
  mutate(type = sub("\\..*", "", variable),
         variable = sub(".*\\.", "", variable)) %>%
  ungroup() %>%
  filter((type == "modeled") | (type == "observed" & model == model[1])) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia"))) %>%
  mutate(variable = factor(variable,
                           levels = c("sink","GPP","NPP"))) %>%
  filter(year >= 1985 & year <= 2012)


data.observed <- readRDS("./outputs/Afritron+Rainfor+Asia+Sullivan_raw_long.RDS") %>%
  filter(variable %in% c("sink","GPP","NPP"))

ggplot() +

  geom_line(data = df.model.sum.time.all.long %>%
              filter(type == "modeled" & model != "Model Ensemble Mean"),
            aes(x = year,
                y = value,
                color = as.factor(model))) +
  geom_line(data = df.model.sum.time.all.long %>%
              dplyr::filter(type == "modeled",
                            model == "Model Ensemble Mean"),
            aes(x = year,
                y = value),color = "black",linetype = 2) +
  geom_line(data = data.observed %>%
              filter(variable != "N"),
            aes(x = year,
                y = value,
                group = model), color = "black",linetype = 1) +

  geom_hline(yintercept = 0, linetype = 3) +
  facet_grid(variable ~ Continent) +
  theme_bw()


ggplot() +

  geom_line(data = df.model.sum.time.all %>%
              filter(model != "Model Ensemble Mean"),
            aes(x = year,
                y = cAGB/10,
                color = as.factor(model))) +

  geom_line(data = df.model.sum.time.all %>%
              dplyr::filter(model == "Model Ensemble Mean"),
            aes(x = year,
                y = cAGB/10),color = "black",linetype = 2) +
  geom_line(data = observed.Biomass,
            aes(x = year,
                y = AGB/10*carbon_content), color = "black",linetype = 1) +

  geom_hline(yintercept = 0, linetype = 3) +
  facet_grid( ~ Continent) +
  theme_bw()


df.model.sum.long <- df.model.sum %>%
  pivot_longer(cols = c(modeled.sink,modeled.GPP,modeled.NPP,
                        observed.sink,observed.GPP,observed.NPP),
               names_to = "variable",
               values_to = "value")

df.model.sum.long.all <- bind_rows(list(df.model.sum.long,
                                        df.model.sum.long %>%
                                          group_by(Continent,plot,plot.size,year,variable) %>%
                                          summarise(value = mean(value,na.rm = TRUE),
                                                    .groups = "keep") %>%mutate(model = "Model Ensemble")
)) %>%
  mutate(type.var = sub("\\..*", "", variable),
         type.obs = sub(".*\\.", "", variable))

df.model.sum.long.all.fltrd <- df.model.sum.long.all %>%
  dplyr::filter(value > -Inf,
                value < Inf)


ggplot() +
  geom_density(data = df.model.sum.long.all.fltrd %>% filter(type.var == "modeled",
                                                             type.obs %in% c("sink","GPP","NPP"),
                                                             model != "Model Ensemble"),
               aes(x = value, fill = model), color = NA,
               alpha = 0.5) +

  geom_density(data = df.model.sum.long.all.fltrd %>% filter(type.var == "modeled",
                                                             type.obs %in% c("sink","GPP","NPP"),
                                                             model == "Model Ensemble"),
               aes(x = value), fill = "red", color = NA,
               alpha = 0.5) +

  geom_density(data = df.model.sum.long.all.fltrd %>% filter(model == model[1],
                                                             type.obs %in% c("sink","GPP","NPP"),
                                                             type.var == "observed"),
               aes(x = value), fill = "black",color = NA,
               alpha = 0.5) +
  facet_wrap(Continent ~ type.obs, scales = "free", ncol = 3) +
  labs(x = "C flux (MC/ha/yr)") +
  theme_bw() +
  guides(fill = "none")

data.obs <- df.model.sum.long.all.fltrd %>%
  filter(model == model[1],
         type.obs %in% c("sink","GPP","NPP"),
         type.var == "observed") %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia"))) %>%
  mutate(type.obs = factor(type.obs,
                           levels = c("sink","GPP","NPP")))

data.obs.sum <- data.obs %>%
  filter(!is.na(value)) %>%
  group_by(Continent,type.obs) %>%
  summarise(N = n(),
            value.med = median(value,na.rm = TRUE),
            value.m = mean(value,na.rm = TRUE),
            value.low = quantile(value,0.25,na.rm = TRUE),
            value.high = quantile(value,0.75,na.rm = TRUE),

            value.verylow = quantile(value,0.025,na.rm = TRUE),
            value.veryhigh = quantile(value,0.975,na.rm = TRUE),

            .groups = "keep") %>%
  mutate(type.obs = factor(type.obs,
                           levels = c("sink","GPP","NPP")))

model2plot <- df.model.sum.long.all.fltrd %>%
  filter(type.var == "modeled",
         type.obs %in% c("sink","GPP","NPP")) %>%
  mutate(type.obs = factor(type.obs,
                           levels = c("sink","GPP","NPP")),
         Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia")),
         model = relevel(factor(model),"Model Ensemble"))

ggplot() +

  geom_rect(data = data.obs.sum,
            aes(xmin = -Inf,xmax = Inf,ymin = value.verylow, ymax = value.veryhigh),
            fill = "black", color = NA, alpha = 0.1) +


  geom_rect(data = data.obs.sum,
            aes(xmin = -Inf,xmax = Inf,ymin = value.low, ymax = value.high),
            fill = "black", color = NA, alpha = 0.25) +

  geom_segment(data = data.obs.sum,
               aes(x = -Inf, xend = Inf,y = value.med, yend = value.med),
               color = "black") +

  geom_boxplot(data = model2plot,
               aes(y = value, fill = model, x = 1 + as.numeric(model)), outlier.shape = NA,
               alpha = 1) +

  geom_vline(color = "black", linetype = 2, xintercept = 2.5) +

  scale_fill_manual(values = c("darkgrey",hex <- hue_pal()(16))) +
  scale_x_continuous(breaks = 2:18,
                     labels = levels(model2plot$model)) +
  # scale_y_continuous(limits = c(-5,8)) +
  facet_grid(type.obs ~ Continent) +
  labs(y = "C flux (MC/ha/yr)",
       x = "") +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


data2plot <-
  df.model.sum.long.all.fltrd %>%
  filter(model == "Model Ensemble") %>%
  mutate(type.obs = factor(type.obs,
                           levels = c("sink","GPP","NPP")),
         Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia"))) %>%
  mutate(type.var = case_when(type.var == "modeled" ~ "Model Ensemble",
                              type.var == "observed" ~ "Observation"))

data2plot2 <-
  df.model.sum.long.all.fltrd %>%
  filter(model != "Model Ensemble",
         type.var == "modeled") %>%
  mutate(type.obs = factor(type.obs,
                           levels = c("sink","GPP","NPP")),
         Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia")),)


ggplot() +
  ggridges::geom_density_ridges(data = data2plot,
                                aes(y = 1,
                                    x = value,
                                    fill = type.var),
                                alpha = 0.5, scale = 0.25) +
  ggridges::geom_density_ridges(data = data2plot2,
                                aes(y = 0,
                                    x = value,
                                    group = model),
                                fill = "grey", color = "black",
                                alpha = 0.5, scale = 0.25) +

  facet_grid(type.obs ~ Continent) +
  labs(x = "C flux (MC/ha/yr)",
       y = "") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  scale_y_continuous(breaks = c()) +
  theme_bw() +
  theme(legend.position = c(0.95,0.92))

