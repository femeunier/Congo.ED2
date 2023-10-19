rm(list = ls())

library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
library(data.table)
library(scales)

carbon_content 	<- 0.456			# Martin et al. 2018 Nature Geoscience
woody.ratio.of.npp <- 0.3     # Malhi et al. 2011 (From Koch Suppl. data Table S3), varies between 0.3 and 0.5

#####################################################################################################
# Data

afritron <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Afritron/Afritron_plots.csv",
                     header = TRUE,
                     skip = 1)

afritron2plot <- afritron %>%
  group_by(Cluster) %>%
  slice_head(n = 1)

sink.data <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Afritron/data_by_censusinterval_africa.csv")

sink.data.m <- sink.data %>%
  group_by(PlotCode) %>%
  summarise(sink.obs = mean(AGBnetchange.ha.yr)*carbon_content) %>%
  rename(plot = PlotCode)

data.interval <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Afritron/2_data_by_censusinterval.csv")

#####################################################################################################
# Trendy Models

Biomass.Trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.centralAfrica.S3.cAGB.rspld_v11.RDS") %>%
  dplyr::filter(!is.na(cAGB))

Biomass.Trendy.search <- Biomass.Trendy %>% dplyr::filter(lat >= (min(afritron$Latitude..dec.,na.rm = TRUE) - 1),
                                                          lat <= (max(afritron$Latitude..dec.,na.rm = TRUE) + 1),
                                                          lon >= (min(afritron$Longitude..dec.,na.rm = TRUE) - 1),
                                                          lon <= (max(afritron$Longitude..dec.,na.rm = TRUE) + 1)) %>%
  mutate(year = floor(year)) %>%
  group_by(lon,lat,model,year,scenario) %>%
  summarise(cAGB = mean(cAGB,na.rm = TRUE),
            cVeg = mean(cVeg,na.rm = TRUE),
            cRoot = mean(cRoot,na.rm = TRUE),
            .groups = "keep")

Biomass.Trendy.m <- Biomass.Trendy %>%
  mutate(year.int = floor(year)) %>%
  group_by(lon,lat,model,year.int,scenario) %>%
  summarise(cAGB = mean(cAGB,na.rm = TRUE),
            cVeg = mean(cVeg,na.rm = TRUE),
            cRoot = mean(cRoot,na.rm = TRUE),
            .groups = "keep")


Biomass.Trendy.mean.sink <- Biomass.Trendy.m %>%
  group_by(lon,lat,model,scenario) %>%
  summarise(AGB_init = cAGB[year.int == min(year.int)],
            AGB_end = cAGB[year.int == max(year.int)],
            year_init = min(year.int),
            year_end = max(year.int),
            .groups = "keep") %>%
  mutate(delta_AGB = AGB_end - AGB_init,
         delta_t = year_end - year_init) %>%
  mutate(sink = 10*delta_AGB/delta_t) %>%
  mutate(sink.bnd = case_when(sink > 0.5 ~ 0.5,
                              sink < -0.5 ~ -0.5,
                              TRUE ~ sink))


npp.trendy <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.npp.centralAfrica.v11.RDS") %>%
  rename(npp = value) %>%
  dplyr::select(-c(scenario,variable)) %>%
  group_by(lon,lat,year,model) %>%
  summarise(npp = mean(npp,na.rm = TRUE),
            .groups = "keep") %>%
  dplyr::filter(!is.na(npp))


npp.trendy.mean <- npp.trendy %>%
  group_by(lon,lat,model) %>%
  summarise(npp.m = mean(npp),
            .groups = "keep")

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = npp.trendy.mean,
              aes(x = lon, y = lat,
                  fill = npp.m*86400*365*10), na.rm = TRUE, alpha = 1) +

  geom_point(data = afritron2plot,
             aes(x = Longitude..dec., y = Latitude..dec.)) +

  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-15, 10)) +
  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()

# Biomass.Trendy %>%
#   group_by(lon,lat,model,scenario) %>%
#   summarise(N = length(cAGB)) %>%
#   filter(N != 52) %>% pull(model) %>% unique()

# Biomass.Trendy.mean.sink.all <- bind_rows(Biomass.Trendy.mean.sink,
#                                           Biomass.Trendy.mean.sink %>% group_by(lat,lon) %>%
#                                             summarise(sink = mean(sink,na.rm = TRUE),
#                                                       .groups = "keep") %>% mutate(model = "Model ensemble"))  #


ggplot() +

  geom_raster(data = Biomass.Trendy.mean.sink,
            aes(x = lon, y = lat,
                fill = sink), na.rm = TRUE, alpha = 1) +

  geom_point(data = afritron2plot,
             aes(x = Longitude..dec., y = Latitude..dec.)) +

  # geom_text_repel(data = afritron2plot,
  #                 aes(x = Longitude..dec., y = Latitude..dec.,
  #                     label = Cluster),max.overlaps = 100) +

  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-15, 10)) +
  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "transparent",
                       limits = c(-0.5,0.5),
                       oob = scales::squish) +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()


Biomass.Trendy.search.mean <- Biomass.Trendy.search %>%
  group_by(year,model) %>%
  summarise(cVeg = mean(cVeg,na.rm = TRUE),
            .groups = "keep")

ggplot(data = Biomass.Trendy.search.mean) +
  geom_line(aes(x = year,y = cVeg,color = model)) +
  theme_bw()



####################################################################################
# Merge boths
# Method #1, per census (also possible per plot)
df.model <- data.frame()
Ngridcells = 1

for (icensus in seq(1,nrow(sink.data))){

  print(icensus/nrow(sink.data))

  cplot <- as.character(sink.data$PlotCode[icensus])
  cCensusInter <- sink.data$Census.Int[icensus]

  cafritronplot <- afritron %>% dplyr::filter(Plot.Code.. %in% c(cplot,paste(cplot,"tf"))) # is tierra firme the proper site?
  cdf.agb <- data.interval %>% dplyr::filter(PlotCode %in% c(cplot,paste(cplot,"tf")),
                                             Census.Int == cCensusInter)

  if (nrow(cdf.agb) > 1) cdf.agb <- cdf.agb %>% filter(Plot.View.Name %in% c("TERRA FIRME","TERRA FIRME new")) # check with Wannes

  clat <- cafritronplot %>% pull(Latitude..dec.)
  clon <- cafritronplot %>% pull(Longitude..dec.)
  cplot.size <- cafritronplot %>% pull(Plot.size..ha.)

  if (is.na(clat)) next()

  start.date <- sink.data$int_ini[icensus] ; start.date.int <- round(start.date)
  end.date <- sink.data$int_fin[icensus] ; end.date.int <- round(end.date)

  delta_t_yr <- end.date - start.date
  delta_t_yr_close <- end.date.int - start.date.int


  # AGB change
  cdf <- Biomass.Trendy.search %>%
    group_by(model) %>%
    mutate(dist = sqrt((clat - lat)**2 + (clon - lon)**2),
           dist.t.init = (year - start.date.int)**2,
           dist.t.end = (year - end.date.int)**2) %>%
    dplyr::filter(dist %in% sort(unique(dist))[1:Ngridcells] &
                  ((dist.t.init == min(dist.t.init) | (dist.t.end == min(dist.t.end))))) %>%
    mutate(timing = case_when(year == start.date.int ~ "init",
                              year == end.date.int ~ "end"))


  # NPP
  cdf.npp <- npp.trendy %>%
    group_by(model) %>%
    mutate(dist = sqrt((clat - lat)**2 + (clon - lon)**2),
           dist.t.init = (year - start.date.int)**2,
           dist.t.end = (year - end.date.int)**2) %>%
    dplyr::filter(dist %in% sort(unique(dist))[1:Ngridcells] &
                    year %in% c((year[dist.t.init == min(dist.t.init)]):(year[dist.t.end == min(dist.t.end)]))) %>%
    mutate(timing = case_when(year == start.date.int ~ "init",
                              year == end.date.int ~ "end",
                              TRUE ~ "inter")) %>%
    arrange(model,timing) %>%
    mutate(npp = 86400*365*npp*10) # kgC/m²/s >> MgC/ha/yr


  cdf.npp.sum <- cdf.npp %>%
    dplyr::select(lon,lat,model,npp) %>%
    group_by(lon,lat,model) %>%
    summarise(Growth = mean(npp)*woody.ratio.of.npp,    #nppwood
              .groups = "keep")


  cdf.sum <- cdf %>% dplyr::select(lon,lat,dist,timing,model,year,cAGB) %>%
    mutate(cAGB = 10*cAGB) %>%  # kgC/m²/yr >> MgC/ha/yr
    pivot_wider(names_from = timing,
                values_from = c(cAGB,year)) %>%
    mutate(delta_AGB = cAGB_end - cAGB_init,
           delta_t = year_end - year_init,
           correction.factor = (delta_t_yr/delta_t_yr_close)) %>%
    mutate(sink.corrected = delta_AGB/delta_t) %>%
    mutate(plot = cplot,
           int_ini = start.date,
           int_fin = end.date,
           actual_delta_t = end.date - start.date,
           plot.size = cplot.size,
           obsAGB_init = cdf.agb[["AGB_ini.ha"]]*carbon_content,
           obsGrowth = sink.data$AGBGain_tot.ha.yr[icensus]*carbon_content,
           obsMort = cdf.agb[["AGBMor_tot.ha.yr"]]*carbon_content)

  cdf.sum.npp <- cdf.sum %>%
    left_join(cdf.npp.sum,
              by = c("model")) %>%
    mutate(Mort = Growth - sink.corrected)

  df.model <- bind_rows(list(df.model,
                             cdf.sum.npp))

}

threshold.area = 0.   # ha
init.year = 0

df.model.filtered <- df.model %>% dplyr::filter(plot.size >= threshold.area,
                                                year_init >= init.year)
sink.data.filtered <- sink.data %>% dplyr::filter(PlotArea >= threshold.area,
                                                  int_ini >= init.year)

df.model.time <- df.model.filtered %>%
  group_by(plot,model,year_init) %>%
  summarise(year_end = unique(year_end),
            correction.factor = unique(correction.factor),
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
  theme_bw()

# ggplot(data = df.model.time,
#        aes(x = year_init, y = cAGB_init, color = as.factor(model))) +
#   geom_point(data = df.model.time,
#              aes(x = year_init, y = obscAGB_init), color = "black") +
#   geom_point() +
#   stat_smooth(method = "lm",
#               se = FALSE) +
#
#   stat_smooth(data = df.model.time,
#               aes(x = year_init, y = obscAGB_init),
#               method = "lm",
#               se = FALSE, color = "black") +
#
#   theme_bw()

df.model.time.all <- bind_rows(list(df.model.time,
                                    df.model.time %>%
                                      group_by(plot,year_init) %>%
                                      summarise(cAGB_init = mean(cAGB_init),
                                                .groups = "keep") %>% mutate(model = "Model Ensemble Mean")))

ggplot() +
  geom_density(data = df.model.time,
               aes(x = cAGB_init, fill = model, color = model), linewidth = 0.5,
               alpha = 0.5) +

  geom_density(data = df.model.time %>% dplyr::filter(model == model[1]),
               aes(x = obscAGB_init), color = "black",
               fill = NA,
               alpha = 0.5) +

  theme_bw()


ggplot() +
  geom_density(data = df.model.time.all %>% dplyr::filter(model == "Model Ensemble Mean"),
               aes(x = cAGB_init, fill = model), color = NA,
               alpha = 0.5) +

  geom_density(data = df.model.time %>% dplyr::filter(model == model[1]),
               aes(x = obscAGB_init), color = "black",
               fill = NA,
               alpha = 0.5) +

  theme_bw()


ggplot(data = df.model.time,
       aes(x = year_init, y = int_ini)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

ggplot(data = df.model.time,
       aes(x = delta_t-actual_delta_t)) +
  geom_histogram() +
  # stat_smooth(method = "lm") +
  theme_bw()

# df.model.time %>%
#   mutate(delta = abs(actual_delta_t - delta_t)) %>%
#   arrange(desc(delta))


# df.model.time.sorted <- df.model.time %>%
#   mutate(year = year_init + delta_t) %>%
#   arrange(year)

# cols <- c("sink.corrected.m")
# df.model.time.sorted.smooth <- data.frame()
#
# for (cmodel in df.model.time.sorted$model){
#   df.model.time.sorted.smooth <- bind_rows(list(df.model.time.sorted.smooth,
#                                                 setDT(df.model.time.sorted %>% filter(model ==cmodel))[SJ(year = (min(year)):(max(year)))[, c("start", "end") := .(year - 5, year + 5)],
#                                                              on = .(year >= start, year < end),
#                                                              c(.(year = i.year), lapply(.SD, mean)), .SDcols = cols, by = .EACHI][, -(1:2)] %>%
#                                                   mutate(model = cmodel)
#   ))
# }
#
# cols <- c("AGBnetchange.ha.yr")
# sink.data.filtered.smooth <-  setDT(sink.data.filtered %>% mutate(year = (int_ini + int_fin)/2))[SJ(year = (min(year)):(max(year)))[, c("start", "end") := .(year - 5, year + 5)],
#                                                         on = .(year >= start, year < end),
#                                                         c(.(year = i.year), lapply(.SD, mean)), .SDcols = cols, by = .EACHI][, -(1:2)]


ggplot() +
  geom_point(data = sink.data.filtered,
             aes(x = mid_int_date,
                 y = AGBnetchange.ha.yr*carbon_content),
             size=0.5, color = "darkgrey") +
  # geom_line(data = sink.data.filtered.smooth,
  #           aes(x = year,
  #               y = AGBnetchange.ha.yr*carbon_content),
  #           linewidth = 2) +
  stat_smooth(data = sink.data.filtered,
              aes(x = mid_int_date,
                  y = AGBnetchange.ha.yr*carbon_content),
              se = FALSE, method = "lm", color = "black") +
  geom_point(data = df.model.time,
             aes(x = year_init + delta_t/2,sink.corrected.m, color = as.factor(model)),
             size=0.5) +
  # geom_line(data = df.model.time.sorted.smooth,
  #           aes(x = year,
  #               y = sink.corrected.m,
  #               color = as.factor(model)),
  #           linewidth = 2) +
  stat_smooth(data = df.model.time,
              aes(x = year_init + delta_t/2,sink.corrected.m, color = as.factor(model)),
              se = FALSE, method = "lm") +
  scale_y_continuous(limits = c(-2,2)) +
  facet_wrap(~ model) +
  theme_bw()


df.model.sum <- df.model.filtered %>%
  group_by(int_ini,plot,model) %>%
  summarise(sink.corrected.m = weighted.mean(sink.corrected,1/dist,
                                             na.rm = TRUE),
            Growth.m = weighted.mean(Growth,1/dist,
                                     na.rm = TRUE),
            Mort.m = weighted.mean(Mort,1/dist,
                                     na.rm = TRUE),
            plot.size = unique(plot.size),
            .groups = "keep") %>%
  ungroup() %>%
  left_join(sink.data.filtered %>% dplyr::select(int_ini,int_fin,PlotCode,AGBnetchange.ha.yr,AGBGain_tot.ha.yr,AGBMor_tot.ha.yr) %>%
              mutate(year = (int_ini + int_fin)/2) %>%
              rename(plot = PlotCode) %>%
              mutate(AGBnetchange.ha.yr = AGBnetchange.ha.yr*carbon_content,
                     AGBGain_tot.ha.yr = AGBGain_tot.ha.yr*carbon_content,
                     AGBMor_tot.ha.yr = AGBMor_tot.ha.yr*carbon_content),  # !!! Check with Hannes
            by = c("plot","int_ini")) %>%
  rename(modeled.sink = sink.corrected.m,
         modeled.mort = Mort.m,
         observed.sink = AGBnetchange.ha.yr,
         observed.growth = AGBGain_tot.ha.yr,
         modeled.growth = Growth.m,
         observed.mort = AGBMor_tot.ha.yr)

Group.year <- 10
df.model.sum.time <- df.model.sum %>%
  mutate(decade = round(year/Group.year)) %>%
  group_by(decade,model) %>%
  summarise(modeled.sink = weighted.mean(modeled.sink,plot.size),
            observed.sink = weighted.mean(observed.sink,plot.size),

            modeled.growth = weighted.mean(modeled.growth,plot.size),
            observed.growth = weighted.mean(observed.growth,plot.size),

            modeled.mort = weighted.mean(modeled.mort,plot.size),
            observed.mort = weighted.mean(observed.mort,plot.size),

            .groups = "keep") %>%
  mutate(year = decade*Group.year)

df.model.sum.time.all <- bind_rows(list(df.model.sum.time,
                                        df.model.sum.time %>% group_by(year,decade) %>%
                                          summarise(modeled.sink = mean(modeled.sink,na.rm = TRUE),
                                                    modeled.growth = mean(modeled.growth,na.rm = TRUE),
                                                    modeled.mort = mean(modeled.mort,na.rm = TRUE),
                                                    .groups = "keep") %>% mutate(model = "Model Ensemble Mean")))

ggplot() +
  geom_line(data = df.model.sum.time,
            aes(x = year,
                y = observed.sink)) +
  geom_line(data = df.model.sum.time,
            aes(x = year,
                y = modeled.sink,
                color = as.factor(model))) +
  geom_line(data = df.model.sum.time.all %>% dplyr::filter(model == "Model Ensemble Mean"),
            aes(x = year,
                y = modeled.sink),color = "black",linetype = 2) +
  geom_hline(yintercept = 0, linetype = 3) +
  theme_bw()


ggplot() +
  geom_line(data = df.model.sum.time,
            aes(x = year,
                y = observed.growth)) +
  geom_line(data = df.model.sum.time,
            aes(x = year,
                y = modeled.growth,
                color = as.factor(model))) +
  geom_line(data = df.model.sum.time.all %>% dplyr::filter(model == "Model Ensemble Mean"),
            aes(x = year,
                y = modeled.growth),color = "black",linetype = 2) +
  geom_hline(yintercept = 0, linetype = 3) +
  theme_bw()

ggplot() +
  geom_line(data = df.model.sum.time,
            aes(x = year,
                y = observed.mort)) +
  geom_line(data = df.model.sum.time,
            aes(x = year,
                y = modeled.mort,
                color = as.factor(model))) +
  geom_line(data = df.model.sum.time.all %>% dplyr::filter(model == "Model Ensemble Mean"),
            aes(x = year,
                y = modeled.mort),color = "black",linetype = 2) +
  geom_hline(yintercept = 0, linetype = 3) +
  theme_bw()


df.model.sum.long <- df.model.sum %>%
  pivot_longer(cols = c(modeled.sink,modeled.growth,modeled.mort,
                        observed.sink,observed.growth,observed.mort),
               names_to = "variable",
               values_to = "value")

df.model.sum.long.all <- bind_rows(list(df.model.sum.long,
                                        df.model.sum.long %>%
                                          group_by(int_ini,plot,plot.size,int_fin,year,variable) %>%
                                          summarise(value = mean(value,na.rm = TRUE),
                                                    .groups = "keep") %>%mutate(model = "Model Ensemble")
                                          )) %>%
  mutate(type.var = sub("\\..*", "", variable),
         type.obs = sub(".*\\.", "", variable))

df.model.sum.long.all.fltrd <- df.model.sum.long.all %>%
  dplyr::filter(value > -5,
                value < 10)


ggplot() +
  geom_density(data = df.model.sum.long.all.fltrd %>% filter(type.var == "modeled",
                                                             type.obs %in% c("sink","growth"),
                                                             model != "Model Ensemble"),
               aes(x = value, fill = model), color = NA,
               alpha = 0.5) +

  geom_density(data = df.model.sum.long.all.fltrd %>% filter(type.var == "modeled",
                                                             type.obs %in% c("sink","growth"),
                                                             model == "Model Ensemble"),
               aes(x = value), fill = "red", color = NA,
               alpha = 0.5) +

  geom_density(data = df.model.sum.long.all.fltrd %>% filter(model == model[1],
                                                             type.obs %in% c("sink","growth"),
                                                             type.var == "observed"),
               aes(x = value), fill = "black",color = NA,
               alpha = 0.5) +
  facet_wrap(~ type.obs, scales = "free") +
  labs(x = "C flux (MC/ha/yr)") +
  theme_bw() +
  guides(fill = "none")


df.model.sum.long.all.fltrd %>% filter((type.var == "modeled" &
                                       model == "Model Ensemble") | (type.var == "observed" & model == model[1])) %>%
  group_by(type.var,type.obs) %>%
  summarise(m = mean(value),
            med = median(value)) %>%
  arrange(type.obs)


ggplot() +
  geom_density(data = df.model.time,
               aes(x = cAGB_init, fill = model), color = NA,
               alpha = 0.5) +

  geom_density(data = df.model.time %>% dplyr::filter(model == model[1]),
               aes(x = obscAGB_init), color = "black",
               fill = NA,
               alpha = 0.5) +

  theme_bw()

ggplot(data = df.model.sum,
       aes(x = modeled.sink, y = observed.sink)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0, color = "red") +
  stat_smooth(method = "lm",
              color = "darkgrey",
              se = FALSE) +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = df.model.sum,
       aes(x = modeled.growth, y = observed.growth)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0, color = "red") +
  stat_smooth(method = "lm",
              color = "darkgrey",
              se = FALSE) +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = df.model.sum,
       aes(x = modeled.mort, y = observed.mort)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0, color = "red") +
  stat_smooth(method = "lm",
              color = "darkgrey",
              se = FALSE) +
  facet_wrap(~ model) +
  theme_bw()

df.all.model.sum <- df.model.sum %>%
  group_by(plot) %>%
  summarise(modeled.growth.m = mean(modeled.growth,na.rm = TRUE),
            modeled.growth.sd = sd(modeled.growth,na.rm = TRUE),
            observed.growth = mean(observed.growth,na.rm = TRUE),

            modeled.sink.m = mean(modeled.sink,na.rm = TRUE),
            modeled.sink.sd = sd(modeled.sink,na.rm = TRUE),
            observed.sink = mean(observed.sink,na.rm = TRUE),

            .groups ="keep")


ggplot(data = df.all.model.sum,
       aes(x = modeled.growth.m, y = observed.growth)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0, color = "red") +
  stat_smooth(method = "lm",
              color = "darkgrey",
              se = FALSE) +
  theme_bw()


ggplot(data = df.all.model.sum,
       aes(x = modeled.sink.m, y = observed.sink)) +
  geom_point() +
  geom_abline(slope = 1,
              intercept = 0, color = "red") +
  stat_smooth(method = "lm",
              color = "darkgrey",
              se = FALSE) +
  theme_bw()

