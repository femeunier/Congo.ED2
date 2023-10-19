rm(list = ls())

# Compare drivers of Caccum

library(dplyr)
library(ggplot2)
library(raster)
library(geodata)
library(tidyr)
library(dismo)
library(tie)
library(randomForest)
library(stringr)
library(ggridges)

rerun = FALSE

#################################################################
# Sites
networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan.RDS")

networks2plot <- networks %>%
  group_by(Continent) %>%
  rename(lat = Lat,lon = Lon) %>%
  dplyr::select(PlotCode,lat,lon,int_ini,int_fin,Continent,AGBnetchange.ha.yr,AGB_ini.ha) %>%
  rename(sink = AGBnetchange.ha.yr,
         AGB_ini = AGB_ini.ha) %>%
  mutate(yr_ini = floor(int_ini),
         yr_fin = floor(int_fin)) %>%
  dplyr::select(-c(int_ini,int_fin))


##################################################################
# Climate

Climate <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/monthly.climate.pantropical.RDS")

##################################################################
# AGB

AGB <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.cAGB.v11.RDS") %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  mutate(cAGB = case_when(all(is.na(cRoot)) ~ cVeg - cRoot,
                          TRUE ~ cVeg)) %>%
  dplyr::select(-c(scenario,cVeg,cRoot))

df4dist <- AGB %>%
  group_by(model) %>%
  filter(year == min(year))

df4dist.climate <- Climate %>%
  filter(year == min(year),
         month == min(month))

models <- unique(df4dist[["model"]])

if (rerun){

  df4comp <- data.frame()
  Ngridcells = 1

  for (isite in seq(1,nrow(networks2plot))){

    print(isite/nrow(networks2plot))

    clat <- networks2plot$lat[isite] ; clon <- networks2plot$lon[isite] ; cContinent = networks2plot$Continent[isite]
    cyears <- seq(networks2plot$yr_ini[isite],networks2plot$yr_fin[isite],1)
    csink <- networks2plot$sink[isite] ; cAGB <- networks2plot$AGB_ini[isite]

    cdist.models <- df4dist %>%
      group_by(model) %>%
      mutate(dist = sqrt((lat - clat)**2 + (lon - clon)**2)) %>%
      arrange(dist) %>%
      slice_head(n = Ngridcells) %>%
      mutate(lat_lon = paste(lat,lon,sep ="_"),
             lat_lon_model = paste(lat_lon,model,sep = "_")) %>%
      dplyr::select(lon,lat,model,dist,
                    lat_lon,
                    lat_lon_model)


    all.df4dist.climate <- data.frame()

    for (cmodel in models){
      clat.model <- as.numeric(cdist.models %>% filter(model == cmodel) %>% pull(lat))
      clon.model <- as.numeric(cdist.models %>% filter(model == cmodel) %>% pull(lon))

      cdf4dist.climate <- df4dist.climate %>%
        ungroup() %>%
        mutate(dist = sqrt((lat - clat.model)**2 + (lon - clon.model)**2)) %>%
        arrange(dist) %>%
        slice_head(n = Ngridcells) %>%
        mutate(lat_lon = paste(lat,lon,sep ="_"),
               lat_lon_model = paste(lat_lon,cmodel,sep = "_")) %>%
        dplyr::select(lon,lat,dist,
                      lat_lon,
                      lat_lon_model)

      all.df4dist.climate <- bind_rows(list(all.df4dist.climate,
                                            cdf4dist.climate %>% mutate(model = cmodel)))

    }

    cClimate <- Climate %>%
      filter(year %in% cyears) %>%
      mutate(lat_lon = paste(lat,lon,sep = "_")) %>%
      dplyr::filter(lat_lon %in% c(unique(all.df4dist.climate[["lat_lon"]])))

    cClimate.Model <- all.df4dist.climate %>%
      full_join(cClimate,
                by = c("lat","lon","lat_lon"))


    cClimate.sum <-
      cClimate.Model %>%
      group_by(model,lat,lon,lat_lon,month) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "keep") %>%
      dplyr::select(-year) %>%
      group_by(model,lat,lon) %>%
      bow(tie(bio1, bio2, bio3, bio4,
              bio5, bio6, bio7, bio8,
              bio9, bio10, bio11, bio12,
              bio13, bio14, bio15, bio16,
              bio17, bio18, bio19,MCWD) := c(biovars(pre*4*365/12,
                                                     (tmin - 273.15)*10,
                                                     (tmax - 273.15)*10)[c(1:19)]))

    cClimate.sum.all <- cClimate.sum %>%
      group_by(model) %>%
      dplyr::select(-c(lat,lon)) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
                .groups = "keep")

    cAGB.df <- AGB %>%
      mutate(lat_lon = paste(lat,lon,sep="_")) %>%
      dplyr::filter(lat_lon %in% c(cdist.models[["lat_lon"]]),
                    year %in% cyears) %>%
      ungroup() %>%
      dplyr::select(-c(lat_lon))

    mod.sink <- cAGB.df %>%
      group_by(model,lat,lon) %>%
      summarise(mod.AGB_ini = cAGB[year == min(year)],
                delta_AGB = cAGB[year == max(year)] - cAGB[year == min(year)],
                delta_t = max(year) - min(year),
                .groups = "keep") %>%
      mutate(mod.sink = delta_AGB/delta_t)

    mod.sink <- mod.sink %>%
      group_by(model) %>%
      summarise(mod.sink = mean(mod.sink),
                mod.AGB_ini = mean(mod.AGB_ini))


    df4comp <- bind_rows(list(df4comp,
                              cClimate.sum.all %>%
                                left_join(mod.sink,
                                          by = "model") %>%
                                mutate(lat = clat,
                                         lon = clon,
                                         Continent = cContinent,
                                         year_ini = networks2plot$yr_ini[isite],
                                         year_fin = networks2plot$yr_fin[isite],
                                         sink = csink,
                                         AGB_ini = cAGB)
    ))

  }


  saveRDS(df4comp,"./outputs/df4comp.RDS")
} else {
  df4comp <- readRDS("./outputs/df4comp.RDS")
}

df4comp.filt <- df4comp %>%
  filter(mod.sink != 0,
         mod.AGB_ini != 0)

df.long <- df4comp.filt %>%
  dplyr::select(model,bio5,bio17,mod.AGB_ini,mod.sink) %>%
  pivot_longer(cols = c(bio5,bio17),
               names_to = "inp",
               values_to = "value.in") %>%
  pivot_longer(cols = c(mod.AGB_ini,mod.sink),
               names_to = "outp",
               values_to = "value.out") %>%
  mutate(inp = factor(inp,
                      levels = c("bio5","bio17")))


ggplot(data = df.long,
       aes(x = value.in, y = value.out)) +
  geom_point(color = "darkgrey") +
  stat_smooth(aes(color = model),
              se = FALSE,
              method = "lm") +
  facet_wrap(outp~ inp,scales = "free") +
  theme_bw()

# df4comp %>%
#   mutate(lon = round(lon),lat = round(lat)) %>%
#   group_by(lon,lat) %>%
#   summarise(N = n()) %>% arrange(desc(N))

# Add MEM

networks2plotformem <- networks2plot %>%
  group_by(lat,lon) %>%
  slice_head(n = 1)

xy2<-geoXY(networks2plotformem$lat,networks2plotformem$lon)
mems<- data.frame(lat = networks2plotformem$lat,
                  lon = networks2plotformem$lon,
                  as.data.frame(dbmem(xy2))) %>%
  dplyr::select(lat,lon,MEM1,MEM2,MEM3,MEM4,MEM5,MEM6,MEM7,MEM8)


df4comp.filt <- df4comp.filt %>%
  dplyr::left_join(mems,
            by = c("lat","lon"))


df4comp.long <- df4comp.filt %>%
  group_by(model) %>%
  mutate(id = 1:n()) %>%
  dplyr::select(model,id,Continent,lat,lon,
                AGB_ini,mod.AGB_ini,
                sink, mod.sink) %>%
  mutate(mod.AGB_ini = mod.AGB_ini*20,
         mod.sink = mod.sink*20) %>% ## units
  rename(mod_AGB = mod.AGB_ini,
         obs_AGB = AGB_ini,
         mod_sink = mod.sink,
         obs_sink = sink) %>%
  pivot_longer(cols = c(mod_AGB,obs_AGB,mod_sink,obs_sink),
               names_to = "var",
               values_to = "value") %>%
  mutate(type = sub(".*\\_", "", var),
         var = sub("\\_.*", "", var))


ggplot(data = df4comp.long %>% pivot_wider(names_from = c(var),
                                           values_from = value),
       aes(x = mod, y = obs, color = Continent, fill = Continent)) +
  geom_point() +
  stat_smooth(method = "lm") +
  stat_smooth(method = "lm",
              color = "darkgrey", fill = "darkgrey") +
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "darkgrey") +
  facet_wrap(~type, scales = "free") +
  theme_bw()

stop()

mf<-lm(log(AGWP)~Min.temp+bio5+wind+bio17+Cloud+Continent+Clay+CEC+MEM1+MEM2+MEM3+MEM4+MEM5+MEM6+MEM7+MEM8,
       data=dat,
       weights=WtAGWP,
       na.action=na.fail)



ggplot(data = df4comp.long,
       aes(x = value, fill = var)) +
  geom_density(color = NA, alpha = 0.5) +
  facet_wrap(Continent~type, scales = "free",nrow = 3) +
  theme_bw()

ggplot(data = df4comp.long %>% filter(type == "sink"),
       aes(x = Continent, y = value, fill = var)) +
  geom_boxplot(color = "black",
               alpha = 0.3,
               outlier.shape = NA) +
  # geom_violin(outlier.shape = NA) +
  # geom_boxplot(color = "black",
  #              alpha = 0.3,
  #              outlier.shape = NA) +
  # facet_wrap(~type, scales = "free") +
  labs(x = "") +
  scale_y_continuous(limits = quantile(df4comp.long %>% filter(type == "sink") %>% pull(value), c(0.1, 0.9),
                                       na.rm = TRUE)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()


ggplot(data = df4comp.long %>%
         filter(type == "sink")) +
  ggridges::geom_density_ridges_gradient(
    aes(y = var, x = value, fill = stat(x)),
    scale = 3,
    rel_min_height = 0.001, gradient_lwd = 0.1,
    alpha = 0.5) +
  facet_wrap(~Continent, scales = "free") +
  scale_x_continuous(expand = c(0, 0)) +
  # labs(x = "") +
  # scale_y_continuous(limits = quantile(df4comp.long %>% filter(type == "sink") %>% pull(value), c(0.1, 0.9),
  #                                      na.rm = TRUE)) +
  geom_vline(xintercept = 0, linetype = 2) +
  # scale_y_discrete(expand = expand_scale(mult = c(0.01, 0.25))) +
  scale_fill_gradient2(low = scales::muted("darkred"),
                       mid = "white",
                       high = scales::muted("darkgreen")) +
  theme_bw()


df4comp.long %>%
  group_by(Continent,type,var) %>%
  summarise(m = mean(value,na.rm = TRUE),
            med = median(value,na.rm = TRUE))

rf.model <- list()
df.fit <- df.partial <- df.RMSE <- data.frame()

for (cmodel in models){

  print(cmodel)

  cdf <- df4comp.filt %>% filter(model == cmodel)

  rf.model[[cmodel]] <-randomForest(log(mod.AGB_ini/mean(mod.AGB_ini)) ~
                                      bio1 + bio2 + bio3 + bio4 +
                                      bio5 + bio6 + bio7 + bio8 +
                                      bio9 + bio10 + bio11 + bio12 +
                                      bio13 + bio14 + bio15 + bio16 +
                                      bio17 + bio18 + bio19 +
                                      MEM1 + MEM2 +  MEM3 + MEM4 +  MEM5 + MEM6 +  MEM7 + MEM8,
                                    data = cdf,
                                    # weights=WtAGB,
                                    na.action=na.fail,
                                    importance = TRUE,
                                    proximity = TRUE)

  df.RMSE <- bind_rows(list(df.RMSE,
                            as.data.frame(randomForest::importance(rf.model[[cmodel]])) %>%
                              tibble::rownames_to_column(var = "biovar") %>%
                              mutate(model = cmodel)
                            ))

  df.fit <- bind_rows(list(df.fit,
                           data.frame(model = cmodel,
                                      predicted = as.vector(exp(rf.model[[cmodel]][["predicted"]])*mean(cdf$mod.AGB_ini)),
                                      observed = cdf$mod.AGB_ini
                                      )))

  for (ibiovar in seq(1,19)){

    cbiovar <- paste0("bio",ibiovar)
    a<-partialPlot(rf.model[[cmodel]],
                   cdf,
                   as.character(cbiovar),
                   plot = FALSE)

    df.partial <- bind_rows(list(df.partial,
                                 data.frame(model = cmodel,
                                            biovar = cbiovar,
                                            x = a[["x"]],
                                            y = a[["y"]])
    ))
  }
}


ggplot(data = df.fit,
       aes(x = predicted, y = observed, color = model)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10() +
  theme_bw()

ggplot(data = df.fit,
       aes(x = predicted, fill = model)) +
  geom_density(color = NA, alpha = 0.5) +
  geom_density(aes(x = observed),
               fill = "black",
               color = NA, alpha = 0.5) +
  # scale_x_log10() +
  theme_bw()

# Add data
# dat <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Sullivan/MultiCensusPlots.csv",header=T,as.is=T) %>%
#   mutate(AGB = AGB*0.456,
#          AGWP = AGWP*0.456) %>%
#   mutate(BRT = AGB/AGWP) %>%
#   mutate(bio5 = bio5+(Warm.rate2*(Midpoint-1985))) %>%
#   mutate(Min.temp = Min.temp+(Warm.rate2*(Midpoint-1985))) %>%
#   mutate(WtAGB = Area^(1/3)) %>%
#   mutate(WtAGWP = MonitorLength^(1/7)) %>%
#   mutate(WtBRT = Area^(1/9)+MonitorLength^(1/12)-1) %>%
#   mutate(CEC = log(CEC))

# # Random forest on continuous variables
df.obs <- df4comp.filt %>% filter(model == model[1])  # Obs. are repeated for each model
rf.obs <-randomForest(log(AGB_ini/mean(AGB_ini)) ~
                        bio5 + bio17 +
                        MEM1 + MEM2,
                     data = df.obs,
                     # weights=WtAGB,
                     na.action=na.fail,
                     importance = TRUE)

randomForest::importance(rf.obs)


df.RMSE <- bind_rows(list(df.RMSE,
                          as.data.frame(randomForest::importance(rf.obs)) %>%
                            tibble::rownames_to_column(var = "biovar") %>%
                            mutate(model = "Obs.")))

for (ibiovar in c(5,17)){

  cbiovar <- paste0("bio",ibiovar)
  a.obs <-partialPlot(rf.obs,
                      df.obs,
                      as.character(cbiovar),
                 plot = FALSE)

  df.partial <- bind_rows(list(df.partial,
                               data.frame(model = "Obs.",
                                          biovar = cbiovar,
                                          x = a.obs[["x"]],
                                          y = a.obs[["y"]])
  ))

}

# biovar.select <- as.data.frame(randomForest::importance(rf.obs)) %>%
#   tibble::rownames_to_column(var = "biovar") %>%
#   arrange(desc(`%IncMSE`)) %>%
#   filter(biovar %in% paste0("bio",1:19)) %>%
#   slice_head(n = 5) %>%
#   pull(biovar)

biovar.select <- c("bio5","bio17")

ggplot() +
  geom_line(data = df.partial %>%
              filter(biovar %in% biovar.select,
                     !(model %in% c("Obs."))),
            aes(x = x, y = y,
                color = model)) +
  geom_line(data = df.partial %>%
              filter(biovar %in% biovar.select,
                     model == "Obs."),
            aes(x = x, y = y),
            color = "black") +
  facet_wrap(~ biovar, scales = "free",
             nrow = 1, ) +
  theme_bw() +
  guides(color = "none")



df.RMSE.sum <- df.RMSE %>%
  group_by(model) %>%
  arrange(desc(`%IncMSE`)) %>%
  slice_head(n = 5) %>%
  filter(biovar %in% biovar.select) %>%
  select(c(biovar,model)) %>%
  mutate(present = TRUE) %>%

  pivot_wider(names_from = "biovar",
              values_from = present,
              values_fill = FALSE) %>%
  pivot_longer(names_to = "biovar",
               values_to = "present",
               cols = -c(model))

ggplot(data = df.RMSE.sum %>%
         filter(model != "Obs.")) +
  geom_tile(aes(y = model, x = biovar, fill = present)) +
  scale_fill_manual(values = c("white",
                               scales::muted("darkgreen"))) +
  theme_bw()


# rf_importance_oob_stat(rf)
# (VI_F=importance(rf))
# importance(rf)
# barplot(t(VI_F[,1]/sum(VI_F)))

# Neater version (Fig. S7)





