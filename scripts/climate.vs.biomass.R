rm(list = ls())

library(YGB)
library(tidyr)

Model.vs.Data.Trendy <- readRDS("./outputs/Model.vs.Data.Trendy.RDS")
Model.vs.Data.Trendy.uni <- Model.vs.Data.Trendy %>%
  dplyr::select(lat,lon,year_init,year_end) %>%
  distinct()

climate <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/monthly.climate.pantropical.RDS") %>%
  mutate(model = "CRUJRA") %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

climate4search <- climate %>%
  dplyr::select(lon,lat) %>%
  distinct() %>%
  mutate(model = "CRUJRA") %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

df.loop <- data.frame()
for (irow in seq(1,nrow(Model.vs.Data.Trendy.uni))){
  print(irow/nrow(Model.vs.Data.Trendy.uni))
  clon <- Model.vs.Data.Trendy.uni$lon[irow]; clat <- Model.vs.Data.Trendy.uni$lat[irow]
  cyearinit <- Model.vs.Data.Trendy.uni$year_init[irow]; cyearend <- Model.vs.Data.Trendy.uni$year_end[irow]

  target <- find.coord.Trendy(Trendy.grid = climate4search,
                              target = c(clon,clat),
                              Ngridcells = 1) %>% unique()

  cdata <- climate %>%
    filter(model.lat.lon %in% target) %>%
    group_by(year,model.lat.lon) %>%
    mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
           E = 3.33,
           Etot = E*Ndays) %>%
    ungroup() %>%
    mutate(diff = pre*4*365/10 - Etot)


  MAT <- unlist(cdata %>%
    group_by(model.lat.lon,year) %>%
    summarise(MAT = mean(tmp),
              .groups = "keep") %>%
    ungroup() %>%
    summarise(MAT.m = mean(MAT),
              .groups = "keep"))

  MAP <- unlist(cdata %>%
    group_by(model.lat.lon,year) %>%
    summarise(MAP = sum(pre)*4*365/10,
              .groups = "keep") %>%
    ungroup() %>%
    summarise(MAP.m = mean(MAP),
              .groups = "keep"))

  MCWD <- unlist(cdata %>%
    group_by(model.lat.lon,year) %>%
    summarise(MCWD = calc.MCWD(diff),
              .groups = "keep") %>%
    ungroup() %>%
    summarise(MCWD.m = mean(MCWD),
              .groups = "keep"))

  df.loop <- bind_rows(df.loop,
                        data.frame(lon = clon, lat = clat,
                                   year_init = cyearinit, year_end = cyearend,
                                   MAP,MCWD,MAT))

}

saveRDS(df.loop,
        "./outputs/df.loop.RDS")

Model.vs.Data.Trendy.vs.climate <- Model.vs.Data.Trendy %>%
  left_join(df.loop,
            by = c("lon","lat","year_init","year_end"))


Model.vs.Data.Trendy.vs.climate.selected <-
  Model.vs.Data.Trendy.vs.climate %>%
  dplyr::select(-c(lon,lat,year_init,year_end,cAGB_end,delta_t,delta_AGB,woody.ratio.of.npp,
                   plot,int_ini,int_fin,actual_delta_t,plot.size,dist))

Model.vs.Data.Trendy.vs.climate.selected.long <-
  Model.vs.Data.Trendy.vs.climate.selected %>%
  pivot_longer(cols = -c(MAP,MCWD,MAT,Continent,model),
               names_to = "variable") %>%
  mutate(type = case_when(variable %in% c("obsSink","obsGrowth","obsMort") ~ "Observed",
                          TRUE ~ "Modeled")) %>%
  pivot_longer(cols = c(MAP,MCWD,MAT),
               names_to = "variable_climate",
               values_to = "value_climate") %>%
  mutate(variable = case_when(variable %in% c("cAGB_init","obsAGB_init") ~ "AGB_init",
                              variable %in% c("sink.corrected","obsSink") ~ "sink",
                              variable %in% c("Growth","obsGrowth") ~ "Growth",
                              variable %in% c("Mort","obsMort") ~ "Mort",
                              TRUE ~ NA_character_)) %>%
  filter(!is.na(variable))

ggplot(data = Model.vs.Data.Trendy.vs.climate.selected.long %>%
         filter(),
       aes(x = value_climate, y = value, shape = type,
           color = Continent, group = interaction(Continent,type))) +
  geom_point(size = 0.1) +
  stat_smooth(method = "lm",se = FALSE) +
  scale_shape_manual(values = c(0,1)) +
  theme_bw() +
  facet_wrap(variable ~ variable_climate,
             ncol = 3,
             scales = "free")
