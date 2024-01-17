rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(rgdal)
library(YGB)
library(TrENDY.analyses)
library(stringr)
library(randomForest)
library(ggpointdensity)

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
  filter(Continent %in% c("Africa","Amazon"))

Tropics.sum <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRUJRA.climate.pantropical.year.RDS")

all.grids <- readRDS("./data/all.grids.RDS")
# all.grids <- data.frame()
# for (cmodel in unique(Biomass.Trendy$model)){
#
#   print(cmodel)
#   cdf <- Biomass.Trendy %>%
#     filter(model == cmodel)
#
#   CN <- colnames(Tropics.sum)
#   Var.names <- CN[!(CN %in% c("lat","lon","year"))]
#
#   test <- resample.df.all.col(bigdf = Tropics.sum %>%
#                                 mutate(model = "CRUJRA"),
#
#                               raster2resample = rasterFromXYZ((cdf %>%
#                                                                  ungroup() %>%
#                                                                  dplyr::select(lat,lon,cVeg))[,c("lon","lat","cVeg")]),
#                               var.names = Var.names,
#                               NULL)
#
#   # plot(rasterFromXYZ((test)[,c("lon","lat","tmax")]))
#
#   all.grids <- bind_rows(all.grids,
#                          test %>%
#                            mutate(model = cmodel))
# }
# saveRDS(all.grids,
#         "./data/all.grids.RDS")

# Merge

modelled.sink <- Biomass.Trendy %>%
  group_by(lat,lon,model,Continent) %>%
  mutate(sink = c(diff(cAGB), NA)) %>%
  ungroup() %>%
  mutate(lat = round(lat,digits = 3),
         lon = round(lon,digits = 3))

# CO2
dataC02 <- read.table("./data/global_co2_ann_1700_2022.txt",
                      stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)


sink.vs.climate <- modelled.sink %>%
  dplyr::select(-c(cVeg,cRoot)) %>%
  left_join(all.grids %>%
              mutate(lat = round(lat,digits = 3),
                     lon = round(lon,digits = 3)),
            by = c("model","year","lat","lon")) %>%
  na.omit() %>%
  left_join(dataC02,
            by = "year")

sink.vs.climate.sum <- sink.vs.climate %>%
  ungroup() %>%
  mutate(year.group = floor(year/5)) %>%
  group_by(Continent,model,lat,lon,year.group) %>%
  summarise(sink.m = mean(sink),
            agb.m = mean(cAGB),

            MAP.m = mean(MAP),
            MCWD.m = mean(MCWD),
            MAT.m = mean(MAT),
            tmax.m = mean(tmax),
            tmin.m = mean(tmin),
            dswrf.m = mean(dswrf),
            CO2.m = mean(CO2),

            pre.sd.sd = sd(pre.sd),
            tmp.sd.sd = sd(tmp.sd),
            tmax.sd.sd = sd(tmax.sd),
            tmin.sd.sd = sd(tmin.sd),
            dswrf.sd.sd = sd(dswrf.sd),

            MAP.sd = sd(MAP),
            MCWD.sd = sd(MCWD),
            MAT.sd = sd(MAT),
            tmax.sd = sd(tmax),
            tmin.sd = sd(tmin),
            dswrf.sd = sd(dswrf),

            pre.sd.m = mean(pre.sd),
            tmp.sd.m = mean(tmp.sd),
            tmax.sd.m = mean(tmax.sd),
            tmin.sd.m = mean(tmin.sd),
            dswrf.sd.m = mean(dswrf.sd),

            .groups = "keep")

# ggplot(data = sink.vs.climate) +
#   geom_point(aes(y = MAP,
#                  x = MCWD,
#                  color = cAGB)) +
#   theme_bw() +
#   scale_color_gradient(low = "white", high = "darkgreen",
#                        limits = c(0,20),
#                        oob = scales::squish) +
#   facet_wrap(~ model)
#
# ggplot(data = sink.vs.climate %>%
#          filter(cAGB > 12),
#        aes(y = sink,
#            x = MCWD, color = model, fill = model)) +
#   geom_point() +
#   theme_bw() +
#   facet_wrap(~ Continent) +
#   stat_smooth(method = "lm")

TF <- sink.vs.climate %>%
  group_by(model,lat,lon) %>%
  summarise(MAP.m = mean(MAP,na.rm = TRUE),
            MCWD.m = mean(MCWD,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(model) %>%
  filter(MCWD.m >= -250) %>%
  # summarise(N = n(),
  #           Dlat = mean(diff(sort(unique(lat)))))
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

all.corr <- df.importance <- df.predict <-
  data.frame()

for (cmodel in unique(sink.vs.climate$model)){

  print(cmodel)

  if (cmodel %in% all.corr$model) next()

  cdf <- sink.vs.climate.sum %>%
    filter(model == cmodel) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  ccdf <- cdf %>%
    ungroup() %>%
    # filter(cAGB >= 10) %>%
    filter(model.lat.lon %in% TF[["model.lat.lon"]]) %>%
    dplyr::select(-c(lat,lon,model,model.lat.lon,year.group))

  LM <- lm(data = ccdf,
           formula = sink.m ~ .)

  colnames(ccdf)

  CN <- colnames(ccdf)
  Var.names <- CN[!(CN %in% c("lat","lon"))]

  # rf=randomForest(x = ccdf %>%
  #                   dplyr::select(!!Var.names),
  #                 y = ccdf[["sink"]],
  #                 do.trace = TRUE,
  #                 ntree = 500, importance = TRUE)

  rf=randomForest(data = ccdf %>%
                    dplyr::select(!!Var.names),
                   sink.m ~ .^2,
                   do.trace = TRUE,
                   ntree = 500, importance = TRUE)

  i=importance(rf)
  i.ordered <- i[order(i[, "%IncMSE"], decreasing=TRUE),]
  # best3=rownames(i)[order(i[, "%IncMSE"], decreasing=TRUE)[1:3]]

  df.predict <- bind_rows(df.predict,
                          data.frame(obs = ccdf$sink.m,
                                     predict.lm = as.vector(predict(LM)),
                                     predict.rf = as.vector(predict(rf))) %>%
                            mutate(model = cmodel))

  df.importance <- bind_rows(df.importance,
                             as.data.frame(i.ordered) %>%
                               tibble::rownames_to_column() %>%
                               ungroup() %>%
                               mutate(model = cmodel) %>%
                               mutate(variable = "sink"))

  corr <- ccdf %>%
    summarise(r2.lm = summary(LM)[["adj.r.squared"]],
              r2.rf = mean(rf$rsq),
              .groups = "keep") %>%
    mutate(model = cmodel)

  all.corr <- bind_rows(all.corr,
                        corr)
}


ggplot(df.predict %>%
         filter(!(grepl("LP",model))),
       aes(x = predict.rf,
           y = obs)) +
  geom_pointdensity() +
  # geom_hex(bin = 100) +
  # geom_point(shape=16, size=0.25, show.legend = FALSE) +
  # stat_bkde2d(bandwidth=c(18036446, 0.05014539),
  #             grid_size=c(128, 128), geom="polygon", aes(fill=..level..)) +

  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  scale_fill_viridis_c() +
  # facet_wrap(~ model) +
  theme_bw()


ggplot(data = df.predict %>%
         filter(!(grepl("LP",model)))) +
  geom_point(aes(x = predict.rf,
                 y = obs,
                 color = model), size = 0.1, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  theme_bw()

ggplot(data = all.corr) +
  geom_density(aes(x = r2.rf), alpha = 0.5) +
  theme_bw()


df.importance.sum <- df.importance %>%
  group_by(rowname,variable) %>%
  summarise(IncMSE.m = mean(`%IncMSE`),
            IncMSE.sd = sd(`%IncMSE`),
            N = n(),
            .groups = "keep") %>%
  arrange((IncMSE.m)) %>%
  ungroup() %>%
  mutate(rowname = factor(rowname,
                          levels = unique(rowname)))

ggplot(data = df.importance.sum,
       aes(x = rowname,
           y = IncMSE.m,
           fill = variable,
           color = variable)) +
  geom_bar(stat = "identity",
           position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = 0.9*IncMSE.m, ymax = IncMSE.m + IncMSE.sd),
                position = position_dodge(0.9), width = 0.5) +
  # geom_point() +
  # facet_wrap(~ model) +
  coord_flip() +
  theme_bw()



# ggplot(data = sink.vs.climate.sum %>%
#          mutate(model.lat.lon = paste0(model,".",lat,".",lon))  %>%
#          filter(model.lat.lon %in% TF[["model.lat.lon"]]),
#        aes(y = sink.m,
#            x =  MCWD.m, color = model, fill = model)) +
#   # geom_point() +
#   theme_bw() +
#   facet_wrap(~ Continent, scales = "free") +
#   stat_smooth(method = "lm")
