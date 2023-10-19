rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(rgdal)
library(YGB)
library(randomForest)
library(ggpointdensity)
library(TrENDY.analyses)

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

Tropics.sum <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/CRUJRA.climate.pantropical.RDS")

all.grids <- data.frame()
for (cmodel in unique(Biomass.Trendy$model)){

  print(cmodel)
  cdf <- Biomass.Trendy %>%
    filter(model == cmodel)

  CN <- colnames(Tropics.sum)
  Var.names <- CN[!(CN %in% c("lat","lon"))]
  test <- resample.df.all.col(bigdf = Tropics.sum %>%
                                mutate(model = "CRUJRA"),
                              raster2resample = rasterFromXYZ((cdf %>%
                                               ungroup() %>%
                                               filter(year == year[1]) %>%
                                               dplyr::select(lat,lon,cVeg))[,c("lon","lat","cVeg")]),
                              var.names = Var.names,
                              NULL)

  # plot(rasterFromXYZ((test)[,c("lon","lat","MAT")]))

  all.grids <- bind_rows(all.grids,
                         test %>%
                           mutate(model = cmodel))
}

# Merge

modelled.sink <- Biomass.Trendy %>%
  group_by(lat,lon,model,Continent) %>%
  mutate(sink = c(diff(cAGB),NA)) %>%
  group_by(lat,lon,model,Continent) %>%
  summarise(sink.m = mean(sink,
                          na.rm = TRUE),
            cAGB.m = mean(cAGB,
                          na.rm = TRUE),
            .groups = "keep") %>%
  mutate(lat = round(lat,digits = 3),
         lon = round(lon,digits = 3))

sink.vs.climate <- modelled.sink %>%
  left_join(all.grids %>%
              mutate(lat = round(lat,digits = 3),
                     lon = round(lon,digits = 3)),
            by = c("model","lat","lon")) %>%
  na.omit()

ggplot(data = sink.vs.climate %>%
         mutate(cAGB.m.bound = case_when(cAGB.m > 20 ~ 20,
                                         TRUE ~ cAGB.m))) +
  geom_point(aes(x = MAP.m,
                 y = cAGB.m.bound)) +
  theme_bw() +
  facet_wrap(~ model)

ggplot(data = sink.vs.climate) +
  geom_point(aes(y = MAP.m,
                 x = MCWD.m,
                 color = cAGB.m)) +
  theme_bw() +
  scale_color_gradient(low = "white", high = "darkgreen",
                       limits = c(0,20),
                       oob = scales::squish) +
  facet_wrap(~ model)


TF <- sink.vs.climate %>%
  group_by(model) %>%
  filter(MCWD.m >= -250) %>%
  # summarise(N = n(),
  #           Dlat = mean(diff(sort(unique(lat)))))
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

all.corr <- df.importance <- df.predict <- data.frame()
for (cmodel in unique(sink.vs.climate$model)){

  print(cmodel)

  cdf <- sink.vs.climate %>%
    filter(model == cmodel) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  ccdf <- cdf %>%
    ungroup() %>%
    # filter(cAGB.m >= median(cAGB.m)) %>%
    filter(model.lat.lon %in% TF[["model.lat.lon"]]) %>%
    dplyr::select(-c(lat,lon,model,model.lat.lon))

  CN <- colnames(ccdf)
  Var.names <- CN[!(CN %in% c("lat","lon","sink.m","model.lat.lon"))]

  LM <- lm(data = ccdf,
            formula = sink.m ~ .^2)

  LM2 <- lm(data = ccdf[,Var.names],
           formula = cAGB.m ~ .^2)

  rf=randomForest(formula = sink.m ~ .^2,
                  data = ccdf,
                  # do.trace = TRUE,
                  ntree = 500,importance = TRUE)

  rf2=randomForest(formula = cAGB.m ~ .^2,
                  data = ccdf[,Var.names],
                  # do.trace = TRUE,
                  ntree = 500,importance = TRUE)

  df.predict <- bind_rows(df.predict,
                          data.frame(obs = ccdf$sink.m,
                                     predict.lm = as.vector(predict(LM)),
                                     predict.rf = as.vector(predict(rf))) %>%
                            mutate(model = cmodel))

  i<-importance(rf)
  i.ordered <- i[order(i[, "%IncMSE"], decreasing=TRUE),]

  i2<-importance(rf2)
  i.ordered2 <- i2[order(i2[, "%IncMSE"], decreasing=TRUE),]

  df.importance <- bind_rows(df.importance,
                             as.data.frame(i.ordered) %>%
                               tibble::rownames_to_column() %>%
                               ungroup() %>%
                               mutate(model = cmodel) %>%
                               mutate(variable = "sink"),
                             as.data.frame(i.ordered2) %>%
                               tibble::rownames_to_column() %>%
                               ungroup() %>%
                               mutate(model = cmodel) %>%
                               mutate(variable = "agb"))

  corr <- data.frame(r2.lm = summary(LM)[["adj.r.squared"]],
                    r2.rf = mean(rf$rsq),
              .groups = "keep") %>%
    mutate(model = cmodel,
           variable = "sink")

  corr2 <- data.frame(r2.lm = summary(LM2)[["adj.r.squared"]],
                     r2.rf = mean(rf2$rsq),
                     .groups = "keep") %>%
    mutate(model = cmodel,
           variable = "agb")

  all.corr <- bind_rows(all.corr,
                        corr,
                        corr2)
}

ggplot(df.predict %>%
         filter(!(grepl("LP",model))),
       aes(x = predict.rf,
           y = obs)) +
  geom_pointdensity() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  scale_color_viridis_c() +
  theme_bw()

ggplot(data = df.predict) +
  geom_point(aes(x = predict.rf,
                 y = obs,
                 color = model), size = 0.1) +
  theme_bw()

all.corr <- all.corr %>%
  arrange(desc(r2.rf))

ggplot(data = all.corr) +
  geom_density(aes(x = r2.lm,fill = variable), alpha = 0.5) +
  facet_wrap(~variable,scales = "free_y") +
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


ggplot(data = sink.vs.climate %>%
         mutate(model.lat.lon = paste0(model,".",lat,".",lon))  %>%
         filter(model.lat.lon %in% TF[["model.lat.lon"]]),
       aes(y = sink.m,
           x =  MCWD.m, color = model, fill = model)) +
  # geom_point() +
  theme_bw() +
  facet_wrap(~ Continent, scales = "free") +
  stat_smooth(method = "lm")
# scale_color_gradient(low = "white", high = "darkgreen",
#                      oob = scales::squish)
