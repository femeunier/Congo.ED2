rm(list = ls())

library(dplyr)
library(ggplot2)
library(caret)
library(xgboost)
library(randomForest)
library(tidyr)
library(lubridate)
library(YGB)
library(SHAPforxgboost)


################################################################################

# library(corrplot)
# model <- readRDS("./outputs/XGB.fit.reclass.CABLE-POP.S3.Africa.Humid_large.gpp.RDS")
# trainingData <- as.data.frame(model$trainingData)
# selected.td <- trainingData %>%
#   dplyr::filter(lon == -9.5,
#          lat == 5.5) %>%
#   dplyr::select(
#     where(
#       ~!all((.x == mean(.x,na.rm = TRUE)))
#     ))
#
# TestData <- as.data.frame(model$test.data)
# pos <- TestData$year > 2015
# test.labels <- model$test.labels
# predicted <- predict(model,TestData)
# plot(test.labels[pos],
#      predicted[pos])

# plot(selected.td$year +
#        (selected.td$month -1/2)/12,selected.td$CO2,type = "l")
#
# plot(selected.td$c3ann_to_c3nfx,selected.td$c3nfx_to_secdn,type = "p")
#
#
# M = cor(selected.td %>%
#           dplyr::select(-c(month,year)))
# corrplot(M, method = 'number') # colorful number
################################################################################
#
# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/XGB.fit.reclass.ORCHIDEE.S3*",
#               "./outputs/"))

xgb_model.prefix <- "XGB.fit.reclass"

models <- TrENDY.analyses::get.model.names.TRENDY()
models <- c("ORCHIDEE")

scenarios <- c("S3")
continents <- c("Africa","America")
all.vars <- c("gpp","nep")
biome.names <- c("Humid_large",
                 "Humid_low",
                 "Humid_seasonal",
                 "Dry_subhumid")


grid <- base::expand.grid(
  list(
    model = models,
    scenario = scenarios,
    continent = continents,
    biome = biome.names
  ))

df.r2 <- df.importance <- df.predict <- df.coord <-
  full.dataset <- data.frame()
all.xgb.models <- list()

list_dir <- list() ; jobname <- "job.sh"

for (irow in seq(1,nrow(grid))){

  crow <- grid[irow,] ; cmodel <- crow[["model"]] ; cscenario <- crow[["scenario"]]
  cContinent <- crow[["continent"]] ; cbiome <- crow[["biome"]]
  cname <- gsub("\\/","",gsub(" ","",
                              paste0(xgb_model.prefix,".",
                                     cmodel,".",
                                     cscenario,".",
                                     cContinent,".",
                                     cbiome)))

  print(paste0(irow,"/",nrow(grid),": ",cname))

  for (cvar in all.vars){

    print(paste0("- ",cvar))
    cname.var <- paste0(cname,".",cvar)

    xgb_model.file <- paste0("./outputs/",cname.var,".RDS")

    if (any(!file.exists(c(xgb_model.file)))){
      next()
    }

    xgb_model <- readRDS(xgb_model.file)
    # all.xgb.models[[cname.var]] <- xgb_model

    test.data <- xgb_model$test.data
    test.label <- xgb_model$test.labels

    predicted <- predict(xgb_model,
                         test.data)

    full.dataset <- bind_rows(full.dataset,
                              bind_rows(
                                bind_cols(
                                  xgb_model$trainingData,
                                          data.frame(value = xgb_model$labels)) %>%
                                  dplyr::select(lon,lat,year,month,value) %>%
                                  mutate(type = "training",
                                         origin = "data"),
                                bind_cols(
                                  test.data,
                                          data.frame(value = test.label)) %>%
                                  dplyr::select(lon,lat,year,month,value) %>%
                                  mutate(type = "test",
                                         origin = "data"),
                                bind_cols(
                                  test.data,
                                          data.frame(value = predicted)) %>%
                                  dplyr::select(lon,lat,year,month,value) %>%
                                  mutate(type = "test",
                                         origin = "model")) %>%
                                arrange(lon,lat,year,month) %>%
                                mutate(model = cmodel,
                                       var = cvar,
                                       biome = cbiome,
                                       continent = cContinent) %>%
                                mutate(model.lat.lon = paste0(model,"_",lat,"_",lon)))

    df.predict <- bind_rows(df.predict,
                            data.frame(pred = predicted,
                                       obs = test.label) %>%
                              mutate(model = cmodel,
                                     var = cvar,
                                     biome = cbiome,
                                     continent = cContinent))

    df.r2 <- bind_rows(df.r2,
                       data.frame(r2 = 1 - sum((predicted- test.label)**2)/sum(((test.label - mean(test.label))**2)),
                                  RMSE = sqrt(1/(length(predicted) - 1)*sum((predicted- test.label)**2,na.rm = TRUE)),
                                  value.m = mean(abs(test.label)),
                                  value.med = median(abs(test.label)),
                                  model = cmodel,
                                  var = cvar,
                                  biome = cbiome,
                                  continent = cContinent))

    importance_matrix = xgb.importance(colnames(test.data),
                                       model = xgb_model$finalModel)

    df.importance <- bind_rows(df.importance,
                               as.data.frame(importance_matrix) %>%
                                 mutate(model = cmodel,
                                        biome = cbiome,
                                        continent = cContinent,
                                        var = cvar))

    df.coord <- bind_rows(df.coord,
                          bind_rows(as.data.frame(test.data),
                                    as.data.frame(xgb_model$trainingData)) %>%
                            ungroup() %>%
                            filter(year == year[1]) %>%
                            dplyr::select(lat,lon) %>%
                            distinct() %>%
                            mutate(model = cmodel,
                                   biome = cbiome,
                                   continent = cContinent)) %>%
      distinct()
  }
}



world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
df.coord <- df.coord %>%
  group_by(model) %>%
  mutate(A = min(diff(sort(unique(lon)))),
         B = min(diff(sort(unique(lat))))) %>%
  mutate(lon.min = lon - A/2,
         lon.max = lon + A/2,
         lat.min = lat - B/2,
         lat.max = lat + B/2)

ggplot() +

  geom_raster(data = df.coord,
              aes(x = lon, y = lat,
                  fill = biome), alpha = 0.5,linewidth = 0.5) +

  geom_sf(data = world,
          fill = NA) +
  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-100, 50),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()

ggplot(df.predict,
       aes(x = pred,
           y = obs)) +
  geom_hex() +
  # geom_density_2d_filled(aes(fill = log10(..density..)),
  #                        bins = 1000,
  #                        alpha = 0.5) +

  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(100,10000), oob = scales::squish,
    trans = "log10") +

  facet_grid(biome ~ var) +
  stat_smooth(method = "lm", se = FALSE,
              color = "red", linetype = 2) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")

ggscater <- ggplot(df.predict %>%
                     filter(var == "nep",
                            biome == "Humid_large"),
                   aes(x = pred,
                       y = obs,
                       color = model)) +
  geom_point(shape = NA) +
  geom_hex(color = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = 1, color = "black") +
  scale_fill_gradient(
    low = NA,
    high = "black",
    limits = c(100,10000), oob = scales::squish,
    trans = "log10") +
  scale_color_brewer(palette = "Set2") +
  stat_smooth(method = "lm", se = FALSE, linetype = 1) +
  # coord_equal() +
  # facet_wrap(~)
  theme_bw() +
  theme(legend.position = c(0.1,0.8)) +
  guides(color = "none")
ggExtra::ggMarginal(ggscater, type = "density",
                    groupColour = TRUE)

ggplot(df.predict,
       aes(x = pred - obs)) +
  geom_density(aes(fill = var,
                   group = interaction(var,
                                       continent,biome)), alpha = 0.5) +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = df.r2 %>% arrange(desc(r2))) +
  geom_density(aes(x = r2,
                   fill = var),alpha = 0.5) +
  facet_grid(~ model) +
  theme_bw()

level.ord <- df.importance %>%
  group_by(Feature) %>%
  summarise(Gain.m = median(Gain)) %>%
  arrange((Gain.m)) %>% pull(Feature)

df.importance.sum <- df.importance %>%
  group_by(continent,biome,var,Feature) %>%
  summarise(Gain.m = mean(Gain),
            Gain.sd = sd(Gain),
            .groups = "keep") %>%
  group_by(continent,biome,var) %>%
  mutate(minimp = sort(Gain.m,decreasing = TRUE)[10]) %>%
  ungroup() %>%
  filter(Feature %in% unique(Feature[Gain.m >= minimp])) %>%
  ungroup() %>%
  mutate(Feature = factor(Feature,
                          levels = level.ord[level.ord %in% Feature]))

ggplot(data = df.importance.sum,
       aes(x = Feature, y = Gain.m,
           fill = continent)) +
  geom_errorbar(aes(ymin = Gain.m*0.9,
                    ymax = Gain.m + Gain.sd),
                position = position_dodge(0.9),
                width = 0.5) +
  geom_bar(alpha = 1,
           stat = "identity",
           position = position_dodge(0.9)) +
  facet_grid(biome ~ var) +
  coord_flip() +
  theme_bw()

ggplot(data = df.importance.sum,
       aes(x = Feature, y = Gain.m,
           fill = var)) +
  geom_bar(alpha = 1,
           stat = "identity",
           position = position_dodge(0.9)) +
  facet_grid(biome ~ continent) +
  coord_flip() +
  theme_bw()

selected.dataset <- full.dataset %>%
  filter(biome == "Humid_large") %>%
  group_by(model,continent) %>%
  filter(model.lat.lon %in%
           sample(unique(model.lat.lon),1,replace = FALSE)) %>%
  ungroup()

selected.dataset.comp <- selected.dataset %>%
  dplyr::select(lon,lat,year,month,
                model,var,value,type,origin,
                biome,continent,model.lat.lon) %>%
  complete(model.lat.lon = unique(selected.dataset$model.lat.lon),
           origin = "model",
           continent = as.character(unique(selected.dataset$continent)),
           model = unique(selected.dataset$model),
           type = "test",
           year = unique(selected.dataset$year),
           var = unique(selected.dataset$var),
           month = 1:12) %>%
  mutate(model = sub("\\_.*", "", model.lat.lon)) %>%
  mutate(lon = as.numeric(sub(".*\\_", "", model.lat.lon)),
         lat = as.numeric(sub(".*_(.*)\\_.*", "\\1", model.lat.lon))) %>%
  ungroup() %>%
  mutate(Continent.check = Congo.ED2::coord2continent(as.numeric(lon),
                                                      as.numeric(lat))) %>%
  filter(continent == Continent.check)

vars2plot <- c("gpp","nep")

ggplot(mapping =
         aes(x = year + (month -1/2)/12,y = value,
             group = var)) +
  geom_line(data = selected.dataset %>%
              filter(origin == "data",
                     var %in% vars2plot) ,
            color = "black") +
  geom_line(data = selected.dataset.comp %>%
              filter(origin == "model",
                     var %in% vars2plot),
            color = "darkblue") +
  scale_x_continuous(limits = c(2010,2020)) +
  facet_grid(continent ~ model) +
  theme_bw()


selected.dataset.seasonal <- full.dataset %>%
  filter(type == "test") %>%
  filter(biome == "Humid_large") %>%
  group_by(var,model,origin,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = selected.dataset.seasonal %>%
         filter(var %in% vars2plot),
       mapping =
         aes(x = month,y = value.m, color = origin, fill = origin,
             group = interaction(var,origin))) +
  geom_ribbon(aes(ymin = value.m - value.sd,
                  ymax = value.m + value.sd), alpha = 0.5, color = NA) +
  geom_line() +
  facet_wrap(~ model) +
  theme_bw()


selected.dataset.year <- full.dataset %>%
  filter(biome == "Humid_large") %>%
  group_by(var,model,type,origin,continent,year,lat,lon) %>%
  summarise(value = mean(value,na.rm = TRUE)) %>%
  group_by(var,model,type,origin,continent,year) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            .groups = "keep")

selected.dataset.year.comp <- selected.dataset.year %>%
  ungroup() %>%
  complete(origin = c("model","data"),
           type = c("test","train"),
           model = unique(selected.dataset.year$model),
           continent = unique(selected.dataset.year$continent),
           year = unique(full.dataset$year),
           var = unique(selected.dataset.year$var))

ggplot(mapping =
         aes(x = year,y = value.m,
             ymin = value.m - value.sd,
             ymax = value.m + value.sd,
             group = var)) +
  geom_line(data = selected.dataset.year.comp %>%
              filter(origin == "data",
                     var %in% vars2plot) %>%
              na.omit(), color = "black") +
  geom_ribbon(data = selected.dataset.year.comp %>%
                filter(origin == "data",
                       var %in% vars2plot) %>%
                na.omit(),
              color = "darkgrey", alpha = 0.5,
              color = NA) +
  geom_point(data = selected.dataset.year.comp %>%
               filter(origin == "model",
                      type == "test",
                      var %in% vars2plot)) +
  geom_errorbar(data =selected.dataset.year.comp %>%
                  filter(origin == "model",
                         var %in% vars2plot,
                         type == "test")) +
  facet_grid(continent ~ model) +
  theme_bw()

stop()
################################################################################

all.shap_long <- df.shape.scores <-
  data.frame()

for (cmodel in models){

  for (cvar in vars){
    selected.model <- all.xgb.models[[cmodel]][[cvar]]

    if (is.null(selected.model)) next()

    print(paste0(cmodel," - ",cvar))

    ids <- sort(sample(1:nrow(selected.model$trainingData),
                       size = 1000, replace = FALSE))
    shap_values <- shap.values(xgb_model = selected.model$finalModel,
                               X_train = selected.model$trainingData[ids,])
    shape.score <- shap_values$mean_shap_score

    df.shape.scores <- bind_rows(df.shape.scores,
                                 data.frame(feature = names(shape.score),
                                            shap = as.vector(shape.score)) %>%
                                   mutate(model = cmodel,
                                          var = cvar))

    shap_long <- shap.prep(shap_contrib = shap_values$shap_score,
                           X_train = selected.model$trainingData[ids,])
    # shap.plot.summary(shap_long)

    all.shap_long <- bind_rows(all.shap_long,
                               shap_long %>%
                                 mutate(model = cmodel,
                                        var = cvar))

  }
}

ggplot(data = all.shap_long ,
       aes(x = rfvalue,y = value,
           color = model)) +
  # geom_point() +
  geom_smooth(method = "loess",
              size = 0.4, se = FALSE) +
  facet_grid(var ~ variable, scales = "free_x") +
  theme_bw()



shap.plot.dependence(data_long = shap_long,x = names(shape.score)[1],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[2],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[3],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[4],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[5],
                     add_hist = TRUE,add_stat_cor = TRUE)

