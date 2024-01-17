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

xgb_model.prefix <- "XGB.fit.global"
suffix.generic <- "global"
compute.shape = FALSE

models <- TrENDY.analyses::get.model.names.TRENDY()
models <- c("CABLE-POP")

scenarios <- c("S2")
continents <- ""
all.vars <- c("gpp","nep",'npp')
biome.names <- readRDS("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2022_global.RDS") %>%
  pull(biome) %>% unique()

grid <- base::expand.grid(
  list(
    model = models,
    scenario = scenarios,
    biome = biome.names
  ))

for (cvar in all.vars){

  print(cvar)

  df.r2 <- df.importance <- df.predict <- df.coord <- all.extract <-
    all.examples <- full.dataset <- df.seasonal <- df.ts <- data.frame()

  all.shap_long <- df.shape.scores <-
    data.frame()

  suffix <- paste0(suffix.generic,
                   ".",cvar)

  for (irow in seq(1,nrow(grid))){

    crow <- grid[irow,] ; cmodel <- crow[["model"]] ; cscenario <- crow[["scenario"]]
    cbiome <- crow[["biome"]]
    cname <- gsub("\\/","",gsub(" ","",
                                paste0(xgb_model.prefix,".",
                                       cmodel,".",
                                       cscenario,".",
                                       cbiome)))

    cname.var <- paste0(cname,".",cvar)

    xgb_model.file <- paste0("./outputs/",cname.var,".RDS")

    if (any(!file.exists(c(xgb_model.file)))){
      next()
    }

    print(cname.var)

    xgb_model <- readRDS(xgb_model.file)

    test.data <- xgb_model$test.data
    test.label <- xgb_model$test.labels

    predicted <- predict(xgb_model,
                         test.data)

    full.dataset <- (bind_rows(

      bind_cols(xgb_model$training.data,
                data.frame(value = xgb_model$labels)) %>%
        mutate(type = "training",
               origin = "data"),

      bind_cols(xgb_model$validation.data,
                data.frame(value = xgb_model$validation.labels)) %>%
        mutate(type = "validation",
               origin = "data"),

      bind_cols(test.data,
                data.frame(value = test.label)) %>%
        mutate(type = "test",
               origin = "data"),

      bind_cols(test.data,
                data.frame(value = predicted)) %>%
        mutate(type = "test",
               origin = "model")) %>%
        arrange(lon,lat,year,month) %>%
        mutate(model = cmodel,
               var = cvar,
               biome = cbiome) %>%
        mutate(model.lat.lon = paste0(model,".",lat,".",lon)))


    df.seasonal <- bind_rows(df.seasonal,
                             full.dataset %>%
                               filter(type == "test") %>%
                               group_by(origin,month) %>%
                               summarise(value.m = mean(value,na.rm = TRUE),
                                         .groups = "keep") %>%
                               mutate(model = cmodel,
                                      var = cvar,
                                      biome = cbiome))

    df.ts <- bind_rows(df.ts,
                       full.dataset %>%
                         filter(type == "test") %>%
                         group_by(origin,year) %>%
                         summarise(value.m = mean(value,na.rm = TRUE),
                                   .groups = "keep") %>%
                         mutate(model = cmodel,
                                var = cvar,
                                biome = cbiome))

    df.predict <- bind_rows(df.predict,
                            data.frame(pred = predicted,
                                       obs = test.label) %>%
                              mutate(model = cmodel,
                                     var = cvar,
                                     biome = cbiome))

    df.r2 <- bind_rows(df.r2,
                       data.frame(r2 = 1 - sum((predicted- test.label)**2)/sum(((test.label - mean(test.label))**2)),
                                  RMSE = sqrt(1/(length(predicted) - 1)*sum((predicted- test.label)**2,na.rm = TRUE)),
                                  value.m = mean(abs(test.label)),
                                  value.med = median(abs(test.label)),
                                  model = cmodel,
                                  var = cvar,
                                  biome = cbiome))

    importance_matrix = xgb.importance(colnames(test.data),
                                       model = xgb_model$finalModel)

    df.importance <- bind_rows(df.importance,
                               as.data.frame(importance_matrix) %>%
                                 mutate(model = cmodel,
                                        biome = cbiome,
                                        var = cvar))

    df.coord <- bind_rows(df.coord,
                          bind_rows(as.data.frame(test.data),
                                    as.data.frame(xgb_model$trainingData)) %>%
                            ungroup() %>%
                            dplyr::select(lat,lon) %>%
                            distinct() %>%
                            mutate(model = cmodel,
                                   biome = cbiome)) %>%
      distinct()


    selected.dataset <- full.dataset %>%
      group_by(model,var,biome) %>%
      filter(model.lat.lon %in%
               sample(unique(model.lat.lon),1,replace = FALSE)) %>%
      ungroup()

    full.dataset.extract <- full.dataset %>%
      filter(model.lat.lon %in% unique(selected.dataset[["model.lat.lon"]]))

    selected.dataset.comp <- selected.dataset %>%
      complete(model.lat.lon = unique(selected.dataset$model.lat.lon),
               origin = "model",
               biome = unique(selected.dataset$biome),
               model = unique(selected.dataset$model),
               type = "test",
               year = unique(selected.dataset$year),
               var = unique(selected.dataset$var),
               month = 1:12)

    all.examples <- bind_rows(all.examples,
                              selected.dataset.comp)

    all.extract <- bind_rows(all.extract,
                             full.dataset.extract)

    if (compute.shape){
      ids <- sort(sample(1:nrow(xgb_model$trainingData),
                         size = 1000, replace = FALSE))
      shap_values <- shap.values(xgb_model = xgb_model$finalModel,
                                 X_train = xgb_model$trainingData[ids,])
      shape.score <- shap_values$mean_shap_score

      df.shape.scores <- bind_rows(df.shape.scores,
                                   data.frame(feature = names(shape.score),
                                              shap = as.vector(shape.score)) %>%
                                     mutate(model = cmodel,
                                            var = cvar,
                                            biome = cbiome))

      shap_long <- shap.prep(shap_contrib = shap_values$shap_score,
                             X_train = xgb_model$trainingData[ids,])

      all.shap_long <- bind_rows(all.shap_long,
                                 shap_long %>%
                                   mutate(model = cmodel,
                                          var = cvar,
                                          biome = cbiome))
    }
  }

  saveRDS(df.r2,
          paste0("./outputs/df.r2.",suffix,".RDS"))
  saveRDS(df.seasonal,
          paste0("./outputs/df.seasonal.",suffix,".RDS"))
  saveRDS(df.ts,
          paste0("./outputs/df.ts.",suffix,".RDS"))
  saveRDS(df.importance,
          paste0("./outputs/df.importance.",suffix,".RDS"))
  saveRDS(df.predict,
          paste0("./outputs/df.predict.",suffix,".RDS"))
  saveRDS(df.coord,
          paste0("./outputs/df.coord.",suffix,".RDS"))
  saveRDS(all.extract,
          paste0("./outputs/all.extract.",suffix,".RDS"))
  saveRDS(all.examples,
          paste0("./outputs/all.examples.",suffix,".RDS"))

  if (compute.shape){
    saveRDS(all.shap_long,
            paste0("./outputs/all.shap_long.",suffix,".RDS"))
    saveRDS(df.shape.scores,
            paste0("./outputs/df.shape.scores.",suffix,".RDS"))
  }
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/post.processing.fits.global.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

