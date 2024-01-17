rm(list = ls())

library(dplyr)
library(ggplot2)
library(xgboost)
library(SHAPforxgboost)
library(TrENDY.analyses)
library(zoo)

models <- get.model.names.TRENDY()
continents = "America"
all.biomes = c("Humid_large","Humid_low","Humid_seasonal")
vars = c("gpp","nep")

xgb.models <- expand.grid(continent = continents,
                          var = vars,
                          biome = all.biomes)


biomes <- readRDS("./outputs/biome.JRA.1901.2023.AI.RDS") %>%
  dplyr::select(lon,lat,biome,model)
years = 2023
Size = 100

# CO2
dataC02 <- read.table("./data/global_co2_ann_1700_2022.txt",
                      stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)

dataC02.all <- data.frame(year = sort(unique(c(dataC02$year,2020:2023)))) %>%
  arrange(year) %>%
  left_join(dataC02,
            by = c("year")) %>%
  mutate(CO2 = na.spline(CO2,method = "natural"))



for (cmodel in models){

  df.shap <- df.shap.long <- data.frame()

  print(paste0("- ", cmodel))

  file <- paste0("./data/grid.",cmodel,".JRA.historical.RDS")

  if (any(!file.exists(file))){
    next()
  }

  climate <- readRDS(file)

  biome <- biomes %>%
    filter(model == cmodel) %>%
    dplyr::select(-model)

  climate.select <- climate %>%
    filter(year %in% years) %>%
    left_join(biome,
              by = c("lat","lon")) %>%
    mutate(continent = Congo.ED2::coord2continent(lon,lat))

  for (i in seq(1,nrow(xgb.models))){
    cContinent = as.character(xgb.models$continent[i])
    cbiome = as.character(xgb.models$biome[i])
    cvar = as.character(xgb.models$var[i])

    xgb.file <- paste0(
      "./outputs/XGB.fit.JRA.historical.",cmodel,".S2.",cContinent,".",cbiome,".",cvar,".RDS")

    print(paste0("-- ", xgb.file))
    if (!file.exists(xgb.file)){
      next()
    }
    xgb_model <- readRDS(xgb.file)

    # With the training data only (--> 12/31/2021)
    tdata <- as.data.frame(xgb_model$training.data) %>%
      filter(!is.na(tmp))
    ids <- sort(sample(1:nrow(tdata),
                       size =  min(nrow(tdata),Size), replace = FALSE))
    shap_values <- shap.values(xgb_model = xgb_model$finalModel,
                               X_train = as.matrix(tdata[ids,xgb_model$finalModel$feature_names]))
    shape.score <- shap_values$mean_shap_score
    shap_long <- shap.prep(shap_contrib = shap_values$shap_score,
                           X_train = as.matrix(tdata[ids,xgb_model$finalModel$feature_names]))

    # With the prediction data

    inputs <- climate.select %>%
      filter(continent %in% cContinent,
             biome %in% cbiome)  %>%
      left_join(dataC02.all,
                by = c("year"))

    cdf <- inputs %>%
      dplyr::select(-c("biome","model","continent"))

    # predicted <- predict(cXGBmodel,
    #                      cdf[,cXGBmodel$finalModel$feature_names])

    ids <- sort(sample(1:nrow(cdf),
                       size = min(nrow(cdf),Size), replace = FALSE))
    shap_values.pred <- shap.values(xgb_model = xgb_model$finalModel,
                                    X_train = as.matrix(cdf[ids,xgb_model$finalModel$feature_names]))
    shape.score.pred <- shap_values.pred$mean_shap_score
    shap_long.pred <- shap.prep(shap_contrib = shap_values.pred$shap_score,
                           X_train = as.matrix(cdf[ids,xgb_model$finalModel$feature_names]))


    cshap <-  data.frame(shap.score = c(shape.score,shape.score.pred),
                         variable = c(names(shape.score),names(shape.score.pred)),
                         origin = c(rep("training",length(shape.score)),
                                    rep("prediction",length(shape.score.pred)))) %>%
      mutate(model = cmodel,
             continent = cContinent,
             biome = cbiome,
             var = cvar)

    df.shap <- bind_rows(df.shap,
                         cshap)

    cshap.long <- bind_rows( as.data.frame(shap_long) %>%
                               mutate(origin = "training",
                                      model = cmodel,
                                      continent = cContinent,
                                      biome = cbiome,
                                      var = cvar),
                             as.data.frame(shap_long.pred) %>%
                               mutate(origin = "prediction",
                                      model = cmodel,
                                      continent = cContinent,
                                      biome = cbiome,
                                      var = cvar))

    df.shap.long <- bind_rows(df.shap.long,
                              cshap.long)


  }

  saveRDS(df.shap,
          paste0("./outputs/shap.predictions.JRA.historical.",cmodel,".RDS"))

  saveRDS(df.shap.long,
          "./outputs/shap.long.predictions.JRA.historical.")

}


# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/SHAPscores.JRA.predictions.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

