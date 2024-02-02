rm(list = ls())

library(dplyr)
library(stringr)
library(zoo)
library(raster)
library(TrENDY.analyses)

models <- TrENDY.analyses::get.model.names.TRENDY()

all.vars <- c("gpp","npp","nep")
years <- 1958:2023
suffixes <- c("IFLAmazon")

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
coord.list <- bind_rows(readRDS("./outputs/Amazon.coord.ILF.RDS"),
                        readRDS("./outputs/Congo.coord.ILF.RDS"))


for (csuffix in suffixes){

  for (cmodel in models){

    df.all <- df.test <- data.frame()

    print(cmodel)

    file <- paste0("./data/grid.",cmodel,".JRA.historical.RDS")

    if (any(!file.exists(file))){
      next()
    }

    climate <- readRDS(file)

    inputs <- climate %>%
      filter(year %in% years) %>%
      mutate(xgb.model = paste0("XGB.fit.JRA.historical.",csuffix,".",model,".S2")) %>%
      left_join(dataC02.all,
                by = c("year")) %>%
      mutate(model.lon.lat = paste0(model,".",round(lon,digits = 2),".",round(lat,digits = 2))) %>%
      filter(model.lon.lat %in% coord.list[["model.lon.lat"]])

    xgb.models <- expand.grid(model = unique(inputs$xgb.model),
                              var = all.vars) %>%
      mutate(xgb.model = paste0(model,".",var,".RDS"))

    for (i in seq(1,nrow(xgb.models))){

      cxgb.model <- as.character(xgb.models[["xgb.model"]][i])
      cvar <- as.character(xgb.models[["var"]][i])
      cxgb.model.novar <- as.character(xgb.models[["model"]][i])

      print(paste0("- ",cxgb.model.novar," - ",cvar))

      cfile <- file.path("/data/gent/vo/000/gvo00074/felicien/R/outputs/",
                         paste0(cxgb.model))

      if (!file.exists(cfile)) next()

      cXGBmodel <- readRDS(cfile)

      all.data <- bind_rows(
        bind_cols(cXGBmodel$test.data,
                  !!as.character(cvar) := cXGBmodel$test.labels),

        bind_cols(cXGBmodel$validation.data,
                  !!as.character(cvar) := cXGBmodel$validation.labels),

        bind_cols(cXGBmodel$training.data,
                  !!as.character(cvar) := cXGBmodel$labels)) %>%
        filter(year %in% years) %>%
        mutate(model.lon.lat = paste0(cmodel,".",round(lon,digits = 2),".",round(lat,digits = 2)))

      test.data <- bind_rows(
        bind_cols(cXGBmodel$test.data,
                  !!as.character(cvar) := cXGBmodel$test.labels)) %>%
        filter(year %in% years) %>%
        mutate(model.lon.lat = paste0(cmodel,".",round(lon,digits = 2),".",round(lat,digits = 2)))


      cdf <- inputs %>%
        filter(xgb.model == cxgb.model.novar) %>%
        filter(model.lon.lat %in% all.data[["model.lon.lat"]]) %>%
        dplyr::select(-c("model","xgb.model","model.lon.lat"))

      predicted <- predict(cXGBmodel,
                           cdf[,cXGBmodel$finalModel$feature_names])

      predicted_test <- predict(cXGBmodel,
                                cXGBmodel$test.data[,cXGBmodel$finalModel$feature_names])

      all_test <- bind_cols(cXGBmodel$test.data[,cXGBmodel$finalModel$feature_names],
                            pred = predicted_test,
                            obs = cXGBmodel$test.labels) %>%
        filter(year %in% years)

      all.predicted <- inputs %>%
        filter(xgb.model == cxgb.model.novar) %>%
        filter(model.lon.lat %in% all.data[["model.lon.lat"]]) %>%
        dplyr::select(c("model","xgb.model","year",
                        "model.lon.lat",
                        "month","lat","lon")) %>%
        mutate(var = cvar,
               pred = predicted)

      cpredicted <- all.predicted %>%
        left_join(all.data %>%
                    dplyr::select(model.lon.lat,year,month,cvar) %>%
                    rename(obs := !!cvar),
                  by = c("model.lon.lat","year","month"))

      df.all <- bind_rows(df.all,
                          cpredicted)
      df.test <- bind_rows(df.test,
                           all_test)

    }

    saveRDS(df.all,
            paste0("./outputs/predictions.XGB.",cmodel,".JRA.historical.",csuffix,".RDS"))
    saveRDS(df.test,
            paste0("./outputs/predictions.XGB.test.",cmodel,".JRA.historical.",csuffix,".RDS"))
  }
}


# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/test.XGBmodels.JRA.historical.IFL.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

