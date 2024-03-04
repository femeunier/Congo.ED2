rm(list = ls())

library(dplyr)
library(caret)
library(xgboost)
library(ggthemes)
library(sf)
library(zoo)

# products <- c("SIF","SIF2","VOD","NIR")
products <- c("SIF","SIF2","VOD","NIR")

df.all <- bind_rows(list(readRDS("./data/GPP/monthly/all.df.GPP2.RDS") %>%
                           mutate(product = "Madani") %>%
                           rename(value = daily.GPP) %>%
                           ungroup(),

                         # readRDS("./data/GPP/monthly/all.df.GPP.RDS") %>%
                         #   mutate(product = "Zhang") %>%
                         #   rename(value = daily.GPP) %>%
                         #   ungroup(),

                         readRDS("./data/GPP/monthly/NIR.GPP.RDS") %>%
                           mutate(product = "NIR") %>%
                           ungroup(),

                         # readRDS("./data/GPP/monthly/df.all.GPP3.RDS") %>%
                         #   mutate(product = "Fluxcom") %>%
                         #   dplyr::select(-value) %>%
                         #   rename(value = daily.GPP) %>%
                         #   ungroup(),
                         #
                         # readRDS("./data/GPP/monthly/all.df.GPP.MODIS.RDS") %>%
                         #   mutate(product = "MODIS") %>%
                         #   rename(value = daily.GPP) %>%
                         #   ungroup(),

                         readRDS("./data/GPP/monthly/SIF.GPP2.RDS") %>%
                           mutate(product = "SIF2") %>%
                           ungroup(),

                         readRDS("./data/GPP/monthly/SIF.GPP.RDS") %>%
                           mutate(product = "SIF") %>%
                           ungroup(),

                         readRDS("./data/GPP/monthly/VOD.GPP.RDS") %>%
                           mutate(product = "VOD") %>%
                           ungroup())) %>%
  dplyr::select(product,lat,lon,year,month,value) %>%
  filter(product %in% products)


dataC02 <- read.table("./data/global_co2_ann_1700_2022.txt",
                      stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)

dataC02.all <- data.frame(year = sort(unique(c(dataC02$year,2020:2023)))) %>%
  arrange(year) %>%
  left_join(dataC02,
            by = c("year")) %>%
  mutate(CO2 = na.spline(CO2,method = "natural"))

climate <- readRDS("./outputs/monthly.climate.pantropical.JRA.historical.RDS") %>%
  left_join(dataC02.all,
            by = c("year"))

df.all.pred.sum <- df.all.pred <-
  df.test.pred <-
  data.frame()

for (cproduct in products){

  print(cproduct)

  GPPproduct.coords <- readRDS("./outputs/Amazon.coord.GPP.products.ILF.RDS") %>%
    filter(model == cproduct)

  GPP.products <- df.all %>%
    filter(product == cproduct) %>%
    mutate(GPP = value/1000*365) %>%
    dplyr::select(product,lat,lon,year,month,GPP) %>%
    mutate(model.lon.lat = paste0(product,".",
                                  round(lon,digits = 2),
                                  ".",
                                  round(lat,digits = 2))) %>%
    filter(model.lon.lat %in% GPPproduct.coords[["model.lon.lat"]]) %>%
    dplyr::select(-c(model.lon.lat))

  year.min <- min(GPP.products[["year"]], na.rm = TRUE)
  year.min <- 1994
  climate.pred <- climate %>%
    mutate(model.lon.lat = paste0(cproduct,".",
                                  round(lon,digits = 2),
                                  ".",
                                  round(lat,digits = 2))) %>%
    filter(model.lon.lat %in% GPPproduct.coords[["model.lon.lat"]]) %>%
    filter(year >= year.min)

  xgb_best_model <- readRDS(paste0("./outputs/",cproduct,".vs.climate.Amazon.ILF_nodrought.RDS"))

  predicted <- predict(xgb_best_model,
                       climate.pred[,xgb_best_model$finalModel$feature_names])
  predicted_test <- predict(xgb_best_model,
                            xgb_best_model$test.data[,xgb_best_model$finalModel$feature_names])


  all_test <- bind_cols(xgb_best_model$test.data[,xgb_best_model$finalModel$feature_names],
                        pred = predicted_test,
                        obs = xgb_best_model$test.labels)


  cpredicted <- bind_cols(climate.pred,
                          GPP = predicted)
  predicted.sum <- cpredicted %>%
    group_by(year,month) %>%
    summarise(gpp.m = mean(GPP,na.rm = TRUE),
              .groups = "keep") %>%
    filter(year >= year.min) %>%
    ungroup() %>%
    mutate(time = year + (month -1/2)/12) %>%
    mutate(slope = coef(lm(gpp.m ~ time))[2],
           intercept = coef(lm(gpp.m ~ time))[1]) %>%
    mutate(mean.pred = slope*(time) + intercept) %>%
    mutate(detrended = gpp.m - mean.pred) %>%
    group_by(month) %>%
    mutate(mean.month = mean(detrended)) %>%
    ungroup() %>%
    mutate(anomaly = detrended - mean.month) %>%
    ungroup() %>%
    mutate(anomaly.m = anomaly/sd(anomaly))

  RS.sum <- GPP.products %>%
    group_by(year,month) %>%
    summarise(gpp.m = mean(GPP,na.rm = TRUE),
              .groups = "keep") %>%
    filter(year >= year.min) %>%
    ungroup() %>%
    mutate(time = year + (month -1/2)/12) %>%
    mutate(slope = coef(lm(gpp.m ~ time))[2],
           intercept = coef(lm(gpp.m ~ time))[1]) %>%
    mutate(mean.pred = slope*(time) + intercept) %>%
    mutate(detrended = gpp.m - mean.pred) %>%
    group_by(month) %>%
    mutate(mean.month = mean(detrended)) %>%
    ungroup() %>%
    mutate(anomaly = detrended - mean.month) %>%
    ungroup() %>%
    mutate(anomaly.m = anomaly/sd(anomaly))

  df.all.pred.sum <- bind_rows(df.all.pred.sum,
                               predicted.sum %>%
                                 dplyr::select(year,month,gpp.m,anomaly.m) %>%
                                 rename(pred = gpp.m,
                                        pred.anomaly = anomaly.m) %>%
                                 left_join(RS.sum %>%
                                             dplyr::select(year,month,gpp.m,anomaly.m) %>%
                                             rename(obs = gpp.m,
                                                    obs.anomaly = anomaly.m),
                                           by = c("year","month")) %>%
                                 mutate(product = cproduct))


  df.all.pred <- bind_rows(df.all.pred,
                           cpredicted %>%
                             dplyr::select(lon,lat,month,year,GPP) %>%
                             rename(pred = GPP) %>%
                             left_join(GPP.products %>%
                                         dplyr::select(-product) %>%
                                         rename(obs = GPP),
                                       by = c("year","month","lat","lon")) %>%
                             mutate(product = cproduct))

  df.test.pred <- bind_rows(df.test.pred,
                            all_test %>%
                              mutate(product = cproduct))


}

saveRDS(df.test.pred,
        "./outputs/test.predictions.SIF.ILF_nodrought.RDS")

saveRDS(df.all.pred,
        "./outputs/all.predictions.SIF.ILF_nodrought.RDS")

saveRDS(df.all.pred.sum,
        "./outputs/all.predictions.SIF.sum.ILF_nodrought.RDS")
