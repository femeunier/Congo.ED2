rm(list = ls())

library(dplyr)
library(caret)
library(xgboost)
library(ggthemes)
library(sf)
library(zoo)

# system2("rsync",
#         c("-avz",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/*.vs.climate.Amazon.ILF.ERA5.RDS",
#           "./outputs"))

products <- c("SIF2","NIR","VOD","SIF")
# products <- c("SIF","SIF2","VOD","NIR",
#               "Madani","Zhang","Fluxcom","MODIS")

df.all <- readRDS("./outputs/All.GPP.products.RDS") %>%
  filter(product %in% products)


dataC02 <- read.table("./data/global_co2_ann_1700_2022.txt",
                      stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)

dataC02.all <- data.frame(year = sort(unique(c(dataC02$year,2020:2024)))) %>%
  arrange(year) %>%
  left_join(dataC02,
            by = c("year")) %>%
  mutate(CO2 = na.spline(CO2,method = "natural"))

climate <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
  left_join(dataC02.all,
            by = c("year"))

df.all.pred <-
  df.test.pred <-
  data.frame()

GPPproduct.coords <- readRDS("./outputs/Coord.ILF.ERA5.RDS")

for (cproduct in products){

  print(cproduct)

  GPP.products <- df.all %>%
    filter(product == cproduct) %>%
    mutate(GPP = value/1000*365) %>%
    dplyr::select(product,lat,lon,year,month,GPP) %>%
    mutate(model.lon.lat = paste0(product,
                                  ".",
                                  round(lon,digits = 2),
                                  ".",
                                  round(lat,digits = 2))) %>%
    filter(model.lon.lat %in% GPPproduct.coords[["model.lon.lat"]]) %>%
    dplyr::select(-c(model.lon.lat))

  # year.min <- min(GPP.products[["year"]], na.rm = TRUE)
  year.min <- 1994

  climate.pred <- climate %>%
    mutate(lon.lat = paste0(round(lon,digits = 2),
                            ".",
                            round(lat,digits = 2))) %>%
    filter(lon.lat %in% GPPproduct.coords[["lon.lat"]]) %>%
    filter(year >= year.min)

  xgb_best_model <- readRDS(paste0("./outputs/",cproduct,".vs.climate.Amazon.ILF.ERA5.RDS"))

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

  # ggplot(data = predicted.sum) +
  #   geom_line(aes(x = year + (month - 1/2)/12,
  #                 y = gpp.m)) +
  #   theme_bw()

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

ggplot(data = cpredicted %>%
         filter(lat == lat[1],
                lon == lon[1])) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = GPP)) +
  theme_bw()


saveRDS(df.test.pred,
        "./outputs/test.predictions.SIF.ILF.ERA5.RDS")

saveRDS(df.all.pred,
        "./outputs/all.predictions.SIF.ILF.ERA5.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/predictions.SIF.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

