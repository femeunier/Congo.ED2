rm(list = ls())

library(dplyr)
library(stringr)
library(zoo)
library(raster)
library(TrENDY.analyses)

models <- TrENDY.analyses::get.model.names.TRENDY()

df.all.ts <- df.all.year <-
  predictions.Amazon <- predictions.Congo <-
  predictions.Amazon.test <- predictions.Congo.test <-
  predictions.Amazon.sum <- predictions.Congo.sum <-
  df.all.ts.MEM <- df.all.year.MEM <-
  all.predictions <-
  data.frame()

Amazon.coord <- readRDS("./outputs/Amazon.coord.ILF.RDS")
Congo.coord <- readRDS("./outputs/Congo.coord.ILF.RDS")

for (cmodel in models){

  print(cmodel)
  cfile <- paste0("./outputs/predictions.XGB.",cmodel,".JRA.historical.IFLAmazon.RDS")
  cfile2 <- paste0("./outputs/predictions.XGB.test.",cmodel,".JRA.historical.IFLAmazon.RDS")

  if (!file.exists(cfile)){
    next()
  }

  predictions.XGB <- readRDS(cfile)
  predictions.XGB.test <- readRDS(cfile2)

  if (nrow(predictions.XGB) == 0) next()

  all.predictions <- bind_rows(all.predictions,
                               predictions.XGB %>%
                                 mutate(model = cmodel))

  predictions.Amazon <- bind_rows(predictions.Amazon,
                                  predictions.XGB %>%
                                    mutate(model = cmodel) %>%
                                    mutate(model.lon.lat =
                                             paste0(model,".",round(lon,digits = 2),".",round(lat,digits = 2))) %>%
                                    filter(model.lon.lat %in% Amazon.coord[["model.lon.lat"]]))

  predictions.Amazon.test <- bind_rows(predictions.Amazon.test,
                                  predictions.XGB.test %>%
                                    mutate(model = cmodel) %>%
                                    mutate(model.lon.lat =
                                             paste0(model,".",round(lon,digits = 2),".",round(lat,digits = 2))) %>%
                                    filter(model.lon.lat %in% Amazon.coord[["model.lon.lat"]]))


  # timeseries
  predictions.XGB.sum <- predictions.XGB %>%
    group_by(model,year,month,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.ts <- bind_rows(list(df.all.ts,
                              predictions.XGB.sum %>%
                                mutate(model = cmodel,
                                       basin = "Amazon")))


  # Yearly
  predictions.XGB.sum.year <- predictions.XGB %>%
    group_by(model,year,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.year <- bind_rows(list(df.all.year,
                                predictions.XGB.sum.year %>%
                                  mutate(model = cmodel,
                                         basin = "Amazon")))

  # r2
  # df.r2 <- bind_rows(df.r2,
  #                    predictions.XGB %>%
  #                      ungroup() %>%
  #   group_by(continent,model,lat,lon,year,var,biome) %>%
  #   summarise(obs.m = mean(obs,na.rm = TRUE),
  #             pred.m = mean(pred,na.rm = TRUE),
  #             pred.sd = sd(pred,na.rm = TRUE),
  #             pred.JRA.m = mean(pred.JRA,na.rm = TRUE),
  #             pred.JRA.sd = sd(pred.JRA,na.rm = TRUE),
  #             .groups = "keep") %>%
  #     ungroup() %>%
  #     filter(!is.na(pred.JRA.m),!is.na(pred.m),!is.na(obs.m)) %>%
  #   group_by(continent,model,lat,lon,var,biome) %>%
  #   summarise(r2 = summary(lm(obs.m ~ pred.m))[["r.squared"]],
  #             r2.JRA = summary(lm(obs.m ~ pred.JRA.m))[["r.squared"]],
  #             .groups = "keep") %>%
  #     mutate(model = cmodel))

}

Amazon <- all.predictions %>%
  ungroup() %>%
  filter(lon >= -90,lon <= -35,
         lat <= 10, lat >= -30,
         year >= 1980)

predictions.Amazon.sum <- predictions.Amazon %>%
  group_by(model,var,year,month) %>%
  summarise(pred = mean(pred,
                        na.rm = TRUE),
            obs = mean(obs,
                       na.rm = TRUE),
            .groups = "keep")

################################################################################
# Congo


for (cmodel in models){

  print(cmodel)
  cfile <- paste0("./outputs/predictions.XGB.",cmodel,".JRA.historical.IFLCongo.RDS")

  if (!file.exists(cfile)){
    next()
  }

  predictions.XGB <- readRDS(cfile)

  if (nrow(predictions.XGB) == 0) next()

  all.predictions <- bind_rows(all.predictions,
                               predictions.XGB %>%
                                 mutate(model = cmodel))


  predictions.Congo <- bind_rows(predictions.Congo,
                                 predictions.XGB %>%
                                   mutate(model = cmodel) %>%
                                   mutate(model.lon.lat =
                                            paste0(model,".",
                                                   round(lon,digits = 2),".",
                                                   round(lat,digits = 2))) %>%
                                   filter(model.lon.lat %in% Congo.coord[["model.lon.lat"]]))


  # timeseries
  predictions.XGB.sum <- predictions.XGB %>%
    group_by(model,year,month,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.ts <- bind_rows(list(df.all.ts,
                              predictions.XGB.sum %>%
                                mutate(model = cmodel,
                                       basin = "Congo")))


  # Yearly
  predictions.XGB.sum.year <- predictions.XGB %>%
    group_by(model,year,var) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              .groups = "keep")

  df.all.year <- bind_rows(list(df.all.year,
                                predictions.XGB.sum.year %>%
                                  mutate(model = cmodel,
                                         basin = "Congo")))

  # r2
  # df.r2 <- bind_rows(df.r2,
  #                    predictions.XGB %>%
  #                      ungroup() %>%
  #   group_by(continent,model,lat,lon,year,var,biome) %>%
  #   summarise(obs.m = mean(obs,na.rm = TRUE),
  #             pred.m = mean(pred,na.rm = TRUE),
  #             pred.sd = sd(pred,na.rm = TRUE),
  #             pred.JRA.m = mean(pred.JRA,na.rm = TRUE),
  #             pred.JRA.sd = sd(pred.JRA,na.rm = TRUE),
  #             .groups = "keep") %>%
  #     ungroup() %>%
  #     filter(!is.na(pred.JRA.m),!is.na(pred.m),!is.na(obs.m)) %>%
  #   group_by(continent,model,lat,lon,var,biome) %>%
  #   summarise(r2 = summary(lm(obs.m ~ pred.m))[["r.squared"]],
  #             r2.JRA = summary(lm(obs.m ~ pred.JRA.m))[["r.squared"]],
  #             .groups = "keep") %>%
  #     mutate(model = cmodel))

}

predictions.Congo.sum <- predictions.Congo %>%
  group_by(model,var,year,month) %>%
  summarise(pred = mean(pred,
                        na.rm = TRUE),
            obs = mean(obs,
                       na.rm = TRUE),
            .groups = "keep")


MEM <- df.all.ts %>%
  group_by(basin,year,month,var) %>%
  summarise(obs.MEM.m = mean(obs.m,na.rm = TRUE),
            pred.MEM.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

MEM.year <- df.all.year %>%
  group_by(basin,year,var) %>%
  summarise(obs.MEM.m = mean(obs.m,na.rm = TRUE),
            pred.MEM.m = mean(pred.m,na.rm = TRUE),
            .groups = "keep")

saveRDS(df.all.ts,"./outputs/df.all.ts.JRA.historical.IFL.RDS")
saveRDS(df.all.year,"./outputs/df.all.year.JRA.historical.IFL.RDS")
saveRDS(MEM,"./outputs/df.all.ts.MEM.JRA.historical.IFL.RDS")
saveRDS(MEM.year,"./outputs/df.all.year.MEM.JRA.historical.IFL.RDS")
saveRDS(predictions.Amazon,"./outputs/Amazon.mean.JRA.historical.IFL.RDS")
saveRDS(predictions.Amazon.test,"./outputs/Amazon.mean.test.JRA.historical.IFL.RDS")
saveRDS(predictions.Congo,"./outputs/Congo.mean.JRA.historical.IFL.RDS")
saveRDS(predictions.Amazon.sum,"./outputs/Amazon.mean.JRA.historical.IFL.sum.RDS")
saveRDS(predictions.Congo.sum,"./outputs/Congo.mean.JRA.historical.IFL.sum.RDS")
saveRDS(Amazon,"./outputs/Amazon.JRA.historical.IFL.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/post.processing.predictions.JRA.historical.ILF.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
