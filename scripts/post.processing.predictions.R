rm(list = ls())

library(dplyr)
library(stringr)
library(zoo)
library(raster)
library(TrENDY.analyses)

models <- TrENDY.analyses::get.model.names.TRENDY()
models <- c("CABLE-POP","CLASSIC")

df.all.ts <- df.all.year <-
  df.all.ts.MEM <- df.all.year.MEM <-
  df.r2 <-
  data.frame()

for (cmodel in models){

  print(cmodel)
  cfile <- paste0("./outputs/predictions.XGB.",cmodel,"ERA5.RDS")

  if (!file.exists(cfile)){
    next()
  }

  predictions.XGB <- readRDS(cfile)

  if (nrow(predictions.XGB) == 0) next()

  # timeseries
  predictions.XGB.sum <- predictions.XGB %>%
    group_by(continent,model,year,month,var,biome) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              pred.ERA5.m = mean(pred.ERA5,na.rm = TRUE),
              pred.ERA5.sd = sd(pred.ERA5,na.rm = TRUE),
              .groups = "keep")

  df.all.ts <- bind_rows(list(df.all.ts,
                              predictions.XGB.sum %>%
                                mutate(model = cmodel)))


  # Yearly
  predictions.XGB.sum.year <- predictions.XGB %>%
    group_by(continent,model,year,var,biome) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              pred.ERA5.m = mean(pred.ERA5,na.rm = TRUE),
              pred.ERA5.sd = sd(pred.ERA5,na.rm = TRUE),
              .groups = "keep")

  df.all.year <- bind_rows(list(df.all.year,
                                predictions.XGB.sum.year %>%
                                    mutate(model = cmodel)))

  # r2
  df.r2 <- bind_rows(df.r2,
                     predictions.XGB %>%
    group_by(continent,model,lat,lon,year,var,biome) %>%
    summarise(obs.m = mean(obs,na.rm = TRUE),
              pred.m = mean(pred,na.rm = TRUE),
              pred.sd = sd(pred,na.rm = TRUE),
              pred.ERA5.m = mean(pred.ERA5,na.rm = TRUE),
              pred.ERA5.sd = sd(pred.ERA5,na.rm = TRUE),
              .groups = "keep") %>%
    group_by(continent,model,lat,lon,var,biome) %>%
    summarise(r2 = summary(lm(obs.m ~ pred.m))[["r.squared"]],
              r2.ERA5 = summary(lm(obs.m ~ pred.ERA5.m))[["r.squared"]],
              .groups = "keep") %>%
      mutate(model = cmodel))

}


MEM <- df.all.ts %>%
  group_by(continent,year,month,var,biome) %>%
  summarise(obs.MEM.m = mean(obs.m,na.rm = TRUE),
            pred.MEM.m = mean(pred.m,na.rm = TRUE),
            pred.ERA5.MEM.m = mean(pred.ERA5.m,na.rm = TRUE),
            .groups = "keep")

MEM.year <- df.all.year %>%
  group_by(continent,year,var,biome) %>%
  summarise(obs.MEM.m = mean(obs.m,na.rm = TRUE),
            pred.MEM.m = mean(pred.m,na.rm = TRUE),
            pred.ERA5.MEM.m = mean(pred.ERA5.m,na.rm = TRUE),
            .groups = "keep")


saveRDS(df.all.ts,"./outputs/df.all.ts.RDS")
saveRDS(df.all.year,"./outputs/df.all.year.RDS")
saveRDS(MEM,"./outputs/df.all.ts.MEM.RDS")
saveRDS(MEM.year,"./outputs/df.all.year.MEM.RDS")
saveRDS(df.r2,"./outputs/df.r2.predictions.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/post.processing.predictions.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
