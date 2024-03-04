rm(list = ls())

library(dplyr)

models <- TrENDY.analyses::get.model.names.TRENDY()
scenario = "S2"

for (model in models){

  print(model)

  model.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",model,".",scenario,".CC.pantropical.v11.RDS")
  if (!file.exists(model.file)){
    next()
  }
  A <- readRDS(model.file)
  Aprime <- A %>%
    filter(year < 2015)

  model.file.prime <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",model,".",scenario,".CC.pantropical.v11.prime.RDS")
  saveRDS(Aprime,
          model.file.prime)
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/modify.CCfiles.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

