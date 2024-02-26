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
    mutate(timing = case_when(year == 2016 & month %in% c(1:4) ~ "2015",
                              year == 2015 & month %in% c(10:12) ~ "2015",
                              TRUE ~ "other")) %>%
    filter(timing == "other") %>%
    dplyr::select(-c(timing))

  model.file.prime <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",model,".",scenario,".CC.pantropical.v11.prime.RDS")
  saveRDS(Aprime,
          model.file.prime)
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/modify.CCfiles.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/

