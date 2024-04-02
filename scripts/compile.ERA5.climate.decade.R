rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)
library(Congo.ED2)

overwrite = FALSE
for (decade in seq(1,10,1)){
  print(decade)

  years2change <- 1940:1949 + (decade - 1)*10
  OP.file <- paste0("./outputs/monthly.climate.pantropical.ERA5_",min(years2change),".RDS")

  if (file.exists(OP.file) & !overwrite){
    next()
  }

  convert.ERA5.decade(decade)
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/compile.ERA5.climate.decade.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

