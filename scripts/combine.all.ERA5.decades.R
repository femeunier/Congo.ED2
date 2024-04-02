rm(list = ls())

library(dplyr)

dir <- "/data/gent/vo/000/gvo00074/felicien/R/outputs/"

all.df <- data.frame()
for (decade in seq(1940,2020,10)){

  print(decade)
  cfile <- paste0(dir,"monthly.climate.pantropical.ERA5_",decade,".RDS")
  if (!file.exists(cfile)) next()
  cdf <- readRDS(cfile)

  all.df <- bind_rows(all.df,
                      cdf)

}

saveRDS(all.df,
        "./outputs/monthly.climate.pantropical.ERA5.RDS")
