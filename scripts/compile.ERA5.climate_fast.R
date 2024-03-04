rm(list = ls())

library(foreach)
library(doParallel)
library(dplyr)
library(doFuture)
library(future.callr)
library(doSNOW)
library("doFuture")

registerDoFuture()
plan(multisession,workers = 4)

# dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/"

years2change <- 1940:2024

all.df.years <- data.frame()
# all.df.years <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
#   filter(!(year %in% years2change))
# print(unique(all.df.years$year))
# print(summary(all.df.years$lat))

# print(summary(all.df.years$lon))

all.df.years <- foreach(cyear=years2change, .combine=bind_rows) %dofuture% {
# for (cyear in years2change){

  library(ncdf4)
  library(reshape2)
  library(dplyr)
  library(lubridate)

  print(cyear)

  dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/"
  vars <- c("t2m","ssrd","tp","strd","d2m")

  ncfile <- file.path(dir,paste0("ERA5_Tropics_",cyear,".nc"))
  nc <- nc_open(ncfile)
  lons <- ncvar_get(nc,"longitude")
  lats <- ncvar_get(nc,"latitude")
  times <- as.Date(ncvar_get(nc,"time")/24,
                   origin = "1900-01-01")

  all.df <- data.frame()

  for (ivar in seq(1,length(vars))){

    cvar = vars[ivar]
    data <- ncvar_get(nc,cvar)

    if (cyear >= 2023){

      expvers <- ncvar_get(nc,"expver")

      melt.data <- melt(data) %>%
        mutate(lon = lons[Var1],
               lat = lats[Var2],
               expver = expvers[Var3],
               time = times[Var4])

      df <- bind_rows(melt.data %>%
                        filter(expver == 1) %>%
                        filter(!is.na(value)) %>%
                        rename(!!cvar := "value") %>%
                        dplyr::select(-c("expver",starts_with("Var"))),
                      melt.data %>%
                        filter(expver == 5) %>%
                        filter(!is.na(value)) %>%
                        rename(!!cvar := "value") %>%
                        dplyr::select(-c("expver",starts_with("Var"))))

    } else{
      df <- melt(data) %>%
        mutate(lon = lons[Var1],
               lat = lats[Var2],
               time = times[Var3]) %>%
        rename(!!cvar := "value") %>%
        dplyr::select(-starts_with("Var"))
    }

    if (ivar > 1){
      all.df <- all.df %>%
        left_join(df,
                  by = c("lat","lon","time"))
    } else{
      all.df <- df
    }
  }

  cdf <- all.df %>%
    mutate(month = month(time),
           day = day(time)) %>%
    mutate(t = t2m - 273.15,
           dewpoint = d2m - 273.15) %>%
    mutate(beta = (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t)),
           rh =   ((112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))) ^ 8) %>%
    mutate(sh = PEcAn.data.atmosphere::rh2qair(rh,
                                               t2m,
                                               101325)) %>%
    mutate(VPD = PEcAn.data.atmosphere::get.vpd(rh*100,
                                                t)) %>%
    group_by(month,day,lat,lon) %>%
    mutate(tmin = min(t2m),
           tmax = max(t2m)) %>%
    group_by(month,lat,lon) %>%
    summarise(tmp = mean(t2m),
              tmin = mean(tmin),
              tmax = mean(tmax),
              pre = mean(tp)*1000*3,
              dswrf = mean(ssrd)*6,
              dlwrf = mean(strd)/(3600),
              spfh = mean(sh),
              VPD = mean(VPD),
              .groups = "keep") %>%
    mutate(year = cyear)

  nc_close(nc)

  cdf
  # all.df.years <- bind_rows(
  #   all.df.years,
  #   cdf %>%
  #     mutate(year = cyear))

}

saveRDS(all.df.years,
        "./outputs/monthly.climate.pantropical.ERA5.RDS")



# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/compile.ERA5.climate_fast.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

