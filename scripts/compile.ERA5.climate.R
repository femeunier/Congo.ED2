rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)

dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/"

all.df.years <- data.frame()
all.df.years <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
  filter(year != 2023)
print(unique(all.df.years$year))
print(summary(all.df.years$lat))
print(summary(all.df.years$lon))

vars <- c("t2m","ssrd","tp","strd","d2m")
for (cyear in 2023:2023){

  print(cyear)

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

    if (cyear == 2023){

      expvers <- ncvar_get(nc,"expver")

      df <- melt(data) %>%
        mutate(lon = lons[Var1],
               lat = lats[Var2],
               time = times[Var4],
               expver = expvers[Var3]) %>%
        filter(expver == 1) %>%
        rename(!!cvar := "value") %>%
        dplyr::select(-c("expver",starts_with("Var")))
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
    mutate(month = month(time)) %>%
    mutate(t = t2m - 273.15,
           dewpoint = d2m - 273.15) %>%
    mutate(beta = (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t)),
           rh =   ((112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))) ^ 8) %>%
    mutate(sh = PEcAn.data.atmosphere::rh2qair(rh,
                                               t2m,
                                               101325)) %>%
    mutate(VPD = PEcAn.data.atmosphere::get.vpd(rh*100,
                         t)) %>%
    group_by(month,lat,lon) %>%
    summarise(tmp = mean(t2m),
              tmin = min(t2m),
              tmax = max(t2m),
              pre = mean(tp)*1000/4,      # mm/6h
              dswrf = mean(ssrd),
              dlwrf = mean(strd)/(3*3600),
              spfh = mean(sh),
              VPD = mean(VPD),
              .groups = "keep")
  nc_close(nc)

  all.df.years <- bind_rows(
    all.df.years,
    cdf %>%
      mutate(year = cyear))

}

saveRDS(all.df.years,
        "./outputs/monthly.climate.pantropical.ERA5.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/compile.ERA5.climate.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

