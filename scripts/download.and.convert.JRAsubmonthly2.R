rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(tidyr)
library(lubridate)
library(PEcAn.data.atmosphere)


all.vars <- c("TMP_GDS4_HTGL",
              "SPFH_GDS4_HTGL")
var.names <- c("tmp","spfh")

correction <- c(1,1)

all.years <- data.frame()

overwrite = TRUE

for (cyear in seq(2019,2010)){

  for (cmonth in seq(1,12)){

    Ndays <- lubridate::days_in_month(paste0(cyear,"/",sprintf("%02d",cmonth),"/01"))
    cyearmonth <- paste0(cyear,sprintf("%02d",cmonth))

    df.daily <- data.frame()

    cOP <- paste0("./outputs/",cyearmonth,".RDS")

    if (file.exists(cOP) & !overwrite){next()}

    for (cday in seq(1,Ndays)){

      df.all <- data.frame()

      for (chour in c("00","06","12","18")){
        ctime <- paste0(cyear,
                        sprintf("%02d",cmonth),
                        sprintf("%02d",cday),
                        chour)
        system2("wget",
                c("-N",
                  paste0("https://data.rda.ucar.edu/ds628.1/fcst_surf/",cyearmonth,"/fcst_surf.",ctime)))
        system2("mv",
                c(paste0("fcst_surf.",
                         ctime),
                  paste0("./outputs/fcst_surf.",ctime,".grib")))

        system2("ncl_convert2nc",
                c(paste0("./outputs/fcst_surf.",
                         ctime,".grib")))

        ncfile <- paste0("fcst_surf.",ctime,".nc")

        if (!file.exists(ncfile)){ next()}

        nc <- nc_open(ncfile)

        lats <- ncvar_get(nc,"g4_lat_0")
        lons <- ncvar_get(nc,"g4_lon_1")

        for (ivar in seq(1,length(all.vars))){

          var <- ncvar_get(nc,all.vars[ivar])
          df <- melt(var) %>%
            rename(lon = Var1,
                   lat = Var2) %>%
            mutate(lon = lons[lon],
                   lat = lats[lat],
                   value = value*correction[ivar]) %>%
            filter(abs(lat) <= 25) %>%
            mutate(lon = case_when(lon > 180 ~ lon-360,
                                   TRUE ~ lon))
          df.all <- bind_rows(df.all,
                              df %>%
                                mutate(var = var.names[ivar],
                                       day = cday,
                                       hour = chour))

        }
        nc_close(nc)

      }

      df.wide <- df.all %>%
        pivot_wider(names_from = var,
                    values_from = value) %>%
        mutate(RH = qair2rh(spfh,
                            tmp - 273.15)) %>%
        mutate(VPD = get.vpd(RH*100,tmp - 273.15))

      df.wide.ave <- df.wide %>%
        group_by(lat,lon) %>%
        summarise(tmp = mean(tmp),
                  tmin = min(tmp),
                  tmax = max(tmp),
                  spfh = mean(spfh,na.rm = TRUE),
                  VPD = mean(VPD,na.rm = TRUE),
                  .groups = "keep")

      df.daily <- bind_rows(df.daily,
                            df.wide.ave %>%
                              mutate(day = cday))
    }

    cmonthdf <- df.daily %>%
      group_by(lon,lat) %>%
      summarise(tmp = mean(tmp, na.rm = TRUE),
                tmin = min(tmin, na.rm = TRUE),
                tmax = max(tmax, na.rm = TRUE),
                tmin2 = mean(tmin, na.rm = TRUE),
                tmax2 = mean(tmax, na.rm = TRUE),
                spfh = mean(spfh, na.rm = TRUE),
                VPD = mean(VPD, na.rm = TRUE),
                .groups = "keep")
    all.years <- bind_rows(all.years,
                           cmonthdf %>%
                             mutate(year = cyear,
                                    month = cmonth))

    system2("rm",
            paste0("./outputs/fcst_surf*"))
    system2("rm",
            paste0("./fcst_surf*"))

    saveRDS(cmonthdf,
            cOP)

  }
}
# https://data.rda.ucar.edu/ds628.8/fcst_surf/202301/fcst_surf.2023013100
# https://data.rda.ucar.edu/ds628.8/fcst_surf/202001/fcst_surf.2020010100
# fcst_surf.2023013100.nc

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/download.and.convert.JRAsubmonthly2.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
