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
init.months <- c("010100","020100","030100","040100","050100","060100",
                 "070100","080100","090100","100100","110100","120100")
end.months <- c("013121","022821","033121","043021","053121","063021",
                "073121","083121","093021","103121","113021","123121")
end.months2 <- c("013121","022921","033121","043021","053121","063021",
                 "073121","083121","093021","103121","113021","123121")
init.month.num <- 1:12
end.month.num <- 1:12


for (cyear in seq(2020,2020)){

  print(cyear)
  df.year <- data.frame()
  cOP <- paste0("./outputs/",cyear,"_JRA.RDS")

  if (file.exists(cOP) & !overwrite){next()}

  for (imonth in seq(1,length(init.months))){
    cmonths <- init.month.num[imonth]:end.month.num[imonth]

    ncfiles <- c() ; compt <- 1
    for (ivar in seq(1,length(var.names))){

      cvar <- var.names[ivar]
      if (cvar == "tmp"){
        cfile <- paste0("fcst_surf.011_",cvar,".reg_tl319.",cyear,init.months[imonth],"_",cyear,
                        end.months[imonth])
      } else {
        cfile <- paste0("fcst_surf.051_",cvar,".reg_tl319.",cyear,init.months[imonth],"_",cyear,
                        end.months[imonth])
      }

      system2("wget",
              c("-N",
                paste0("https://data.rda.ucar.edu/ds628.0/fcst_surf/",
                       cyear,"/",cfile)
              ))

      if (!file.exists(cfile)){
        if (cvar == "tmp"){
          cfile <- paste0("fcst_surf.011_",cvar,".reg_tl319.",cyear,init.months[imonth],"_",cyear,
                          end.months2[imonth])
        } else {
          cfile <- paste0("fcst_surf.051_",cvar,".reg_tl319.",cyear,init.months[imonth],"_",cyear,
                          end.months2[imonth])
        }

        system2("wget",
                c("-N",
                  paste0("https://data.rda.ucar.edu/ds628.0/fcst_surf/",
                         cyear,"/",cfile)
                ))
      }

      system2("mv",
              c(cfile,paste0("./outputs/",cfile,".grib")
              ))

      system2("ncl_convert2nc",
              c(paste0("./outputs/",cfile,".grib")))

      system2("mv",
              c(paste0(cfile,".nc"),
                "./outputs/"))

      ncfile <- paste0("./outputs/",cfile,".nc")
      ncfiles[compt] <- ncfile ; compt <- compt + 1
    }

    nc <- nc_open(ncfiles[1])
    nc2 <- nc_open(ncfiles[2])

    all.lats <- ncvar_get(nc,"g4_lat_2")
    lats <- which(abs(all.lats) <= 25)
    clats <- all.lats[lats]
    all.lons <- ncvar_get(nc,"g4_lon_3")
    all.times <- as.Date(ncvar_get(nc,"initial_time0_hours")/24,
                         origin = "1800-01-01 00:00")
    all.years <- year(all.times)
    which.years <- which(all.years == cyear)
    all.months <- month(all.times)
    all.days <- day(all.times)

    for (cmonth in cmonths){

      df.all <- data.frame()

      months <- which(all.months == cmonth)
      days <- all.days[months]

      var <- ncvar_get(nc,all.vars[1],
                       start = c(1,min(lats),1,min(months)),
                       c(length(all.lons),length(lats),1,length(months)))

      var2 <- ncvar_get(nc2,all.vars[2],
                        start = c(1,min(lats),1,min(months)),
                        c(length(all.lons),length(lats),1,length(months)))

      df <- melt(var) %>%
        rename(lon = Var1,
               lat = Var2,
               time = Var3) %>%
        mutate(lon = all.lons[lon],
               lat = clats[lat],
               day = days[time],
               value = value*correction[ivar]) %>%
        filter(abs(lat) <= 25) %>%
        mutate(lon = case_when(lon > 180 ~ lon-360,
                               TRUE ~ lon))

      df2 <- melt(var2) %>%
        rename(lon = Var1,
               lat = Var2,
               time = Var3) %>%
        mutate(lon = all.lons[lon],
               lat = clats[lat],
               day = days[time],
               value = value*correction[ivar]) %>%
        filter(abs(lat) <= 25) %>%
        mutate(lon = case_when(lon > 180 ~ lon-360,
                               TRUE ~ lon))

      tmp <- df$value ;    spfh <- df2$value
      df.all <- bind_rows(df.all,
                          df %>%
                            group_by(lat,lon,day) %>%
                            summarise(tmin = min(value),
                                      tmean = mean(value),
                                      tmax = max(value),
                                      .groups = "keep") %>%
                            group_by(lat,lon) %>%
                            summarise(tmin = mean(tmin),
                                      tmean = mean(tmean),
                                      tmax = mean(tmax),
                                      .groups = "keep") %>%
                            pivot_longer(cols = c(tmin,tmean,tmax),
                                         values_to = "value",
                                         names_to = "var"),
                          df2 %>%
                            group_by(lat,lon) %>%
                            summarise(value = mean(value),
                                      var = "spfh",
                                      .groups = "keep"))

      RH <- qair2rh(spfh,
                    tmp - 273.15)
      VPD <- get.vpd(RH*100,tmp - 273.15)
      vpd <- data.frame(lat = df$lat,
                        lon = df$lon,
                        VPD) %>%
        group_by(lat,lon) %>%
        summarise(value = mean(VPD),
                  var = "VPD",
                  .groups = "keep")

      df.all <- bind_rows(df.all,
                          vpd)

      df.year <- bind_rows(
        df.year,
        df.all %>% mutate(month = cmonth,
                          year = cyear))
    }

    nc_close(nc) ; nc_close(nc2)

  }

  system2("rm",
          paste0("./outputs/fcst_surf.0*"))
  system2("rm",
          paste0("./fcst_surf.0*"))

  saveRDS(df.year,
          cOP)
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/download.and.convert.JRA_historical2.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
