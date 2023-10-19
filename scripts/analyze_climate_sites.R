rm(list = ls())

library(dplyr)
library(tidyr)
library(pracma)
library(lubridate)
library(ncdf4)

directory <- c("/data/gent/vo/000/gvo00074/ED_common_data/met/Amazon/Paracou/ERA5_Paracou_processed/ERA5_Paracou_1/ED2/",
               "/data/gent/vo/000/gvo00074/ED_common_data/met/Amazon/Manaus/ERA5_Manaus_processed/ERA5_Manaus_1/ED2/",
               "/data/gent/vo/000/gvo00074/ED_common_data/met/CB/extracted/ERA5_lat_0_lon_25_1/ED2/")

directory <- c("/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/Paracou/",
               "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/Manaus/",
               "/data/gent/vo/000/gvo00074/ED_common_data/met/CRUNCEP/YGB/")

site.name <- c("Paracou","Manaus","YGB")

years = 1960:1969
months = c("JAN","FEB","MAR","APR","MAY","JUN",
           "JUL","AUG","SEP","OCT","NOV","DEC")

df <- data.frame()

for (i in seq(1,length(directory))){

  dir <- directory[i]
  compt = 0

  for (iyear in seq(1,length(years))){

    print(iyear/length(years))

    for (imonth in seq(1,length(months))){

      cyear = years[iyear]
      cmonth = months[imonth]

      cfile <- file.path(dir,
                         paste0(cyear,cmonth,".h5"))

      nc <- nc_open(cfile)

      prate <- ncvar_get(nc,"prate")
      incoming_sw <- ncvar_get(nc,"vbdsf") + ncvar_get(nc,"vddsf") + ncvar_get(nc,"nbdsf") + ncvar_get(nc,"nddsf")
      tmp <- ncvar_get(nc,"tmp")

      cdf <- data.frame(year = cyear,
                        month = cmonth,
                        time = compt + 1:length(tmp),
                        tmp,
                        prate = prate,
                        sw = incoming_sw)
      compt = compt + length(tmp)

      df <- bind_rows(list(df,
                           cdf %>% mutate(Site = site.name[i])))


      nc_close(nc)

    }
  }
}

saveRDS(df,
        "./outputs/climate_CRU_sites.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/analyze_climate_sites.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
