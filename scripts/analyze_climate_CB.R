rm(list = ls())

library(dplyr)
library(tidyr)
library(pracma)
library(lubridate)
library(ncdf4)

ED_REG_LATMIN = -15
ED_REG_LATMAX = 10
ED_REG_LONMIN = -10
ED_REG_LONMAX = 45

GRID_RES = 1

X = seq(ED_REG_LONMIN,ED_REG_LONMAX,GRID_RES)
Y = seq(ED_REG_LATMIN,ED_REG_LATMAX,GRID_RES)

land.sea.mask <- readRDS("./data/LandSeaMask.RDS")

df.mask <- data.frame(lat = as.vector(meshgrid(Y,X)[[1]]),
                      lon = as.vector(meshgrid(Y,X)[[2]])) %>%
  left_join(land.sea.mask,
            by = c("lat","lon")) %>%
  filter(mask == 1)

years = 1961:1969
months = c("JAN","FEB","MAR","APR","MAY","JUN",
           "JUL","AUG","SEP","OCT","NOV","DEC")

df <- data.frame()

for (i in seq(1,nrow(df.mask))){

  print(i/nrow(df.mask))

  clat = df.mask[["lat"]][i]
  clon = df.mask[["lon"]][i]
  dir <- file.path("/data/gent/vo/000/gvo00074/ED_common_data/met/CB/extracted/",
                   paste0("ERA5_lat_",clat,"_lon_",clon,"_1"),
                   "ED2")

  for (iyear in seq(1,length(years))){
    for (imonth in seq(1,length(months))){

      cyear = years[iyear]
      cmonth = months[imonth]

      cfile <- file.path(dir,
                         paste0(cyear,cmonth,".h5"))


      nc <- nc_open(cfile)

      prate <- ncvar_get(nc,"prate")
      incoming_sw <- ncvar_get(nc,"vbdsf") + ncvar_get(nc,"vddsf") + ncvar_get(nc,"nbdsf") + ncvar_get(nc,"nddsf")
      tmp <- ncvar_get(nc,"tmp")

      cdf <- data.frame(lat = clat,
                        lon = clon,
                        year = cyear,
                        month = cmonth,
                        tmp = mean(tmp),
                        prate = mean(prate),
                        sw = mean(incoming_sw))

      df <- bind_rows(list(df,
                           cdf))


      nc_close(nc)

    }
  }
}

saveRDS(df,
        "./outputs/climate_BC_ERA5.RDS")

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/analyze_climate_CB.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
