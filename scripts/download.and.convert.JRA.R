rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(tidyr)

for (cyear in seq(2020,2023)){
  for (cmonth in seq(1,12)){

    cyearmonth <- paste0(cyear,sprintf("%02d",cmonth))
    system2("wget",
            c("-N",
              paste0("https://data.rda.ucar.edu/ds628.9/fcst_phy2m/",cyearmonth,"/fcst_phy2m.",cyearmonth)))
    system2("mv",
            c(paste0("fcst_phy2m.",
                     cyearmonth),
              paste0("./outputs/fcst_phy2m.",cyearmonth,".grib")))

    system2("ncl_convert2nc",
            c(paste0("/home/femeunier/Documents/projects/Congo.ED2/outputs/fcst_phy2m.",
                     cyearmonth,".grib")))
  }
}
