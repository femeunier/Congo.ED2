rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(tidyr)

vars <- c("tprat","dswrf","dlwrf")
num <- c("061","204","205")
init.months <- c("01")
end.months <- c("12")

# https://data.rda.ucar.edu/ds628.0/fcst_phy2m/1958/fcst_phy2m.061_tprat.reg_tl319.1958010100_1958033121

for (cyear in seq(1958,2023)){
  for (imonth in seq(1,length(init.months))){
    for (ivar in seq(1,length(vars))){

      cfile <- paste0("fcst_phy2m.",
                      num[ivar],"_",vars[ivar],".reg_tl319.",
                      cyear,init.months[imonth],"_",cyear,end.months[imonth])

      system2("wget",
              c("-N",
                paste0("https://data.rda.ucar.edu/ds628.1/fcst_phy2m/",cyear,"/",
                       cfile)))

      system2("mv",
              c(cfile,
                paste0("./outputs/",cfile,".grib")))
      system2("ncl_convert2nc",
              c(paste0("./outputs/",cfile,".grib")))
      system2("mv",
              c(paste0(cfile,".nc"),
                paste0("./outputs/")))


      system2("rm",
              paste0("./fcst_phy2m.*"))
      system2("rm",
              paste0("./outputs/",cfile,".grib"))

    }
  }
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/download.and.convert.JRA_historical.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/download.and.convert.JRAsubmonthly_historical.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
