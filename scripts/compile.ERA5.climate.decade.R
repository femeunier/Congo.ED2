rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)

decade <- 3

convert.ERA5.decade(decade)

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/compile.ERA5.climate.decade.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

