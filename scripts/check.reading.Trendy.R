rm(list = ls())

library(ncdf4)
library(dplyr)

cmodel <- "CLASSIC"
# A <- readRDS(paste0("./outputs/Trendy.",cmodel,".S2.gpp.pantropical.v11.RDS")
B <- readRDS(paste0("./outputs/Trendy.",cmodel,".S2.CC.pantropical.v11.RDS"))
ncfile <- paste0("/data/gent/vo/000/gvo00074/felicien/TrENDYv11/",cmodel,"_S2_gpp.nc")
nc <- nc_open(ncfile)

lats <- ncvar_get(nc,"latitude")
lons <- ncvar_get(nc,"longitude")
lons[lons>180] <- lons[lons>180] -360
times <- ncvar_get(nc,"time")

gpp <- ncvar_get(nc,"gpp")

nc_close(nc)

clat = 0 ; clon = -60
pos.lat <- which.min((lats-clat)**2)
pos.lon <- which.min((lons-clon)**2)
Nyears <- 201
pos.time <- (12*Nyears) + (1:12)

gpp[pos.lon,pos.lat,pos.time]



# df <- A %>%
#   ungroup() %>%
#   filter(lat == lats[pos.lat],
#          lon == lons[pos.lon],
#          year %in% (1700 + Nyears)) %>%
#   # dplyr::select(year,month,value) %>%
#   arrange(year,month)


df <- B %>%
  ungroup() %>%
  filter(lat == lats[pos.lat],
         lon == lons[pos.lon],
         year %in% (1700 + Nyears)) %>%
  # dplyr::select(year,month,value) %>%
  arrange(year,month)
