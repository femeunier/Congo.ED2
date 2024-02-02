rm(list = ls())

library(raster)
library(reshape2)
library(dplyr)

e <- extent(-90,165,-23.5,23.5)

# https://figshare.com/articles/dataset/Monthly_GPP_at_0_5_degree/5048011?backTo=/collections/A_global_moderate_resolution_dataset_of_gross_primary_production_of_vegetation_for_2000-2016/3789814
# Zhang based on Modis + NCEP

all.df.GPP <- data.frame()
all.years = 2000:2016
months <- 1:12

dir <- "/home/femeunier/Documents/projects/Congo.ED2/data/GPP/monthly/"

for (cyear in all.years){
  print(cyear)
  for (cmonth in months){

    cfile <- file.path(dir,paste0("GPP.VPM.",cyear,".M",sprintf("%02d",cmonth),".v20.HD.tif"))
    craster <- raster(cfile)

    NofDays <- as.numeric(lubridate::days_in_month(as.Date(paste0("01/",sprintf("%02d",cmonth),"/",cyear))))
    craster.cropped <- raster::crop(craster, e)
    cdf.GPP <- as.data.frame(craster.cropped,xy = TRUE) %>%
      rename_at(vars(starts_with('GPP')), function(x) str_sub(x,1,3)) %>%

      rename(lon = x, lat = y) %>%
      mutate(daily.GPP = GPP/NofDays) %>%
      filter(!is.na(daily.GPP))

    all.df.GPP <- bind_rows(list(all.df.GPP,
                                 cdf.GPP %>% mutate(year = cyear,
                                                    month = cmonth)))

  }
}

df.GPP.sum <- all.df.GPP %>%
  group_by(lat,lon) %>%
  summarise(GPP.m = mean(daily.GPP)/1000*365,
            .groups = "keep") %>%
    mutate(continent = case_when(lon <= -35 ~ "Amazon",
                                 lon <= 45 ~ "Africa",
                                 TRUE ~ "Australasia"))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = df.GPP.sum) +
  geom_raster(aes(x=lon, y = lat, fill = GPP.m)) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90,150),ylim = c(-1, 1)*23.5, expand = FALSE) +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = NA) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggplot(data = df.GPP.sum) +
  geom_density(aes(x = GPP.m, fill = continent), alpha = 0.5) +
  theme_bw()

saveRDS(all.df.GPP,
        "./data/GPP/monthly/all.df.GPP.RDS")

################################################################################
# https://daac.ornl.gov/VEGETATION/guides/Global_Monthly_GPP.html
# Madhani, based on fluxnet
rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(raster)
library(lubridate)

ncfile <- "/home/femeunier/Documents/projects/Congo.ED2/data/GPP/gross_primary_productivity_monthly_1982-2016.nc4"
nc <- nc_open(ncfile)

all.lats <- ncvar_get(nc,"lat"); pos.lats <- which(abs(all.lats) < 23.5) ; lats <- all.lats[pos.lats]
all.lons <- ncvar_get(nc,"lon"); pos.lons <- which(all.lons > - 90 & all.lons < 150) ; lons <- all.lons[pos.lons]
all.times <- ncvar_get(nc,"time")

df.all <- data.frame()

for (i in seq(1,length(all.times))){

  print(i/length(all.times))

  GPP <- ncvar_get(nc,"GPP",start = c(min(pos.lons),min(pos.lats),i),count = c(length(pos.lons),length(pos.lats),1))

  df.GPP <- melt(GPP) %>%
    mutate(lon = lons[Var1],
           lat = lats[Var2]) %>%
    dplyr::select(-c(Var1,Var2))

  craster <- raster(SpatialPixelsDataFrame(points = df.GPP[c("lon","lat")],
                                           data = df.GPP["value"],
                                           tolerance = 0.5))
  craster.agg <- aggregate(craster,0.5/res(craster))
  df.craster.agg <- as.data.frame(craster.agg,xy = TRUE) %>%
    rename(lon = x,
           lat = y) %>% filter(!is.na(value))

  df.all <- bind_rows(list(df.all,
                           df.craster.agg %>%
                             mutate(time = all.times[i])))


}

nc_close(nc)

df.all <- df.all %>%
  mutate(time = as.Date(time,origin = "1982-01-01 00:00:00"),
         year = year(time),
         month = month(time)) %>%
  dplyr::select(-time) %>%
  rename(daily.GPP = value)

saveRDS(df.all,
        "./data/GPP/monthly/all.df.GPP2.RDS")

################################################################################
rm(list = ls())

# https://www.cen.uni-hamburg.de/en/icdc/data/land/modis-primaryproduction.html


library(ncdf4)
library(reshape2)
library(dplyr)
library(raster)
library(lubridate)

all.years <- 2000:2018
main.dir <- "/home/femeunier/Documents/projects/Congo.ED2/data/GPP/MODIS/modis_gpp/global"

df.all.gpp <- data.frame()
for (cyear in all.years){

 print(cyear)
 cdir <- file.path(main.dir,cyear)
 all.files <- unique(paste0(gsub(".nc","",unique(tools::file_path_sans_ext(list.files(cdir)))),".nc"))

 for (cfile in all.files){

   nc <- nc_open(file.path(cdir,cfile))

   all.lats <- ncvar_get(nc,"lat"); pos.lats <- which(abs(all.lats) < 23.5) ; lats <- all.lats[pos.lats]
   all.lons <- ncvar_get(nc,"lon"); pos.lons <- which(all.lons > - 90 & all.lons < 150) ; lons <- all.lons[pos.lons]
   time <- as.Date(ncvar_get(nc,"time"),
                   origin = "2000-01-01")

   gpp <- ncvar_get(nc,"gpp",
                    start = c(min(pos.lons),min(pos.lats),1),count = c(length(pos.lons),length(pos.lats),1))

   df.GPP <- melt(gpp) %>%
     mutate(lon = lons[Var1],
            lat = lats[Var2]) %>%
     dplyr::select(-c(Var1,Var2)) %>%
     mutate(year = lubridate::year(time),
            month = lubridate::month(time)) %>%
     filter(!is.na(value)) %>%
     mutate(daily.GPP = value)

   df.all.gpp <- bind_rows(list(df.all.gpp,
                                df.GPP))

   nc_close(nc)
 }
}

df.all.gpp.sum <- df.all.gpp %>%
  group_by(year,month,lat,lon) %>%
  summarise(daily.GPP = mean(daily.GPP*1000/8,na.rm = TRUE),
            .groups = "keep")

saveRDS(df.all.gpp.sum,
        "./data/GPP/monthly/all.df.GPP.MODIS.RDS")


# ##############################################################################
# rm(list = ls())
#
# library(dplyr)
# library(MODISTools)
#
# all.raws <- readRDS("./outputs/Afritron+Rainfor+Asia+Sullivan_raw.RDS")
# cfile <- "./data/GPP/df.MODIS.RDS"
#
# if (file.exists(cfile)) {
#   all.df.GPP <- readRDS(cfile)
# } else {
#   all.df.GPP <- data.frame()
# }
#
# all.coords <- all.raws %>%
#   dplyr::select(Lat,Lon) %>%
#   distinct()
#
# for (icoord in seq(1,nrow(all.coords))){
#
#   print(icoord/nrow(all.coords))
#   cdf <- all.raws %>%
#     filter(Lat == all.coords[["Lat"]][icoord] & Lon == all.coords[["Lon"]][icoord])
#
#   clon <- unique(cdf$Lon) ; clat <- unique(cdf$Lat)
#
#
#
#   for (itime in seq(1,nrow(cdf))){
#
#     start.date <- cdf$int_ini[itime] ; end.date <- cdf$int_fin[itime]
#
#     exists <- nrow(all.df.GPP %>%
#       filter(Lon == clon, Lat == clat, int_ini == start.date, int_fin == end.date)) > 0
#
#     if (exists) next()
#
#     if (end.date > 2000 & start.date < 2022){
#
#       subset <- mt_subset(product = "MOD17A2HGF",
#                           lat = clat,
#                           lon = clon,
#                           band = "Gpp_500m",   # PsnNet_500m
#                           start = as.Date((start.date-floor(start.date))*365,
#                                           origin = paste0(floor(start.date),"/01/01")),
#                           end =  as.Date((end.date-floor(end.date))*365,
#                                          origin = paste0(floor(end.date),"/01/01")),
#                           km_lr = 0,
#                           km_ab = 0,
#                           progress = FALSE)
#       obs.gpp <- mean(subset$value*0.0001,na.rm = TRUE)/8
#
#     } else {
#       obs.gpp = NA
#     }
#
#     all.df.GPP <- bind_rows(list(all.df.GPP,
#                                  data.frame(Lon = clon,
#                                             Lat = clat,
#                                             int_ini = start.date,
#                                             int_fin = end.date,
#                                             obs.GPP = obs.gpp)))
#
#   }
# }
#
# saveRDS(all.df.GPP %>%
#           mutate(daily.GPP = obs.GPP),"./data/GPP/df.MODIS.RDS")


################################################################################
# rm(list = ls())
#
# library(ncdf4)
# library(reshape2)
# library(dplyr)
#
# # https://www.cen.uni-hamburg.de/en/icdc/data/land/modis-primaryproduction.html
# ncfile <- "~/Downloads/MODIS-C006_MOD17A2H__GrossPrimaryProduction__8DAY__LPDAAC__GLOBAL_0.5degree__UHAM-ICDC__20001226__fv0.01.nc"
# nc <- nc_open(ncfile)
# GPP <- ncvar_get(nc,"gpp")
# NPP <- ncvar_get(nc,"net_photosynthesis")
#
# lats <- ncvar_get(nc,"lat")
# lons <- ncvar_get(nc,"lon")
# times <- ncvar_get(nc,"time")
# nc_close(nc)
#
# df.GPP <- melt(GPP) %>%
#   mutate(lon = lons[Var1],
#          lat = lats[Var2],
#          time = times) %>%
#   dplyr::select(-c(Var1,Var2)) %>%
#   filter(!is.na(value),
#          abs(lat) <= 23) %>%
#   mutate(continent = case_when(lon <= -35 ~ "Amazon",
#                                lon <= 45 ~ "Africa",
#                                TRUE ~ "Australasia"))
#
#
# world <- ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot(data = df.GPP) +
#   geom_raster(aes(x=lon, y = lat, fill = value*365/8)) +
#   geom_sf(data = world,fill = NA) +
#   coord_sf(ylim = c(-1, 1)*23.5, expand = FALSE) +
#   scale_fill_gradient2(low = "darkred",mid = "white",high = "darkgreen",na.value = NA) +
#   labs(x = "",y = "") +
#   theme_bw() +
#   theme(text = element_text(size = 20),
#         strip.background = element_blank(),
#         strip.text.x = element_blank())
#
# ggplot(data = df.GPP) +
#   geom_density(aes(x = value*365, fill = continent), alpha = 0.5) +
#   theme_bw()
#

################################################################################
# Fluxcom_CRU

rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(reshape2)

all.years = 1950:2017
months = 1:12

dir <- "/home/femeunier/Documents/projects/Congo.ED2/data/GPP/Fluxcom_CRU/"

df.all.GPP <- data.frame()
for (cyear in all.years){

  print(cyear)

    cfile <- file.path(dir,paste0("GPP.RS_METEO.FP-ALL.MLM-ALL.METEO-CRUJRA_v1.720_360.monthly.",cyear,".nc"))

    nc <- nc_open(cfile)

    all.lats <- ncvar_get(nc,"lat"); pos.lats <- which(abs(all.lats) < 23.5) ; lats <- all.lats[pos.lats]
    all.lons <- ncvar_get(nc,"lon"); pos.lons <- which(all.lons > - 90 & all.lons < 150) ; lons <- all.lons[pos.lons]

    gpp <- ncvar_get(nc,"GPP",
                     start = c(min(pos.lons),min(pos.lats),1),count = c(length(pos.lons),length(pos.lats),12))

    df.GPP <- melt(gpp) %>%
      mutate(lon = lons[Var1],
             lat = lats[Var2]) %>%
      rename(month = Var3) %>%
      dplyr::select(-c(Var1,Var2)) %>%
      filter(!is.na(value)) %>%
      mutate(daily.GPP = value)

    nc_close(nc)

    df.all.GPP <- bind_rows(df.all.GPP,
                            df.GPP %>%
                              mutate(year = cyear))
}


saveRDS(df.all.GPP,
        "./data/GPP/df.all.GPP3.RDS")

################################################################################
# https://researchdata.tuwien.ac.at/records/1k7aj-bdz35


rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)
library(raster)
library(TrENDY.analyses)
library(ggplot2)
library(tidyr)

ncfile <- "/home/femeunier/Downloads/VODCA2GPP_v1.nc"
nc <- nc_open(ncfile)

all.lats <- ncvar_get(nc,"lat")
pos.lats <- which(abs(all.lats) <= 25)
lats <- all.lats[pos.lats]
lons <- ncvar_get(nc,"lon")
all.times <- as.Date(ncvar_get(nc,"time"),
                     origin = "1858-11-17 00:00:00")
years <- unique(year(all.times))

biomes <- readRDS("./outputs/biome.CRUJRA.1901.2022.AI.RDS") %>%
  filter(model == unique(model)[14])
craster <- rasterFromXYZ(biomes %>%
                           dplyr::select(c(lon,lat,MAP)))

# df.all <- readRDS("./outputs/VOD.GPP.RDS")
df.all <- data.frame()
for (cyear in years){

  print(cyear)
  which.year <- which(year(all.times) == cyear)
  times <- all.times[which.year]

  GPP <- ncvar_get(nc,"GPP",
                   start = c(1,min(pos.lats),min(which.year)),
                   count = c(length(lons),length(pos.lats),length(which.year)))

  df.GPP <- melt(GPP) %>%
    mutate(lat = lats[Var2],
           lon = lons[Var1],
           time = times[Var3]) %>%
    dplyr::select(-c(Var1,Var2,Var3)) %>%
    mutate(month = month(time),
           year = year(time))

  df.GPP.sum <- df.GPP %>%
    group_by(year,month,lat,lon) %>%
    summarise(value = mean(value),
              .groups = "keep")

  cdf <- resample.df.all.col(bigdf = df.GPP.sum,

                              raster2resample = craster,
                              var.names = c("value"),
                              NULL)

  df.all <- bind_rows(df.all,
                      cdf %>%
                        filter(!is.na(value)))

  print(nrow(df.all)/1e6)
}

nc_close(nc)

saveRDS(df.all,
        "./data/GPP/monthly/VOD.GPP.RDS")
