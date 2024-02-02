rm(list = ls())

library(dplyr)
library(TrENDY.analyses)
library(raster)

# files <- c("GPP.products.Amazon.ILF.RDS",
#            "GPP.products.Congo.ILF.RDS",
#            "Amazon.mean.JRA.historical.IFL.RDS",
#            "Congo.mean.JRA.historical.IFL.RDS")
#
# for (cfile in files){
#   system2("scp",
#           c(paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }

################################################################################

grid <- readRDS("./data/GPP/monthly/SIF.GPP.RDS") %>%
  rename(daily.GPP = value) %>%
  filter(year == year[1],
         month == month[1]) %>%
  dplyr::select(lon,lat,daily.GPP)

rast <- rasterFromXYZ((grid %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","daily.GPP")))[,c("lon","lat","daily.GPP")])

years <- 1992:2023

Trendy.data <-  bind_rows(readRDS("./outputs/Amazon.mean.JRA.historical.IFL.RDS") %>%
                            mutate(basin = "Amazon"),
                          readRDS("./outputs/Congo.mean.JRA.historical.IFL.RDS") %>%
                            mutate(basin = "Congo")) %>%
  dplyr::select(-c(xgb.model,model.lon.lat)) %>%
  filter(year %in% years,
         var == "gpp")

models <- sort(unique(Trendy.data$model))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

Trendy.data.rspld <- data.frame()
for (cmodel in models){
  print(cmodel)

  ctrendy.data <- Trendy.data %>%
    filter(model == cmodel)

  ctrendy.data.rspld <- resample.df.all.col(bigdf = ctrendy.data %>%
                                              dplyr::select(lat,lon,year,month,model,pred),

                                            raster2resample = rast,
                                            var.names = c("pred"),
                                            res = 0.00001) %>%
    filter(!is.na(pred))

  Trendy.data.rspld <- bind_rows(Trendy.data.rspld,
                                 ctrendy.data.rspld)

}

saveRDS(Trendy.data.rspld,
        "./outputs/Trendy.data.rspld.pred.RDS")


# ################################################################################
#
# rm(list = ls())
#
# library(dplyr)
# library(TrENDY.analyses)
# library(raster)
#
# files <- c("GPP.products.Amazon.ILF.RDS",
#            "GPP.products.Congo.ILF.RDS",
#            "Amazon.mean.JRA.historical.IFL.RDS",
#            "Congo.mean.JRA.historical.IFL.RDS")
#
# for (cfile in files){
#   system2("scp",
#           c(paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }
#
# ################################################################################
#
# grid <- readRDS("./data/GPP/monthly/SIF.GPP.RDS") %>%
#   rename(daily.GPP = value) %>%
#   filter(year == year[1],
#          month == month[1]) %>%
#   dplyr::select(lon,lat,daily.GPP)
#
# rast <- rasterFromXYZ((grid %>%
#                          ungroup() %>%
#                          dplyr::select(c("lat","lon","daily.GPP")))[,c("lon","lat","daily.GPP")])
#
# years <- 1992:2022
#
# Trendy.data <-  bind_rows(readRDS("./outputs/Amazon.mean.JRA.historical.IFL.RDS") %>%
#                             mutate(basin = "Amazon"),
#                           readRDS("./outputs/Congo.mean.JRA.historical.IFL.RDS") %>%
#                             mutate(basin = "Congo")) %>%
#   dplyr::select(-c(xgb.model,model.lon.lat)) %>%
#   filter(year %in% years,
#          var == "gpp")
#
# models <- sort(unique(Trendy.data$model))
#
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# Trendy.data.rspld <- data.frame()
# for (cmodel in models){
#   print(cmodel)
#
#   ctrendy.data <- Trendy.data %>%
#     filter(model == cmodel)
#
#   ctrendy.data.rspld <- resample.df.all.col(bigdf = ctrendy.data %>%
#                                               dplyr::select(lat,lon,year,month,model,obs),
#
#                                             raster2resample = rast,
#                                             var.names = c("obs"),
#                                             res = 0.00001) %>%
#     filter(!is.na(obs))
#
#   Trendy.data.rspld <- bind_rows(Trendy.data.rspld,
#                                  ctrendy.data.rspld)
#
# }
#
# saveRDS(Trendy.data.rspld,
#         "./outputs/Trendy.data.rspld.RDS")
#

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/resample.trendy.ILF.GPPproducts.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
