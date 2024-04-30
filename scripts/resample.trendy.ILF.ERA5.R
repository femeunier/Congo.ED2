rm(list = ls())

library(dplyr)
library(TrENDY.analyses)
library(raster)
library(ggplot2)

files <- c("GPP.products.Amazon.ILF.RDS",
           "Amazon.mean.ERA5.IFL.RDS")

# for (cfile in files){
#   system2("rsync",
#           c("-avz",
#             paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
#             "./outputs/"))
# }

################################################################################

grid <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
  rename(daily.GPP = VPD) %>%
  filter(year == year[1],
         month == month[1]) %>%
  dplyr::select(lon,lat,daily.GPP)

rast <- rasterFromXYZ((grid %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","daily.GPP")))[,c("lon","lat","daily.GPP")])

years <- 1992:2024

Trendy.data <-  bind_rows(readRDS("./outputs/Amazon.mean.ERA5.IFL.RDS") %>%
                            mutate(basin = "Amazon")) %>%
  dplyr::select(-c(xgb.model,model.lon.lat)) %>%
  filter(year %in% years,
         var == "gpp")

Trendy.data.sum <- Trendy.data %>%
  group_by(var,year,basin,month,model) %>%
  summarise(pred.m = mean(pred),
            obs.m = mean(obs),
            .groups = "keep")

Trendy.data.sum.sum <- Trendy.data.sum %>%
  group_by(var,year,basin,month) %>%
  summarise(pred.m = mean(pred.m),
            obs.m = mean(obs.m),
            .groups = "keep")

ggplot(data = Trendy.data.sum.sum) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = pred.m)) +
  geom_point(aes(x = year + (month - 1/2)/12,
                 y = obs.m ), linetype = 2) +
  theme_bw() +
  scale_x_continuous(limits = c(2000,2025))

models <- sort(unique(Trendy.data$model))

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

Trendy.data.rspld <- data.frame()
for (cmodel in models){

  ctrendy.data <- Trendy.data %>%
    filter(model == cmodel) %>%
    dplyr::select(lat,lon,year,month,model,pred)

  ctrendy.data.rspld <- data.frame()

  for (cyear in years){
    print(paste0(cmodel," - ",cyear))
    ctrendy.data.rspld <- bind_rows(ctrendy.data.rspld,
                                    resample.df.all.col(bigdf = ctrendy.data %>%
                                                          filter(year == cyear),

                                                        raster2resample = rast,
                                                        var.names = c("pred"),
                                                        res = 0.00001) %>%
                                      filter(!is.na(pred)))
  }



  Trendy.data.rspld <- bind_rows(Trendy.data.rspld,
                                 ctrendy.data.rspld)

}

saveRDS(Trendy.data.rspld,
        "./outputs/Trendy.data.rspld.ERA5.pred.RDS")


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
