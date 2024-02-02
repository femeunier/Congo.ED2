rm(list = ls())

library(raster)

years <- 1992:2020
months <- 1:12
SF <- 0.1

e <- extent(-180,180,-25,25)

all.df <- data.frame()
for (cyear in years){
  for (cmonth in months){

    print(paste0(cyear," - ",cmonth))

    cfile <- file.path("~/Downloads/SIF_GPP/",cyear,"Month",paste0("GPP_v21_",cyear,"_",sprintf("%02d",cmonth),".tif"))

    if (!file.exists(cfile)){
      next()
    }
    r <- raster(cfile)

    Ndays <- as.numeric(lubridate::days_in_month(as.Date(paste0(cyear,"/",sprintf("%02d",cmonth),"/01"))))
    craster <- r*SF/Ndays

    all.raster.cr <- (crop(craster,e))
    cdf <- as.data.frame(all.raster.cr, xy = TRUE) %>%
      rename(lon = x,
             lat = y,
             value = starts_with("GPP"))

    ccdf <- raster::aggregate(raster(suppressWarnings(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                                                             data = cdf["value"],
                                                                     tolerance = 0.001))),
                      10)

    all.df <- bind_rows(all.df,
                        as.data.frame(ccdf,
                                      xy = TRUE) %>%
                          rename(lon = x,
                                 lat = y) %>%
                          mutate(year = cyear,
                                 month = cmonth))

  }
}

saveRDS(all.df,
        "./data/GPP/monthly/SIF.GPP2.RDS")
