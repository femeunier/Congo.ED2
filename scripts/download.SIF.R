rm(list = ls())

library(R.utils)
library(raster)
library(ggplot2)
library(dplyr)
library(maptools)
library(akima)
library(RColorBrewer)
library(pracma)

dir <- "/home/femeunier/Documents/projects/SIF.data/data/"
dir <- "/home/femeunier/Downloads/"

biomes <- readRDS("./outputs/biome.CRUJRA.1901.2022.AI.RDS") %>%
  filter(model == unique(model)[14])
raster <- rasterFromXYZ(biomes %>%
                           dplyr::select(c(lon,lat,MAP)))

years <- 2000:2022
months <- 1:12

SF <- 0.01/1000*1000/365*12        # Scaling factor

first <- TRUE

# iyear <- imonth <- 1

e <- extent(-180,180,-25,25)

all.df <- data.frame()
for (iyear in seq(1,length(years))){
  for (imonth in seq(1,length(months))){
    year <- years[iyear]
    month <- months[imonth]

    print(paste(year,"-",month))

    file.name <- paste0("GOSIF_GPP_",year,".M",sprintf("%02d",month),"_Mean.tif")
    file <- file.path(dir,file.name)

    gz.name <- paste0(file.name,".gz")
    tmp.f <- file.path(dir,gz.name)
    f <- paste0('http://data.globalecology.unh.edu/data/GOSIF-GPP_v2/Monthly/Mean/',gz.name)

    if (!file.exists(tmp.f)) {

      test <- tryCatch(download.file(f, tmp.f, mode = 'wb'),
                       error = function(e) NULL)

      if(is.null(test)){
        next()
      }

    }

    if (file.exists(file.path(dir,paste0(file.name,".tmp")))){
      t <- file.remove(file.path(dir,paste0(file.name,".tmp")))
    }

    if (!file.exists(file.path(dir,file.name))){
      gunzip(tmp.f, remove = FALSE)
    }


    craster <- raster(file.path(dir,file.name))
    craster[craster >= 32766] <- NA_real_       # Water bodies, oceans
    craster <- craster*SF

    dumb <- file.remove(file)

    all.raster.cr <- (crop(craster,e))
    cdf <- as.data.frame(all.raster.cr, xy = TRUE) %>%
      rename(lon = x,
             lat = y,
             value = starts_with("GOSIF"))

    ccdf <-aggregate(raster(suppressWarnings(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                                                   data = cdf["value"],
                                                                   tolerance = 0.01))),
                     10)

    all.df <- bind_rows(all.df,
                        as.data.frame(ccdf,
                                      xy = TRUE) %>%
                          rename(lon = x,
                                 lat = y) %>%
                          mutate(year = years[iyear],
                                 month = months[imonth]))



  }
}

# plot(craster)
# plot(crop(craster,extent(8,42,-12,12)))

data(wrld_simpl)
mymap <- fortify(wrld_simpl)

cols <- c("white",fliplr(t(rev(brewer.pal(9, 'YlGn')))))

ggplot() +

  geom_tile(data = all.df %>%
              filter(month == month[1],
                     year == year[1]),
            aes(x = lon, y = lat, fill = value), alpha = 1) +
  geom_map(data = mymap,
           map = mymap,
           aes(x = long, y = lat, map_id = id), fill = NA, color = "black") +
  # geom_point(aes(x = (e@xmin+e@xmax)/2,
  #                y = (e@ymin+e@ymax)/2),
  #           color = "red", fill = NA) +
  # scale_fill_gradientn(colours = cols) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  # scale_x_continuous(limits = c(-10, 45),
  #                    expand = c(0, 0)) +
  # scale_y_continuous(limits = c(-15,10),
  #                    expand = c(0, 0)) +
  labs(x = "Lon", y = "Lat", fill = "SIF") +
  coord_equal() +
  theme_void()

saveRDS(all.df,
        "./data/GPP/monthly/SIF.GPP.RDS")


