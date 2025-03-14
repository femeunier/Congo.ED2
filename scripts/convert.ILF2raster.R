rm(list = ls())

library(sf)
library(ggthemes)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(rgeos)

IFL <- read_sf(dsn = "/home/femeunier/Documents/projects/Congo.ED2/data/IFL/",
                      layer = "ifl_2020")
IFL$type = 1
res = 0.1
r <- raster(ncol=360/res, nrow=180/res)
extent(r) <- extent(-180,
                    180,
                    -90,90)
r.crop <- crop(r,extent(-180,180,-23.25,23.25))
rp <- rasterize(IFL, r.crop,
                'type',
                fun = mean)

writeRaster(rp, './outputs/ILF.tif')

plot(rp)

rp.df <- as.data.frame(rp,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         is.undisturbed = layer) %>%
  mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
                                           is.undisturbed < 0.5 ~ 0,
                                           is.undisturbed >= 0.5 ~1))


world <- map_data("world")

ggplot() +
  geom_tile(data = rp.df %>%
              filter(is.undisturbed.factor == 1),
            aes(x = lon,y = lat, fill = is.undisturbed.factor)) +
  geom_map(
    data = world,map = world,aes(map_id = region),
    color = "lightgray", fill = NA, size = 0.1) +
  # geom_sf(data = CongoBasin,size = 0.5,color = "black",fill = NA) +
  labs(x = "",y = "") +
  coord_sf() +
  theme_void() +
  scale_x_continuous(limits = c(-120,165)) +
  scale_y_continuous(limits = c(-23,23)) +
  theme(legend.position = "none")


saveRDS(rp.df,
        "./outputs/ILF2020.df")

