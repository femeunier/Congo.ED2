rm(list = ls())

library(TrENDY.analyses)
library(ggthemes)
library(sf)
library(rgeos)
library(raster)

ILF.df  <- readRDS("./outputs/ILF2020.df")

cfile <- paste0("./data/OCO2/z_cams_l_cams55_202309_FT23r3_ra_sfc_mm_co2_flux.nc")
nc <- nc_open(cfile)
lats <- ncvar_get(nc,"latitude")
lons <- ncvar_get(nc,"longitude")
nc_close(nc)
Grid <- expand.grid(lat = lats,
                    lon = lons,
                    value = 1)

grid <- rasterFromXYZ((Grid %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","value")))[,c("lon","lat","value")])

test <- Grid %>%
  filter(lon <= -30, lon >= -90,
         lat <= 10, lat >= -25)

cILF <- resample.df.all.col(bigdf = ILF.df %>%
                              mutate(is.undisturbed = case_when(is.na(is.undisturbed) ~ 0,
                                                                TRUE ~ is.undisturbed)) %>%
                              dplyr::select(lat,lon,is.undisturbed) %>%
                              mutate(model = "ILF"),

                            raster2resample = grid,
                            var.names = c("is.undisturbed"),
                            res = 0.00001) %>%
  dplyr::select(lon,lat,is.undisturbed) %>%
  mutate(is.undisturbed.factor = case_when(is.na(is.undisturbed) ~ 0,
                                           is.undisturbed < 0.5 ~ 0,
                                           is.undisturbed >= 0.5 ~1))


Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")
Amazon <- as_Spatial(Amazon.shp)

sp <- SpatialPoints(test[,c("lon","lat")])
out = as.data.frame(gIntersection(sp,Amazon))

all.coord.ILF <-
  data.frame(lon = round(out$x,digits = 2),
             lat = round(out$y,digits = 2)) %>%
  left_join(cILF %>%
              mutate(lon = round(lon,digits = 2),
                     lat = round(lat,digits = 2)),
            by = c("lat","lon")) %>%
  filter(is.undisturbed.factor == 1) %>%
  mutate(lon.lat =
           paste0(lon,".",lat))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_raster(data = ILF.df,
              aes(x = lon, y = lat,
                  fill = as.factor(is.undisturbed.factor)), alpha = 1,
              linewidth = 0.5,
              show.legend = FALSE) +


  geom_point(data = all.coord.ILF ,
             aes(x = lon, y = lat), shape = "+", color = "red") +

  geom_sf(data = world,
          fill = NA) +
  geom_sf(data = Amazon.shp,fill = NA, color = "red", fill = NA) +
  # # geom_sf(data = Congo.shp,fill = NA, color = "red", fill = NA) +

  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-100, 150),
           ylim = c(-25, 25)) +
  labs(x = "",y = "") +
  # facet_wrap(~ model) +
  theme_map() +
  labs(fill = "") +
  scale_fill_manual(values = c("white","darkgrey")) +
  theme(text = element_text(size = 20),
        legend.position = "top")

saveRDS(all.coord.ILF,
        "./outputs/coord.ILF.OCO2.RD2")
