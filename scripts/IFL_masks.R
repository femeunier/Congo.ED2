rm(list = ls())

################################################################################
# IFL

IFL <- readOGR(dsn = "~/Downloads/IFL/", layer = "ifl_2020")
IFL$type = 1
N = 1
r <- raster(ncol=720*N, nrow=360*N)
extent(r) <- extent(-180,
                    180,
                    -90,90)

rp <- rasterize(IFL, r, 'type',fun = mean)
plot(rp)

rp.df <- as.data.frame(rp,xy = TRUE) %>%
  rename(lon = x,
         lat = y,
         is.undisturbed = layer) %>%
  mutate(is.undisturbed = case_when(is.na(is.undisturbed) ~ 0,
                                    is.undisturbed < 0.5 ~ 0,
                                    is.undisturbed >= 0.5 ~1))

saveRDS(rp.df,
        "./outputs/ILF2020.df")
