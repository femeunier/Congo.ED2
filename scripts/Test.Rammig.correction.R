rm(list = ls())

library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(tidyr)
library(data.table)
library(scales)
library(TrENDY.analyses)
library(CongoAS)
library(raster)

carbon_content 	<- 0.456			# Martin et al. 2018 Nature Geoscience
woody.ratio.of.npp <- 0.3     # Malhi et al. 2011 (From Koch Suppl. data Table S3), varies between 0.3 and 0.5

#####################################################################################################
# Data

networks <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/outputs/Afritron+Rainfor+Asia+Sullivan.RDS") %>%
  slice_head(n = 10) %>%
  mutate(region = as.character(coord2region.point(lon = Lon,
                                                  lat = Lat))) %>%
  mutate(timing = factor(case_when(int_int <= 1990 ~ "Pre",
                                   TRUE ~ "Post"),
                         levels = c("Pre","Post")))

table(networks$region)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_point(data = networks,
            aes(x = Lon, y = Lat, color = region),na.rm = TRUE, alpha = 1) +
  geom_polygon(data = CongoAS::all.basins ,
               aes(x = long, y = lat, group = group), color = "black", linewidth = 1.5,
               fill=NA) +
  geom_sf(fill = NA) +
  labs(x = "",y = "") +
  coord_sf(ylim = c(-20, 15),
           xlim = c(-100, 140),
           expand = FALSE) +
  theme_bw()



ggplot(data = networks) +
  geom_boxplot(aes(x = Continent, y = AGBnetchange.ha.yr, group = region, fill = region)) +
  theme_bw()

networks.sum <- networks %>%
  group_by(region) %>%
  summarise(n = n(),
            mean.x = mean(AGB_ini.ha,na.rm = TRUE),
            var.x = var(AGB_ini.ha, na.rm = TRUE),
            .groups = "keep")


# Test with a model

Trendy.model <- readRDS("/home/femeunier/Documents/projects/TrENDY.analyses/outputs/Trendy.pantropical.S2.cAGB.ORCHIDEE.v11.RDS") %>%
  dplyr::select(-c(scenario,model)) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  mutate(cAGB = cVeg - cRoot)

Trendy.model.pos <- Trendy.model %>%
  filter(year == min(year)) %>%
  mutate(ID = 1:n()) %>%
  dplyr::select(ID,lon,lat)

Trendy.model.pos.raster <- rasterFromXYZ(Trendy.model.pos[,c("lon","lat","ID")])
sp = SpatialPoints(networks[,c("Lon","Lat")])
crs(sp) <- crs(Trendy.model.pos.raster) <- "+proj=longlat +ellps=WGS84 +no_defs"

networks <- networks %>%
  ungroup() %>%
  mutate(gridcell = raster::extract(Trendy.model.pos.raster,sp))

cov.mod <- function(x){

  x <- x[!is.na(x)]
  tot <- 0
  for (i in 1:length(x)){
    sum = sum(1/2*(x - x[i])**2)
    tot = tot + sum
  }

  return(tot/(length(x)**2))

}


networks.gridcell <- networks %>%
  group_by(gridcell) %>%
  summarise(n = n(),
            var.eps = cov.mod(AGB_ini.ha),
            region = region[1],
            .groups = "keep") %>%
  left_join(networks.sum %>% rename(n.region = n),
            by = "region") %>%
  mutate(var.x.corr = case_when(var.eps > var.x ~ var.x,
                                TRUE ~ var.x - var.eps)) %>%   # Check with Anja Rammig
  mutate(rmax = sqrt(var.x.corr)/sqrt(var.x))

