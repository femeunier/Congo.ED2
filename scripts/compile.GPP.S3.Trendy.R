rm(list = ls())

library(dplyr)
library(tidyr)
library(Congo.ED2)
library(TrENDY.analyses)

# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.*.S3.gpp.pantropical.v11.RDS",
#               "./outputs/"))

biomes <- readRDS("./outputs/biome.JRA.1901.2023.AI.RDS")

model.names <- get.model.names.TRENDY(version = "v11")

all.data <- data.frame()
for (cmodel in model.names){

  print(cmodel)

  cfile <- paste0("./outputs/",
                  "Trendy.",cmodel,".S3.gpp.pantropical.v11.RDS")
  if (!file.exists(cfile)){
    next()
  }

  cdata <- readRDS(cfile)
  cbiomes <- biomes %>%
    filter(model == cmodel)

  cdata.biome <- cdata %>%
    left_join(cbiomes %>%
                dplyr::select(lat,lon,biome),
              by = c("lat","lon")) %>%
    dplyr::select(lon,lat,year,month,value,biome) %>%
    filter(year >= 1950) %>%
    mutate(model = cmodel,
           continent = coord2continent(lon,lat))

  cdata.biome.sum <- cdata.biome %>%
    group_by(year,month,continent,biome,model) %>%
    summarise(value = mean(value,na.rm = TRUE),
              .groups = "keep")

  all.data <- bind_rows(all.data,
                        cdata.biome.sum)

}

saveRDS(all.data,
        "./outputs/GPP.S3.Trendy.biome.RDS")

