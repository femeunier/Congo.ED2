rm(list = ls())

library(stringr)

dir <- "/home/femeunier/Documents/projects/Congo.ED2/data/LUH/scenario/AIM/"
list_files <- list.files(dir)

lats <- as.numeric(unlist(qdapRegex::ex_between(list_files, "lat", "lon")))
lons <- as.numeric(unlist(qdapRegex::ex_between(list_files, "lon", ".lu")))

df <- data.frame(lat = lats,
                 lon = lons)

df.select <- df %>%
  filter(lat >= -20, lat <= 15,
         lon >= -15, lon <= 50)

files2keep <- paste0("lat",df.select$lat,"lon",df.select$lon,".lu")
files2delete <- list_files[!(list_files %in% files2keep)]

write(file.path(dir,files2delete),"./files2delete.txt",
      append = FALSE)

# xargs -a ../../../files2delete.txt rm
