coord2continent <- function(lon,lat){

  continent = rep(NA_character_,length(lon))
  continent[lon <= -20 & lon >= -85] <- "America"
  continent[lon > -20 & lon <= 55 ] <- "Africa"
  continent[lon > 90 & lon < 175 & lat > - 10] <- "Asia"
  continent[ lon > 100 & lon < 175 & lat <= -10] <- "Australia"

  continent <- factor(continent,
                      levels = c("America","Africa","Asia","Australia"))
  return(continent)

}
