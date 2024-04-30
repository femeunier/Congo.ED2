coord2continent <- function(lon,lat){

  continent = rep(NA_character_,length(lon))
  continent[lon <= -20 & lon >= -180] <- "America"
  continent[lon > -20 & lon <= 60] <- "Africa"
  continent[lon > 60] <- "Australasia"
  # continent[lon > 90 & lon < 175 & lat > - 10] <- "Asia"
  # continent[ lon > 100 & lon < 175 & lat <= -10] <- "Australia"
  # continent <- factor(continent,
  #                     levels = c("America","Africa","Asia","Australia"))

  continent <- factor(continent,
                      levels = c("America","Africa","Australasia"))
  return(continent)

}
