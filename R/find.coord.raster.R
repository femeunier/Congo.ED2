find.coord.raster <- function(craster.df,target,Ngridcells = 1){

  clon <- target[1]
  clat <- target[2]

  model.lat.lons <-  craster.df %>%
    mutate(dist = sqrt((clat - lat)**2 + (clon - lon)**2)) %>%
    dplyr::filter(dist %in% sort(unique(dist))[1:Ngridcells]) %>%
    pull(model.lat.lon)

  return(model.lat.lons)


}
