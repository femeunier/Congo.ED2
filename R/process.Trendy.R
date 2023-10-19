process.Trendy <- function(Biomass.Trendy,NPP.trendy,target,params){

  start.date.int <- target[1]
  end.date.int <- target[2]

  woody.ratio.of.npp <- params[1]

  # AGB change
  cdf <-  Biomass.Trendy %>%
    group_by(model) %>%
    mutate(dist.t.init = (year - start.date.int)**2,
           dist.t.end = (year - end.date.int)**2) %>%
    filter(year >= year[which.min(dist.t.init)], year <=  year[which.min(dist.t.end)]) %>%
    mutate(timing = case_when(year == min(year) ~ "init",
                              year == max(year) ~ "end",
                              TRUE ~ "inter"))

  year.target <- unique(cdf$year)

  # NPP
  cdf.npp <- NPP.trendy %>%
    group_by(model) %>%
    dplyr::filter(year %in% year.target) %>%
    mutate(timing = case_when(year == min(year) ~ "init",
                              year == max(year) ~ "end",
                              TRUE ~ "inter")) %>%
    arrange(model,year) %>%
    mutate(npp = 86400*365*npp*10) # kgC/m²/s >> MgC/ha/yr


  cdf.npp.sum <- cdf.npp %>%
    dplyr::select(lon,lat,year,timing,model,npp) %>%
    group_by(lon,lat,year,model) %>%
    summarise(Growth = mean(npp)*woody.ratio.of.npp,   #nppwood
              .groups = "keep")


  cdf.sum <- cdf %>%
    dplyr::select(lon,lat,year,timing,model,year,cAGB) %>%
    mutate(cAGB = 10*cAGB) %>%  # kgC/m²/yr >> MgC/ha/yr
    group_by(lon,lat,model) %>%
    mutate(sink.corrected = c(NA,diff(cAGB)))

  cdf.sum.npp <- cdf.sum %>%
    left_join(cdf.npp.sum,
              by = c("model","lon","lat","year")) %>%
    mutate(Mort = Growth - sink.corrected)

  return(cdf.sum.npp)


}
