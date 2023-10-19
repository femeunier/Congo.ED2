process.Trendy.sum2 <- function(Biomass.Trendy,GPP.NPP.trendy,target){

  start.date.int <- target[1]
  end.date.int <- target[2]

  # AGB change
  cdf <-  Biomass.Trendy %>%
    group_by(model) %>%
    mutate(dist.t.init = (year - start.date.int)**2,
           dist.t.end = (year - end.date.int)**2) %>%
    filter(dist.t.init == min(dist.t.init) | dist.t.end <=  min(dist.t.end)) %>%
    mutate(timing = case_when(year == min(year) ~ "init",
                              year == max(year) ~ "end",
                              TRUE ~ "inter"))

  year.target <- unique(cdf$year)

  # NPP
  cdf.gpp.npp <- GPP.NPP.trendy %>%
    group_by(model) %>%
    dplyr::filter(year %in% year.target) %>%
    mutate(timing = case_when(year == min(year) ~ "init",
                              year == max(year) ~ "end",
                              TRUE ~ "inter")) %>%
    arrange(model,year) %>%
    mutate(npp = 86400*365*npp*10,
           gpp = 86400*365*gpp*10) # kgC/m²/s >> MgC/ha/yr


  cdf.gpp.npp.sum <- cdf.gpp.npp %>%
    dplyr::select(lon,lat,timing,model,gpp,npp) %>%
    group_by(lon,lat,model) %>%
    summarise(GPP.m = mean(gpp),
              NPP.m = mean(npp),
              .groups = "keep")



  cdf.sum <- cdf %>%
    dplyr::select(lon,lat,timing,model,year,cAGB) %>%
    mutate(cAGB = 10*cAGB) %>%  # kgC/m²/yr >> MgC/ha/yr
    pivot_wider(names_from = c(timing),
                values_from = c(year,cAGB)) %>%
    mutate(delta_t = year_end - year_init,
           delta_AGB = cAGB_end - cAGB_init) %>%
    mutate(sink.corrected = delta_AGB/delta_t)

  cdf.sum.npp <- cdf.sum %>%
    left_join(cdf.gpp.npp.sum,
              by = c("model","lon","lat"))

  return(cdf.sum.npp)


}
