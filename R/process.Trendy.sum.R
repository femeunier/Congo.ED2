process.Trendy.sum <- function(Biomass.Trendy,NPP.trendy,target,params){

  start.date.int <- target[1]
  end.date.int <- target[2]

  default.woody.ratio.of.npp <- params[[1]]
  model.df <- params[[2]]

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
  cdf.npp <- NPP.trendy %>%
    group_by(model) %>%
    dplyr::filter(year %in% year.target) %>%
    mutate(timing = case_when(year == min(year) ~ "init",
                              year == max(year) ~ "end",
                              TRUE ~ "inter")) %>%
    arrange(model,year) %>%
    mutate(npp = 86400*365*npp*10) # kgC/m²/s >> MgC/ha/yr


  cdf.npp.sum <- cdf.npp %>%
    dplyr::select(lon,lat,timing,model,npp) %>%
    group_by(lon,lat,model) %>%
    summarise(NPP.m = mean(npp),
              .groups = "keep") %>%
    mutate(default.woody.ratio.of.npp = default.woody.ratio.of.npp) %>%   #nppwood
    left_join(model.df,
              by = "model") %>%
    mutate(woody.ratio.of.npp = case_when(is.na(woody.ratio.of.npp) ~ default.woody.ratio.of.npp,
                                          TRUE ~ woody.ratio.of.npp)) %>%
    dplyr::select(-default.woody.ratio.of.npp) %>%
    mutate(Growth = NPP.m*woody.ratio.of.npp)



  cdf.sum <- cdf %>%
    dplyr::select(lon,lat,timing,model,year,cAGB) %>%
    mutate(cAGB = 10*cAGB) %>%  # kgC/m²/yr >> MgC/ha/yr
    pivot_wider(names_from = c(timing),
                values_from = c(year,cAGB)) %>%
    mutate(delta_t = year_end - year_init,
           delta_AGB = cAGB_end - cAGB_init) %>%
    mutate(sink.corrected = delta_AGB/delta_t)

  cdf.sum.npp <- cdf.sum %>%
    left_join(cdf.npp.sum,
              by = c("model","lon","lat")) %>%
    mutate(Mort = Growth - sink.corrected)

  return(cdf.sum.npp)


}
