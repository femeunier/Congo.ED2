process.GPP.sum <- function(GPP.observed,
                        target){

  start.date <- target[1]
  end.date <- target[2]

  # AGB change
  cdf <-  GPP.observed %>%
    group_by(model) %>%
    mutate(dist.t.init = (time - start.date)**2,
           dist.t.end = (time - end.date)**2) %>%
    filter(dist.t.init == min(dist.t.init) | dist.t.end <=  min(dist.t.end)) %>%
    mutate(timing = case_when(time >= min(time) & time <= max(time) ~ "in",
                              TRUE ~ "out"))


  cdf.sum <- cdf %>%
    dplyr::select(lon,lat,timing,year,model,month,daily.GPP) %>%
    mutate(GPP = 10*daily.GPP*365/1000) %>%  # gC/m²/d >> MgC/ha/yr
    filter(timing == "in") %>%
    group_by(model) %>%
    summarise(GPP = mean(GPP),
              .groups = "keep")

  return(cdf.sum)


}
