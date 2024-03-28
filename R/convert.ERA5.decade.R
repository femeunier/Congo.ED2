convert.ERA5.decade <- function(decade,
                                dir_prefix = "/data/gent/vo/000/gvo00074/ED_common_data/met/Tropics/ERA5_Tropics",
                                OPpath_prefix = "./outputs/monthly.climate.pantropical.ERA5"){

  years2change <- 1940:1949 + (decade - 1)*10

  all.df.years <- data.frame()
  # all.df.years <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
  #   filter(!(year %in% years2change))
  # print(unique(all.df.years$year))
  # print(summary(all.df.years$lat))
  # print(summary(all.df.years$lon))

  vars <- c("t2m","ssrd","tp","strd","d2m")
  for (cyear in years2change){

    ncfile <- file.path(dir,paste0(prefix,"_",cyear,".nc"))

    if (!file.exists(ncfile)) next()

    nc <- nc_open(ncfile)
    lons <- ncvar_get(nc,"longitude")
    lats <- ncvar_get(nc,"latitude")
    times <- as.Date(ncvar_get(nc,"time")/24,
                     origin = "1900-01-01")
    months <- month(times)

    all.df <- data.frame()

    for (cmonth in sort(unique(months))){

      print(paste0(cyear," - ", cmonth))

      pos.month <- which(months == cmonth)
      for (ivar in seq(1,length(vars))){

        cvar = vars[ivar]
        ctimes <- times[pos.month]

        if (cyear >= 2023){

          data <- ncvar_get(nc,cvar,
                            start = c(1,1,1,min(pos.month)),
                            count = c(-1,-1,-1,length(pos.month)))

          expvers <- ncvar_get(nc,"expver")

          melt.data <- melt(data) %>%
            mutate(lon = lons[Var1],
                   lat = lats[Var2],
                   expver = expvers[Var3],
                   time = ctimes[Var4])

          df <- bind_rows(melt.data %>%
                            filter(expver == 1) %>%
                            filter(!is.na(value)) %>%
                            rename(!!cvar := "value") %>%
                            dplyr::select(-c("expver",starts_with("Var"))),
                          melt.data %>%
                            filter(expver == 5) %>%
                            filter(!is.na(value)) %>%
                            rename(!!cvar := "value") %>%
                            dplyr::select(-c("expver",starts_with("Var"))))

        } else{

          data <- ncvar_get(nc,cvar,
                            start = c(1,1,min(pos.month)),
                            count = c(-1,-1,length(pos.month)))

          df <- melt(data) %>%
            mutate(lon = lons[Var1],
                   lat = lats[Var2],
                   time = ctimes[Var3]) %>%
            rename(!!cvar := "value") %>%
            dplyr::select(-starts_with("Var"))
        }

        if (ivar > 1){
          all.df <- all.df %>%
            left_join(df,
                      by = c("lat","lon","time"))
        } else{
          all.df <- df
        }
      }

      cdf <- all.df %>%
        mutate(month = month(time),
               day = day(time)) %>%
        mutate(t = t2m - 273.15,
               dewpoint = d2m - 273.15) %>%
        mutate(beta = (112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t)),
               rh =   ((112 - (0.1 * t) + dewpoint) / (112 + (0.9 * t))) ^ 8) %>%
        mutate(sh = PEcAn.data.atmosphere::rh2qair(rh,
                                                   t2m,
                                                   101325)) %>%
        mutate(VPD = PEcAn.data.atmosphere::get.vpd(rh*100,
                                                    t)) %>%
        group_by(month,day,lat,lon) %>%
        mutate(tmin = min(t2m),
               tmax = max(t2m)) %>%
        group_by(month,lat,lon) %>%
        summarise(tmp = mean(t2m),
                  tmin = mean(tmin),
                  tmax = mean(tmax),
                  pre = mean(tp)*1000*3,
                  dswrf = mean(ssrd)*6,
                  dlwrf = mean(strd)/(3600),
                  spfh = mean(sh),
                  VPD = mean(VPD),
                  .groups = "keep")

      all.df.years <- bind_rows(
        all.df.years,
        cdf %>%
          mutate(year = cyear))

    }

    nc_close(nc)

  }

  saveRDS(all.df.years,
          paste0(OPpath_prefix,"_",min(years2change),".RDS"))

}
