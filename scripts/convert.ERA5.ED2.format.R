rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(tidyr)
library(PEcAn.ED2)
library(Yoko.regrowth)

# Manaus
# Latitude: -3.117034
# Longitude: -60.025780

# Paracou
# Latitude: 5.3
# Longitude: -52.9


slat = 5.25
slon = -53
years <- c(1960:1969)                                                          # years available
main.dir <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Amazon/Paracou"  # location of the file

prefix <- "ERA5_Paracou_"

vars <- c("t2m","sp","tp","u10","v10","ssrd","strd","d2m")

var.names <- vars

df.all <- data.frame()
for (iyear in seq(1,length(years))){
  print(years[iyear])
  ERA5.file <- file.path(main.dir,paste0(prefix,years[iyear],".nc"))

  if (!file.exists(ERA5.file)) {
    stop(paste("Missing file:",ERA5.file))
    next()
  }

  ncfile <- nc_open(ERA5.file)

  for (ivar in seq(1,length(vars))){
    if(ivar == 1) {
      temp.time <- ncvar_get(ncfile,"time")
      ctime <- temp.time/24
      cdf <- data.frame(time = ctime)
    }
    cvar <- apply(ncvar_get(ncfile,vars[ivar]),3,mean)   # 4 sites (2lon, 2lat) --> we take the mean
    cdf <- cdf %>% mutate(value = cvar)
    colnames(cdf)[ivar + 1] <- var.names[ivar]
  }
  nc_close(ncfile)
  df.all <- bind_rows(list(df.all,
                           cdf))
}

df.all.time <- df.all %>% mutate(t = as.POSIXct(time*86400,
                                                tz = "UTC",
                                                origin = "1900-01-01")) %>%
  mutate(year = year(t),
         month = month(t),
         day = day(t),
         h = hour(t),
         min = minute(t),
         sec = second(t))



df.all.time.all <- df.all.time

df.all.time.all.local.time <- df.all.time.all %>% mutate(t = t + 0) %>%
  mutate(year = year(t),
         month = month(t),
         day = day(t),
         h = hour(t),
         min = minute(t),
         sec = second(t)) %>%
  dplyr::filter(year >= min(year),
                year <= 2021)

# Source = https://keelingcurve.ucsd.edu/permissions-and-data-sources/
CO2.data <- read.csv(file = "./data/CO2.csv",header = TRUE) %>%
  mutate(year = floor(time)) %>%
  mutate(CO2 = case_when(CO2 == -99.99 ~ NA_real_,
                         TRUE ~ CO2)) %>%
  group_by(year) %>%
  summarise(CO2 = mean(CO2,na.rm = TRUE)/1e6,
            .groups = "keep")
CO2.data.all <- data.frame(year = 1700:2022) %>%
  left_join(CO2.data,
            by = "year") %>%
  mutate(CO2.interp = na.approx(CO2, maxgap = 50, rule = 2)) %>%
  dplyr::select(-CO2) %>%
  rename(CO2 = CO2.interp)

df.all.time.CO2 <- df.all.time.all.local.time %>% left_join(CO2.data.all,
                                                            by = "year") %>%
  filter(!is.na(t))


df.all.long <- df.all.time.CO2 %>%
  pivot_longer(cols = -c(time,t,year,month,day,h,min,sec),
               names_to = "var",
               values_to = "value") %>% arrange(t)

l.cycle <- df.all.long %>%
  group_by(year,var) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(var) %>%
  mutate(value.m.smooth = rollapply(value.m, width = 10, FUN = mean, align = "center", partial = TRUE)) %>%
  ungroup()

ggplot(l.cycle) +
  geom_line(aes(x = year,y = value.m)) +
  geom_line(aes(x = year,value.m.smooth),
            color = "red") +
  facet_wrap(~var,scales = "free") +
  labs(x = "",y = "") +
  theme_bw()

s.cycle <- df.all.long %>%
  group_by(month,var) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(s.cycle) +
  geom_line(aes(x = month,y = value.m)) +
  facet_wrap(~var,scales = "free") +
  labs(x = "",y = "") +
  scale_x_continuous(breaks = 1:12) +
  theme_bw()

d.cycle <- df.all.long %>%
  group_by(h,var) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(d.cycle) +
  geom_line(aes(x = h,y = value.m)) +
  facet_wrap(~var,scales = "free") +
  labs(x = "",y = "") +
  theme_bw()


#########################################################################################################

start_year <- 1960 ; end_year <- 1969

start_date <- paste0(start_year,"-01-01")
end_date <- paste0(end_year,"-12-31")
overwrite = TRUE

colnames(df.all.time.CO2)[colnames(df.all.time.CO2) == "CO2"] <- "mole_fraction_of_carbon_dioxide_in_air"
vars <- unique(c(vars,"mole_fraction_of_carbon_dioxide_in_air"))

local.data.point <- vars %>%
  purrr::map_dfc(function(vname) {

    nn <- df.all.time.CO2[,vname]

    if (!is.numeric(nn)) {
      PEcAn.logger::logger.severe(paste0(
        "Expected raster object to be numeric, but it has type `",
        paste0(typeof(nn), collapse = " "),
        "`"
      ))
    }

    nn

  }) %>%
  `colnames<-`(vars)

site.XTS <- xts::xts(local.data.point, order.by = (df.all.time.CO2$t))
site.XTS[site.XTS<0] <- 0

destination <- file.path(main.dir,paste0("ERA5_Paracou_processed"))
dir.create(destination,showWarnings = FALSE)

saveRDS(object = site.XTS,
        file = file.path(destination,"ERA5_reanalysis.RDS"))

# system2("scp",paste(file.path(destination,"ERA5_reanalysis.RDS"),
#                     "hpc:/data/gent/vo/000/gvo00074/felicien/R/data/"))
# for (year in seq(start_year,end_year)){
#
#   print(year)
#
#   out <- Yoko.regrowth::met2CF.ERA5(
#     lat = slat,
#     long = slon,
#     start_date =  paste0(year,"-01-01"),
#     end_date = paste0(year,"-12-31"),
#     sitename = "Yoko",
#     outfolder = destination,
#     out.xts = list(site.XTS %>% subset(year(index(.)) == year)),
#     overwrite = overwrite,
#     verbose = FALSE,
#     type = "reanalysis")
# }
#
# for (year in seq(start_year,end_year)){
#
#   print(year)
#   Yoko.regrowth::mod.met2model.ED2(
#     in.path =  "/data/gent/vo/000/gvo00074/ED_common_data/met/Amazon/Paracou/ERA5_Paracou_processed/ERA5_Paracou_1/",
#     in.prefix = "ERA5.1",
#     outfolder = "/data/gent/vo/000/gvo00074/ED_common_data/met/Amazon/Paracou/ERA5_Paracou_processed/ERA5_Paracou_1/ED2",
#     start_date =  paste0(year,"-01-01"),
#     end_date = paste0(year,"-12-31"),
#     lat = slat,
#     lon = slon,
#     lst = 0,
#     overwrite = overwrite)
# }

###############################################################################################

start_year <- 1960 ; end_year <- 1969

start_date <- paste0(start_year,"-01-01")
end_date <- paste0(end_year,"-12-31")
overwrite = TRUE

destination <- "/data/gent/vo/000/gvo00074/ED_common_data/met/Amazon/Paracou/ERA5_Paracou_processed/"
site.XTS <- readRDS(file = file.path("/data/gent/vo/000/gvo00074/ED_common_data/met/Amazon/Paracou/ERA5_Paracou_processed/ERA5_reanalysis.RDS"))

for (year in seq(start_year,end_year)){

  print(year)

  lat = slat ; long = slon ; start_date =  paste0(year,"-01-01") ; end_date = paste0(year,"-12-31")
  sitename = "Paracou" ; outfolder = destination ; out.xts = list(site.XTS %>% subset(year(index(.)) == year))
  overwrite = overwrite ; verbose = FALSE ; type = "reanalysis"


  out <- Yoko.regrowth::met2CF.ERA5(
    lat = slat,
    long = slon,
    start_date =  paste0(year,"-01-01"),
    end_date = paste0(year,"-12-31"),
    sitename = "Paracou",
    outfolder = destination,
    out.xts = list(site.XTS %>% subset(year(index(.)) == year)),
    overwrite = overwrite,
    verbose = FALSE,
    type = "reanalysis")
}

for (year in seq(start_year,end_year)){

  print(year)
  Yoko.regrowth::mod.met2model.ED2(
    in.path =  file.path(destination,"ERA5_Paracou_1"),
    in.prefix = "ERA5.1",
    outfolder = file.path(destination,"ERA5_Paracou_1","ED2"),
    start_date =  paste0(year,"-01-01"),
    end_date = paste0(year,"-12-31"),
    lat = slat,
    lon = slon,
    lst = 0,
    overwrite = overwrite)
}

# scp /home/femeunier/Documents/projects/Yoko.regrowth/scripts/process.ERA5.Yoko.HPC.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
