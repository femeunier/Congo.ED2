rm(list = ls())

library(ncdf4)
library(reshape2)
library(dplyr)
library(ggplot2)
library(TrENDY.analyses)
library(raster)
library(tidyr)
library(lubridate)
library(PEcAn.data.atmosphere)

system2("rsync",
        c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/20*",
          "./outputs/"))

all.years <- data.frame()

for (cyear in seq(2020,2023)){
  for (cmonth in seq(1,12)){

    cyearmonth <- paste0(cyear,sprintf("%02d",cmonth))

    print(cyearmonth)
    cfile <- paste0("./outputs/",cyearmonth,".RDS")

    if (!file.exists(cfile)){ next()}
    df.month <- readRDS(cfile)
    all.years <- bind_rows(all.years,
                           df.month %>%
                             mutate(month = cmonth,
                                    year = cyear))

  }
}

df2 <- readRDS("./outputs/selected.RDS") %>%
  dplyr::select(year,month,lon,lat,tmp,tmin,tmax,spfh,VPD)

grid <- rasterFromXYZ((df2 %>%
                         filter(year == 2022,
                                month == 1) %>%
                         ungroup() %>%
                         dplyr::select(c("lat","lon","tmin")))[,c("lon","lat","tmin")])

df.rspld <- resample.df.all.col(bigdf = all.years %>%
                                  mutate(source = "JRA"),

                                raster2resample = grid,
                                var.names = c("tmp","tmin","tmax",
                                              "tmin2","tmax2","spfh","VPD"),
                                0.00092311) %>%
  filter(!is.na(tmp))

saveRDS(df.rspld,
        "./outputs/JRA.rspld.submonthly.RDS")

combined.all <- bind_rows(df.rspld %>%
                        mutate(source = "JRA"),
                      df2 %>%
                        mutate(source = "CRUJRA"))

ggplot(data = combined.all %>%
         filter(lon == 12.25,
                lat == 1.25)) +
  geom_line(aes(year + (month -1/2)/12, y = tmp, color = source)) +
  # facet_wrap(~ var, scales = "free") +
  theme_bw()

combined <- combined.all %>%
  filter(month == 1, year == 2022)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = combined) +
  geom_raster(aes(x=lon,y = lat,
                  fill = tmp),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-150, 180), ylim = c(-25, 25), expand = FALSE) +
  # scale_fill_gradient2(limits = c(-5,5), oob = scales::squish) +
  facet_wrap(~ source) +
  labs(x = "",y = "") +
  theme_bw()

combined.wide <- combined %>%
  pivot_wider(names_from = source,
              values_from = -c(lat,lon,month,year)) %>%
  mutate(diff = tmp_JRA - tmp_CRUJRA)

hist(combined.wide$diff)


ggplot(data = combined.wide) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-150, 180), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient2(limits = c(-1,1), oob = scales::squish) +
  # facet_wrap(~ source) +
  labs(x = "",y = "") +
  theme_bw()


combined.long <- combined %>%
  # mutate(tmin = case_when(source == "JRA" ~ tmin2,
  #                         TRUE ~ tmin),
  #        tmax = case_when(source == "JRA" ~ tmax2,
  #                         TRUE ~ tmax)) %>%
  dplyr::select(-c(tmin2,tmax2)) %>%
  pivot_longer(cols = -c(lat,lon,source,year, month),
               names_to = "var")

ggplot(data = combined.long) +
  geom_density(aes(x = value, fill = source), alpha = 0.5, color = NA) +
  facet_wrap( ~ var,scales = "free") +
  theme_bw()

plot(combined$spfh,combined$VPD)
