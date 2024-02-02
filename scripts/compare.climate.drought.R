rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dismo)
library(tie)

coord <- readRDS("./outputs/Amazon.coord.ILF.RDS") %>%
  filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

climate <- readRDS("./outputs/monthly.climate.pantropical.JRA.historical.RDS") %>%
  filter(year >= 1994)

climate.select <- climate %>%
  mutate(timing = case_when(year %in% c(1992:2020) ~ "original",
                            # year == 2010 ~ "2010",
                            year == 2023 ~ "2023",
                            TRUE ~ NA_character_)) %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
  filter(lon.lat %in% coord[["lon.lat"]]) %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01"))))

climate.sum <- climate.select %>%
  group_by(year,month) %>%
  summarise(tmp = mean(tmp),
            spfh = mean(spfh),
            VPD = mean(VPD),
            dswrf = mean(dswrf),
            dlwrf = mean(dlwrf),
            tmin = mean(tmin),
            tmax = mean(tmax),
            pre = mean(pre*N*4),
            .groups = "keep") %>%
  pivot_longer(cols = -c(year,month),
               names_to = "variable",
               values_to = "value")

climate.sum %>%
  mutate(semester = case_when(month <= 6 ~ 1,
                              TRUE ~ 2)) %>%
  filter(variable == "tmp") %>%
  group_by(year,semester) %>%
  summarise(value.m = mean(value)) %>%
  arrange(desc(value.m))

ggplot(data = climate.sum %>%
         filter(variable == "tmp") ) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value)) +
  theme_bw()


climate.sum.anomaly <- climate.sum %>%
  mutate(time = year + (month -1/2)/12) %>%
  group_by(variable) %>%
  mutate(slope = coef(lm(value ~ time))[2],
         intercept = coef(lm(value ~ time))[1]) %>%
  # mutate(mean.obs = slope*(time) + intercept) %>%
  mutate(mean.obs = mean(value)) %>%
  mutate(detrended = value - mean.obs) %>%
  group_by(variable,month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  # mutate(reconstructed = intercept + time*slope + mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(variable) %>%
  mutate(anomaly.m = anomaly/sd(anomaly))

ggplot(data = climate.sum,
       aes(x = year + (month - 1/2)/12,
           y = value)) +
  geom_line(data = climate.sum.anomaly,
            aes(y = reconstructed),
            color = "red", size = 0.4) +
  geom_line() +
  facet_wrap(~ variable,scales = "free") +
  scale_x_continuous(limits = c(1990,2024),
                     expand = c(0,0)) +
  # stat_smooth(method = "lm",
  #             color = "red",
  #             se = FALSE) +
  theme_bw()

droughts <- data.frame(x1 = c(1997,2009,2004,2015,2023) + 0.5/12,
                       x2 = c(1998,2010,2005,2016,2023) +
                         11.5/12)

ggplot() +
  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +
  geom_line(data = climate.sum.anomaly,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.m)) +
  facet_wrap(~ variable,scales = "free") +
  scale_x_continuous(limits = c(1990,2024),
                     expand = c(0,0)) +
  scale_y_continuous(limits = c(-1,1)*3.5) +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  theme_bw()

saveRDS(climate.sum.anomaly,
        "./outputs/climate.anomalies.RDS")

# plot(climate.sum.anomaly %>%
#        # filter(year == 1958) %>%
#        filter(variable == "tmp") %>%
#        pull(mean.obs),type = "l")

stop()

################################################################################

climate.select.oct <- climate.select %>%
  filter(month == 10)

climate.select.oct.long <- climate.select.oct %>%
  dplyr::select(-c(lon.lat)) %>%
  pivot_longer(cols = -c(timing,lon,lat),
               names_to = "biovar",
               values_to = "value")

ggplot(data = climate.select.oct.long) +
  geom_density(aes(x = value,
                   fill = timing),
               alpha = 0.3, color = NA) +
    facet_wrap(~ biovar,scales = "free") +
    theme_bw()

sc <- climate.select %>%
  # group_by(timing,month) %>%
  dplyr::select(-c(lon.lat)) %>%
  pivot_longer(cols = -c(timing,lon,lat,month),
               names_to = "biovar",
               values_to = "value")  %>%
  filter(biovar != "year") %>%
  group_by(timing,month,biovar) %>%
  summarise(value.m = mean(value),
            .groups = "keep")

ggplot(data = sc %>%
         filter(biovar %in% c("tmp",
                              "spfh",'tmin',
                              "tmax","pre",
                              "VPD"),
                timing %in% c("original","2023"))) +
  geom_line(aes(x = month,
                y = value.m,
                color = timing)) +
  facet_wrap(~ biovar,scales = "free") +
  theme_bw()


################################################################################
# Only MAP first
monthly.precip <- climate.select %>%
  ungroup() %>%
  mutate(pre = pre*4*N)

MAP <- monthly.precip %>%
  group_by(lat,lon,timing,year) %>%
  summarise(MAP = sum(pre),
            .groups = "keep") %>%
  group_by(lat,lon,timing) %>%
  summarise(MAP = mean(MAP),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = MAP) +
  geom_raster(aes(x=lon,y = lat,
                  fill = MAP),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ timing) +
  scale_fill_gradient(limits = c(0,3000),
                      oob = scales::squish,
                      low =  "white",high = "darkblue") +
  theme_bw()

MAP.diff <- MAP %>%
  pivot_wider(names_from = timing,
              values_from = MAP) %>%
  mutate(diff_2010 = original - `2010`,
         diff_2023 = original - `2023`) %>%
  dplyr::select(-c(original,`2010`,`2023`)) %>%
  pivot_longer(cols = c(diff_2010,diff_2023),
               names_to = "year",
               values_to = "diff")

ggplot(data = MAP.diff) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ year) +
  scale_fill_gradient2(limits = c(-500,500)*2,
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "white",high = "darkgreen") +
  theme_bw()

ggplot(data = MAP %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35)) +
  geom_density(aes(x = MAP,
                   fill = timing),
               alpha = 0.3, color = NA) +
  theme_bw()

ggplot(data = monthly.precip  %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35) %>%
         group_by(month,timing) %>%
         summarise(precip = mean(pre),
                   .groups = "keep")) +
  geom_line(aes(x = month, color = timing, y = precip)) +
  theme_bw()

################################################################################
# All biovars

climate.biovars <- monthly.precip %>%
  group_by(timing,month,lat,lon) %>%
  summarise(pre = mean(pre),
            tmin = mean(tmin),
            tmax = mean(tmax),
            .groups = "keep") %>%
  group_by(timing,lat,lon) %>%
  bow(tie(bio1, bio2, bio3, bio4,
          bio5, bio6, bio7, bio8,
          bio9, bio10, bio11, bio12,
          bio13, bio14, bio15, bio16,
          bio17, bio18, bio19) := c(biovars(pre,
                                            (tmin - 273.15)*10,
                                            (tmax - 273.15)*10)[c(1:19)]))

climate.biovars.long <- climate.biovars %>%
  pivot_longer(cols = -c(timing,lon,lat),
               names_to = "biovar",
               values_to = "value") %>%
  mutate(biovar = factor(biovar,
                         levels = paste0("bio",1:19)))

ggplot(data = climate.biovars.long %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35)) +
  geom_density(aes(x = value,
                   fill = timing),
               alpha = 0.3, color = NA) +
  facet_wrap(~ biovar, scales = "free") +
  theme_bw()

climate.biovars.long.wide <- climate.biovars.long %>%
  pivot_wider(names_from = timing,
              values_from = value) %>%
  mutate(diff_2010 = original - `2010`,
         diff_2023 = original - `2023`) %>%
  dplyr::select(-c(original,`2010`,`2023`)) %>%
  pivot_longer(cols = c(diff_2010,diff_2023),
               names_to = "year",
               values_to = "diff")

ggplot(data = climate.biovars.long.wide %>%
         filter(abs(lat) <= 25,
                lon >= -90, lon <= -35)) +
  geom_density(aes(x = diff,
                   fill = year),
               alpha = 0.3, color = NA) +
  facet_wrap(~ biovar, scales = "free") +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  theme_bw()


ggplot(data = climate.biovars.long.wide %>%
         filter(biovar %in% paste0("bio",
                                   12:18))) +
  geom_raster(aes(x=lon,y = lat,
                  fill = diff),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  labs(x = "",y = "") +
  facet_grid(biovar ~ year) +
  scale_fill_gradient2(limits = c(-1000,1000),
                       oob = scales::squish,
                       midpoint = 0,
                       low = "darkred",mid = "white",high = "darkgreen") +
  theme_bw()
