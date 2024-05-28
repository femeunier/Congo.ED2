rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(dismo)
library(tie)

coord <- readRDS("./outputs/Coord.ILF.ERA5.RDS") %>%
  # filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

climate <- readRDS("./outputs/monthly.climate.global.ERA5.RDS") %>%
  filter(year >= 1994) %>%
  mutate(tmp = tmp -273.15,
         tmin = tmin -273.15,
         tmax = tmax -273.15)

climate.select <- climate %>%
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
            # pre = mean(pre*N*4),
            pre = mean(pre*N*8),
            .groups = "keep") %>%
  pivot_longer(cols = -c(year,month),
               names_to = "variable",
               values_to = "value")


cmonth <- month(today())

climate.sum <- bind_rows(climate.sum,
                         climate.sum %>%
                           filter(year == 2024,
                                  month == 1) %>%
                           mutate(month = cmonth + 1,
                                  value = NA))

climate.sum.anomaly <- climate.sum %>%
  mutate(time = year + (month -1/2)/12) %>%
  group_by(variable) %>%
  mutate(slope = coef(lm(value ~ time))[2],
         intercept = coef(lm(value ~ time))[1]) %>%

  # mutate(mean.obs = slope*(time) + intercept) %>%
  mutate(mean.obs = mean(value,na.rm = TRUE)) %>%

  mutate(detrended = value - mean.obs) %>%
  group_by(variable,month) %>%
  mutate(mean.month = mean(detrended,na.rm = TRUE)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  group_by(variable,month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly,na.rm = TRUE))


droughts <- data.frame(x1 = c(1997,2015,2023) + 0.5/12,
                       x2 = c(1998,2016,2023) +
                         11.5/12)


climate.sum.anomaly.select <- climate.sum.anomaly %>%
  filter(variable %in% c("pre","tmp"))



climate.sum.anomaly.select.group <- climate.sum.anomaly.select %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:cmonth) ~ "2023",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA)) %>%
  mutate(value = case_when(!is.na(groups) ~ value,
                           TRUE ~ NA_real_))%>%
  mutate(value = case_when((year == 1998 & month == 5) |
                             (year == 2016 & month == 4) |
                             (year == 2010 & month == 5) |
                             (year == 2024 & month == (cmonth + 1)) ~ NA,
                           TRUE ~ value)) %>%
  arrange(year,month,variable)


saveRDS(climate.sum.anomaly.select,
        "./outputs/climate.sum.anomaly.select.RDS")


climate.sum.anomaly.select.mod <-
  climate.sum.anomaly.select %>%
  mutate(month.mod = month - 6) %>%
  mutate(month.mod = case_when(month.mod <= 0 ~ month.mod + 12,
                               TRUE ~ month.mod))

climate.sum.anomaly.select.group.mod <-
  climate.sum.anomaly.select.group %>%
  mutate(month.mod = month - 6) %>%
  mutate(month.mod = case_when(month.mod <= 0 ~ month.mod + 12,
                               TRUE ~ month.mod))


climate.sum.anomaly.selected <- climate.sum.anomaly %>%
  filter(variable %in% c("tmp","pre")) %>%
  dplyr::select(year,month,variable,anomaly.m,anomaly,value)

climate.sum.anomaly.selected.group <- climate.sum.anomaly.selected %>%
  mutate(groups =  case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                             year == 2024 & month %in% c(1:4) ~ "2023",

                             year == 2016 & month %in% c(1:4) ~ "2015",
                             year == 2015 & month %in% c(8:12) ~ "2015",

                             year == 1997 & month %in% 9:12 ~ "1997",
                             year == 1998 & month %in% 1:5 ~ "1997",

                             TRUE ~ NA)) %>%
  mutate(value = case_when(!is.na(groups) ~ value,
                           TRUE ~ NA_real_),
         anomaly.m = case_when(!is.na(groups) ~ anomaly.m,
                               TRUE ~ NA_real_),
         anomaly = case_when(!is.na(groups) ~ anomaly,
                             TRUE ~ NA_real_)) %>%
  mutate(value = case_when((year == 1998 & month == 5) |
                             (year == 2024 & month == (cmonth + 1)) |
                             (year == 2010 & month == 5) |
                             (year == 2016 & month == 4) ~ NA,
                           TRUE ~ value),
         anomaly.m = case_when((year == 1998 & month == 5) |
                                 (year == 2024 & month == (cmonth + 1)) |
                                 (year == 2010 & month == 5) |
                                 (year == 2016 & month == 4) ~ NA,
                               TRUE ~ anomaly.m),
         anomaly = case_when((year == 1998 & month == 5) |
                               (year == 2024 & month == (cmonth + 1)) |
                               (year == 2010 & month == 5) |
                               (year == 2016 & month == 4) ~ NA,
                             TRUE ~ anomaly)) %>%
  arrange(year,month,variable)


climate.sum.anomaly.selected.group %>%
  filter(!is.na(groups)) %>%
  group_by(groups,variable) %>%
  summarise(anomaly.m = mean(anomaly.m,
                             na.rm = TRUE),
            anomaly = mean(anomaly,
                           na.rm = TRUE),
            value.m = mean(value,
                           na.rm = TRUE))


saveRDS(climate.sum.anomaly.selected,
        "./outputs/climate.sum.anomaly.selected.RDS")



# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/analyze.climate.Amazon.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

