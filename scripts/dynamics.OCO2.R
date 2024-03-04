rm(list = ls())

library(dplyr)
library(ncdf4)
library(reshape2)


all.coord.ILF <- readRDS("./outputs/coord.ILF.OCO2.RD2")

all.df <- data.frame()
years = 2014:2023
for (cyear in years){
  print(cyear)
  for (cmonth in 1:12){

    cfile <- paste0("./data/OCO2/z_cams_l_cams55_",
                    cyear,
                    sprintf("%02d",cmonth),
                    "_FT23r3_ra_sfc_mm_co2_flux.nc")
    if (!file.exists(cfile)){next()}
    nc <- nc_open(cfile)
    lats <- ncvar_get(nc,"latitude")
    lons <- ncvar_get(nc,"longitude")

    flux <- ncvar_get(nc,"flux_apos_bio")
    cdf <- melt(flux) %>%
      rename(lon = Var1,
             lat = Var2) %>%
      mutate(lat = lats[lat],
             lon = lons[lon]) %>%
      filter(abs(lat) <= 25) %>%
      mutate(month = cmonth,
             year = cyear)

    nc_close(nc)

    all.df <- bind_rows(all.df,
                        cdf %>%
                          mutate(lon.lat = paste0(round(lon,digits = 2),
                                                  ".",
                                                  round(lat,digits = 2))) %>%
                          filter(lon.lat %in% all.coord.ILF[["lon.lat"]])
                        )
  }
}

ts <- all.df %>%
  group_by(year,month) %>%
  summarise(value.m = mean(value),
            .groups = "keep")

ggplot(data = ts) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value.m)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw()

SC <- all.df %>%
  group_by(month) %>%
  summarise(value.m = mean(value),
            .groups = "keep")


ggplot(data = SC) +
  geom_line(aes(x = month,
                y = value.m), color = "black") +
  geom_line(data = ts %>%
              filter(year == 2023),
            aes(x = month,
                y = value.m,
                group = as.factor(year)), color = "red", size = 0.5) +
  geom_line(data = ts,
            aes(x = month,
                y = value.m,
                group = as.factor(year)), color = "lightgrey", size = 0.25) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "C flux [kgC/m²/month]") +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  theme_bw() +
  theme(text = element_text(size = 20))

Window = 6
GPPvsNBP <- readRDS("./outputs/GPP.anomalies.RDS") %>%
  dplyr::select(year,month,pred.m.rm,source) %>%
  left_join(ts %>% mutate(value.m.rm = rollapply(value.m, width=Window,
                                    FUN=function(x) mean(x, na.rm=TRUE),
                                    partial=TRUE, fill=NA, align="center")),
            by = c("year","month"))

ggplot(data = GPPvsNBP,
       aes(x = pred.m.rm,
           y = value.m)) +
  geom_point() +
  stat_smooth(method = "lm") +
  facet_wrap(~ source, scales = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Monthly GPP",
       y = "Monthly C flux from OCO2 inversion") +
  theme_bw() +
  theme(text = element_text(size = 20))

GPPvsNBP %>%
  group_by(source) %>%
  summarise(r2 = summary(lm(formula = value.m ~ (pred.m)))[["adj.r.squared"]],
            .groups = "keep")

