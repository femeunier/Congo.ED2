rm(list = ls())

system2("rsync",
        paste("-avz",
              "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/climate_ERA5_sites.RDS",
              "./outputs/"))

system2("rsync",
        paste("-avz",
              "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/climate_CRU_sites.RDS",
              "./outputs/"))

Months <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

climate_ERA5_sites <- readRDS("./outputs/climate_ERA5_sites.RDS") %>%
  mutate(month.nr = as.numeric(factor(month,
                                      levels = Months)),
         Site = factor(Site,
                       levels = c("Paracou","Manaus","YGB")))

YGB <- climate_ERA5_sites %>% filter(Site == "YGB")

climate_ERA5_sites.sum <- climate_ERA5_sites %>%
  group_by(month.nr,Site) %>%
  summarise(tmp = mean(tmp - 273.15,
                       na.rm = TRUE),
            Pmm = mean(prate)*86400*365/12,
            sw = mean(sw),
            .groups = "keep")

climate_ERA5_sites.sum.long <- climate_ERA5_sites.sum %>%
  pivot_longer(cols = c(tmp,Pmm,sw),
               names_to = "var",
               values_to = "value")

climate_ERA5_sites.sum %>% group_by(Site) %>% summarise(MAP = sum(Pmm))

ggplot(data = climate_ERA5_sites.sum.long) +
  geom_line(aes(x = month.nr, y = value, color = Site)) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  facet_wrap(~var,scales = "free") +
  labs(x = "",y = "") +
  theme_bw()


N = 4
climate_ERA5_sites.sum.hr <- climate_ERA5_sites %>%
  group_by(year,month.nr,Site) %>%
  mutate(timing = rep(1:N,length(tmp)/N)) %>%
  group_by(Site,timing) %>%
  summarise(tmp = mean(tmp - 273.15,
                       na.rm = TRUE),
            Pmm = mean(prate)*86400*3,
            sw = mean(sw),
            .groups = "keep")


climate_ERA5_sites.sum.hr.long <- climate_ERA5_sites.sum.hr %>%
  pivot_longer(cols = c(tmp,Pmm,sw),
               names_to = "var",
               values_to = "value")


ggplot(data = climate_ERA5_sites.sum.hr.long) +
  geom_line(aes(x = (timing-0.5)*3, y = value, color = Site)) +
  facet_wrap(~var,scales = "free") +
  labs(x = "",y = "") +
  theme_bw()


