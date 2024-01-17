rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

system2("scp",
       c("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/JRA.rspld.monthly.historical.RDS",
         "./outputs/"))

df1 <- readRDS("./outputs/JRA.rspld.submonthly.historical.RDS")
df2 <- readRDS("./outputs/JRA.rspld.monthly.historical.RDS")

merge.vars <- inner_join(df1,
                         df2 %>%
                           rename(month = Var3),
                         by = c("lon","lat","year","month","source")) %>%
  rename(tmp = tmean)

df.all <- bind_rows(merge.vars %>%
                      mutate(source = "JRA"),

                    readRDS("./outputs/selected.RDS") %>%
                      filter(year >= 2020) %>%
                      mutate(source = "CRUJRA"))

df.all.long <- df.all %>%
  pivot_longer(cols = -c(lat,lon,source,year, month),
               names_to = "var")


ggplot(data = df.all.long) +
  geom_density(aes(x = value, fill = source), alpha = 0.5, color = NA) +
  facet_wrap( ~ var,scales = "free") +
  theme_bw()


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = merge.vars %>%
         filter(year == year[1], month == month[1])) +
  geom_raster(aes(x=lon,y = lat,
                  fill = pre),alpha = 0.3) +
  geom_sf(data = world,fill = NA) +
  coord_sf(xlim = c(-150, 180), ylim = c(-25, 25), expand = FALSE) +
  # scale_fill_gradient2(limits = c(-1,1)*0.5, oob = scales::squish) +
  # facet_wrap(~ source) +
  labs(x = "",y = "") +
  theme_bw()

merge.vars[sapply(merge.vars, is.infinite)] <- NA

saveRDS(merge.vars %>%
          dplyr::select(-source),
        "./outputs/monthly.climate.pantropical.JRA.historical.RDS")
system2("rsync",
        c("-avz",
          "./outputs/monthly.climate.pantropical.JRA.historical.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
