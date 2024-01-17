rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

df1 <- readRDS("./outputs/JRA.rspld.submonthly.RDS")
df2 <- readRDS("./outputs/JRA.rspld.monthly.RDS")

merge.vars <- inner_join(df1,df2,
                         by = c("lon","lat","year","month","source")) %>%
  dplyr::select(-c(tmin2,tmax2))

df.all <- bind_rows(merge.vars,

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

# # # Hack
# merge.vars <- merge.vars %>%
#   mutate(tmin = case_when(year == 2020 & month == 12 ~ tmin + 70,
#          TRUE  ~tmin))


merge.vars[sapply(merge.vars, is.infinite)] <- NA

saveRDS(merge.vars %>% dplyr::select(-source),
        "./outputs/monthly.climate.pantropical.JRA.RDS")
system2("scp",
        c("./outputs/monthly.climate.pantropical.JRA.RDS",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/"))
# scp
