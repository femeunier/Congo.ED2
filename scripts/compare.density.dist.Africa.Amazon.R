rm(list = ls())

library(dplyr)
library(tidyr)
library(PEcAn.ED2)
library(stringr)
library(raster)
library(rhdf5)
library(pracma)
library(lubridate)
library(ggplot2)

# source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")
source("/home/femeunier/Documents/ED2.2/ED2/R-utils/h5read_opt.r")

final.year <- 2000
h5files.cluster <- c(paste0("/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/out/CB_IPHEN_-1_X_25E_Y_4N/histo/history-S-",final.year,"-01-01-000000-g01.h5"),
                     paste0("/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/out/CB_IPHEN_2_X_25E_Y_4N/histo/history-S-",final.year,"-01-01-000000-g01.h5"),
                     paste0("/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/out/Manaus/histo/history-S-",final.year,"-01-01-000000-g01.h5"),
                     paste0("/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/out/Manaus_PHEN2/histo/history-S-",final.year,"-01-01-000000-g01.h5"),
                     paste0("/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/out/Paracou/histo/history-S-",final.year,"-01-01-000000-g01.h5"),
                     paste0("/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/out/Paracou_PHEN2/histo/history-S-",final.year,"-01-01-000000-g01.h5"))
h5files <- c("Congo_evergreen.h5","Congo_DD.h5","Manaus_evergreen.h5","Manaus_DD.h5","Paracou.h5","Paracou.h5")

for (i in seq(1,length(h5files.cluster))){

   system2("rsync",paste("-avz",
                         paste0("hpc:",h5files.cluster[i]),
                         paste0("./outputs/",h5files[i])))
}

phen <- c("Evegreen","Drought-deciduous","Evegreen","Drought-deciduous","Evegreen","Drought-deciduous")
loc <- c("YGB","YGB","Manaus","Manaus","Paracou","Paracou")

df_sum <- data.frame()

for (ih5file in seq(1,length(h5files))){

  mymont    = lapply(h5read_opt(paste0("./outputs/",h5files[ih5file],sep = "")),FUN=aperm)
  names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

  PACO <- mymont$PACO.N
  PFT <- mymont$PFT
  Hite <- mymont$HITE
  DBH <- mymont$DBH
  patch_num <- length(PACO)
  PACOID <- rep(1:patch_num,PACO)
  PA_area <- mymont$AREA[PACOID]
  NPLANT <- mymont$NPLANT
  LAI <- mymont$LAI.CO
  AGB.co <- mymont$AGB.CO

  print(paste(loc[ih5file],"-",phen[ih5file]))
  print(paste(max(DBH[DBH > 100]),"-",round(1e5*NPLANT[DBH == max(DBH)])/10))

  df <-
    data.frame(
      pft = PFT,
      dbh = DBH,
      paid = PACOID,
      pa_area = PA_area,
      lai = LAI,
      agb = AGB.co,
      nplant = NPLANT
    ) %>% rename(DBH = dbh,PFT = pft) %>% #filter((pft == 17 & dbh < 0.1 & nplant > 1))
    mutate(
      DBH_class = case_when(
        DBH < 1 ~ 0,
        DBH < 10 ~ 1,
        DBH < 20 ~ 2,
        DBH < 30 ~ 3,
        DBH < 40 ~ 4,
        DBH < 50 ~ 5,
        DBH < 60 ~ 6,
        DBH < 70 ~ 7,
        DBH < 80 ~ 8,
        DBH < 90 ~ 9,
        DBH < 100 ~ 10,
        DBH >= 100 ~ 11),
      BA = (pi*DBH**2)/4)


  df_sum <- bind_rows(list(df_sum,
                           df %>% filter(DBH_class > -Inf) %>%
    group_by(DBH_class) %>%
    summarise(n = sum(nplant*pa_area)*10000,
              BA = sum(BA*nplant*pa_area),
              AGB = sum(agb*nplant*pa_area),
              LAI = sum(lai*pa_area),
              dbh_m = mean(DBH)) %>% mutate(Phenology = phen[ih5file],
                                            Site = loc[ih5file])))
}


df_sum <- df_sum %>%
  mutate(Site = factor(Site,
                       levels = c("Paracou","Manaus","YGB")))

ggplot(data = df_sum %>% filter(DBH_class > 1)) +

  geom_bar(aes(x = as.factor(DBH_class),
               y = n,
               fill = as.factor(Phenology)),
           stat = "identity",position = position_dodge2(preserve = "single")) +

  labs(x="",y="",fill = "Phenology") +
  theme_bw() +
  facet_wrap(~Site) +
  theme(legend.position = c(0.9,0.8))

data.files <- c("/home/femeunier/Documents/data/Yangambi/data/inventories/YGB_24_MIX_02 MCU.csv",
                "/home/femeunier/Documents/data/Yangambi/data/inventories/YGB-27 MIX-05 MCU.csv")

data <- data.frame()
for (idata.file in seq(1,length(data.files))){
  data <- bind_rows(list(data,
                         read.csv(data.files[idata.file]) %>%
                           rename(DBH = DBH4) %>%
                           mutate(DBH = DBH/10) %>%
                           filter(!is.na(DBH)) %>%
                           mutate(
                             DBH_class = case_when(
                               DBH < 1 ~ 0,
                               DBH < 10 ~ 1,
                               DBH < 20 ~ 2,
                               DBH < 30 ~ 3,
                               DBH < 40 ~ 4,
                               DBH < 50 ~ 5,
                               DBH < 60 ~ 6,
                               DBH < 70 ~ 7,
                               DBH < 80 ~ 8,
                               DBH < 90 ~ 9,
                               DBH < 100 ~ 10,
                               DBH >= 100 ~ 11)) %>%
                           group_by(DBH_class) %>%
                           summarise(n = 10000*length(X)/10000,
                                     .groups = "keep") %>% mutate(Phenology = tools::file_path_sans_ext(basename(data.files[idata.file])),
                                                                  Site = "YGB")
                         ))
}




data.vs.sim <- bind_rows(list(data,
                              df_sum)) %>%
  mutate(Site = factor(Site,
                       levels = c("Paracou","Manaus","YGB")))

df_sum %>% group_by(Site,Phenology) %>% summarise(AGB = sum(AGB),
                                                  LAI = sum(LAI))

data.vs.sim %>% group_by(Site,Phenology) %>%
  filter(DBH_class > 10) %>%
  summarise(N = sum(n))

data.vs.sim %>% group_by(Site,Phenology) %>%
  filter(DBH_class > 1) %>%
  summarise(N = sum(n))



ggplot(data = data.vs.sim %>% filter(DBH_class > 1)) +

  geom_bar(aes(x = as.factor(DBH_class),
               y = n,
               fill = as.factor(Phenology)),
           stat = "identity",position = position_dodge2(preserve = "single")) +

  scale_fill_manual(values = c("darkblue","darkgreen","black","darkgrey")) +

  # scale_y_log10(limits = c(1,10010),breaks = c(2,101,10001),labels = c(1,100,10000),expand = c(0,0)) +
  labs(x="",y="",fill = "Phenology") +
  theme_bw() +
  facet_wrap(~ Site) +
  theme(legend.position = c(0.9,0.8))


write.csv(df_sum %>% filter(DBH_class > 1),
          file = "./outputs/Summary.Marijn.csv")

