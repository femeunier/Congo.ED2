rm(list = ls())

library(dplyr)

load("/home/femeunier/Documents/Figures/LU/Yoko_lu.RData")


#   !      Reset the disturbance rates.  We only populate the transitions that are !
#   ! may be positive.  The first index is the new land use type, and the second   !
#   ! index is the old land use type.  Both use the following convention:          !
#   !                                                                              !
#   !  1.  Pasture                                                                 !
#   !  2.  Forest plantation                                                       !
#   !  3.  Tree fall                                                               !
#   !  4.  Burnt                                                                   !
#   !  5.  Abandoned                                                               !
#   !  6.  Logged (felling)                                                        !
#   !  7.  Logged (skid trail / road)                                              !
#   !  8.  Agricultural lands (cropland / pasture)                                 !

plot(datum$patch$lu[[84]],
     datum$patch$agb[[84]])



df.all <- data.frame()
for (i in seq(1,100)){

  df.patch <- data.frame(ipa = 1:length(datum$patch$lu[[i]]),
                         lu = datum$patch$lu[[i]],
                         gpp = datum$patch$gpp[[i]],
                         age = datum$patch$age[[i]],
                         area = datum$patch$area[[i]],
                         agb = datum$patch$agb[[i]])
  df.cohort <- data.frame(ipa = datum$cohort$ipa[[i]],
                          pft = datum$cohort$pft[[i]],
                          nplant = datum$cohort$nplant[[i]]) %>%
    filter(nplant > 0)

  df.cohort.sum <- df.cohort %>%
    group_by(ipa) %>%
    summarise(pft.1 = any(pft == 1),
              pft.2 = any(pft == 2),
              pft.3 = any(pft == 3),
              pft.4 = any(pft == 4)) %>%
    left_join(df.patch,
              by = "ipa")

  df.all <- bind_rows(list(df.all,
                           df.cohort.sum %>% mutate(t = i)))
}


ggplot(data = df.all) +
  geom_point(aes(x = age,y = agb, color = as.factor(lu))) +
  theme_bw()

