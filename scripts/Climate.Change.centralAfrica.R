rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(geodata)
library(tidyr)
library(YGB)
library(dismo)
library(tie)


file <- "./outputs/Climate.Change.climate.Africa.RDS"

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Climate.Change.climate.Africa.RDS",
                      file))

CCCA <- readRDS(file)

CCMA.filt <- CCCA %>%
  filter((scenario == "historical" & timing == "reference") |
           (scenario != "historical" & timing == "end"))

CCMA.filt.wide <- CCMA.filt %>%
  pivot_wider(names_from = "var",
              values_from = "value.m") %>%
  mutate(pr = 86400*365/12*pr)

reference <- CCMA.filt.wide %>%
  filter(timing == "reference") %>%

  group_by(model,month,lon,lat) %>%
  summarise(Pmm = mean(pr),
            .groups = "keep") %>%
  group_by(model,lon,lat) %>%
  mutate(Ndays = c(31,28,31,30,31,30,31,31,30,31,30,31),
         E = 3.33,
         Etot = E*Ndays) %>%
  mutate(diff = Pmm - Etot) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(month) %>%
  mutate(MCWD = min(CWD)) %>%

  group_by(model,lat,lon) %>%
  mutate(MAP = sum(Pmm)) %>%

  filter(month == 1) %>%
  dplyr::select(model,lon,lat,MAP,MCWD)

ggplot(data = reference) +
  geom_density(aes(x = MCWD, fill = model), alpha = 0.5) +
  theme_bw()


CCMA.filt.wide.cat <- CCMA.filt.wide %>%
  left_join(reference,
            by = c("model","lat","lon")) %>%
  mutate(cat = case_when(MCWD > -250 ~ 2,
                         MAP < 1000 ~ 1,
                         TRUE ~ 3))

CCMA.filt.wide.scenar <-  CCMA.filt.wide.cat %>%
  group_by(model,scenario,cat) %>%
  summarise(pr = mean(pr)*12,
            tas = mean(tas),
            tasmin = mean(tasmin),
            tasmax = mean(tasmax),
            .groups = "keep")


CCMA.filt.wide.scenar.wide <- CCMA.filt.wide.scenar %>%
  ungroup() %>%
  pivot_wider(names_from = scenario,
              values_from = c(pr,tas,tasmin,tasmax))

CCMA.filt.wide.scenar.wide.long <- CCMA.filt.wide.scenar.wide %>%
  pivot_longer(cols = - c(model,cat,
                          pr_historical,tasmin_historical,tas_historical,tasmax_historical),
               names_to = "var",
               values_to = "value") %>%
  mutate(scenar = case_when(grepl("ssp126",var) ~ "ssp126",
                            grepl("ssp245",var) ~ "ssp245",
                            grepl("ssp370",var) ~ "ssp370",
                            grepl("ssp585",var) ~ "ssp585"),
         var = case_when(grepl("tasmin",var) ~ "tasmin",
                         grepl("tasmax",var) ~ "tasmax",
                         grepl("pr",var) ~ "pr",
                         grepl("tas",var) ~ "tas"))

ggplot(data = CCMA.filt.wide.scenar.wide) +

  geom_segment(aes(x = pr_historical,
                   xend = pr_ssp245,

                   y = tasmax_historical,
                   yend = tasmax_ssp245,

                   color = as.factor(cat),
                   group = interaction(as.factor(cat),
                                       model)),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # facet_wrap(~ cat) +

  theme_bw()

CCMA.biovar <- CCMA.filt.wide.cat %>%
  group_by(scenario,model,lat,lon) %>%
  bow(tie(bio1, bio2, bio3, bio4,
          bio5, bio6, bio7, bio8,
          bio9, bio10, bio11, bio12,
          bio13, bio14, bio15, bio16,
          bio17, bio18, bio19,
          MCWD, cat) := c(biovars(pr,
                                  (tasmin - 273.15)*10,
                                  (tasmax - 273.15)*10)[c(1:19)],
                          unique(MCWD),unique(cat)))

CCMA.biovar.long <- CCMA.biovar %>%
  pivot_longer(cols = -c(scenario,model,lat,lon,cat,MCWD),
               names_to = "var",
               values_to = "value") %>%
  mutate(var = factor(var,
                      levels = c(paste0("bio",1:19))))

ggplot(data = CCMA.biovar.long %>% filter(model == model[1])) +
  geom_density(aes(x = value, fill = scenario), alpha = 0.5) +
  facet_wrap(~ var, scales = "free") +
  theme_bw()

ggplot(data = CCMA.biovar.long %>% filter(var == "bio1")) +
  geom_density(aes(x = value, fill = scenario)) +
  facet_wrap(~ model, scales = "free") +
  theme_bw()


CCMA.biovar.long.sum <- CCMA.biovar.long %>%
  group_by(var) %>%
  mutate(value.scaled =  (value - min(value))/(max(value) - min(value))) %>%
  group_by(model,scenario,var,cat) %>%
  summarise(m = mean(value,na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = scenario,
              values_from = m) %>%
  mutate(var = factor(var,
                      levels = c(paste0("bio",1:19)))) %>%
  mutate(var.type = case_when(var %in% paste0("bio",c(1,2,5:11)) ~ "Temp",
                              var %in% paste0("bio",c(3,4,15)) ~ "Percent",
                              TRUE ~ "Precip")) %>%
  # for rescaling, precip --> /month
  mutate(historical = case_when(var %in% paste0("bio",c(12)) ~ historical/12,
                                var %in% paste0("bio",seq(16:19)) ~ historical/4,
                                TRUE ~ historical),
         ssp126 = case_when(var %in% paste0("bio",c(12)) ~ ssp126/12,
                           var %in% paste0("bio",seq(16:19)) ~ ssp126/4,
                           TRUE ~ ssp126),
         ssp245 = case_when(var %in% paste0("bio",c(12)) ~ ssp245/12,
                            var %in% paste0("bio",seq(16:19)) ~ ssp245/4,
                            TRUE ~ ssp245),
         ssp370 = case_when(var %in% paste0("bio",c(12)) ~ ssp370/12,
                           var %in% paste0("bio",seq(16:19)) ~ ssp370/4,
                           TRUE ~ ssp370),
         ssp585 = case_when(var %in% paste0("bio",c(12)) ~ ssp585/12,
                           var %in% paste0("bio",seq(16:19)) ~ ssp585/4,
                           TRUE ~ ssp585))

ggplot(data = CCMA.biovar.long.sum %>%
         ungroup() %>%
         filter(var.type == "Temp")) +

  geom_segment(aes(x = 0, xend = 1,y = historical,yend = ssp126,
                   color = as.factor(cat),group = interaction(as.factor(cat),model))) +

  geom_segment(aes(x = 1,xend = 2,y = ssp126,yend = ssp245,
                   color = as.factor(cat),group = interaction(as.factor(cat),model))) +

  geom_segment(aes(x = 2,xend = 3,y = ssp245,yend = ssp370,
                   color = as.factor(cat),group = interaction(as.factor(cat),model))) +

  geom_segment(aes(x = 3,xend = 4,y = ssp370,yend = ssp585,
                   color = as.factor(cat),group = interaction(as.factor(cat),model)),
               arrow = arrow(length = unit(0.2, "cm"))) +

  facet_wrap( ~ var, nrow = 2) +
  geom_hline(yintercept = 0, linetype = 2) +

  theme_bw()


ggplot(data = CCMA.biovar.long.sum %>%
         ungroup() %>%
         filter(var.type == "Precip")) +

  geom_segment(aes(x = 0, xend = 1,y = historical,yend = ssp126,
                   color = as.factor(cat),group = interaction(as.factor(cat),model))) +

  geom_segment(aes(x = 1,xend = 2,y = ssp126,yend = ssp245,
                   color = as.factor(cat),group = interaction(as.factor(cat),model))) +

  geom_segment(aes(x = 2,xend = 3,y = ssp245,yend = ssp370,
                   color = as.factor(cat),group = interaction(as.factor(cat),model))) +

  geom_segment(aes(x = 3,xend = 4,y = ssp370,yend = ssp585,
                   color = as.factor(cat),group = interaction(as.factor(cat),model)),
               arrow = arrow(length = unit(0.2, "cm"))) +

  facet_wrap( ~ var, nrow = 2) +
  geom_hline(yintercept = 0, linetype = 2) +

  theme_bw()


CCMA.biovar.long.sum.long <-
  CCMA.biovar.long.sum %>%
  pivot_longer(cols = c(ssp126,ssp245,ssp370,ssp585),
               names_to = "scenario",
               values_to = "value") %>%
  mutate(scenario = as.numeric(factor(scenario,
                                      levels = c("ssp126","ssp245","ssp370","ssp585"))))

ggplot(data = CCMA.biovar.long.sum.long %>%
         ungroup()) +

  geom_segment(aes(x = 0,
                   xend = scenario,

                   y = historical,
                   yend = value - historical,

                   color = as.factor(cat),
                   group = interaction(as.factor(cat),
                                       model)),
               arrow = arrow(length = unit(0.2, "cm"))) +
  facet_wrap(~ var) +
  geom_hline(yintercept = 0, linetype = 2) +

  theme_bw()


