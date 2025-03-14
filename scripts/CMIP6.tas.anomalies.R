rm(list = ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(zoo)

# system2("rsync",
#         paste("-avz","hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/df.monthly.tas.*",
#               "./outputs/"))

l.files <- list.files("./outputs",pattern = "df.monthly.tas.*")
l.files <- l.files[!grepl("pantropical|basin",l.files)]
OP.files.no.ext <- tools::file_path_sans_ext((l.files))
all.attributes <- strsplit(OP.files.no.ext,split = "\\.")

scenars <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                        function(i){
                                                          data.frame(var = all.attributes[[i]][4])}))))
models <- as.character(as.vector(unlist(purrr::map_dfr(1:length(all.attributes),
                                                       function(i){
                                                         data.frame(var = all.attributes[[i]][5])}))))
data.sum <- data.sum.sum <- data.frame()
for (ifile in seq(1,length(l.files))){

  print(ifile)
  cscenario <- scenars[ifile]

  cdata <- readRDS(paste0("./outputs/",l.files[ifile])) %>%
    mutate(model = models[ifile])

  data.sum <- bind_rows(list(data.sum,
                             cdata %>%
                               group_by(year,month,scenario,model) %>%
                               summarise(tas.m = mean(tas,na.rm = TRUE),
                                         .groups = "keep")))

}

df.models <- data.sum %>%
  dplyr::select(scenario,model) %>%
  distinct() %>%
  mutate(exist = TRUE) %>%
  pivot_wider(names_from = scenario,
             values_from = exist) %>%
  filter(historical,ssp126,ssp245,
         ssp370,ssp585)

models <- unique(df.models$model)
scenarios <- colnames(df.models %>%
                        dplyr::select(-model))

data.mod <- data.frame()
for (cmodel in models){
  for (cscenario in scenarios){
    if (cscenario == "historical"){

      cdf <- data.sum %>%
        filter(model == cmodel,
               scenario == cscenario)

    } else {

      cdf <- data.sum %>%
        filter(model == cmodel,
               scenario %in% c("historical",cscenario)) %>%
        mutate(scenario = cscenario)

    }

    data.mod <- bind_rows(data.mod,
                          cdf)

  }
}

data.mod <- data.mod %>%
  filter(tas.m <= 400)

Window = 6

# stop()
#
# saveRDS(data.mod,
#         "~/Documents/projects/SIAmazon/data/CMIP6.tas.RDS")

data.mod.anomalies <- data.mod %>%
  filter((year >= 1994 & scenario != "historical") |
           ((year %in% 1985:2014 & scenario == "historical")))%>%
  group_by(model,scenario) %>%
  mutate(global.mean = mean(tas.m[year <= 2023],na.rm = TRUE)) %>%
  group_by(model,scenario,month) %>%
  mutate(mean.month = mean(tas.m[year <= 2023] - global.mean[year <= 2023],
                           na.rm = TRUE)) %>%
  group_by(model,scenario) %>%
  mutate(anomaly.month = tas.m - mean.month - global.mean) %>%
  mutate(anomaly.month.rm = rollapply(anomaly.month, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center"))

data.mod.anomalies.sum <- data.mod.anomalies %>%
  group_by(year,scenario) %>%
  summarise(tas.m = mean(tas.m,na.rm = TRUE),
            anomaly.month.rm.m = mean(anomaly.month.rm,na.rm = TRUE),
            .groups = "keep")

ggplot(data = data.mod.anomalies.sum) +
  geom_line(aes(x = year,
                y = tas.m,
                color = scenario)) +
  theme_bw()


ggplot(data = data.mod.anomalies.sum) +
  geom_line(aes(x = year,
                y = anomaly.month.rm.m,
                color = scenario)) +
  theme_bw()


# coord <- readRDS("./outputs/Amazon.coord.ILF.RDS") %>%
#   filter(model == "ORCHIDEE") %>%
#   mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))
#
# climate <- readRDS("./outputs/monthly.climate.pantropical.JRA.historical.RDS") %>%
#   filter(year >= 1985)

coord <- readRDS("./outputs/Coord.ILF.ERA5.RDS") %>%
  # filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

climate <- bind_rows(
  readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
  filter(year >= 1985, year <= 2023),
  readRDS("/home/femeunier/Documents/data/monthly.climate.global.ERA5.2024.RDS") %>%
    filter(month <= 5))

climate.select <- climate %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2))) %>%
  filter(lon.lat %in% coord[["lon.lat"]])

data <- climate.select %>%
  dplyr::select(lon,lat,tmp,year,month) %>%
  group_by(year,month) %>%
  summarise(value = mean(tmp),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(global.mean = mean(value[year >= 1994 &year <= 2023],na.rm = TRUE)) %>%
  group_by(month) %>%
  mutate(mean.month = mean(value[year >= 1994 & year <= 2023] - global.mean[year >= 1994 & year <= 2023],
                           na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(anomaly.month = value - mean.month - global.mean) %>%
  mutate(anomaly.month.rm = rollapply(anomaly.month, width=Window,
                                      FUN=function(x) mean(x, na.rm=TRUE),
                                      partial=TRUE, fill=NA, align="center"))


CMIP.vs.JRA <- data.mod.anomalies %>%
  filter(scenario == "historical") %>%
  ungroup() %>%
  dplyr::select(year,month,model,tas.m) %>%
  left_join(data %>%
              ungroup() %>%
              dplyr::select(year,month,value),
            by = c("year","month"))

weights <- CMIP.vs.JRA %>%
  group_by(model) %>%
  summarise(RMSE = sqrt(sum((tas.m-value)**2,na.rm = TRUE)),
            bias = mean(abs(tas.m - value),na.rm = TRUE),
            .groups = "keep") %>%
  arrange(RMSE) %>%
  ungroup() %>%
  slice_head(n = 10) %>%
  mutate(w = 1/bias**2) %>%
  mutate(w = w/sum(w)) %>%
  arrange(model)

saveRDS(weights,
        "./outputs/weights.tas.RDS")

sum(round(weights$w, digits = 2))
ggplot() +
  geom_line(data = data.mod.anomalies %>%
              filter(scenario == "historical",
                     model == weights %>% filter(w == max(w)) %>% pull(model)),
            aes(x = year + (month - 1/2)/12,
                y = tas.m),
            color = "red") +
  geom_line(data = data,
            aes(x = year + (month - 1/2)/12,
                y = value)) +

  theme_bw()

ggplot() +
  geom_line(data = data.mod.anomalies %>%
              filter(scenario == "historical",
                     model == weights %>% filter(w == max(w)) %>% pull(model)) %>%
              group_by(year) %>%
              summarise(tas.m = mean(tas.m)),
            aes(x = year,
                y = tas.m),
            color = "red") +
  geom_line(data = data %>%
              group_by(year) %>%
              summarise(value = mean(value)),
            aes(x = year,
                y = value)) +

  theme_bw()

data.mod.anomalies.sum <- data.mod.anomalies %>%
  group_by(scenario,year,model,month) %>%
  summarise(tas.m = mean(tas.m),
            anomaly.month.rm = mean(anomaly.month.rm),
            .groups = "keep") %>%
  ungroup() %>%
  left_join(weights,
            by = "model") %>%
  group_by(scenario,year,month) %>%
  filter(!is.na(RMSE)) %>%
  summarise(anomaly.month.rm = weighted.mean(anomaly.month.rm,
                                             w = 1/bias**2,
                                             na.rm = TRUE),
            tas.m.m = weighted.mean(tas.m,
                                    w = 1/bias**2,
                                    na.rm = TRUE),
            .groups = "keep")


data.mod.anomalies.sum <- data.mod.anomalies.sum %>%
  ungroup() %>%
  mutate(global.mean = mean(tas.m.m[year <= 2023],na.rm = TRUE)) %>%
  group_by(scenario,month) %>%
  mutate(mean.month = mean(tas.m.m[year <= 2023] - global.mean[year <= 2023],
                           na.rm = TRUE)) %>%
  group_by(scenario) %>%
  mutate(anomaly.month2 = tas.m.m - mean.month - global.mean) %>%
  mutate(anomaly.month.rm2 = rollapply(anomaly.month2, width=Window,
                                      FUN=function(x) mean(x, na.rm=TRUE),
                                      partial=TRUE, fill=NA, align="center"))

ggplot(data = data.mod.anomalies.sum %>%
         group_by(year,month) %>%
         mutate(N = n()) %>%
         filter(N == 4 | (N == 5 & scenario == "historical"))) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = tas.m.m -273.15,
                color = scenario)) +
  geom_line(data = data,
            aes(x = year + (month - 1/2)/12,
                y = value-273.15),
            color = "black",linetype = 2) +
  # geom_smooth(data = data.mod.anomalies.sum %>%
  #               group_by(year,month) %>%
  #               mutate(N = n()) %>%
  #               filter(N == 4 | (N == 5 & scenario == "historical")),
  #             aes(x = year + (month - 1/2)/12,
  #                 y = rollmean(tas.m.m-273.15, k = 12,na.pad = TRUE),
  #                 color = scenario),
  #             se = FALSE) +
  #
  # geom_smooth(data = data,
  #             aes(x = year + (month - 1/2)/12,
  #                 y = rollmean(value-273.15, k = 12,na.pad = TRUE)),
  #             color = "black", linetype = 1, se = FALSE) +
  scale_color_manual(values = c("grey","#263b5d","#8b9bac","#b48a40","#6a2d31"))+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none") +
  labs(x = "",y = "") +

  scale_x_continuous(limits = c(1994,2100))



d.select <- data.mod.anomalies.sum %>%
  filter(year %in% c(2024:2053)) %>%
  group_by(scenario) %>%
  mutate(time = year + (month -1/2)/12,
         tas.m.m = tas.m.m - 273.15)

d.select %>%
  summarise(slope = coef(lm(formula = tas.m.m ~ time))[2]*10,   # Per decade
            .groups = "keep")

ggplot(data = d.select,
       aes(x = time, y = tas.m.m,
           color = scenario)) +
  # geom_line() +
  stat_smooth(method = "lm", se = FALSE) +
  theme_bw()

ggplot(data = data.mod.anomalies.sum %>%
         group_by(year,month) %>%
         mutate(N = n()) %>%
         filter(N == 4 | (N == 5 & scenario == "historical")) %>%
         group_by(year,scenario) %>%
         summarise(tas.m.m = mean(tas.m.m,na.rm = TRUE),
                   .groups = "keep")) +
  geom_line(aes(x = year,
                y = tas.m.m -273.15,
                color = scenario)) +
  geom_line(data = data %>%
               filter(year < 2024) %>%
              group_by(year) %>%
              summarise(value = mean(value,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = year ,
                y = value-273.15),
            color = "black",linetype = 1) +

  # geom_point(data = data %>%
  #             filter(year == 2024) %>%
  #             group_by(year) %>%
  #             summarise(value = mean(value,na.rm = TRUE),
  #                       .groups = "keep"),
  #           aes(x = year ,
  #               y = value-273.15),
  #           color = "black",linetype = 1) +

  scale_color_manual(values = c("grey","#263b5d","#8b9bac","#b48a40","#6a2d31"))+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  guides(color = "none") +
  labs(x = "",y = "") +
  scale_y_continuous(limits = c(24,31)) +
  scale_x_continuous(limits = c(1994,2100))


ggplot(data = data.mod.anomalies.sum %>%
         group_by(year,month) %>%
         mutate(N = n()) %>%
         filter(N == 4 | (N == 5 & scenario == "historical")),) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly.month.rm2,
                color = scenario)) +
  geom_line(data = data,
            aes(x = year + (month - 1/2)/12,
                y = anomaly.month.rm)) +
  scale_color_manual(values = c("grey","#263b5d","#8b9bac","#b48a40","#6a2d31"))+

  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  guides(color = "none") +
  labs(x = "",y = "") +

  scale_x_continuous(limits = c(1994,2100)) +
  theme_bw() +
  theme(text = element_text(size = 20))


all2plot <- bind_rows(data.mod.anomalies.sum %>%
                        group_by(scenario) %>%
                        filter((scenario == "historical" & year %in% c(1985:2014)) |
                                 (scenario != "historical" & year %in% c(2024:2053))) ,
                      data %>%
                        filter(year >= 1994) %>%
                        # filter(year <= 2014) %>%
                        mutate(scenario = "ERA5")) %>%
  mutate(scenario = factor(scenario,
                           levels = c("ssp585","ssp370","ssp245","ssp126",
                                      "historical","ERA5")))

ggplot(data = all2plot) +


  geom_boxplot(aes(y = anomaly.month.rm,
                   x = scenario,
                   fill = scenario),
               alpha = 0.7) +
  # geom_point(data = all2plot %>%
  #              filter((year == 2023 & month > 6) |
  #                       (year == 2024),
  #                     scenario == "ERA5"),
  #            aes(y = anomaly.month.rm,
  #                x = scenario),
  #            color = "red") +

  geom_vline(xintercept = 0, linetype = 2) +
  # facet_wrap(~ scenario) +
  labs(x = "",y = "") +
  scale_fill_manual(values = c("#6a2d31","#b48a40","#8b9bac","#263b5d","grey","black"))+
  geom_hline(linetype = 2,yintercept = 0) +
  scale_y_continuous(breaks = (-1:5)*0.5) +
  coord_flip() +
  # scale_y_continuous() +
  guides(fill = "none") +

  theme_bw() +
  theme(text = element_text(size = 20))

data %>%
  filter(month > 6,
         year == 2023) %>%
  pull(anomaly.month.rm) %>%
  mean()


all2plot %>%
  group_by(scenario) %>%
  mutate(Q1 = quantile(anomaly.month.rm,0.25),
            Q3 = quantile(anomaly.month.rm,0.75)) %>%
  mutate(IQR = (Q3 - Q1)) %>%
  summarise(low = max(c(min(anomaly.month.rm),Q1 - 1.5*IQR)),
            high = min(c(max(anomaly.month.rm),Q3 + 1.5*IQR)),
            Q1 = unique(Q1),
            Q3 = unique(Q3))

all2plot %>%
  filter(scenario == "ERA5",
         year == 2023)

all2plot %>%
  filter(scenario == "ERA5",
         anomaly.month.rm >= 0.74) %>%
  arrange(desc(anomaly.month.rm))

all2plot %>%
  group_by(scenario) %>%
  summarise(med = median(anomaly.month.rm),
            m = mean(anomaly.month.rm),
            .groups = "keep")

all2plot %>%
  filter(year == 2023,
         month %in% 7:12) %>%
  summarise(m = mean(anomaly.month.rm),
            m2 = mean(anomaly.month))

################################################################################

all2plot2 <- bind_rows(data.mod.anomalies.sum %>%
                         group_by(scenario) %>%
                        filter((scenario == "historical" & year %in% c(1985:2014)) |
                                 (scenario != "historical" & year %in% c(2071:2100))) ,
                      data %>%
                        filter(year >= 1994) %>%
                        mutate(scenario = "ERA5")) %>%
  mutate(scenario = factor(scenario,
                           levels = c("ssp585","ssp370","ssp245","ssp126",
                                      "historical","ERA5")))

ggplot(data = all2plot2) +

  geom_boxplot(aes(y = anomaly.month.rm,
                   x = scenario,
                   fill = scenario),
               alpha = 0.7) +

  geom_vline(xintercept = 0, linetype = 2) +
  # facet_wrap(~ scenario) +
  labs(x = "",y = "") +
  scale_fill_manual(values = c("#6a2d31","#b48a40","#8b9bac","#263b5d","grey","black"))+
  geom_hline(linetype = 2,yintercept = 0) +
  scale_y_continuous(breaks = (-1:12)*0.5) +
  coord_flip() +
  # scale_y_continuous() +
  guides(fill = "none") +

  theme_bw() +
  theme(text = element_text(size = 14))

