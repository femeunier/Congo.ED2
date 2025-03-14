rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(lubridate)

cmonth <- month(today())

Window = 6

files <- c("climate.sum.anomaly.select.RDS",
           "climate.sum.anomaly.selected.RDS")

for (cfile in files){
  system2("scp",
          c(paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
            "./outputs/"))
}

coord <- readRDS("./outputs/Coord.ILF.ERA5.RDS") %>%
  # filter(model == "ORCHIDEE") %>%
  mutate(lon.lat = paste0(round(lon,digits = 2),".",round(lat,digits = 2)))

droughts <- data.frame(x1 = c(1997,2015,2023) + 0.5/12,
                       x2 = c(1998,2016,2023) +
                         11.5/12)


climate.sum.anomaly.select <- readRDS("./outputs/climate.sum.anomaly.select.RDS")

climate.sum.anomaly.select.group <- climate.sum.anomaly.select %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:(cmonth + 1)) ~ "2023",

                            # year == 2009 & month %in% c(8:12) ~ "2010",
                            # year == 2010 & month %in% c(1:5) ~ "2010",

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


ggplot() +

  geom_line(data = climate.sum.anomaly.select,
            aes(x = month,
                y = value,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.sum.anomaly.select %>%
              filter(year >= 1994 & year <= 2023) %>%
              group_by(variable,month) %>%
              summarise(value.m = mean(value,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = month,
                y = value.m),
            linewidth = 0.8,
            linetype = 1) +

  geom_line(data = climate.sum.anomaly.select.group %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = value,
                color = as.factor(groups)),
            linewidth =  0.8,
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +

  # geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~ variable,scales = "free") +

  theme_bw() +
  labs(x = "", y = "") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


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

# ggplot() +
#
#   geom_line(data = climate.sum.anomaly.select.mod,
#             aes(x = month.mod,
#                 y = value,
#                 group = year),
#             color = "grey", size = 0.15) +
#
#   geom_line(data = climate.sum.anomaly.select.mod %>%
#               group_by(variable,month.mod) %>%
#               summarise(value.m = mean(value),
#                         .groups = "keep"),
#             aes(x = month.mod,
#                 y = value.m),
#             linetype = 1) +
#
#   geom_line(data = climate.sum.anomaly.select.group.mod %>%
#               filter(!is.na(groups)),
#             aes(x = month.mod,
#                 y = value,
#                 color = as.factor(groups)),
#             show.legend = FALSE) +
#
#   scale_x_continuous(breaks = 1:12,
#                      labels = c("J","A","S","O","N","D",
#                                 "J","F","M","A","M","J")) +
#   scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
#
#   # geom_smooth(method = "lm", formula = y~x) +
#   facet_wrap(~ variable,scales = "free") +
#
#   theme_bw() +
#   labs(x = "", y = "") +
#   theme(panel.grid = element_blank()) +
#   theme(text = element_text(size = 20),
#         strip.background = element_blank(),
#         strip.text = element_blank())

climate.sum.anomaly.select.group %>%
  filter(variable == "tmp") %>%
  arrange(desc(anomaly.m))


climate.sum.anomaly.select %>%
  filter(variable == "tmp",
         month == 10) %>%
  arrange(value) %>%
  summarise(mean(value))


climate.sum.anomaly.select %>%
  ungroup() %>%
  filter(month %in% c(7:12),
         year == 2023) %>%
  # filter(variable == "pre") %>%
  group_by(variable) %>%
  summarise(anomaly.m = mean(anomaly.m,
                             na.rm = TRUE),
            anomaly = mean(anomaly),
            value.m = mean(value,
                           na.rm = TRUE))


climate.sum.anomaly.select %>%
  filter(year <= 2023) %>%
  # filter(month %in% c(1,2,7:12)) %>%
  filter(variable == "tmp") %>%
  group_by(variable) %>%
  summarise(anomaly.m = mean(anomaly.m,
                             na.rm = TRUE),
            anomaly = mean(anomaly),
            value.m = mean(value,
                           na.rm = TRUE))


climate.sum.anomaly.select %>%
  ungroup() %>%
  filter(month %in% c(7:12),
         year == 2023) %>%
  filter(variable == "pre") %>%
  group_by(year) %>%
  summarise(m = mean(value,
                     na.rm = TRUE)) %>%
  arrange(desc(m)) %>%
  ungroup() %>%
  summarise(m = mean(m))

climate.sum.anomaly.select %>%
  filter(month %in% c(1:6)) %>%
  filter(variable == "tmp") %>%
  group_by(year) %>%
  summarise(m = mean(value,
                     na.rm = TRUE)) %>%
  arrange(desc(m))


climate.sum.anomaly.select.group %>%
  filter(!is.na(groups)) %>%
  group_by(groups,variable) %>%
  summarise(m = mean(value,na.rm = TRUE))


climate.sum.anomaly.select.group %>%
  filter(groups == "2023") %>%
  filter(variable == "tmp") %>%
  pull(value) %>% mean(na.rm = TRUE)

climate.sum.anomaly.select %>%
  filter(year < 2023) %>%
  filter(variable == "tmp") %>%
  filter(month %in% c(7:12,1:4)) %>%
  pull(value) %>% mean()

climate.sum.anomaly.select.group %>%
  filter((year == 2024 & month %in% c(1:4)) |
         (year == 2023 & month %in% c(7:12))) %>%
  filter(variable == "pre") %>%
  pull(anomaly) %>% mean(na.rm = TRUE)



# climate.sum.anomaly %>%
#   filter(variable %in% c("pre","tmp")) %>%
#   group_by(variable) %>%
#   summarise(pvalue )


climate.sum.anomaly.selected <- readRDS("./outputs/climate.sum.anomaly.selected.RDS")


climate.sum.anomaly.selected.group <- climate.sum.anomaly.selected %>%
  mutate(groups =  case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                             year == 2024 & month %in% c(1:(cmonth + 1)) ~ "2023",

                             # year == 2009 & month %in% c(8:12) ~ "2010",
                             # year == 2010 & month %in% c(1:5) ~ "2010",

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

ggplot() +

  geom_line(data = climate.sum.anomaly.selected %>%
              filter(year %in% 1994:2023),
            aes(x = month,
                y = anomaly.m,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.sum.anomaly.selected.group %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = anomaly.m,
                color = as.factor(groups)),
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +  ##e7298a
  theme_bw() +
  labs(x = "", y = "") +
  facet_wrap(~ variable) +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


climate.sum.anomaly.selected.mod <-
  climate.sum.anomaly.selected %>%
  mutate(month.mod = month - 6) %>%
  mutate(month.mod = case_when(month.mod <= 0 ~ month.mod + 12,
                               TRUE ~ month.mod))

climate.sum.anomaly.selected.group.mod <-
  climate.sum.anomaly.selected.group %>%
  mutate(month.mod = month - 6) %>%
  mutate(month.mod = case_when(month.mod <= 0 ~ month.mod + 12,
                               TRUE ~ month.mod))

# ggplot() +
#
#   geom_line(data = climate.sum.anomaly.selected.mod %>%
#               filter(year %in% 1994:2023),
#             aes(x = month.mod,
#                 y = anomaly.m,
#                 group = year),
#             color = "grey", size = 0.15) +
#
#   geom_line(data = climate.sum.anomaly.selected.group.mod %>%
#               filter(!is.na(groups)),
#             aes(x = month.mod,
#                 y = anomaly.m,
#                 color = as.factor(groups)),
#             show.legend = FALSE) +
#
#   scale_x_continuous(breaks = 1:12,
#                      labels = c("J","A","S","O","N","D",
#                                 "J","F","M","A","M","J")) +
#   geom_hline(yintercept = 0, linetype = 2,
#              color = "black") +
#   scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
#   theme_bw() +
#   labs(x = "", y = "") +
#   facet_wrap(~ variable) +
#   theme(panel.grid = element_blank()) +
#   theme(text = element_text(size = 20),
#         strip.background = element_blank(),
#         strip.text = element_blank())



ggplot() +

  geom_line(data = climate.sum.anomaly.selected %>%
              filter(year %in% 1994:2024),
            aes(x = month,
                y = anomaly,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.sum.anomaly.selected.group %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = anomaly,
                color = as.factor(groups)),
            linewidth =  0.8,
            show.legend = FALSE) +

  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  geom_hline(yintercept = 0, linetype = 2,
             color = "black") +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  theme_bw() +
  labs(x = "", y = "") +
  facet_wrap(~ variable, scales = "free") +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())


