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

climate <- readRDS("./outputs/monthly.climate.pantropical.ERA5.RDS") %>%
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

climate.sum <- bind_rows(climate.sum,
                         climate.sum %>%
                           filter(year == 2024,
                                  month == 1) %>%
                           mutate(month = 3,
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

# ggplot(data = climate.sum,
#        aes(x = year + (month - 1/2)/12,
#            y = value)) +
#   geom_line(data = climate.sum.anomaly,
#             aes(y = reconstructed),
#             color = "red", size = 0.4) +
#   geom_line() +
#   facet_wrap(~ variable,scales = "free") +
#   scale_x_continuous(limits = c(1990,2024),
#                      expand = c(0,0)) +
#   stat_smooth(method = "lm",
#               color = "red",
#               se = FALSE) +
#   theme_bw()

droughts <- data.frame(x1 = c(1997,2015,2023) + 0.5/12,
                       x2 = c(1998,2016,2023) +
                         11.5/12)

# ggplot() +
#   geom_rect(data = droughts,
#             aes(xmin = x1, xmax = x2,
#                 ymin = -Inf, ymax = Inf), color = NA,
#             alpha = 0.3, fill = "grey") +
#   geom_line(data = climate.sum.anomaly,
#             aes(x = year + (month - 1/2)/12,
#                 y = anomaly.m)) +
#   facet_wrap(~ variable,scales = "free") +
#   scale_x_continuous(limits = c(1990,2024),
#                      expand = c(0,0)) +
#   scale_y_continuous(limits = c(-1,1)*3.5) +
#   geom_hline(yintercept = 0, linetype = 2, color = "black") +
#   theme_bw()

# ggplot() +
#   geom_rect(data = droughts,
#             aes(xmin = x1, xmax = x2,
#                 ymin = -Inf, ymax = Inf), color = NA,
#             alpha = 0.3, fill = "grey") +
#   geom_line(data = climate.sum.anomaly %>%
#               filter(variable %in% c("pre","tmp")),
#             aes(x = year + (month - 1/2)/12,
#                 y = mean.obs),
#             linetype = 2) +
#   geom_line(data = climate.sum.anomaly %>%
#               filter(variable %in% c("pre","tmp")),
#             aes(x = year + (month - 1/2)/12,
#                 y = value)) +
#   stat_smooth(method = "lm", se = TRUE,linetype = 2) +
#   # geom_smooth(method = "lm", formula = y~x) +
#   facet_wrap(~ variable,scales = "free") +
#   scale_x_continuous(limits = c(1990,2024),
#                      expand = c(0,0)) +
#   theme_bw() +
#   labs(x = "", y = "") +
#   theme(panel.grid = element_blank()) +
#   theme(text = element_text(size = 20),
#         strip.background = element_blank(),
#         strip.text = element_blank())


climate.sum.anomaly.select <- climate.sum.anomaly %>%
  filter(variable %in% c("pre","tmp"))

climate.sum.anomaly.select %>%
  mutate(time = year + (month - 1/2)/12) %>%
  group_by(variable) %>%
  summarise(r2 = summary(lm(formula = value ~ time))[["r.squared"]],
            pval = summary(lm(formula = value ~ time))[["coefficients"]][2,4])


climate.sum.anomaly.select.group <- climate.sum.anomaly.select %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:3) ~ "2023",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA)) %>%
  mutate(value = case_when(!is.na(groups) ~ value,
                               TRUE ~ NA_real_))%>%
  mutate(value = case_when((year == 1998 & month == 5) |
                             (year == 2016 & month == 4) |
                             (year == 2024 & month == 3) ~ NA,
                               TRUE ~ value)) %>%
  arrange(year,month,variable)


ggplot() +

  geom_line(data = climate.sum.anomaly.select,
            aes(x = month,
                y = value,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.sum.anomaly.select %>%
              group_by(variable,month) %>%
              summarise(value.m = mean(value,na.rm = TRUE),
                        .groups = "keep"),
            aes(x = month,
                y = value.m),
            linetype = 1) +

  geom_line(data = climate.sum.anomaly.select.group %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = value,
                color = as.factor(groups)),
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
  ungroup() %>%
  filter(month %in% c(10),
         year == 2023) %>%
  # filter(variable == "pre") %>%
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


# climate.sum.anomaly %>%
#   filter(variable %in% c("pre","tmp")) %>%
#   group_by(variable) %>%
#   summarise(pvalue )


climate.sum.anomaly.selected <- climate.sum.anomaly %>%
  filter(variable %in% c("tmp","pre")) %>%
  dplyr::select(year,month,variable,anomaly.m,anomaly,value)

climate.sum.anomaly.selected.group <- climate.sum.anomaly.selected %>%
  mutate(groups = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:3) ~ "2023",

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
                             (year == 2024 & month == 3) |
                                 (year == 2016 & month == 4) ~ NA,
                               TRUE ~ value),
         anomaly.m = case_when((year == 1998 & month == 5) |
                                 (year == 2024 & month == 3) |
                                 (year == 2016 & month == 4) ~ NA,
                            TRUE ~ anomaly.m),
         anomaly = case_when((year == 1998 & month == 5) |
                               (year == 2024 & month == 3) |
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
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
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
              filter(year %in% 1994:2023),
            aes(x = month,
                y = anomaly,
                group = year),
            color = "grey", size = 0.15) +

  geom_line(data = climate.sum.anomaly.selected.group %>%
              filter(!is.na(groups)),
            aes(x = month,
                y = anomaly,
                color = as.factor(groups)),
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



# ggplot() +
#
#   geom_line(data = climate.sum.anomaly.selected.mod %>%
#               filter(year %in% 1994:2023),
#             aes(x = month.mod,
#                 y = anomaly,
#                 group = year),
#             color = "grey", size = 0.15) +
#
#   geom_line(data = climate.sum.anomaly.selected.group.mod %>%
#               filter(!is.na(groups)),
#             aes(x = month.mod,
#                 y = anomaly,
#                 color = as.factor(groups)),
#             show.legend = FALSE) +
#
#   scale_x_continuous(breaks = 1:12,
#                      labels = c("J","A","S","O","N","D",
#                                 "J","F","M","A","M","J")) +
#   geom_hline(yintercept = 0, linetype = 2,
#              color = "black") +
#   scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
#   # geom_vline(xintercept = 6.5, color = "black",linetype = 2) +
#   theme_bw() +
#   labs(x = "", y = "") +
#   facet_wrap(~ variable, scales = "free") +
#   theme(panel.grid = element_blank()) +
#   theme(text = element_text(size = 20),
#         strip.background = element_blank(),
#         strip.text = element_blank())


