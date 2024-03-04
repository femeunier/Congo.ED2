rm(list = ls())

library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)

Window = 6
climate.sum.anomaly <- readRDS("./outputs/climate.anomalies.ERA5.RDS") %>%
  group_by(variable) %>%
  mutate(anomaly.month = value - mean.month - mean(value)) %>%
  dplyr::select(-c(time,slope,intercept,mean.obs,
                   detrended,mean.month,reconstructed)) %>%
  filter(year >= 1994) %>%
  group_by(variable) %>%
  mutate(value.rm = rollapply(value, width=Window,
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE, fill=NA, align="center"),
         anomaly.month.rm = rollapply(anomaly.month, width=Window,
                              FUN=function(x) mean(x, na.rm=TRUE),
                              partial=TRUE, fill=NA, align="center"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="center")) %>%
  pivot_wider(names_from = variable,
              values_from = c(value,value.rm,anomaly,anomaly.m,anomaly.m.rm,
                              anomaly.month,anomaly.month.rm))


GPPanomalies <- readRDS("./outputs/GPP.anomalies.ERA5.RDS")

combined <- climate.sum.anomaly %>%
  left_join(GPPanomalies %>%
              dplyr::select(year,month,source,anomaly.m,anomaly.m.rm),
            by = c("year","month")) %>%
  mutate(timing = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                            year == 2024 & month %in% c(1:3) ~ "2023",

                            year == 2016 & month %in% c(1:4) ~ "2015",
                            year == 2015 & month %in% c(8:12) ~ "2015",

                            year == 1997 & month %in% 9:12 ~ "1997",
                            year == 1998 & month %in% 1:5 ~ "1997",

                            TRUE ~ NA))


hull_cyl <- combined %>%
  filter(!is.na(timing)) %>%
  group_by(source,timing) %>%
  dplyr::slice(chull(anomaly.month.rm_tmp,anomaly.m.rm))

X = 2

ggplot(data = combined ,
       aes(x = anomaly.month.rm_tmp,
           y = anomaly.m.rm)) +

    geom_vline(xintercept = 0, color = "black", linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +

  geom_polygon(data = hull_cyl,
               aes(fill = as.factor(timing)),
               alpha = 0.2, show.legend = FALSE) +

  geom_point(size = 0.5,
             color = "grey") +
  geom_point(data = combined %>%
               filter(!is.na(timing)),
             aes(color = as.factor(timing))) +

  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +

  facet_wrap(~ source, nrow = 2) +
  geom_smooth(method = "lm", formula= y ~ poly(x,X),
              se = FALSE,
              color = "black") +
  theme_bw() +
  labs(x = "", y = "", color = "") +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

combined %>%
  group_by(source) %>%
  summarise(r2 = summary(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[["adj.r.squared"]],
            slope2 = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[3],
            slope = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[2],
            intercept = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_tmp,X)))[1],
            .groups = "keep")

stop()

################################################################################
# VPD

hull_cyl <- combined %>%
  filter(!is.na(timing)) %>%
  group_by(source,timing) %>%
  dplyr::slice(chull(anomaly.month.rm_VPD,anomaly.m.rm))

X = 2

ggplot(data = combined ,
       aes(x = anomaly.month.rm_VPD,
           y = anomaly.m.rm)) +

  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +

  geom_polygon(data = hull_cyl,
               aes(fill = as.factor(timing)),
               alpha = 0.2, show.legend = FALSE) +

  geom_point(size = 0.5,
             color = "grey") +
  geom_point(data = combined %>%
               filter(!is.na(timing)),
             aes(color = as.factor(timing))) +

  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +

  facet_wrap(~ source) +
  geom_smooth(method = "lm", formula= y ~ poly(x,X),
              se = FALSE,
              color = "black") +
  theme_bw() +
  labs(x = "", y = "", color = "") +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

combined %>%
  group_by(source) %>%
  summarise(r2 = summary(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_VPD,X)))[["adj.r.squared"]],
            slope2 = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_VPD,X)))[3],
            slope = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_VPD,X)))[2],
            intercept = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_VPD,X)))[1],
            .groups = "keep")

################################################################################
# Precip

hull_cyl <- combined %>%
  filter(!is.na(timing)) %>%
  group_by(source,timing) %>%
  dplyr::slice(chull(anomaly.month.rm_pre,anomaly.m.rm))

X = 2

ggplot(data = combined ,
       aes(x = anomaly.month.rm_pre,
           y = anomaly.m.rm)) +

  geom_vline(xintercept = 0, color = "black", linetype = 2) +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +

  geom_polygon(data = hull_cyl,
               aes(fill = as.factor(timing)),
               alpha = 0.2, show.legend = FALSE) +

  geom_point(size = 0.5,
             color = "grey") +
  geom_point(data = combined %>%
               filter(!is.na(timing)),
             aes(color = as.factor(timing))) +

  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +

  facet_wrap(~ source,nrow = 2) +
  geom_smooth(method = "lm", formula= y ~ poly(x,X),
              se = FALSE,
              color = "black") +
  theme_bw() +
  labs(x = "", y = "", color = "") +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())

combined %>%
  group_by(source) %>%
  summarise(r2 = summary(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_pre,X)))[["adj.r.squared"]],
            slope2 = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_pre,X)))[3],
            slope = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_pre,X)))[2],
            intercept = coef(lm(formula = anomaly.m.rm ~ poly(anomaly.month.rm_pre,X)))[1],
            .groups = "keep")





# ################################################################################
# # Attempt 2
#
# ggplot(data = combined ,
#        aes(x = anomaly.month.rm_tmp,
#            y = anomaly.m.rm)) +
#
#   geom_vline(xintercept = 0, color = "black", linetype = 2) +
#   geom_hline(yintercept = 0, color = "black", linetype = 2) +
#
#   geom_polygon(data = hull_cyl,
#                aes(fill = as.factor(timing),
#                    group = interaction(timing, source)),
#                alpha = 0.2, show.legend = FALSE) +
#
#   geom_point(size = 1,
#              aes(shape = source),
#              color = "grey") +
#   geom_point(data = combined %>%
#                filter(!is.na(timing)),
#              aes(color = as.factor(timing),
#                  shape = source)) +
#
#   scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
#   scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
#
#   geom_smooth(aes(linetype = source),
#               method = "lm", formula= y ~ poly(x,X),
#               se = FALSE,
#               color = "black") +
#   theme_bw() +
#   labs(x = "", y = "", color = "") +
#   guides(color = "none",
#          linetype = "none",shape = "none") +
#   theme(text = element_text(size = 20),
#         panel.grid = element_blank(),
#         strip.background = element_blank(),
#         strip.text = element_blank())
#
#
# # # Other vars
# # var = "anomaly.month.rm_dlwrf"
# #
# # hull_cyl <- combined %>%
# #   filter(!is.na(timing)) %>%
# #   group_by(source,timing) %>%
# #   dplyr::slice(chull(get(var),anomaly.m.rm))
# #
# #
# # ggplot(data = combined ,
# #        aes(x = get(var),
# #            y = anomaly.m.rm)) +
# #
# #   geom_vline(xintercept = 0, color = "black", linetype = 2) +
# #   geom_hline(yintercept = 0, color = "black", linetype = 2) +
# #
# #   geom_polygon(data = hull_cyl,
# #                aes(fill = as.factor(timing)),
# #                alpha = 0.2, show.legend = FALSE) +
# #
# #   geom_point(size = 0.5,
# #              color = "grey") +
# #   geom_point(data = combined %>%
# #                filter(!is.na(timing)),
# #              aes(color = as.factor(timing))) +
# #
# #   scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
# #   scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
# #
# #   facet_wrap(~ source) +
# #   geom_smooth(method = "lm", formula= y ~ poly(x,X),
# #               se = FALSE,
# #               color = "black") +
# #   theme_bw() +
# #   labs(x = "", y = "", color = "") +
# #   guides(color = "none") +
# #   theme(text = element_text(size = 20),
# #         panel.grid = element_blank(),
# #         strip.background = element_blank(),
# #         strip.text = element_blank())
# #
# # combined %>%
# #   group_by(source) %>%
# #   summarise(r2 = summary(lm(formula = anomaly.m.rm ~ poly(get(var),X)))[["adj.r.squared"]],
# #             slope2 = coef(lm(formula = anomaly.m.rm ~ poly(get(var),X)))[3],
# #             slope = coef(lm(formula = anomaly.m.rm ~ poly(get(var),X)))[2],
# #             intercept = coef(lm(formula = anomaly.m.rm ~ poly(get(var),X)))[1],
# #             .groups = "keep")
#
