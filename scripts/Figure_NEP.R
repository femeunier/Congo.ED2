rm(list = ls())

library(dplyr)
library(ggplot2)
library(zoo)
library(tidyr)

A <- readRDS("./outputs/CC.anomaly.RDS")

A %>%
  filter(year == 2023, month %in% c(7:12))

MEM.area.anomalies.wide <- A  %>%
  filter(year <= 2021)

droughts <- data.frame(x1 = c(1997 + 9/12,
                              # 2010 + 7/12,
                              2015 + 8/12,
                              2023 + 7/12) + 0.5/12,
                       x2 = c(1998 + 4/12 ,
                              # 2010 + 10/12,
                              2016 + 3/12,
                              2024 + 2/12) + 0.5/12)

Window = 6

A2plot <- A %>%
  arrange(year,month) %>%
  ungroup() %>%
  filter(year <= 2021) %>%
  mutate(pred.m.rm_nep = rollapply(pred.m_nep, width=Window,
                               FUN=function(x) mean(x, na.rm=TRUE),
                               partial=TRUE, fill=NA, align="center"),
         time = year + (month - 1/2)/12)

ggplot(data = A2plot) +

  geom_rect(data = droughts,
            aes(xmin = x1, xmax = x2,
                ymin = -Inf, ymax = Inf), color = NA,
            alpha = 0.3, fill = "grey") +
  geom_point(aes(x = time,
                y = pred.m_nep),
             size = 0.2) +
  geom_line(aes(x = time,
                y = pred.m.rm_nep)) +
  stat_smooth(aes(x = time,
                  y = pred.m_nep),
              method = "lm",
              se = FALSE, color = "black",
              linetype = 2) +
  scale_x_continuous(limits = c(1994,2021)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  labs(x = "", y = "") +
  theme(text = element_text(size = 20))

summary(lm(data = A2plot,
           formula = pred.m_nep ~ time))
coef(lm(data = A2plot,
           formula = pred.m_nep ~ time))

ggplot(data = A2plot) +
  geom_density(aes(x = pred.m_nep),
               color = NA, fill = "grey",
               alpha = 0.7) +
  theme_minimal() +
  coord_flip() +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "", y = "") +
  scale_x_continuous(breaks = c()) +
  scale_y_continuous(breaks = c()) +
  theme(text = element_text(size = 20))


(A2plot %>%
  filter(pred.m_nep < 0) %>%
  nrow())/nrow(A2plot)

A2plot %>%
  filter(year <= 2011) %>%
  pull(pred.m_nep) %>%
  summary()


data <- readRDS("./data/Afritron/Amazon.ts.RDS")
plot(data$year,data$AGBnetchange.ha.yr,type = "l")

data %>%
  filter(year >= 1994) %>%
  pull(AGBnetchange.ha.yr) %>% summary()

y_new = approx(data$year, data$AGBnetchange.ha.yr,
               xout=(A2plot$time))[["y"]]


MEM2plot <- MEM.area.anomalies.wide %>%
  mutate(timing = case_when(year == 2023 & month %in% c(7:12) ~ "2023",
                                       year == 2016 & month %in% c(1:3) ~ "2015",
                                       year == 2015 & month %in% c(10:12) ~ "2015",
                                       year == 1997 & month %in% 9:12 ~ "1997",
                                       year == 1998 & month %in% 1:4 ~ "1997",
                                       TRUE ~ NA_character_)) %>%
  filter(year >= 1994)

hull_cyl <- MEM2plot %>%
  filter(!is.na(timing)) %>%
  group_by(timing) %>%
  dplyr::slice(chull(anomaly.m_gpp,anomaly.m_nep))

ggplot(data = MEM2plot ,
       aes(x = anomaly.m_gpp,
           y = anomaly.m_nep)) +
  geom_point(size = 0.5,
             color = "grey") +
  geom_point(data = MEM2plot %>%
               filter(!is.na(timing)),
             aes(color = timing),
             size = 0.5) +
  geom_polygon(data = hull_cyl,
               aes(fill = as.factor(timing)),
               alpha = 0.2, show.legend = FALSE) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  geom_abline(slope = 1,
              intercept = 0, linetype = 2) +
  stat_smooth(method = "lm", se = FALSE,
              color = "black", fill = "grey", alpha = 0.5) +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  theme_bw() +
  scale_x_continuous(limits = c(-5,2)) +
  scale_y_continuous(limits = c(-5,2)) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme(text = element_text(size = 20))



X = 1

MEM2plot2 <- MEM2plot %>%
  dplyr::select(year,month,timing,
                anomaly.m_gpp,anomaly.m_nep,
                anomaly_gpp,anomaly_nep) %>%
  pivot_longer(-c(year,month,timing)) %>%
  mutate(var1 = sub("\\_.*", "", name),
         var2 = sub(".*\\_", "", name)) %>%
  dplyr::select(-name) %>%
  pivot_wider(names_from = var2,
              values_from = value)

hull_cyl2 <- MEM2plot2 %>%
  filter(!is.na(timing)) %>%
  group_by(timing,var1) %>%
  dplyr::slice(chull(gpp,nep))

ggplot(data = MEM2plot2 ,
       aes(x = gpp,
           y = nep)) +
  geom_point(size = 0.5,
             color = "grey") +
  geom_point(data = MEM2plot2 %>%
               filter(!is.na(timing)),
             aes(color = timing),
             size = 0.5) +
  geom_polygon(data = hull_cyl2,
               aes(fill = as.factor(timing)),
               alpha = 0.2, show.legend = FALSE) +
  scale_color_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02","#7570b3")) +
  geom_abline(slope = 1,
              intercept = 0, linetype = 2) +
  geom_smooth(method = "lm", formula= y ~ poly(x,X),
              se = FALSE,
              color = "black") +
  geom_hline(yintercept = 0,linetype = 2, color = "black") +
  geom_vline(xintercept = 0,linetype = 2, color = "black") +
  theme_bw() +
  facet_wrap(~ var1, scales = "free") +
  # scale_x_continuous(limits = c(-5,2)) +
  # scale_y_continuous(limits = c(-5,2)) +
  labs(x = "",y = "") +
  guides(color = "none",
         fill = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

MEM2plot2 %>%
  group_by(var1) %>%
  summarise(r2 = summary(lm(formula = nep ~ gpp))[["r.squared"]],
            slope = coef(lm(formula = nep ~ gpp))[2],
            intercept = coef(lm(formula = nep ~ gpp))[1],
            .groups = "keep")



summary(lm(data = MEM2plot,
   formula = anomaly.m_nep ~ anomaly.m_gpp))
coef(lm(data = MEM2plot,
           formula = anomaly.m_nep ~ anomaly.m_gpp))
