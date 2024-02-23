rm(list = ls())

Trendy.data <-
  readRDS("./outputs/Amazon.mean.test.JRA.historical.IFL.RDS") %>%
  filter(year %in% c(1994:2021)) %>%
  mutate(timing = case_when(
    (year == 2016 & month %in% c(1:3)) |
      (year == 2015 & month %in% c(10:12)) |
      (year == 1997 & month %in% c(10:12)) |
      (year == 1998 & month %in% 1:4)~ "Drought",
    year >= 1994 ~ "Other",
    TRUE ~ "Before"
  )) %>%
  mutate(residual = pred - obs)

A <- Trendy.data %>%
  group_by(model,timing) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  group_by(model) %>%
  mutate(Ntot = sum(N)) %>%
  pivot_wider(names_from = timing,
              values_from = N) %>%
  mutate(ratio = Drought/Ntot)


RS.data <- readRDS("./outputs/test.predictions.SIF.ILF.RDS") %>%
  filter(product %in% c("SIF","SIF2","VOD","NIR")) %>%
  filter(year  %in% c(1994:2021)) %>%
  mutate(timing = case_when(
    (year == 2016 & month %in% c(1:3)) |
      (year == 2015 & month %in% c(10:12)) |
      (year == 1997 & month %in% c(10:12)) |
      (year == 1998 & month %in% 1:4)~ "Drought",
    year >= 1994 ~ "Other",
    TRUE ~ "Before"
  )) %>%
  mutate(residual = pred - obs)

all <- bind_rows(Trendy.data %>%
                   mutate(source = "TrENDY"),
                 RS.data %>%
                   mutate(source = "RS"))

ggplot(data = all %>%
         filter(timing %in% c("Drought","Other"))) +
  geom_density(aes(x = residual, fill = timing),
               alpha = 0.5) +
  facet_wrap(~ source) +
  theme_bw()

Trendy.data.sorted <- all %>%
  filter(timing %in% c("Drought","Other")) %>%
  group_by(timing,source) %>%
  arrange(residual) %>%
  mutate(cdens = (as.numeric(1:n()))/n())


ggplot(data = Trendy.data.sorted) +
  geom_line(aes(x = residual, y = 100*cdens, color = timing)) +
  facet_wrap(~ source) +
  theme_bw() +
  labs(x = "", y = "") +
  guides(color = "none") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

library(scales)
hue_pal()(2)
