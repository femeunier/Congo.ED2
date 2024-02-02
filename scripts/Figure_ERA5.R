rm(list = ls())

system2("scp",
        c("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/df.ERA5.RDS",
          "/home/femeunier/Documents/projects/Congo.ED2/outputs/"))

df.ERA5 <- readRDS("./outputs/df.ERA5.RDS")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
                      layer = "amazon_sensulatissimo_gmm_v1")

ggplot(data = df.ERA5 %>%
         filter(year == 2023,
                month == 10)) +
  geom_raster(aes(x=lon,y = lat,
                  fill = tmp - 273.15),alpha = 0.7) +
  geom_sf(data = world,fill = NA,
          color = "grey") +
  geom_sf(data = Amazon.shp,fill = NA, color = "black") +

  coord_sf(xlim = c(-90, -35), ylim = c(-25, 25), expand = FALSE) +
  scale_fill_gradient(limits = c(25,30),
                      oob = scales::squish,
                      low = "white",high = "darkred") +

  labs(x = "",y = "") +
  theme_bw()


ggplot(data = df.ERA5 %>%
         filter(year == 2023,
                month == 10)) +
  geom_density(aes(x=tmp - 273.15),
               color = NA, fill = "grey",
               alpha = 0.7) +
  theme_bw()

df.ERA5.sum <- df.ERA5 %>%
  group_by(year,month) %>%
  summarise(pre = mean(MAP),
            tmp = mean(tmp),
            .groups = "keep")

df.ERA5.sum.long <- df.ERA5.sum %>%
  pivot_longer(cols = c(pre,tmp),
               names_to = "variable")

df.JRA.sum.long <- readRDS("./outputs/climate.anomalies.RDS") %>%
  filter(variable %in% c("tmp","pre"))

all <- bind_rows(df.ERA5.sum.long %>%
                   dplyr::mutate(source = "ERA5"),
                 df.JRA.sum.long %>%
                   dplyr::select(year,month,variable,value) %>%
                   dplyr::mutate(source = "JRA")) %>%
  filter(year >= 1994) %>%
  mutate(value = case_when(variable == "pre" ~ value,
                           TRUE ~ value - 273.15))

all.wide <- all %>%
  pivot_wider(names_from = "source",
              values_from = "value")

ggplot(data = all.wide,
       aes(x = JRA, y = ERA5)) +
  geom_point(size = 0.5, color = "grey") +
  geom_point(data = all.wide %>%
               filter(year == 2023, month %in% c(10)),
             size = 1, color = "darkblue") +
  stat_smooth(method = "lm",
              se = FALSE, color = "black") +
  facet_wrap(~ variable,scales = "free") +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  theme_bw() +
  labs(x = "",y = "") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

all.wide %>%
  group_by(variable) %>%
  summarise(r2 = summary(lm(ERA5 ~ JRA))[["r.squared"]],
            slope = coef(lm(ERA5 ~ JRA))[2],
            intercept = coef(lm(ERA5 ~ JRA))[1],
            .groups = "keep")


all.anomalies <- all %>%
  group_by(variable, source) %>%
  mutate(mean.obs = mean(value)) %>%
  mutate(detrended = value - mean.obs) %>%
  group_by(variable, source, month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  group_by(variable, source, month) %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  ungroup() %>%
  dplyr::select(year,month,variable,value,source,anomaly,anomaly.m)

all.anomalies.wide <- all.anomalies %>%
  pivot_wider(names_from = source,
              values_from = c(value,anomaly,anomaly.m))

ggplot(data = all.anomalies.wide,
       aes(x = anomaly.m_JRA, y = anomaly.m_ERA5)) +
  geom_point(size = 0.5, color = "grey") +
  geom_point(data = all.anomalies.wide %>%
               filter(year == 2023, month %in% c(10)),
             size = 1, color = "darkblue") +
  stat_smooth(method = "lm",
              se = FALSE, color = "black") +
  facet_wrap(~ variable,scales = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  theme_bw() +
  labs(x = "",y = "") +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        strip.text = element_blank())

all.anomalies.wide %>%
  group_by(variable) %>%
  summarise(r2 = summary(lm(anomaly.m_ERA5 ~ anomaly.m_JRA))[["r.squared"]],
            slope = coef(lm(anomaly.m_ERA5 ~ anomaly.m_JRA))[2],
            intercept = coef(lm(anomaly.m_ERA5 ~ anomaly.m_JRA))[1],
            .groups = "keep")


################################################################################

all %>%
  filter(year == 2023,
         month == 10)

ggplot(data = all) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value,
                color = source)) +
  facet_wrap(~ variable, scales = "free") +
  scale_x_continuous(limits = c(1994,2024)) +
  theme_bw()

df.SC <- all %>%
  group_by(source,variable,month) %>%
  summarise(value.m = mean(value),
            .groups = "keep")

ggplot(data = df.SC) +
  geom_line(aes(x = month,
                y = value.m,
                color = source)) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()


df.lt.ts <- all %>%
  group_by(source,variable,year) %>%
  summarise(value.m = case_when(variable == "pre" ~ sum(value),
                                TRUE ~ mean(value)),
            .groups = "keep") %>%
  distinct() %>%
  arrange(year)

ggplot(data = df.lt.ts) +
  geom_line(aes(x = year,
                y = value.m,
                color = source)) +
  facet_wrap(~ variable, scales = "free") +
  scale_x_continuous(limits = c(1994,2023)) +
  theme_bw()

