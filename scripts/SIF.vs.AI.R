rm(list = ls())

library(zoo)

files <- c("RSanomalies.ERA5.product.RDS")

for (cfile in files){
  system2("scp",
          c(paste0("hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/",cfile),
            "./outputs/"))
}

A <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/outputs/RSanomalies.ERA5.product.RDS") %>%
  filter(product == "SIF")

plot(A$year + (A$month - 1/2)/12,
     A$anomaly.m,type = "l")

Amazon.coord <- readRDS("/home/femeunier/Documents/projects/Congo.vs.Amazon/data/Amazon.coord.ILF.RDS") %>%
  filter(model == "DLEM") %>%
  mutate(lon.lat = paste0(lon,".",lat))
all.df <- readRDS("/home/femeunier/Documents/projects/Congo.ED2/data/GPP/monthly/SIF.GPP.2023.RDS") %>%
  mutate(lon = case_when(abs(lon - floor(lon)-0.275) < 1e-6  ~ round(floor(lon) + 0.25,digits = 2),
                         abs(lon - floor(lon) - 0.775) < 1e-6  ~ round(floor(lon) + 0.75,digits = 2),
                         abs(lon - floor(lon)- 0.775) < 1e-6  ~ round(floor(lon) + 0.25,digits = 2),
                         abs(lon - floor(lon)- 0.725) < 1e-6  ~ round(floor(lon) + 0.75,digits = 2),
                         TRUE ~ lon)) %>%
  mutate(lon.lat = paste0(lon,".",lat))

all.df.filtered <- all.df %>%
  filter(lon.lat %in% Amazon.coord[["lon.lat"]])

df.sum <- all.df.filtered %>%
  group_by(year,month) %>%
  summarise(value.m = mean(value,
                           na.rm = TRUE),
            N = n(),
            .groups = "keep")

Window = 6

B <- df.sum %>%
  ungroup() %>%
  mutate(time = year + (month - 1/2)/12) %>%
  mutate(slope = coef(lm(value.m ~ time))[2],
         intercept = coef(lm(value.m ~ time))[1]) %>%

  mutate(mean.obs =  intercept + slope *time) %>%
  # mutate(mean.obs = mean(value.m,
  #                        na.rm = TRUE)) %>%
  mutate(detrended = value.m - mean.obs) %>%
  group_by(month) %>%
  mutate(mean.month = mean(detrended)) %>%
  mutate(anomaly = detrended - mean.month) %>%
  mutate(reconstructed = mean.obs + mean.month) %>%
  ungroup() %>%
  mutate(anomaly.m = anomaly/sd(anomaly)) %>%
  mutate(anomaly.rm = rollapply(anomaly, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"),
         anomaly.m.rm = rollapply(anomaly.m, width=Window,
                                  FUN=function(x) mean(x, na.rm=TRUE),
                                  partial=TRUE, fill=NA, align="right"),
         value.m.rm = rollapply(value.m, width=Window,
                                FUN=function(x) mean(x, na.rm=TRUE),
                                partial=TRUE, fill=NA, align="right"))

# B <- readRDS("~/Documents/projects/Congo.ED2/outputs/All.GPP.products.RDS") %>%
#   filter(product == "SIF") %>%
#   mutate(lon.lat = paste0(lon,".",lat)) %>%
#   filter(lon.lat %in% Amazon.coord[["lon.lat"]]) %>%
#   group_by(year,month) %>%
#   summarise(value.m = mean(value,
#                            na.rm = TRUE),
#             .groups = "keep") %>%
#   ungroup() %>%
#   mutate(time = year + (month - 1/2)/12) %>%
#   mutate(slope = coef(lm(value.m ~ time))[2],
#          intercept = coef(lm(value.m ~ time))[1]) %>%
#
#   mutate(mean.obs =  intercept + slope *time) %>%
#   # mutate(mean.obs = mean(value.m,
#   #                        na.rm = TRUE)) %>%
#   mutate(detrended = value.m - mean.obs) %>%
#   group_by(month) %>%
#   mutate(mean.month = mean(detrended)) %>%
#   mutate(anomaly = detrended - mean.month) %>%
#   mutate(reconstructed = mean.obs + mean.month) %>%
#   ungroup() %>%
#   mutate(anomaly.m = anomaly/sd(anomaly)) %>%
#   mutate(anomaly.rm = rollapply(anomaly, width=Window,
#                                 FUN=function(x) mean(x, na.rm=TRUE),
#                                 partial=TRUE, fill=NA, align="right"),
#          anomaly.m.rm = rollapply(anomaly.m, width=Window,
#                                   FUN=function(x) mean(x, na.rm=TRUE),
#                                   partial=TRUE, fill=NA, align="right"),
#          value.m.rm = rollapply(value.m, width=Window,
#                                 FUN=function(x) mean(x, na.rm=TRUE),
#                                 partial=TRUE, fill=NA, align="right"))

all <- B %>%
  dplyr::select(year,month,anomaly.m,value.m) %>%
  mutate(gpp = value.m *365/1000) %>%
  left_join(A %>%
              ungroup() %>%
              dplyr::select(year,month,anomaly.m,gpp.m) %>%
              rename(bis = anomaly.m),
            by = c("year","month"))

ggplot(all) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = anomaly)) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = bis),
            color = "red") +
  theme_bw()

ggplot(all) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = gpp)) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = gpp.m),
            color = "red") +
  # scale_x_continuous(limits = c(2005,2010)) +
  theme_bw()
