rm(list = ls())

# Add MODIS GPP and NPP
# Add Fluxcom NEE

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(Congo.ED2)
library(lubridate)

carbon_content = 0.456
default.woody.ratio.of.npp = 0.3

################################################################################
# Inventories

# Afritron
Afritron <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Afritron/2_data_by_censusinterval.csv") %>%
  dplyr::select(Continent,Country,PlotCode,PlotArea,Lat,Lon,Plot.View.Name,Census.Int,
                AGB_ini.ha,AGB_fin.ha,int_c_length,
                int_ini,int_fin,
                AGBnetchange.ha.yr,AGBGain_tot.ha.yr,AGBMor_tot.ha.yr,
                ClusterCode)

Afritron.sum <- Afritron %>%
  group_by(PlotCode,PlotArea) %>%
  summarise(Country = unique(Country),
            Lat = unique(Lat),
            Lon = unique(Lon),
            Plot.View.Name = unique(Plot.View.Name),
            AGB_ini.ha = AGB_ini.ha[1],
            AGB_fin.ha = AGB_fin.ha[length(AGB_fin.ha)],
            int_ini = int_ini[1],
            int_fin = int_fin[length(int_fin)],
            AGBnetchange.ha.yr = weighted.mean(AGBnetchange.ha.yr,
                                      int_c_length,
                                      na.rm = TRUE),
            AGBGain_tot.ha.yr = weighted.mean(AGBGain_tot.ha.yr,
                                     int_c_length,
                                     na.rm = TRUE),
            AGBMor_tot.ha.yr = weighted.mean(AGBMor_tot.ha.yr,
                                    int_c_length,
                                    na.rm = TRUE),
            int_c_length = sum(int_c_length),
            ClusterCode = unique(ClusterCode),
            .groups = "keep") %>%
  mutate(Continent = "Africa")


Afritron.raw <- Afritron %>%
  rowwise() %>%
  summarise(Country = (Country),
            PlotArea = unique(PlotArea),
            PlotCode = unique(PlotCode),
            Lat = (Lat),
            Lon = (Lon),
            Plot.View.Name = (Plot.View.Name),
            AGB_ini.ha = AGB_ini.ha[1],
            AGB_fin.ha = AGB_fin.ha[length(AGB_fin.ha)],
            int_ini = int_ini[1],
            int_fin = int_fin[length(int_fin)],
            AGBnetchange.ha.yr = weighted.mean(AGBnetchange.ha.yr,
                                               int_c_length,
                                               na.rm = TRUE),
            AGBGain_tot.ha.yr = weighted.mean(AGBGain_tot.ha.yr,
                                              int_c_length,
                                              na.rm = TRUE),
            AGBMor_tot.ha.yr = weighted.mean(AGBMor_tot.ha.yr,
                                             int_c_length,
                                             na.rm = TRUE),
            int_c_length = sum(int_c_length),
            ClusterCode = unique(ClusterCode),
            .groups = "keep") %>%
  mutate(Continent = "Africa")

#################################################################################
# Rainfor

data.census <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Rainfor/data_by_cens.csv") %>%
  dplyr::select(PlotViewName,
                int_ini,int_fin,int_c_length,
                AGB_ini,AGB_fin,
                plot_area,
                AGBnetchange,AGBGain_tot,AGBMor_tot)

ggplot(data = data.census) +
  geom_density(aes(x = (AGB_ini+AGB_fin)/2*carbon_content)) +  # Average (/2) + biomass2C (/2)
  theme_bw()

summary(data.census$AGB_ini/10*carbon_content)

ggplot(data = data.census) +
  geom_point(aes(x = plot_area, y = AGB_ini*carbon_content)) +
  theme_bw()

Plot.rainfor <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Rainfor/metadata.csv") %>%
  dplyr::select(CountryName,PlotCode,LatitudeDecimal,LongitudeDecimal,PlotArea,PlotViewName,ClusterCode) %>%
  rename(Country = CountryName,
         Lat = LatitudeDecimal,
         Lon = LongitudeDecimal)

Rainfor <- data.census %>%
  left_join(Plot.rainfor,
            by = "PlotViewName") %>%
  rename(Plot.View.Name = PlotViewName,
         AGB_ini.ha = AGB_ini,
         AGB_fin.ha = AGB_fin,
         AGBnetchange.ha.yr = AGBnetchange,
         AGBGain_tot.ha.yr = AGBGain_tot,
         AGBMor_tot.ha.yr = AGBMor_tot) %>%
  arrange(PlotCode,int_ini)

# Wrong coding
Rainfor$int_c_length[which(Rainfor$PlotCode=="ELD-03")[1]] <- (Rainfor$int_fin[which(Rainfor$PlotCode=="ELD-03")[1]] -
                                                            Rainfor$int_ini[which(Rainfor$PlotCode=="ELD-03")[1]])

Rainfor.sum <- Rainfor %>%
  group_by(PlotCode,PlotArea) %>%
  summarise(Country = unique(Country),
            Lat = unique(Lat),
            Lon = unique(Lon),
            Plot.View.Name = unique(Plot.View.Name),
            AGB_ini.ha = AGB_ini.ha[1],
            AGB_fin.ha = AGB_fin.ha[length(AGB_fin.ha)],
            int_ini = int_ini[1],
            int_fin = int_fin[length(int_fin)],
            AGBnetchange.ha.yr = weighted.mean(AGBnetchange.ha.yr,
                                               int_c_length,
                                               na.rm = TRUE),
            AGBGain_tot.ha.yr = weighted.mean(AGBGain_tot.ha.yr,
                                              int_c_length,
                                              na.rm = TRUE),
            AGBMor_tot.ha.yr = weighted.mean(AGBMor_tot.ha.yr,
                                             int_c_length,
                                             na.rm = TRUE),
            int_c_length = sum(int_c_length),
            ClusterCode = unique(ClusterCode),
            .groups = "keep")  %>%
  mutate(Continent = "Amazon")

Rainfor.raw <- Rainfor %>%
  rowwise() %>%
  summarise(Country = unique(Country),
            PlotArea = unique(PlotArea),
            PlotCode = unique(PlotCode),
            Lat = unique(Lat),
            Lon = unique(Lon),
            Plot.View.Name = unique(Plot.View.Name),
            AGB_ini.ha = AGB_ini.ha[1],
            AGB_fin.ha = AGB_fin.ha[length(AGB_fin.ha)],
            int_ini = int_ini[1],
            int_fin = int_fin[length(int_fin)],
            AGBnetchange.ha.yr = weighted.mean(AGBnetchange.ha.yr,
                                               int_c_length,
                                               na.rm = TRUE),
            AGBGain_tot.ha.yr = weighted.mean(AGBGain_tot.ha.yr,
                                              int_c_length,
                                              na.rm = TRUE),
            AGBMor_tot.ha.yr = weighted.mean(AGBMor_tot.ha.yr,
                                             int_c_length,
                                             na.rm = TRUE),
            int_c_length = sum(int_c_length),
            ClusterCode = unique(ClusterCode),
            .groups = "keep")  %>%
  mutate(Continent = "Amazon")

################################################################################
# Asia

data.Asia <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Borneo/41467_2017_1997_MOESM4_ESM.csv",stringsAsFactors = FALSE) %>%
  filter(Edge.Effects == "no")

Asia.sum <- data.frame(Continent = "Asia",
                         Country = data.Asia$Country,
                         PlotCode = data.Asia$Plot.Code,
                         PlotArea = data.Asia$Plot.Area,
                         Lat = data.Asia$Latitude.Decimal,
                         Lon = data.Asia$Longitude.Decimal,
                         Plot.View.Name = NA,
                         AGB_ini.ha = data.Asia$Initial.AGB ,
                         AGB_fin.ha = data.Asia$Initial.AGB + data.Asia$Plot.Mean.AGB.Change*data.Asia$Monitoring.Length,
                         int_c_length = data.Asia$Monitoring.Length,
                         int_ini = data.Asia$First.Census.Decimal.Date,
                         int_fin = data.Asia$Last.Census.Decimal.Date,
                         AGBnetchange.ha.yr = data.Asia$Plot.Mean.AGB.Change,
                         AGBGain_tot.ha.yr = data.Asia$AGWP,
                         AGBMor_tot.ha.yr = data.Asia$AGB.Mortality,
                         ClusterCode = data.Asia$Site)

# Merge
networks <- bind_rows(list(Afritron.sum,
                           Rainfor.sum,
                           Asia.sum))

# networks <- bind_rows(list(Afritron %>% mutate(Continent = "Africa"),
#                            Rainfor %>% mutate(Continent = "Amazon"),
#                            Asia.sum))

# Add Sullivan
sullivan <- read.csv("/home/femeunier/Documents/projects/Congo.ED2/data/Sullivan/MultiCensusPlots.csv")
extra.plots <- sullivan %>% filter(!(PlotCode %in% (networks %>% pull(PlotCode)))) %>%
  filter(!(PlotCode %in% c("ALP-01a","ALP-02a","ALP-02b"))) # double --> not need

extra.plots %>%
  group_by(Continent) %>%
  summarise(N = length(Area))

extra.plots %>%
  filter(Continent == "SA") %>%
  pull(PlotCode) %>% unique() %>% sort()

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +

  geom_point(data = bind_rows(list(networks %>%
                                     ungroup() %>%
                                     dplyr::select(Lat,Lon) %>%
                                     mutate(Sullivan = FALSE),
                                   extra.plots %>%
                                       dplyr::select(Lat,Long) %>%
                                     rename(Lon = Long) %>%
                                     mutate(Sullivan = TRUE))),
             aes(x = Lon, y = Lat, color = Sullivan), alpha = 0.5) +

  geom_sf(data = world,
          fill = NA) +
  coord_sf(xlim = c(-80, 150),
           ylim = c(-20, 12)) +
  labs(x = "",y = "") +
  theme_bw()

plot2addfromSullivan <-  extra.plots %>%
  # filter(Continent != "AU") %>%                                # Let's not consider Australia
  dplyr::select(PlotCode,Area,
                Lat,Long,
                AGB,
                FirstCens,LastCens,MonitorLength,Midpoint,
                Mort,AGWP,
                Continent) %>%
  rename(PlotArea = Area,
         Lon = Long,
         AGB_ini.ha = AGB,
         int_ini = FirstCens,
         int_fin = LastCens,
         int_c_length = MonitorLength,
         int_int = Midpoint,
         AGBMor_tot.ha.yr = Mort,
         AGBGain_tot.ha.yr = AGWP) %>%
  mutate(Country = NA,
         Plot.View.Name = NA,
         Sullivan = TRUE,
         ClusterCode = substr(PlotCode,1,3),
         AGBnetchange.ha.yr = AGBGain_tot.ha.yr - AGBMor_tot.ha.yr,
         AGB_fin.ha = AGB_ini.ha + (AGBnetchange.ha.yr)*int_c_length,
         Continent = case_when(Continent == "SA" ~ "Amazon",
                               Continent == "AF" ~ "Africa",
                               Continent == "AS" ~ "Asia",
                               TRUE ~ "Australia"))

networks <- bind_rows(list(networks %>%
                             mutate(Sullivan = FALSE),
                           plot2addfromSullivan
                        ))



networks %>%
  group_by(ClusterCode,Continent) %>%
  summarise(N = length(Lat),
            Nlat = length(unique(round(Lat))),
            Nlon = length(unique(round(Lon))),
            Ninit = length(unique(floor(int_ini))),
            .groups = "keep") %>%
  arrange(desc(Nlon))


# Cluster averages

networks.av <- networks %>%
  mutate(w = int_c_length*PlotArea) %>%
  group_by(ClusterCode,Continent) %>%
  summarise(N = length(PlotCode),

            Country = unique(Country),

            PlotArea = mean(PlotArea, na.rm = TRUE),

            Lat = weighted.mean(Lat,w, na.rm = TRUE),
            Lon = weighted.mean(Lon,w, na.rm = TRUE),

            AGB_ini.ha = weighted.mean(AGB_ini.ha,w, na.rm = TRUE),
            AGB_fin.ha = weighted.mean(AGB_fin.ha,w, na.rm = TRUE),

            int_ini = weighted.mean(int_ini,w, na.rm = TRUE),
            int_fin = weighted.mean(int_fin,w, na.rm = TRUE),
            int_c_length = weighted.mean(int_c_length,w, na.rm = TRUE),

            AGBnetchange.ha.yr = weighted.mean(AGBnetchange.ha.yr,w, na.rm = TRUE),
            AGBGain_tot.ha.yr = weighted.mean(AGBGain_tot.ha.yr,w, na.rm = TRUE),
            AGBMor_tot.ha.yr = weighted.mean(AGBMor_tot.ha.yr,w, na.rm = TRUE),

            .groups = "keep")

doubleganger <-
  networks %>%
  group_by(PlotCode) %>%
  summarise(N = length(Continent)) %>%
  filter(N > 1)

networks.mod <- networks %>%
  group_by(PlotCode) %>%
  mutate(nr = 1:length(Lat)) %>%
  mutate(PlotCode = case_when(PlotCode %in% as.character(doubleganger[["PlotCode"]]) ~ paste0(PlotCode,as.character(nr), sep = "."),
                              TRUE ~ as.character(PlotCode))) %>%
  dplyr::select(-nr) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia")),
         int_int = (int_ini + int_fin)/2)


bind_rows(list(
  networks.mod %>%
  group_by(Continent) %>%
  summarise(N.site = length(PlotArea),
            N.years = sum(int_c_length),
            N.years.ha = sum(int_c_length*PlotArea)),
  networks.mod %>%
    mutate(Continent = "Total") %>%
    group_by(Continent) %>%
    summarise(N.site = length(PlotArea),
              N.years = sum(int_c_length),
              N.years.ha = sum(int_c_length*PlotArea))))


saveRDS(networks,"./outputs/Afritron+Rainfor+Asia+Sullivan.RDS")

all.raws <- bind_rows(Afritron.raw,
                      Rainfor.raw,
                      Asia.sum,
                      plot2addfromSullivan)

################################################################################
# RS

observed.GPP <- bind_rows(list(readRDS("./data/GPP/monthly/all.df.GPP2.RDS") %>%
                                 mutate(model = "Madani"),
                               readRDS("./data/GPP/monthly/all.df.GPP.RDS") %>%
                                 mutate(model = "Zhang"),
                               readRDS("./data/GPP/df.all.GPP3.RDS") %>%
                                 mutate(model = "Fluxcom"),
                               readRDS("./data/GPP/monthly/all.df.GPP.MODIS.RDS") %>%
                                 mutate(model = "MODIS")
                               )) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

networks.long <- data.frame()

for (i in seq(1,nrow(all.raws))){

  print(i/nrow(all.raws))

  cdf <- all.raws[i,]
  years <- floor(cdf %>% pull(int_ini)): ceiling(cdf %>% pull(int_fin))
  Nyears <- length(years)
  df2replicate <- cdf

  networks.long <- bind_rows(networks.long,
                             df2replicate[rep(seq_len(nrow(df2replicate)), Nyears), ] %>%
                               ungroup() %>%
                               mutate(year = years) )
}

all.coords <- networks.long %>%
  dplyr::select(Lat,Lon) %>%
  distinct()
search.GPP <- observed.GPP %>%
  filter(month == 1, year == 2010) %>%
  filter(!is.na(daily.GPP))

all.df.GPP <- readRDS("./outputs/all.df.GPP.RDS")

# all.df.GPP <- data.frame()
# for (icoord in seq(1,nrow(all.coords))){
#
#   print(icoord/nrow(all.coords))
#   cdf <- all.raws %>%
#     filter(Lat == all.coords[["Lat"]][icoord] & Lon == all.coords[["Lon"]][icoord])
#
#   clon <- unique(cdf$Lon) ; clat <- unique(cdf$Lat)
#
#   GPP.model.lat.lons <- find.coord.raster(search.GPP %>%
#                                       group_by(model),
#                                     target = c(clon,clat),
#                                     Ngridcells = 1) %>%
#     unique()
#
#   cobserved.GPP <- observed.GPP %>%
#     filter(model.lat.lon %in% c(GPP.model.lat.lons))
#
#   for (itime in seq(1,nrow(cdf))){
#
#     start.date <- cdf$int_ini[itime] ; end.date <- cdf$int_fin[itime]
#
#     obs.gpp <- cobserved.GPP %>%
#       mutate(time = year + (month-1/2)/12) %>%
#       filter(time >= start.date & time <= end.date) %>%
#       group_by(model) %>%
#       summarise(obs.GPP = mean(daily.GPP)*365*10/1000,
#                 .groups = "keep")
#
#     all.df.GPP <- bind_rows(list(all.df.GPP,
#                                  data.frame(Lon = clon,
#                                             Lat = clat,
#                                             int_ini = start.date,
#                                             int_fin = end.date,
#                                             obs.gpp)))
#   }
# }
# saveRDS(all.df.GPP,"./outputs/all.df.GPP.RDS")

networks.long.GPP <- networks.long %>%
  left_join(all.df.GPP %>% distinct(),
            by = c("Lon","Lat","int_ini","int_fin")) %>%
  mutate(obs.GPP  = case_when( (year < 2000 | year > 2016) & model == "Zhang" ~ NA_real_,
                               (year < 1982 | year > 2016) & model == "Madani" ~ NA_real_,
                               (year < 1950 | year > 2017) & model == "Fluxcom" ~ NA_real_,
                               (year < 2000 | year > 2022) & model == "MODIS" ~ NA_real_,
                              TRUE ~ obs.GPP))

data.observed <- bind_rows(list(networks.long.GPP %>%
  mutate(weights = (int_fin - int_ini)**(1/3) + (PlotArea)**(1/4) - 1) %>%
  group_by(Continent,year) %>%
  dplyr::select(-c(model,obs.GPP)) %>%
  mutate(model = "Inventories") %>%
    distinct() %>%
  summarise(N = mean(c(length(AGBnetchange.ha.yr[!is.na(AGBnetchange.ha.yr)]),
                       length(AGBMor_tot.ha.yr[!is.na(AGBMor_tot.ha.yr)]),
                       length(AGBGain_tot.ha.yr[!is.na(AGBGain_tot.ha.yr)]))),
            sink = weighted.mean(AGBnetchange.ha.yr*carbon_content,weights,na.rm = TRUE),
            mort = weighted.mean(AGBMor_tot.ha.yr*carbon_content,weights,na.rm = TRUE),
            growth = weighted.mean(AGBGain_tot.ha.yr*carbon_content,weights,na.rm = TRUE),
            NPP = weighted.mean(AGBGain_tot.ha.yr*carbon_content/default.woody.ratio.of.npp,weights,na.rm = TRUE),
            .groups = "keep") %>%
  pivot_longer(cols = c(N,sink,growth,mort,NPP),
               names_to = "variable",
               values_to = "value"),
  networks.long.GPP %>%
    mutate(weights = (int_fin - int_ini)**(1/3) + (PlotArea)**(1/4) - 1) %>%
    group_by(Continent,model,year) %>%
    summarise(GPP = weighted.mean(obs.GPP,weights,na.rm = TRUE),
              .groups = "keep") %>%
    pivot_longer(cols = c(GPP),
                 names_to = "variable",
                 values_to = "value"))) %>%
  ungroup() %>%
  complete(Continent = factor(c("Amazon","Africa","Asia","Australia")),
           year = min(year):max(year),
           variable = factor(c("sink","growth","mort","GPP","NPP","N")),
           fill = list(value = NA)) %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia"))) %>%
  mutate(variable = factor(variable,
                           levels = c("sink","growth","mort","N","GPP","NPP"))) %>%
  mutate(model = case_when(variable != "GPP" ~ "Inventory",
                           TRUE ~ model)) %>%
  ungroup() %>%
  distinct() %>%
  filter(year <= 2012,
         year>= 1985,
         !is.na(model))

ggplot() +
  geom_line(data = data.observed %>% filter(variable != "N"),
            aes(x = year,
                y = value,
                color = model),linetype = 1) +

  geom_hline(yintercept = 0, linetype = 3) +
  facet_grid(variable ~ Continent) +
  theme_bw()


ggplot() +
  geom_line(data = data.observed %>% filter(variable == "N"),
            aes(x = year,
                y = value), color = "black",linetype = 1) +

  geom_hline(yintercept = 0, linetype = 3) +
  facet_grid(variable ~ Continent) +
  theme_bw()

saveRDS(all.raws,"./outputs/Afritron+Rainfor+Asia+Sullivan_raw.RDS")
saveRDS(data.observed,"./outputs/Afritron+Rainfor+Asia+Sullivan_raw_long.RDS")

bind_rows(list(
  networks.av %>%
    group_by(Continent) %>%
    summarise(N.site = length(PlotArea),
              N.years = sum(int_c_length),
              N.years.ha = sum(int_c_length*PlotArea)),
  networks.av %>%
    mutate(Continent = "Total") %>%
    group_by(Continent) %>%
    summarise(N.site = length(PlotArea),
              N.years = sum(int_c_length),
              N.years.ha = sum(int_c_length*PlotArea))))


saveRDS(networks.av,"./outputs/Afritron+Rainfor+Asia+Sullivan_av.RDS")


networks.long <- networks.av %>%
  pivot_longer(cols = c(PlotArea,AGB_ini.ha,AGBnetchange.ha.yr,AGBGain_tot.ha.yr,AGBMor_tot.ha.yr),
               names_to = "variable",
               values_to = "value") %>%
  mutate(Continent = factor(Continent,
                            levels = c("Amazon","Africa","Asia","Australia")),
         int_int = (int_ini + int_fin)/2)

ggplot(data = networks.long) +
  geom_density(aes(x = value, fill = Continent), color = NA, alpha = 0.5) +
  facet_wrap(~ variable, scales = "free",nrow=1) +
  theme_bw()

ggplot(data = networks.long %>%
         dplyr::filter(variable %in% c("AGBnetchange.ha.yr",
                                       "AGBGain_tot.ha.yr",
                                       "AGBMor_tot.ha.yr"))) +
  geom_boxplot(aes(y = value, x = Continent, fill = Continent), outlier.shape = NA) +
  scale_y_continuous(limits = c(-10,10)) +
  labs(x = "", y = "AGB net change") +
  facet_wrap(~ variable) +
  theme_bw()

networks.long %>%
  group_by(variable,Continent) %>%
  summarise(m = mean(value, na.rm = TRUE),
            med = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            .groups = "keep")

networks.long.decade <- networks.long %>%
  mutate(decade = floor(int_int/10)*10) %>%
  group_by(Continent,decade,variable) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = networks.long.decade) +
  geom_line(aes(x = decade, y = value.m, color = Continent)) +
  facet_wrap(~ variable, scales = "free",nrow=1) +
  theme_bw()

networks2plot <- networks %>%
  mutate(netGain = case_when(AGB_fin.ha >= AGB_ini.ha ~ TRUE,
                             TRUE ~ FALSE),
         LargePlot = case_when(PlotArea >= 1 ~ TRUE,
                           TRUE ~ FALSE)) %>%
  mutate(AGB_fin.ha.rel = AGB_fin.ha - AGB_ini.ha,
         AGB_fin.ha.rel2 = (AGB_fin.ha/AGB_ini.ha - 1)*100,
         AGB_ini.ha.rel = 0)

all.raws2plot <- all.raws %>%
  mutate(netGain = case_when(AGB_fin.ha >= AGB_ini.ha ~ TRUE,
                             TRUE ~ FALSE),
         LargePlot = case_when(PlotArea >= 1 ~ TRUE,
                               TRUE ~ FALSE)) %>%
  mutate(AGB_fin.ha.rel = AGB_fin.ha - AGB_ini.ha,
         AGB_fin.ha.rel2 = (AGB_fin.ha/AGB_ini.ha - 1)*100,
         AGB_ini.ha.rel = 0)

lm.networks2plot <- bind_rows(list(networks2plot %>%
  dplyr::select(int_ini,AGB_ini.ha,Continent,PlotArea) %>% rename(time = int_ini,
                                               AGB = AGB_ini.ha),
  networks2plot %>%
    dplyr::select(int_fin,AGB_fin.ha,Continent,PlotArea) %>% rename(time = int_fin,
                                                 AGB = AGB_fin.ha)
))

ggplot(data = all.raws2plot) +

  geom_point(aes(x = int_ini, y = AGB_ini.ha),
             show.legend = FALSE, color = "black", size = 0.2) +

  geom_point(aes(x = int_fin, y = AGB_fin.ha),
             show.legend = FALSE, color = "black", size = 0.2) +


  geom_segment(aes(x = int_ini, xend = int_fin,
                   y = AGB_ini.ha, yend = AGB_fin.ha,
                   color = netGain,
                   linetype = LargePlot),
               show.legend = FALSE) +


  facet_wrap(~ Continent) +
  scale_color_manual(values = c("#de2d26","#2ca25f")) +
  scale_x_continuous(breaks = seq(1960,2020,20),
                     limits = c(1960,2020)) +
  scale_y_continuous(limits = c(0,1250),
                     breaks = c(0,250,500,750,1000)) +
  stat_smooth(data = lm.networks2plot,
              aes(x = time, y = AGB, weight = PlotArea),
              color = "black", alpha = 0.5,
              method = "lm") +
  scale_linetype_manual(values = c(2,1)) +
  labs(x = "", y = "AGB (Mg/ha)") +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(angle = 0, hjust = 0),
        panel.spacing = unit(2, "lines"))


lm(data = lm.networks2plot,
   formula = AGB ~ time,
   weights = lm.networks2plot$PlotArea)

fitted_models = lm.networks2plot %>%
  group_by(Continent) %>% do(model = lm(AGB ~ time, data = .,
                                        weighted = PlotArea))
summary(fitted_models$model[[3]])


ggplot(data = all.raws2plot) +
  geom_histogram(aes(x = AGBnetchange.ha.yr, fill = netGain), bins = 100,
                 show.legend = FALSE) +
  scale_fill_manual(values = c("#de2d26","#2ca25f")) +
  facet_wrap(~ Continent, scales = "free") +
  labs(x = "",y = "",fill = "") +
  scale_x_continuous(limits = c(-10,10)) +
  scale_y_continuous(limits = c(0,30)) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_minimal() +
  theme(text = element_text(size = 14),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.1),
        axis.line = element_line(colour = 'black', linewidth = 0.1),
        strip.text = element_blank(),
        panel.spacing = unit(2, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave(plot = last_plot(),
       filename = "./Figures/density_sink.png",
       width = 12, height = 4, units = "cm", dpi = 300)

ggplot(data = all.raws2plot %>%
         mutate(Continent = factor(Continent,
                                   levels = c("Amazon","Africa","Asia","Australia"))),
       aes(y = AGBnetchange.ha.yr, x = as.factor(Continent))) +
  # geom_violin()+
  geom_jitter(aes(color = netGain),
              size = 0.5,
              width = 0.25, show.legend = FALSE) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_color_manual(values = c("#de2d26","#2ca25f")) +
  scale_x_discrete(labels = c("","","")) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(y = "Sink (Mg/ha/yr)", x = "") +
  facet_wrap(~ Continent, scales = "free_y") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_text(angle = 0, hjust = 0))


ggplot(data = all.raws2plot) +
  geom_segment(aes(x = int_ini, xend = int_fin,
                   y = AGB_ini.ha.rel, yend = AGB_fin.ha.rel,
                   color = netGain,
                   linetype = LargePlot),
               show.legend = FALSE) +
  geom_point(aes(x = int_fin, y = AGB_fin.ha.rel),
             color = "black",size = 0.2,
             show.legend = FALSE) +
  facet_wrap(~ Continent) +
  scale_color_manual(values =  c("red","Darkgreen")) +
  scale_x_continuous(breaks = seq(1960,2020,20),
                     limits = c(1960,2020)) +
  scale_linetype_manual(values = c(2,1)) +
  labs(x = "", y = "AGB change (Mg/ha)") +
  theme_minimal()

library(lubridate)

all.raws2plot %>% filter(Continent == "Africa",
                         year(int_ini)  < 2015,
                         year(int_fin)  > 2017)

ggplot(data = all.raws2plot) +
  geom_segment(aes(x = int_ini, xend = int_fin,
                   y = AGB_ini.ha.rel, yend = AGB_fin.ha.rel2,
                   color = netGain,
                   linetype = LargePlot),
               show.legend = FALSE) +
  geom_point(aes(x = int_fin, y = AGB_fin.ha.rel2),
             color = "black",size = 0.2,
             show.legend = FALSE) +
  facet_wrap(~ Continent) +
  scale_color_manual(values =  c("red","Darkgreen")) +
  scale_x_continuous(breaks = seq(1960,2020,20),
                     limits = c(1960,2020)) +
  scale_y_continuous(limits = c(-50,50)) +
  scale_linetype_manual(values = c(2,1)) +
  labs(x = "", y = "AGB change (%)") +
  theme_minimal()


all.df <- data.frame()
for (isite in seq(1,nrow(networks2plot))){
  print(isite/nrow(networks2plot))

  cdata <- networks2plot[isite,]

  cini <- cdata[["int_ini"]]
  cfin <- cdata[["int_fin"]]

  cAGBinit <- cdata[["AGB_ini.ha"]]
  cAGBfin <- cdata[["AGB_fin.ha"]]

  cyears <- seq(floor(cini)-1,ceiling(cfin)+1,1/12)

  if (is.na(cAGBinit) | is.na(cAGBfin)) next()

  cdf <- data.frame(year = cyears,
             AGB = approx(c(cini,cfin),c(cAGBinit,cAGBfin), xout=cyears)[["y"]],
             ClusterCode = cdata[["ClusterCode"]],
             PlotCode = cdata[["PlotCode"]],
             PlotArea = cdata[["PlotArea"]],
             Continent = cdata[["Continent"]]) %>%
    mutate(weights = (cfin - cini)**(1/3) + (PlotArea)**(1/4) - 1)

  all.df <- bind_rows(list(all.df,
                           cdf))


}

all.df.sum <- all.df %>%
  group_by(Continent,year) %>%
  summarise(AGB = weighted.mean(AGB,
                                w = weights,
                                na.rm = TRUE),
            # AGB.m = mean(AGB,na.rm = TRUE),
            # AGB.sd = sd(AGB,na.rm = TRUE),
            .groups = "keep")



ggplot(data = all.df.sum %>%
         filter(year >= 1983,year <= 2010),
       aes(x = year, y = AGB)) +
  geom_line() +
  facet_wrap(~ Continent, nrow = 1) +
  scale_x_continuous(breaks = seq(1960,2020,20),
                     limits = c(1980,2020)) +
  # scale_y_continuous(limits = c(0,1250),
  #                    breaks = c(0,250,500,750,1000)) +
  scale_linetype_manual(values = c(2,1)) +
  labs(x = "", y = "AGB (Mg/ha)") +
  theme_bw() +
  theme(text = element_text(size = 20),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        strip.text = element_text(angle = 0, hjust = 0),
        panel.spacing = unit(2, "lines"))

saveRDS(all.df.sum,
        "observed.Biomass.RDS")
