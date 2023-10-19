rm(list = ls())

library(ncdf4)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

ncfile <- "/home/femeunier/Documents/projects/Congo.ED2/data/transitions_crop.nc"

nc <- nc_open(ncfile)

all.lats <- ncvar_get(nc,"lat")
all.lons <- ncvar_get(nc,"lon")
all.times <- ncvar_get(nc,"time")

select.lats <- 1:length(all.lats)
select.lons <- 1:length(all.lons)
select.times <- 1115:1165

all.data <- data.frame(time = all.times)

names.var <- names(nc$var)
names.var.selected <- names.var[grepl("*_to_*",names.var)]

for (ivar in seq(1,length(names.var.selected))){

  print(ivar/length(names.var.selected))
  cvar <- names.var.selected[ivar]
  cdata <- ncvar_get(nc,cvar)

  cdf <- melt(cdata[select.lons,select.lats,select.times]) %>%
    rename(lon = Var1,
           lat = Var2,
           time = Var3,
           !!cvar := value) %>%
    mutate(lon = all.lons[select.lons][lon],
           lat = all.lats[select.lats][lat],
           time = 850 + all.times[select.times][time])

  if (ivar == 1){
    all.df <- cdf
  } else {
    all.df[cvar] <- cdf[cvar]
  }
}


names.var.biomass <- names.var[grepl("*bioh",names.var)]
names.var.area <- names.var[grepl("*harv",names.var)]

for (ivar in seq(1,length(names.var.biomass))){

  print(ivar/length(names.var.biomass))
  cvar <- names.var.biomass[ivar]
  cvar2 <- names.var.area[ivar]

  cdata <- ncvar_get(nc,cvar)
  cdata2 <- ncvar_get(nc,cvar2)

  cdf <- melt(cdata[select.lons,select.lats,select.times]) %>%
    rename(lon = Var1,
           lat = Var2,
           time = Var3,
           !!cvar := value) %>%
    mutate(lon = all.lons[select.lons][lon],
           lat = all.lats[select.lats][lat],
           time = 850 + all.times[select.times][time])

  cdf2 <- melt(cdata2[select.lons,select.lats,select.times]) %>%
    rename(lon = Var1,
           lat = Var2,
           time = Var3,
           !!cvar := value) %>%
    mutate(lon = all.lons[select.lons][lon],
           lat = all.lats[select.lats][lat],
           time = 850 + all.times[select.times][time])

  if (ivar == 1){
    all.df.biomass <- cdf
    all.df.area <- cdf2
  } else {
    all.df.biomass[cvar] <- cdf[cvar]
    all.df.area[cvar] <- cdf2[cvar]
  }
}

nc_close(nc)


all.data.mut <- all.df %>%
  mutate(others = c3ann_to_urban + c3ann_to_secdn +
           c3nfx_to_urban + c3nfx_to_secdn +
           c3per_to_secdn + c3per_to_urban +
           c4ann_to_secdn + c4ann_to_urban +
           c4per_to_secdn + c4per_to_urban +
           pastr_to_secdn + pastr_to_urban +
           primf_to_secdn + primf_to_urban +
           primn_to_c3ann + primn_to_c3nfx + primn_to_c3per + primn_to_c4ann + primn_to_c4per + primn_to_pastr + primn_to_range +
           primn_to_secdf + primn_to_urban +
           range_to_secdn + range_to_urban +
           secdf_to_secdn + secdn_to_range +
           secdn_to_secdf + secdn_to_urban +
           urban_to_c3ann + urban_to_c3nfx +  urban_to_c3per +  urban_to_c4ann +  urban_to_c4per + urban_to_pastr + urban_to_range + urban_to_secdf + urban_to_secdn +
           secdf_to_urban +
           secdn_to_c3ann + secdn_to_c3nfx + secdn_to_c3per + secdn_to_c4ann + secdn_to_c4per + secdn_to_pastr,
         internal = c3ann_to_c3nfx + c3ann_to_c3per + c3ann_to_c4ann + c3ann_to_c4per +
           c3nfx_to_c3ann + c3nfx_to_c3per + c3nfx_to_c4ann + c3nfx_to_c4per +
           c3per_to_c3ann + c3per_to_c3nfx + c3per_to_c4ann + c3per_to_c4per +
           c4ann_to_c3ann + c4ann_to_c3nfx + c4ann_to_c3per + c4ann_to_c4per +
           c4per_to_c3ann + c4per_to_c3nfx + c4per_to_c3per + c4per_to_c4ann +
           pastr_to_range +
           range_to_pastr,
         cp = c3ann_to_pastr + c3ann_to_range +
           c3nfx_to_pastr + c3nfx_to_range +
           c3per_to_pastr + c3per_to_range +
           c4ann_to_pastr + c4ann_to_range +
           c4per_to_pastr + c4per_to_range ,   # cp – Cropland to pasture (yr-1).
         pc = pastr_to_c3ann + pastr_to_c3nfx + pastr_to_c3per + pastr_to_c4ann +
           pastr_to_c4per +
           range_to_c3ann + range_to_c3nfx + range_to_c3per + range_to_c4ann + range_to_c4per,   # pc – Pasture to cropland (yr-1).
         pv = 0,   # pv – Pasture to primary forest (in ED-2 this goes to abandonment) (yr-1).
         vp = primf_to_pastr + primf_to_range,   # vp – Primary forest to pasture (yr-1).
         vc = primf_to_c3ann + primf_to_c3nfx + primf_to_c3per + primf_to_c4ann +
           primf_to_c4per,   # vc – Primary forest to cropland (yr-1).
         cv = 0,   # cv – Cropland to primary forest (in ED-2 this goes to abandonment) (yr-1).
         sc = secdf_to_c3ann + secdf_to_c3nfx + secdf_to_c3per + secdf_to_c4ann + secdf_to_c4per,   # sc – Secondary forest to cropland (yr-1).
         cs = c3ann_to_secdf +
           c3nfx_to_secdf +
           c3per_to_secdf +
           c4ann_to_secdf +
           c4per_to_secdf,   # cs – Cropland to secondary forest (in ED-2 this goes to abandonment) (yr-1).
         sp = secdf_to_pastr + secdf_to_range,   # sp – Secondary forest to pasture (yr-1).
         ps = pastr_to_secdf + range_to_secdf,   # ps – Pasture to secondary forest (in ED-2 this goes to abandonment) (yr-1).
         vs =  0) %>%  # vs – Primary forest to secondary forest (assumed logging in ED-2) (yr-1).
  dplyr::select(time,lat,lon,cp,pc,pv,vp,vc,cv,sc,cs,sp,ps,vs) %>%
  rename(pasture2seconday = ps,
         secondary2pasture = sp,
         crop2secondary = cs,
         secondary2crop = sc,
         primary2crop = vc,
         primary2pasture = vp,
         pasture2crop = pc,
         crop2pasture = cp,
         pasture2primary = pv,
         cropland2primary = cv,
         primary2secondary = vs)


all.data.mut.long <- all.data.mut %>%
  pivot_longer(cols = -c(time,lon,lat),
               values_to = "rate",
               names_to = "transition")

all.data.mut.long.type <- all.data.mut.long %>%
  mutate(direction = case_when(transition %in% c("primary2pasture","primary2crop","secondary2crop","secondary2pasture","primary2secondary") ~ "deforestation",
                               transition %in% c("pasture2primary","cropland2primary","crop2secondary","pasture2seconday") ~ "regrowth",
                               TRUE ~ "weird"))

all.data.mut.dominant <- all.data.mut.long.type %>%
  group_by(lat,lon,direction,transition) %>%
  summarise(rate.m = mean(rate),
            .groups = "keep") %>%
  group_by(lat,lon,direction) %>%
  summarise(rate.max = max(rate.m),
            dom.transition = transition[rate.m == rate.max],
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world ) +
  geom_tile(data = all.data.mut.dominant %>% filter(!is.na(dom.transition)),
            aes(x = lon, y = lat,fill = as.factor(dom.transition)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ direction) +
  theme_bw()


ggplot(data = world) +
  geom_tile(data = all.data.mut.dominant %>% filter(!is.na(dom.transition)) ,
            aes(x = lon, y = lat,fill = rate.max),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~direction) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  theme_bw()


ggplot() +
  geom_density(data = all.data.mut.dominant %>%
                 filter(!is.na(dom.transition),
                        direction != "weird",
                        rate.max > 0) ,
            aes(x = rate.max)) +
  labs(x = "",y = "") +
  facet_wrap(~direction,
             scales = "free") +
  theme_bw()



simplest <- all.data.mut %>% mutate(deforestation = primary2pasture + primary2crop + secondary2crop + secondary2pasture + primary2secondary,
                                    regrowth = pasture2primary + cropland2primary + crop2secondary + pasture2seconday) %>%
  dplyr::select(time,lat,lon,regrowth,deforestation)

rates.m.df <- simplest %>%
  group_by(lat,lon) %>%
  summarise(regrowth = mean(regrowth),
            deforestation = mean(deforestation),
            .groups = "keep") %>%
  pivot_longer(cols = c(regrowth,deforestation),
               names_to = "variable",
               values_to = "rate")

ggplot(data = world) +
  geom_tile(data = rates.m.df,
            aes(x = lon, y = lat,fill = rate),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~variable) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent",
                      limits = c(0,0.05),oob = scales::squish) +
  theme_bw()


rates.time.m.df <- simplest %>%
  group_by(time,lat,lon) %>%
  summarise(regrowth = mean(regrowth),
            deforestation = mean(deforestation),
            .groups = "keep") %>%
  pivot_longer(cols = c(regrowth,deforestation),
               names_to = "variable",
               values_to = "rate")


select.site <- rates.m.df %>% arrange(desc(rate)) %>%
  ungroup() %>%
  slice_head(n = 1)

clon <- 25.25 ; clat = 0.25
select.site <- rates.m.df %>%
  ungroup() %>%
  mutate(dist = sqrt((clat -lat)**2 + (clon-lon)**2)) %>%
  arrange((dist)) %>%
  slice_head(n = 1)


ggplot() +
  geom_line(data = rates.time.m.df %>%
              ungroup() %>%
              dplyr::filter(lat == as.numeric(select.site$lat),
                            lon == as.numeric(select.site$lon)),
            aes(x = time, y = rate, group = interaction(lat,lon))) +

  geom_line(data = all.data.mut.long.type %>%
              filter(direction != "weird") %>%
              rename(variable = direction) %>%
              dplyr::filter(lat == as.numeric(select.site$lat),
                            lon == as.numeric(select.site$lon)),
            aes(x = time, y = rate,
                color = transition,
                group = interaction(lat,lon,transition)),
            linetype = 2) +

  facet_wrap(~variable) +
  theme_bw()


##############################################################################

all.df.biomass.long <- all.df.biomass %>% pivot_longer(cols = -c(lon,lat,time),
                                                       names_to = "ecosystem",
                                                       values_to = "biomass") %>%
  mutate(ecosystem = sub("\\_.*", "", ecosystem))

all.df.biomass.long.m <- all.df.biomass.long %>%
  group_by(lat,lon,ecosystem) %>%
  summarise(biomass = mean(biomass,na.rm = TRUE),
            .groups = "keep")

ggplot(data = world ) +
  geom_tile(data = all.df.biomass.long.m,
            aes(x = lon, y = lat,fill = biomass/1000),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ ecosystem) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  theme_bw()

ggplot(data = world ) +
  geom_line(data = all.df.biomass.long %>% dplyr::filter(lat == as.numeric(select.site$lat),
                                                         lon == as.numeric(select.site$lon)),
            aes(x = time, y = biomass/1000, color = ecosystem),na.rm = TRUE, alpha = 1) +
  theme_bw()


all.df.area.long <- all.df.area %>% pivot_longer(cols = -c(lon,lat,time),
                                                       names_to = "ecosystem",
                                                       values_to = "area") %>%
  mutate(ecosystem = sub("\\_.*", "", ecosystem))

all.df.area.long.m <- all.df.area.long %>%
  group_by(lat,lon,ecosystem) %>%
  summarise(area = mean(area,na.rm = TRUE),
            .groups = "keep")

ggplot(data = world ) +
  geom_tile(data = all.df.area.long.m,
            aes(x = lon, y = lat,fill = area),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  labs(x = "",y = "") +
  facet_wrap(~ ecosystem) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent",
                      limits = c(0,0.01),
                      oob = scales::squish) +
  theme_bw()

ggplot(data = world ) +
  geom_line(data = all.df.area.long %>% dplyr::filter(lat == as.numeric(select.site$lat),
                                                         lon == as.numeric(select.site$lon)),
            aes(x = time, y = area, color = ecosystem),na.rm = TRUE, alpha = 1) +
  theme_bw()

