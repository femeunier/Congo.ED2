rm(list = ls())

library(ncdf4)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)

ncfile <- "/home/femeunier/Documents/projects/Congo.ED2/data/transitions_tropics_20.nc"

nc <- nc_open(ncfile)

all.lats <- ncvar_get(nc,"lat")
all.lons <- ncvar_get(nc,"lon")
all.times <- ncvar_get(nc,"time")

select.lats <- 1:length(all.lats)
select.lons <- 1:length(all.lons)
select.times <- 1:length(all.times)

all.data <- data.frame(time = all.times)

names.var <- names(nc$var)
names.var.selected <- names.var

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
           time = 850 + all.times[select.times][time]) %>%
    filter(!is.na(get(cvar)))

  if (ivar == 1){
    all.df <- cdf
  } else {
    all.df[cvar] <- cdf[cvar]
  }
}


all.data.mut <- all.df %>%
  ungroup() %>%
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
         vp = primf_to_pastr + primf_to_range,   # vp – Primary forest to pasture (yr-1).
         vc = primf_to_c3ann + primf_to_c3nfx + primf_to_c3per + primf_to_c4ann +
           primf_to_c4per,   # vc – Primary forest to cropland (yr-1).
         sc = secdf_to_c3ann + secdf_to_c3nfx + secdf_to_c3per + secdf_to_c4ann + secdf_to_c4per,   # sc – Secondary forest to cropland (yr-1).
         cs = c3ann_to_secdf +
           c3nfx_to_secdf +
           c3per_to_secdf +
           c4ann_to_secdf +
           c4per_to_secdf,   # cs – Cropland to secondary forest (in ED-2 this goes to abandonment) (yr-1).
         sp = secdf_to_pastr + secdf_to_range,   # sp – Secondary forest to pasture (yr-1).
         ps = pastr_to_secdf + range_to_secdf) %>% # ps – Pasture to secondary forest (in ED-2 this goes to abandonment) (yr-1).)
  dplyr::select(time,lat,lon,
                others,internal,
                cp,pc,vp,vc,sc,cs,sp,ps) %>%
  dplyr::select(
    where(
      ~!all((.x == mean(.x,na.rm = TRUE)))
    ))

saveRDS(all.data.mut,
        "./data/transitions_tropics_20_reclass.RDS")

system2("rsync",
        paste("-avz",
              "./data/transitions_tropics_20_reclass.RDS",
              "hpc:/data/gent/vo/000/gvo00074/ED_common_data/land_use/LUH/transitions_tropics_20_reclass.RDS"))
