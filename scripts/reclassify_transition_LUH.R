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

# Example Yoko
clat = 0.25 ; clon = 25.25
dist.df <- expand.grid(all.lats,all.lons) %>% rename(lat = Var1,
                                                     lon = Var2) %>%
  mutate(dist = sqrt((lat - clat)**2 + (lon - clon)**2)) %>% arrange(dist) %>% slice_head(n = 1)

lat.pos <- which(all.lats == as.numeric(dist.df$lat))
lon.pos <- which(all.lons == as.numeric(dist.df$lon))


names.var <- names(nc$var)
# names.var.selected <- names.var[grepl("primf_to*",names.var)]
names.var.selected <- names.var[grepl("*_to_*",names.var)]

all.data <- data.frame(time = all.times)

for (ivar in seq(1,length(names.var.selected))){

  cvar <- names.var.selected[ivar]
  cdata <- ncvar_get(nc,cvar)

  all.data[cvar] <- as.vector(cdata[lon.pos,lat.pos,])
}

nc_close(nc)

all.data.mut <- all.data %>%
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
         vs =  0)   # vs – Primary forest to secondary forest (assumed logging in ED-2) (yr-1).


all.data.mut.select <- all.data.mut %>%
  dplyr::select(cp,pc,pv,vp,vc,cv,sc,cs,sp,ps,vs)

all.data.mut.renamed <- all.data.mut %>%
  dplyr::select(time,ps,sp,cs,sc,vc,vp,pc,cp) %>%
  rename(pasture2secondary = ps,
         secondary2pasture = sp,
         crop2secondary = cs,
         secondary2crop = sc,
         primary2crop = vc,
         primary2pasture = vp,
         pasture2crop = pc,
         crop2pasture = cp)

all.data.mut.long <-
  all.data.mut.renamed %>%
  pivot_longer(cols = -time,
               values_to = "rate",
               names_to = "transition")

all.data.long.select <- all.data.mut.long %>%
  group_by(transition) %>%
  summarise(rate.max = mean(rate)) %>%
  arrange(desc(rate.max)) %>%
  filter(rate.max > 0)

ggplot(data = all.data.mut.long %>%
         filter(transition %in% (all.data.long.select %>% pull(transition)))) +
  geom_line(aes(x = time, y = rate, color = as.factor(transition)),
            show.legend = TRUE) +
  theme_bw() +
  theme(legend.position = c(0.2,0.7))


ggplot(data = all.data.mut.renamed %>%
         mutate(deforestation = primary2crop + primary2pasture + secondary2crop + secondary2pasture,
                regrowth = crop2secondary + pasture2secondary,
                internal = pasture2crop + crop2pasture) %>%
         dplyr::select(time,deforestation,
                       regrowth,internal) %>%
         pivot_longer(cols = -c(time),
                      values_to = "rate",
                      names_to = "transition")) +
  geom_line(aes(x = time, y = rate, color = as.factor(transition)),
            show.legend = TRUE) +
  theme_bw() +
  theme(legend.position = c(0.2,0.7))


ggplot(data = all.data.mut.long) +
  geom_boxplot(aes(x = transition,
                y = rate, fill = as.factor(transition)),
            show.legend = TRUE) +
  theme_bw() +
  scale_y_log10()


# all.data.long.select <- all.data.long %>%
#   group_by(transition) %>%
#   summarise(rate.max = mean(rate)) %>%
#   arrange(desc(rate.max)) %>%
#   filter(rate.max > 0.0001)
#
# ggplot(data = all.data.long %>% filter(transition %in% (all.data.long.select %>% pull(transition)))) +
#   geom_line(aes(x = time, y = rate, color = as.factor(transition)),
#             show.legend = TRUE) +
#   theme_bw() +
#   theme(legend.position = c(0.2,0.7))
# ggplot(data = df) +
#   geom_line(aes(x = time, y = cumsum(primf_to_secdn)),
#             show.legend = FALSE) +
#   theme_bw()

###########################################################################

delta_lon = 0.5 ; delta_lat = 0.5 ; erad <- 6370997.

yeara       = 850
yearz       = 850 + nrow(all.data.mut.select) - 1

sbh <-1e8*0       # Timber demand on mature secondary forest (kgC).
f_sbh <- 1*0      # Timber demand on mature secondary forest (grid fraction). This is currently ignored in ED-2.
vbh <- 1e8*0      # Timber demand on mature primary forest (kgC). This is combined with vbh2 (see below).
f_vbh <- 1*0      # Timber demand on mature primary forest (grid fraction). This is currently ignored in ED-2.
sbh2 <- 1e8*0     # Timber demand on young secondary forest (kgC).
f_sbh2 <- 1*0     # Timber demand on young secondary forest (grid fraction). This is currently ignored in ED-2.
vbh2 <- 1e8*0     # Timber demand on primary vegetation other than forest (kgC). This is combined with vbh (see above).
f_vbh2 <- 1 *0    # Timber demand on primary vegetation other than forest (grid fraction). This is currently ignored in ED-2.

npft.harvest  = 3
harvest.pft   = c(2,3,4)
tropical      = harvest.pft %in% c(2,3,4,12,13,14,15)
mindbh.slog   = ifelse(tropical,50.0,0.0)
harvprob.slog = ifelse(tropical,0.25,0.25)
mindbh.fplt   = ifelse(tropical,50.0,0.0)
harvprob.fplt = ifelse(tropical,0.25,0.25)

source("/home/femeunier/Documents/projects/Congo.ED2/scripts/solid.angle.r")

charwlon    = paste("WEST.LONGITUDE =",sprintf("%.3f",clon - delta_lon/2))
charelon    = paste("EAST.LONGITUDE =",sprintf("%.3f",clon + delta_lon/2))
charslat    = paste("SOUTH.LATITUDE =",sprintf("%.3f",clat - delta_lat/2))
charnlat    = paste("NORTH.LATITUDE =",sprintf("%.3f",clat + delta_lat/2))
chararea    = paste("BLOCK.AREA     =", solid.angle(sw = cbind(clon - delta_lon/2,clat - delta_lat/2),
                                                      ne = cbind(clon + delta_lon/2,clat + delta_lat/2),
                                                      degrees = TRUE,
                                                      radius  = erad))

charyeara   = paste("FIRST.LUYEAR   =",yeara)
charyearz   = paste("LAST.LUYEAR    =",yearz)
charnpft    = paste("N.PFT.HARVEST  =",npft.harvest)
charpfth    = paste(c("HARVEST.PFT    =",harvest.pft  ),collapse=" ")
chardbhslog = paste(c("MINDBH.SLOG    =",mindbh.slog  ),collapse=" ")
charprbslog = paste(c("HARVPROB.SLOG  =",harvprob.slog),collapse=" ")
chardbhfplt = paste(c("MINDBH.FPLT    =",mindbh.fplt  ),collapse=" ")
charprbfplt = paste(c("HARVPROB.FPLT  =",harvprob.fplt),collapse=" ")
charlabel   = paste(c("year",dimnames(all.data.mut.select)[[2]],c("f_sbh","vbh","f_vbh","sbh2","f_sbh2","vbh2","f_vbh2")),collapse=" ")
disturb     = format(((all.data.mut.select)) %>%
                       mutate(year = yeara:yearz) %>%
                       relocate(year) %>%
                       mutate(sbh,f_sbh,
                              vbh,f_vbh,
                              sbh2,f_sbh2,
                              vbh2,f_vbh2))

outfile <- file.path("/home/femeunier/Documents/projects/Congo.ED2/outputs",
                     paste0("Yoko.test-lat",clat,"lon",clon,".lu"))

dum     = write(x=charwlon   , file=outfile, append = FALSE)
dum     = write(x=charelon   , file=outfile, append = TRUE )
dum     = write(x=charslat   , file=outfile, append = TRUE )
dum     = write(x=charnlat   , file=outfile, append = TRUE )
dum     = write(x=chararea   , file=outfile, append = TRUE )
dum     = write(x=charyeara  , file=outfile, append = TRUE )
dum     = write(x=charyearz  , file=outfile, append = TRUE )
dum     = write(x=charnpft   , file=outfile, append = TRUE )
dum     = write(x=charpfth   , file=outfile, append = TRUE )
dum     = write(x=chardbhslog, file=outfile, append = TRUE )
dum     = write(x=charprbslog, file=outfile, append = TRUE )
dum     = write(x=chardbhfplt, file=outfile, append = TRUE )
dum     = write(x=charprbfplt, file=outfile, append = TRUE )
dum     = write(x=charlabel,file=outfile, append = TRUE )
dum     = write.table( x         = disturb,
                       file      = outfile,
                       append    = TRUE,
                       quote     = FALSE,
                       sep       = " ",
                       row.names = FALSE,
                       col.names = FALSE) # end write.table

matplot(disturb,type = "l")


system2("scp",paste(outfile,
                    "hpc:/data/gent/vo/000/gvo00074/ED_common_data/land_use/glu+sa2/"))


# # Example regional
#
# primf_to_secdn.m <- melt(apply(primf_to_secdn[,,],c(1,2),sum,na.rm = TRUE)) %>%
#   rename(lon = Var1,
#          lat = Var2,
#          transition = value) %>%
#   mutate(lat = all.lats[lat],
#          lon = all.lons[lon])
#
# world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#
# ggplot(data = world) +
#   geom_tile(data = primf_to_secdn.m,
#             aes(x = lon, y = lat,fill = transition),na.rm = TRUE, alpha = 1) +
#   geom_sf(fill = NA) +
#   coord_sf(xlim = c(-10, 50),
#            ylim = c(-20, 15),
#            expand = FALSE) +
#   scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
#   labs(x = "",y = "") +
#   theme_bw()
#
#
# hist(primf_to_secdn.m %>% filter(transition > 0) %>% pull(transition))
#
#
# primf_to_secdn.m %>% arrange(desc(transition))
