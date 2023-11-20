rm(list = ls())

library(dplyr)
library(ggplot2)
library(caret)
library(xgboost)
library(randomForest)
library(tidyr)
library(lubridate)
library(YGB)
library(SHAPforxgboost)

################################################################################
#
# cmodel = "CABLE-POP"
# cdf <- readRDS(paste0("./outputs/Trendy.",cmodel,".S2.CC.pantropical.v11.RDS")) %>%
#   dplyr::select(-c(time,time.unit)) %>%
#   mutate(gpp = gpp*86400*365) %>%
#   mutate(lat = round(lat,digits = 3),
#          lon = round(lon,digits = 3))
#
# cgrid <- readRDS(paste0("./outputs/grid.",cmodel,".RDS")) %>%
#   mutate(lat = round(lat,digits = 3),
#          lon = round(lon,digits = 3))
#
# sink.vs.climate <- cdf %>%
#   left_join(cgrid,
#             by = c("lon","lat","year","month")) %>%
#   na.omit()
#
# TF <- sink.vs.climate %>%
#
#   mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
#   mutate(pre = pre*4*N) %>%
#   group_by(model,lon,lat,month) %>%
#   summarise(Pmm = mean(pre),
#             N = mean(N),
#             .groups = "keep") %>%
#   ungroup() %>%
#   mutate(diff = Pmm - 3.33*N) %>%
#   group_by(model,lon,lat) %>%
#   mutate(wettest.month = which.max(diff)) %>%
#   mutate(month.ord = 1 + (month - wettest.month)) %>%
#   mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
#                                TRUE ~ month.ord)) %>%
#   arrange(model,lat,lon,month.ord) %>%
#   mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
#                          TRUE ~ NA_real_)) %>%
#   mutate(CWD = calc.CWD(diff,CWD[1])) %>%
#   arrange(lat,lon,month) %>%
#   mutate(MCWD = min(CWD),
#          MAP = sum(Pmm)) %>%
#   filter(MCWD >= -200) %>%
#   mutate(model.lat.lon = paste0(model,".",lat,".",lon))
#
#
# sink.vs.climate.selected <- sink.vs.climate %>%
#   mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
#   filter(model.lat.lon %in% TF[["model.lat.lon"]])
#
# hist(sink.vs.climate.selected$lat)
# hist(sink.vs.climate$lat)
#
################################################################################

# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/xgb_model.*",
#               "./outputs/"))

# xgb_model.prefix <- "xgb_model.time.latlon.Africa."
xgb_model.prefix <- "XGB.fit."

models <- TrENDY.analyses::get.model.names.TRENDY()
models <- c("CLASSIC","CLM5.0","JULES","ORCHIDEE","LPJ")
vars <- c("gpp","npp","nep")

df.r2 <- df.importance <- df.predict <- df.coord <-
  full.dataset <- data.frame()
all.xgb.models <- list()
for (cmodel in models){

  print(cmodel)
  all.xgb.models[[cmodel]] <- list()

  for (cvar in vars){

    xgb_model.file <- paste0("./outputs/",xgb_model.prefix,cmodel,".S2.Africa.Humid_large.",cvar,".RDS")

    if (any(!file.exists(c(xgb_model.file)))){
      next()
    }

    xgb_model <- readRDS(xgb_model.file)
    all.xgb.models[[cmodel]][[cvar]] <- xgb_model

    test.data <- xgb_model$test.data
    test.label <- xgb_model$test.labels

    predicted <- predict(xgb_model,
                         test.data)

    full.dataset <- bind_rows(full.dataset,
                              bind_rows(
      bind_cols(xgb_model$trainingData,
                data.frame(value = xgb_model$labels)) %>%
        mutate(type = "training",
               origin = "data"),
      bind_cols(test.data,
                data.frame(value = test.label)) %>%
        mutate(type = "test",
               origin = "data"),
      bind_cols(test.data,
                data.frame(value = predicted)) %>%
        mutate(type = "test",
               origin = "model")) %>%
      arrange(lon,lat,year,month) %>%
      mutate(model = cmodel,
             var = cvar) %>%
      mutate(model.lat.lon = paste0(model,".",lat,".",lon)))

    # selected <- full.dataset %>%
    #   ungroup() %>%
    #   filter(model.lat.lon %in%
    #            sample(unique(model.lat.lon),1,replace = FALSE))
#     ggplot(mapping =
#              aes(x = year + (month -1/2)/12,y = gpp,color = origin)) +
#       geom_line(data = selected %>%
#                   filter(origin == "data"),color = "black") +
#       geom_point(data = selected %>%
#                   filter(origin == "model"), color = "darkblue") +
#       scale_x_continuous(limits = c(2000,2020)) +
#       theme_bw()

    df.predict <- bind_rows(df.predict,
                            data.frame(pred = predicted,
                                       obs = test.label) %>%
                              mutate(model = cmodel,
                                     var = cvar))

    df.r2 <- bind_rows(df.r2,
                       data.frame(r2 = 1 - sum((predicted- test.label)**2)/sum(((test.label - mean(test.label))**2)),
                                  model = cmodel,
                                  var = cvar))

    importance_matrix = xgb.importance(colnames(test.data),
                                       model = xgb_model$finalModel)

    df.importance <- bind_rows(df.importance,
                               as.data.frame(importance_matrix) %>%
                                 mutate(model = cmodel,
                                        var = cvar))

    if (cvar == vars[[1]]){
      df.coord <- bind_rows(df.coord,
                            bind_rows(as.data.frame(test.data),
                                      as.data.frame(xgb_model$trainingData)) %>%
                              ungroup() %>%
                              filter(year == year[1]) %>%
                              dplyr::select(lat,lon) %>%
                              distinct() %>%
                              mutate(model = cmodel))
    }

  }
}

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
df.coord <- df.coord %>%
  group_by(model) %>%
  mutate(A = min(diff(sort(unique(lon)))),
         B = min(diff(sort(unique(lat))))) %>%
  mutate(lon.min = lon - A/2,
         lon.max = lon + A/2,
         lat.min = lat - B/2,
         lat.max = lat + B/2)


ggplot() +

  geom_rect(data = df.coord %>%
              filter(model == "CLASSIC"),
            aes(xmin = lon.min, xmax = lon.max,
                ymin = lat.min, ymax = lat.max),
            fill = "darkgreen", alpha = 0.5,
            color = "black",linewidth = 0.5) +

  geom_sf(data = world,
          fill = NA) +
  # coord_sf(xlim = c(-15, 50),
  #          ylim = c(-20, 15)) +
  coord_sf(xlim = c(-100, 50),
           ylim = c(-20, 15)) +
  labs(x = "",y = "") +
  facet_wrap(~ model) +
  theme_bw()

ggplot(df.predict,
       aes(x = pred,
           y = obs)) +
  geom_hex() +
  # geom_density_2d_filled(aes(fill = log10(..density..)),
  #                        bins = 1000,
  #                        alpha = 0.5) +

  geom_abline(slope = 1, intercept = 0, linetype = 1) +
  scale_fill_viridis_c(trans = "log10") +
  # scale_fill_brewer() +
  facet_wrap(~ var,
             nrow = 1) +
  stat_smooth(method = "lm", se = FALSE,
              color = "red", linetype = 2) +
  coord_equal() +
  theme_bw() +
  guides(fill = "none")

ggscater <- ggplot(df.predict %>%
                     filter(var == "gpp"),
       aes(x = pred,
           y = obs,
           color = model)) +
  geom_point(shape = NA) +
  geom_hex(color = NA) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = 1, color = "black") +
  scale_fill_viridis_c(trans = "log10") +
  stat_smooth(method = "lm", se = FALSE, linetype = 1) +
  # coord_equal() +
  # facet_wrap(~)
  theme_bw() +
  theme(legend.position = c(0.1,0.8)) +
  guides(color = "none")
ggExtra::ggMarginal(ggscater, type = "density",
                    groupColour = TRUE)

ggplot(df.predict,
       aes(x = pred - obs)) +
  geom_density(aes(fill = var), alpha = 0.5) +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = df.r2) +
  geom_density(aes(x = r2,
               fill = var),alpha = 0.5) +
  # facet_wrap(~ var) +
  theme_bw()

level.ord <- df.importance %>%
  group_by(Feature) %>%
  summarise(Gain.m = median(Gain)) %>%
  arrange((Gain.m)) %>% pull(Feature)

df.importance.sum <- df.importance %>%
  group_by(var,Feature) %>%
  summarise(Gain.m = mean(Gain),
            Gain.sd = sd(Gain),
            .groups = "keep") %>%
  mutate(Feature = factor(Feature,
                          levels = level.ord))

ggplot(data = df.importance.sum,
       aes(x = Feature, y = Gain.m,
           fill = var)) +
  geom_errorbar(aes(ymin = Gain.m*0.9,
                    ymax = Gain.m + Gain.sd),
                position = position_dodge(0.9),
                width = 0.5) +
  geom_bar(alpha = 1,
           stat = "identity",
           position = position_dodge(0.9)) +
  # facet_wrap(~ var) +
  coord_flip() +
  theme_bw()


ggplot(data = df.importance,
       aes(x = Feature, y = Gain,
           fill = var)) +
  geom_bar(alpha = 1,
           stat = "identity",
           position = position_dodge(0.9)) +
  facet_wrap(~ model) +
  coord_flip() +
  theme_bw()

selected.dataset <- full.dataset %>%
  group_by(model) %>%
  filter(model.lat.lon %in%
           sample(unique(model.lat.lon),1,replace = FALSE)) %>%
  ungroup()

selected.dataset.comp <- selected.dataset %>%
  complete(model.lat.lon = unique(selected.dataset$model.lat.lon),
           origin = "model",
           model = unique(selected.dataset$model),
           type = "test",
           year = unique(selected.dataset$year),
           var = unique(selected.dataset$var),
           month = 1:12) %>%
  rowwise() %>%
  filter(grepl(model,model.lat.lon))


vars2plot <- c("gpp","npp","nep")

ggplot(mapping =
         aes(x = year + (month -1/2)/12,y = value,
             group = var)) +
  geom_line(data = selected.dataset %>%
              filter(origin == "data",
                     var %in% vars2plot) ,
            color = "black") +
  geom_line(data = selected.dataset.comp %>%
              filter(origin == "model",
                     var %in% vars2plot),
             color = "darkblue",
            linewidth = 1) +
  scale_x_continuous(limits = c(2000,2020)) +
  facet_wrap(~ model,nrow = 1) +
  labs(x = "") +
  theme_bw()


selected.dataset.seasonal <- full.dataset %>%
  filter(type == "test") %>%
  group_by(var,model,origin,month) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            .groups = "keep")

ggplot(data = selected.dataset.seasonal %>%
         filter(var %in% vars2plot),
       mapping =
         aes(x = month,y = value.m, color = origin, fill = origin,
             group = interaction(var,origin))) +
  geom_ribbon(aes(ymin = value.m - value.sd,
                  ymax = value.m + value.sd), alpha = 0.5, color = NA) +
  geom_line() +
  facet_wrap(~ model,
             nrow = 1) +
  scale_x_continuous(breaks = 1:12) +
  theme_bw()


selected.dataset.year <- full.dataset %>%
  group_by(var,model,type,origin,year) %>%
  summarise(value.m = mean(value,na.rm = TRUE),
            value.sd = sd(value,na.rm = TRUE),
            .groups = "keep")

selected.dataset.year.comp <- selected.dataset.year %>%
  ungroup() %>%
  complete(origin = c("model","data"),
           type = c("test","train"),
           model = unique(selected.dataset.year$model),
           year = unique(full.dataset$year),
           var = unique(selected.dataset.year$var))

ggplot(mapping =
         aes(x = year,y = value.m,
             ymin = value.m - value.sd,
             ymax = value.m + value.sd,
             group = var)) +
  geom_line(data = selected.dataset.year.comp %>%
              filter(origin == "data",
                     var %in% vars2plot) %>%
              na.omit(), color = "black") +
  geom_ribbon(data = selected.dataset.year.comp %>%
              filter(origin == "data",
                     var %in% vars2plot) %>%
              na.omit(),
              color = "darkgrey", alpha = 0.5,
              color = NA) +
  geom_point(data = selected.dataset.year.comp %>%
               filter(origin == "model",
                      type == "test",
                      var %in% vars2plot)) +
  geom_errorbar(data =selected.dataset.year.comp %>%
                  filter(origin == "model",
                         var %in% vars2plot,
                         type == "test")) +
  facet_wrap(~ model,nrow = 1) +
  theme_bw()

stop()
################################################################################

all.shap_long <- df.shape.scores <-
  data.frame()

for (cmodel in models){

  for (cvar in vars){
    selected.model <- all.xgb.models[[cmodel]][[cvar]]

    if (is.null(selected.model)) next()

    print(paste0(cmodel," - ",cvar))

    ids <- sort(sample(1:nrow(selected.model$trainingData),
                       size = 1000, replace = FALSE))
    shap_values <- shap.values(xgb_model = selected.model$finalModel,
                               X_train = selected.model$trainingData[ids,])
    shape.score <- shap_values$mean_shap_score

    df.shape.scores <- bind_rows(df.shape.scores,
                                 data.frame(feature = names(shape.score),
                                            shap = as.vector(shape.score)) %>%
                                   mutate(model = cmodel,
                                          var = cvar))

    shap_long <- shap.prep(shap_contrib = shap_values$shap_score,
                           X_train = selected.model$trainingData[ids,])
    # shap.plot.summary(shap_long)

    all.shap_long <- bind_rows(all.shap_long,
                               shap_long %>%
                                 mutate(model = cmodel,
                                        var = cvar))

  }
}

ggplot(data = all.shap_long ,
       aes(x = rfvalue,y = value,
           color = model)) +
  # geom_point() +
  geom_smooth(method = "loess",
              size = 0.4, se = FALSE) +
  facet_grid(var ~ variable, scales = "free_x") +
  theme_bw()



shap.plot.dependence(data_long = shap_long,x = names(shape.score)[1],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[2],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[3],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[4],
                     add_hist = TRUE,add_stat_cor = TRUE)
shap.plot.dependence(data_long = shap_long, x = names(shape.score)[5],
                     add_hist = TRUE,add_stat_cor = TRUE)

