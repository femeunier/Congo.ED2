rm(list = ls())

library(dplyr)
library(caret)
library(xgboost)
library(ggthemes)
library(sf)
library(zoo)

products <- c("SIF2","VOD","NIR","SIF")

df.all <- bind_rows(list(

  # readRDS("./data/GPP/monthly/all.df.GPP2.RDS") %>%
  #                          mutate(product = "Madani") %>%
  #                          rename(value = daily.GPP) %>%
  #                          ungroup(),

                         # readRDS("./data/GPP/monthly/all.df.GPP.RDS") %>%
                         #   mutate(product = "Zhang") %>%
                         #   rename(value = daily.GPP) %>%
                         #   ungroup(),

                         readRDS("./data/GPP/monthly/NIR.GPP.RDS") %>%
                           mutate(product = "NIR") %>%
                           ungroup(),

                         # readRDS("./data/GPP/monthly/df.all.GPP3.RDS") %>%
                         #   mutate(product = "Fluxcom") %>%
                         #   dplyr::select(-value) %>%
                         #   rename(value = daily.GPP) %>%
                         #   ungroup(),
                         #
                         # readRDS("./data/GPP/monthly/all.df.GPP.MODIS.RDS") %>%
                         #   mutate(product = "MODIS") %>%
                         #   rename(value = daily.GPP) %>%
                         #   ungroup(),

                         readRDS("./data/GPP/monthly/SIF.GPP2.RDS") %>%
                           mutate(product = "SIF2") %>%
                           ungroup(),

                         readRDS("./data/GPP/monthly/SIF.GPP.RDS") %>%
                           mutate(product = "SIF") %>%
                           ungroup(),

                         readRDS("./data/GPP/monthly/VOD.GPP.RDS") %>%
                           mutate(product = "VOD") %>%
                           ungroup())) %>%
  dplyr::select(product,lat,lon,year,month,value)


dataC02 <- read.table("./data/global_co2_ann_1700_2022.txt",
                      stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)

dataC02.all <- data.frame(year = sort(unique(c(dataC02$year,2020:2024)))) %>%
  arrange(year) %>%
  left_join(dataC02,
            by = c("year")) %>%
  mutate(CO2 = na.spline(CO2,method = "natural"))

xgb_trcontrol <- caret::trainControl(
  method = "cv",
  number = 8,
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE
)

xgb_grid <- base::expand.grid(
  list(
    nrounds = c(100, 200),
    max_depth = c(10, 15, 20), # maximum depth of a tree
    colsample_bytree = seq(0.5), # subsample ratio of columns when construction each tree
    eta = 0.1, # learning rate
    gamma = 0, # minimum loss reduction
    min_child_weight = 1,  # minimum sum of instance weight (hessian) needed ina child
    subsample = 1 # subsample ratio of the training instances
  ))

frac.train = 0.6


for (cproduct in c(products)){

  print(cproduct)

  climate <- readRDS(paste0("data/grid.",cproduct,".ERA5.RDS")) %>%
    left_join(dataC02.all,
              by = c("year")) %>%
    dplyr::select(-any_of(c("product",
                          "model",
                          "year == cyear")))

  GPPproduct.coords <- readRDS("./outputs/Amazon.coord.GPP.products.ILF.RDS") %>%
    filter(model == cproduct)

  GPP.products <- df.all %>%
    filter(product == cproduct) %>%
    mutate(GPP = value/1000*365) %>%
    dplyr::select(product,lat,lon,year,month,GPP) %>%
    mutate(model.lon.lat = paste0(product,".",
                                  round(lon,digits = 2),
                                  ".",
                                  round(lat,digits = 2))) %>%
    filter(model.lon.lat %in% GPPproduct.coords[["model.lon.lat"]]) %>%
    dplyr::select(-c(model.lon.lat))

  # GPP.products.NA <- GPP.products %>%
  #   filter(is.na(GPP)) %>%
  #   dplyr::select(lat,lon) %>%
  #   distinct()
  #
  # world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  # Amazon.shp <- read_sf(dsn = "/home/femeunier/Downloads/AmazonBasinLimits/",
  #                       layer = "amazon_sensulatissimo_gmm_v1")
  #
  # ggplot() +
  #
  #   geom_tile(data = GPP.products.NA,
  #             aes(x = lon, y = lat), alpha = 0.5, fill = NA,
  #             linewidth = 0.5, color = "black",
  #             show.legend = FALSE) +
  #
  #   geom_sf(data = world,
  #           fill = NA, color = "black") +
  #
  #   geom_sf(data = Amazon.shp,fill = NA, color = "red", fill = NA) +
  #
  #   # coord_sf(xlim = c(-15, 50),
  #   #          ylim = c(-20, 15)) +
  #   coord_sf(xlim = c(-90, -30),
  #            ylim = c(-25, 12)) +
  #   labs(x = "",y = "",fill = "") +
  #   # facet_wrap(~ model) +
  #   theme_map() +
  #   scale_fill_gradient(limits = c(2.5,3.5),oob = scales::squish,
  #                       low = "white", high = "darkgreen",
  #                       breaks = c(2.5,3,3.5)) +
  #   theme(text = element_text(size = 20),
  #         legend.position = "top")

  in_out <- GPP.products %>%
    filter(!is.na(GPP)) %>%
    left_join(climate,
              by = c("lat","lon","year","month")) %>%
    mutate(id = 1:n())

  cccdf <-  in_out %>%
    group_by(year,lat,lon) %>%
    mutate(group = sample(
      c("train", "validation", "test"),
      size = n(),
      replace = TRUE,
      prob = c(as.numeric(frac.train),(1-as.numeric(frac.train))/2,(1-as.numeric(frac.train))/2))) %>%
    ungroup() %>%
    dplyr::select(-c(product,GPP))

  all.data <- cbind(cccdf %>%
                      dplyr::select(-c(id)),
                    in_out %>%
                      dplyr::select(GPP)) %>%
    na.omit()

  op.file <- paste0("./outputs/",cproduct,".vs.climate.Amazon.ILF.ERA5.RDS")

  train <- cccdf %>%
    filter(group == "train") %>%
    dplyr::select(-group)

  validation <- cccdf %>%
    filter(group == "validation") %>%
    dplyr::select(-group)

  test <- cccdf %>%
    filter(group == "test") %>%
    dplyr::select(-group)

  # Training data
  data <- as.matrix(train %>%
                      dplyr::select(-id))
  label <- in_out %>%
    filter(id %in% (train[["id"]])) %>%
    pull(GPP)

  # Validation data
  validation.data <- as.matrix(validation %>%
                                 dplyr::select(-id))
  validation.label <- in_out %>%
    filter(id %in% (validation[["id"]])) %>%
    pull(GPP)

  # Test data
  test.data <- as.matrix(test %>%
                           dplyr::select(-id))
  test.label <- in_out %>%
    filter(id %in% (test[["id"]])) %>%
    pull(GPP)

  xgb_model <- caret::train(
    data,label,
    trControl = xgb_trcontrol,
    tuneGrid = xgb_grid,
    method = "xgbTree",
    nthread = 16,
    verbosity = 1)

  # We rerun with the best set of parameters, with test and validation data together
  xgb_best_model <- caret::train(
    x = rbind(data,
              validation.data),
    y = c(label,
          validation.label),
    trControl = xgb_trcontrol,
    tuneGrid = xgb_model$bestTune,
    method = "xgbTree",
    nthread = 16,
    verbosity = 1)

  xgb_best_model$training.data <- data
  xgb_best_model$labels <- label

  xgb_best_model$validation.data <- validation.data
  xgb_best_model$validation.labels <- validation.label

  xgb_best_model$test.data <- test.data
  xgb_best_model$test.labels <- test.label

  saveRDS(xgb_best_model,
          op.file)

}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/test.XGBoost.SIF.ERA5.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

