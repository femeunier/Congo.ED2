rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)
library(reshape2)
library(lattice)
library(rgdal)
library(YGB)
library(caret)
library(TrENDY.analyses)
library(stringr)
library(randomForest)
library(ggpointdensity)
library(lubridate)
library(xgboost)
library(zoo)

overwrite = TRUE
models <- TrENDY.analyses::get.model.names.TRENDY()
models <- c("CABLE-POP")

all.models <- all.grids <- all.grids.transitions <- data.frame()

existing.models <- c()
for (cmodel in models){

  grid.file <- paste0("./data/grid.",cmodel,".RDS")
  grid.file.transition <- paste0("./data/grid.",cmodel,".transitions.RDS")
  model.file <- paste0("./outputs/Trendy.",cmodel,".S3.CC.pantropical.v11.RDS")

  if (!all(file.exists(model.file,grid.file,grid.file.transition))){
    next()
  }

  all.models <- bind_rows(all.models,
                          readRDS(model.file) %>%
                            dplyr::select(-starts_with("time.unit")) %>%
                            mutate(model = cmodel))

  all.grids <- bind_rows(all.grids,
                         readRDS(grid.file) %>%
                           mutate(model = cmodel))

  all.grids.transitions <- bind_rows(all.grids.transitions,
                                     readRDS(grid.file.transition) %>%
                                       rename(year = time) %>%
                                       mutate(model = cmodel))


  existing.models <- c(existing.models,
                       cmodel)
}

models <- existing.models

CC.Trendy <- all.models %>%
  # na.omit() %>%
  mutate(Continent = Congo.ED2::coord2continent(lon,lon)) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
  filter(Continent %in% c("Africa"))

# Merge

modelled.sink <- CC.Trendy %>%
  ungroup() %>%
  mutate(lat = round(lat,digits = 2),
         lon = round(lon,digits = 2))

# CO2
dataC02 <- read.table("./data/CO2_1700_2019_TRENDYv2020.txt",
                      stringsAsFactors = FALSE) %>%
  mutate(year = as.numeric(str_sub(V1,7,10)),
         CO2 = as.numeric(str_sub(V1,12,17))) %>%
  dplyr::select(year,CO2) %>%
  mutate(month = 1)

dataC02.all <- data.frame(year = rep(sort(unique(dataC02$year)),12)) %>%
  group_by(year) %>%
  mutate(month = 1:12) %>%
  arrange(year) %>%
  left_join(dataC02,
            by = c("year","month")) %>%
  mutate(CO2 = na.approx(CO2))

sink.vs.climate <- modelled.sink %>%
  left_join(all.grids %>%
              mutate(lat = round(lat,digits = 2),
                     lon = round(lon,digits = 2)),
            by = c("model","year","lat","lon","month")) %>%

  left_join(all.grids.transitions %>%
              mutate(lat = round(lat,digits = 2),
                     lon = round(lon,digits = 2)),
            by = c("model","year","lat","lon")) %>%

  # na.omit() %>%
  left_join(dataC02.all,
            by = c("year","month")) %>%
  ungroup()

all.corr <- df.importance <- df.predict <-
  data.frame()

TF <- sink.vs.climate %>%
  group_by(model,lat,lon,year) %>%
  filter(!any(is.na(pre))) %>%
  ungroup() %>%
  mutate(N = days_in_month(as.Date(paste0(year,"/",sprintf("%20d",month),"/01")))) %>%
  mutate(pre = pre*4*N) %>%
  group_by(model,lon,lat,month) %>%
  summarise(Pmm = mean(pre),
            N = mean(N),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(diff = Pmm - 3.33*N) %>%
  group_by(model,lon,lat) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(model,lat,lon,month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(lat,lon,month) %>%
  mutate(MCWD = min(CWD),
         MAP = sum(Pmm)) %>%
  filter(MCWD >= -150) %>%
  mutate(model.lat.lon = paste0(model,".",lat,".",lon))

xgb_trcontrol <- caret::trainControl(
  method = "cv",
  number = 15,
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


all.corr <- df.importance <- df.predict <-
  data.frame()

vars <- c("gpp")

sink.vs.climate <- sink.vs.climate %>%
  mutate(gpp = gpp*86400*365,
         ra = ra*86400*365,
         rh = rh*86400*365,
         npp = npp*86400*365,
         nep = nep*86400*365)

for (cmodel in models){

  print(cmodel)

  cdf <- sink.vs.climate %>%
    filter(model == cmodel) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  ccdf <- cdf %>%
    ungroup() %>%
    filter(model.lat.lon %in% TF[["model.lat.lon"]]) %>%
    # na.omit() %>%
    mutate(id = 1:n())

  cccdf <- ccdf %>%
    dplyr::select(-c(time,
                     model,Continent,model.lat.lon,
                     gpp,npp,nep,ra,rh)) %>%
    dplyr::select(
      where(~!all((.x) == 0)))

  # selected <- cccdf %>%
  #   slice_sample(prop = 0.7) %>%
  #   pull(id) %>%
  #   sort()

  selected <- cccdf %>%
    # slice_sample(prop = 0.7) %>%
    # group_by(lat,lon) %>%
    filter(year %in% sample(unique(year),0.7*length(unique(year)),replace = FALSE)) %>%
    pull(id) %>%
    sort()

  for (cvar in vars){

    print(paste0("- ",cvar))

    if (nrow(all.corr) > 0){
      if (nrow(all.corr %>%
               filter(var == cvar,
                      model == cmodel)) > 0){
        next()
      }
    }


    all.data <- cbind(cccdf %>%
                        dplyr::select(-c(id)),
                      ccdf %>%
                        dplyr::select(!!cvar)) %>%
      na.omit() %>%
      group_by(lat,lon) %>%
      filter(!all(get(cvar) == 0)) %>%
      ungroup()

    op.file <- paste0("./outputs/",
                      "xgb_model.time.latlon.Africa.S3.",cmodel,".",cvar,".RDS")

    if (file.exists(op.file) & !overwrite) next()

    if (nrow(all.data) == 0) next()

    LM <- lm(data = all.data,
             formula = as.formula(paste0(cvar," ~ .")))

    train <- cccdf %>%
      filter(id %in% selected)
    test <- cccdf %>%
      filter(!(id %in% selected))

    data <- as.matrix(train %>%
                        dplyr::select(-id))
    label <- ccdf %>%
      filter(id %in% selected) %>%
      pull(!!cvar)

    test.data <- as.matrix(test %>%
                             dplyr::select(-id))
    test.label <- ccdf %>%
      filter(!(id %in% selected)) %>%
      pull(!!cvar)

    xgb_model <- caret::train(
      data,label,
      trControl = xgb_trcontrol,
      tuneGrid = xgb_grid,
      method = "xgbTree",
      nthread = 16,
      verbosity = 1)

    # xgb_model <- train(data,
    #                    label,
    #                    method = "ranger",
    #                    importance = TRUE)

    xgb_model$trainingData <- data
    xgb_model$labels <- label
    xgb_model$test.data <- test.data
    xgb_model$test.labels <- test.label

    saveRDS(xgb_model,
            op.file)

    # saveRDS(cbind(test.data,ccdf %>%
    #                 filter(!(id %in% selected)) %>%
    #                 dplyr::select(!!cvar)),
    #         paste0("./outputs/",
    #                "test.data.",cmodel,".",cvar,".RDS"))

    predicted <- predict(xgb_model,
                         test.data)

    df.predict <- bind_rows(df.predict,
                            data.frame(pred = predicted,
                                       obs = test.label) %>%
                              mutate(model = cmodel,
                                     var = cvar))

    corr <- ccdf %>%
      summarise(r2.lm = summary(LM)[["adj.r.squared"]],
                r2.rf = 1 - sum((predicted- test.label)**2)/sum(((test.label - mean(test.label))**2)),
                .groups = "keep") %>%
      mutate(model = cmodel,
             var = cvar)

    # plot(predicted,test.label)

    all.corr <- bind_rows(all.corr,
                          corr)
  }
}

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/CC.vs.climate.LUH.R hpc:/data/gent/vo/000/gvo00074/felicien/R

