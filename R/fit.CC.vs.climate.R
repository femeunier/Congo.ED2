fit.CC.vs.climate <- function(model = "CABLE-POP",
                              scenario = "S2",
                              vars = c("gpp","npp","nep","ra","rh","nbp"),
                              biome.names = c("Tropical seasonal forest/savanna"),
                              continents = c("Africa"),
                              xgb.model.prefix = "xgb.model",
                              grid.suffix = "",
                              frac.train = 0.6,
                              biome.file = "/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2022.AI.RDS",
                              overwrite = TRUE,
                              transition.suffix = "transitions",
                              CC.suffix = "CC.pantropical.v11",
                              climate.vars = c("tmp","tmin","tmax","spfh","VPD","pre","dswrf","dlwrf")){


  if (continents == ""){
    continents <- c("Africa","America","Australasia")
  }

  biomes <- readRDS(biome.file) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  grid.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",model,grid.suffix,".RDS")
  model.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",model,".",scenario,".",CC.suffix,".RDS")

  if (scenario == "S3"){
    grid.file.transition <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",model,".",transition.suffix,".RDS")
    all.files <- c(model.file,grid.file,grid.file.transition)
  } else {
    all.files <- c(model.file,grid.file)
  }

  if (!all(file.exists(all.files))){
    stop(paste("Not all files exist, check:",
               all.files[which(!file.exists(all.files))]))
  }

  all.models <- readRDS(model.file) %>%
    dplyr::select(-starts_with("time.unit"))

  all.grids <- readRDS(grid.file) %>%
    dplyr::select(c("lon","lat",
                    "year","month",
                    "model",
                    climate.vars))

  if (scenario == "S3"){
    all.grids.transitions <- readRDS(grid.file.transition) %>%
      rename(year = time)
  }

  CC.Trendy <- all.models %>%
    mutate(continent = Congo.ED2::coord2continent(lon,lon)) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
    filter(continent %in% continents)

  existing.vars <- intersect(vars,colnames(CC.Trendy))

  # Merge

  modelled.sink <- CC.Trendy %>%
    ungroup() %>%
    mutate(lat = round(lat,digits = 2),
           lon = round(lon,digits = 2)) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  # CO2
  dataC02 <- read.table("/data/gent/vo/000/gvo00074/felicien/R/data/global_co2_ann_1700_2022.txt",
                        stringsAsFactors = FALSE) %>%
    rename(year = V1,
           CO2 = V2)

  dataC02.all <- data.frame(year = sort(unique(c(dataC02$year,2020:2023)))) %>%
    arrange(year) %>%
    left_join(dataC02,
              by = c("year")) %>%
    mutate(CO2 = na.spline(CO2,method = "natural"))


  if (scenario == "S2"){

    sink.vs.climate <- modelled.sink %>%
      left_join(all.grids %>%
                  dplyr::select(-starts_with("model")) %>%
                  mutate(lat = round(lat,digits = 2),
                         lon = round(lon,digits = 2)),
                by = c("year","lat","lon","month")) %>%
      left_join(dataC02.all,
                by = c("year")) %>%
      ungroup()
  } else if (scenario == "S3"){

    sink.vs.climate <- modelled.sink %>%
      left_join(all.grids %>%
                  dplyr::select(-starts_with("model")) %>%
                  mutate(lat = round(lat,digits = 2),
                         lon = round(lon,digits = 2)),
                by = c("year","lat","lon","month")) %>%

      left_join(all.grids.transitions %>%
                  dplyr::select(-starts_with("model")) %>%
                  mutate(lat = round(lat,digits = 2),
                         lon = round(lon,digits = 2)),
                by = c("year","lat","lon")) %>%

      left_join(dataC02.all,
                by = c("year")) %>%
      ungroup()

  }

  TF <- biomes %>%
    mutate(lat = round(lat,digits = 2),
           lon = round(lon,digits = 2)) %>%
    filter(biome %in% biome.names) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

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

  for (cvar in existing.vars){
    sink.vs.climate[[cvar]] <- sink.vs.climate[[cvar]]*86400*365
  }

  for (cvar in vars){

    print(paste0("- ",cvar))

    ccdf <- sink.vs.climate %>%
      ungroup() %>%
      dplyr::filter(!is.na(get(cvar))) %>%
      mutate(lat = round(lat,digits = 2),
             lon = round(lon,digits = 2)) %>%
      mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
      filter(model.lat.lon %in% TF[["model.lat.lon"]]) %>%
      mutate(id = 1:n())

    cccdf <- ccdf %>%
      dplyr::select(-c(any_of(c("time","continent","model.lat.lon","model.lon.lat",
                                "gpp","npp","nep","ra","rh","nbp")))) %>%
      dplyr::select(
        where(
          ~!all((.x == mean(.x,na.rm = TRUE)))
        )
      ) # remove constant columns (full of 0 for instance)


    # selected <- cccdf %>%
    #   filter(year %in% sample(unique(year),
    #                           as.numeric(frac.train)*length(unique(year)),
    #                           replace = FALSE)) %>%
    #   pull(id) %>%
    #   sort()


    cccdf <-  cccdf %>%
      group_by(year,lat,lon) %>%
      mutate(group = sample(
        c("train", "validation", "test"),
        size = n(),
        replace = TRUE,
        prob = c(as.numeric(frac.train),(1-as.numeric(frac.train))/2,(1-as.numeric(frac.train))/2))) %>%
      ungroup()


    all.data <- cbind(cccdf %>%
                        dplyr::select(-c(id)),
                      ccdf %>%
                        dplyr::select(!!cvar)) %>%
      na.omit() %>%
      group_by(lat,lon) %>%
      filter(!all(get(cvar) == 0)) %>%
      ungroup()

    op.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/",
                      xgb.model.prefix,".",cvar,".RDS")
    if (file.exists(op.file) & !as.logical(overwrite)) next()

    if (nrow(all.data) == 0) next()


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
    label <- ccdf %>%
      filter(id %in% (train[["id"]])) %>%
      pull(!!cvar)

    # Validation data
    validation.data <- as.matrix(validation %>%
                                   dplyr::select(-id))
    validation.label <- ccdf %>%
      filter(id %in% (validation[["id"]])) %>%
      pull(!!cvar)

    # Test data
    test.data <- as.matrix(test %>%
                             dplyr::select(-id))
    test.label <- ccdf %>%
      filter(id %in% (test[["id"]])) %>%
      pull(!!cvar)

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

  return(TRUE)
}

