fit.CC.vs.climate.RS.coordlist <- function(product = "NIR",
                                           coord.list = "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Amazon.coord.ILF.RDS",
                                           xgb.model.prefix = "xgb.model",
                                           grid.suffix = "",
                                           frac.train = 0.6,
                                           overwrite = TRUE,
                                           climate.vars = c("tmp","tmin","tmax","spfh","VPD","pre","dswrf","dlwrf")){

  cproduct <- product
  if (!file.exists(coord.list)){
    stop("Coord list does not exist")
  } else {
    coord.list <- readRDS(coord.list) %>%
      filter(model == "DLEM") %>%
      mutate(lon.lat = paste0(lon,".",lat)) %>%
      dplyr::select(-any_of(c("model","model.lon.lat")))
  }

  grid.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",product,".",grid.suffix,".RDS")
  model.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/GPP.products.RDS")

  all.files <- c(grid.file,model.file)
  if (!all(file.exists(all.files))){
    stop(paste("Not all files exist, check:",
               all.files[which(!file.exists(all.files))]))
  }

  all.models <- readRDS(model.file) %>%
    filter(product == cproduct) %>%
    dplyr::select(-product)

  all.climate.vars <- unique(c("lon","lat",
                               "year","month",
                               "model",climate.vars))

  all.grids <- readRDS(grid.file) %>%
    dplyr::select(any_of(c(all.climate.vars)))

  CC.Trendy <- all.models

  # Merge

  modelled.sink <- CC.Trendy %>%
    ungroup() %>%
    mutate(lat = round(lat,digits = 2),
           lon = round(lon,digits = 2)) %>%
    mutate(lon.lat = paste0(lon,".",lat))

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


  sink.vs.climate <- modelled.sink %>%
    left_join(all.grids %>%
                dplyr::select(-starts_with("product")) %>%
                mutate(lat = round(lat,digits = 2),
                       lon = round(lon,digits = 2)),
              by = c("year","lat","lon","month")) %>%
    left_join(dataC02.all,
              by = c("year")) %>%
    ungroup()

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
    sink.vs.climate[[cvar]] <- sink.vs.climate[[cvar]]*365/1000
  }

  ccdf <- sink.vs.climate %>%
    ungroup() %>%
    mutate(lon.lat = paste0(lon,".",lat)) %>%
    filter(lon.lat %in% coord.list[["lon.lat"]]) %>%
    mutate(id = 1:n())

  if (!all((all.climate.vars %in% climate.vars))){
    ccdf.bis <- ccdf %>%
      dplyr::select(-any_of(all.climate.vars[!(all.climate.vars %in% climate.vars)]))
  } else {
    ccdf.bis <- ccdf
  }

  cccdf <- ccdf %>%
    dplyr::select(-any_of(c("time","continent","model.lat.lon","model.lon.lat",
                            "gpp","npp","nep","ra","rh","nbp"))) %>%
    dplyr::select(
      where(
        ~!all((.x == mean(.x,na.rm = TRUE)))
      )
    ) # remove constant columns (full of 0 for instance)

  cccdf <-  cccdf %>%
    group_by(any_of(c(year,lat,lon))) %>%
    mutate(group = sample(
      c("train", "validation", "test"),
      size = n(),
      replace = TRUE,
      prob = c(as.numeric(frac.train),(1-as.numeric(frac.train))/2,(1-as.numeric(frac.train))/2))) %>%
    ungroup()

  if (!all((all.climate.vars %in% climate.vars))){
    cccdf.bis <- cccdf %>%
      dplyr::select(-any_of(all.climate.vars[!(all.climate.vars %in% climate.vars)]))
  } else {
    cccdf.bis <- cccdf
  }

  # switch
  ccdf.tris <- ccdf.bis ; ccdf.bis <- ccdf ; ccdf <- ccdf.tris
  cccdf.tris <- cccdf.bis ; cccdf.bis <- cccdf ; cccdf <- cccdf.tris

  for (cvar in vars){

    print(paste0("- ",cvar))

    all.data <- cbind(cccdf %>%
                        dplyr::select(-c(id)),
                      ccdf %>%
                        dplyr::select(!!cvar)) %>%
      na.omit() %>%
      # group_by(lat,lon) %>%
      # filter(!all(get(cvar) == 0)) %>%
      ungroup()

    op.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/",
                      xgb.model.prefix,".",cvar,".RDS")
    if (file.exists(op.file) & !as.logical(overwrite)) next()

    if (nrow(all.data) == 0) next()

    train <- cccdf %>%
      filter(group == "train") %>%
      dplyr::select(-group)

    train.bis <- cccdf.bis %>%
      filter(group == "train") %>%
      dplyr::select(-group)

    validation <- cccdf %>%
      filter(group == "validation") %>%
      dplyr::select(-group)

    validation.bis <- cccdf.bis %>%
      filter(group == "validation") %>%
      dplyr::select(-group)

    test <- cccdf %>%
      filter(group == "test") %>%
      dplyr::select(-group)

    test.bis <- cccdf.bis %>%
      filter(group == "test") %>%
      dplyr::select(-group)

    # Training data
    data <- as.matrix(train %>%
                        dplyr::select(-id))
    label <- ccdf %>%
      filter(id %in% (train[["id"]])) %>%
      pull(!!cvar)

    data.bis <- as.matrix(train.bis %>%
                            dplyr::select(-id))
    label.bis <- ccdf.bis %>%
      filter(id %in% (train.bis[["id"]])) %>%
      pull(!!cvar)

    # Validation data
    validation.data <- as.matrix(validation %>%
                                   dplyr::select(-id))
    validation.label <- ccdf %>%
      filter(id %in% (validation[["id"]])) %>%
      pull(!!cvar)

    validation.data.bis <- as.matrix(validation.bis %>%
                                       dplyr::select(-id))
    validation.label.bis <- ccdf.bis %>%
      filter(id %in% (validation.bis[["id"]])) %>%
      pull(!!cvar)

    # Test data
    test.data <- as.matrix(test %>%
                             dplyr::select(-id))
    test.label <- ccdf %>%
      filter(id %in% (test[["id"]])) %>%
      pull(!!cvar)

    test.data.bis <- as.matrix(test.bis %>%
                                 dplyr::select(-id))
    test.label.bis <- ccdf.bis %>%
      filter(id %in% (test.bis[["id"]])) %>%
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

    xgb_best_model$training.data <- data.bis
    xgb_best_model$labels <- label.bis

    xgb_best_model$validation.data <- validation.data.bis
    xgb_best_model$validation.labels <- validation.label.bis

    xgb_best_model$test.data <- test.data.bis
    xgb_best_model$test.labels <- test.label.bis

    saveRDS(xgb_best_model,
            op.file)

  }

  return(TRUE)
}

