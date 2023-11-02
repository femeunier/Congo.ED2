
fit.CC.vs.climate <- function(model = "CABLE-POP",
                              scenario = "S2",
                              vars = c("gpp","npp","nep","ra","rh"),
                              biome.names = c("Tropical seasonal forest/savanna"),
                              continents = c("Africa"),
                              xgb.model.prefix = "xgb.model",
                              frac.train = 0.7,
                              biome.file = "/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2019.RDS",
                              overwrite = TRUE){

  biomes <- readRDS(biome.file) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon))

  grid.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",model,".RDS")
  model.file <- paste0("/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.",model,".",scenario,".CC.pantropical.v11.RDS")

  if (scenario == "S3"){
    grid.file.transition <- paste0("/data/gent/vo/000/gvo00074/felicien/R/data/grid.",model,".transitions.RDS")
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

  all.grids <- readRDS(grid.file)

  if (scenario == "S3"){
    all.grids.transitions <- readRDS(grid.file.transition) %>%
      rename(year = time)
  }

  CC.Trendy <- all.models %>%
    mutate(continent = Congo.ED2::coord2continent(lon,lon)) %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
    filter(continent %in% continents)

  # Merge

  modelled.sink <- CC.Trendy %>%
    ungroup() %>%
    mutate(lat = round(lat,digits = 2),
           lon = round(lon,digits = 2))

  # CO2
  dataC02 <- read.table("/data/gent/vo/000/gvo00074/felicien/R/data/CO2_1700_2019_TRENDYv2020.txt",
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


  if (scenario == "S2"){

    sink.vs.climate <- modelled.sink %>%
      left_join(all.grids %>%
                  dplyr::select(-starts_with("model")) %>%
                  mutate(lat = round(lat,digits = 2),
                         lon = round(lon,digits = 2)),
                by = c("year","lat","lon","month")) %>%
      left_join(dataC02.all,
                by = c("year","month")) %>%
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
                by = c("year","month")) %>%
      ungroup()

  }

  TF <- biomes %>%
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

  sink.vs.climate <- sink.vs.climate %>%
    mutate(gpp = gpp*86400*365,
           ra = ra*86400*365,
           rh = rh*86400*365,
           npp = npp*86400*365,
           nep = nep*86400*365)

  ccdf <- sink.vs.climate %>%
    ungroup() %>%
    mutate(model.lat.lon = paste0(model,".",lat,".",lon)) %>%
    filter(model.lat.lon %in% TF[["model.lat.lon"]]) %>%
    mutate(id = 1:n())

  cccdf <- ccdf %>%
    dplyr::select(-c(time,continent,model.lat.lon,
                     gpp,npp,nep,ra,rh))

  selected <- cccdf %>%
    filter(year %in% sample(unique(year),
                            as.numeric(frac.train)*length(unique(year)),
                            replace = FALSE)) %>%
    pull(id) %>%
    sort()

  for (cvar in vars){

    print(paste0("- ",cvar))

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

    xgb_model$trainingData <- data
    xgb_model$labels <- label
    xgb_model$test.data <- test.data
    xgb_model$test.labels <- test.label

    saveRDS(xgb_model,
            op.file)

  }

  return(TRUE)
}

