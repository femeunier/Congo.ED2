rm(list = ls())

####################################################################"
# Libraries

library(dplyr)
library(ggplot2)
library(ggthemes)
library(caret)
library(xgboost)
library(zoo)

####################################################################"
# Load and explore data

data <- readRDS("~/Downloads/DataFrame_Orchidee_CRUJRA.RDS")

# Specific year/month to plot
cdf2plot <- data %>%
  filter(year == year[1],
         month == 1)

# Worldmap of countries
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world,fill = NA,color = "grey") +
  geom_tile(data = cdf2plot ,
             aes(x = lon, y = lat,
                 fill = tmp)) +
  theme_map() +
  theme(panel.grid.major = element_blank(),
        legend.position = "none",
        text = element_text(size = 24))

# Compute time series over the entire region
ts <- data %>%
  group_by(year,month) %>%
  summarise(NBP.m = mean(mean_NBP,na.rm = TRUE),
            .groups = "keep")

ElNino <- data.frame(xinit = c(1997,2015),
                     xend = c(1998,2016) + 1) # Rough estimates of the Elnino years

ggplot() +
  geom_rect(data = ElNino,
            aes(xmin = xinit, xmax = xend,
                ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.5, color = NA) +
  geom_line(data = ts,
            aes(x = year + (month -1/2)/12,
                y = NBP.m)) +
  theme_bw()

# Define the seasonality
ts.norm <- ts %>%
  filter(year %in% c(1981:2010)) %>%               # Selected years to define the climatology
  group_by(month) %>%
  summarise(NBP.norm.m = mean(NBP.m,na.rm = TRUE), # Monthly mean
            NBP.norm.sd = sd(NBP.m,na.rm = TRUE),  # Monthly standard deviation
            .groups = "keep")

ts.anomaly <- ts %>%
  left_join(ts.norm,
            by = "month") %>%
  mutate(anomaly = NBP.m - NBP.norm.m,                       # Absolute anomalies
         anomaly.sd = (NBP.m - NBP.norm.m)/NBP.norm.sd)      # Normalized anomalies: # of monthly sd

ggplot(data = ts.anomaly) +
  geom_rect(data = ElNino,
            aes(xmin = xinit, xmax = xend,
                ymin = -Inf, ymax = Inf),
            fill = "lightgrey", alpha = 0.5, color = NA) +
  geom_line(aes(x = year + (month -1/2)/12,
                y = anomaly.sd)) +
  geom_hline(yintercept = 0, color = "black",linetype = 2) +
  theme_bw()


####################################################################"
# Fit machine learning model

# Climate variables
climate.vars <- c("tmp","pre","tmin","tmax","dlwrf","dswrf","spfh","VPD")

# Add coordinates + time dimension
all.input.vars <- c("lon","lat","year","month",climate.vars)

# Name of the y to fit
target.var <- c("mean_NBP")

# We also add yearly atmospheric data
dataC02.all <- read.table("/home/femeunier/Documents/projects/Santiago/data/global_co2_ann_1700_2024.txt",
                      stringsAsFactors = FALSE) %>%
  rename(year = V1,
         CO2 = V2)

# Merge climate/NBP with CO2
sink.vs.climate <- data %>%
  left_join(dataC02.all,
            by = "year")

# Unit conversion: kgC/m²/yr
sink.vs.climate[[target.var]] <- sink.vs.climate[[target.var]]*86400*365

# Machine learning settings
xgb_trcontrol <- caret::trainControl(
  method = "cv",
  number = 8,
  allowParallel = TRUE,
  verboseIter = TRUE,
  returnData = FALSE
)

# Hyperparameters to explore for the machine learning algorithm
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

# Fraction of the data that will be used for training, note (1-frac.train)/2 will be used for completely independent test
frac.train <- 0.6

# Adding a id column for each row
ccdf <- sink.vs.climate %>%
  ungroup() %>%
  mutate(id = 1:n())

# Removing some unnecessary columns and coordinates (only useful for some models)
cccdf <- ccdf %>%
  dplyr::select(-any_of(c("time","continent","model.lat.lon","model.lon.lat",
                          "gpp","npp","nep","ra","rh","nbp",target.var))) %>%
  dplyr::select(
    where(
      ~!all((.x == mean(.x,na.rm = TRUE)))
    )
  ) # remove constant columns (full of 0 for instance)

# Split the data into training/validatin/test
cccdf <-  cccdf %>%
  group_by(year,lat,lon) %>%
  mutate(group = sample(
    c("train", "validation", "test"),
    size = n(),
    replace = TRUE,
    prob = c(as.numeric(frac.train),
             (1-as.numeric(frac.train))/2,
             (1-as.numeric(frac.train))/2))) %>%
  ungroup()

all.data <- cbind(cccdf %>%
                    dplyr::select(-c(id)),
                  ccdf %>%
                    dplyr::select(!!target.var)) %>%
  na.omit() %>%
  ungroup()

train <- cccdf %>%
  filter(group == "train") %>%
  dplyr::select(-group)

validation <- cccdf %>%
  filter(group == "validation") %>%
  dplyr::select(-group)

test <- cccdf %>%
  filter(group == "test") %>%
  dplyr::select(-group)


# Training data formating
data <- as.matrix(train %>%
                    dplyr::select(-id))
labels <- ccdf %>%
  filter(id %in% (train[["id"]])) %>%
  pull(!!target.var)


# Validation data formating
validation.data <- as.matrix(validation %>%
                               dplyr::select(-id))
validation.labels <- ccdf %>%
  filter(id %in% (validation[["id"]])) %>%
  pull(!!target.var)

# Test data formating
test.data <- as.matrix(test %>%
                         dplyr::select(-id))
test.labels <- ccdf %>%
  filter(id %in% (test[["id"]])) %>%
  pull(!!target.var)

# First we optimize the machine learning model hyperparameters
xgb_model <- caret::train(
  data,labels,
  trControl = xgb_trcontrol,
  tuneGrid = xgb_grid,
  method = "xgbTree",
  nthread = 16,
  verbosity = 1)

# Then we rerun with the best set of hyperparameters, with test and validation data merged together
xgb_best_model <- caret::train(
  x = rbind(data,
            validation.data),
  y = c(labels,
        validation.labels),
  trControl = xgb_trcontrol,
  tuneGrid = xgb_model$bestTune,
  method = "xgbTree",
  nthread = 16,
  verbosity = 1)

# We add the dataset for further use later
xgb_best_model$training.data <- data
xgb_best_model$labels <- labels

xgb_best_model$validation.data <- validation.data
xgb_best_model$validation.labels <- validation.labels

xgb_best_model$test.data <- test.data
xgb_best_model$test.labels <- test.labels

summary(xgb_best_model$results)

# Saving the actual models
saveRDS(xgb_best_model,"XGB.model.RDS")

# Make model predictions
predicted <- predict(xgb_best_model,
                     ccdf[,xgb_model$finalModel$feature_names])

all.predicted <- cbind(ccdf,
                       predicted)

ggplot(data = all.predicted)+
  geom_hex(aes(x = mean_NBP,
               y = predicted)) +
  theme_bw() +
  geom_abline(slope = 1,
              intercept = 0, color = "red",linetype = 2)

# Model predictions specifically for the test data
predicted_test <- predict(xgb_best_model,
                          test.data[,xgb_best_model$finalModel$feature_names])

all_test <- bind_cols(test.data,
                      pred = predicted_test,
                      obs = xgb_best_model$test.labels)

ggplot(data = all_test)+
  geom_hex(aes(x = pred,
               y = obs)) +
  theme_bw() +
  geom_abline(slope = 1,
              intercept = 0, color = "red",linetype = 2)

# We save the model predictions for further use
saveRDS(all.predicted,
        paste0("./outputs/predictions.XGB.RDS"))
saveRDS(all_test,
        paste0("./outputs/predictions.XGB.test.RDS"))



