rm(list = ls())

library(epwshiftr)
library(dplyr)
library(CongoAS)
library(RNetCDF)
library(stringr)
library(ncdf4)
library(ncdf4.helpers)
library(reshape2)
library(lubridate)
library(tidyr)

scenarios = c("historical","ssp126","ssp245","ssp370","ssp585")
variable = "tas"
variants = "r1i1p1f1"

overwrite = TRUE

########################################################################################################

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2")] <- "CMIP"
activities[grepl("land",scenarios)] <- "LUMIP"

########################################################################################################
# Functions

download_size <- function(url) {
  as.numeric(httr::HEAD(url)$headers$`content-length`)
}

retry.func <- function (expr, isError = function(x) inherits(x, "try-error"),
                        maxErrors = 5, sleep = 0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]",
                    utils::capture.output(utils::str(retval)))
      # warning(msg)
      return(0)
    }
    else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]",
                    attempts, maxErrors, utils::capture.output(utils::str(retval)))
      # warning(msg)
    }
    if (sleep > 0)
      Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(1)
}

options(timeout=6000)

# data.nodes.status <- get_data_node()

######################################################################################################################

df.all <- data.frame()

for (iscenario in seq(1,length(scenarios))){

  dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
                               scenarios[iscenario])
  dest.dir <- file.path(dest.dir.scenar,variable)

  cscenario <- scenarios[iscenario]

  pr <- init_cmip6_index(activity = activities[iscenario],
                         variable = variable,
                         frequency = 'mon',
                         experiment = scenarios[iscenario],
                         source = NULL,
                         variant = variants,
                         replica = FALSE,
                         latest = TRUE,
                         resolution = NULL,
                         limit = 10000L,
                         data_node = NULL)

  if (nrow(pr) == 0) next()

  files2download.prc <- pr

  # Add model loop

  print(paste("--- Looking for files for scenario ",scenarios[iscenario], " and variable ",variable))

  models <- unique(files2download.prc$source_id)

  for (imodel in seq(1, length(models))){

    cmodel <- models[imodel]
    print(cmodel)
    cdf <- files2download.prc %>%
      filter(source_id == cmodel)

    if (nrow(cdf) == 0) next()

    dest.files <- file.path(dest.dir,
                            basename(cdf$file_url))

    if (all(file.exists(dest.files))){
      df.all <- bind_rows(df.all,
                          data.frame(model = cmodel,
                                     scenario = cscenario))
    }
  }
}

df.all.wide <- df.all %>%
  mutate(exist = TRUE) %>%
  pivot_wider(values_from = exist,
              names_from = scenario)
df.selected <- df.all.wide %>%
  filter(historical,
         ssp126,
         ssp245,
         ssp370,
         ssp585) %>%
  filter(model != "IITM-ESM")

sites <- bind_rows(readRDS("./outputs/Amazon.coord.ERA5.RDS"),
                   readRDS("./outputs/Congo.coord.ERA5.RDS"))

for (iscenario in seq(1,length(scenarios))){

  dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
                               scenarios[iscenario])

  dest.dir <- file.path(dest.dir.scenar,variable)

  pr <- init_cmip6_index(activity = activities[iscenario],
                         variable = variable,
                         frequency = 'mon',
                         experiment = scenarios[iscenario],
                         source = NULL,
                         variant = variants,
                         replica = FALSE,
                         latest = TRUE,
                         resolution = NULL,
                         limit = 10000L,
                         data_node = NULL)

  if (nrow(pr) == 0) next()


  files2download.prc <- pr %>%
    filter(source_id %in% df.selected[["model"]])


  print(paste("--- Looking for files for scenario ",scenarios[iscenario], " and variable ",variable))

  models <- unique(files2download.prc$source_id)

  for (imodel in seq(1,length(models))){

    df.month <- data.frame()
    cmodel <- models[imodel]
    print(cmodel)

    OP.file <- paste0("./outputs/","df.monthly.tas.basin.",scenarios[iscenario],".",models[imodel],".RDS")

    if (!overwrite & file.exists(OP.file)){
      next()
    }

    dest.files <- file.path(dest.dir,
                            basename(files2download.prc %>%
                                       filter(source_id == cmodel) %>% pull(file_url)))

    if (all(file.exists(dest.files))){

      tmp <- read.and.filter.ncfiles(ncfiles = dest.files,
                                     coord.analysis = continent2coord("Tropics"),
                                     var = variable,
                                     aggr = FALSE) %>%
        filter(year %in% 1980:2100) %>%
        mutate(lat.lon = paste0(lat,lon,sep= "_"))

      if (nrow(tmp) == 0) next()

      search <- tmp %>%
        group_by(lat,lon) %>%
        slice_head(n = 1) %>%
        ungroup()

      df.dist <- data.frame()
      for (isite in seq(1,nrow(sites))){

        print(isite/nrow(sites))
        clat <- sites[isite,"lat"]
        clon <- sites[isite,"lon"]

        df.dist <- bind_rows(df.dist,
                             search %>%
          mutate(dist = sqrt((lat-clat)**2 + (lon-clon)**2)) %>%
          arrange(dist) %>%
          slice_head(n = 1))
      }

      df.dist <- df.dist %>%
        dplyr::select(lat,lon,lat.lon) %>%
        distinct()

      df.month <- tmp %>%
        dplyr::filter(lat.lon %in% df.dist[["lat.lon"]]) %>%
        dplyr::select(-lat.lon) %>%
        mutate(scenario = scenarios[iscenario])

      saveRDS(df.month,
              OP.file)

    }
  }
}


# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/compile.tas.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
# scp /home/femeunier/Documents/projects/Congo.ED2/outputs/ILF2020.df hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/
