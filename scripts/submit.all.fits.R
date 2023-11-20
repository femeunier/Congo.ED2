rm(list = ls())

library(dplyr)
library(tidyr)
library(ED2scenarios)
library(Congo.ED2)
library(purrr)

biome.file <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2019.AI.RDS"

overwrite = TRUE
models <- TrENDY.analyses::get.model.names.TRENDY()
scenarios <- c("S2","S3")
prefix <- "XGB.fit.reclass."
transition.suffix <- "transitions_reclass"
continents <- c("Africa","America")
all.vars <- c("gpp","nep","npp")
biome.names <- c("Humid_large",
                 "Humid_low",
                 "Humid_seasonal",
                 "Dry_subhumid")

# biome.names <- readRDS(biome.file) %>%
#   pull(biome) %>% unique()

main.dir <- "/data/gent/vo/000/gvo00074/felicien/R"

biomes <- readRDS(biome.file)
biome.sum <- biomes %>%
  mutate(continent = Congo.ED2::coord2continent(lon,lon)) %>%
  group_by(model,continent,biome) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  mutate(model.continent.biome = paste0(model,".",continent,".",biome)) %>%
  filter(!is.na(continent))

grid <- base::expand.grid(
  list(
    model = models,
    scenario = scenarios,
    continent = continents,
    biome = biome.names
  ))  %>%
  mutate(model.continent.biome = paste0(model,".",continent,".",biome)) %>%
  ungroup() %>%
  dplyr::filter(model.continent.biome %in% biome.sum[["model.continent.biome"]]) %>%
  left_join(biome.sum %>% ungroup() %>%
              dplyr::select(model.continent.biome,N),
            by = "model.continent.biome") %>%
  filter(N > 50) %>%
  arrange(model)

list_dir <- list() ; jobname <- "job.sh"

for (irow in seq(1,nrow(grid))){

  crow <- grid[irow,]
  cdir <- file.path(main.dir,"outputs",
                    cname <- (gsub("\\/","",gsub(" ","",
               paste0("xgb.fit.",
                 crow[["model"]],".",
                 crow[["scenario"]],".",
                 crow[["continent"]],".",
                 crow[["biome"]])))))
  dir.create(cdir,showWarnings = FALSE)

  Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
  write.script.fit(
    file = Rscript.name,
    model = crow[["model"]],
    scenario = crow[["scenario"]],
    vars = all.vars,
    biome.names = crow[["biome"]],
    continents = crow[["continent"]],
    xgb.model.prefix = gsub("\\/","",gsub(" ","",
                                          paste0(prefix,
                                                 crow[["model"]],".",
                                                 crow[["scenario"]],".",
                                                 crow[["continent"]],".",
                                                 crow[["biome"]]))),
    biome.file = biome.file,
    frac.train = 0.6,
    overwrite = overwrite,
    transition.suffix = transition.suffix)


  # Create job file
  ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                           nodes = 1,ppn = 16,mem = 64,walltime = 24,
                           prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                           CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                           Rscript = Rscript.name)
  list_dir[[cname]] = cdir

}

dumb <- write_bash_submission(file = file.path(getwd(),"All.fits.XGB.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/submit.all.fits.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
