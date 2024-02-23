rm(list = ls())

library(dplyr)
library(tidyr)
library(ED2scenarios)
library(Congo.ED2)
library(purrr)

biome.file <- "/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2022_global.RDS"

overwrite = TRUE
models <- TrENDY.analyses::get.model.names.TRENDY()

scenarios <- c("S2")
prefix <- "XGB.fit.global."
GridSuffix = ".all.years_global"
transition.suffix <- "transitions_reclass"
all.vars <- c("gpp","nep","npp")

biome.names <- readRDS(biome.file) %>%
  pull(biome) %>% unique()

main.dir <- "/data/gent/vo/000/gvo00074/felicien/R"

biomes <- readRDS(biome.file)
biome.sum <- biomes %>%
  group_by(model,biome) %>%
  summarise(N = n(),
            .groups = "keep") %>%
  mutate(model.biome = paste0(model,".",biome))

grid <- base::expand.grid(
  list(
    model = models,
    scenario = scenarios,
    biome = biome.names
  ))  %>%
  mutate(model.biome = paste0(model,".",biome)) %>%
  ungroup() %>%
  dplyr::filter(model.biome %in% biome.sum[["model.biome"]]) %>%
  left_join(biome.sum %>% ungroup() %>%
              dplyr::select(model.biome,N),
            by = "model.biome") %>%
  filter(N > 50) %>%
  arrange(model)

list_dir <- list() ; jobname <- "job.sh"

for (irow in seq(1,nrow(grid))){

  crow <- grid[irow,]
  cdir <- file.path(main.dir,"outputs",
                    cname <- (gsub("\\/","",gsub(" ","",
                                                 paste0("xgb.fit.global.",
                                                        crow[["model"]],".",
                                                        crow[["scenario"]],".",
                                                        crow[["biome"]])))))
  dir.create(cdir,showWarnings = FALSE)

  Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
  write.script.fit(
    file = Rscript.name,
    model = crow[["model"]],
    scenario = crow[["scenario"]],
    vars = all.vars,
    biome.names = crow[["biome"]],
    continents = "",
    xgb.model.prefix = gsub("\\/","",gsub(" ","",
                                          paste0(prefix,
                                                 crow[["model"]],".",
                                                 crow[["scenario"]],".",
                                                 crow[["biome"]]))),
    grid.suffix = GridSuffix,
    biome.file = biome.file,
    frac.train = 0.6,
    overwrite = overwrite,
    transition.suffix = transition.suffix,
    CC.suffix = "CC.global.v11")


  # Create job file
  ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                           nodes = 1,ppn = 16,mem = 64,walltime = 72,
                           prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                           CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                           Rscript = Rscript.name)
  list_dir[[cname]] = cdir

}

dumb <- write_bash_submission(file = file.path(getwd(),"All.fits.XGB.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/submit.all.fits.global.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
