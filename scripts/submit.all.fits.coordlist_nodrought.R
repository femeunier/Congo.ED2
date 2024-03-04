rm(list = ls())

library(dplyr)
library(tidyr)
library(ED2scenarios)
library(Congo.ED2)
library(purrr)

overwrite = TRUE
models <- TrENDY.analyses::get.model.names.TRENDY()

suffix <- "Amazon"
scenarios <- c("S2")
prefix <- paste0("XGB.fit.JRA.historical.IFL",suffix,"_nodrought",".")
GridSuffix = ".JRA.historical"
transition.suffix <- "transitions_reclass"
all.vars <- c("gpp","nep","npp")
CC.suffix <- "CC.pantropical.v11.prime"

main.dir <- "/data/gent/vo/000/gvo00074/felicien/R"

grid <- base::expand.grid(
  list(
    model = models,
    scenario = scenarios
  ))

coord.list.file <- paste0("/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/",suffix,".coord.ILF.RDS")

list_dir <- list() ; jobname <- "job.sh"

for (irow in seq(1,nrow(grid))){

  crow <- grid[irow,]
  cdir <- file.path(main.dir,"outputs",
                    cname <- (gsub("\\/","",gsub(" ","",
                                                 paste0("xgb.fit.ILF",suffix,"_nodrought.",
                                                        crow[["model"]],".",
                                                        crow[["scenario"]])))))
  dir.create(cdir,showWarnings = FALSE)

  Rscript.name <- file.path(cdir,script.name <- "Rscript.R")
  write.script.fit.coordlist(
    file = Rscript.name,
    model = crow[["model"]],
    scenario = crow[["scenario"]],
    vars = all.vars,
    coord.list = coord.list.file,
    xgb.model.prefix = gsub("\\/","",gsub(" ","",
                                          paste0(prefix,
                                                 crow[["model"]],".",
                                                 crow[["scenario"]]))),
    grid.suffix = GridSuffix,
    frac.train = 0.6,
    overwrite = overwrite,
    transition.suffix = transition.suffix,
    CC.suffix = CC.suffix)

  # Create job file
  ED2scenarios::write_jobR(file = file.path(cdir,jobname),
                           nodes = 1,ppn = 16,mem = 64,walltime = 36,
                           prerun = "ml purge ; ml R-bundle-Bioconductor/3.15-foss-2021b-R-4.2.0",
                           CD = "/data/gent/vo/000/gvo00074/felicien/R/",
                           Rscript = Rscript.name)
  list_dir[[cname]] = cdir

}

dumb <- write_bash_submission(file = file.path(getwd(),"All.fits.XGB.ILF.sh"),
                              list_files = list_dir,
                              job_name = jobname)

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/submit.all.fits.coordlist_nodrought.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/
