rm(list = ls())

library(rhdf5)
library(dplyr)
library(tidyr)

outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/Yoko/histo/"
sim.names <- c("Yoko_default","Yoko_lu")

df <- data.frame()

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

for (i in seq(1,length(sim.names))){

  details.file <- file.info(list.files(path = paste0(outdir), full.names = TRUE,
                                       pattern = glob2rx(paste0(sim.names[i],"*.h5"))))


  for (year in seq(1550,2014,5)){
    for (month in seq(1,1)){

      h5file <- file.path(outdir,paste0(sim.names[i],"-S-",year,"-",sprintf("%02d",month),"-01-000000-g01.h5"))

      if (!file.exists(h5file)) next()

      mymont =  tryCatch({
        lapply(h5read_opt(h5file),FUN=aperm)
      }, error = function(e) {
        message("Hit error: ", e, h5file)
        return(NULL)
      })

      if (is.null(mymont)) next()

      names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

      AGB <- sum(mymont$AGB.PY)
      AGB.tree <- sum(mymont$AGB.PY[1,,c(2,3,4)])
      LAI <- sum(mymont$LAI.PY)
      LAI.tree <- sum(mymont$LAI.PY[1,,c(2,3,4)])

      df <- bind_rows(list(df,
                           data.frame(sim = sim.names[i],
                                      year,month,
                                      AGB,
                                      AGB.tree,
                                      LAI,
                                      LAI.tree)))
    }
  }
}

saveRDS(object = df,
        file = file.path('.',"./outputs/compare.lu.RDS"))

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/analyze_LU_Yoko.R hpc:/data/gent/vo/000/gvo00074/felicien/R
