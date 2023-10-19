rm(list = ls())

library(dplyr)
library(tidyr)
library(purrr)
library(rhdf5)

select.sites <- readRDS("./outputs/select.sites.RDS")
land.sea.mask <- readRDS("./data/LandSeaMask.RDS")

source("/data/gent/vo/000/gvo00074/felicien/R/h5read_opt.r")

df.mask <- select.sites

ref_dir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/run/"
rundir <- "/user/scratchkyukon/gent/gvo000/gvo00074/felicien/CB/run/grid"
outdir <- "/kyukon/scratch/gent/vo/000/gvo00074/felicien/CB/out"

df <- data.frame()

phen_schemes <- c(-1,0,2)

for (iphen in seq(1,length(phen_schemes))){

  print("- ",iphen/length(phen_schemes))
  for (i in seq(1,nrow(df.mask))){

    print(paste0("- ",i/nrow(df.mask)))

    clat <- round(df.mask[["lat"]][i])
    clon <- round(df.mask[["lon"]][i])

    run_name <- paste0("CB_IPHEN_",phen_schemes[iphen],"_X_",abs(clon),ifelse(clon<0,"W","E"),"_Y_",abs(clat),ifelse(clat<0,"S","N"))

    run_ref <- file.path(rundir,run_name)
    out_ref <- file.path(outdir,run_name)

    details.file <- file.info(list.files(path = file.path(out_ref,"histo"), full.names = TRUE,pattern = ".h5"))

    if (nrow(details.file)>0){
      files.OP.ordered <- details.file[with(details.file, order(as.POSIXct(mtime),decreasing = TRUE)), ]
      h5file <- file.path(rownames(files.OP.ordered)[1])
      h5file.name <- basename(h5file)

      final.year <- as.numeric(stringr::str_split(h5file.name,"-")[[1]][3])

      for (year in sort(unique(c(seq(1700,final.year,25)),final.year))){

        h5file <- file.path(out_ref,"histo",paste0("history-S-",year,"-01-01-000000-g01.h5"))

        if (!file.exists(h5file)) next()

        mymont =  tryCatch({
          lapply(h5read_opt(h5file),FUN=aperm)
        }, error = function(e) {
          message("Hit error: ", e)
          return(NULL)
        })

        if (is.null(mymont)) next()


        names(mymont) <- gsub(x = names(mymont), pattern = "\\_", replacement = ".")

        AGB <- sum(mymont$AGB.PY)
        AGB.tree <- sum(mymont$AGB.PY[1,,c(2,3,4)])
        LAI <- sum(mymont$LAI.PY)
        LAI.tree <- sum(mymont$LAI.PY[1,,c(2,3,4)])

        df <- bind_rows(list(df,
                             data.frame(lat = df.mask[["lat"]][i],
                                        lon = df.mask[["lon"]][i],
                                        phen = phen_schemes[iphen],
                                        year,
                                        AGB,
                                        AGB.tree,
                                        LAI,
                                        LAI.tree)))
      }
    }
  }
}

saveRDS(object = df,
        file = file.path('.',"./outputs/df.selected.sites.CB.phn.RDS"))

# scp /home/femeunier/Documents/projects/Congo.ED2/scripts/analyze_selected_sites_CB_phen.R hpc:/data/gent/vo/000/gvo00074/felicien/R

