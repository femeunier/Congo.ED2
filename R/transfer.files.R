transfer.files <- function(files,
                           base = "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/",
                           source = "outputs",
                           destination = "./outputs/"){

  for (file in files){
    system2("rsync",
            paste(
              file.path(base,source,file),
              destination
            ))
  }
}


