write.script.fit.coordlist <- function(file = "script.R",
                                       model = "CABLE-POP",
                                       scenario = "S2",
                                       vars = c("gpp", "npp", "nep", "ra", "rh"),
                                       coord.list = "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Amazon.coord.ILF.RDS",
                                       xgb.model.prefix = "xgb.model",
                                       grid.suffix = "",
                                       frac.train = 0.6,
                                       overwrite = TRUE,
                                       transition.suffix = NULL,
                                       CC.suffix = "CC.pantropical.v11",
                                       climate.vars = c("tmp","tmin","tmax","spfh","VPD","pre","dswrf","dlwrf")){

  writeLines("rm(list = ls())",con = file)
  write("",file=file,append=TRUE)
  write("Congo.ED2::load.everything()",file=file,append=TRUE)
  write("",file=file,append=TRUE)

  write(paste0("model <- \"",model,"\""),
        file=file,append=TRUE)
  write(paste0("scenario <- \"",scenario,"\""),
        file=file,append=TRUE)
  write(paste0("vars <- c('",paste0(vars,collapse = "', '"),"')"),
        file=file,append=TRUE)
  write(paste0("coord.list <- \"",coord.list,"\""),
        file=file,append=TRUE)

  write(paste0("xgb.model.prefix <- \"",xgb.model.prefix,"\""),
        file=file,append=TRUE)
  write(paste0("grid.suffix <- \"",grid.suffix,"\""),
        file=file,append=TRUE)
  write(paste0("frac.train <- \"",frac.train,"\""),
        file=file,append=TRUE)
  write(paste0("overwrite <- \"",overwrite,"\""),
        file=file,append=TRUE)

  if (!is.null(transition.suffix)){
    write(paste0("transition.suffix <- \"",transition.suffix,"\""),
          file=file,append=TRUE)
  }

  write(paste0("climate.vars <- c('",paste0(climate.vars,collapse = "', '"),"')"),
        file=file,append=TRUE)

  write(paste0("CC.suffix <- \"",CC.suffix,"\""),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)

  write("fit.CC.vs.climate.coordlist(model,scenario,
        vars,coord.list,xgb.model.prefix,grid.suffix,
        frac.train,overwrite,transition.suffix,CC.suffix,climate.vars)",
        file=file,append=TRUE)
}



