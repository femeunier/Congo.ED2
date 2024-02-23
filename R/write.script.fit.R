write.script.fit <- function(file = "script.R",
                             model = "CABLE-POP",
                             scenario = "S2",
                             vars = c("gpp", "npp", "nep", "ra", "rh"),
                             biome.names = c("Tropical seasonal forest/savanna"),
                             continents = c("Africa"),
                             xgb.model.prefix = "xgb.model",
                             grid.suffix = "",
                             frac.train = 0.6,
                             biome.file = "/data/gent/vo/000/gvo00074/felicien/R/outputs/biome.CRUJRA.1901.2019.RDS",
                             overwrite = TRUE,
                             transition.suffix = "transitions",
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
  write(paste0("biome.names <- \"",biome.names,"\""),
        file=file,append=TRUE)
  write(paste0("continents <- \"",continents,"\""),
        file=file,append=TRUE)
  write(paste0("xgb.model.prefix <- \"",xgb.model.prefix,"\""),
        file=file,append=TRUE)
  write(paste0("grid.suffix <- \"",grid.suffix,"\""),
        file=file,append=TRUE)
  write(paste0("frac.train <- \"",frac.train,"\""),
        file=file,append=TRUE)
  write(paste0("biome.file <- \"",biome.file,"\""),
        file=file,append=TRUE)
  write(paste0("overwrite <- \"",overwrite,"\""),
        file=file,append=TRUE)
  write(paste0("transition.suffix <- \"",transition.suffix,"\""),
        file=file,append=TRUE)

  write(paste0("climate.vars <- c('",paste0(climate.vars,collapse = "', '"),"')"),
        file=file,append=TRUE)

  write(paste0("CC.suffix <- \"",CC.suffix,"\""),
        file=file,append=TRUE)

  write("",file=file,append=TRUE)

  write("fit.CC.vs.climate(model,scenario,
        vars,biome.names,continents,xgb.model.prefix,grid.suffix,
        frac.train,biome.file,overwrite,transition.suffix,CC.suffix,climate.vars)",
        file=file,append=TRUE)
}



