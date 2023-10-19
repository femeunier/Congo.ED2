rm(list = ls())

library(dplyr)
library(tidyr)
library(TrENDY.analyses)

system2("rsync",
        paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy*",
              "./outputs/"))

model.names <- get.model.names.TRENDY(version = "v11")

# model.names <- c("ORCHIDEEv3")

scenario <- c("S2")
# variables <- c("cVeg","cRoot")
variables <- c("npp")

df.model <- data.frame()
compt.model <- 0
for (cmodel in model.names){

  print(cmodel)
    for (cvariable in variables){

      op.file <- paste0("./outputs/Trendy",cmodel,".",scenario,".",cvariable,"_centralAfrica_v11.RDS")

      if (!file.exists(op.file)) {
        warning(paste0("could not find file: ",op.file))
        next()
      }

      cdf <- readRDS(op.file)

      # if (cmodel == "ISBA-CTRIP"){stop()}

      df.model <- bind_rows(list(df.model,
                                 cdf %>% mutate(model = cmodel,
                                                scenario,
                                                variable = cvariable)
                                 ))

      compt.model <- compt.model + 1

  }
}

# df.model.wide <- df.model %>% pivot_wider(names_from = variable,
#                                           values_from = value) %>%
#   mutate(cAGB = case_when(!is.na(cRoot) ~ cVeg - cRoot,
#                           TRUE ~ cVeg))

saveRDS(df.model,"./outputs/Trendy.npp.centralAfrica.v11.RDS")
