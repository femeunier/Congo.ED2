rm(list = ls())

all.files <- list.files("~/Documents/data/YGB/",pattern = "*.csv",
                        full.names = TRUE)

all <- data.frame()


for (ifile in seq(1,length(all.files))){
  cfile <- all.files[ifile]
  if (!file.exists(cfile)){
    next()
  }

  cdata <- read.csv(cfile,header = TRUE,skip = 3)
  all <- bind_rows(all,
                   cdata[,c(1,3,6)] %>%
                     rename(timestamp = Smp,
                            temp = Smp.2,
                            precip = Tot) %>%
                     mutate(timestamp = sub("\\.","",as.character(timestamp))) %>%
                     mutate(file = cfile))

}

all <- all %>%
  mutate(year = as.numeric(substr(timestamp,1,4)),
         month = as.numeric(substr(timestamp,5,6)),
         day = as.numeric(substr(timestamp,7,8)))

all.day <- all %>%
  group_by(year,month,day) %>%
  summarise(temp = mean(temp,na.rm = TRUE),
            precip = sum(precip,na.rm = TRUE),
            .groups = "keep")

all.month <- all.day %>%
  group_by(year,month) %>%
  summarise(Ndays = length(precip),
            temp = mean(temp,na.rm = TRUE),
            precip = sum(precip,na.rm = TRUE),
            .groups = "keep")

all.month.long <- all.month %>%
  pivot_longer(cols = c(temp,precip),
               names_to = "var")

ggplot(data = all.month.long) +
  geom_line(aes(x = year + (month - 1/2)/12,
                y = value)) +
  facet_wrap(~ var, scales = "free") +
  labs(x = "") +
  theme_bw()
