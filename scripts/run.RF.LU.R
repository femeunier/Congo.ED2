rm(list = ls())

library(ranger)
library(Metrics)
library(pdp)
library(dplyr)
library(randomForest)
library(ggplot2)
library(caret)
library(ranger)
library(edarf)
library(TrENDY.analyses)
library(tidyr)

location = "Congo"
cscenar = "ssp126"

if (location == "Congo"){
  e <- as(extent(-10, 52, -15, 10), 'SpatialPolygons')
} else {
  e <- as(extent(-82, -35, -20, 15), 'SpatialPolygons')
}

biovars <- aggregate(
  crop(brick("/home/femeunier/Documents/projects/CongoVOD/outputs/Biovars_2.5m_Pantropical.grd")[[1]],e),
  c(6,6),method = "mean")

data <- readRDS(file = paste0("./outputs/LU.vs.Climate_",location,".RDS"))

data.na <- data %>%
  filter(!is.na(lu),!is.na(bio1))

id2keep <- data %>%
  filter(!is.na(lu),!is.na(bio1)) %>%
  pull(id)


ind <- sample(2, nrow(data.na), replace = TRUE, prob = c(0.7, 0.3))
train <- data.na[ind==1,]
test <- data.na[ind==2,]


# rf <- randomForest(as.factor(lu) ~ .,
#                    # maxnodes = 5,
#                    # ntree = 200,
#                    data=train %>%
#                      dplyr::select(-id),
#                    proximity=TRUE,na.action = na.exclude)
# print(rf)
# plot(rf)


rf2 <- ranger(
  as.factor(lu) ~ .,
  data = train %>%
    dplyr::select(-c(id,lon,lat)),
  importance = "impurity",probability = TRUE,
  num.trees = 5000)


# reprtree:::plot.getTree(rf, k = 50)
###############################
# Train data

p1 <- predict(rf2, train)
confusionMatrix(as.factor(apply(p1$predictions,c(1),which.max)), as.factor(train$lu))

###############################
# Test data

p2 <- predict(rf2, test)
confusionMatrix(as.factor(apply(p2$predictions,c(1),which.max)), as.factor(test$lu))

# ###############################
#
# t <- tuneRF(train %>%
#               dplyr::select(-c(id,lon,lat,lu)),
#             as.factor(train %>% dplyr::select(lu) %>% pull(lu)),
#             stepFactor = 0.5,
#             plot = TRUE,
#             ntreeTry = 100,
#             trace = TRUE,
#             improve = 0.05,
#             doBest = TRUE)
#
# ###############################

# hist(treesize(rf),
#      main = "No. of Nodes for the Trees",
#      col = "green")
# varImpPlot(rf,
#            sort = T,
#            main = "Top 10 - Variable Importance")

imp <- ranger::importance(rf2)

df.imp <- as.data.frame(imp) %>%
  tibble::rownames_to_column("var") %>%
  arrange((imp)) %>%
  mutate(var = factor(var,
                      levels = var))

impvar <- df.imp$var

ggplot(data = df.imp) +
  geom_segment(aes(x = as.factor(var), xend = as.factor(var),
                   y = 0, yend = imp)) +
  geom_point(aes(x = as.factor(var),
                 y = imp)) +
  coord_flip() +
  labs(x = "") +
  theme_bw()


####################################################################################################################

# stop()
# Vars <- c("MCWD","bio1","bio12")
# Vars <- c("bio5","MCWD")
# pd_df <- partial_dependence(fit = rf2,
#                             vars = Vars,
#                             interaction = TRUE,
#                             data = data.na %>%
#                               dplyr::select(-c(id)),
#                             n = c(100, 200))
# pd_df.long <- pd_df %>%
#   pivot_longer(cols = c(Vars)) %>%
#   filter(!is.na(value)) %>%
#   pivot_longer(cols = -c("name","value"),
#                values_to = "y",
#                names_to = "x")
#
# ggplot(data = pd_df.long) +
#   geom_line(aes(x = value, y = y,
#                 color = as.factor(x))) +
#   facet_wrap(~ name, scales = "free") +
#   theme_bw()

# pdp.test <- pdp::partial(rf2, pred.var = c("bio12", "bio2"),
#                          approx = TRUE,
#                          chull = TRUE)
#
# plotmo(rf, pmethod="partdep",
#        degree1 = c("MCWD","bio12"),
#        degree2 = c("MCWD","bio12"))
# plot.pdp <- autoplot(pdp.test, contour = TRUE,
#                      legend.title = "Partial dependence")

####################################################################################################################
# New test data

climate.month.monthly.bio <- resample.df.all.col(bigdf = readRDS(paste0("./outputs/df.Bio_",cscenar,".RDS")) %>%
                                                   ungroup() %>%
                                                   filter(model == model[1]),
                                                 var.names = c(paste0("bio",1:19),"MCWD"),
                                                 raster2resample = biovars)

cmodel <- unique(climate.month.monthly.bio$model)

cdf.climate.month.monthly.bio <- climate.month.monthly.bio %>%
  filter(model == cmodel) %>%
  mutate(id = 1:n()) %>%
  filter(id %in% id2keep)

extra2keep <- cdf.climate.month.monthly.bio %>%
  filter(!is.na(bio1)) %>% pull(id)

p.test <- data.frame(present = data.na %>%
                       filter(id %in% extra2keep) %>%
                       pull(lu),
                     present.predicted = as.factor(apply(predict(rf2,
                                                                 data.na %>%
                                                                   filter(id %in% extra2keep))[["predictions"]],
                                                         c(1),
                                                         which.max)),
                     prob.present.predicted = apply(predict(rf2,
                                                            data.na %>%
                                                              filter(id %in% extra2keep))[["predictions"]],
                                                    c(1),
                                                    max),

                     future = as.factor(apply(predict(rf2,
                                                      data.na %>%
                                                        filter(id %in% extra2keep) %>%
                                                        mutate(bio7 = bio7+20))[["predictions"]],
                                              c(1),
                                              which.max)),

                     prob.future.predicted = apply(predict(rf2,
                                                           data.na %>%
                                                             filter(id %in% extra2keep) %>%
                                                             mutate(bio7 = bio7+20))[["predictions"]],
                                                    c(1),
                                                    max)



                     # future = as.factor(apply(predict(rf2,
                     #                                  cdf.climate.month.monthly.bio %>%
                     #                                    filter(!is.na(bio1)))[["predictions"]],
                     #                          c(1),
                     #                          which.max)),
                     #
                     # prob.future.predicted = apply(predict(rf2, cdf.climate.month.monthly.bio %>%
                     #                                         filter(!is.na(bio1)))[["predictions"]],
                     #                                c(1),
                     #                                max)
)

ggplot(data = p.test %>%
         dplyr::select(prob.present.predicted,prob.future.predicted) %>%
         pivot_longer(cols = -c())) +
  geom_density(aes(x = value, fill = name), alpha = 0.5) +
  theme_bw()

df.all <- bind_rows(data.na %>% dplyr::select(-lu) %>%
                      mutate(source = "data",
                             model = "bioclim",
                             scenar = "historical") %>%
                      pivot_longer(cols = c("MCWD",starts_with("bio"))) %>%
                      mutate(name = factor(name,
                                           levels = c("MCWD",paste0("bio",1:19)))),
                    cdf.climate.month.monthly.bio %>%
                      dplyr::select(-c(lat,lon)) %>%
                      ungroup() %>%
                      mutate(source = "ESM",
                             scenar = cscenar,
                             model = cmodel) %>%
                      pivot_longer(cols = c("MCWD",starts_with("bio"))) %>%
                      mutate(name = factor(name,
                                           levels = c("MCWD",paste0("bio",1:19)))))


ggplot(data = df.all) +
  geom_density(aes(x = value, fill = interaction(scenar,source)), color = NA, alpha = 0.5) +
  facet_wrap(~name, scales = "free") +
  theme_bw()




###################################

data.long <- data.na %>%
  pivot_longer(cols = c("MCWD",starts_with("bio")))

ggplot(data = data.long %>%
         mutate(lu = factor(lu,
                            levels = c(1,2,3))) %>%
         filter(name %in% impvar[seq(length(impvar),length(impvar)-4,-1)])) +
  geom_density(aes(x = value, fill = as.factor(lu)), color = NA, alpha = 0.5) +
  facet_wrap(~name, scales = "free") +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  theme_bw()


ggplot(data = data.na %>%
         # filter(lu %in% c(1,2) |
         #          MCWD >= -400) %>%
         mutate(lu = factor(lu,
                            levels = c(1,2,3))) %>%
         dplyr::select(c("MCWD","bio5","bio7","bio12","lu")),
       aes(x = MCWD, y = bio12, fill = as.factor(lu))) +
  stat_density_2d(geom = "polygon",
                  aes(alpha = after_stat(level)),
                  bins = 20)  +
  # geom_density_2d(aes(x = bio7, y = bio12,
  #                     fill = as.factor(lu)), color = NA, alpha = 0.5) +
  # facet_wrap(~name, scales = "free") +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  theme_bw()


ggplot(data = data.na %>%
         mutate(lu = factor(lu,
                            levels = c(1,2,3))),
       aes(x = MCWD, y = bio12, color= as.factor(lu), shape = as.factor(lu))) +
  geom_point(size = 0.1) +
  geom_point(data = data.na %>%
               filter(lu == 3) %>%
               mutate(lu = factor(lu,
                                  levels = c(1,2,3))),,
             size = 1)+
  # geom_density_2d(aes(x = bio7, y = bio12,
  #                     fill = as.factor(lu)), color = NA, alpha = 0.5) +
  # facet_wrap(~name, scales = "free") +
  scale_fill_manual(values = c("#c49402","#005401","#448704")) +
  theme_bw()

