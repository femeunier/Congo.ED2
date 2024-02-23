rm(list = ls())

library(ncdf4)
library(dplyr)
library(lubridate)
library(reshape2)
library(raster)
library(TrENDY.analyses)
library(Congo.ED2)
library(ggplot2)
library(tidyr)
library(pals)

palette <- c(kelly(n=17)[2:17])

################################################################################

GPPproducts2keep <- c("VOD","SIF","SIF2","NIR")

RS.data <- bind_rows(readRDS("./outputs/GPP.products.Amazon.ILF.sum.RDS")) %>%
  filter(model %in% GPPproducts2keep) %>%
  rename(product = model) %>%
  filter(year %in% 2001:2019)
products <- sort(unique(RS.data$product))

RS.data.sum <- RS.data %>%
  group_by(year,month) %>%
  summarise(value.m = mean(value.m,
                           na.rm = TRUE),
            .groups = "keep")

RS.monthly <- RS.data %>%
  group_by(product,month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")
RS.monthly.MEM <- RS.data %>%
  group_by(product,month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")

RS.yearly <- RS.data %>%
  group_by(product,year) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")
RS.yearly.MEM <- RS.data %>%
  group_by(product,year) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(year) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_line(data = RS.monthly,
            aes(x = month,
                y = value.m,
                color = product), linewidth = 0.25) +
  geom_line(data = RS.monthly.MEM,
            aes(x = month,
                y = value.m), color = "black",linetype = 1, linewidth = 0.5) +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(2,5)) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))


ggplot() +
  geom_line(data = RS.yearly,
            aes(x = year,
                y = value.m,
                color = product),
            size = 0.25) +
  geom_line(data = RS.yearly.MEM,
            aes(x = year,
                y = value.m), color = "black",linetype = 1, linewidth = 0.5) +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(2,5)) +
  scale_x_continuous(limits = c(2000,2020),
                     breaks = seq(2000,2020,5)) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))


################################################################################

Trendy.data <- bind_rows(readRDS("./outputs/Amazon.mean.JRA.historical.IFL.sum.RDS")) %>%
  filter(var == "gpp") %>%
  filter(year %in% 2001:2019)

# MEM
Trendy.data.sum <- Trendy.data %>%
  group_by(year,month) %>%
  summarise(value.MEM = mean(pred),
            .groups = "keep") %>%
  ungroup()

models <- sort(unique(Trendy.data$model))


Trendy.monthly <- Trendy.data %>%
  group_by(model,month) %>%
  summarise(value.m = mean(obs,na.rm = TRUE),
            .groups = "keep")
Trendy.monthly.MEM <- Trendy.data %>%
  group_by(model,month) %>%
  summarise(value.m = mean(obs,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(month) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")

Trendy.yearly <- Trendy.data %>%
  group_by(model,year) %>%
  summarise(value.m = mean(obs,na.rm = TRUE),
            .groups = "keep")
Trendy.yearly.MEM <- Trendy.data %>%
  group_by(model,year) %>%
  summarise(value.m = mean(obs,na.rm = TRUE),
            .groups = "keep") %>%
  group_by(year) %>%
  summarise(value.m = mean(value.m,na.rm = TRUE),
            .groups = "keep")


ggplot() +
  geom_line(data = Trendy.monthly,
            aes(x = month,
                y = value.m,
                color = model), linewidth = 0.25) +
  geom_line(data = Trendy.monthly.MEM,
            aes(x = month,
                y = value.m), color = "black",linetype = 1, linewidth = 0.5) +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(2,5)) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J",
                                "J","A","S","O","N","D")) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))

ggplot() +
  geom_line(data = Trendy.yearly,
            aes(x = year,
                y = value.m,
                color = model),
            size = 0.25) +
  geom_line(data = Trendy.yearly.MEM,
            aes(x = year,
                y = value.m), color = "black",linetype = 1, linewidth = 0.5) +
  scale_color_manual(values = palette) +
  scale_y_continuous(limits = c(2,5)) +
  scale_x_continuous(limits = c(2000,2020),
                     breaks = seq(2000,2020,5)) +
  labs(x = "",y = "") +
  guides(color = "none") +
  theme_bw() +
  theme(text = element_text(size = 20))


