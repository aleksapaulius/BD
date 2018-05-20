### MAIN

# Loading Packages -------------------------------------------------------------

library('readxl')
library('dplyr')
library('reshape2')
library('tempdisagg')
library('zoo')
library('ggplot2')
library('forcats')
library('magrittr')
library('ggfortify')
library('forecast')
library('stargazer')
library('Cairo')
library('lubridate')
library('urca')

source('00_functions.R')


# INPUT ------------------------------------------------------------------------

## importing data
source('00_input.R')

## cleaning imported data
source('01_data_cleaning.R')


# DESCRIPTIVE STATISTICS -------------------------------------------------------

## Tikrinam, kokio dažnumo duomenis turim
table(all.variables.stat$stebėjimų_dažnumas)

## kokio laikotarpio duomenis turim?
all.variables.dates <- all.variables.stat[,c("kintamasis", "stebėjimo_pradžia", "stebėjimo_pabaiga", "stebėjimų_dažnumas")]
all.variables.dates$stebėjimo_pradžia <- substr(all.variables.dates$stebėjimo_pradžia, 1, 4)
all.variables.dates$stebėjimo_pabaiga <- substr(all.variables.dates$stebėjimo_pabaiga, 1, 4)
all.variables.dates <- melt(all.variables.dates, id=c("kintamasis","stebėjimų_dažnumas"))

cairo_pdf(filename = "plots/Turimi duomenys.pdf",width = 14, height = 8)
ggplot(all.variables.dates, aes(x = value, y = kintamasis, group = kintamasis)) +
  geom_point(aes(colour = stebėjimų_dažnumas)) + geom_line(aes(colour = stebėjimų_dažnumas)) +
  ggtitle("Turimi duomenys") +
  labs(x = 'Metai', y = 'Duomuo', color = "")
dev.off()


# PLOTS ------------------------------------------------------------------------

# source('01_plots.R')


# DATA MANIPULATION ------------------------------------------------------------

source('01_data_manipulation.R')


# DATA DISAGGREGATION ----------------------------------------------------------

source('01_disaggregation.R')


# STATIONARITY -----------------------------------------------------------------

source('01_stationarity.R')


# CORRELATION ------------------------------------------------------------------

source('01_correlation.R')


# MODEL ------------------------------------------------------------------------

# paruosiam duomenis modeliavimui
alldata.model <- alldata.m
alldata.model[1,diff1.variables] <- NA
alldata.model[2:nrow(alldata.model), diff1.variables] <- lapply(alldata.m[diff1.variables], diff)
alldata.model['ratio'] <- alldata.model$cash/alldata.model$deposits
LTL.dates <- c(apply(expand.grid(2006:2014, 1:9),1,paste,collapse=" 0"), apply(expand.grid(2006:2014, 10:12),1,paste,collapse=" "), '2015 01')
EUR.dates <- c(apply(expand.grid(2015:2017, 1:9),1,paste,collapse=" 0"), apply(expand.grid(2015:2017, 10:12),1,paste,collapse=" ")) %>% setdiff('2015 01')

alldata.model[alldata.model$date %in% LTL.dates, 'dummy_LTL'] <- 1
alldata.model[alldata.model$date %in% EUR.dates, 'dummy_LTL'] <- 0
alldata.model[alldata.model$date %in% LTL.dates, 'dummy_EUR'] <- 0
alldata.model[alldata.model$date %in% EUR.dates, 'dummy_EUR'] <- 1

# creating formula
lm.regressors <- setdiff(names(alldata.model), c('date', 'cash', 'deposits', 'ratio'))
f <- as.formula(paste("ratio ~ ", paste(lm.regressors, collapse=" + ")))

# linear model
fit <- lm(f, data = alldata.model)

# fit <- lm(log(turnover) ~ day.type + month + factor(holiday, exclude=c('12','13')), data = mdata[mdata$date %in% train.range,])













