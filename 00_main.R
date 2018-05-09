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

modeling.period <- 2006:2017

## Paliekam duomenis tik nuo 2006
data.money                <- data.money[substr(data.money$date, 1, 4) %in% modeling.period,]
data.tax                  <- data.tax[substr(data.tax$date, 1, 4) %in% modeling.period,]
data.cpi                  <- data.cpi[substr(data.cpi$date, 1, 4) %in% modeling.period,]
data.regulation           <- data.regulation[substr(data.regulation$date, 1, 4) %in% modeling.period,]
data.unemp                <- data.unemp[substr(data.unemp$date, 1, 4) %in% modeling.period,]
data.payments             <- data.payments[substr(data.payments$date, 1, 4) %in% modeling.period,]
data.alcohol.consumption  <- data.alcohol.consumption[substr(data.alcohol.consumption$date, 1, 4) %in% modeling.period,]
data.alcohol.price        <- data.alcohol.price[substr(data.alcohol.price$date, 1, 4) %in% modeling.period,]
data.bankrupts            <- data.bankrupts[substr(data.bankrupts$date, 1, 4) %in% modeling.period,]
data.cards                <- data.cards[substr(data.cards$date, 1, 4) %in% modeling.period,]
data.emigrants            <- data.emigrants[substr(data.emigrants$date, 1, 4) %in% modeling.period,]
data.loans.deposits       <- data.loans.deposits[substr(data.loans.deposits$date, 1, 4) %in% modeling.period,]
data.retail               <- data.retail[substr(data.retail$date, 1, 4) %in% modeling.period,]
data.wage                 <- data.wage[substr(data.wage$date, 1, 4) %in% modeling.period,]
data.travel               <- data.travel[substr(data.travel$date, 1, 4) %in% modeling.period,]


# STATIONARITY -----------------------------------------------------------------

source('01_stationarity.R')


# DATA DISAGGREGATION ----------------------------------------------------------

source('01_disaggregation.R')


# CORRELATION ------------------------------------------------------------------

# MODEL ------------------------------------------------------------------------

# PLOTS ------------------------------------------------------------------------

# OUTPUT -----------------------------------------------------------------------





