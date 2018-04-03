### 

# Loading Packages -------------------------------------------------------------

library("Quandl")
library("readxl")
library("dplyr")
library("reshape2")
library("tempdisagg")
library('zoo')

source('00_functions.R')


# INPUT ------------------------------------------------------------------------

options(stringsAsFactors = F, check.names = F)

# pinigu statistika (grynieji pinigai, indeliai)
data.money <- read_excel("input/pinigu_statistika.xlsx")

# tax
data.tax <- read.csv2(file = "input/tax.csv", sep = ",")

# cpi
data.cpi <- read.csv2(file = "input/cpi.csv", sep = ",")

# economic regulation
data.regulation1 <- read.csv2(file = "input/vt_istaigu_vadovai.csv", sep = ",")
data.regulation2 <- read.csv2(file = "input/instituciju_skaicius.csv", sep = ",")

# unemployment
data.unemp <- read.csv2(file = "input/unemp.csv", sep = ",")

# electronic payments
data.payments <- read.csv2(file = "input/payments.csv", sep = ";")

# alcohol consumption
data.alcohol.consumption <- read.csv2(file = "input/alcohol_consumption.csv", sep = ",")

# alcohol price
data.alcohol.price <- read.csv2(file = "input/alcohol_price.csv", sep = ",")

# bankrupts
data.bankrupts <- read.csv2(file = "input/bankruptcy.csv", sep = ",")

# cards
data.cards <- read.csv2(file = "input/cards.csv", sep = ";")

# emigrants
data.emigrants <- read.csv2(file = "input/emigrants.csv", sep = ",")

# loans and deposits
data.loans.deposits2015 <- read.csv2(file = "input/loans_deposits_2015_2017.csv", sep = ";")
data.deposits1999 <- read.csv2(file = "input/deposits_1999_2004.csv", sep = ",")
data.deposits2004 <- read.csv2(file = "input/deposits_2004_2014.csv", sep = ",")
data.loans1999 <- read.csv2(file = "input/loans_1999_2004.csv", sep = ",")
data.loans2004 <- read.csv2(file = "input/loans_2004_2014.csv", sep = ",")

# retail
data.retail <- read.csv2(file = "input/mazmenine_prekyba.csv", sep = ",")

# minimum wage
data.wage <- read.csv2(file = "input/minimum_wage.csv", sep = ",")

# tourists
data.tourists <- read.csv2(file = "input/tourists.csv", sep = ",")

# travel_agencies
data.travel.agencies <- read.csv2(file = "input/travel_agencies.csv", sep = ",")


# USER SELECTIONS --------------------------------------------------------------

all.data.list <- list(
  data.alcohol.consumption,
  data.alcohol.price,
  data.bankrupts,
  data.cards,
  data.cpi,
  data.payments,
  data.emigrants,
  data.loans.deposits2015,
  data.deposits1999,
  data.deposits2004,
  data.loans1999,
  data.loans2004,
  data.money,
  data.regulation,
  data.retail,
  data.tax,
  data.tourists,
  data.travel.agencies,
  data.unemp,
  data.wage
)


# DATA MANIPULATION ------------------------------------------------------------

### cleaning imported data
source('01_data_cleaning.R')

### checking date scales
all.variables.desc <- frequency.desc(all.data.list)
table(all.variables.desc$frequency_name)

# unify date format

today <- Sys.Date()
format(today, format="%Y %m")

format(all.variables.desc$min_date[11], format="%Y %m")


# MODEL ------------------------------------------------------------------------

# PLOTS ------------------------------------------------------------------------

# OUTPUT -----------------------------------------------------------------------












