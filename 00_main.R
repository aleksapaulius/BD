### 

# Loading Packages -------------------------------------------------------------

library('Quandl')
library('readxl')
library('dplyr')
library('reshape2')
library('tempdisagg')
library('zoo')
library('ggplot2')
library('forcats')

source('00_functions.R')


# INPUT ------------------------------------------------------------------------

options(stringsAsFactors = F, check.names = F)

# pinigu statistika (grynieji pinigai, indeliai)
data.money <- read.csv2(file = "input/money.csv", sep = ",", dec = '.')

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
data.loans.deposits2015 <- read.csv2(file = "input/loans_deposits_2015_2017.csv", sep = ";", dec = '.')
data.deposits1999 <- read.csv2(file = "input/deposits_1999_2004.csv", sep = ",", dec = '.')
data.deposits2005 <- read.csv2(file = "input/deposits_2005_2014.csv", sep = ",", dec = '.')
data.loans1999 <- read.csv2(file = "input/loans_1999_2004.csv", sep = ",", dec = '.')
data.loans2005 <- read.csv2(file = "input/loans_2005_2014.csv", sep = ",", dec = '.')

# retail
data.retail <- read.csv2(file = "input/mazmenine_prekyba.csv", sep = ",")

# minimum wage
data.wage <- read.csv2(file = "input/minimum_wage.csv", sep = ",")

# tourists
data.tourists <- read.csv2(file = "input/tourists.csv", sep = ",")

# travel_agencies
data.travel.agencies <- read.csv2(file = "input/travel_agencies.csv", sep = ",")


# USER SELECTIONS --------------------------------------------------------------


# DATA MANIPULATION ------------------------------------------------------------

### cleaning imported data
source('01_data_cleaning.R')

# combine all data tables into a list
all.data.list <- list(
  data.money,
  data.tax,
  data.cpi,
  data.regulation,
  data.unemp,
  data.payments,
  data.alcohol.consumption,
  data.alcohol.price,
  data.bankrupts,
  data.cards,
  data.emigrants,
  data.loans.deposits,
  data.retail,
  data.wage,
  data.travel
)

# checking date scales
all.variables.desc <- frequency.desc(all.data.list)
table(all.variables.desc$frequency_name)

# kokius duomenis turim?
all.variables.dates <- all.variables.desc[,c("variable", "min_date", "max_date", "frequency_name")]
all.variables.dates$min_date <- substr(all.variables.dates$min_date, 1, 4)
all.variables.dates$max_date <- substr(all.variables.dates$max_date, 1, 4)
all.variables.dates <- melt(all.variables.dates, id=c("variable","frequency_name"))

pdf(file="plots/Turimi duomenys.pdf",width = 14, height = 8)
ggplot(all.variables.dates, aes(x = value, y = variable, group = variable)) +
  geom_point(aes(colour = frequency_name)) + geom_line(aes(colour = frequency_name)) +
  ggtitle("Turimi duomenys") +
  labs(x = 'Metai', y = 'Duomuo', color = "")
dev.off()


# DESCRIPTIVE STATISTICS



# DATA DISAGGREGATION

# quarterly -> monthly
"payments_out_number"
"payments_out_value"
"payments_in_number"
"payments_in_value"
"cash_out_number"    
"cash_out_value"
"cash_in_number"
"cash_in_value"
"credit_cards"
"debit_cards"




# semiannual -> monthly

# yearly -> monthly




# MODEL ------------------------------------------------------------------------

# PLOTS ------------------------------------------------------------------------

# OUTPUT -----------------------------------------------------------------------












