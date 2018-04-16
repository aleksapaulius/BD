### 

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


source('00_functions.R')


# INPUT ------------------------------------------------------------------------

options(stringsAsFactors = F, check.names = F)

## pinigu statistika (grynieji pinigai, indeliai)
data.money <- read.csv2(file = "input/money.csv", sep = ",", dec = '.')
data.money2015 <- read.csv2(file = "input/money_2015.csv", sep = ";", dec = '.')

## tax
data.tax <- read.csv2(file = "input/tax.csv", sep = ",")

## cpi
data.cpi <- read.csv2(file = "input/cpi.csv", sep = ",")

## economic regulation
data.regulation1 <- read.csv2(file = "input/vt_istaigu_vadovai.csv", sep = ",")
data.regulation2 <- read.csv2(file = "input/instituciju_skaicius.csv", sep = ",")

## unemployment
data.unemp <- read.csv2(file = "input/unemp.csv", sep = ",")

## electronic payments
data.payments <- read.csv2(file = "input/payments.csv", sep = ";")

## alcohol consumption
data.alcohol.consumption <- read.csv2(file = "input/alcohol_consumption.csv", sep = ",")

## alcohol price
data.alcohol.price <- read.csv2(file = "input/alcohol_price.csv", sep = ",")

## bankrupts
data.bankrupts <- read.csv2(file = "input/bankruptcy.csv", sep = ",")

## cards
data.cards <- read.csv2(file = "input/cards.csv", sep = ";")

## emigrants
data.emigrants <- read.csv2(file = "input/emigrants.csv", sep = ",")

## loans and deposits
data.loans.deposits2015 <- read.csv2(file = "input/loans_deposits_2015_2017.csv", sep = ";", dec = '.')
data.deposits1999 <- read.csv2(file = "input/deposits_1999_2004.csv", sep = ",", dec = '.')
data.deposits2005 <- read.csv2(file = "input/deposits_2005_2014.csv", sep = ",", dec = '.')
data.loans1999 <- read.csv2(file = "input/loans_1999_2004.csv", sep = ",", dec = '.')
data.loans2005 <- read.csv2(file = "input/loans_2005_2014.csv", sep = ",", dec = '.')

## retail
data.retail <- read.csv2(file = "input/mazmenine_prekyba.csv", sep = ",")

## minimum wage
data.wage <- read.csv2(file = "input/minimum_wage.csv", sep = ",")

## tourists
data.tourists <- read.csv2(file = "input/tourists.csv", sep = ",")

## travel_agencies
data.travel.agencies <- read.csv2(file = "input/travel_agencies.csv", sep = ",")


# USER SELECTIONS --------------------------------------------------------------

modeling.period <- 2006:2018


# DESCRIPTIVE STATISTICS -------------------------------------------------------

## cleaning imported data
source('01_data_cleaning.R')

## combine all data tables into a list
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

## checking date scales
all.variables.stat <- statistics(all.data.list)
table(all.variables.stat$frequency_name)

## kokio laikotarpio duomenis turim?
all.variables.dates <- all.variables.stat[,c("variable", "min_date", "max_date", "frequency_name")]
all.variables.dates$min_date <- substr(all.variables.dates$min_date, 1, 4)
all.variables.dates$max_date <- substr(all.variables.dates$max_date, 1, 4)
all.variables.dates <- melt(all.variables.dates, id=c("variable","frequency_name"))

pdf(file="plots/Turimi duomenys.pdf",width = 14, height = 8)
ggplot(all.variables.dates, aes(x = value, y = variable, group = variable)) +
  geom_point(aes(colour = frequency_name)) + geom_line(aes(colour = frequency_name)) +
  ggtitle("Turimi duomenys") +
  labs(x = 'Metai', y = 'Duomuo', color = "")
dev.off()


## Duomenu aprasomoji statistika

## pinigu statistika (grynieji pinigai, indeliai)
autoplot(ts(data.money$cash, start = c(1993, 12), frequency = 12)) + 
  labs(title="Grynieji pinigai esantys apyvartoje", 
       subtitle="Likučiai laikotarpio pabaigoje", 
       caption="Šaltinis: Lietuvos bankas", 
       y="mln. Eur") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_gray()
all.variables.stat[all.variables.stat$variable == 'cash',]

autoplot(ts(data.money$deposits, start = c(1993, 12), frequency = 12)) + 
  labs(title="Indėliai", 
       subtitle="Likučiai laikotarpio pabaigoje", 
       caption="Šaltinis: Lietuvos bankas", 
       y="mln. Eur") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_gray()
all.variables.stat[all.variables.stat$variable == 'deposits',]

## tax
autoplot(ts(data.tax$tax_gpm, start = 2007, frequency = 1)) + 
  labs(title="Grynieji pinigai esantys apyvartoje", 
       subtitle="Likučiai laikotarpio pabaigoje", 
       caption="Šaltinis: Lietuvos bankas", 
       y="mln. Eur") +
  theme(plot.title = element_text(hjust=0.5)) +
  theme_gray()
all.variables.stat[all.variables.stat$variable == 'cash',]

## cpi

## economic regulation

## unemployment

## electronic payments

## alcohol consumption

## alcohol price

## bankrupts

## cards

## emigrants

## loans and deposits

## retail

## minimum wage

## tourists

## travel_agencies












# DATA MANIPULATION ------------------------------------------------------------

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


# DATA DISAGGREGATION ----------------------------------------------------------

# quarterly -> monthly
# "payments_out_number"
# "payments_out_value"
# "payments_in_number"
# "payments_in_value"
# "cash_out_number"    
# "cash_out_value"
# "cash_in_number"
# "cash_in_value"
# "credit_cards"
# "debit_cards"

## no indicator (Denton-Cholette) 
payments_in_number.q <- ts(data.payments$payments_in_number, start = c(2006, 1), frequency = 4)


m1 <- td(payments_in_number.q ~ 1, to = "monthly", method = "denton-cholette")
m2 <- td(payments_in_number.q ~ 1, to = "monthly", method = "chow-lin-maxlog")


plot(predict(m1))
lines(predict(m2), col = 'red')  



## semiannual -> monthly

## yearly -> monthly




# MODEL ------------------------------------------------------------------------

# PLOTS ------------------------------------------------------------------------

# OUTPUT -----------------------------------------------------------------------












