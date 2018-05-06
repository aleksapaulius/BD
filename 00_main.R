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


# CORRELATION ------------------------------------------------------------------

# DATA DISAGGREGATION ----------------------------------------------------------

alldata <- data.money

## mėnesiniai

### "Grynieji pinigai"
### "VKI"
### "VKI alkoholiui"               
### "Indėlių eurais palūkanų norma"
### "Indėlių litais palūkanų norma"
### "Indėliai"                     
### "Emigrantų skaičius"
### "Paskolų eurais palūkanų norma"
### "Paskolų litais palūkanų norma"
### "Paskolų eurais vertė"
### "Paskolų litais vertė"
### "Mažmeninės prekybos apimtis"
### "Moterų nedarbas"
### "Vyrų nedarbas"

cash.q <- ts(alldata$cash, start = c(2006, 1), frequency = 12)
deposits.q <- ts(alldata$deposits, start = c(2006, 1), frequency = 12)


## ketvirtiniai -> mėnesiniai

### "Grynųjų įnešimo operacijų skaičius"
cash_in_number.q <- ts(data.payments$cash_in_number, start = c(2006, 1), frequency = 4)
cash_in_number.m <- td(cash_in_number.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['cash_in_number'] <- predict(cash_in_number.m)

### "Grynųjų įnešimo operacijų vertė"
cash_in_value.q <- ts(data.payments$cash_in_value, start = c(2006, 1), frequency = 4)
cash_in_value.m <- td(cash_in_value.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['cash_in_value'] <- predict(cash_in_value.m)

### "Grynųjų išėmimo operacijų skaičius"
cash_out_number.q <- ts(data.payments$cash_out_number, start = c(2006, 1), frequency = 4)
cash_out_number.m <- td(cash_out_number.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['cash_out_number'] <- predict(cash_out_number.m)

### "Grynųjų išėmimo operacijų vertė"
cash_out_value.q <- ts(data.payments$cash_out_value, start = c(2006, 1), frequency = 4)
cash_out_value.m <- td(cash_out_value.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['cash_out_value'] <- predict(cash_out_value.m)

### "Kredito kortelių skaičius"
credit_cards.q <- ts(data.cards$credit_cards, start = c(2006, 1), frequency = 4)
credit_cards.m <- td(credit_cards.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['credit_cards'] <- predict(credit_cards.m)

### "Debeto kortelių skaičius"
debit_cards.q <- ts(data.cards$debit_cards, start = c(2006, 1), frequency = 4)
debit_cards.m <- td(debit_cards.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['debit_cards'] <- predict(debit_cards.m)

### "Gautų mokėjimų skaičius"
payments_in_number.q <- ts(data.payments$payments_in_number, start = c(2006, 1), frequency = 4)
payments_in_number.m <- td(payments_in_number.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['payments_in_number'] <- predict(payments_in_number.m)

### "Gautų mokėjimų vertė"
payments_in_value.q <- ts(data.payments$payments_in_value, start = c(2006, 1), frequency = 4)
payments_in_value.m <- td(payments_in_value.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['payments_in_value'] <- predict(payments_in_value.m)

### "Išeinančių mokėjimų skaičius"
payments_out_number.q <- ts(data.payments$payments_out_number, start = c(2006, 1), frequency = 4)
payments_out_number.m <- td(payments_out_number.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['payments_out_number'] <- predict(payments_out_number.m)

### "Išeinančių mokėjimų vertė"
payments_out_value.q <- ts(data.payments$payments_out_value, start = c(2006, 1), frequency = 4)
payments_out_value.m <- td(payments_out_value.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['payments_out_value'] <- predict(payments_out_value.m)


## metiniai -> mėnesiniai

### "Alkoholio suvartojimas"
alcohol_consumption.q <- ts(data.alcohol.consumption$alcohol_consumption, start = c(2006, 1), frequency = 1)
alcohol_consumption.m <- td(alcohol_consumption.q ~ cash.q + deposits.q, to = "monthly", method = "litterman-maxlog")
alldata['alcohol_consumption'] <- predict(alcohol_consumption.m)


### "Alkoholio kaina"                             
### "Bankrotų skaičius"                            
### "Išvykstančių turistų skaičius"               
### "Ekonominis reguliavimas: darbuotojų skaičius" 
### "Ekonominis reguliavimas: įstaigų skaičius"   
### "Akcizo mokesčiai"                             
### "GPM"                                         
### "Pelno mokesčiai"                              
### "Pridėtinės vertės mokesčiai"                 
### "Kelionių agentūrų skaičius"

## pusmetiniai -> mėnesiniai

### "Minimalus atlyginimas"

# MODEL ------------------------------------------------------------------------

# PLOTS ------------------------------------------------------------------------

# OUTPUT -----------------------------------------------------------------------





