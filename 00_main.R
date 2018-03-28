
# Loading Packages -------------------------------------------------------------

library("Quandl")
library("readxl")
library("dplyr")
library("reshape2")

# INPUT ------------------------------------------------------------------------

options(stringsAsFactors = F)

# pinigu statistika (grynieji pinigai, indeliai)
data.pinigu.statistika <- read_excel("input/pinigu_statistika.xlsx")

# tax
data.tax <- read.csv2(file = "input/tax.csv", sep = ",", check.names = F)

# cpi
data.cpi <- read.csv2(file = "input/cpi.csv", sep = ",", check.names = F)

# economic regulation
data.regulation1 <- read.csv2(file = "input/vt_istaigu_vadovai.csv", sep = ",", check.names = F)
data.regulation2 <- read.csv2(file = "input/instituciju_skaicius.csv", sep = ",", check.names = F)

# unemployment
data.unemp <- read.csv2(file = "input/unemp.csv", sep = ",", check.names = F)

# interest rate | euribor 3M rates: https://www.quandl.com/data/BOF/QS_D_IEUTIO3M-EURIBOR-3-Months-Daily
data.euribor <- Quandl("BOF/QS_D_IEUTIO3M")

# electronic payments
data.payments <- read.csv2(file = "input/payments.csv", sep = ";", check.names = F)

# alcohol consumption
data.alcohol.consumption <- read.csv2(file = "input/alcohol_consumption.csv", sep = ",", check.names = F)

# alcohol price
data.alcohol.price <- read.csv2(file = "input/alcohol_price.csv", sep = ",", check.names = F)

# bankrupts
data.bankrupts <- read.csv2(file = "input/bankruptcy.csv", sep = ",", check.names = F)

# cards
data.cards <- read.csv2(file = "input/cards.csv", sep = ";", check.names = F)

# emigrants
data.emigrants <- read.csv2(file = "input/emigrants.csv", sep = ",", check.names = F)

# loan interest
data.loan.interest <- read.csv2(file = "input/interest.csv", sep = ";", check.names = F)

# retail
data.retail <- read.csv2(file = "input/mazmenine_prekyba.csv", sep = ",", check.names = F)

# minimum wage
data.wage <- read.csv2(file = "input/minimum_wage.csv", sep = ",", check.names = F)

# tourists
data.tourists <- read.csv2(file = "input/tourists.csv", sep = ",", check.names = F)

# travel_agencies
data.travel.agencies <- read.csv2(file = "input/travel_agencies.csv", sep = ";", check.names = F)


# USER SELECTIONS --------------------------------------------------------------

# DATA MANIPULATION ------------------------------------------------------------

# cleaning imported data
names(data.alcohol.consumption)[names(data.alcohol.consumption) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'alcohol_consumption')
data.alcohol.consumption <- arrange(data.alcohol.consumption[,names(data.alcohol.consumption) %in% c('date', 'alcohol_consumption')], date)

names(data.alcohol.price)[names(data.alcohol.price) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'alcohol_price')
data.alcohol.price <- arrange(data.alcohol.price[,names(data.alcohol.price) %in% c('date', 'alcohol_price')], date)

names(data.bankrupts)[names(data.bankrupts) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'bankrupts')
data.bankrupts <- arrange(data.bankrupts[,names(data.bankrupts) %in% c('date', 'bankrupts')], date)

for (i in 1:nrow(data.cards)) {data.cards[i,'variable1'] <- strsplit(data.cards$lt_long_title[i], '; ')[[1]][3]}
for (i in 1:nrow(data.cards)) {data.cards[i,'variable2'] <- strsplit(data.cards$lt_long_title[i], '; ')[[1]][4]}
data.cards <- data.cards[data.cards$variable1 == 'Kortelių skaičius',]
dc <- "Šalyje įsteigtų mokėjimo paslaugų teikėjų išleistos debeto kortelės"
cc <- "Šalyje įsteigtų mokėjimo paslaugų teikėjų išleistos kredito kortelės  (įskaitant virtualiąsias korteles)"
data.cards <- data.cards[data.cards$variable2 %in% c(cc, dc),]
data.cards <- arrange(data.cards[,names(data.cards) %in% c('date', 'value', 'variable2')], date)
data.cards[data.cards$variable2 == dc,'variable2'] <- 'debit_cards'
data.cards[data.cards$variable2 == cc,'variable2'] <- 'credit_cards'
data.cards <- dcast(data.cards, date ~ variable2)

names(data.cpi)[names(data.cpi) %in% c('Laikotarpis', 'Europos individualaus vartojimo išlaidų pagal paskirtį klasifikatorius (ECOICOP)', 'Reikšmė')] <- c('date', 'variable', 'value')
data.cpi <- arrange(data.cpi[,names(data.cpi) %in% c('date', 'variable', 'value')], date)
data.cpi <- dcast(data.cpi, date ~ variable)
names(data.cpi)[names(data.cpi) == 'VARTOJIMO PREKĖS IR PASLAUGOS'] <- 'cpi'
names(data.cpi)[names(data.cpi) == 'ALKOHOLINIAI GĖRIMAI, TABAKAS IR NARKOTIKAI'] <- 'cpi.alcohol'

payments_out_number <- 'MSS.Q.F020.I00A.Z00Z.NT.10.200.000.Z.A0000.SR'
payments_out_value <- 'MSS.Q.F020.I00A.Z00Z.VT.10.200.000.E.A0000.SR'
payments_in_number <- 'MSS.Q.F030.I00A.Z00Z.NT.90.200.000.Z.A0000.SR'
payments_in_value <- 'MSS.Q.F030.I00A.Z00Z.VT.90.200.000.E.A0000.SR'
cash_out_atm_number <- 'MSS.Q.F120.I10.I111.NT.10.200.000.Z.A0000.SR'
cash_out_atm_value <- 'MSS.Q.F120.I10.I111.VT.10.200.000.E.A0000.SR'
cash_in_atm_number <- 'MSS.Q.F120.I10.I110.NT.10.200.000.Z.A0000.SR'
cash_in_atm_value <- 'MSS.Q.F120.I10.I110.VT.10.200.000.E.A0000.SR'
cash_out_branch_number <- 'MSS.Q.F102.IOT.Y000.NT.X0.200.000.Z.A0000.SR'
cash_out_branch_value <- 'MSS.Q.F102.IOT.Y000.VT.X0.200.000.E.A0000.SR'
cash_in_branch_number <- 'MSS.Q.F103.IOT.Y000.NT.X0.200.000.Z.A0000.SR'
cash_in_branch_value <- 'MSS.Q.F103.IOT.Y000.VT.X0.200.000.E.A0000.SR'
all.payments.list <- c(payments_out_number, payments_out_value, payments_in_number, payments_in_value, cash_out_atm_number, cash_out_atm_value, cash_in_atm_number, cash_in_atm_value, cash_out_branch_number, cash_out_branch_value, cash_in_branch_number, cash_in_branch_value)
data.payments <- data.payments[data.payments$code %in% c(all.payments.list),]
data.payments[data.payments$code == payments_out_number,'variable'] <- 'payments_out_number'
data.payments[data.payments$code == payments_out_value,'variable'] <- 'payments_out_value'
data.payments[data.payments$code == payments_in_number,'variable'] <- 'payments_in_number'
data.payments[data.payments$code == payments_in_value,'variable'] <- 'payments_in_value'
data.payments[data.payments$code == cash_out_atm_number,'variable'] <- 'cash_out_atm_number'
data.payments[data.payments$code == cash_out_atm_value,'variable'] <- 'cash_out_atm_value'
data.payments[data.payments$code == cash_in_atm_number,'variable'] <- 'cash_in_atm_number'
data.payments[data.payments$code == cash_in_atm_value,'variable'] <- 'cash_in_atm_value'
data.payments[data.payments$code == cash_out_branch_number,'variable'] <- 'cash_out_branch_number'
data.payments[data.payments$code == cash_out_branch_value,'variable'] <- 'cash_out_branch_value'
data.payments[data.payments$code == cash_in_branch_number,'variable'] <- 'cash_in_branch_number'
data.payments[data.payments$code == cash_in_branch_value,'variable'] <- 'cash_in_branch_value'
data.payments <- arrange(data.payments[,names(data.payments) %in% c('date', 'value', 'variable')], date)
data.payments <- dcast(data.payments, date ~ variable)

names(data.emigrants)[names(data.emigrants) %in% c('Laikotarpis', 'Rodiklis', 'Reikšmė')] <- c('date', 'variable', 'value')
data.emigrants <- arrange(data.emigrants[,names(data.emigrants) %in% c('date', 'variable', 'value')], date)
data.emigrants <- dcast(data.emigrants, date ~ variable)
names(data.emigrants)[names(data.emigrants) == 'Mėnesinis emigrantų skaičius'] <- 'emigrants'

names(data.euribor)[names(data.euribor) == 'Value'] <- 'euribor'






# FORECASTS ----------------------------------------------------------------

# PLOTS---------------------------------------------------------------------

# OUTPUT -----------------------------------------------------------------------











