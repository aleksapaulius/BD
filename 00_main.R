
# Loading Packages -------------------------------------------------------------

library("Quandl")
library("readxl")
library("dplyr")
library("reshape2")
library("tempdisagg")

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

# interest rate | euribor 3M rates: https://www.quandl.com/data/BOF/QS_D_IEUTIO3M-EURIBOR-3-Months-Daily
data.euribor <- Quandl("BOF/QS_D_IEUTIO3M")

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
data.loans.deposits <- read.csv2(file = "input/loans_deposits_2015_2017.csv", sep = ";")
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
data.travel.agencies <- read.csv2(file = "input/travel_agencies.csv", sep = ";")


# USER SELECTIONS --------------------------------------------------------------

# DATA MANIPULATION ------------------------------------------------------------

### cleaning imported data

# data.alcohol.consumption
names(data.alcohol.consumption)[names(data.alcohol.consumption) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'alcohol_consumption')
data.alcohol.consumption <- arrange(data.alcohol.consumption[,names(data.alcohol.consumption) %in% c('date', 'alcohol_consumption')], date)

# data.alcohol.price
names(data.alcohol.price)[names(data.alcohol.price) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'alcohol_price')
data.alcohol.price <- arrange(data.alcohol.price[,names(data.alcohol.price) %in% c('date', 'alcohol_price')], date)

# data.bankrupts
names(data.bankrupts)[names(data.bankrupts) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'bankrupts')
data.bankrupts <- arrange(data.bankrupts[,names(data.bankrupts) %in% c('date', 'bankrupts')], date)

# data.cards
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

# data.cpi
names(data.cpi)[names(data.cpi) %in% c('Laikotarpis', 'Europos individualaus vartojimo išlaidų pagal paskirtį klasifikatorius (ECOICOP)', 'Reikšmė')] <- c('date', 'variable', 'value')
data.cpi <- arrange(data.cpi[,names(data.cpi) %in% c('date', 'variable', 'value')], date)
data.cpi <- dcast(data.cpi, date ~ variable)
names(data.cpi)[names(data.cpi) == 'VARTOJIMO PREKĖS IR PASLAUGOS'] <- 'cpi'
names(data.cpi)[names(data.cpi) == 'ALKOHOLINIAI GĖRIMAI, TABAKAS IR NARKOTIKAI'] <- 'cpi.alcohol'

# data.payments
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
data.payments <- data.payments[data.payments$code %in% all.payments.list,]
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
data.payments["cash_in_number"] <- data.payments$cash_in_atm_number + data.payments$cash_in_branch_number
data.payments["cash_in_value"] <- data.payments$cash_in_atm_value + data.payments$cash_in_branch_value
data.payments["cash_out_number"] <- data.payments$cash_out_atm_number + data.payments$cash_out_branch_number
data.payments["cash_out_value"] <- data.payments$cash_out_atm_value + data.payments$cash_out_branch_value
data.payments <- data.payments[,c("date", "payments_in_number", "payments_in_value", "payments_out_number", "payments_out_value", "cash_in_number", "cash_in_value", "cash_out_number", "cash_out_value")]

# data.emigrants
names(data.emigrants)[names(data.emigrants) %in% c('Laikotarpis', 'Rodiklis', 'Reikšmė')] <- c('date', 'variable', 'value')
data.emigrants <- arrange(data.emigrants[,names(data.emigrants) %in% c('date', 'variable', 'value')], date)
data.emigrants <- dcast(data.emigrants, date ~ variable)
names(data.emigrants)[names(data.emigrants) == 'Mėnesinis emigrantų skaičius'] <- 'emigrants'

names(data.euribor)[names(data.euribor) == 'Value'] <- 'euribor'

# data.loans.deposits
deposit_interest_EUR <- 'PNS.M.L22.A.C.A.U2.250.200.N.LT.PC___.E.SR'
loan_interest_EUR <- 'PNS.M.A2U.A.C.A.U2.250.200.P.LT.PC___.E.SR'
loan_value_EUR <- 'PNS.M.A2U.A.B.A.U2.250.200.N.LT.PC___.E.SR'
data.loans.deposits <- data.loans.deposits[data.loans.deposits$code %in% c(deposit_interest_EUR, loan_interest_EUR, loan_value_EUR),]
data.loans.deposits[data.loans.deposits$code == deposit_interest_EUR,'variable'] <- 'deposit_interest_EUR'
data.loans.deposits[data.loans.deposits$code == loan_interest_EUR,'variable'] <- 'loan_interest_EUR'
data.loans.deposits[data.loans.deposits$code == loan_value_EUR,'variable'] <- 'loan_value_EUR'
data.loans.deposits <- arrange(data.loans.deposits[,names(data.loans.deposits) %in% c('date', 'value', 'variable')], date)
data.loans.deposits <- dcast(data.loans.deposits, date ~ variable)

# data.money
data.money <- data.money[,c(1,2,3,6,9)]
names(data.money) <- c("date", "cash", "deposits_1d", "deposits_2m", "deposits_3m")
data.money <- data.money[8:260,]
data.money$deposits_3m[data.money$deposits_3m == "…"] <- NA
data.money$cash <- as.numeric(data.money$cash)
data.money$deposits_1d <- as.numeric(data.money$deposits_1d)
data.money$deposits_2m <- as.numeric(data.money$deposits_2m)
data.money$deposits_3m <- as.numeric(data.money$deposits_3m)
data.money$deposits_3m[is.na(data.money$deposits_3m)] <- 0
data.money["deposits"] <- data.money$deposits_1d + data.money$deposits_2m + data.money$deposits_3m

# regulation
data.regulation <- merge(data.regulation1, data.regulation2)
names(data.regulation)[which(names(data.regulation) == "Metai")] <- "date"
names(data.regulation)[which(names(data.regulation) == "Valstybės ir savivaldybių institucijų ir įstaigų užimtų pareigybių skaičius, sk.")] <- "regulation_employees"
names(data.regulation)[which(names(data.regulation) == "Valstybės ir savivaldybių institucijų ir įstaigų skaičius, sk.")] <- "regulation_institutions"


# FORECASTS ----------------------------------------------------------------

# PLOTS---------------------------------------------------------------------

# OUTPUT -----------------------------------------------------------------------











