
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















