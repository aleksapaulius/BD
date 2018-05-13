
# DATA DISAGGREGATION ----------------------------------------------------------

# Chow-Lin (Chow and Lin, 1971) is suited for stationary or cointegrated series, 
# while Fernandez (Fernández, 1981) and Litterman (Litterman, 1983) deal with non-cointegrated series.

# merging all monthly data into one data frame
alldata.m <- data.money
alldata.m <- merge(alldata.m, data.cpi)
alldata.m <- merge(alldata.m, data.loans.deposits)
alldata.m <- merge(alldata.m, data.emigrants)
alldata.m <- merge(alldata.m, data.retail)
alldata.m <- merge(alldata.m, data.unemp)

# creating time series objects of monthly data
for (i in 2:ncol(alldata.m)) {
  assign(paste0(names(alldata.m)[i], '.m'), ts(alldata.m[,i], start = c(2006, 1), frequency = 12))
}

# removing not complete data and non-relative data
regressors <- c("cash.m", "deposits.m", "cpi_alcohol.m", "cpi.m", "emigrants.m", "retail_volume.m", "unemp_female.m", "unemp_male.m")

# creating formula for disaggregation
f <- as.formula(paste("x.ts ~ ", paste(regressors, collapse=" + ")))


## ketvirtiniai -> mėnesiniai

# merging all quarterly data into one data frame
alldata.q <- data.payments
alldata.q <- merge(alldata.q, data.cards)

# disaggregating quarterly data
for (i in 2:ncol(alldata.q)) {
  assign('x.ts', ts(alldata.q[,i], start = c(2006, 1), frequency = 4))
  assign('m.ts', td(f, to = "monthly", method = "litterman-maxlog"))
  alldata.m[names(alldata.q[i])] <- predict(m.ts)
}


## metiniai -> mėnesiniai

# merging all yearly data into one data frame
alldata.y <- data.frame(date = modeling.period)
alldata.y <- merge(alldata.y, data.regulation, all.x = T) 
alldata.y <- merge(alldata.y, data.tax, all.x = T)
alldata.y <- merge(alldata.y, data.travel, all.x = T)
alldata.y <- merge(alldata.y, data.alcohol.consumption, all.x = T)
alldata.y <- merge(alldata.y, data.alcohol.price, all.x = T)
alldata.y <- merge(alldata.y, data.bankrupts, all.x = T)

# disaggregating yearly data
for (i in 2:ncol(alldata.y)) {
  assign('x.ts', ts(alldata.y[,i], start = c(2006), frequency = 1))
  assign('m.ts', td(f, to = "monthly", method = "litterman-maxlog"))
  alldata.m[names(alldata.y[i])] <- predict(m.ts)
}


## pusmetiniai -> mėnesiniai

# merging all semiannual data into one data frame
alldata.s <- data.wage

# disaggregating semiannual data
for (i in 2:ncol(alldata.s)) {
  assign('x.ts', ts(alldata.s[,i], start = c(2006), frequency = 2))
  assign('m.ts', td(f, to = "monthly", method = "litterman-maxlog"))
  alldata.m[names(alldata.s[i])] <- predict(m.ts)
}







