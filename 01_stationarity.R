
# STATIONARITY -----------------------------------------------------------------

# Phillips-Perron Test
# H0 - turi unit root, H1 - neturi vienetinės šaknies (stacionaru)


?ur.pp

# data.money
pp.data <- ur.pp(data.money$cash, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2] # jei taip, tai tada stacionaru (atmetam nulinę hipotezę)

pp.data <- ur.pp(data.money$deposits, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.tax
pp.data <- ur.pp(data.tax$tax_gpm, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.tax$tax_excise, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.tax$tax_pelno, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.tax$tax_vat, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.cpi
pp.data <- ur.pp(data.cpi$cpi_alcohol, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.cpi$cpi, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.regulation
pp.data <- ur.pp(data.regulation$regulation_employees, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.regulation$regulation_institutions, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.unemp
pp.data <- ur.pp(data.unemp$unemp_female, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.unemp$unemp_male, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.payments
pp.data <- ur.pp(data.payments$payments_in_number, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.payments$payments_in_value, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.payments$payments_out_number, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.payments$payments_out_value, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.payments$cash_in_number, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.payments$cash_in_value, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.payments$cash_out_number, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.payments$cash_out_value, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.alcohol.consumption
pp.data <- ur.pp(data.alcohol.consumption$alcohol_consumption, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.alcohol.price
pp.data <- ur.pp(data.alcohol.price$alcohol_price, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.bankrupts
pp.data <- ur.pp(data.bankrupts$bankrupts, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.cards
pp.data <- ur.pp(data.cards$credit_cards, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.cards$debit_cards, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.emigrants
pp.data <- ur.pp(data.emigrants$emigrants, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.loans.deposits
pp.data <- ur.pp(data.loans.deposits$loan_interest_EUR, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.loans.deposits$loan_value_EUR, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.loans.deposits$deposit_interest_EUR, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.loans.deposits$loan_interest_LTL, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.loans.deposits$loan_value_LTL, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.loans.deposits$deposit_interest_LTL, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.retail
pp.data <- ur.pp(data.retail$retail_volume, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.wage
pp.data <- ur.pp(data.wage$minimum_wage, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

# data.travel
pp.data <- ur.pp(data.travel$outgoing_tourists, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]

pp.data <- ur.pp(data.travel$travel_agencies, type="Z-tau", model="trend", lags="short")
pp.data@teststat < pp.data@cval[2]




# data(nporg)
# gnp <- na.omit(nporg[, "gnp.r"])
# pp.gnp <- ur.pp(gnp, type="Z-tau", model="trend", lags="short")
# pp.gnp
# pp.gnp@cval
# pp.gnp@lags
# 
# summary(pp.gnp)





















