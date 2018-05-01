
### cleaning imported data

# data.money
data.money["deposits"] <- data.money$deposits_1d + data.money$deposits_2m + data.money$deposits_3m
data.money <- data.money[,c('date', 'cash', 'deposits')] %>% arrange(date)
data.money$cash <- data.money$cash / 3.4528
data.money$deposits <- data.money$deposits / 3.4528
cash <- 'BPS.M.L10.X.1.10.300.000.LT.N.PA___.E.SR'
deposits_1d	<- 'BPS.M.L21.A.1.U2.300.000.LT.N.P____.E.SR'
deposits_2m	<- 'BPS.M.L22.K.1.U2.300.000.LT.N.P____.E.SR'
deposits_3m <- 'BPS.M.L23.G.1.U2.300.000.LT.N.P____.E.SR'
data.money2015 <- data.money2015[data.money2015$code %in% c(cash, deposits_1d, deposits_2m, deposits_3m),]
data.money2015[data.money2015$code == cash,'variable'] <- 'cash'
data.money2015[data.money2015$code == deposits_1d,'variable'] <- 'deposits_1d'
data.money2015[data.money2015$code == deposits_2m,'variable'] <- 'deposits_2m'
data.money2015[data.money2015$code == deposits_3m,'variable'] <- 'deposits_3m'
data.money2015 <- data.money2015[,names(data.money2015) %in% c('date', 'value', 'variable')] %>% arrange(date)
data.money2015 <- dcast(data.money2015, date ~ variable)
data.money2015$date <- gsub('-', ' ', data.money2015$date)
data.money2015["deposits"] <- data.money2015$deposits_1d + data.money2015$deposits_2m + data.money2015$deposits_3m
data.money2015 <- data.money2015[,c('date', 'cash', 'deposits')] %>% arrange(date)
data.money <- rbind(data.money, data.money2015) %>% arrange(date)

# tax
data.tax <- dcast(data.tax, Laikotarpis ~ Mokėtojai + Mokesčiai, value.var = 'Reikšmė')
data.tax <- data.tax[colSums(!is.na(data.tax)) > 0]
names(data.tax)[names(data.tax) == "Laikotarpis"] <- "date"
names(data.tax)[names(data.tax) == "Gyventojai_Gyventojų pajamų mokestis"] <- "tax_gpm"
names(data.tax)[names(data.tax) == "Įmonės_Akcizai"] <- "tax_excise"
names(data.tax)[names(data.tax) == "Įmonės_Pelno mokestis"] <- "tax_pelno"
names(data.tax)[names(data.tax) == "Įmonės_Pridėtinės vertės mokestis"] <- "tax_vat"

# data.cpi
names(data.cpi)[names(data.cpi) %in% c('Laikotarpis', 'Europos.individualaus.vartojimo.išlaidų.pagal.paskirtį.klasifikatorius..ECOICOP.', 'Reikšmė')] <- c('date', 'variable', 'value')
data.cpi <- dcast(data.cpi, date ~ variable) %>% arrange(date)
names(data.cpi)[names(data.cpi) == 'VARTOJIMO PREKĖS IR PASLAUGOS'] <- 'cpi'
names(data.cpi)[names(data.cpi) == 'ALKOHOLINIAI GĖRIMAI, TABAKAS IR NARKOTIKAI'] <- 'cpi_alcohol'
data.cpi$date <- gsub('M', ' ', data.cpi$date)

# regulation
data.regulation <- merge(data.regulation1, data.regulation2)
names(data.regulation)[names(data.regulation) == "Metai"] <- "date"
names(data.regulation)[names(data.regulation) == "Valstybės.ir.savivaldybių.institucijų.ir.įstaigų.užimtų.pareigybių.skaičius..sk."] <- "regulation_employees"
names(data.regulation)[names(data.regulation) == "Valstybės.ir.savivaldybių.institucijų.ir.įstaigų.skaičius..sk."] <- "regulation_institutions"

# unemployment
data.unemp <- dcast(data.unemp, Laikotarpis ~ Rodiklis + Lytis, value.var = 'Reikšmė')
names(data.unemp)[names(data.unemp) == "Laikotarpis"] <- "date"
names(data.unemp)[names(data.unemp) == "Nedarbo lygis, pašalinus sezono įtaką_Moterys"] <- "unemp_female"
names(data.unemp)[names(data.unemp) == "Nedarbo lygis, pašalinus sezono įtaką_Vyrai"] <- "unemp_male"
data.unemp$date <- gsub('M', ' ', data.unemp$date)

# data.payments
payments_out_number <- 'MSS.Q.F020.I00A.Z00Z.NT.10.200.000.Z.A0000.SR'  # Visų mokėjimų negrynaisiais pinigais (išskyrus mokėjimus el. pinigais) Lietuvoje skaičius
payments_out_value <- 'MSS.Q.F020.I00A.Z00Z.VT.10.200.000.E.A0000.SR'   # Visų mokėjimų negrynaisiais pinigais (išskyrus mokėjimus el. pinigais) Lietuvoje vertė
payments_in_number <- 'MSS.Q.F030.I00A.Z00Z.NT.90.200.000.Z.A0000.SR'   # Visų Lietuvos mokėjimo paslaugų teikėjų klientų gautų mokėjimų iš užsienio negrynaisiais pinigais skaičius
payments_in_value <- 'MSS.Q.F030.I00A.Z00Z.VT.90.200.000.E.A0000.SR'    # Visų Lietuvos mokėjimo paslaugų teikėjų klientų gautų mokėjimų iš užsienio negrynaisiais pinigais vertė
cash_out_atm_number <- 'MSS.Q.F120.I10.I111.NT.10.200.000.Z.A0000.SR'   # Grynųjų pinigų išdavimo operacijų skaičius Lietuvos mokėjimo paslaugų teikėjų aptarnaujamuose bankomatuose Lietuvoje Lietuvos mokėjimo paslaugų teikėjų išleistomis mokėjimo kortelėmis
cash_out_atm_value <- 'MSS.Q.F120.I10.I111.VT.10.200.000.E.A0000.SR'    # Grynųjų pinigų išdavimo operacijų vertė Lietuvos mokėjimo paslaugų teikėjų aptarnaujamuose bankomatuose Lietuvoje Lietuvos mokėjimo paslaugų teikėjų išleistomis mokėjimo kortelėmis
cash_in_atm_number <- 'MSS.Q.F120.I10.I110.NT.10.200.000.Z.A0000.SR'    # Grynųjų pinigų priėmimo operacijų skaičius Lietuvos mokėjimo paslaugų teikėjų aptarnaujamuose bankomatuose Lietuvoje Lietuvos mokėjimo paslaugų teikėjų išleistomis mokėjimo kortelėmis
cash_in_atm_value <- 'MSS.Q.F120.I10.I110.VT.10.200.000.E.A0000.SR'     # Grynųjų pinigų priėmimo operacijų vertė Lietuvos mokėjimo paslaugų teikėjų aptarnaujamuose bankomatuose Lietuvoje Lietuvos mokėjimo paslaugų teikėjų išleistomis mokėjimo kortelėmis
cash_out_branch_number <- 'MSS.Q.F102.IOT.Y000.NT.X0.200.000.Z.A0000.SR'# Visų Lietuvos mokėjimo paslaugų teikėjų klientų aptarnavimo vietose grynųjų pinigų išdavimo operacijų skaičius
cash_out_branch_value <- 'MSS.Q.F102.IOT.Y000.VT.X0.200.000.E.A0000.SR' # Visų Lietuvos mokėjimo paslaugų teikėjų klientų aptarnavimo vietose grynųjų pinigų išdavimo operacijų vertė
cash_in_branch_number <- 'MSS.Q.F103.IOT.Y000.NT.X0.200.000.Z.A0000.SR' # Visų Lietuvos mokėjimo paslaugų teikėjų klientų aptarnavimo vietose grynųjų pinigų priėmimo operacijų skaičius
cash_in_branch_value <- 'MSS.Q.F103.IOT.Y000.VT.X0.200.000.E.A0000.SR'  # Visų Lietuvos mokėjimo paslaugų teikėjų klientų aptarnavimo vietose grynųjų pinigų priėmimo operacijų vertė
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

# data.alcohol.consumption
names(data.alcohol.consumption)[names(data.alcohol.consumption) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'alcohol_consumption')
data.alcohol.consumption <- data.alcohol.consumption[,names(data.alcohol.consumption) %in% c('date', 'alcohol_consumption')] %>% arrange(date)

# data.alcohol.price
names(data.alcohol.price)[names(data.alcohol.price) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'alcohol_price')
data.alcohol.price <- data.alcohol.price[,names(data.alcohol.price) %in% c('date', 'alcohol_price')] %>% arrange(date)

# data.bankrupts
names(data.bankrupts)[names(data.bankrupts) %in% c('Laikotarpis', 'Reikšmė')] <- c('date', 'bankrupts')
data.bankrupts <- data.bankrupts[,names(data.bankrupts) %in% c('date', 'bankrupts')] %>% arrange(date)

# data.cards
dc <- "MSS.Q.S101.I12.Z00Z.NT.X0.200.Z0Z.Z.A0000.SR"
cc <- "MSS.Q.S101.I13V.Z00Z.NT.X0.200.Z0Z.Z.A0000.SR"
data.cards <- data.cards[data.cards$code %in% c(dc, cc),]
data.cards[data.cards$code == dc,'variable'] <- 'debit_cards'
data.cards[data.cards$code == cc,'variable'] <- 'credit_cards'
data.cards <- dcast(data.cards, date ~ variable) %>% arrange(date)

# data.emigrants
names(data.emigrants)[names(data.emigrants) %in% c('Laikotarpis', 'Rodiklis', 'Reikšmė')] <- c('date', 'variable', 'value')
data.emigrants <- data.emigrants[,names(data.emigrants) %in% c('date', 'variable', 'value')] %>% arrange(date)
data.emigrants <- dcast(data.emigrants, date ~ variable)
names(data.emigrants)[names(data.emigrants) == 'Mėnesinis emigrantų skaičius'] <- 'emigrants'
data.emigrants$date <- gsub('M', ' ', data.emigrants$date)

# data.loans.deposits
deposit_interest_EUR <- 'PNS.M.L22.A.C.A.U2.250.200.N.LT.PC___.E.SR'  # Euro zonos namų ūkių sutarto termino indėliai pinigų finansų įstaigose – naujų susitarimų palūkanų normos
loan_interest_EUR <- 'PNS.M.A2U.A.C.A.U2.250.200.P.LT.PC___.E.SR'     # Pinigų finansų įstaigų paskolos euro zonos namų ūkiams vartojimui – tikrųjų naujų paskolų palūkanų normos
loan_value_EUR <- 'PNS.M.A2U.A.B.A.U2.250.200.N.LT.PC___.E.SR'        # Pinigų finansų įstaigų paskolos euro zonos namų ūkiams vartojimui – naujų susitarimų sumos
data.loans.deposits2015 <- data.loans.deposits2015[data.loans.deposits2015$code %in% c(deposit_interest_EUR, loan_interest_EUR, loan_value_EUR),]
data.loans.deposits2015[data.loans.deposits2015$code == deposit_interest_EUR,'variable'] <- 'deposit_interest_EUR'
data.loans.deposits2015[data.loans.deposits2015$code == loan_interest_EUR,'variable'] <- 'loan_interest_EUR'
data.loans.deposits2015[data.loans.deposits2015$code == loan_value_EUR,'variable'] <- 'loan_value_EUR'
data.loans.deposits2015 <- data.loans.deposits2015[,names(data.loans.deposits2015) %in% c('date', 'value', 'variable')] %>% arrange(date)
data.loans.deposits2015 <- dcast(data.loans.deposits2015, date ~ variable)
data.loans.deposits2015$date <- gsub('-', ' ', data.loans.deposits2015$date)

# loan_value_EUR = Vartojimo paskolos eurais fiksuojant pradinę normą (eurais)
# loan_value_LTL = Vartojimo paskolos litais fiksuojant pradinę normą (eurais)

data.loans.deposits <- merge(data.loans2005, data.deposits2005, all = T)
data.loans.deposits <- merge(data.loans.deposits, data.loans.deposits2015, all = T)

# remove data up to 2006
data.loans.deposits <- data.loans.deposits[ , !(colnames(data.loans.deposits) %in% c("loan_interest_not_LTL", "deposit_interest_not_LTL"))] 


# retail
names(data.retail)[names(data.retail) == "Laikotarpis"] <- "date"
names(data.retail)[names(data.retail) == "Reikšmė"] <- "retail_volume"
data.retail <- data.retail[,names(data.retail) %in% c('date', 'retail_volume')] %>% arrange(date)
data.retail$date <- gsub('M', ' ', data.retail$date)

# minimum wage
data.wage <- data.wage[,c('TIME', 'Value')]
names(data.wage)[names(data.wage) == 'TIME'] <- "date"
names(data.wage)[names(data.wage) == 'Value'] <- "minimum_wage"
data.wage$minimum_wage <- as.numeric(data.wage$minimum_wage)

# tourists
names(data.tourists)[names(data.tourists) == "Laikotarpis"] <- "date"
names(data.tourists)[names(data.tourists) == "Reikšmė"] <- "outgoing_tourists"
data.tourists <- data.tourists[,names(data.tourists) %in% c('date', 'outgoing_tourists')] %>% arrange(date)

# travel agencies
names(data.travel.agencies)[names(data.travel.agencies) == "Laikotarpis"] <- "date"
names(data.travel.agencies)[names(data.travel.agencies) == "Reikšmė"] <- "travel_agencies"
data.travel.agencies <- data.travel.agencies[,names(data.travel.agencies) %in% c('date', 'travel_agencies')] %>% arrange(date)
data.travel <- merge(data.tourists, data.travel.agencies)


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

all.variables.stat <- statistics(all.data.list)


lt.names.table <- data.frame(variable = c("cash", "deposits", "tax_gpm", "tax_excise", "tax_pelno", "tax_vat", "cpi_alcohol", "cpi", 
                                           "regulation_employees", "regulation_institutions", "unemp_female", "unemp_male", "payments_in_number", 
                                           "payments_in_value", "payments_out_number", "payments_out_value", "cash_in_number", "cash_in_value", 
                                           "cash_out_number", "cash_out_value", "alcohol_consumption", "alcohol_price", "bankrupts", "credit_cards", 
                                           "debit_cards", "emigrants", "loan_interest_EUR", "loan_value_EUR", "deposit_interest_EUR", "loan_interest_LTL", 
                                           "loan_value_LTL", "deposit_interest_LTL", "retail_volume", "minimum_wage", "outgoing_tourists", "travel_agencies"), 
                             kintamasis = c("Grynieji pinigai", "Indėliai", "GPM", "Akcizo mokesčiai", "Pelno mokesčiai", "Pridėtinės vertės mokesčiai", "VKI alkoholiui", "VKI", 
                                          "Ekonominis reguliavimas: darbuotojų skaičius", "Ekonominis reguliavimas: įstaigų skaičius", "Moterų nedarbas", "Vyrų nedarbas", "Gautų mokėjimų skaičius", 
                                          "Gautų mokėjimų vertė", "Išeinančių mokėjimų skaičius", "Išeinančių mokėjimų vertė", "Grynųjų įnešimo operacijų skaičius", "Grynųjų įnešimo operacijų vertė", 
                                          "Grynųjų išėmimo operacijų skaičius", "Grynųjų išėmimo operacijų vertė", "Alkoholio suvartojimas", "Alkoholio kaina", "Bankrotų skaičius", "Kredito kortelių skaičius", 
                                          "Debeto kortelių skaičius", "Emigrantų skaičius", "Paskolų eurais palūkanų norma", "Paskolų eurais vertė", "Indėlių eurais palūkanų norma", "Paskolų litais palūkanų norma", 
                                          "Paskolų litais vertė", "Indėlių litais palūkanų norma", "Mažmeninės prekybos apimtis", "Minimalus atlyginimas", "Išvykstančių turistų skaičius", "Kelionių agentūrų skaičius"), 
                             stringsAsFactors = FALSE)

all.variables.stat <- merge(all.variables.stat, lt.names.table, all.x = T)

all.variables.stat <- all.variables.stat[,c(1,12,2:11)]








