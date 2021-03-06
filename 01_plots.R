# PLOTS ------------------------------------------------------------------------

## pinigu statistika (grynieji pinigai, indeliai)
df <- data.money
df <- melt(df, id='date')
df <- df[!is.na(df$value),]
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(date, value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free", 
             labeller = as_labeller(c(`cash` = "Grynieji pinigai esantys apyvartoje", `deposits` = "Indėliai"))) +
  labs(title="Grynieji pinigai ir indėliai", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas", x = 'Metai', y = 'mln. Eur', color=NULL) +
  theme(strip.background = element_blank())

all.variables.stat[all.variables.stat$variable %in% c('cash', 'deposits'),]

## tax
df <- melt(data.tax, id='date')
df$date <- as.Date(as.yearmon(df$date) + 11/12, frac = 1)
df$value <- df$value / 1000
brks <- df$date[seq(1, length(df$date), 1)]
lbls <- lubridate::year(brks)
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Įmonių ir gyventojų sumokėti mokesčiai", 
       subtitle="Metiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas", 
       y="mln. Eur",
       x = 'Metai',
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("Gyventojų pajamų mokestis", "Akcizai", 'Pelno mokestis', 'Pridėtinės vertės mokestis'),
                     values = c("tax_gpm" = "#F8766D", "tax_excise" = "#00BFC4", "tax_pelno" = "#7CAE00", "tax_vat" = "#C77CFF"))

all.variables.stat[all.variables.stat$variable %in% c('tax_gpm', 'tax_excise', 'tax_pelno', 'tax_vat'),]

## cpi
df <- melt(data.cpi, id='date')
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Suderinti vartotojų kainų indeksai (2015 m. – 100)", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas",
       x = 'Metai',
       y = 'Indeksas',
       color=NULL) +  # title and caption
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("ALKOHOLINIAI GĖRIMAI, TABAKAS IR NARKOTIKAI", "VARTOJIMO PREKĖS IR PASLAUGOS"),
                     values = c("cpi_alcohol" = "#F8766D", "cpi" = "#00BFC4"))

all.variables.stat[all.variables.stat$variable %in% c('cpi_alcohol', 'cpi'),]

## economic regulation
df <- melt(data.regulation, id='date')
df$date <- as.Date(as.yearmon(df$date) + 11/12, frac = 1)
brks <- df$date[seq(1, length(df$date), 1)]
lbls <- lubridate::year(brks)
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Ekonominis reguliavimas", 
       subtitle="Metiniai duomenys", 
       caption="Šaltinis: Valstybės tarnybos departamentas", 
       y="Vienetai",
       x = 'Metai',
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("Užimtų vadovaujamų pareigybių skaičius", "Valstybės ir savivaldybių institucijų ir įstaigų skaičius"),
                     values = c("regulation_employees" = "#F8766D", "regulation_institutions" = "#00BFC4"))

all.variables.stat[all.variables.stat$variable %in% c('regulation_employees', 'regulation_institutions'),]

## unemployment
df <- melt(data.unemp, id='date')
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Nedarbo lygis, pašalinus sezono įtaką", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas",
       x = 'Metai',
       y = 'Proc.',
       color=NULL) +  # title and caption
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("Moterys", "Vyrai"),
                     values = c("unemp_female" = "#F8766D", "unemp_male" = "#00BFC4"))

all.variables.stat[all.variables.stat$variable %in% c('unemp_female', 'unemp_male'),]

## electronic payments
df <- data.payments
df$date <- gsub('-Q1', '-03-31', df$date)
df$date <- gsub('-Q2', '-06-30 ', df$date)
df$date <- gsub('-Q3', '-09-30 ', df$date)
df$date <- gsub('-Q4', '-12-31 ', df$date)
df$date <- as.Date(df$date)

df1 <- df[,c('date', 'payments_in_number', 'payments_in_value')]
df1 <- melt(df1, id='date')
brks <- df1$date[seq(1, length(df1$date), 4)]
lbls <- lubridate::year(brks)
ggplot(df1, aes(date, value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free", labeller = as_labeller(c(`payments_in_number` = "Skaičius", `payments_in_value` = "Vertė"))) +
  scale_x_date(labels = lbls, breaks = brks) +
  labs(title="Gauti mokėjimai iš užsienio negrynaisiais pinigais", 
       subtitle="Ketvirtiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas", x = 'Metai', y = 'Tūkst.', color=NULL) +
  theme(strip.background = element_blank())

all.variables.stat[all.variables.stat$variable %in% c('payments_in_number', 'payments_in_value'),]

df1 <- df[,c('date', 'payments_out_number', 'payments_out_value')]
df1 <- melt(df1, id='date')
brks <- df1$date[seq(1, length(df1$date), 4)]
lbls <- lubridate::year(brks)
ggplot(df1, aes(date, value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free", labeller = as_labeller(c(`payments_out_number` = "Skaičius", `payments_out_value` = "Vertė"))) +
  scale_x_date(labels = lbls, breaks = brks) +
  labs(title="Visi mokėjimai negrynaisiais pinigais Lietuvoje", 
       subtitle="Ketvirtiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas", x = 'Metai', y = 'Tūkst.', color=NULL) +
  theme(strip.background = element_blank())

all.variables.stat[all.variables.stat$variable %in% c('payments_out_number', 'payments_out_value'),]

df1 <- df[,c('date', 'cash_in_number', 'cash_in_value', 'cash_out_number', 'cash_out_value')]
df1 <- melt(df1, id='date')
brks <- df1$date[seq(1, length(df1$date), 4)]
lbls <- lubridate::year(brks)
ggplot(df1, aes(date, value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free", 
             labeller = as_labeller(c(`cash_in_number` = "Priėmimo operacijų skaičius", `cash_in_value` = "Priėmimo operacijų vertė", `cash_out_number` = "Išdavimo operacijų skaičius", `cash_out_value` = "Išdavimo operacijų vertė"))) +
  scale_x_date(labels = lbls, breaks = brks) +
  labs(title="Grynųjų pinigų operacijos", 
       subtitle="Ketvirtiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas", x = 'Metai', y = 'Tūkst.', color=NULL) +
  theme(strip.background = element_blank())

all.variables.stat[all.variables.stat$variable %in% c('cash_in_number', 'cash_in_value', 'cash_out_number', 'cash_out_value'),]

## alcohol consumption and price
df <- merge(data.alcohol.consumption, data.alcohol.price)
df <- melt(df, id='date')
df$date <- as.Date(as.yearmon(df$date) + 11/12, frac = 1)
brks <- df$date[seq(1, length(df$date), 1)]
lbls <- lubridate::year(brks)
ggplot(df, aes(date, value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free", 
             labeller = as_labeller(c(`alcohol_consumption` = "Suvartojimas, (l) absoliutaus alk. gyventojui", `alcohol_price` = "Mažmeninė kaina, Eur"))) +
  scale_x_date(labels = lbls, breaks = brks) +
  labs(title="Alkoholio suvartojimas ir kaina", 
       subtitle="Metiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas", x = 'Metai', y = '', color=NULL) +
  theme(strip.background = element_blank())

all.variables.stat[all.variables.stat$variable %in% c('alcohol_consumption', 'alcohol_price'),]

## bankrupts
df <- melt(data.bankrupts, id='date')
df$date <- as.Date(as.yearmon(df$date) + 11/12, frac = 1)
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value)) + 
  labs(title="Atitinkamais metais pradėtų bankroto procesų skaičius", 
       subtitle="Metiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas", 
       y="Skaičius",
       x = 'Metai',
       color=NULL)

all.variables.stat[all.variables.stat$variable %in% c('bankrupts'),]

## cards
df <- melt(data.cards, id='date')
df$date <- gsub('-Q1', '-03-31', df$date)
df$date <- gsub('-Q2', '-06-30 ', df$date)
df$date <- gsub('-Q3', '-09-30 ', df$date)
df$date <- gsub('-Q4', '-12-31 ', df$date)
df$date <- as.Date(df$date)
df$value <- df$value / 1000
brks <- df1$date[seq(1, length(df1$date), 4)]
lbls <- lubridate::year(brks)
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  scale_x_date(labels = lbls, breaks = brks) +
  labs(title="Mokėjimo kortelės", 
       subtitle="Ketvirtiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas",
       x = 'Metai',
       y = 'Tūkst.',
       color=NULL) +  # title and caption
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("Kreditinės", "Debetinės"),
                     values = c("credit_cards" = "#F8766D", "debit_cards" = "#00BFC4"))

all.variables.stat[all.variables.stat$variable %in% c('credit_cards', 'debit_cards'),]

## emigrants
df <- melt(data.emigrants, id='date')
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value)) + 
  labs(title="Emigrantų skaičius", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas", 
       y="Asmenys",
       x = 'Metai',
       color=NULL)

all.variables.stat[all.variables.stat$variable %in% c('emigrants'),]

## loans and deposits
df <- data.loans.deposits[,c('date', 'loan_value_EUR', 'loan_value_LTL')]
df <- melt(df, id='date')
df <- df[!is.na(df$value),]
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Suteiktų paskolų vertės", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas", 
       y="mln. Eur",
       x = 'Metai',
       color=NULL) +  # title and caption
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("Paskolos eurais", "Paskolos litais"),
                     values = c("loan_value_EUR" = "#F8766D", "loan_value_LTL" = "#00BFC4"))

all.variables.stat[all.variables.stat$variable %in% c('loan_value_EUR', 'loan_value_LTL'),]

df <- data.loans.deposits[,c('date', 'loan_interest_EUR', 'loan_interest_LTL')]
df <- melt(df, id='date')
df <- df[!is.na(df$value),]
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Paskolos namų ūkių vartojimui (palūkanų normos)", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas", 
       y="proc.",
       x = 'Metai',
       color=NULL) +  # title and caption
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("Paskolos eurais", "Paskolos litais"),
                     values = c("loan_interest_EUR" = "#F8766D", "loan_interest_LTL" = "#00BFC4"))

all.variables.stat[all.variables.stat$variable %in% c('loan_interest_EUR', 'loan_interest_LTL'),]

df <- data.loans.deposits[,c('date', 'deposit_interest_EUR', 'deposit_interest_LTL')]
df <- melt(df, id='date')
df <- df[!is.na(df$value),]
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Indėliai pinigų finansų įstaigose (palūkanų normos)", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos bankas", 
       y="proc.",
       x = 'Metai',
       color=NULL) +  # title and caption
  scale_color_manual(breaks = unique(as.character(df$variable)), 
                     labels = c("Indėliai eurais", "Indėliai litais"),
                     values = c("deposit_interest_EUR" = "#F8766D", "deposit_interest_LTL" = "#00BFC4"))

all.variables.stat[all.variables.stat$variable %in% c('deposit_interest_EUR', 'deposit_interest_LTL'),]

## retail
df <- melt(data.retail, id='date')
df$date <- as.Date(as.yearmon(df$date, "%Y %m"))
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value)) + 
  labs(title="Maisto, gėrimų ir tabako mažmeninė prekyba (palyginti su 2015 m.)", 
       subtitle="Mėnesiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas", 
       y="Proc.",
       x = 'Metai',
       color=NULL)

all.variables.stat[all.variables.stat$variable %in% c('retail_volume'),]

## minimum wage
df <- melt(data.wage, id='date')
df$date <- gsub('S1', '-06-30', df$date)
df$date <- gsub('S2', '-12-31', df$date)
df$date <- as.Date(df$date)
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value)) + 
  labs(title="Minimalus mėnesinis atlyginimas", 
       subtitle="Pusmetiniai duomenys", 
       caption="Šaltinis: Eurostat", 
       y="Eur.",
       x = 'Metai',
       color=NULL)

all.variables.stat[all.variables.stat$variable %in% c('minimum_wage'),]

## travel
df <- melt(data.travel, id='date')
df$date <- as.Date(as.yearmon(df$date) + 11/12, frac = 1)
ggplot(df, aes(date, value)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free", labeller = as_labeller(c(`outgoing_tourists` = "Išvykstantys turistai (tūkst.)", `travel_agencies` = "Kelionių agentūros (vnt.)"))) +
  labs(title="Kelionių agentūros ir išvykstantys turistai", 
       subtitle="Metiniai duomenys", 
       caption="Šaltinis: Lietuvos statistikos departamentas", x = 'Metai', y = '', color=NULL) +
  theme(strip.background = element_blank())

all.variables.stat[all.variables.stat$variable %in% c('outgoing_tourists', 'travel_agencies'),]


