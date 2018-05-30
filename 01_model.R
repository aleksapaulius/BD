# MODEL ------------------------------------------------------------------------

# paruosiam duomenis modeliavimui
alldata.model <- alldata.m[,!(names(alldata.m) %in% c('loan_interest_LTL', 'loan_value_LTL', 'deposit_interest_LTL'))]
diff1.variables <- setdiff(diff1.variables, c('loan_interest_LTL', 'loan_value_LTL', 'deposit_interest_LTL', 'cash', 'deposits', 'santykis'))
alldata.model[2:nrow(alldata.model), diff1.variables] <- lapply(alldata.m[diff1.variables], diff)
alldata.model <- alldata.model[2:nrow(alldata.model),]

EUR.dates <- c(apply(expand.grid(2015:2017, 1:9),1,paste,collapse=" 0"), apply(expand.grid(2015:2017, 10:12),1,paste,collapse=" "))
alldata.model[alldata.model$date %in% EUR.dates, 'dummy_EUR'] <- 1
alldata.model[is.na(alldata.model$dummy_EUR), 'dummy_EUR'] <- 0

alldata.model <- alldata.model[,!(names(alldata.model) %in% c('date', 'cash', 'deposits', "emigrants", 'payments_out_value', 'travel_agencies'))]


model.stat <- matrix(0, ncol = 4, nrow = 1) %>% data.frame()
names(model.stat) <- c('RSE', 'Adjusted R-squared', 'F-Statistic', 'any-aliased')

# renaming variables
for (var.name in setdiff(unique(names(alldata.model)), c('santykis', 'dummy_EUR'))) {
  names(alldata.model)[names(alldata.model) == var.name] <- all.variables.stat[all.variables.stat$variable == var.name,"kintamasis"]
}

names(alldata.model) <- gsub(" ", "_", names(alldata.model))
names(alldata.model) <- gsub(":", "_", names(alldata.model))

# changing scale of variables
alldata.model$Gautų_mokėjimų_vertė <- alldata.model$Gautų_mokėjimų_vertė / 100000
alldata.model$Išeinančių_mokėjimų_skaičius <- alldata.model$Išeinančių_mokėjimų_skaičius / 100
alldata.model$Grynųjų_įnešimo_operacijų_vertė <- alldata.model$Grynųjų_įnešimo_operacijų_vertė / 100000
alldata.model$Grynųjų_įnešimo_operacijų_vertė <- alldata.model$Grynųjų_įnešimo_operacijų_vertė / 100000
alldata.model$Grynųjų_išėmimo_operacijų_skaičius <- alldata.model$Grynųjų_išėmimo_operacijų_skaičius / 100
alldata.model$Grynųjų_išėmimo_operacijų_vertė <- alldata.model$Grynųjų_išėmimo_operacijų_vertė / 10000
alldata.model$Grynųjų_išėmimo_operacijų_vertė <- alldata.model$Grynųjų_išėmimo_operacijų_vertė / 10000
alldata.model$Kredito_kortelių_skaičius <- alldata.model$Kredito_kortelių_skaičius / 100
alldata.model$Debeto_kortelių_skaičius <- alldata.model$Debeto_kortelių_skaičius / 100000
alldata.model$GPM <- alldata.model$GPM / 1000
alldata.model$GPM <- alldata.model$GPM / 1000
alldata.model$Akcizo_mokesčiai <- alldata.model$Akcizo_mokesčiai / 100
alldata.model$Pelno_mokesčiai <- alldata.model$Pelno_mokesčiai / 10000
alldata.model$Pridėtinės_vertės_mokesčiai <- alldata.model$Pridėtinės_vertės_mokesčiai / 1000
alldata.model$Pridėtinės_vertės_mokesčiai <- alldata.model$Pridėtinės_vertės_mokesčiai / 1000
alldata.model$santykis <- alldata.model$santykis * 10




# MODEL 1
lm.regressors1 <- setdiff(names(alldata.model), 'santykis')
f1 <- as.formula(paste("santykis ~ ", paste(lm.regressors1, collapse=" + ")))
fit1 <- lm(f1, data = alldata.model)
model.stat.fit1 <-  model.stat
model.stat.fit1$RSE <- summary(fit1)$sigma
model.stat.fit1$`Adjusted R-squared` <- summary(fit1)$adj.r.squared
model.stat.fit1$`F-Statistic` <- summary(fit1)$fstatistic[1]
model.stat.fit1$`any-aliased` <- any(summary(fit1)$aliased)


# vif_func(in_frame=alldata.model[,names(alldata.model) %in% lm.regressors1],thresh=5,trace=T)
lm.regressors2 <- vif_func(in_frame=alldata.model[,names(alldata.model) %in% lm.regressors1],thresh=4,trace=F)


# MODEL 2
f2 <- as.formula(paste("santykis ~ ", paste(lm.regressors2, collapse=" + ")))
fit2 <- lm(f2, data = alldata.model)
model.stat.fit2 <-  model.stat
model.stat.fit2$RSE <- summary(fit2)$sigma
model.stat.fit2$`Adjusted R-squared` <- summary(fit2)$adj.r.squared
model.stat.fit2$`F-Statistic` <- summary(fit2)$fstatistic[1]
model.stat.fit2$`any-aliased` <- any(summary(fit2)$aliased)


# MODEL 3
lm.regressors3 <- setdiff(lm.regressors2, 
                          c('Išeinančių_mokėjimų_skaičius', 'Moterų_nedarbas', 'Grynųjų_išėmimo_operacijų_skaičius', 'Išvykstančių_turistų_skaičius', 'Paskolų_eurais_vertė',
                            'VKI', 'Indėlių_eurais_palūkanų_norma', 'Akcizo_mokesčiai', 'VKI_alkoholiui', 'Gautų_mokėjimų_skaičius'))
f3 <- as.formula(paste("santykis ~ ", paste(lm.regressors3, collapse=" + ")))
fit3 <- lm(f3, data = alldata.model)
model.stat.fit3 <-  model.stat
model.stat.fit3$RSE <- summary(fit3)$sigma
model.stat.fit3$`Adjusted R-squared` <- summary(fit3)$adj.r.squared
model.stat.fit3$`F-Statistic` <- summary(fit3)$fstatistic[1]
model.stat.fit3$`any-aliased` <- any(summary(fit3)$aliased)





