# MODEL ------------------------------------------------------------------------

# paruosiam duomenis modeliavimui
alldata.model <- alldata.m[,!(names(alldata.m) %in% c('loan_interest_LTL', 'loan_value_LTL', 'deposit_interest_LTL'))]
diff1.variables <- setdiff(diff1.variables, c('loan_interest_LTL', 'loan_value_LTL', 'deposit_interest_LTL', 'cash', 'deposits', 'ratio'))
alldata.model[1:nrow(alldata.model)-1, diff1.variables] <- lapply(alldata.m[diff1.variables], diff)
alldata.model[nrow(alldata.model),diff1.variables] <- NA

EUR.dates <- c(apply(expand.grid(2015:2017, 1:9),1,paste,collapse=" 0"), apply(expand.grid(2015:2017, 10:12),1,paste,collapse=" "))
alldata.model[alldata.model$date %in% EUR.dates, 'dummy_EUR'] <- 1
alldata.model[is.na(alldata.model$dummy_EUR), 'dummy_EUR'] <- 0

alldata.model <- alldata.model[complete.cases(alldata.model),]

# creating formula and model
lm.regressors1 <- setdiff(names(alldata.model), c('date', 'cash', 'deposits', 'ratio'))
f1 <- as.formula(paste("ratio ~ ", paste(lm.regressors1, collapse=" + ")))
fit1 <- lm(f1, data = alldata.model)
#summary(fit1)
