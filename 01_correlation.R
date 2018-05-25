
# CORRELATION ------------------------------------------------------------------

cor.names <- c("spearman")

alldata.m['santykis'] <- alldata.m$cash/alldata.m$deposits
cor.df <- alldata.m[,!(names(alldata.m) %in% c('date', 'loan_interest_LTL', 'loan_value_LTL', 'deposit_interest_LTL'))]
cor.df <- lapply(cor.df, as.numeric) %>% data.frame()


# calculating correlation
for (i in cor.names) {
  all.cor <- cor(cor.df[,2:ncol(cor.df)], use = 'complete.obs', method = i)
  
  abs.all.cor <- abs(all.cor)
  
  highest.cor <- rowSums(abs.all.cor)[order(rowSums(abs.all.cor), decreasing = TRUE)][1:5] %>% names()
  lowest.cor <- rowSums(abs.all.cor)[order(rowSums(abs.all.cor), decreasing = FALSE)][1:5] %>% names()
  
  assign(paste0("highest.cor.", i), melt(cor(cor.df[,highest.cor], use = 'complete.obs'), variable.factor=FALSE))
  assign(paste0("lowest.cor.", i), melt(cor(cor.df[,lowest.cor], use = 'complete.obs')))
}


# renaming variables
highest.cor.spearman[,c("Var1", "Var2")]  <- lapply(highest.cor.spearman[,c("Var1", "Var2")], as.character)
lowest.cor.spearman[,c("Var1", "Var2")]   <- lapply(lowest.cor.spearman[,c("Var1", "Var2")], as.character)

for (name in unique(highest.cor.spearman$Var1)) {
  highest.cor.spearman[highest.cor.spearman$Var1 == name,"Var1"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
  highest.cor.spearman[highest.cor.spearman$Var2 == name,"Var2"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
}

for (name in unique(lowest.cor.spearman$Var1)) {
  lowest.cor.spearman[lowest.cor.spearman$Var1 == name,"Var1"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
  lowest.cor.spearman[lowest.cor.spearman$Var2 == name,"Var2"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
}


# pagrindiniai, kurie koreliuoja su santykiu daugiausiai

# abs.all.cor <- data.frame(abs(all.cor))['santykis'] %>% t()
# abs.all.cor <- abs.all.cor[,order(abs.all.cor[1,], decreasing = TRUE)] %>% t() %>% data.frame()
# names(abs.all.cor)[1:5]

plot1 <- ggplot(cor.df, aes(x=santykis, y=cash_in_number)) + 
  geom_point() + geom_smooth(method = "loess") +
  labs(subtitle="Subtitle", y="Grynųjų įnešimo operacijų skaičius", x="santykis", title="Counts Plot")
plot2 <- ggplot(cor.df, aes(x=santykis, y=credit_cards)) + 
  geom_point() +  geom_smooth(method = "loess") +
  labs(subtitle="Subtitle", y="Kredito kortelių skaičius", x="santykis", title="Counts Plot")
plot3 <- ggplot(cor.df, aes(x=santykis, y=debit_cards)) + 
  geom_point() +  geom_smooth(method = "loess") +
  labs(subtitle="Subtitle", y="Debeto kortelių skaičius", x="santykis", title="Counts Plot")
plot4 <- ggplot(cor.df, aes(x=santykis, y=unemp_male)) + 
  geom_point() +  geom_smooth(method = "loess") +
  labs(subtitle="Subtitle", y="Vyrų nedarbas", x="santykis", title="Counts Plot")



# correlation test
oldw <- getOption("warn")
options(warn = -1)
test.all.cor <- data.frame(all.cor[,'santykis'])
names(test.all.cor)[1] <- 'ratio.cor'
test.all.cor <- test.all.cor %>% t() %>% data.frame()
for (var.name in unique(names(test.all.cor))) {
  res <- cor.test(alldata.m$santykis, alldata.m[,var.name], alternative = "two.sided", method = "spearman")
  test.all.cor[1,var.name] <- res$p.value
}
options(warn = oldw)

test.all.cor <- test.all.cor[which(test.all.cor[1,]>0.05)]

for (var.name in unique(names(test.all.cor))) {
  names(test.all.cor)[names(test.all.cor) == var.name] <- all.variables.stat[all.variables.stat$variable == var.name,"kintamasis"]
}

# test.all.cor %>% names()
# "emigrants", "payments_out_value", "travel_agencies"
# The p-value of the test is 1.29410^{-10}, which is less than the significance level alpha = 0.05. 
# We can conclude that wt and mpg are significantly correlated with a correlation coefficient of -0.87 
# and p-value of 1.29410^{-10} .


# korelograma tarp labiausiai koreliuojančių su santykiu
highest.cor.ratio <- c("santykis", "cash_in_number", "credit_cards", "debit_cards", "unemp_male", "unemp_female", "tax_gpm", "retail_volume", "deposit_interest_EUR", "tax_excise", "tax_vat")
ratio.cor.df <- cor.df[,highest.cor.ratio]

for (var.name in setdiff(unique(names(ratio.cor.df)), 'santykis')) {
  names(ratio.cor.df)[names(ratio.cor.df) == var.name] <- all.variables.stat[all.variables.stat$variable == var.name,"kintamasis"]
}

corr <- round(cor(ratio.cor.df, use = 'complete.obs', method = "spearman"), 2)

# ggcorrplot(corr, hc.order = TRUE, 
#            type = "lower", 
#            lab = TRUE, 
#            lab_size = 3, 
#            method="circle", 
#            colors = c("tomato2", "white", "springgreen3"), 
#            title="Korelograma tarp labiausiai koreliuojančių su santykiu", 
#            ggtheme=theme_bw)





