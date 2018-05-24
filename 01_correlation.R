
# CORRELATION ------------------------------------------------------------------

cor.names <- c("spearman", "pearson")

alldata.m['ratio'] <- alldata.m$cash/alldata.m$deposits


# calculating correlation
for (i in cor.names) {
  all.cor <- cor(alldata.m[,2:ncol(alldata.m)], use = 'complete.obs', method = i)
  
  abs.all.cor <- abs(all.cor)
  
  highest.cor <- rowSums(abs.all.cor)[order(rowSums(abs.all.cor), decreasing = TRUE)][1:5] %>% names()
  lowest.cor <- rowSums(abs.all.cor)[order(rowSums(abs.all.cor), decreasing = FALSE)][1:5] %>% names()
  
  assign(paste0("highest.cor.", i), melt(cor(alldata.m[,highest.cor], use = 'complete.obs'), variable.factor=FALSE))
  assign(paste0("lowest.cor.", i), melt(cor(alldata.m[,lowest.cor], use = 'complete.obs')))
}


# renaming variables
highest.cor.spearman[,c("Var1", "Var2")]  <- lapply(highest.cor.spearman[,c("Var1", "Var2")], as.character)
highest.cor.pearson[,c("Var1", "Var2")]   <- lapply(highest.cor.pearson[,c("Var1", "Var2")], as.character)
lowest.cor.spearman[,c("Var1", "Var2")]   <- lapply(lowest.cor.spearman[,c("Var1", "Var2")], as.character)
lowest.cor.pearson[,c("Var1", "Var2")]    <- lapply(lowest.cor.pearson[,c("Var1", "Var2")], as.character)

for (name in unique(highest.cor.spearman$Var1)) {
  highest.cor.spearman[highest.cor.spearman$Var1 == name,"Var1"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
  highest.cor.spearman[highest.cor.spearman$Var2 == name,"Var2"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
}

for (name in unique(highest.cor.pearson$Var1)) {
  highest.cor.pearson[highest.cor.pearson$Var1 == name,"Var1"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
  highest.cor.pearson[highest.cor.pearson$Var2 == name,"Var2"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
}

for (name in unique(lowest.cor.spearman$Var1)) {
  lowest.cor.spearman[lowest.cor.spearman$Var1 == name,"Var1"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
  lowest.cor.spearman[lowest.cor.spearman$Var2 == name,"Var2"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
}

for (name in unique(lowest.cor.pearson$Var1)) {
  lowest.cor.pearson[lowest.cor.pearson$Var1 == name,"Var1"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
  lowest.cor.pearson[lowest.cor.pearson$Var2 == name,"Var2"] <- all.variables.stat[all.variables.stat$variable == name,"kintamasis"]
}


# correlation test
test.all.cor <- data.frame(all.cor[,'ratio'])
names(test.all.cor)[1] <- 'ratio.cor'
test.all.cor <- test.all.cor %>% t() %>% data.frame()
for (var.name in unique(names(test.all.cor))) {
  res <- cor.test(alldata.m$ratio, alldata.m[,var.name], alternative = "two.sided", method = "spearman")
  test.all.cor[1,var.name] <- res$p.value
}









# pagrindiniai 
ggplot(alldata.m, aes(x=ratio, y=credit_cards)) + geom_point()

ggplot(alldata.m, aes(x=ratio, y=cash_in_number)) + geom_point()

ggplot(alldata.m, aes(x=ratio, y=regulation_institutions)) + geom_point()

ggplot(alldata.m, aes(x=ratio, y=unemp_male)) + geom_point()



g <- ggplot(alldata.m, aes(ratio, credit_cards))
g + geom_count(col="tomato3", show.legend=F) +
  labs(subtitle="Subtitle", 
       y="ratio", 
       x="credit_cards", 
       title="Counts Plot")

g <- ggplot(alldata.m, aes(ratio, credit_cards))
g + geom_point() +
  labs(subtitle="Subtitle", 
       y="ratio", 
       x="credit_cards", 
       title="Counts Plot")



corr <- round(cor(cor(alldata.m[,highest.cor], use = 'complete.obs', method = "spearman")), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)






