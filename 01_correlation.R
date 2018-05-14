
# CORRELATION ------------------------------------------------------------------

cor.names <- c("spearman", "pearson")

# calculating correlation
for (i in cor.names) {
  all.cor <- cor(alldata.m[,2:ncol(alldata.m)], use = 'complete.obs', method = i)
  
  highest.cor <- names(rowSums(all.cor)[order(rowSums(all.cor), decreasing = TRUE)][1:5])
  lowest.cor <- names(rowSums(all.cor)[order(rowSums(all.cor), decreasing = FALSE)][1:5])
  
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




# m.df <- melt(cor(alldata.m[,highest.cor], use = 'complete.obs'))
# qplot(x=Var1, y=Var2, data=m.df, fill=value, geom="tile")
# m.df <- melt(cor(alldata.m[,lowest.cor], use = 'complete.obs'))
# qplot(x=Var1, y=Var2, data=m.df, fill=value, geom="tile")

# plot.cor(lowest.cor.spearman)









