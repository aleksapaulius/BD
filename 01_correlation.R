
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


tileg(waste.cor.all.spearman)
tileg(waste.cor.all.pearson)

tileg(cor(alldata.m[,highest.cor], use = 'complete.obs'))


tileg <- function(dd,text=T){
  dd  <- data.frame(dd)
  lvl1 <- colnames(dd)
  
  lvl2 <- rownames(dd)
  
  dd <- cbind(dd,names=lvl2)
  d <- melt(dd,id.vars="names")
  colnames(d ) <- c("var1","var2","value")
  
  d$var1<-factor(d$var1,levels=rev(lvl2))
  d$var2<-factor(d$var2,levels=lvl1)
  
  if(all(is.na(d$value))){
    df <- data.frame()
    print(ggplot(df)+ xlim(-5, 5) + ylim(-5, 5)+theme_bw()+xlab('')+ylab("")+
            annotate("text", label = "No Data", x = 0, y = 0, size = 8, colour = "Black")+
            theme(axis.ticks = element_blank(), axis.text = element_blank()))
    
  }else{
    fff <- ggplot(d, aes(x=var2, y=var1, fill=value,label=round(value,2))) +
      theme_bw()+
      geom_tile()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x  = element_text(angle=90, vjust=0.5,hjust=1))
    
    if(text){
      fff <- fff+geom_text()
    }
    
    if(any(sapply(d$value<0,isTRUE))){
      fff <- fff+scale_fill_gradient2(mid="white", high="#CC0000", low="#0066CC")
    }else{
      fff <- fff+scale_fill_gradient2(mid="yellow", high="#FF3333", low="#33CC00",midpoint=mean(range(d$value,na.rm=T),na.rm=T))
    }
    print(fff)
  }
}







