# -------------------------------------------------------------------------

# PREPARE DATA ------------------------------------------------------------

# creates new data table with with time series frequency description
statistics <- function(xlist){
  xlist <<- xlist
  
  freq.temp <- data.frame(dažnis = c(1,2,4,12,52), stebėjimų_dažnumas = c('metiniai', 'pusmetiniai', 'ketvirtiniai', 'mėnesiniai', 'savaitiniai'), stringsAsFactors = FALSE)
  
  desc.temp <- data.frame(variable = character(),
                          stebėjimo_pradžia       = character(),
                          stebėjimo_pabaiga       = character(),
                          dažnis                  = numeric(), 
                          vidurkis                = numeric(),
                          standartinis_nuokrypis  = numeric(),
                          mediana                 = numeric(),
                          stebėjimų_skaičius      = numeric(),
                          mažiausia_reikšmė       = numeric(),
                          didžiausia_reikšmė      = numeric(),
                          stringsAsFactors=FALSE)
  
  for (i in 1:length(xlist)) {
    for (j in which(!(names(xlist[[i]]) == 'date'))) {
      variable             <- names(xlist[[i]])[j]
      stebėjimo_pradžia      <- xlist[[i]]$date[1]
      stebėjimo_pabaiga      <- xlist[[i]]$date[length(xlist[[i]]$date)]
      dažnis                 <- xlist[[i]]$date %>% as.character() %>% substr(1,4) %>% table() %>% max()
      vidurkis               <- round(mean(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      standartinis_nuokrypis <- round(sd(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      mediana                <- round(median(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      stebėjimų_skaičius     <- round(length(as.matrix(xlist[[i]][j])), digits = 2)
      mažiausia_reikšmė      <- round(min(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      didžiausia_reikšmė     <- round(max(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      
      new.row <- c(variable, stebėjimo_pradžia, stebėjimo_pabaiga, dažnis, vidurkis, standartinis_nuokrypis, mediana, stebėjimų_skaičius, mažiausia_reikšmė, didžiausia_reikšmė)
      desc.temp[nrow(desc.temp) + 1,] = new.row
    }
  }
  
  desc.temp <- merge(desc.temp, freq.temp, all.x = T)
  
  return(desc.temp)
}


# plot correlation
plot.cor <- function(dd,text=T, plot.title = ""){
  d <- dd
  d$Var1<-factor(d$Var1)
  d$Var2<-factor(d$Var2)
  
  fff <- ggplot(d, aes(x=Var2, y=Var1, fill=value,label=round(value,2))) +
    labs(title = plot.title,
         color=NULL) +  # title and caption
    theme_bw()+
    geom_tile()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x  = element_text(angle=90, vjust=0.5,hjust=1))
  
  if(text){
    fff <- fff+geom_text()
  }
  
  fff <- fff+scale_fill_gradient2(mid="white", high="#CC0000", low="#0066CC", name = "Koeficientas")
  print(fff)
}


# VIF
vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}



















