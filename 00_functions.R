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























