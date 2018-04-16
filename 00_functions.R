# -------------------------------------------------------------------------

# PREPARE DATA ------------------------------------------------------------

# creates new data table with with time series frequency description
statistics <- function(xlist){
  xlist <<- xlist
  
  freq.temp <- data.frame(freq=c(1,2,4,12,52), frequency_name=c('yearly', 'semiannual', 'quarterly', 'monthly', 'weekly'), stringsAsFactors=FALSE)
  
  desc.temp <- data.frame(variable = character(),
                          min_date = character(),
                          max_date = character(),
                          freq     = numeric(), 
                          mean     = numeric(),
                          sd       = numeric(),
                          median   = numeric(),
                          n        = numeric(),
                          min      = numeric(),
                          max      = numeric(),
                          stringsAsFactors=FALSE)
  
  for (i in 1:length(xlist)) {
    for (j in which(!(names(xlist[[i]]) == 'date'))) {
      variable <- names(xlist[[i]])[j]
      min_date <- xlist[[i]]$date[1]
      max_date <- xlist[[i]]$date[length(xlist[[i]]$date)]
      freq     <- xlist[[i]]$date %>% as.character() %>% substr(1,4) %>% table() %>% max()
      mean     <- round(mean(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      sd       <- round(sd(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      median   <- round(median(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      n        <- round(length(as.matrix(xlist[[i]][j])), digits = 2)
      min      <- round(min(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      max      <- round(max(as.matrix(xlist[[i]][j]), na.rm = T), digits = 2)
      
      new.row <- c(variable, min_date, max_date, freq, mean, sd, median, n, min, max)
      desc.temp[nrow(desc.temp) + 1,] = new.row
    }
  }
  
  desc.temp <- merge(desc.temp, freq.temp, all.x = T)
  
  return(desc.temp)
}






















