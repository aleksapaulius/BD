# -------------------------------------------------------------------------

# PREPARE DATA ------------------------------------------------------------

# creates new data table with with time series frequency description
frequency.desc <- function(xlist){
  xlist <<- xlist
  
  freq.temp <- data.frame(freq=c(1,2,4,12,52), frequency_name=c('yearly', 'semiannual', 'quarterly', 'monthly', 'weekly'), stringsAsFactors=FALSE)
  
  all.variables.desc <- data.frame(variable=character(),
                                   min_date=character(),
                                   max_date=character(),
                                   freq=numeric(), stringsAsFactors=FALSE)
  
  for (i in 1:length(xlist)) {
    for (j in which(!(names(xlist[[i]]) == 'date'))) {
      variable <- names(xlist[[i]])[j]
      min_date <- xlist[[i]]$date[1]
      max_date <- xlist[[i]]$date[length(xlist[[i]]$date)]
      freq <- xlist[[i]]$date %>% as.character() %>% substr(1,4) %>% table() %>% max()
      new.row <- c(variable, min_date, max_date, freq)
      all.variables.desc[nrow(all.variables.desc) + 1,] = new.row
    }
  }
  
  all.variables.desc <- merge(all.variables.desc, freq.temp, all.x = T)
  
  return(all.variables.desc)
}






















