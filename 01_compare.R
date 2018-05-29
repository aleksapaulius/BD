
# COMPARE ----------------------------------------------------------------------

ratio.stat <- data.frame(date = 2005:2017,
                         LSD = c(0.311, 0.306, 0.297, 0.291, 0.296, 0.297, 0.290, 0.285, 0.280, 0.271, 0.258, NA, NA), 
                         LLRI = c(0.21, 0.21, 0.18, 0.18, 0.23, 0.28, 0.27, 0.27, 0.26, 0.25, 0.24, NA, NA), 
                         FS = c(0.144, 0.129, 0.123, 0.125, 0.164, 0.149, 0.147, 0.148, 0.150, NA, NA, NA, NA))

my.ratio <- alldata.m[,c('date', 'santykis')]
my.ratio$date <- substr(my.ratio$date, 1, 4)
my.ratio <- aggregate(my.ratio$santykis, list(my.ratio$date), mean)
names(my.ratio) <- c('date', 'PPM')
my.ratio$PPM <- round(my.ratio$PPM, digits = 3)

ratio.stat <- merge(ratio.stat, my.ratio, all = T)
ratio.stat[,c('LSD', 'LLRI', 'FS', 'PPM')] <- ratio.stat[,c('LSD', 'LLRI', 'FS', 'PPM')] * 100



























