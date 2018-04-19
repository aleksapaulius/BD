library(ggplot2)
library(quantmod)
data("economics", package = "ggplot2")

# Compute % Returns
economics$returns_perc <- c(0, diff(economics$psavert)/economics$psavert[-length(economics$psavert)])

library(ggplot2)
theme_set(theme_classic())

# Allow Default X Axis Labels
ggplot(economics, aes(date, returns_perc)) + 
  geom_line() +
  scale_x_date(labels = date_format("%Y %m")) 





autoplot(AirPassengers) + 
  labs(title="AirPassengers") + 
#  scale_x_date(labels = date_format("%Y %m")) +
  theme(plot.title = element_text(hjust=0.5))




library(ggplot2)
library(lubridate)
theme_set(theme_bw())
df <- economics_long[economics_long$variable %in% c("psavert", "uempmed"), ]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]
# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)
# plot
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Time Series of Returns Percentage", 
       subtitle="Drawn from Long Data format", 
       caption="Source: Economics", 
       y="Returns %", 
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  #scale_color_manual(labels = c("psavert", "uempmed"), 
  #                   values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid



library(ggplot2)
library(lubridate)
theme_set(theme_bw())
df <- economics[, c("date", "psavert", "uempmed")]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]
# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)
# plot
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=psavert, col="psavert")) + 
  geom_line(aes(y=uempmed, col="uempmed")) + 
  labs(title="Time Series of Returns Percentage", 
       subtitle="Drawn From Wide Data format", 
       caption="Source: Economics", y="Returns %") +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  scale_color_manual(name="", 
                     values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(panel.grid.minor = element_blank())  # turn off minor grid





library(ggplot2)
library(forecast)
theme_set(theme_classic())

# Subset data
nottem_small <- window(nottem, start=c(1920, 1), end=c(1925, 12))  # subset a smaller timewindow

# Plot
ggseasonplot(AirPassengers) + labs(title="Seasonal plot: International Airline Passengers")
ggseasonplot(nottem_small) + labs(title="Seasonal plot: Air temperatures at Nottingham Castle")



ggseasonplot(ts(data.money$deposits, start = c(1993, 12), frequency = 12))









####
####
####

library(ggplot2)
library(lubridate)
theme_set(theme_bw())
df <- economics_long[economics_long$variable %in% c("psavert", "uempmed"), ]
df <- df[lubridate::year(df$date) %in% c(1967:1981), ]
# labels and breaks for X axis text
brks <- df$date[seq(1, length(df$date), 12)]
lbls <- lubridate::year(brks)
# plot
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Time Series of Returns Percentage", 
       subtitle="Drawn from Long Data format", 
       caption="Source: Economics", 
       y="Returns %", 
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  scale_color_manual(labels = c("psavert", "uempmed"), 
                     values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid


df <- melt(data.tax, id='date')
as.Date(as.yearmon(df$date))
format(as.yearmon(df$date), "%Y %m")
df$date <- as.Date(as.yearmon(df$date))
brks <- df$date[seq(1, length(df$date), 1)]
lbls <- lubridate::year(brks)
ggplot(df, aes(x=date)) + 
  geom_line(aes(y=value, col=variable)) + 
  labs(title="Time Series of Returns Percentage", 
       subtitle="Drawn from Long Data format", 
       caption="Source: Economics", 
       y="Returns %", 
       color=NULL) +  # title and caption
  scale_x_date(labels = lbls, breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid








