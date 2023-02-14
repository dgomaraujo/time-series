rm(list=list=ls())
library(fpp2)
data<-read.csv
Y <- ts(data[,2], start=c(1992,1), frequency = 12)

#################################
# Preliminary analysis
#################################

# Time plot
autoplot(Y) +
  ggtitle("Time Plot: Real US Retail Sales Per Day") +
  ylab("Millions of 2017 Dollars")

# Data has a strong trend. Investigate transformations
# Tirar a primeira diferenca

DY <- diff(Y)

# Time plot of differenced data
autoplot(DY) +
  ggtitle("Time Plot: Change in Real US Retail Sales Per Day") +
  ylab("Millions of 2017 Dollars")

# Series appears trend-stationary, use to investigate seasonality
ggseasonplot(DY) +
  ggtitle("Seasonal Plot: Change in Daily Retail Sales") +
  ylab("Millions of 2017 Dollars")

# Lets look at another seasonal plot, the subseries plot
ggsubseriesplot(DY)

######################################################
# Our series, Y has trend and seasonality
# To remove the trend, we take the first difference
# The first differenced series still has seasonality
#
# Forecast with various methods
######################################################

#######
# Use a benchmark method to forecast
# y_t = y_[t-s] + e_t
#######
fit <- snaive(DY) #residual SD = 287.06
print(summary(fit))
checkresiduals(fit)

#######
# fit ets method
#######
fit_ets <- ets(Y) #residual sd = 212.7824
print(summary(fit_ets))
checkresiduals(fit_ets)

#######
# fit with arima model
#######
fit_arima <- auto.arima(venlafaxina_ts,d=1,D=1,stepwise=FALSE,approximation=FALSE,trace=TRUE) #residual SD = 197.8
print(summary(fit_arima))
checkresiduals(fit_arima)

#######
# forecast with arima model
#######
fcst <- forecast (fit_arima, h=12)
autoplot(fcst, include = 60)
print(summary(fcst))
