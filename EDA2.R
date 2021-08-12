library(forecast)
library(aTSA)

MWDat <- read.csv("C:/Users/Wali/Downloads/original.csv")
head(MWDat$Total.Energy.Use.from.Electricity..MW.)
length(MWDat$Total.Energy.Use.from.Electricity..MW.)

plot(MWDat$Total.Energy.Use.from.Electricity..MW.)

values = MWDat$Total.Energy.Use.from.Electricity..MW.

MAE <- function(true, pred){
  if (length(true) != length(pred)){
    return (-1)
  }
  return ((1/length(true))*sum(abs(true - pred)))
}

training_data = values[1:113880]
test_data = values[113881:122640]

length(training_data) + length(test_data)

ser1 <- diff(training_data, lag = 24, differences = 1)
ser2 <- diff(ser1, lag = 8760, differences=1)
tser <- ts(ser2)
#Confirm stationarity with dickeyfuller test
#The nullhypothesis is that the time series is non-stationary
adf.test(tser)
plot(tser)
#indeed the plot resembles white noise

#Obtain the best arima model after differencing for the training data
model1 <- auto.arima(tser)
leftover = 8760
f1 <- forecast:: forecast(model1, h=leftover)
plot(f1, xlab = 'Time', ylab = 'Double Differenced MW')

#holt winters 
#fcast <- dshw(training_data, period1 = 24, period2 = 24*365)
fcast <- dshw(training_data, period1 = 24, period2 = 24*365,h =8760)
plot(fcast$mean)
#Get MAE
MAE(test_data, fcast$mean[1:8760])


