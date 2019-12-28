rm(list=ls(all=TRUE))
# setwd("C:/Users/pavan85/Downloads/Batch13_7202c_RcodeForGNQ_Datasets/RcodeForGNQ_Datasets")
# setwd("F:/AA_Clases/20160626_TimeSeries_Batch17/Data&Rcode")
# setwd("F:/AA_Clases/20160626_TimeSeries_Batch17/Data&Rcode/20160626_CSE7202_Activity_TimeSeries_Batch17")
setwd("F:/AA_Clases/20160806_CSE7202_Activity_TimeSeries_Batch18")
data1 <-read.csv("DeliveryRate.csv")

data = data1$DeliveryRate[1:32]
# Creating the Time series Object
datatimeseries <- ts(data, 
                     frequency=12, 
                     start=c(2013,1))
datatimeseries

#Plotting
plot.ts(datatimeseries)


#Decomposition

datatimeseriescomponents <- 
  decompose(datatimeseries)
##Decomposition to estimate the trend, seasonal and irregular components of this time series
# no seasonality and time series has no or less than 2 periods
plot(datatimeseriescomponents)


#ACF and PACF of real world data

par(mfrow=c(1,2))
# plot.ts(datatimeseries)
acf(datatimeseries, lag.max=20)
pacf(datatimeseries, lag.max=20)
library(forecast)
# ndiffs(datatimeseries)
# 
# ndiffs(datatimeseries)
# ARIMA
#Differencing the time series and make it stationary
par(mfrow=c(1,4))
plot.ts(datatimeseries,main="Actual Data")
timeseriesdiff1 <-  diff(datatimeseries, differences=1)
plot.ts(timeseriesdiff1,main="Data with one difference")
#Differencing the time series and make it stationary
timeseriesdiff2 <- diff(datatimeseries, differences=2)
plot.ts(timeseriesdiff2,main="Data with two differences")
#Differencing the time series and make it stationary
timeseriesdiff3 <- diff(datatimeseries, differences=3)
plot.ts(timeseriesdiff3,main="Data with three differences")

#Plotting acf and pacf plots for differenced time series once
par(mfrow=c(1,2))
acf(timeseriesdiff1, lag.max=20)
pacf(timeseriesdiff1, lag.max=20)
acf(timeseriesdiff1, lag.max=20,plot=F)
pacf(timeseriesdiff1, lag.max=20,plot=F)

#Plotting acf and pacf plots for for differenced time series twice
par(mfrow=c(1,2))
acf(timeseriesdiff2, lag.max=20)
pacf(timeseriesdiff2, lag.max=20)
acf(timeseriesdiff2, lag.max=20,plot=F)
pacf(timeseriesdiff2, lag.max=20,plot=F)

#Plotting acf and pacf plots for for differenced time series thrice
par(mfrow=c(1,2))
acf(timeseriesdiff3, lag.max=20)
pacf(timeseriesdiff3, lag.max=20)
acf(timeseriesdiff3, lag.max=20,plot=F)
pacf(timeseriesdiff3, lag.max=20,plot=F)

# Fitting the Arima Model by change p,d,q values in order = c(p,d,q)

fit_arima<- arima(datatimeseries, order=c(1,0,1)) 
fit_arima

# ( 5  marks)
library("forecast") 
# Forecasting the pick rate for the next 4 months 
arimatimeseriesforecasts <- forecast.Arima(fit_arima, h=4)
arimatimeseriesforecasts
arimatimeseriesforecasts$Forecast
plot.forecast(arimatimeseriesforecasts)
summary(fit_arima)
forecasts = (data.frame(forecast.Arima(fit_arima, h=4)))$Point.Forecast
mean(abs(forecasts-data1$DeliveryRate[33:36]))  # MAE = 4.45  on test
# MAE 3.924057 on train data
 
# ##Auto Arima
# 
# autoarima = auto.arima(datatimeseries)
# autoarima
# arima.errors(autoarima)
# summary(autoarima)

# Holt Winters
plot(datatimeseriescomponents)
HoltsModel <- HoltWinters(datatimeseries,beta = TRUE,gamma = TRUE)
HoltsModel$fitted
plot(HoltsModel)
HoltsModel$SSE

HoltsModel <- HoltWinters(datatimeseries)
HoltsModel$fitted
plot(HoltsModel)
HoltsModel$SSE
library(forecast)

HoltsPredict <- 
  forecast.HoltWinters(HoltsModel, 
                       h=4)
MAEHoltWinters <- mean(abs(HoltsPredict$fitted-(data.frame(HoltsPredict))$Point.Forecast))

plot.forecast(HoltsPredict)
plot.forecast(HoltsPredict, 
              shadecols=terrain.colors(3))
plot.forecast(HoltsPredict,
              shadecols="oldstyle")
