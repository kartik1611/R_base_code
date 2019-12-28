rm(list = ls(all.names = TRUE))
# setwd("F:/AA_Clases/20160417")
setwd("F:/AA_Clases/20160806_CSE7202_Activity_TimeSeries_Batch18")
# setwd("F:/AA_Clases/20160626_TimeSeries_Batch17/Time_Series")
Data <- read.csv("monthly-car-sales-in-quebec-1960.csv",header = T)

TrainData <- Data[1:84,]

library(tseries)
TSData <- ts(TrainData[,2],start = c(1960,1),frequency = 12)

# colnames(TSData)
# TSData[]
# Plotting TS
plot.ts(TSData)
# Decomposing TS
DecomposeTimeSeries <- decompose(TSData)
str(DecomposeTimeSeries$seasonal)

DecomposeTimeSeries$seasonal[]

plot(DecomposeTimeSeries)

DecomposeTimeSeries$seasonal[,1]
DecomposeTimeSeries$seasonal <- as.data.frame(DecomposeTimeSeries$seasonal)

str(DecomposeTimeSeries$seasonal)

DecomposeTimeSeries$seasonal

Decompose <- as.matrix(DecomposeTimeSeries$seasonal)
# myTs <- matrix(DecomposeTimeSeries$seasonal,nrow = 7,ncol = 12,byrow = T)
# colnames(myTs) <- month.name
# # myTs <- as.data.frame(myTs)
# # myTs$January
# myTs[1,]


library(forecast)
#Holt Winters
# Creating Holts Model
HoltWinterModel1 <- HoltWinters(TSData,alpha = TRUE,beta = FALSE,gamma = TRUE)
HoltWinterModel1$fitted
plot(HoltWinterModel1)
# Forecasting Holt's Model

HoltPrediction <- forecast(HoltWinterModel1,h=24)
Data$Monthly.car.sales.in.Quebec.1960.1968[86:nrow(Data)-1]
#Calculating the Error metric for Holts model
mean(abs(data.frame(HoltPrediction)$Point.Forecast-
Data$Monthly.car.sales.in.Quebec.1960.1968[86:nrow(Data)-1]))
# 1770.458

# AutoAr


par(mfrow=c(1,2))
# To Forecast using ARIMA arriving at p,d,q value using the ACF,PACF plots
acf(TSData)
pacf(TSData)

FirstOrderTS <- diff(TSData,differences = 1)
SecondOrderTS <- diff(TSData,differences = 2)
ThirdOrderTS <- diff(TSData,differences = 3)

acf(FirstOrderTS)
pacf(FirstOrderTS)

acf(SecondOrderTS)
pacf(SecondOrderTS)

acf(ThirdOrderTS)
pacf(ThirdOrderTS)
plot.ts(FirstOrderTS)
plot.ts(SecondOrderTS)
plot.ts(ThirdOrderTS)
# d=3,p=7,q=5
# Running the model ARIMA
ArimaFirstOrder <- arima(FirstOrderTS,order = c(1,1,1))
# Forecasting using ARIMA
ArimaForecast <- forecast.Arima(ArimaFirstOrder,h=24)
# Calculating the error metrics using 
mean(abs(data.frame(ArimaForecast)$Point.Forecast - Data$Monthly.car.sales.in.Quebec.1960.1968[86:nrow(Data)-1]))
