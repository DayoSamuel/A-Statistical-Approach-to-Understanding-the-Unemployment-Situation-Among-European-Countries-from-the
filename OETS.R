install.packages("TTR")
install.packages("forecast")
library(corrplot)
library(caret)
library("TTR")
library("forecast")


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4 
  mysd	<- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}


library(tidyverse)
library(dplyr)

# importing data
# set the correct working directory to capture data
Combined_Class <-read.csv("Combined_Class.csv")

Old_EU <- Combined_Class %>% select(1:2, 5,7)
Reduced_Old_EU <- filter(Old_EU, Reformation_Class == "Old EU" )
head(as.data.frame(Reduced_Old_EU))
str(Reduced_Old_EU)
#Define the obj of the Time Series analysis.
Reduced_Old_EU_1 <- Reduced_Old_EU[ ,c('Percentage_Total_Unemployment',
                                       'Year')]
Reduced_Old_EU_1

#Reading Time Series

TotalUnemployment_Old <-Reduced_Old_EU_1[,c('Percentage_Total_Unemployment')]
TotalUnemployment_Old
TotalUnemployment_Oldseries <- ts(TotalUnemployment_Old,frequency = 12, start = c(2012), end = c(2021))
TotalUnemployment_Oldseries


#Ploting Time series. 
plot.ts(TotalUnemployment_Oldseries)

#Transform the time series by calculating the natural log of the original data

logTotalUnemployment_Oldseries <- log(TotalUnemployment_Oldseries)
plot.ts(logTotalUnemployment_Oldseries)


#Decompising Time Series

####### Decomposing Seasonal Data ######

TotalUnemployment_Oldseriescomponents <- decompose(TotalUnemployment_Oldseries)

TotalUnemployment_Oldseriescomponents$seasonal 

plot(TotalUnemployment_Oldseriescomponents)

#Seasonally Adjusting
TotalUnemployment_Oldseriescomponents <- decompose(TotalUnemployment_Oldseries)
TotalUnemployment_Oldseriesseasonallyadjusted <- TotalUnemployment_Oldseries - TotalUnemployment_Oldseriescomponents$seasonal

plot(TotalUnemployment_Oldseriesseasonallyadjusted)

#Predictive model for the log of yearly unemployment in EU using Holtwinters
logTotalUnemployment_Oldseries <- log(TotalUnemployment_Oldseries)
TotalUnemployment_Oldseriesforecasts <- HoltWinters(logTotalUnemployment_Oldseries)
TotalUnemployment_Oldseriesforecasts
TotalUnemployment_Oldseriesforecasts$SSE

plot(TotalUnemployment_Oldseriesforecasts)


#Forecasts Jan 2021 to Dec 2025
TotalUnemployment_Oldseriesforecasts2 <- forecast(TotalUnemployment_Oldseriesforecasts, h=48)
TotalUnemployment_Oldseriesforecasts2


plot(TotalUnemployment_Oldseriesforecasts2)


acf(TotalUnemployment_Oldseriesforecasts2$residuals, lag.max=20 , na.action = na.pass)
Box.test(TotalUnemployment_Oldseriesforecasts2$residuals, lag=20, type="Ljung-Box")


plot.ts(TotalUnemployment_Oldseriesforecasts2$residuals) # make time series plot

TotalUnemployment_Oldseriesforecasts2$residuals <- TotalUnemployment_Oldseriesforecasts2$residuals [!is.na(TotalUnemployment_Oldseriesforecasts2$residuals)]
plotForecastErrors(TotalUnemployment_Oldseriesforecasts2$residuals)
mean(TotalUnemployment_Oldseriesforecasts2$residuals)

#Forcasting using Arima 
#Getting the model
acf(TotalUnemployment_Oldseries, lag.max=20, plot=FALSE) # get the values

pacf(TotalUnemployment_Oldseries, lag.max=20)
pacf(TotalUnemployment_Oldseries, lag.max=20, plot=FALSE)

auto.arima(TotalUnemployment_Old)

auto.arima(TotalUnemployment_Old,ic='bic')
####### Forecasting Using an ARIMA Model #######

TotalUnemployment_Oldseriesarima <- arima(TotalUnemployment_Oldseries, order=c(1,0,1))
TotalUnemployment_Oldseriesarima 

TotalUnemployment_Oldseriesforecasts1 <- forecast(TotalUnemployment_Oldseriesarima , h=48)
TotalUnemployment_Oldseriesforecasts1

plot(TotalUnemployment_Oldseriesforecasts1)

acf(TotalUnemployment_Oldseriesforecasts1$residuals, lag.max=20)
Box.test(TotalUnemployment_Oldseriesforecasts1$residuals, lag=20, type="Ljung-Box")

plot.ts(TotalUnemployment_Oldseriesforecasts1$residuals)

#Plot forecasterror

plotForecastErrors(TotalUnemployment_Oldseriesforecasts1$residuals) 
mean(TotalUnemployment_Oldseriesforecasts1$residuals)



