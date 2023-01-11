#Install packages

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

New_EU <- Combined_Class %>% select(1:2, 5,7)
Reduced_New_EU <- filter(New_EU, Reformation_Class == "New EU" )
head(as.data.frame(Reduced_New_EU))
str(Reduced_New_EU)
#Define the obj of the Time Series analysis.
Reduced_New_EU_1 <- Reduced_New_EU[ ,c('Percentage_Total_Unemployment',
                                     'Year')]
Reduced_New_EU_1

#Reading Time Series

TotalUnemployment_New <-Reduced_New_EU_1[,c('Percentage_Total_Unemployment')]
TotalUnemployment_New
TotalUnemployment_Newseries <- ts(TotalUnemployment_New,frequency = 12, start = c(2012), end = c(2021))
TotalUnemployment_Newseries


#Ploting Time series. 
plot.ts(TotalUnemployment_Newseries)

#Transform the time series by calculating the natural log of the original data

logTotalUnemployment_Newseries <- log(TotalUnemployment_Newseries)
plot.ts(logTotalUnemployment_Newseries)


#Decompising Time Series

####### Decomposing Seasonal Data ######

TotalUnemployment_Newseriescomponents <- decompose(TotalUnemployment_Newseries)

TotalUnemployment_Newseriescomponents$seasonal 

plot(TotalUnemployment_Newseriescomponents)

#Seasonally Adjusting
TotalUnemployment_Newseriescomponents <- decompose(TotalUnemployment_Newseries)
TotalUnemployment_Newseriesseasonallyadjusted <- TotalUnemployment_Newseries - TotalUnemployment_Newseriescomponents$seasonal

plot(TotalUnemployment_Newseriesseasonallyadjusted)

#Predictive model for the log of yearly unemployment in EU using Holtwinters
logTotalUnemployment_Newseries <- log(TotalUnemployment_Newseries)
TotalUnemployment_Newseriesforecasts <- HoltWinters(logTotalUnemployment_Newseries)
TotalUnemployment_Newseriesforecasts
TotalUnemployment_Newseriesforecasts$SSE

plot(TotalUnemployment_Newseriesforecasts)


#Forecasts Jan 2021 to Dec 2025
TotalUnemployment_Newseriesforecasts2 <- forecast(TotalUnemployment_Newseriesforecasts, h=48)
TotalUnemployment_Newseriesforecasts2


plot(TotalUnemployment_Newseriesforecasts2)


acf(TotalUnemployment_Newseriesforecasts2$residuals, lag.max=20 , na.action = na.pass)
Box.test(TotalUnemployment_Newseriesforecasts2$residuals, lag=20, type="Ljung-Box")


plot.ts(TotalUnemployment_Newseriesforecasts2$residuals) # make time series plot

TotalUnemployment_Newseriesforecasts2$residuals <- TotalUnemployment_Newseriesforecasts2$residuals [!is.na(TotalUnemployment_Newseriesforecasts2$residuals)]
plotForecastErrors(TotalUnemployment_Newseriesforecasts2$residuals)
mean(TotalUnemployment_Newseriesforecasts2$residuals)

#Forcasting using Arima 
#Getting the model
acf(TotalUnemployment_Newseries, lag.max=20, plot=FALSE) # get the values

pacf(TotalUnemployment_Newseries, lag.max=20)
pacf(TotalUnemployment_Newseries, lag.max=20, plot=FALSE)

auto.arima(TotalUnemployment_New)

auto.arima(TotalUnemployment_New,ic='bic')
####### Forecasting Using an ARIMA Model #######

TotalUnemployment_Newseriesarima <- arima(TotalUnemployment_Newseries, order=c(1,0,2))
TotalUnemployment_Newseriesarima 

TotalUnemployment_Newseriesforecasts1 <- forecast(TotalUnemployment_Newseriesarima , h=48)
TotalUnemployment_Newseriesforecasts1

plot(TotalUnemployment_Newseriesforecasts1)

acf(TotalUnemployment_Newseriesforecasts1$residuals, lag.max=20)
Box.test(TotalUnemployment_Newseriesforecasts1$residuals, lag=20, type="Ljung-Box")

plot.ts(TotalUnemployment_Newseriesforecasts1$residuals)

#Plot forecasterror

plotForecastErrors(TotalUnemployment_Newseriesforecasts1$residuals) 
mean(TotalUnemployment_Newseriesforecasts1$residuals)




