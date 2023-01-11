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

UnderDeveloped_EU <- Combined_Class %>% select(1:2, 6,7)
Reduced_UnderDeveloped_EU <- filter(UnderDeveloped_EU, Development_Class == "UnderDeveloped Countries" )
head(as.data.frame(Reduced_UnderDeveloped_EU))
str(Reduced_UnderDeveloped_EU)
#Define the obj of the Time Series analysis.
Reduced_UnderDeveloped_EU_1 <- Reduced_UnderDeveloped_EU[ ,c('Percentage_Total_Unemployment',
                                                   'Year')]
Reduced_UnderDeveloped_EU_1

#Reading Time Series

TotalUnemployment_UnderDeveloped <-Reduced_UnderDeveloped_EU_1[,c('Percentage_Total_Unemployment')]
TotalUnemployment_UnderDeveloped
TotalUnemployment_UnderDevelopedseries <- ts(TotalUnemployment_UnderDeveloped,frequency = 12, start = c(2012), end = c(2021))
TotalUnemployment_UnderDevelopedseries


#Ploting Time series. 
plot.ts(TotalUnemployment_UnderDevelopedseries)

#Transform the time series by calculating the natural log of the original data

logTotalUnemployment_UnderDevelopedseries <- log(TotalUnemployment_UnderDevelopedseries)
plot.ts(logTotalUnemployment_UnderDevelopedseries)


#Decompising Time Series

####### Decomposing Seasonal Data ######

TotalUnemployment_UnderDevelopedseriescomponents <- decompose(TotalUnemployment_UnderDevelopedseries)

TotalUnemployment_UnderDevelopedseriescomponents$seasonal 

plot(TotalUnemployment_UnderDevelopedseriescomponents)

#Seasonally Adjusting
TotalUnemployment_UnderDevelopedseriescomponents <- decompose(TotalUnemployment_UnderDevelopedseries)
TotalUnemployment_UnderDevelopedseriesseasonallyadjusted <- TotalUnemployment_UnderDevelopedseries - TotalUnemployment_UnderDevelopedseriescomponents$seasonal

plot(TotalUnemployment_UnderDevelopedseriesseasonallyadjusted)

#Predictive model for the log of yearly unemployment in EU using Holtwinters
logTotalUnemployment_UnderDevelopedseries <- log(TotalUnemployment_UnderDevelopedseries)
TotalUnemployment_UnderDevelopedseriesforecasts <- HoltWinters(logTotalUnemployment_UnderDevelopedseries)
TotalUnemployment_UnderDevelopedseriesforecasts
TotalUnemployment_UnderDevelopedseriesforecasts$SSE

plot(TotalUnemployment_UnderDevelopedseriesforecasts)


#Forecasts Jan 2021 to Dec 2025
TotalUnemployment_UnderDevelopedseriesforecasts2 <- forecast(TotalUnemployment_UnderDevelopedseriesforecasts, h=48)
TotalUnemployment_UnderDevelopedseriesforecasts2


plot(TotalUnemployment_UnderDevelopedseriesforecasts2)


acf(TotalUnemployment_UnderDevelopedseriesforecasts2$residuals, lag.max=20 , na.action = na.pass)
Box.test(TotalUnemployment_UnderDevelopedseriesforecasts2$residuals, lag=20, type="Ljung-Box")


plot.ts(TotalUnemployment_UnderDevelopedseriesforecasts2$residuals) # make time series plot

TotalUnemployment_UnderDevelopedseriesforecasts2$residuals <- TotalUnemployment_UnderDevelopedseriesforecasts2$residuals [!is.na(TotalUnemployment_UnderDevelopedseriesforecasts2$residuals)]
plotForecastErrors(TotalUnemployment_UnderDevelopedseriesforecasts2$residuals)
mean(TotalUnemployment_UnderDevelopedseriesforecasts2$residuals)

#Forcasting using Arima 
#Getting the model
acf(TotalUnemployment_UnderDevelopedseries, lag.max=20, plot=FALSE) # get the values

pacf(TotalUnemployment_UnderDevelopedseries, lag.max=20)
pacf(TotalUnemployment_UnderDevelopedseries, lag.max=20, plot=FALSE)

auto.arima(TotalUnemployment_UnderDeveloped)

auto.arima(TotalUnemployment_Developed,ic='bic')
####### Forecasting Using an ARIMA Model #######

TotalUnemployment_UnderDevelopedseriesarima <- arima(TotalUnemployment_UnderDevelopedseries, order=c(1,0,0))
TotalUnemployment_UnderDevelopedseriesarima 

TotalUnemployment_UnderDevelopedseriesforecasts1 <- forecast(TotalUnemployment_UnderDevelopedseriesarima , h=48)
TotalUnemployment_UnderDevelopedseriesforecasts1

plot(TotalUnemployment_UnderDevelopedseriesforecasts1)

acf(TotalUnemployment_UnderDevelopedseriesforecasts1$residuals, lag.max=20)
Box.test(TotalUnemployment_UnderDevelopedseriesforecasts1$residuals, lag=20, type="Ljung-Box")

plot.ts(TotalUnemployment_UnderDevelopedseriesforecasts1$residuals)

#Plot forecasterror

plotForecastErrors(TotalUnemployment_UnderDevelopedseriesforecasts1$residuals) 
mean(TotalUnemployment_UnderDevelopedseriesforecasts1$residuals)



