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

Developed_EU <- Combined_Class %>% select(1:2, 6,7)
Reduced_Developed_EU <- filter(Developed_EU, Development_Class == "Developed Countries" )
head(as.data.frame(Reduced_Developed_EU))
str(Reduced_Developed_EU)
#Define the obj of the Time Series analysis.
Reduced_Developed_EU_1 <- Reduced_Developed_EU[ ,c('Percentage_Total_Unemployment',
                                       'Year')]
Reduced_Developed_EU_1

#Reading Time Series

TotalUnemployment_Developed <-Reduced_Developed_EU_1[,c('Percentage_Total_Unemployment')]
TotalUnemployment_Developed
TotalUnemployment_Developedseries <- ts(TotalUnemployment_Developed,frequency = 12, start = c(2012), end = c(2021))
TotalUnemployment_Developedseries


#Ploting Time series. 
plot.ts(TotalUnemployment_Developedseries)

#Transform the time series by calculating the natural log of the original data

logTotalUnemployment_Developedseries <- log(TotalUnemployment_Developedseries)
plot.ts(logTotalUnemployment_Developedseries)


#Decompising Time Series

####### Decomposing Seasonal Data ######

TotalUnemployment_Developedseriescomponents <- decompose(TotalUnemployment_Developedseries)

TotalUnemployment_Developedseriescomponents$seasonal 

plot(TotalUnemployment_Developedseriescomponents)

#Seasonally Adjusting
TotalUnemployment_Developedseriescomponents <- decompose(TotalUnemployment_Developedseries)
TotalUnemployment_Developedseriesseasonallyadjusted <- TotalUnemployment_Developedseries - TotalUnemployment_Developedseriescomponents$seasonal

plot(TotalUnemployment_Developedseriesseasonallyadjusted)

#Predictive model for the log of yearly unemployment in EU using Holtwinters
logTotalUnemployment_Developedseries <- log(TotalUnemployment_Developedseries)
TotalUnemployment_Developedseriesforecasts <- HoltWinters(logTotalUnemployment_Developedseries)
TotalUnemployment_Developedseriesforecasts
TotalUnemployment_Developedseriesforecasts$SSE

plot(TotalUnemployment_Developedseriesforecasts)


#Forecasts Jan 2021 to Dec 2025
TotalUnemployment_Developedseriesforecasts2 <- forecast(TotalUnemployment_Developedseriesforecasts, h=48)
TotalUnemployment_Developedseriesforecasts2


plot(TotalUnemployment_Developedseriesforecasts2)


acf(TotalUnemployment_Developedseriesforecasts2$residuals, lag.max=20 , na.action = na.pass)
Box.test(TotalUnemployment_Developedseriesforecasts2$residuals, lag=20, type="Ljung-Box")


plot.ts(TotalUnemployment_Developedseriesforecasts2$residuals) # make time series plot

TotalUnemployment_Developedseriesforecasts2$residuals <- TotalUnemployment_Developedseriesforecasts2$residuals [!is.na(TotalUnemployment_Developedseriesforecasts2$residuals)]
plotForecastErrors(TotalUnemployment_Developedseriesforecasts2$residuals)
mean(TotalUnemployment_Developedseriesforecasts2$residuals)

#Forcasting using Arima 
#Getting the model
acf(TotalUnemployment_Developedseries, lag.max=20, plot=FALSE) # get the values

pacf(TotalUnemployment_Developedseries, lag.max=20)
pacf(TotalUnemployment_Developedseries, lag.max=20, plot=FALSE)

auto.arima(TotalUnemployment_Developed)

auto.arima(TotalUnemployment_Developed,ic='bic')
####### Forecasting Using an ARIMA Model #######

TotalUnemployment_Developedseriesarima <- arima(TotalUnemployment_Developedseries, order=c(1,0,1))
TotalUnemployment_Developedseriesarima 

TotalUnemployment_Developedseriesforecasts1 <- forecast(TotalUnemployment_Developedseriesarima , h=48)
TotalUnemployment_Developedseriesforecasts1

plot(TotalUnemployment_Developedseriesforecasts1)

acf(TotalUnemployment_Developedseriesforecasts1$residuals, lag.max=20)
Box.test(TotalUnemployment_Developedseriesforecasts1$residuals, lag=20, type="Ljung-Box")

plot.ts(TotalUnemployment_Developedseriesforecasts1$residuals)


plotForecastErrors(TotalUnemployment_Developedseriesforecasts1$residuals) 
mean(TotalUnemployment_Developedseriesforecasts1$residuals)



