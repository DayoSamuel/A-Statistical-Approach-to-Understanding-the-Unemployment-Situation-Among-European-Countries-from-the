#Install packages

install.packages("car")
install.packages("corrplot")
install.packages("caret")
library(car)
library(corrplot)
library(caret)

Developed_EU <- read.csv(file = 'Developed_countries.csv')
head(as.data.frame(Developed_EU))
str(Developed_EU)

#Define the obj of the regression analysis. 
Developed_EU_reduced <- Developed_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                           'GDP_US_DOLLARS')] 
cor(Developed_EU_reduced)
corrplot(cor(Developed_EU_reduced))

#Step 3 perform the linear regression analysis. 
?Im

#To write Y= Percentage_Total_Unemployment, X= Total_Population
model_5 <-lm(Percentage_Total_Unemployment ~ Total_Population, Developed_EU_reduced)
summary.lm(model_5)

#To visualize the fitted regression line first, we should draw the scatter plot

plot(Percentage_Total_Unemployment ~ Total_Population, Developed_EU_reduced,
     col = "blue",
     main = "Regression: Percentage of Total Unemployment & Total Population",
     xlab = "Total Population",
     ylab = "Total Unemployment")

#Then, adding the regression line to the plot:

abline(model_5, col="red")
plot(model_5, 1)
#Normality of residuals:
plot(model_5, 2)
#Equal variances of the residuals (Homoscedasticity)
plot(model_5, 3)


#OLD EU

UnderDeveloped_EU <- read.csv(file = 'UnderDeveloped_countries.csv')
head(as.data.frame(UnderDeveloped_EU))
str(UnderDeveloped_EU)

#Define the obj of the regression analysis. 
UnderDeveloped_EU_reduced <- UnderDeveloped_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                             'GDP_US_DOLLARS')] 
cor(UnderDeveloped_EU_reduced)
corrplot(cor(UnderDeveloped_EU_reduced))

#Step 3 perform the linear regression analysis. 
?Im

#To write Y= Percentage_Total_Unemployment, X= Total_Population
model_6 <-lm(Percentage_Total_Unemployment ~ Total_Population, Old_EU_reduced)
summary.lm(model_6)

#To visualise the fitted regression line first, we should draw the scatter plot

plot(Percentage_Total_Unemployment ~ Total_Population, UnderDeveloped_EU_reduced,
     col = "blue",
     main = "Regression: Percentage of Total Unemployment & Total Population",
     xlab = "Total Population",
     ylab = "Total Unemployment")
#Then, adding the regression line to the plot:

abline(model_6, col="red")
plot(model_6, 1)
#Normality of residuals:
plot(model_6, 2)
#Equal variances of the residuals (Homoscedasticity)
plot(model_6, 3)



