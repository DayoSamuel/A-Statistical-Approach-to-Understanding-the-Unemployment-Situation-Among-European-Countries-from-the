#Install packages

install.packages("car")
install.packages("corrplot")
install.packages("caret")
library(car)
library(corrplot)
library(caret)

New_EU <- read.csv(file = 'New_EU_countries.csv')
head(as.data.frame(New_EU))
str(New_EU)

#Define the obj of the regression analysis. 
New_EU_reduced <- New_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                           'GDP_US_DOLLARS')] 
cor(New_EU_reduced)
corrplot(cor(New_EU_reduced))

#Step 3 perform the linear regression analysis. 
?Im

#To write Y= Percentage_Total_Unemployment, X= Total_Population
model_1 <-lm(Percentage_Total_Unemployment ~ Total_Population, New_EU_reduced)
summary.lm(model_1)

#To visualize the fitted regression line first, we should draw the scatter plot

plot(Percentage_Total_Unemployment ~ Total_Population, New_EU_reduced,
     col = "blue",
     main = "Regression: Percentage of Total Unemployment & Total Population",
     xlab = "Total Population",
     ylab = "Total Unemployment")

#Then, adding the regression line to the plot:

abline(model_1, col="red")
plot(model_1, 1)
#Normality of residuals:
plot(model_1, 2)
#Equal variances of the residuals (Homoscedasticity)
plot(model_1, 3)


#OLD EU

Old_EU <- read.csv(file = 'Old_EU_countries.csv')
head(as.data.frame(Old_EU))
str(Old_EU)

#Define the obj of the regression analysis. 
Old_EU_reduced <- Old_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                             'GDP_US_DOLLARS')] 
cor(Old_EU_reduced)
corrplot(cor(Old_EU_reduced))

#Step 3 perform the linear regression analysis. 
?Im

#To write Y= Percentage_Total_Unemployment, X= Total_Population
model_2 <-lm(Percentage_Total_Unemployment ~ Total_Population, Old_EU_reduced)
summary.lm(model_2)

#To visualise the fitted regression line first, we should draw the scatter plot

plot(Percentage_Total_Unemployment ~ Total_Population, Old_EU_reduced,
     col = "blue",
     main = "Regression: Percentage of Total Unemployment & Total Population",
     xlab = "Total Population",
     ylab = "Total Unemployment")
#Then, adding the regression line to the plot:

abline(model_2, col="red")
plot(model_2, 1)
#Normality of residuals:
plot(model_2, 2)
#Equal variances of the residuals (Homoscedasticity)
plot(model_2, 3)

