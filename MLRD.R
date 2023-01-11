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


corrplot(cor(Developed_EU_reduced))

model_7 <-lm(Percentage_Total_Unemployment ~ Total_Population + GDP_US_DOLLARS, Developed_EU_reduced)
summary.lm(model_7)


data.frame(colnames(Developed_EU_reduced))

pairs(Developed_EU_reduced[,c(2,1,3)], lower.panel = NULL, pch = 19,cex = 0.2)

plot(model_7,1)
plot(model_7,2)
plot(model_7,3)
vif(model_7)

#using lasso regression

install.packages("glmnet")

library(glmnet)
#define response variable
y <- Developed_EU_reduced$Percentage_Total_Unemployment
#define matrix of predictor variables
x <- data.matrix(Developed_EU_reduced[,c('Total_Population','GDP_US_DOLLARS')])
#perform k-fold cross-validation to find optimal lamba value
cv_model <- cv.glmnet(x,y, alpha =1)

#find optimal lambda that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

plot(cv_model)
#find co-eff of best model
best_model <-glmnet(x,y, alpha =1, lambda = best_lambda)
coef(best_model)

#define new obs Total = 9.269985e-09 , GDP 1.625898e-13
new = matrix(c(9.269985e-09 , 1.625898e-13), nrow = 1, ncol = 2)
predict(best_model,s=best_lambda,newx = new)

#now calculate R SQUARED

y_predicted <- predict(best_model,s=best_lambda,newx = x)
#find SST and SSE
sst <- sum ((y - mean(y))^2)
sse <- sum ((y_predicted - y)^2)

#find R-squared
rsq <- 1 - sse/sst
rsq

#Underdeveloped

UnderDeveloped_EU <- read.csv(file = 'UnderDeveloped_countries.csv')
head(as.data.frame(UnderDeveloped_EU))
str(UnderDeveloped_EU)

#Define the obj of the regression analysis. 
UnderDeveloped_EU_reduced <- UnderDeveloped_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                                                   'GDP_US_DOLLARS')] 
cor(UnderDeveloped_EU_reduced)
corrplot(cor(UnderDeveloped_EU_reduced))


model_8 <-lm(Percentage_Total_Unemployment ~ Total_Population + GDP_US_DOLLARS, UnderDeveloped_EU_reduced)
summary.lm(model_8)


data.frame(colnames(UnderDeveloped_EU_reduced))

pairs(UnderDeveloped_EU_reduced[,c(2,1,3)], lower.panel = NULL, pch = 19,cex = 0.2)

plot(model_8,1)
plot(model_8,2)
plot(model_8,3)
vif(model_8)

