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


model_3 <-lm(Percentage_Total_Unemployment ~ Total_Population + GDP_US_DOLLARS, New_EU_reduced)
summary.lm(model_3)


data.frame(colnames(New_EU_reduced))

pairs(New_EU_reduced[,c(2,1,3)], lower.panel = NULL, pch = 19,cex = 0.2)

plot(model_3,1)
plot(model_3,2)
plot(model_3,3)
vif(model_3)

#OLD EU

Old_EU <- read.csv(file = 'Old_EU_countries.csv')
head(as.data.frame(Old_EU))
str(Old_EU)

#Define the obj of the regression analysis. 
Old_EU_reduced <- Old_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                             'GDP_US_DOLLARS')] 
cor(Old_EU_reduced)
corrplot(cor(Old_EU_reduced))


#creating a model 

model_4 <-lm(Percentage_Total_Unemployment ~ Total_Population + GDP_US_DOLLARS, Old_EU_reduced)
summary.lm(model_4)


data.frame(colnames(Old_EU_reduced))

pairs(Old_EU_reduced[,c(2,1,3)], lower.panel = NULL, pch = 19,cex = 0.2)

plot(model_4,1)
plot(model_4,2)
plot(model_4,3)
vif(model_4)
