#Install packages

install.packages("datarium")
install.packages("qqplotr")
install.packages("RVAideMemoire")
install.packages("car")
install.packages("corrplot")
install.packages("tidyverse")

#install.packages("dplyr") #This package will be installed by tidyverse
#install.packages("ggplot2") #This package will be installed by tidyverse
library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(corrplot)
library(tidyverse)

#Import New EU data

New_EU <- read.csv(file = 'New_EU_countries.csv')
head(as.data.frame(New_EU))
str(New_EU)

#Define the obj of the correlation difference and analysis for New EU.
#Reduce variables to only numerical (IVs AND DVs)

New_EU_reduced <- New_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                             'GDP_US_DOLLARS')] 
cor(New_EU_reduced)
corrplot(cor(New_EU_reduced))


cor(New_EU_reduced$Percentage_Total_Unemployment, New_EU_reduced$Total_Population)
cor(New_EU_reduced$Percentage_Total_Unemployment, New_EU_reduced$GDP_US_DOLLARS, method = "pearson")
round(cor(New_EU_reduced), digits = 2)
corrplot(cor(New_EU_reduced), method = "number", type = "upper")

#Import Old EU data

Old_EU <- read.csv(file = 'Old_EU_countries.csv')
head(as.data.frame(Old_EU))
str(Old_EU)

#Define the obj of the correlation difference and analysis for Old EU.
#Reduce variables to only numerical (IVs AND DVs)

Old_EU_reduced <- Old_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                             'GDP_US_DOLLARS')] 
cor(Old_EU_reduced)
corrplot(cor(Old_EU_reduced))


cor(Old_EU_reduced$Percentage_Total_Unemployment, Old_EU_reduced$Total_Population)
cor(Old_EU_reduced$Percentage_Total_Unemployment, Old_EU_reduced$GDP_US_DOLLARS, method = "pearson")
round(cor(Old_EU_reduced), digits = 2)
corrplot(cor(Old_EU_reduced), method = "number", type = "upper")

#Import Developed EU data

Developed_EU <- read.csv(file = 'Developed_countries.csv')
head(as.data.frame(Developed_EU))
str(Developed_EU)


#Define the obj of the correlation difference and analysis for Developed EU.
#Reduce variables to only numerical (IVs AND DVs)

Developed_EU_reduced <- Developed_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                                         'GDP_US_DOLLARS')] 
cor(Developed_EU_reduced)
corrplot(cor(Developed_EU_reduced))


cor(Developed_EU_reduced$Percentage_Total_Unemployment, Developed_EU_reduced$Total_Population)
cor(Developed_EU_reduced$Percentage_Total_Unemployment, Developed_EU_reduced$GDP_US_DOLLARS, method = "pearson")
round(cor(Developed_EU_reduced), digits = 2)
corrplot(cor(Developed_EU_reduced), method = "number", type = "upper")



#Import UnderDeveloped EU data

UnderDeveloped_EU <- read.csv(file = 'UnderDeveloped_countries.csv')
head(as.data.frame(UnderDeveloped_EU))
str(UnderDeveloped_EU)


#Define the obj of the correlation difference and analysis for UnderDeveloped EU.
#Reduce variables to only numerical (IVs AND DVs)
UnderDeveloped_EU_reduced <- UnderDeveloped_EU[ ,c('Total_Population', 'Percentage_Total_Unemployment',
                                                   'GDP_US_DOLLARS')] 
cor(UnderDeveloped_EU_reduced)
corrplot(cor(UnderDeveloped_EU_reduced))

cor(UnderDeveloped_EU_reduced$Percentage_Total_Unemployment, UnderDeveloped_EU_reduced$Total_Population)
cor(UnderDeveloped_EU_reduced$Percentage_Total_Unemployment, UnderDeveloped_EU_reduced$GDP_US_DOLLARS, method = "pearson")
round(cor(UnderDeveloped_EU_reduced), digits = 2)
corrplot(cor(UnderDeveloped_EU_reduced), method = "number", type = "upper")

