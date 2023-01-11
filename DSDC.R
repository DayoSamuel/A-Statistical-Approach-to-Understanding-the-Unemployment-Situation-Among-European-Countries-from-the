#Importing Developed EU Countries csv into DataFrame

Developed_countries <- read.csv("Developed_countries.csv", header = TRUE)

names(Developed_countries)
head(Developed_countries)
tail(Developed_countries)
str(Developed_countries)
summary(Developed_countries)

#Importing UnderDeveloped EU Countries csv into DataFrame

UnderDeveloped_countries <- read.csv("UnderDeveloped_countries.csv", header = TRUE)

names(UnderDeveloped_countries)
head(UnderDeveloped_countries)
tail(UnderDeveloped_countries)
str(UnderDeveloped_countries)
summary(UnderDeveloped_countries)


#Finding Mean of Total Population of Developed Countries
mean(Developed_countries$Total_Population)

#Finding Mean of Total Population of UnderDeveloped Countries
mean(UnderDeveloped_countries$Total_Population)

#Finding Median of Total Population of Developed Countries
median(Developed_countries$Total_Population)

#Finding Median of Total Population of UnderDeveloped Countries
median(UnderDeveloped_countries$Total_Population)


#Finding mode of Total Population of Developed Countries
mode(Developed_countries$Total_Population)

#Finding mode of Total Population of UnderDeveloped Countries
mode(UnderDeveloped_countries$Total_Population)

#Finding Standard Deviation of Total Population of Developed Countries
sd(Developed_countries$Total_Population)

#Finding Standard Deviation of Total Population of UnderDeveloped Countries
sd(UnderDeveloped_countries$Total_Population)

#Install moments for skewness and kurtosis

installed.packages("moments")
library("moments")

#Finding skewness of Total Population of Developed Countries
skewness(Developed_countries$Total_Population)
hist(Developed_countries$Total_Population)


#Finding skewness of Total Population of UnderDeveloped Countries
skewness(UnderDeveloped_countries$Total_Population)
hist(UnderDeveloped_countries$Total_Population)

#Finding kurtosis of Total Population of Developed Countries
kurtosis(Developed_countries$Total_Population)
hist(Developed_countries$Total_Population)

#Finding kurtosis of Total Population of UnderDeveloped Countries
kurtosis(UnderDeveloped_countries$Total_Population)
hist(UnderDeveloped_countries$Total_Population)


########################################GDP DESCRIPTIVE###########################

#Finding Mean of GDP_US_DOLLARS of Developed Countries
mean(Developed_countries$GDP_US_DOLLARS)

#Finding Mean of GDP_US_DOLLARS of UnderDeveloped Countries
mean(UnderDeveloped_countries$GDP_US_DOLLARS)

#Finding Median of GDP_US_DOLLARS of Developed Countries
median(Developed_countries$GDP_US_DOLLARS)

#Finding Median of GDP_US_DOLLARS of UnderDeveloped Countries
median(UnderDeveloped_countries$GDP_US_DOLLARS)


#Finding mode of GDP_US_DOLLARS of Developed Countries
mode(Developed_countries$GDP_US_DOLLARS)

#Finding mode of GDP_US_DOLLARS of UnderDeveloped Countries
mode(UnderDeveloped_countries$GDP_US_DOLLARS)

#Finding Standard Deviation of GDP_US_DOLLARS of Developed Countries
sd(Developed_countries$GDP_US_DOLLARS)

#Finding Standard Deviation of GDP_US_DOLLARS of UnderDeveloped Countries
sd(UnderDeveloped_countries$GDP_US_DOLLARS)

#Install moments for skewness and kurtosis

installed.packages("moments")
library("moments")

#Finding skewness of GDP_US_DOLLARS of Developed Countries
skewness(Developed_countries$GDP_US_DOLLARS)
hist(Developed_countries$GDP_US_DOLLARS)


#Finding skewness of GDP_US_DOLLARS of UnderDeveloped Countries
skewness(UnderDeveloped_countries$GDP_US_DOLLARS)
hist(UnderDeveloped_countries$GDP_US_DOLLARS)

#Finding kurtosis of GDP_US_DOLLARS of Developed Countries
kurtosis(Developed_countries$GDP_US_DOLLARS)
hist(Developed_countries$GDP_US_DOLLARS)

#Finding kurtosis of GDP_US_DOLLARS of UnderDeveloped Countries
kurtosis(UnderDeveloped_countries$GDP_US_DOLLARS)
hist(UnderDeveloped_countries$GDP_US_DOLLARS)


#Finding skewness of Percentage_Total_Unemployment of Developed Countries
skewness(Developed_countries$Percentage_Total_Unemployment)
hist(Developed_countries$Percentage_Total_Unemployment)


#Finding skewness of Percentage_Total_Unemployment of UnderDeveloped Countries
skewness(UnderDeveloped_countries$Percentage_Total_Unemployment)
hist(UnderDeveloped_countries$Percentage_Total_Unemployment)

#Finding kurtosis of Percentage_Total_Unemployment of Developed Countries
kurtosis(Developed_countries$Percentage_Total_Unemployment)
hist(Developed_countries$Percentage_Total_Unemployment)

#Finding kurtosis of Percentage_Total_Unemployment of UnderDeveloped Countries
kurtosis(UnderDeveloped_countries$Percentage_Total_Unemployment)
hist(UnderDeveloped_countries$Percentage_Total_Unemployment)


#Histogram of Unemployment rate 
hist(Developed_countries$Percentage_Total_Unemployment)
hist(UnderDeveloped_countries$Percentage_Total_Unemployment)

#Histogram of GDP_US_DOLLARS 
hist(Developed_countries$GDP_US_DOLLARS)
hist(UnderDeveloped_countries$GDP_US_DOLLARS)


#Finding Outliers in Population for UnderDeveloped Countries.

boxplot(UnderDeveloped_countries$Total_Population,
        ylab="Underdeveloped EU Total Population",
        col="orange",
        border="brown"
)

out <- boxplot.stats(UnderDeveloped_countries$Total_Population)$out
out_ind <- which(UnderDeveloped_countries$Total_Population %in% c(out))
out_ind


UnderDeveloped_countries[out_ind,]

#Finding Outliers in GDP for UnderDeveloped Countries

boxplot(UnderDeveloped_countries$GDP_US_DOLLARS,
        ylab="UnderDeveloped EU GDP",
        col="orange",
        border="brown"
)

out_1 <- boxplot.stats(UnderDeveloped_countries$GDP_US_DOLLARS)$out
out1_ind <- which(UnderDeveloped_countries$GDP_US_DOLLARS %in% c(out_1))
out1_ind


UnderDeveloped_countries[out1_ind,]


#Finding Outliers in Percentage_Total_Unemployment  for UnderDeveloped Countries

boxplot(UnderDeveloped_countries$Percentage_Total_Unemployment,
        ylab="Percentage_Total_Unemployment",
        col="orange",
        border="brown"
)

out_2 <- boxplot.stats(UnderDeveloped_countries$Percentage_Total_Unemployment)$out
out2_ind <- which(UnderDeveloped_countries$Percentage_Total_Unemployment %in% c(out_2))
out2_ind


UnderDeveloped_countries[out2_ind,]



#Finding Outliers in Population for Developed Countries.

boxplot(Developed_countries$Total_Population,
        ylab="Developed EU Total Population",
        col="orange",
        border="brown"
)

out_3 <- boxplot.stats(Developed_countries$Total_Population)$out
out3_ind <- which(Developed_countries$Total_Population %in% c(out_3))
out3_ind


Developed_countries[out3_ind,]

#Finding Outliers in GDP for Developed Countries

boxplot(Developed_countries$GDP_US_DOLLARS,
        ylab="Developed EU GDP",
        col="orange",
        border="brown"
)

out_4 <- boxplot.stats(Developed_countries$GDP_US_DOLLARS)$out
out4_ind <- which(Developed_countries$GDP_US_DOLLARS %in% c(out_4))
out4_ind


Developed_countries[out4_ind,]


#Finding Outliers in Percentage_Total_Unemployment  for Developed Countries

boxplot(Developed_countries$Percentage_Total_Unemployment,
        ylab="Developed EU Percentage of Total Unemployment",
        col="orange",
        border="brown"
)

out_5 <- boxplot.stats(Developed_countries$Percentage_Total_Unemployment)$out
out5_ind <- which(Developed_countries$Percentage_Total_Unemployment %in% c(out_5))
out5_ind


Developed_countries[out5_ind,]



#Performing Null Hypothesis & Alternative Hypothesis for Developed EU 
jarque.test(Developed_countries$Percentage_Total_Unemployment)
jarque.test(Developed_countries$Total_Population)
jarque.test(Developed_countries$GDP_US_DOLLARS)


#Performing Null Hypothesis & Alternative Hypothesis for Underdeveloped EU 
jarque.test(UnderDeveloped_countries$Percentage_Total_Unemployment)
jarque.test(UnderDeveloped_countries$Total_Population)
jarque.test(UnderDeveloped_countries$GDP_US_DOLLARS)

