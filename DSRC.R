#Importing New EU Countries csv into DataFrame

New_EU_countries <- read.csv("New_EU_countries.csv", header = TRUE)

names(New_EU_countries)
head(New_EU_countries)
tail(New_EU_countries)
str(New_EU_countries)
summary(New_EU_countries)

#Importing Old_EU_countries EU Countries csv into DataFrame

Old_EU_countries <- read.csv("Old_EU_countries.csv", header = TRUE)

names(Old_EU_countries)
head(Old_EU_countries)
tail(Old_EU_countries)
str(Old_EU_countries)
summary(Old_EU_countries)


#Finding Mean of Total Population of New_EU_countries
mean(New_EU_countries$Total_Population)

#Finding Mean of Total Population of Old_EU_countries
mean(Old_EU_countries$Total_Population)

#Finding Median of Total Population of New_EU_countries
median(New_EU_countries$Total_Population)

#Finding Median of Total Population of Old_EU_countries
median(Old_EU_countries$Total_Population)


#Finding mode of Total Population of New_EU_countries
mode(New_EU_countries$Total_Population)

#Finding mode of Total Population of Old_EU_countries
mode(Old_EU_countries$Total_Population)

#Finding Standard Deviation of Total Population of New_EU_countries
sd(New_EU_countries$Total_Population)

#Finding Standard Deviation of Total Population of Old_EU_countries
sd(Old_EU_countries$Total_Population)

#Install moments for skewness and kurtosis

installed.packages("moments")
library("moments")

#Finding skewness of Total Population of New_EU_countries
skewness(New_EU_countries$Total_Population)
hist(New_EU_countries$Total_Population)


#Finding skewness of Total Population of Old_EU_countries
skewness(Old_EU_countries$Total_Population)
hist(Old_EU_countries$Total_Population)

#Finding kurtosis of Total Population of New_EU_countries
kurtosis(New_EU_countries$Total_Population)
hist(New_EU_countries$Total_Population)

#Finding kurtosis of Total Population of Old_EU_countries
kurtosis(Old_EU_countries$Total_Population)
hist(Old_EU_countries$Total_Population)


########################################GDP DESCRIPTIVE###########################

#Finding Mean of GDP_US_DOLLARS of New_EU_countries
mean(New_EU_countries$TGDP_US_DOLLARS)

#Finding Mean of GDP_US_DOLLARS of Old_EU_countries
mean(Old_EU_countries$GDP_US_DOLLARS)

#Finding Median of GDP_US_DOLLARS of New_EU_countries
median(New_EU_countries$GDP_US_DOLLARS)

#Finding Median of GDP_US_DOLLARS of Old_EU_countries
median(Old_EU_countries$GDP_US_DOLLARS)


#Finding mode of GDP_US_DOLLARS of New_EU_countries
mode(New_EU_countries$GDP_US_DOLLARS)

#Finding mode of GDP_US_DOLLARS of Old_EU_countries
mode(Old_EU_countries$GDP_US_DOLLARS)

#Finding Standard Deviation of GDP_US_DOLLARS of New_EU_countries
sd(New_EU_countries$GDP_US_DOLLARS)

#Finding Standard Deviation of GDP_US_DOLLARS of Old_EU_countries
sd(Old_EU_countries$GDP_US_DOLLARS)

#Install moments for skewness and kurtosis

installed.packages("moments")
library("moments")

#Finding skewness of GDP_US_DOLLARS of New_EU_countries
skewness(New_EU_countries$GDP_US_DOLLARS)
hist(New_EU_countries$GDP_US_DOLLARS)


#Finding skewness of GDP_US_DOLLARS of Old_EU_countries
skewness(Old_EU_countries$GDP_US_DOLLARS)
hist(Old_EU_countries$GDP_US_DOLLARS)

#Finding kurtosis of GDP_US_DOLLARS of New_EU_countries
kurtosis(New_EU_countries$GDP_US_DOLLARS)
hist(New_EU_countries$GDP_US_DOLLARS)

#Finding kurtosis of GDP_US_DOLLARS of Old_EU_countries
kurtosis(Old_EU_countries$GDP_US_DOLLARS)
hist(Old_EU_countries$GDP_US_DOLLARS)


#Finding skewness of Percentage_Total_Unemployment of New_EU_countries
skewness(New_EU_countries$Percentage_Total_Unemployment)
hist(New_EU_countries$Percentage_Total_Unemployment)


#Finding skewness of Percentage_Total_Unemployment of Old_EU_countries
skewness(Old_EU_countries$Percentage_Total_Unemployment)
hist(Old_EU_countries$Percentage_Total_Unemployment)

#Finding kurtosis of Percentage_Total_Unemployment of New_EU_countries
kurtosis(New_EU_countries$Percentage_Total_Unemployment)
hist(New_EU_countries$Percentage_Total_Unemployment)

#Finding kurtosis of Percentage_Total_Unemployment of Old_EU_countries
kurtosis(Old_EU_countries$Percentage_Total_Unemployment)
hist(Old_EU_countries$Percentage_Total_Unemployment)




#Histogram of Unemployment rate 
hist(New_EU_countries$Percentage_Total_Unemployment)
hist(Old_EU_countries$Percentage_Total_Unemployment)


#Histogram of GDP_US_DOLLARS 
hist(New_EU_countries$GDP_US_DOLLARS)
hist(Old_EU_countries$GDP_US_DOLLARS)


#Finding Outliers in Population for NEW EU Countries.

boxplot(New_EU_countries$Total_Population,
        ylab="NEW EU Total Population",
        col="orange",
        border="brown"
)

out <- boxplot.stats(New_EU_countries$Total_Population)$out
out_ind <- which(New_EU_countries$Total_Population %in% c(out))
out_ind


New_EU_countries[out_ind,]

#Finding Outliers in GDP for New EU Countries

boxplot(New_EU_countries$GDP_US_DOLLARS,
        ylab="NEW EU GDP",
        col="orange",
        border="brown"
)

out_1 <- boxplot.stats(New_EU_countries$GDP_US_DOLLARS)$out
out1_ind <- which(New_EU_countries$GDP_US_DOLLARS %in% c(out_1))
out1_ind


New_EU_countries[out1_ind,]


#Finding Outliers in Percentage_Total_Unemployment  for NEW EU Countries

boxplot(New_EU_countries$Percentage_Total_Unemployment,
        ylab="Percentage_Total_Unemployment",
        col="orange",
        border="brown"
)

out_2 <- boxplot.stats(New_EU_countries$Percentage_Total_Unemployment)$out
out2_ind <- which(New_EU_countries$Percentage_Total_Unemployment %in% c(out_2))
out2_ind


New_EU_countries[out2_ind,]



#Finding Outliers in Population for OLD EU Countries.

boxplot(Old_EU_countries$Total_Population,
        ylab="Total_Population",
        col="orange",
        border="brown"
)

out_3 <- boxplot.stats(Old_EU_countries$Total_Population)$out
out3_ind <- which(Old_EU_countries$Total_Population %in% c(out_3))
out3_ind


Old_EU_countries[out3_ind,]

#Finding Outliers in GDP for OLD EU Countries

boxplot(Old_EU_countries$GDP_US_DOLLARS,
        ylab="OLD EU GDP",
        col="orange",
        border="brown"
)

out_4 <- boxplot.stats(Old_EU_countries$GDP_US_DOLLARS)$out
out4_ind <- which(Old_EU_countries$GDP_US_DOLLARS %in% c(out_4))
out4_ind


Old_EU_countries[out4_ind,]


#Finding Outliers in Percentage_Total_Unemployment  for OLD EU Countries

boxplot(Old_EU_countries$Percentage_Total_Unemployment,
        ylab="OLD EU Percentage of Total Unemployment",
        col="orange",
        border="brown"
)

out_5 <- boxplot.stats(Old_EU_countries$Percentage_Total_Unemployment)$out
out5_ind <- which(Old_EU_countries$Percentage_Total_Unemployment %in% c(out_5))
out5_ind


Old_EU_countries[out5_ind,]



#Performing Null Hypothesis & Alternative Hypothesis for New EU 
jarque.test(New_EU_countries$Percentage_Total_Unemployment)
jarque.test(New_EU_countries$Total_Population)
jarque.test(New_EU_countries$GDP_US_DOLLARS)


#Performing Null Hypothesis & Alternative Hypothesis for OLD EU 
jarque.test(Old_EU_countries$Percentage_Total_Unemployment)
jarque.test(Old_EU_countries$Total_Population)
jarque.test(Old_EU_countries$GDP_US_DOLLARS)

