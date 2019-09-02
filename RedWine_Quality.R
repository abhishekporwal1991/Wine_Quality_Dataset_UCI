#---
#title: "Wine Quality"
#author: "Abhishek Porwal"
#date: "March 21, 2018"
#---


# Red wine exploration
#================================

# Loading the required packages
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(GGally)
library(memisc)
library(corrplot)

# Loading the data
setwd("C:/Users/dell/Desktop/Sapient Online assignment")
getwd()
RedWine = read.csv("winequality-red.csv",sep = ";", header = T)

# Red wine data- 1599 observations with 11 variables + quality(12th) as target variable.
names(RedWine)
summary(RedWine)
str(RedWine)

# Quality distribution

# Wine quality is a discrete variable. Its value ranging from 3 to 8. Median value is at 6.

ggplot(RedWine, aes(quality)) + geom_histogram(colour = "black", fill = "#993366",
                                               binwidth = 0.2) +
  xlab("Wine Quality") + ylab("count") + ggtitle("Distribution of (Red) Wine Quality")

# Distribution of other chemical properties using histogram

p1 = ggplot(RedWine, aes(x=fixed.acidity)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.5)

p2 = ggplot(RedWine, aes(x=volatile.acidity)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.05)

p3 = ggplot(RedWine, aes(x=citric.acid)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.05)

p4 = ggplot(RedWine, aes(x=residual.sugar)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.4)

p5 = ggplot(RedWine, aes(x=chlorides)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.025)

p6 = ggplot(RedWine, aes(x=free.sulfur.dioxide)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 4)

p7 = ggplot(RedWine, aes(x=total.sulfur.dioxide)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 20)

p8 = ggplot(RedWine, aes(x=density)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.001)

p9 = ggplot(RedWine, aes(x=pH)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.05)

p10 = ggplot(RedWine, aes(x=sulphates)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.1)

p11 = ggplot(RedWine, aes(x=alcohol)) + 
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.25)

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, ncol = 3)

# Observations:

# 1. Normal: volatile.acidity, density, pH
# 2. Positive Skewed: fixed.acidity, citric.acid, free.sulfur.dioxide, total.sulfur.dioxide,sulphates,alcohol
# 3. Long Tail: choloride, residual.sugar

# Rescale variable - Taking  "log" of the skewed data

# Original data
p1 = ggplot(RedWine, aes(x=sulphates)) +
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.1)

# Original data
RedWine$log_sulphate = log(RedWine$sulphates)

p2 = ggplot(RedWine, aes(x=log_sulphate)) +
  geom_histogram(colour = "black", fill = "#993366", binwidth = 0.1) +
  xlab("Logarithm of Sulphates") 

grid.arrange(p1,p2, ncol =1) # In comparison, log scale is more normally distributed.

# correlation between the wine quality and the other chemical properties
CorMatrix = cor(RedWine)
corrplot(CorMatrix, type = "upper", method = "number")  # Alcohol is having highest correlation with the wine quality

# Scatter plot of quality Vs Alcohol
ggplot(aes(x=quality,y=alcohol),data=RedWine)+
      geom_point() + ggtitle("Red Wine")

# modification in scatter plot with median line
ggplot(RedWine, aes(x = quality, y = alcohol)) +
  geom_point(color = '#993366', alpha = 0.25) +
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .5), color = '#FF6660') +
  xlab("Wine quality") + ylab("Alcohol") +
  ggtitle("Red Wine Quality and Alcohol") # In the plot wine quality is increasing with the increase in the alcohol contain of wine.

# Transforming Wine Quality into Categorical data
RedWine$grade_number = cut(RedWine$quality, c(2.5,3.5,4.5,5.5,6.5,7.5,8.5),
                           labels = c('3','4','5','6','7','8'))
str(RedWine)

# Plotting boxplot
ggplot(RedWine, aes(x = grade_number, y = alcohol, fill = grade_number)) +
  geom_boxplot() +
  xlab("Wine Grade") + ylab("Alcohol") +
  ggtitle("Alcohol Vs (Red) Wine Quality")

# Similar analysis for other 3 chemical components (volatile acid, citric acid $ Sulphates) having higher value of correlation coefficient with wine quality.

# Boxplot of Volatile acid Vs Wine Quality
ggplot(RedWine, aes(x=grade_number, y = volatile.acidity, fill= grade_number)) + 
 geom_boxplot() +
  xlab("Wine grade") + ylab("Volatile acidity") +
  ggtitle("Volatile acid Vs (Red) Wine Quality")

# Boxplot of citric acid Vs Wine Quality
ggplot(RedWine, aes(x = grade_number, y = citric.acid, fill = grade_number)) +
  geom_boxplot() +
  xlab("Wine Grade") + ylab("Citric acid") +
  ggtitle("Citric acid Vs (Red) Wine Quality")

# Boxplot of Sulphates Vs Wine Quality
ggplot(RedWine, aes(x=grade_number, y = sulphates, fill = grade_number)) +
  geom_boxplot() +
  xlab("Wine Quality") + ylab("Sulphates") +
  ggtitle(" Sulphates Vs (Red) Wine Quality")

# Analysis using density plot
ggplot(RedWine, aes(x=alcohol, fill = grade_number)) +
  geom_density(aes(y=..density..), alpha = 0.5) +
  xlab("Wine grade") + ylab("Alcohol") +
  ggtitle("Alcohol distribution of different wine grade")

# The density plots are unclear so we can do labeling of wine quality....
# 1. Grade 3 & 4 = Low grade
# 2. Grade 5 & 6 = Medium grade
# 3. Grade 7 & 8 = High grade

RedWine$grade = cut(RedWine$quality, c(2.5,4.5,6.5,8.5),
                    labels = c("Low", "Medium", "High"))
str(RedWine)

# Wine grade Vs Alcohol
ggplot(RedWine, aes(x=alcohol, fill = grade)) +
  geom_density(aes(y=..density..), alpha = 0.5, position = "identity") +
  xlab("Wine grade") + ylab("Alcohol") +
  ggtitle("Alcohol distribution of different wine grade(Red)")

# Wine grade Vs Volatile acidity
ggplot(RedWine, aes(x=volatile.acidity, fill = grade)) +
  geom_density(aes(y=..density..), alpha = 0.5, position = "identity") +
  xlab("Wine Grade") + ylab("Volatile acidity") +
  ggtitle("Distribution of Volatile Acidity with Wine grade(Red)")

# Wine grade Vs Citric acid
ggplot(RedWine, aes(x=citric.acid, fill = grade)) +
  geom_density(aes(y=..density..), alpha = 0.5, position = "identity") +
  xlab("Wine Grade") + ylab("citric acid") +
  ggtitle("Distribution of citric Acid with Wine grade(Red)")

  # Wine grade Vs Logarithmic sulphate
ggplot(RedWine, aes(x=log_sulphate, fill = grade)) +
  geom_density(aes(y=..density..), alpha = 0.5, position = "identity") +
  xlab("Wine Grade") + ylab("Sulphates") +
  ggtitle("Distribution of Sulphates with Wine grade (Red)")

# Main chemical Property Vs Wine Quality - using multi dimensional approach

# Plot using jitter
ggplot(RedWine, aes(x=quality, y=alcohol, color = volatile.acidity)) +
  geom_jitter(alpha=0.5, size=3) +
  xlab("Wine Quality") + ylab("Alcohol") +
  scale_color_gradient2(low="red", high = "blue", mid = "#9933CC", midpoint = 0.8)

# We can see that higher quality wine have higher percentage of alcohol with lower amount of volatile acidity.

# log sulphates Vs Wine Quality

ggplot(RedWine, aes(x=alcohol, y=volatile.acidity)) +
  geom_point(aes(color = log_sulphate), alpha = 0.5, size = 3) +
  xlab("alcohol") + ylab("Log of Sulphates") +
  scale_color_gradient2(low="red", high="blue", mid="#9933CC", midpoint = -0.25) +
  facet_grid(grade~.)

# We can see higher quality wine have higher alcohol, lower volatile acidity and higher sulphates.
 
# Another way to look into the main Chemical Property Vs Wine Quality

ggplot(RedWine, aes(x=volatile.acidity, y=alcohol)) +
  xlab("volatile acidity") + ylab("alcohol") +
  geom_point(aes(color=grade), size=2)

ggplot(RedWine, aes(x=log_sulphate, y=citric.acid)) +
  geom_point(aes(color = grade), size=2) +
  xlab("log Sulphate") + ylab("citric acid")

# Higher quality wine have higher citric acid and higher sulphates.

# Linear Multi-variable Model
# Modeling is done to identify the mathematical relationship between chemical properties of wine and the wine quality. It is done in incremental form.
m1 = lm(quality~volatile.acidity, data=RedWine)
m2 = update(m1,~. + alcohol)
m3 = update(m2,~. + sulphates)
m4 = update(m3,~. + citric.acid)
m5 = update(m4,~. + chlorides)
m6 = update(m5,~. + total.sulfur.dioxide)
m7 = update(m6,~. + density)
mtable(m1,m2,m3,m4,m5,m6,m7)

# The model with 6 features (m6) has the lowest AIC (akaike information criterion) number. As the number of feature increase AIC become higher.The parameter of the predictor also changed.

# The model can be described as

# wine_quality = 2.985 - 1.104*volatile.acidity + 0.276*alcohol + 0.908*sulphates + 0.065*citric.acid - 1.763*chlorides - 0.002*total.sulfur.dioxide

# -----------------------