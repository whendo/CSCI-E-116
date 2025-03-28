# Bill Henderson
# CSCI E-116 Section 1
# Assignment 01
# 2024Feb04

# Part A: Linear Regression: An Example of Turning A Small Dataset Into Knowledge
# Importing data
library(readxl)
corporate.tax = read_excel("P02_Corporate tax.xlsx")

# On Page 55, there are three equations. Replicate these regression results
model1 = lm(ypcg ~ ctax, data=corporate.tax)
summary(model1)

model2 = lm(ypcg ~ ctax + ypc2000, data=corporate.tax)
summary(model2)

model3 = lm(ypcg ~ ctax + ypc2000 + dty + ctax*dty, data=corporate.tax)
summary(model3)

# Based on Equation 3, use its coefficients (alpha and betas) to predict a hypothetical GDP 
# per capita growth rate when a country has a corporate tax rate = 20%, GDP per capita in
# 2000 = $10,000, and debt to GDP ratio = 35%
inputs = data.frame(ctax=20, ypc2000=10000, dty=35)
model3.prediction = predict(model3, inputs)
print("predicted GDP per capita: ")
print(unname(model3.prediction))
      
# Plot a chart similar to Figure 4. (The red line is a regression, fitted line)
plot(ypcg ~ ctax, 
     data=corporate.tax,
     main = "The association between the average corporate tax
     rate '00-'08 and the average GDP per capita growth 
     '00-'15",
     xlab = "Average Corporate Tax Rate '00-'08",
     ylab = "Average GDP per capita Growth '00-'15",
     col='blue',
     pch = 16
     )
abline(model1, col='red')

abbrs = data.frame(fullname = c("Ireland", "Latvia", "United States", "Japan", "Italy"),
                   abb = c("IRL", "LVA", "USA", "JPN", "ITY"))

for (i in seq_len(nrow(abbrs))){
  c = corporate.tax[corporate.tax$country==abbrs[i,]$fullname,]
  text(x=c$ctax,y=c$ypcg, labels = abbrs[i,]$abb, pos = 1)
}
#####
# Briefly explain why I use GDP per capita in 2000, not 2015? Why do I use corporate tax
# rates averaged from 2000 to 2008 instead of from 2000 to 2015?
#####
# We use the data points around year 2000 because we want to minimize outside effects
# and are aiming to focus on short term change in GDP growth.

# The dataset provides more variables (description as follows). Play around by adding these
# variables. And present the best model (using adj. R2) and briefly explain the result
test = lm(ypcg ~ . -country, data=corporate.tax)
summary(test)

test = lm(ypcg ~ ypc2000 + ctax + ihc -country, data=corporate.tax)
summary(test)

test = lm(ypcg ~ ypc2000 + ctax  -country, data=corporate.tax)
summary(test)

#Explanation
# The highest Adj R^2 value was for the first model that included all variables except
# the country variable. This model included quite a few explanatory variables that weren't
# statistically significant per their p values. The overfitting caused by these 
# insignificant variables skews the Adj R^2 value, leading us to believe the model is better
# than it is. The model with no insignificant variables with an alpha < 0.05 led to an 
# Adj R^2 of 0.52, much lower than the model with all the variables. 
#####
# Part B: See Exec summary doc separate from this file
#####
source("~/Documents/workspace/harvard/CSCI-E-116/Assignment01/Data and Script 2/H02b_crossSection.R", echo = TRUE)

