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
     xlab = "Average C orporate Tax R ate ’00-’08",
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

