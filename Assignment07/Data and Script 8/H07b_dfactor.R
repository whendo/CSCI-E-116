########################################################################## 
# H07b: Dynamic Factor Model
# By William Yu, UCLA Anderson Forecast
# updated 10/22/2024
##########################################################################
library(readxl)  
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(mice)
library(tseries) 
library(quantmod) 
library(forecast) 
library(vars)
library(lmtest)

setwd("C:/Users/wiyu/documents/zip08/2024 Q4 Fall_Harvard/Data/")
macro = read_xlsx("W09c_macro.xlsx") 
str(macro)
summary(macro)
macro1 = macro[,-1] # Delete the first column time stamp

# Principal Component Analysis
macro2 = prcomp(macro1, scale=TRUE)
# Scale=T means standardize or normalize the different scales of variables
summary(macro2)
# PC1: 52%, PC2:20%, PC3: 7.6%, PC4: 6%, PC5: 3.6%. Accumulated: 89%
macro2$center
macro2$scale
macro2$rotation  #loading factor
pc8 = macro2$x[,1:8] # The first 8 PCs
# Add back time stamp variable
pc8a = cbind(macro[,1], pc8)

ggplot(pc8a,aes(x=time,y=PC1)) + geom_line() # Level, trend factor (positive)
ggplot(pc8a,aes(x=time,y=-PC2)) + geom_line() # Housing factor (negative)
ggplot(pc8a,aes(x=time,y=PC3)) + geom_line() # Hours factor (positive)
ggplot(pc8a,aes(x=time,y=PC4)) + geom_line() # Goods factor (positive)
ggplot(pc8a,aes(x=time,y=PC5)) + geom_line() # Other factor (positive)

pc8.month = ts(pc8a[,-1], start=c(1990,1), frequency=12)
pc8.qtr = aggregate(pc8.month, nfrequency=4,mean) 
plot(pc8.qtr)

ggAcf(pc8[,3],lag=60)

getSymbols("GDPC1", src="FRED")  
gdp = GDPC1['1990-01-01/2023-04-01']
data = data.frame(gdp,pc8.qtr)

fit01 = lm(GDPC1~.,data)
summary(fit01)
checkresiduals(fit01)

gdp_arima = auto.arima(gdp); plot(forecast(gdp_arima))

##
## Turn PC/Factor into dynamic
##

# ARIMA Method
pc1.arima = auto.arima(pc8.qtr[,1]); plot(forecast(pc1.arima))
pc1.arima
pc2.arima = auto.arima(pc8.qtr[,2]); plot(forecast(pc2.arima))
pc3.arima = auto.arima(pc8.qtr[,3]); plot(forecast(pc3.arima))
pc4.arima = auto.arima(pc8.qtr[,4]); plot(forecast(pc4.arima))
pc5.arima = auto.arima(pc8.qtr[,5]); plot(forecast(pc5.arima))
pc6.arima = auto.arima(pc8.qtr[,6]); plot(forecast(pc6.arima))
pc7.arima = auto.arima(pc8.qtr[,7]); plot(forecast(pc7.arima))
pc8.arima = auto.arima(pc8.qtr[,8]); plot(forecast(pc8.arima))

macro_lf8 = as.data.frame(macro2$rotation[,1:8])
macro_pc8 = as.data.frame(macro2$x[,1:8])

# VAR Method
# Select the lag of VAR model
VARselect(pc8.qtr, lag=20, type="both") # Not good.

# Model with 4 lag
var01 = VAR(pc8.qtr, p=4, type="both")  
summary(var01)

fcast01 = predict(var01, n.ahead = 12, ci=0.95)
fcast01
plot(fcast01)
