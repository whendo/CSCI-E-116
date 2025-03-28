###########################################################################################
# H06a VAR Models
# By William Yu, UCLA Anderson Forecast
# 10/10/2023
###########################################################################################
# setwd("C:/Users/wiyu/documents/zip08/2023 Q4 Fall_Harvard/Data")

library(quantmod) 
library(tseries)
library(forecast)
library(readxl) 
library(fpp2)
library(vars)
library(lmtest)

##
## VAR Models
##
getSymbols("GDPC1", src="FRED")     # Real GDP
getSymbols("UNRATE", src="FRED")    # Unemployment Rate
getSymbols("CPILFESL", src="FRED")  # Core Consumer Price Index
getSymbols("DFF", src="FRED")       # Federal Fund Rate       

gdp = GDPC1['1957/2023-12-31']
unrate = apply.quarterly(UNRATE['1957/2023-12-31'], mean)
ccpi = apply.quarterly(CPILFESL['1957/2023-12-31'], mean)
frr = apply.quarterly(DFF['1957/2023-12-31'], mean)

gdp = ts(gdp, frequency=4, start=c(1957,1))
unrate = ts(unrate, frequency=4, start=c(1957,1))
ccpi = ts(ccpi, frequency=4, start=c(1957,1))
frr = ts(frr, frequency=4, start=c(1957,1))

gdpg = diff(log(gdp))*100   # annualized growth rate
inflation = diff(log(ccpi))*100 

##
## 4 variable models
##
data00 = cbind(gdpg, unrate, inflation, frr)
data0 = window(data00, start=c(1957,2))
# Remove the first row of the data to make sure there is no missing value
plot(data0)

# Select the lag of VAR model
VARselect(data0, lag=20,type="const")

# Model with 4 lag
var01 = VAR(data0, p=4, type="const")  
summary(var01)

imp01 = irf(var01, impulse="frr", response="gdpg", n.ahead=12, ortho=F, runs=1000)
plot(imp01)
imp02 = irf(var01, impulse="frr", response="unrate", n.ahead=12, ortho=F, runs=1000)
plot(imp02)
imp03 = irf(var01, impulse="frr", response="inflation", n.ahead=12, ortho=F, runs=1000)
plot(imp03)
imp04 = irf(var01, impulse="inflation", response="frr", n.ahead=12, ortho=F, runs=1000)
plot(imp04)
imp05 = irf(var01, impulse="unrate", response="frr", n.ahead=12, ortho=F, runs=1000)
plot(imp05)
imp06 = irf(var01, impulse="gdpg", response="frr", n.ahead=12, ortho=F, runs=1000)
plot(imp06)

fcast01 = predict(var01, n.ahead = 12, ci=0.95)
fcast01
plot(fcast01)

##
## 3 variable models
##
data11 = cbind(gdpg, inflation, frr)
data1 = window(data11, start=c(1957,2))
# Remove the first row of the data to make sure there is no missing value

# Select the lag of VAR model
VARselect(data1, lag=20,type="const")

# Model with 4 lag
var11 = VAR(data1, p=4, type="const")  
summary(var11)

imp11 = irf(var11, impulse="frr", response="gdpg", n.ahead=12, ortho=F, runs=1000)
plot(imp11)
imp12 = irf(var11, impulse="frr", response="inflation", n.ahead=12, ortho=F, runs=1000)
plot(imp12)
imp13 = irf(var11, impulse="inflation", response="frr", n.ahead=12, ortho=F, runs=1000)
plot(imp13)
imp14 = irf(var11, impulse="gdpg", response="frr", n.ahead=12, ortho=F, runs=1000)
plot(imp14)

fcast11 = predict(var11, n.ahead = 12, ci=0.95)
fcast11
plot(fcast11)

##
## compared to ARIMA models
##
arima_gdpg = auto.arima(gdpg)    
arima_unrate = auto.arima(unrate) 
arima_inflation = auto.arima(inflation) 
arima_frr = auto.arima(frr) 
farima_gdpg = forecast(arima_gdpg,12) 
farima_unrate = forecast(arima_unrate,12) 
farima_inflation = forecast(arima_inflation,12) 
farima_frr = forecast(arima_frr,12) 

par(mfrow=c(4,1))
plot(farima_gdpg)
plot(farima_unrate)
plot(farima_inflation)
plot(farima_frr)
par(mfrow=c(1,1))

fcast01$fcst$frr
farima_frr

fcast01$fcst$inflation
farima_inflation

##
## Granger Causality Test
##

# Null Hypothesis: X does not Granger- cause Y

grangertest(gdpg ~ unrate, order = 12, data=data0)
grangertest(unrate ~ gdpg, order = 12, data=data0)

grangertest(frr ~ inflation, order = 12, data=data0)
grangertest(inflation ~ frr, order = 12, data=data0)


