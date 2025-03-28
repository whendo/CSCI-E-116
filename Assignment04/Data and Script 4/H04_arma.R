###########################################################################################
# H04: Models for Cycle, ARMA
# By William Yu, UCLA Anderson Forecast
# 4/30/2024
###########################################################################################
library(forecast)
library(quantmod) 
library(tseries)

##
## Check the autocorrealtions 
##

# Monthly Unemployment rate
getSymbols("UNRATE", src="FRED")   # https://fred.stlouisfed.org/series/UNRATE 
urate=UNRATE
plot(urate)
ggAcf(urate,lag=60)
# Show both ACF and PACF charts
ggtsdisplay(urate)

# Quarterly Real GDP growth
getSymbols("A191RL1Q225SBEA", src="FRED")   # https://fred.stlouisfed.org/series/A191RL1Q225SBEA 
gdpg=A191RL1Q225SBEA
plot(gdpg)
ggAcf(gdpg,lag=20)
acf(gdpg,lag.max=20)

gdpg01 = gdpg['1947/2019-10-01']
ggAcf(gdpg01,lag=20)
ggtsdisplay(gdpg01)

# Monthly Payroll jobs Change
getSymbols("PAYEMS", src="FRED")   # https://fred.stlouisfed.org/series/PAYEMS 
eead=diff(PAYEMS)
plot(eead)
ggAcf(eead,lag=60)
eead01 = eead['1939/2020-02-01']   # Before the pandemic
ggAcf(eead01,lag=60)
ggtsdisplay(eead01)

# Monthly Vehicles Sales
getSymbols("TOTALSA", src="FRED")   # https://fred.stlouisfed.org/series/TOTALSA
car=TOTALSA
plot(car)
ggAcf(car,lag=60)

par(mfrow=c(2,2))
acf(gdpg,lag.max=20)
acf(urate,lag.max=60)
acf(eead01,lag.max=60, na.action=na.pass)
acf(car,lag.max=60)
par(mfrow=c(1,1))

# Retail Sales Growth NSA
getSymbols("RETAILSMNSA", src="FRED")   # https://fred.stlouisfed.org/series/RETAILSMNSA
retailn=RETAILSMNSA
plot(retailn)
retailg=diff(log(retailn))
plot(retailg)
ggAcf(retailg,lag=60)

# Cass-Shiller Home Price Index, US-10 City average
getSymbols("SPCS10RSA", src="FRED")  # https://fred.stlouisfed.org/series/SPCS10RSA
cshome = SPCS10RSA
cshomer = diff(log(cshome))  # calculate growth rate, home price return
ggtsdisplay(cshomer)

# Stocks Prices and returns
getSymbols("NASDAQCOM", src="FRED")   # https://fred.stlouisfed.org/series/NASDAQCOM
nasd=NASDAQCOM  # Daily data
nasdm=apply.monthly(nasd, mean, na.rm=T)   # Nasdaq Index: convert it to monthly data
nasdmr=diff(log(nasdm))   # calculate growth rate, stock price return
np = length(nasdmr)
random=ts(rnorm(np))

plot(cshome)
par(mfrow=c(2,2))
plot(cshomer)
plot(nasdm)
plot(nasdmr)
plot(random)
par(mfrow=c(1,1))

# Autocorrelation Function
par(mfrow=c(2,2))
acf(cshomer,lag.max=60, na.action=na.pass)
acf(nasdm,lag.max=60)
acf(nasdmr,lag.max=60, na.action=na.pass)
acf(random,lag.max=60)
par(mfrow=c(1,1))

ggAcf(cshome,lag=60)
ggAcf(cshomer,lag=60)
ggAcf(nasdm,lag=60)
ggAcf(random,lag=60)
ggAcf(nasdmr,lag=60)

# Partial Autocorrelation Function --> Help to determine the order of AR term
pacf(cshomer,lag.max=60,na.action=na.pass)
pacf(nasdm,lag.max=60)
pacf(random,lag.max=60)
pacf(nasdmr,lag.max=60,na.action=na.pass)

getSymbols("AAPL", src="yahoo")  
aapl = Ad(AAPL)  # Adjusted price
aaplr = diff(log(aapl)) 
ggtsdisplay(aaplr)

getSymbols("TSLA", src="yahoo")  
tsla = Ad(TSLA)  # Adjusted prices
tslar = diff(log(tsla)) 
ggtsdisplay(tslar)

##
## Autocorrelation Tests
## When p-value < 0.05, reject the null hypothesis: the series has no-autocorrelation. In other words, the series has autocorrelation.
# lag=10 for non-seasonal data and lag=2m for seasonal data where m is the period of seasoanlity or T/5 (whoever is smaller)
Box.test(gdpg01, lag=8, type="Ljung-Box")
Box.test(urate, lag=10, type="Ljung-Box")
Box.test(random, lag=10, type="Ljung-Box")

##################################################################################################
# Model 6: ARMA Models
##################################################################################################
model6a = arima(cshomer, order=c(1,0,0)) # AR(1) 
model6b = arima(cshomer, order=c(0,0,1)) # MA(1) 
model6d = arima(cshomer, order=c(2,0,0)) # AR(2)
model6e = arima(cshomer, order=c(0,0,2)) # MA(2)
model6f = arima(cshomer, order=c(2,0,1)) # ARMA(2,1)
model6g = arima(cshomer, order=c(1,0,2)) # ARMA(1,2)
model6h = arima(cshomer, order=c(2,0,2)) # ARMA(2,2)

summary(model6a)
summary(model6d)
summary(model6g)
summary(model6h)

# Forecast error evaluation with Root Mean Square Error (RMSE): 
accuracy(model6a)
accuracy(model6b)
accuracy(model6d)
accuracy(model6e)
accuracy(model6f)   
accuracy(model6g)
accuracy(model6h)   # the best one with the lowest RMSE

for6h = forecast(model6h,24)  # forecast the next 24 months
for6h
plot(for6h)
abline(h=0)
grid()

# A shortcut
auto.arima(cshomer) # growth rate
auto.arima(cshome)  # level data

model6i=auto.arima(cshomer)
for6i = forecast(model6i,24)  
for6i
plot(for6i)
abline(h=0)
grid()

par(mfrow=c(1,2))
plot(for6h)
plot(for6i)
par(mfrow=c(1,1))

