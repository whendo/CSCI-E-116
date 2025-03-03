###########################################################################################
# H05a: ARIMA Models
# By William Yu, UCLA Anderson Forecast
# 9/30/2023
###########################################################################################
library(readxl)
library(dplyr)
library(forecast)
library(quantmod) 
library(tseries)

###########################################################################################################
# Seasonal ARMA Model
###########################################################################################################
getSymbols("HOUST", src="FRED") # Housing Starts; https://fred.stlouisfed.org/series/HOUST
hssa = HOUST
auto.arima(hssa)

getSymbols("HOUSTNSA", src="FRED") # Housing Starts; https://fred.stlouisfed.org/series/HOUSTNSA
hs = HOUSTNSA
plot(hs)

ggtsdisplay(hs)
# The significant spikes at lag 1 and 2 in PACF suggest AR(2)
# The significant spikes at seasonality in ACF suggest seasonality at MA term
# The significant spikes at seasonality in PACF suggest seasonality at AR term

model7a = arima(hs, order=c(2,0,3))                       # ARMA(2,3) without seasonal factor 
model7b = arima(hs, order=c(2,0,3), seasonal=c(1,0,1))    # ARMA(2,3) & seasonal factor (wrong order) in ARMA term
model7c = arima(hs, order=c(2,0,3), seasonal=c(12,0,0))   # ARMA(2,3) & seasonal factor in AR term
model7d = arima(hs, order=c(2,0,3), seasonal=c(0,0,12))   # ARMA(2,3) & seasonal factor in MA terms
model7e = arima(hs, order=c(2,0,3), seasonal=c(12,0,12))  # ARMA(2,3) & seasonal factor in ARMA terms
model7f = arima(hs, order=c(0,0,0), seasonal=c(12,0,12))  # seasonal model without cycle model
model7g = arima(hs, order=c(2,0,3), seasonal = list(order = c(2,0,3), period = 12))

# Forecast error evaluation with Root Mean Square Error (RMSE): 
accuracy(model7a)   # the worst model with the highest RMSE
accuracy(model7b)   
accuracy(model7c)
accuracy(model7d) 
accuracy(model7e)   # the best model with the lowest RMSE
accuracy(model7f)    
accuracy(model7g)  

accuracy(auto.arima(hs, seasonal=T)) # Not good!
accuracy(auto.arima(hs))             # The same, not good!

# Check autocorrelation of residuals
ggtsdisplay(residuals(model7e))
checkresiduals(model7e)

for7e = forecast(model7e,120)  # Forecast seasonally adjusted series for the next 120 periods
for7e
for7e %>% autoplot()

for7f = forecast(model7f,120)  
for7f
for7f %>% autoplot()

getSymbols("SP500", src="FRED") # S&P 500; https://fred.stlouisfed.org/series/SP500
getSymbols("DCOILWTICO", src="FRED") # WTI Crude Oil Prices; https://fred.stlouisfed.org/series/DCOILWTICO
getSymbols("DEXUSEU", src="FRED") # US/Euro Exchange Rate; https://fred.stlouisfed.org/series/DEXUSEU
getSymbols("PAYEMS", src="FRED") # US Payroll Employment; https://fred.stlouisfed.org/series/PAYEMS
getSymbols("DGS10", src="FRED") # 10-year Treasury Rate; https://fred.stlouisfed.org/series/DGS10
getSymbols("CPIAUCSL", src="FRED") # Consumer Price Index; https://fred.stlouisfed.org/series/CPIAUCSL
getSymbols("TOTALSA", src="FRED") # Total Vehicle Sales; https://fred.stlouisfed.org/series/TOTALSA
getSymbols("POPTHM", src="FRED") # US Population; https://fred.stlouisfed.org/series/POPTHM
getSymbols("CSUSHPISA", src="FRED") # US National Home Price; https://fred.stlouisfed.org/series/CSUSHPISA

sp500 = SP500
oil = DCOILWTICO
useu = DEXUSEU
payroll = PAYEMS
tr10 = DGS10
cpi = CPIAUCSL
autos = TOTALSA
pop = POPTHM
homeprice = CSUSHPISA

par(mfrow=c(3,3))
plot(sp500)
plot(oil)
plot(useu)
plot(payroll)
plot(tr10)
plot(cpi)
plot(autos)
plot(pop)
plot(homeprice)
par(mfrow=c(1,1))

##
## Unit Root Test KPSS: Null Hypothesis - the series is stationary  
##
install.packages("urca")
library(urca)
# KPSS Test: test-stat is bigger than 1% critical value, reject the null. 
sp500 %>% ur.kpss %>% summary()         # SP500 has a unit root.
oil %>% ur.kpss %>% summary()           # Unit root
useu %>% ur.kpss %>% summary()          # Unit root
payroll %>% ur.kpss %>% summary()       # Unit root  
tr10 %>% ur.kpss %>% summary()          # Unit root
cpi %>% ur.kpss %>% summary()           # Unit root
autos %>% ur.kpss %>% summary()         # Unit root
pop %>% ur.kpss %>% summary()           # Unit root
homeprice %>% ur.kpss %>% summary()     # Unit root
diff(payroll) %>% ur.kpss %>% summary() # stationary

# ARIMA Models
auto.arima(sp500)     
auto.arima(oil)       
auto.arima(useu)      
auto.arima(payroll)   
auto.arima(tr10)      
auto.arima(cpi)       
auto.arima(autos)     
auto.arima(pop)       
auto.arima(homeprice) 

auto.arima(sp500) %>% forecast(h=10) %>% autoplot(80) 
auto.arima(oil) %>% forecast(h=10) %>% autoplot(include=80) 
auto.arima(useu) %>% forecast(h=10) %>% autoplot(include=80) 
auto.arima(payroll) %>% forecast(h=10) %>% autoplot(include=80) 
auto.arima(tr10) %>% forecast(h=10) %>% autoplot(include=80) 
auto.arima(cpi) %>% forecast(h=20) %>% autoplot(include=160) 
auto.arima(autos) %>% forecast(h=10) %>% autoplot(include=80) 
auto.arima(pop) %>% forecast(h=20) %>% autoplot(include=160) 
auto.arima(homeprice) %>% forecast(h=20) %>% autoplot(include=160) 

## Compare
auto.arima(homeprice) %>% forecast(h=20)
forecast(homeprice, 20)

# Revisit Housing starts forecast
plot(hs)
model7h = arima(hs, order=c(0,1,1), seasonal=c(12,0,12))
for7h = forecast(model7h,120)  
for7h
for7h %>% autoplot()

model7i = arima(hs, order=c(0,1,1), seasonal = list(order = c(1,0,2), period = 12))
for7i = forecast(model7i, 120)  
for7i
for7i %>% autoplot()

## With a drift term
auto.arima(sp500) 
arima(sp500, order=c(1,1,0))
arima(sp500, order=c(1,1,0), include.drift=TRUE) # Doesn't work
Arima(sp500, order=c(1,1,0))
Arima(sp500, order=c(1,1,0), include.drift=TRUE)

Arima(homeprice, order=c(1,1,0)) # AIC: 290
Arima(homeprice, order=c(3,2,2)) # AIC: 238
Arima(homeprice, order=c(3,1,2), include.drift=TRUE) # AIC: 239

# Random walk with a drift
rwf(sp500, 10, drift=F)
rwf(sp500, 10, drift=T)
auto.arima(sp500) %>% forecast(h=10)

auto.arima(tr10) %>% forecast(h=10)
# Pure random walk 
rwf(tr10, 10)

## Cross Validation // Rolling Window Testset
e1=tsCV(homeprice, rwf, drift=F,h=1)
sqrt(mean(e1^2, na.rm=T))
e2=tsCV(homeprice, rwf, drift=T,h=1)
sqrt(mean(e2^2, na.rm=T))
e3=tsCV(homeprice, forecast, h=1)
sqrt(mean(e3^2, na.rm=T))

for_hp = function(x, h){forecast(Arima(x, order=c(3,2,2)), h=h)}
e4=tsCV(homeprice, for_hp, h=1)
sqrt(mean(e4^2, na.rm=T))

##
## COVID-19 Forecast
## 
covid19 = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") 
nyc = subset(covid19, county == "New York City", select = c(date, deaths))
la = subset(covid19, county == "Los Angeles", select = c(date, deaths))
head(nyc)
str(nyc)
p=nrow(nyc)
nyc = nyc %>% mutate(date = as.Date(date))
la = la %>% mutate(date = as.Date(date))
str(nyc)
plot(nyc, type="l")

auto.arima(nyc$deaths)
for8_nyc = forecast(auto.arima(nyc$deaths),30)
plot(for8_nyc)

auto.arima(la$deaths)
for8_la = forecast(auto.arima(la$deaths),30)
plot(for8_la)

nyc1=nyc[1:70,]
for8_nyc1 = forecast(auto.arima(nyc1$deaths),p-70)

par(mfrow=c(1,2))
plot(for8_nyc1, ylim=c(0,100000), main="Time Series Forecast COVID19 Deaths in NYC", lwd=2, cex.main=0.9)
grid()
plot(nyc, type="l", ylim=c(0,100000), main="Actual Accumulated COVID19 Deaths in NYC", lwd=2, cex.main=0.9)
grid()
par(mfrow=c(1,1))

la1=la[1:250,]
for8_la1 = forecast(auto.arima(la1$deaths),p-250)

par(mfrow=c(1,2))
plot(for8_la1, ylim=c(0,35000), main="Time Series Forecast COVID19 Deaths in L.A.", lwd=2,cex.main=0.9)
grid()
plot(la, type="l", ylim=c(0,35000), main="Actual Accumulated COVID19 Deaths in L.A.", lwd=2,cex.main=0.9)
grid()
par(mfrow=c(1,1))