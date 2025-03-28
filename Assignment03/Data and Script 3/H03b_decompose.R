######################################################################################################
# H03b: Time Series Decomposition / Seasonal Adjustment
# By William Yu, UCLA Anderson Forecast
# 5/19/2023
##################################################################################################### 
# Reference; http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
library(quantmod) 
library(tseries)
library(fpp2)
library(tsibble)
library(seasonal)

getSymbols("RETAILSMSA", src="FRED")          # https://fred.stlouisfed.org/series/RETAILSMSA
getSymbols("RETAILSMNSA", src="FRED")         # https://fred.stlouisfed.org/series/RETAILSMNSA

retail = ts(RETAILSMSA,frequency=12,start=c(1992,1))    # Seasonally adjusted data
retailn = ts(RETAILSMNSA,frequency=12,start=c(1992,1))  # Non-seasonally adjusted data
ts.plot(retail, retailn, col=c("blue","red"))

ggseasonplot(retailn, year.labels=T, year.labels.left=T)
ggseasonplot(retailn, polar=T)
ggsubseriesplot(retailn)

##
## Decomposition of time series data
##

## Method 1: Simple Decomposition
retail.comps = decompose(retailn)
plot(retail.comps)
plot(retail.comps$seasonal)                 
retail.sa = retailn - retail.comps$seasonal   # To get a seasonally adjusted data, a preliminary way   
ts.plot(retail, retail.sa, col=c("blue","black"))
# retail is official seasonally adjusted retail series done by gov't; retail.sa is done by this simple function. 

# Method 2: Census X11 Decomposition (The better method)
retail.comp1 = retailn %>% seas(x11="")
plot(retail.comp1)
autoplot(retail.comp1)
retail.comp1$data %>% tail()
retail.comp1 %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal Factor")
retail.sa1 = retail.comp1$data[,'seasonaladj']
ts.plot(retail, retail.sa1, col=c("blue","green"))

# Method 3: tslm function and forecast
fit01=tslm(retailn~trend+season)
summary(fit01)
fcast.fit01=forecast(fit01,h=24)

autoplot(retailn, series="Actual retail sales") +
  autolayer(fitted(fit01),series="Fitted") +
  autolayer(fcast.fit01, series="Forecast") +    
  xlab("Year")+ylab("Reatil Sales") +
  ggtitle("Monthly Retail Sales")

# Example: Australian beer production
autoplot(ausbeer)
beer2 = window(ausbeer, start=1992)
fit.beer = tslm(beer2 ~ trend + season)
fcast = forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")

##
## Calendar Adjustments
##
data2=cbind(Monthly=retailn, DailyAverage=retailn/monthdays(retailn))
autoplot(data2, facet=T) + xlab("Years") + ylab("Retail Sales") 

##
## Data transformation
##

# Box-Cox transformations
lambda=seq(-2,2,by=0.1)
x=4
w=(x^lambda-1)/lambda
plot(lambda,w)
grid()
log(4)

lambda=-1
x=seq(0.5,5, by=0.1)
y=(x^lambda-1)/lambda
plot(x, y, type="l")

lambda=0.3
x=seq(0.5,5, by=0.1)
y=(x^lambda-1)/lambda
plot(x, y, type="l")

lambda=1
x=seq(0.5,5, by=0.1)
y=(x^lambda-1)/lambda
plot(x, y, type="l")

lambda=2
x=seq(0.5,5, by=0.1)
y=(x^lambda-1)/lambda
plot(x, y, type="l")

getSymbols("IPB50001N", src="FRED") # Industrial Production;   https://fred.stlouisfed.org/series/IPB50001N
ip=IPB50001N
autoplot(ip)

lam=BoxCox.lambda(ip)
lam
autoplot(BoxCox(ip,lam))
autoplot(log(ip))





