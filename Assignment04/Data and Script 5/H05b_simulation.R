###########################################################################################
# H05b. Time Series Simulation 
# By William Yu, UCLA Anderson Forecast
# 4/30/2024
###########################################################################################
library(forecast)
library(quantmod) 
library(tseries)
library(ggplot2)

##
## Time Series
## Random Walk Simulation
##

RW = function(N, x0, mu, variance) {
  z = cumsum(rnorm(n=N, mean=0, 
                  sd=sqrt(variance)))
  t = 1:N
  x = x0 + t*mu + z
  return(x)
}
# mu is the drift

##
## Random Walk without drift
##
P1 = RW(120,10,0,0.001)
P2 = RW(120,10,0,0.001)
P3 = RW(120,10,0,0.001)
P4 = RW(120,10,0,0.001)

plot(P1, main="Random Walk", 
     xlab="t",ylab="Price", ylim=c(9.4,10.6),
     typ='l', col="red")
par(new=T)  # to draw in the same plot
plot(P2, main="Random Walk", 
     xlab="t",ylab="Price", ylim=c(9.4,10.6),
     typ='l', col="blue")
par(new=T) 
plot(P3, main="Random Walk", 
     xlab="t",ylab="Price", ylim=c(9.4,10.6),
     typ='l', col="black")
par(new=T)  
plot(P4, main="Random Walk", 
     xlab="t",ylab="Price", ylim=c(9.4,10.6),
     typ='l', col="green")

##
## Random Walk with drift
##

P5 = RW(120,10,0.01,0.002)
P6 = RW(120,10,0.01,0.002)
P7 = RW(120,10,0.01,0.002)
P8 = RW(120,10,0.01,0.002)

plot(P5, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="red")
par(new=T)
plot(P6, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="blue")
par(new=T)
plot(P7, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="black")
par(new=T)
plot(P8, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="dark green")

P9 = RW(120,10,0.02,0.002)
P10 = RW(120,10,0.02,0.002)
P11 = RW(120,10,0.02,0.002)
P12 = RW(120,10,0.02,0.002)

plot(P9, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="red")
par(new=T)
plot(P10, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="blue")
par(new=T)
plot(P11, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="black")
par(new=T)
plot(P12, main="Random Walk with Drift", 
     xlab="t",ylab="Price", 
     typ='l', col="brown")

##
## ARIMA Simulation
##
p20 = arima.sim(list(order=c(1,0,0), ar=.7), n=1200); ggtsdisplay(p20)
p21 = arima.sim(list(order=c(1,0,0), ar=.2), n=1200); ggtsdisplay(p21)
p22 = arima.sim(list(order=c(0,0,1), ma=.7), n=1200); ggtsdisplay(p22)
p23 = arima.sim(list(order=c(1,0,0), ar=-.5), n=1200); ggtsdisplay(p23)
p24 = arima.sim(list(order=c(1,0,1), ar=.6, ma=.4), n=1200); ggtsdisplay(p24)
p25 = arima.sim(list(order=c(2,0,0), ar=c(0.6, 0.3)), n=1200); ggtsdisplay(p25)
p25a = arima.sim(list(order=c(2,0,0), ar=c(0.6, 0.5)), n=1200); ggtsdisplay(p25a)
p26 = arima.sim(list(order=c(2,0,0), ar=c(0.5, -0.3)), n=1200); ggtsdisplay(p26)

# With random walk component
p31 = arima.sim(list(order=c(1,1,0), ar=.5), n=1200); plot(p31)
p32 = arima.sim(list(order=c(1,1,0), ar=.5), n=1200); plot(p32)
p33 = arima.sim(list(order=c(1,2,0), ar=.5), n=1200); plot(p33)

##
## Deterministic trend --linear trend
##
getSymbols("GDPC1", src="FRED")  
gdp.ts = ts(GDPC1,frequency=4,start=c(1947,1))
gdp84= window(gdp.ts, start=c(1984,1))
model01 = tslm(GDPC1~trend, data=gdp84)
summary(model01)
fcast01 = forecast(model01, h=10)
fcast01
checkresiduals(model01)

##
## Deterministic trend with ARIMA errors
##
trend02=seq_along(gdp84)
model02 = auto.arima(gdp84, d=0, xreg=trend02)
summary(model02)
checkresiduals(model02)
fcast02 = forecast(model02, xreg=cbind(trend=length(gdp84)+1:10))
fcast02

##
## Stochastic trend
##
model03 = auto.arima(gdp84,d=1) # as an arima(p,1,q)
fcast03 = forecast(model03, h=10)

autoplot(gdp84) +
  autolayer(fcast03, series="Stochastic trend") +
  autolayer(fcast02, series="Determinstic trend") +
  ggtitle("Forecasts from trend models") +
  xlab("Year") + ylab("Real GDP") +
  guides(colour=guide_legend(title="Forecast"))

fcast02; fcast03
