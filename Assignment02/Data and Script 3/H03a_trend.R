######################################################################################################
# H03a: Models for trends
# By William Yu, UCLA Anderson Forecast
# 6/29/2024
##################################################################################################### 
library(quantmod) 
library(tseries)
library(fpp2)
library(ggplot2)

# Real GDP: https://fred.stlouisfed.org/series/GDPC1
# Reference: https://rpubs.com/DanielSLee/ImportingManagingFinancialData

getSymbols("GDPC1", src="FRED")  
plot(GDPC1)   
head(GDPC1); tail(GDPC1)
plot(log(GDPC1))

# Real GDP Growth Rate; https://fred.stlouisfed.org/series/A191RL1Q225SBEA/
getSymbols("A191RL1Q225SBEA", src="FRED")   
plot(A191RL1Q225SBEA)   

gdp = data.frame(GDPC1)
trend = data.frame(c(1:length(GDPC1)))
trend2 = trend^2
lrgdp = data.frame(log(GDPC1))
gdp.df = cbind(gdp,trend,trend2,lrgdp)
names(gdp.df) = c("rgdp","trend","trend2","lrgdp")
gdp.ts = ts(gdp.df,frequency=4,start=c(1947,1))

# Model 1: Linear Trend Model
model01 = lm(rgdp~trend,data=gdp.df)
summary(model01)  
fit01 = predict(model01)
fit01.ts = ts(fit01,frequency=4,start=c(1947,1))
ts.plot(GDPC1, fit01.ts, col = c("black","red"))

# Model 2: Quadratic Trend Model
model02 = lm(rgdp~trend+trend2,data=gdp.df)
summary(model02)  
fit02 = predict(model02)
fit02.ts = ts(fit02,frequency=4,start=c(1947,1))
ts.plot(GDPC1, fit02.ts, lty=c(1,2), lwd=c(1,2))

# Model 3: Log-Linear Trend Model
model03 = lm(lrgdp~trend,data=gdp.df)
summary(model03)
fit03 = predict(model03)
fit03.ts = ts(fit03,frequency=4,start=c(1947,1))
ts.plot(GDPC1, exp(fit03.ts), lty=c(1,5), lwd=c(1,3),col=c("black","blue"))

##
## Start the sample from 1984Q1
##
?window
gdp84=window(gdp.ts, start=c(1984,1))
n=nrow(gdp84)
gdp.df2=data.frame(gdp84)
gdp.df2$trend=seq(1,n)
gdp.df2$trend2=(gdp.df2$trend)^2

# Model 1: Linear Trend Model
model01a = lm(rgdp~trend,data=gdp.df2)
summary(model01a)  
fit01a = predict(model01a)
fit01a.ts = ts(fit01a,frequency=4,start=c(1984,1))
ts.plot(gdp84[,"rgdp"], fit01a.ts, col=c("black","red"))

# Model 2: Quadratic Trend dModel
model02a = lm(rgdp~trend+trend2,data=gdp.df2)
summary(model02a)  
fit02a = predict(model02a)
fit02a.ts = ts(fit02a,frequency=4,start=c(1984,1))
ts.plot(gdp84[,"rgdp"], fit02a.ts, lty=c(1,2), lwd=c(1,2))

# Model 3: Log-Linear Trend Model
model03a = lm(lrgdp~trend,data=gdp.df2)
summary(model03a)
fit03a = predict(model03a)
fit03a.ts = ts(fit03a,frequency=4,start=c(1984,1))
ts.plot(gdp84[,"rgdp"], exp(fit03a.ts), lty=c(1,5), lwd=c(1,3),col=c("black","blue"))

#
# Out-of-sample check or test-set check
#
testq = 12  # Using the final 12 quarters/3 years as testing period
m = nrow(gdp.df2)-12
gdp.df2b = gdp.df2[1:m,]
gdp.df2t = gdp.df2[(m+1):nrow(gdp.df2),]

# Model 1: Linear Trend Model
model01b = lm(rgdp~trend,data=gdp.df2b)
summary(model01b)  
fit01b = predict(model01b,newdata=gdp.df2t)
rmse1 = sqrt(mean((fit01b-gdp.df2t[,"rgdp"])^2))

# Model 2: Quadratic Trend Model
model02b = lm(rgdp~trend+trend2,data=gdp.df2b)
summary(model02b)  
fit02b = predict(model02b,newdata=gdp.df2t)
rmse2=sqrt(mean((fit02b-gdp.df2t[,"rgdp"])^2))

# Model 3: Log-Linear Trend Model
model03b = lm(lrgdp~trend,data=gdp.df2b)
summary(model03b)
fit03b = predict(model03b,newdata=gdp.df2t)
rmse3=sqrt(mean((exp(fit03b)-gdp.df2t[,"rgdp"])^2))

rmse1;rmse2;rmse3

fit01b.ts = ts(fit01b,frequency=4,start=c(2015,2))
fit02b.ts = ts(fit02b,frequency=4,start=c(2015,2))
fit03b.ts = ts(fit03b,frequency=4,start=c(2015,2))
ts.plot(gdp84[-(1:m),"rgdp"], fit01b.ts, col=c("black","red"))
ts.plot(gdp84[-(1:m),"rgdp"], fit02b.ts, lty=c(1,2), lwd=c(1,2))
ts.plot(gdp84[-(1:m),"rgdp"], exp(fit03b.ts), lty=c(1,5), lwd=c(1,3),col=c("black","blue"))

##
## Using tslm function
##
h=10
model.01c = tslm(rgdp~trend, data=gdp.ts)
fcast.01c = forecast(model.01c, h=h)
model.02c = tslm(rgdp~trend+I(trend^2), data=gdp.ts)
fcast.02c = forecast(model.02c, h=h)
model.spline = tslm(rgdp~trend+I(trend^2)+I(trend^3), data=gdp.ts)
fcast.spl = forecast(model.spline,h=h)

autoplot(gdp.ts[,"rgdp"]) +
  autolayer(fitted(model.01c), series="Linear") +
  autolayer(fitted(model.02c), series="Quadratic") +
  autolayer(fitted(model.spline), series="Cubic Spline") +
  autolayer(fcast.01c, series="Linear") +
  autolayer(fcast.02c, series="Quadratic") +  
  autolayer(fcast.spl, series="Cubic Spline") +
  xlab("Year") + ylab("Real GDP") +
  ggtitle("U.S. Real GDP and Forecasts") +
  guides(colour = guide_legend(title = ""))

##
## Model 6: Holt's Exponential Smoothing Model
##
str(ausair)
air = window(ausair, start=1990)
fc = holt(air, h=5)
fc = holt(air, h=15)
fc$model
fc2 = holt(air, damped=TRUE, phi = 0.95, h=15)
fc3 = holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's, phi=0.95", PI=FALSE) +
  autolayer(fc3, series="Damped Holt's, phi=0.9", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

# Applying Holt's Method on GDP forecast
holt.gdp = holt(gdp.df2b[,"rgdp"], h=m)
autoplot(holt.gdp)
holt.gdp$model
# Holt's forecast on the testset
accuracy(holt.gdp, gdp.df2t[,"rgdp"])

##
## Model 7: Logistics Trend Model
##

# Simple logistic function equation
y = seq(-9,9,0.1)
p7 = 1/(1+exp(-y))
plot(y,p7, type="l", lwd=3, main="Logistic function")

# Simulate and estimate Logistic trend model
# Step 1: Simulation Data
set.seed(123)  
time = 1:100
true_params = list(K = 1400, r = 0.1, t0 = 50)
population = true_params$K / (1 + exp(-true_params$r * (time - true_params$t0))) + rnorm(100, sd = 30)
data = data.frame(time = time, population = population)

# Step 2: Define the Logistic Model
logistic_model = population ~ K / (1 + exp(-r * (time - t0)))

# Step 3: Estimate the Parameters
initial_params = list(K = max(population), r = 0.1, t0 = median(time))
fit = nls(logistic_model, data = data, start = initial_params)         # Nonlinear least square

summary(fit)

data$predicted_population = predict(fit, newdata = data)

ggplot(data, aes(x = time)) +
  geom_point(aes(y = population), color = 'blue', alpha = 0.5) +
  geom_line(aes(y = predicted_population), color = 'red') +
  labs(title = "Logistic Trend Model", x = "Time", y = "Population") +
  theme_minimal()
