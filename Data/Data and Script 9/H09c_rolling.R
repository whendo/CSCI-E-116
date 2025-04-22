######################################################################################################
# H09c: Rolling
# By William Yu, UCLA Anderson Forecast
# 4/8/2025
# https://www.rdocumentation.org/packages/zoo/versions/1.8-11/topics/rollapply
###################################################################################################### 
library(dplyr)
library(ggplot2)
library(tseries) 
library(quantmod) 
library(forecast) 

##
## Time-varying / Rolling Beta!
##
getSymbols("NVDA", src="yahoo") 
getSymbols("^GSPC", src="yahoo")  

nvda = NVDA['2007-01-03/2025-02-28']
sp500 = GSPC['2007-01-03/2025-02-28']

nvda = diff(log(nvda[,6]))
sp500 = diff(log(sp500[,6]))
data = cbind(nvda, sp500)
colnames(data) = c("nvda","sp500")
nrow(data)

ols = lm(nvda~sp500)
summary(ols)
ols[[1]] # ols result's first element -- coefficient (out of 13 elements)
ols[[1]][[2]] # coefficient's second result -- beta

# Rolling beta
?rollapply
# 300 is the rolling window's width
beta = rollapply(data, 300, by.column = F,
                 function(x) lm(x[,1]~x[,2])[[1]][[2]])
alpha = rollapply(data, 300, by.column = F,
                 function(x) lm(x[,1]~x[,2])[[1]][[1]])

plot(beta)
plot(alpha)

##
## Time Series Cross-Validation -- Rolling Window Validation
##
getSymbols("GDPC1", src="FRED")  
# set the trainset
gdp_train = GDPC1['1990-01-01/2023-01-01'] # the length of the testset is chosen as 7 quarters
nrow(gdp_train)
data_length = nrow(GDPC1) 

## (1) ARIMA Method
## One testset
gdp_arima = auto.arima(gdp_train)
fcst01 = forecast(gdp_arima, 7)
plot(fcst01)
fcst_arima = as.vector(fcst01$mean)
rmse_arima = sqrt(mean((fcst_arima-as.vector(GDPC1[(data_length-6):(data_length),]))^2))
rmse_arima  

## Cross-validation: rolling window testset
test = rollapply(gdp_train, 100, 
                 FUN = function(x) {
                   gdp_arima = auto.arima(x)
                   fcst02 = forecast(gdp_arima, 7)
                   plot(fcst02)
                   fcst_arima = as.vector(fcst02$mean)
                 }, by.column = FALSE)  

## (2) Structural Time Series / State Space Model
## One testset
ssm = StructTS(gdp_train, type = "trend")
plot(forecast(ssm, h = 7))
fcst03 = forecast(ssm, h = 7)
fcst_ssm = as.vector(fcst03$mean)
rmse_ssm = sqrt(mean((fcst_ssm-as.vector(GDPC1[(data_length-6):(data_length),]))^2))
rmse_ssm  

## Cross-validation: rolling window testset
test1 = rollapply(gdp_train, 100, 
                  FUN = function(x) {
                    ssm = StructTS(x, type = "trend")
                    fcst04 = forecast(ssm, h = 7)
                    plot(fcst04)
                    fcst_ssm = as.vector(fcst04$mean)
                  }, by.column = FALSE) 
