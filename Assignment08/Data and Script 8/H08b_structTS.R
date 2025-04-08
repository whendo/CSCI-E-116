######################################################################################################
# H08b: Structural Time Series Models
# By William Yu, UCLA Anderson Forecast
# 3/20/2024
# Reference: State Space Models in R, Journal of Statistical Software, May 2011, 41(4)
# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/StructTS
###################################################################################################### 
library(forecast)
library(quantmod) 

nile = Nile
plot(nile)

fitNile = StructTS(Nile, "level")
# level: local level model
# trend: local linear trend
# BSM: a local trend with an additional seasonal component
fitNile

plot(nile, type = "o")
lines(fitted(fitNile), lty = "dashed", lwd = 2, col="blue")
lines(tsSmooth(fitNile), lty = "dotted", lwd = 4, col="red")

# Example: Oil Prices 
getSymbols("DCOILWTICO", src="FRED")  # WTI Crude Oil Prices
oilm = apply.monthly(DCOILWTICO, mean, na.rm=T) 
m = nrow(oilm)-3              # Hold out final 3 months as a testset
oil = ts(oilm[1:m], frequency=12, start=c(1986,1))
plot(oil)

fit01 = StructTS(oil, "level")
fit02 = StructTS(oil, "trend")

## Compare Test-set forecast errors
auto.arima(oil) %>% forecast(h=3)
forecast(fit01, h = 3) 
forecast(fit02, h = 3) 
tail(oilm)

# Example: Retail Sales 
getSymbols("RSAFSNA", src="FRED")     # Total Retail and Food Services NSA
plot(RSAFSNA)
m = nrow(RSAFSNA)-4                   # Hold out the final 4 periods as testset
rs = ts(RSAFSNA[1:m], frequency=12, start=c(1992,1))
plot(rs)
        
fit03 = StructTS(rs, type = "BSM")
par(mfrow = c(4, 1)) 
plot(log(rs))
plot(cbind(fitted(fit03), resids=resid(fit03)))
par(mfrow = c(1, 1))
plot(forecast(fit03, h = 12))

arima_rs = auto.arima(rs) %>% forecast(h=4)
auto.arima(rs)
arima_rs
fcst03 = forecast(fit03, h = 4) 
fcst03
tail(RSAFSNA)

# RMSE for ARIMA model
fcst_arima = as.vector(arima_rs$mean)
rmse_arima = sqrt(mean((fcst_arima-as.vector(RSAFSNA[(m+1):(m+4),]))^2))
rmse_arima

# RMSE for State Space Model
fcst_sp = as.vector(fcst03$mean)
rmse_sp = sqrt(mean((fcst_sp-as.vector(RSAFSNA[(m+1):(m+4),]))^2))
rmse_sp

# Nov 2023 to Feb 2024 - RMSE: ARIMA 18617, State Space 15182

# Example: S&P500
getSymbols("SP500", src="FRED") # S&P 500; https://fred.stlouisfed.org/series/SP500
p = length(SP500); p
plot(SP500)
m = nrow(SP500)-100  
sp500 = SP500[1:m,]

fit04 = StructTS(sp500, type = "trend")
plot(forecast(fit04, h = 100))

auto.arima(sp500) %>% forecast(h=100) %>% plot()
arima_sp500 = auto.arima(sp500) %>% forecast(h=100)


