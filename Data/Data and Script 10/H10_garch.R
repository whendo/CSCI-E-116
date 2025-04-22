######################################################################################################
# H10: Time-Varying Volatility Model
# By William Yu, UCLA Anderson Forecast
# 4/8/2025
# Reference: https://faculty.washington.edu/ezivot/econ589/Introduction_to_the_rugarch_package.pdf
# https://cran.r-project.org/web/packages/rugarch/rugarch.pdf
###################################################################################################### 
library(quantmod)
library(rugarch)

# Volatilities observed
getSymbols("VIXCLS", src="FRED")  #CBOE Volatility Index
vix = na.omit(VIXCLS)
plot(vix)
acf(vix,lag.max=60)
vix_arima = auto.arima(vix) 
vix_arima
vix_arima %>% forecast(h=20) %>% autoplot(150) 

##
## Univariate GARCH Model
## 
getSymbols("^GSPC", src="yahoo")
sp = GSPC$GSPC.Adjusted

plot(sp)

sp_returns = dailyReturn(sp)  # diff(log(sp)) doesn't work well in GARCH modeling
plot(sp_returns)
mean(sp_returns); sd(sp_returns)
plot(density(sp_returns), main="Smoothed Histogram of Daily Stock Returns",lty=1, lwd=5, col="red")
acf(sp_returns,lag.max=60)

sp2 = sp_returns^2
plot(sp2)
mean(sp2); sd(sp2)
acf(sp2, lag.max=60)

##
## Constant mean, standard GARCH(1,1) model
##
spec01 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0), include.mean = FALSE))

gfit01 = ugarchfit(spec = spec01, data = sp_returns)
gfit01   # log Likelihood: 9112

# Plot the conditional volatility
plot(gfit01, which = 1)  
plot(gfit01, which = 2) 
plot(gfit01, which = 3)  # Conditional variance
plot(gfit01, which = 4)  # ACF of stock return
plot(gfit01, which = 5)  # ACF of squared return
plot(gfit01, which = 6)  # ACF of absolute return
plot(gfit01, which = 8) 
plot(gfit01, which = 9) # QQ Plot
plot(gfit01, which = 10)
plot(gfit01, which = 11)

# Forecast future volatility
forecasted_volatility = ugarchforecast(gfit01, n.ahead = 100)
print(forecasted_volatility)
plot(forecasted_volatility, which = 3)  # sigma (unconditional)

##
## Mean is following ARMA(1,1) with standard GARCH(1,1) model
##
spec02 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE))
gfit02 = ugarchfit(spec = spec02, data = sp_returns)
gfit02  # log Likelihood: 9138

# Plot the conditional volatility
plot(gfit02, which = 1)  

# Forecast future volatility
forecasted_volatility = ugarchforecast(gfit02, n.ahead = 100)
print(forecasted_volatility)
plot(forecasted_volatility, which = 3)  

##
## GARCH-in-mean model (archm = TRUE)
##
spec03 = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model = list(armaOrder = c(1, 1), include.mean = TRUE,
                                      archm = TRUE))

gfit03 = ugarchfit(spec = spec03, data = sp_returns)
gfit03   # log Likelihood: 9138

# Plot the conditional volatility
plot(gfit03, which = 1)  
plot(gfit03, which = 3)  

# Forecast future volatility
forecasted_volatility = ugarchforecast(gfit03, n.ahead = 100)
print(forecasted_volatility)
plot(forecasted_volatility, which = 3)  

##
## Value at risk model (Something is wrong!)
##
cl = makePSOCKcluster(10)
spec04 = ugarchspec(variance.model = list(model="eGARCH"), distribution.model ="jsu")
roll = ugarchroll(spec04, sp_returns, n.start = 1000, refit.every = 100,
                  refit.window = "moving", solver = "hybrid", calculate.VaR = TRUE,
                  VaR.alpha = c(0.01, 0.05), cluster = cl, keep.coef = TRUE)
show(roll)
report(roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.95)
plot(roll)
