######################################################################################################
# H05c: Time Series Regression Models
# By William Yu, UCLA Anderson Forecast
# 9/27/2024
###################################################################################################### 
setwd("C:/Users/wiyu/documents/zip08/2024 Q4 Fall MSBA/Data")
library(quantmod) 
library(tseries)
library(fpp2)
library(dplyr)
library(seasonal)
library(dynlm)
library(readxl)
library(AER)

getSymbols("DSPIC96", src="FRED")     # Real Disposable Personal Income;        https://fred.stlouisfed.org/series/DSPIC96
getSymbols("PCEC96", src="FRED")      # Real Personal Consumption Expenditures; https://fred.stlouisfed.org/series/PCEC96
getSymbols("CSUSHPISA", src="FRED")   # S&P/Case-Shiller U.S. National Home Price Index; https://fred.stlouisfed.org/series/CSUSHPISA

income = DSPIC96['2002/2019-12-01']
consumption = PCEC96['2002/2019-12-01']
housing = CSUSHPISA['2002/2019-12-01']

plot(income)
plot(consumption)
plot(housing)

income_mg = diff(log(income))
consume_mg = diff(log(consumption))
housing_mg = diff(log(housing))

income_q = apply.quarterly(income,sum)
consumption_q = apply.quarterly(consumption,sum)
housing_q = apply.quarterly(housing,mean)

income_qg = diff(log(income_q))
consume_qg = diff(log(consumption_q))
housing_qg = diff(log(housing_q))

income_y = apply.yearly(income,sum)
consumption_y = apply.yearly(consumption,sum)
housing_y = apply.yearly(housing,mean)

income_yg = diff(log(income_y))
consume_yg = diff(log(consumption_y))
housing_yg = diff(log(housing_y))

data1 = cbind(income, consumption, housing, income_mg, consume_mg, housing_mg, 
            income_qg, consume_qg, housing_qg, income_yg, consume_yg, housing_yg)
names(data1) = c("income","consumption", "housing", "income_mg","consume_mg","housing_mg", 
               "income_qg","consume_qg","housing_qg", "income_yg","consume_yg","housing_yg")

ggAcf(income, lag=60)
ggAcf(income_mg, lag=60)
ggAcf(income_qg, lag=20)
ggAcf(income_yg, lag=5)

fit01=lm(consumption ~ income, data=data1)
summary(fit01)
checkresiduals(fit01)  # Not good!

fit02=lm(consume_mg ~ income_mg, data=data1)
summary(fit02)
checkresiduals(fit02)

fit03=lm(consume_qg ~ income_qg, data=data1)
summary(fit03)
checkresiduals(fit03)

##
## Heteroscedasticity and Autocorrelation Consistent (HAC) Covariance Matrix
##
## Reference: https://cran.r-project.org/web/packages/sandwich/vignettes/sandwich.pdf
coeftest(fit03)
coeftest(fit03, vcov = vcovHAC)
coeftest(fit03, vcov = NeweyWest)

fit04=lm(consume_yg ~ income_yg, data=data1)
summary(fit04)
checkresiduals(fit04)
coeftest(fit04, vcov = vcovHAC)

fit05=lm(consume_yg ~ income_yg + housing_yg, data=data1)
summary(fit05)
checkresiduals(fit05)
coeftest(fit05, vcov = vcovHAC)

###################################################################################################
# How to Explain Santa Clara County Job Growth Pattern?
###################################################################################################
svalley = read_excel("W08f_Svalley.xlsx")  
svalley.ts=ts(svalley, frequency=12, start=c(1990,1))

par(mfrow=c(2,1))
plot(svalley.ts[,"sce"], ylab="Santa Clara Jobs")
plot(svalley.ts[,"nasd"], ylab="NASDAQ Index")
par(mfrow=c(1,1))

svalley.comp = svalley.ts[,"sce"] %>% seas(x11="")
sce1 = svalley.comp$data[,'seasonaladj']
sce1.mr = diff(log(sce1))

par(mfrow=c(2,1))
plot(sce1, ylab="Santa Clara Jobs")
plot(svalley.ts[,"nasd"], ylab="NASDAQ Index")
par(mfrow=c(1,1))

svalley.ts = cbind(svalley.ts, sce1, sce1.mr) 

colnames(svalley.ts)=c("time","nasd","nasdr","nasd.yr","nasd.sum12mr","nasd.yr6","nasd.yr12","sce","sce.mr",
                       "sce.yr","sce.sum12mr","sec1","sce1.mr")

# Test to see if NASDAQ Index could predict job growth in Sana Clara County (Silicon Valley)
fit01 = lm(sce1 ~ nasd, data=svalley.ts)       # Non-stationary regression
summary(fit01)                                 # There is a risk of spurious regression: Not recommend!!
fit02 = lm(sce1.mr ~ nasdr, data=svalley.ts)   # sccr.mr: Santa Clara County Monthly Job growth
summary(fit02)
fit03 = lm(sce.yr~ nasd.yr, data= svalley.ts)  # scce.yr and nasdaq.yr: Year-over-year returns
summary(fit03)
fit04 = lm(sce.yr~ nasd.yr6, data=svalley.ts)  # nasdaq.yr6:  YOY returns at 6 month lag
summary(fit04)
fit05 = lm(sce.yr~ nasd.yr12,data=svalley.ts)  # nasdaq.yr12: YOY returns at 12 month lag
summary(fit05)

checkresiduals(fit04) # Model residual shows autocorrelations
coeftest(fit04)
coeftest(fit04, vcov = vcovHAC) 

## 
## Regression with ARIMA errors
##
fit04a = auto.arima(svalley.ts[-c(1:18),"sce.yr"], xreg=svalley.ts[-c(1:18),"nasd.yr6"])
summary(fit04a)
checkresiduals(fit04a)
# But the coefficient of xreg is negative!

# Keep in mind, in order to forecast y(sce.yr), you need to forecast x (nasd.yr) first
n = nrow(svalley.ts)
fcast = forecast(fit04a, xreg=rep(svalley.ts[n,"nasd.yr6"],24))
autoplot(fcast) + xlab("Year") + ylab("Year-over-year Growth Rate")

##
## Insurance example
##

autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")

# Lagged predictors. Test 0, 1, 2 or 3 lags.
head(insurance)
Advert = cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = stats::lag(insurance[,"TV.advert"],-1),
  AdLag2 = stats::lag(insurance[,"TV.advert"],-2),
  AdLag3 = stats::lag(insurance[,"TV.advert"],-3)) %>%
  head(NROW(insurance))

# Restrict data so models use same fitting period
fit1 = auto.arima(insurance[4:40,1], xreg=Advert[4:40,1],stationary=TRUE)
fit2 = auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], stationary=TRUE)
fit3 = auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], stationary=TRUE)
fit4 = auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], stationary=TRUE)

# Choose the optimal lag length for advertising based on the AICs
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

summary(fit2)

# Import the excel data into Data frame --> Best model: One lag (current month and last month)

bestfit = auto.arima(insurance[,1], xreg=Advert[,1:2], stationary=TRUE)
summary(bestfit)

# Assuming the future monthly advertising are 8 units
fcast05 = forecast(bestfit, h=20, xreg=cbind(AdLag0 = rep(8,20),
                                             AdLag1 = c(Advert[40,1], rep(8,19))))
autoplot(fcast05) + ylab("Quotes") + ggtitle("Forecast quotes with future advertising set to 8")

##
## Dynamic OLS Model
##
getSymbols("GDPC1", src="FRED")  
getSymbols("WTISPLC", src="FRED")     # WTI Crude Oil Prices;https://fred.stlouisfed.org/series/WTISPLC#0
getSymbols("CPIAUCSL", src="FRED")    # Consumer Price Index; https://fred.stlouisfed.org/series/CPIAUCSL
oil=apply.quarterly(WTISPLC, mean, na.rm=T)  # Convert average monthly oil prices to quarterly prices
cpi=apply.quarterly(CPIAUCSL, mean, na.rm=T) 

oil.ts=ts(oil,frequency=4, start=c(1946,1))
cpi.ts=ts(cpi,frequency=4, start=c(1947,1))
gdp=ts(GDPC1, frequency=4, start=c(1947,1))

oilg=diff(log(oil.ts))
cpig=diff(log(cpi.ts))
yg=diff(log(gdp))

# A Structural Model
model10a = dynlm(cpig ~ yg + oilg)   
summary(model10a)  
checkresiduals(model10a)  # Residuals have some autocorrelation. Not a good model 

model10aa = dynlm(cpig ~ yg + oilg + L(oilg))   
summary(model10aa)  

model10aaa = dynlm(cpig ~ yg + oilg + L(oilg) + L(oilg,2) + + L(oilg,3))   
summary(model10aaa)  

# AR(1) Model, A Reduced Form model
model10b = dynlm(cpig ~ L(cpig))
summary(model10b) 
checkresiduals(model10b)

# A Mixed Model
model10c = dynlm(cpig ~ L(cpig) + yg + oilg)
summary(model10c) 
checkresiduals(model10c)

model10d = dynlm(cpig ~ L(cpig, 1) + yg + oilg)
summary(model10d) 

model10e = dynlm(cpig ~ L(cpig, 1) + L(cpig, 2) + yg + oilg)
summary(model10e) 

model10ee = dynlm(cpig ~ stats::lag(cpig, -1) + stats::lag(cpig, -2) + yg + oilg)
summary(model10ee) 

model10eee = lm(cpig ~ lag(cpig, 1) + lag(cpig, 2) + yg + oilg)
summary(model10eee) 

model10f = dynlm(cpig ~ L(cpig, 1) + L(cpig, 2) + L(cpig, 3) + yg + oilg)
summary(model10f) 

model10g = dynlm(cpig ~ L(cpig, 1) + L(cpig, 2) + L(cpig, 3)  + L(cpig, 4) + yg + oilg)
summary(model10g) 

model10h = dynlm(cpig ~ L(cpig, 1) + L(cpig, 2) + L(cpig, 3)  + L(cpig, 4) + yg + oilg + L(oilg,1))
summary(model10h) 
checkresiduals(model10h)
