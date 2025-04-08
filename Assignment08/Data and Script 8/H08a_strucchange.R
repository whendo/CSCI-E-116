################################################################################ 
# H08a: Structural Change
# By William Yu, UCLA Anderson Forecast
# updated 10/19/2022
# Reference: https://cran.r-project.org/web/packages/strucchange/strucchange.pdf
# https://cran.r-project.org/web/packages/strucchange/vignettes/strucchange-intro.pdf
################################################################################
install.packages("strucchange")
library(readxl)  
library(dplyr)
library(strucchange)  

## Nile data with one breakpoint: the annual flows drop in 1898
## because the first Ashwan dam was built
data("Nile")
plot(Nile)

## Compute breakpoints
bp.nile = breakpoints(Nile ~ 1)
summary(bp.nile)

## Fit and visualize segmented and unsegmented model
fm0 = lm(Nile ~ 1) # 1 is for the intercept of a flat trend.
summary(fm0)

fm1 = lm(Nile ~ breakfactor(bp.nile, breaks = 1))
summary(fm1)

plot(Nile)
lines(ts(fitted(fm0), start = 1871), col = 3)
lines(ts(fitted(fm1), start = 1871), col = 4)
lines(bp.nile, breaks = 1)

## confidence interval
ci.nile = confint(bp.nile)
ci.nile
lines(ci.nile)

## UK Seatbelt data: a SARIMA(1,0,0)(1,0,0)_12 model
## (fitted by OLS) is used and reveals (at least) two
## breakpoints - one in 1973 associated with the oil crisis and
## one in 1983 due to the introduction of compulsory
## wearing of seatbelts in the UK.
data("UKDriverDeaths")
?UKDriverDeaths
seatbelt = log10(UKDriverDeaths)
plot(seatbelt)
seatbelt = cbind(seatbelt, stats::lag(seatbelt, k = -1), stats::lag(seatbelt, k = -12))
colnames(seatbelt) = c("y", "ylag1", "ylag12")
seatbelt = window(seatbelt, start = c(1970, 1), end = c(1984,12))
plot(seatbelt[,"y"], ylab = expression(log[10](casualties)))

## Testing -- Empirical Fluctuation Process
fit01 = lm(y ~ ylag1 + ylag12, data = seatbelt)
summary(fit01)
re.seat = efp(y ~ ylag1 + ylag12, data = seatbelt, type = "RE")
plot(re.seat)

## Dating
bp.seat = breakpoints(y ~ ylag1 + ylag12, data = seatbelt, h = 0.1)
summary(bp.seat)
lines(bp.seat, breaks = 2)

# fm1 = lm(seatbelt ~ breakfactor(bp.seat, breaks = 2))
# summary(fm1)

## load and plot data
data("PhillipsCurve")
?PhillipsCurve
uk = window(PhillipsCurve, start = 1948)
plot(uk[, "dp"])

## AR(1) inflation model
## estimate breakpoints
fit02 = lm(dp ~ dp1, data = uk)
summary(fit02)

bp.inf = breakpoints(dp ~ dp1, data = uk, h = 8) 
# minimal segment size (fraction or # of observations)
summary(bp.inf)

## fit segmented model with three breaks
fac.inf = breakfactor(bp.inf, breaks = 2, label = "seg")
fm.inf = lm(dp ~ 0 + fac.inf/dp1, data = uk)
summary(fm.inf)

## Phillips curve equation
## estimate breakpoints
fit03 = lm(dp ~ dp1 + dw + du, data = uk)
summary(fit03)

bp.pc = breakpoints(dp ~ dp1 + dw + du, data = uk, h = 5, breaks = 5)
summary(bp.pc)

## Fit segmented model with three breaks
fac.pc = breakfactor(bp.pc, breaks = 2, label = "seg")
fm.pc = lm(dp ~ 0 + fac.pc/dp1 + fac.pc/dw + fac.pc/du, data = uk)
summary(fm.pc)

