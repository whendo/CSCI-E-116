######################################################################################################
# H09a Bayesian Structural Time Series Model (BSTS) with regressors
# By William Yu, UCLA Anderson Forecast
# 4/8/2025
# https://en.wikipedia.org/wiki/Bayesian_structural_time_series
# https://people.ischool.berkeley.edu/~hal/Papers/2012/fat.pdf
###################################################################################################### 
# install.packages("bsts")
library(bsts)

# You could compare this method to the ones you tries in Assignment 5!!

set.seed(42)
n = 100  # Number of observations

# Generate a time series of sales with a trend and seasonality
time = 1:n
trend = 0.3 * time
seasonality = 10 * sin(2 * pi * time / 12)
random_noise = rnorm(n, mean = 0, sd = 3)
sales = trend + seasonality + random_noise
plot(sales, type="l")

# Add a marketing campaign variable (regressor)
campaign = ifelse(time > 50 & time < 80, 1, 0)  # Marketing active from time 51 to 79
sales = sales + 5 * campaign  # Adding the effect of the campaign
plot(sales, type="l")

# Combine into a data frame
data = data.frame(
  time = time,
  sales = sales,
  campaign = campaign
)

# Create the state space with trend and seasonality components
ss = AddLocalLinearTrend(list(), data$sales)
ss = AddSeasonal(ss, data$sales, nseasons = 12)  # Monthly seasonality

# Fit the BSTS model with campaign as a regressor
model = bsts(
  sales ~ campaign,
  state.specification = ss,
  niter = 1000,  # Number of MCMC iterations
  data = data
)

# Plot the observed vs. predicted sales
plot(model, main = "BSTS Model: Observed vs. Predicted Sales")

# Plot the components
plot(model, "components")

# Plot the impact of the campaign
impact = summary(model)
print(impact)

# Forecasting the next 50 observations
horizon = 50

# Create a new data frame with future values for the campaign regressor
newdata = data.frame(
  campaign = rep(0, horizon)  # Assume no campaign in the next 50 periods; adjust as needed
)

pred = predict(model, horizon = horizon, newdata=newdata)

# Plot the forecast results
plot(pred, main = "BSTS Forecast for Sales with Campaign Effect")  # Do not specify xlim here

# Add historical data with the correct time range
lines(1:n, data$sales, col = "blue")  # Plot historical data in blue

# Add forecasted points
points((n + 1):(n + horizon), pred$mean, col = "red", pch = 19)  # Forecasted values

# Add legend
legend("topright", legend = c("Historical Data", "Forecast"), col = c("blue", "red"), pch = c(NA, 19), lty = c(1, NA))
