######################################################################################################
# H09b: Google Causal Impact
# By William Yu, UCLA Anderson Forecast
# 3/4/2025
# https://google.github.io/CausalImpact/CausalImpact.html
# https://static.googleusercontent.com/media/research.google.com/en//pubs/archive/41854.pdf
######################################################################################################
# install.packages("CausalImpact")
library(CausalImpact)

# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
# Pre-intervention period (70 days)
pre_period = 1:70
post_period = 71:100

# Create a baseline trend for sales
sales = 100 + arima.sim(model = list(order = c(1, 0, 0), ar = 0.8), n = 100)

# Simulate an impact for the post-intervention period
sales[post_period] = sales[post_period] + 20

# Generate control variables (not affected by the intervention)
control1 = 90 + arima.sim(model = list(order = c(1, 0, 0), ar = 0.8), n = 100)
control2 = 110 + arima.sim(model = list(order = c(1, 0, 0), ar = 0.8), n = 100)

# Combine data into a data frame
data = cbind(sales, control1, control2)

# Define pre and post periods
pre_period_range = c(1, 70)
post_period_range = c(71, 100)

# Run Causal Impact analysis
impact = CausalImpact(data, pre_period_range, post_period_range)

# Print summary and detailed report
summary(impact)
summary(impact, "report")

# Plot the results
plot(impact)
