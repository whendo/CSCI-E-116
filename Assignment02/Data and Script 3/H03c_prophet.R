#####################################################################################################
# H03c: Introduction to Facebook Prophet 
# By William Yu, UCLA Anderson Forecast
# 9/23/2024
# R Reference: https://facebook.github.io/prophet/docs/quick_start.html#r-api
# Python Reference: https://facebook.github.io/prophet/docs/quick_start.html#python-api
# If you want to use Prophet in Python, you need to install older version:
# pip install pystan == 2.19.1.1
# pip install fbprophet == 0.7.1
##################################################################################################### 
library(data.table)
library(dplyr)
library(ggplot2)
library(prophet)
library(quantmod) 
library(lubridate)
library(readxl)

setwd("C:/Users/wiyu/documents/Zip08/2024 Q4 Fall_Harvard/Data/")
df = read.table("W08j_manning.txt", header=TRUE)

# The data always come with ds(timedate stamp) and y (data)
m = prophet(df)
future = make_future_dataframe(m, periods = 365)
tail(future)

forecast = predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m, forecast)
prophet_plot_components(m, forecast)

##
## weekly: day of the week
## yearly: day of the year
## daily: within day
## monthly: day of the month
##

##
## Structural Changes
## https://facebook.github.io/prophet/docs/trend_changepoints.html#automatic-changepoint-detection-in-prophet
plot(m, forecast) + add_changepoints_to_plot(m)

# If the trend changes are being overfit (too much flexibility) or underfit (not enough flexibility), 
# you can adjust the strength of the sparse prior using the input argument changepoint_prior_scale. 
# By default, this parameter is set to 0.05. Increasing it will make the trend more flexible.

m1 = prophet(df, changepoint.prior.scale = 0.5)
forecast1 = predict(m1, future)
plot(m1, forecast1)
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Less flexible
m2 = prophet(df, changepoint.prior.scale = 0.001)
forecast2 = predict(m2, future)
plot(m2, forecast2)
tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

##
## Modeling holidays and special events
##
library(dplyr)
playoffs = data_frame(
  holiday = 'playoff',
  ds = as.Date(c('2008-01-13', '2009-01-03', '2010-01-16',
                 '2010-01-24', '2010-02-07', '2011-01-08',
                 '2013-01-12', '2014-01-12', '2014-01-19',
                 '2014-02-02', '2015-01-11', '2016-01-17',
                 '2016-01-24', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
superbowls = data_frame(
  holiday = 'superbowl',
  ds = as.Date(c('2010-02-07', '2014-02-02', '2016-02-07')),
  lower_window = 0,
  upper_window = 1
)
holiday1 = bind_rows(playoffs, superbowls)

m3 = prophet(df, holidays = holiday1)
forecast3 = predict(m3, future)
tail(forecast3[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

# Holiday effect can be seen
forecast3 %>% 
  select(ds, playoff, superbowl) %>% 
  filter(abs(playoff + superbowl) > 0) %>%
  tail(10)

prophet_plot_components(m3, forecast3)

# Built-in Country Holidays
m4 = prophet(holidays = NULL)
m4 = add_country_holidays(m4, country_name = 'US')
m4 = fit.prophet(m4, df)
m4$train.holiday.names

# UK
m5 = prophet(holidays = NULL)
m5 = add_country_holidays(m5, country_name = 'UK')
m5 = fit.prophet(m5, df)
m5$train.holiday.names

forecast = predict(m, future)
prophet_plot_components(m, forecast)

##
## Fourier Order for Seasonalities
##
m = prophet(df)
prophet:::plot_yearly(m)

# The default values are often appropriate, but they can be increased 
# when the seasonality needs to fit higher-frequency changes, 
# and generally be less smooth. 

m6 = prophet(df, yearly.seasonality = 20)
prophet:::plot_yearly(m6)

# Prophet will by default fit weekly and yearly seasonalities, 
# if the time series is more than two cycles long. It will also fit daily 
# seasonality for a sub-daily time series. You can add other seasonalities 
# (monthly, quarterly, hourly) using the add_seasonality method (Python) or function (R).

m7 = prophet(weekly.seasonality=FALSE)
m7 = add_seasonality(m7, name='monthly', period=30.5, fourier.order=5)
m7 = fit.prophet(m7, df)
forecast7 = predict(m7, future)
prophet_plot_components(m7, forecast7)

# Additional regressors
nfl_sunday = function(ds) {
  dates = as.Date(ds)
  month = as.numeric(format(dates, '%m'))
  as.numeric((weekdays(dates) == "Sunday") & (month > 8 | month < 2))
}
df$nfl_sunday = nfl_sunday(df$ds)
m8 = prophet()
m8 = add_regressor(m8, 'nfl_sunday')
m8 = fit.prophet(m8, df)
future$nfl_sunday = nfl_sunday(future$ds)
forecast8 = predict(m8, future)
prophet_plot_components(m8, forecast8)

##
## Sub-daily data
##
disney = read.csv("W08h_disney_waitime.csv")
disney$datetime = mdy_hm(disney$datetime)
disney2 = disney %>% subset(waittime!=-999)
colnames(disney2) = c("ds","y")

m9 = prophet(disney2)
future = make_future_dataframe(m9, periods = 300, freq = 60 * 60)
fcst = predict(m9, future)
prophet_plot_components(m9, fcst)

##
## Exercise on the Stock Market Prices
##
getSymbols("SP500", src="FRED")   # https://fred.stlouisfed.org/series/SP500#0
plot(SP500)
index(SP500)
sp500=data.frame(SP500)
sp500.df=cbind(index(SP500), sp500)
colnames(sp500.df)=c("ds","y")
m10 = prophet(sp500.df)
future = make_future_dataframe(m10, periods = 300)
forecast10 = predict(m10, future)
tail(forecast10[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m10, forecast10)
prophet_plot_components(m10, forecast10)

##
## TSA Air Travel
##
tsa = read_excel("W09e_tsa.xlsx") 
tsa_test = read_excel("W09e_tsa.xlsx", sheet="test") 

tsa %>% ggplot() +
  geom_line(aes(x=ds, y=travel_train/1000000), color="blue") +
  ylab('# of daily air travelers (Million)') 

tsa = tsa %>% rename(y=travel_train)
m11 = prophet(tsa)
future = make_future_dataframe(m11, periods = 81) # Size of testset
tail(future)
forecast11 = predict(m11, future)
tail(forecast11[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m11, forecast11)
prophet_plot_components(m11, forecast11)

forecast11a = subset(forecast11, ds>=as.Date("2024-07-01"))

ggplot() +
geom_line(data=forecast11a, aes(x=ds, y=yhat), color="blue") +  
geom_line(data=tsa_test, aes(x=ds, y=travel_test), color="red") 

# Long-term forecast
m12 = prophet(tsa)
future = make_future_dataframe(m12, periods = 365)
tail(future)
forecast12 = predict(m12, future)
tail(forecast12[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m12, forecast12)
prophet_plot_components(m12, forecast12)


##
## Note: Web data scraping via R
##

# Highlight the website's data, usually in the beginning of a table. Right click the mouse and select "Inspect",
# Find the "<table" and click it and then right click the mouse and select "copy" 
# and choose "copy Xpath" and paste it into html_nodes(xpath=' ')

library(rvest)
url = 'https://www.tsa.gov/travel/passenger-volumes'
webdata = url %>% read_html() %>%
  html_nodes(xpath = '//*[@id="block-mainpagecontent"]/div/div/div/div/div/div[2]/div/div/div/div/div[2]/table') %>%
  html_table(fill = TRUE)
df1 = webdata[[1]]
write.csv(df1,"tsa.csv")
