library(tseries)
library(lmtest)
library(urca)
library(forecast)

# Test for getting optimal model
data = read.csv("RawDataWithoutDate.csv")
summary(lm(WTI~worldwide_car_sale+US_trade_balance+US_unemployment_people+tesla_stock_price+US_natural_gas_price+domestic_renewable_energy+domestic_nuclerar_energy+US_interest_rate+gold_price ,data=data))

# Tentative best model which has the largest adjusted R squared value
model = lm(WTI~gold_price+domestic_nuclerar_energy+US_unemployment_people+tesla_stock_price+US_natural_gas_price,data=data)
summary(model)

###
# Diagnosis
###
# Correlation
cor(data)

# Heteroscedasticity test
bptest(model)
gqtest(model)

# DW p value 0~4. 2정도면 no-autocorrealtion
# Autocorrelation test
dwtest(model)
bgtest(model)

# Normality test
jarque.bera.test(resid(model))


###
# Time Series + ARIMA
###

# Set time series variable
WTIts = ts(data=data$WTI, start=c(2017,01), end=c(2019,12), frequency = 12)
WTIts
plot(WTIts, main="WTI")

# Unit Test for WTIts. Test whether WTI is stationary or not
WTIts.unit = ur.df(WTIts, lags=1, type='drift')
summary(WTIts.unit)  # non-stationary

# Difference to make it stationary
# 차분을 통해 데이터 정상화(stationary)
WTIts.diff1 = diff(WTIts, differences = 1)
WTIts.diff2 = diff(WTIts, differences = 2)
WTIts.diff3 = diff(WTIts, differences = 3)

# We can see the stationary pattern
# 차분을 통해 정상화(stationary) 패턴을 확인
plot(WTIts, main="WTI")
plot(WTIts.diff1, main="WTI.diff1")
plot(WTIts.diff2, main="WTI.diff2")
plot(WTIts.diff3, main="WTI.diff3")

# Test whether it is stationary or not by ADF
# ADF를 통해 정상화(stationary) 확인
summary(ur.df(WTIts.diff2, lags=1, type='drift')) # stationary
summary(ur.df(WTIts.diff3, lags=1, type='drift')) # more stationary

# ACF and PACF for getting q and p
# 차분한 데이터로 ARIMA 모형 확인
acf(WTIts.diff3, lag.max = 30)  # lag 2부터 점선 안에 존재. lag 절단값 = 2 --> MA(1)
pacf(WTIts.diff3, lag.max = 30) # lag 6부터 점선 안에 존재. lag 절단값 = 6 --> AR(5)


# My ARIMA model, ARIMA(5,3,1)
# where d=3, p=5, q=1
WTIts.arima = arima(WTIts, order = c(5,3,1))


# Automatically detect optimal ARIMA parameters
# 자동으로 ARIMA 모형 확인  
WTIts.autoArima = auto.arima(WTIts) # (1,0,0)


# ARIMA 비교
WTIts.arima = arima(WTIts, order = c(5,3,1))
WTIts.arima
WTIts.autoArima

# AIC (the lower, the better)
# autoArima: 206.59, arima: 209.06

# Predict & Plot
WTIts.autoArima.forecast = forecast(WTIts.autoArima)
WTIts.arima.forecast = forecast(WTIts.arima)
plot(WTIts.autoArima.forecast, ylim=range(30, 90))
plot(WTIts.arima.forecast, ylim=range(30, 90))
