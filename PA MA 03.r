setwd("C:/Users/aleen/Desktop/Data Science/Predictive Analytics/Assignments/MA 03/")
getwd()

library(AER)
library(ggplot2)
library(urca)
# 1
data(USMacroSW)
#View(USMacroSW)
#end(USMacroSW) # 2005
?USMacroSW
ts1 <- ts(USMacroSW[,c(6)], frequency=4, start=1970, end=end(USMacroSW))
# 1.1
typeof(ts1)
autoplot(ts, main="GBP/USD exchange rate 1970-2005")
ggAcf(ts1, main="GBP/USD exchange rate ACF")
ggPacf(ts1, main="GBP/USD exchange rate ACF")
ts1 %>% ur.kpss() %>% summary() # Not stationary
ts1 %>% diff() %>% ur.kpss() %>% summary() # Stationary with one differencing
ts2 = diff(ts1)
ts2 <- na.omit(ts2)
autoplot(ts2, main="Change in GBP/USD exchange rate 1970-2005")
ggAcf(ts2, main="Change in GBP/USD exchange rate ACF")
ggPacf(ts2, main="Change in GBP/USD exchange rate PACF")
# 1.2 
train1 <- window(ts2, start=start(ts2), end=c(1998,4))
test1 <- window(ts2, start=c(1999), end=end(ts2))
fit001 <- Arima(train1, order=c(0,0,1))
summary(fit001)
checkresiduals((fit001))
fit100 <- Arima(train1, order=c(1,0,0))
summary(fit100)
checkresiduals(fit100)
fit101 <- Arima(train1, order=c(1,0,1))
summary(fit101)
checkresiduals(fit101)
# 1.3 
fcast <- forecast(fit001,h=29)
autoplot(fcast) + xlab("Year") +
  ylab("Forecast of Change in GDP/USD exchange")
# 1.4
arima1.4 <- auto.arima(ts1, seasonal=FALSE, stepwise=FALSE, approximation=FALSE) 
summary(arima1.4) # ARIMA(2,1,3)
forecast(arima1.4) # 169.5953 = 1.695 vs actual 1.779

# 2 
data(USMacroG)
# 2.1
ts2 <- ts(USMacroG[,c(1,2,4)], frequency = frequency(USMacroG))
autoplot(ts2, facets=TRUE)
ts2_growth <- diff(log(ts2))
ts2_growth <- na.omit(ts2_growth)
gdp_growth <- ts2_growth[,1]
consumption_growth <- ts2_growth[,2]
government_growth <- ts2_growth[,3]
# Simple linear regression
ts2 %>% as.data.frame() %>% ggplot(aes(x = consumption, y = gdp)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
(fit1 <- tslm(gdp_growth ~ consumption_growth))
checkresiduals(fit1)
# Multivariate linear regression 
(fit2 <- tslm(gdp_growth ~ consumption_growth + government_growth))
checkresiduals(fit2)

# 2.2
arima2.2.1 <- auto.arima(consumption_growth, seasonal=FALSE, stepwise=FALSE, approximation=FALSE) 
summary(arima2.2.1)
arima2.2.2 <- auto.arima(government_growth, seasonal=FALSE, stepwise=FALSE, approximation=FALSE) 
summary(arima2.2.2)
forecast(arima2.2.1,h=4)
forecast(arima2.2.2,h=4)

intercept <- 0.0010964
coef_consumption_growth <- 0.7229458
coef_government_growth <-  0.1605951

gdp_growth1 <- intercept + coef_consumption_growth * 0.008779564 + coef_government_growth * 0.006694601
gdp_growth2 <- intercept + coef_consumption_growth * 0.008659066 + coef_government_growth * 0.006874341
gdp_growth3 <- intercept + coef_consumption_growth * 0.008912426 + coef_government_growth * 0.007001854
gdp_growth4 <- intercept + coef_consumption_growth * 0.008912426 + coef_government_growth * 0.007092316

gdp_growth1 # 0.008518669
gdp_growth2 # 0.008460421
gdp_growth3 # 0.008664064
gdp_growth4 # 0.008678592

arima2.2.3 <- auto.arima(gdp_growth, seasonal=FALSE, stepwise=FALSE, approximation=FALSE) 
forecast(arima2.2.3, h=4) # Very similar results. 

  