setwd("C:/Users/aleen/Desktop/Data Science/Predictive Analytics/Assignments/")
getwd()

library(fpp2)
library(ggplot2)

# Load the data ItalyGDP.csv and transform the dataset into a ts object. 
# The dataset includes information on Italian GDP from 1970 to 2016.

df <- read.csv("C:/Users/aleen/Desktop/Data Science/Predictive Analytics/Assignments/MA 02/ItalyGDP.csv", header=TRUE)
gdp <- ts(df[,c(7)], frequency = 1, start=c(1970), end=c(2016))


#################################################################################
# 1: Plot the data and the autocorrelation function. 
# Describe the properties of the series and test for stationarity using KPSS test. 

autoplot(gdp, main = "Italy's GDP") # The mean is not constant, but increasing over time. 
ggAcf(gdp, main = "ACF")

# The time series has a clear and pronounced trend of growth and exhibits no signs of seasonality. 
# There is evidence of small cycles of diminishing GDP.
# After the 11th lag, the autocorrelation stops being significant. 
# The data is clearly non-stationary. We will have to address the trend by differenciating the time series. 
# There is no aggresive variancce so there is no need to use logarithms. 

# "One way to determine more objectively whether differencing is required is to use a unit root test."
# I will use the KPSS test. 
library(urca)
gdp %>% ur.kpss() %>% summary()
library(tseries)
gdp %>% ur.kpss(type="tau") %>% summary()
gdp %>% ur.kpss(type="mu") %>% summary()
# summary(kpss.test(gdp, null = "Trend"))
# H0: The data is stationary. H1: The data is not stationary. 
# The test statistic is larger than all of the critical values. 
# The null hypothesis is rejected. The data is not stationary.
ndiffs(gdp) # It seems it is recommended we difference the data one time

gdp %>% diff() %>% autoplot(main="Change in Italy's GDP")
ggAcf(diff(gdp), main="ACF of change in Italy's GDP")
ggPacf((diff(gdp)), main="PACF of change in Italy's GDP") # Both the ACF and the PACF suggest the ts is now white noise.
gdp %>% diff() %>% ur.kpss() %>% summary() # The test statistic is significantly lower than the critical values. stationarity is again suggested through KPSS.
# Now the data clearly is white noise. 
#################################################################################

#################################################################################
# 2: Transform the series into GDP growth using the formula below and test for unit root using 
# Augmented Dickey Fuller test. Make sure you specify the correct features of the data. Is the series stationary?
# GDP_growth = log(xt −xt−1)
gdp_growth = diff(log(gdp)) # log(diff()) gives NaNs because some of the differences are negative. 
autoplot(gdp_growth, main="Italy's GDP growth") # The new time series looks like a random walk with drift. 
ndiffs(gdp_growth) # It is suggested we difference it again. 
summary(ur.df(gdp_growth, type="drift", selectlags="AIC"))
# H0: a unit root is present; not stationary; H1: stationarity / trend stationarity 
# The test statistic is lesser than the critical values at all levels of confidence, 
# which means I accept the H0, therefore, the series is not stationary. 
#################################################################################

#################################################################################
# 3: Look at the autocorrelation and partial autocorrelation function, choose a model for the series and 
# write down the equation of the model. Estimate the parameters of your chosen model using Arima() function 
# and write the resulting model (Hint: Arima() function returns an estimate of the unconditional mean of the process, 
# not of the intercept..) 
ggAcf(gdp_growth, main="ACF of Italy's GDP growth") # A larger spike every 5th lag suggests seasonality. 
# I It will be impossible to use the autocorrelation function to
# distinguish between a random walk and a very persistent stationary AR
ggPacf(diff(gdp_growth), main="PACF of Italy's GDP growth") # First lag is significant, with a spike in the 5th, almost significant.
# or using ggtsdisplay(gdp_growth)
# The ACF is decaying exponentially
ndiffs(gdp_growth)

arima1 <- auto.arima(diff(gdp_growth), seasonal=FALSE, stepwise=FALSE, approximation=FALSE) # We make sure R works out the best model fit. 
arima1
autoplot(forecast(arima1))
# It is a Simple Exponential SMoothening Model = SES 
#################################################################################


#################################################################################
# 4: Check the residuals of your model. Do they satisfy the assumptions about the residuals we made in class?
checkresiduals(arima1)
# The ACF plot of the residuals  shows that all autocorrelations are within the threshold limits;
# The residuals are behaving like white noise. 
# A portmanteau test returns a large p-value of 0.6781, which also suggests that the residuals are white noise.
#################################################################################


#################################################################################
# 5: Estimate an innovation states space model using the ets function for your growth series. 
# Comment on your model: do the features estimated with ets()correspond to those you found above 
# (in terms of trend and seasonality)? 
# start(gdp_growth) # 1971 1
# end(gdp_growth) # 2016 1 
# train <- window(gdp_growth, start=1971,end=2007)	
# test <- window(gdp_growth, start=2007,end=2016)
#ets1 <- ets(train)s
ets1 <- ets(gdp_growth)
summary(ets1)
autoplot(ets1)
checkresiduals(ets1)
cbind('Residuals' = residuals(ets1),
      'Forecast errors' = residuals(ets1,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")
# The model ETS(A,N,N) is a SES with Additive Errors
# The model is the equivalent of the ARIMA(0,1,1,)
# AICc cannot be used to compare models that are not in the same class. we cannot compare our ARIMA to our ETS
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

fets(diff(gdp_growth),9)
farima(gdp_growth, 9)
# Compute CV errors for ETS as e1
e1 <- tsCV(gdp_growth, fets, h=1)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(gdp_growth, farima, h=1)
# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
# The ETS model has a smaller tsCV statistic.
# The model has no multiplicative errors so the residuals are equivalent to the one-step training errors. 
#################################################################################


#################################################################################
# 6: Forecast the growth rate of Italian GDP in 2019 using the model you estimated in the previous question. 
# The forecast of the OECD is 0.93%. How does your model perform?
ets1 %>% forecast(h=3) %>%
  autoplot() +
  ylab("Forecast of growth rate in Italy's GDP")
# 0.0004811827 = 0.04% is my forecast for 2019. Much lower than the OECD's forecast. 
forecast(ets1,3)

exp(0.0004811827) # => 1.000481

ets1 <- ets(diff(gdp_growth))
summary(ets1)
autoplot(ets1)
checkresiduals(ets1)
cbind('Residuals' = residuals(ets1),
      'Forecast errors' = residuals(ets1,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")
# The model ETS(A,N,N) is a SES with Additive Errors
# The model is the equivalent of the ARIMA(0,1,1,)
# AICc cannot be used to compare models that are not in the same class. we cannot compare our ARIMA to our ETS
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

fets(diff(gdp_growth),9)
farima(gdp_growth, 9)
# Compute CV errors for ETS as e1
e1 <- tsCV(gdp_growth, fets, h=1)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(gdp_growth, farima, h=1)
# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
# The ETS model has a smaller tsCV statistic.
# The model has no multiplicative errors so the residuals are equivalent to the one-step training errors. 
#################################################################################


#################################################################################
# 6: Forecast the growth rate of Italian GDP in 2019 using the model you estimated in the previous question. 
# The forecast of the OECD is 0.93%. How does your model perform?
ets1 %>% forecast(h=3) %>%
  autoplot() +                                             
  ylab("Forecast of growth rate in Italy's GDP")
# 0.0004811827 = 0.04% is my forecast for 2019. Much lower than the OECD's forecast. 
forecast(ets1,3)

exp(-0.0001027009) # => 0.9998973
