
# What is the working directory?
setwd("C:/Users/aleen/Desktop/Data Science/Predictive Analytics")
getwd()

# Install AER
#install.packages("AER")

# Import necessary packages
library(ggplot2) # Needed for plots 
library(fpp2) # Needed for datasets
library(AER) # Needed for datasets 

###############################################################
##### QUESTION 1 #####

# Loading the data 
data(CPS1985)
# Information about the data set
?CPS1985
# Heag of the data frame
head(CPS1985)
# Shape of data frame 
dim(CPS1985) # 534 observations of 11 variables 
# Previewing the data frame
head(CPS1985, 5)
# Viewing the data frame
View(CPS1985) 

#########################################
### 1.1 DESCRIPTIVE AND SUMMARY STATISTICS ###
# Describe the dataset and present summary statistics for the variables wage, education, experience and wage.

# Call the variable/column names
names(CPS1985)
# What is the structure of the variables? 
str(CPS1985)
# Summary statistics for wage, eduation, experience and age
summary(CPS1985[,c(1:4)])


#########################################
### 1.2 PLOTTING RELATIONSHIP BETWEEN WAGE AND EXPERIENCE ###
# what can you say about the relationship between these two variables? Plot the fitted values obtained from the following regression model: simple linear regression. 

# Create a new data set containing only the variables we need for the simple linear regression
reg_data = CPS1985[,c(1,3)] 
# Summary 
summary(reg_data)
# Histogram of the variable Experience. Blue = Median; Red = Mean
qplot(reg_data[,2], geom="histogram", binwidth=1) +
  labs(title = "Historgram of Average Years of Experience") +
  labs(x ="Average Years of Experience") +
  labs(y = "Frequency") +
  scale_y_continuous(breaks = c(1:30), minor_breaks = NULL) +
  scale_x_continuous(breaks = c(0:55), minor_breaks = NULL) +
  geom_vline(xintercept = mean(reg_data[,2]), show.legend=TRUE, color="red") +
  geom_vline(xintercept = median(reg_data[,2]), show.legend=TRUE, color="blue")
# Histogram of the variable Wage. Blue = Median; Red = Mean
qplot(reg_data[,1], geom="histogram", binwidth=1) +
  labs(title = "Historgram of Average Wage") +
  labs(x ="Average Wage per Hour") +
  labs(y = "Frequency") +
  scale_x_continuous(breaks = c(1:44.5), minor_breaks = NULL) +
  geom_vline(xintercept = mean(reg_data[,1]), show.legend=TRUE, color="red") +
  geom_vline(xintercept = median(reg_data[,1]), show.legend=TRUE, color="blue")
# Scatterplot of the two variables 
qplot(wage, experience, data=reg_data, main = "Relationship between Wage and Experience") 

# According to the scatterplot, there is no clear Linearity
# A linear Regression, at least not one performed without transforming the variables, is not advised
# This is a rigid model due to omitted variable bias. 
# Should we remove the outliers?
# Should we use log transformation?
# The scatterplot illustrates a high level of heterosccedasticity. 

qplot(log(wage), experience, data=reg_data, main = "Relationship between Income and Experience") 
# I applied log to wage
qplot(log(experience), log(wage), data=reg_data, main = "Relationship between Income and Experience") 
# How about this model?

# Formula: wage <- a + b * exp + e; wage = intercept + slope * experience 
# Creating the model with lm(linear models)
model1 <- lm(wage ~ experience, data = CPS1985) 
# Summary statistics for the model
summary(model1)
library(tidyverse)
p <- ggplot(reg_data, aes(x=experience, y=wage)) + # Basis for plot
  geom_point() + # Add points for scatterplot
  geom_smooth(method=lm) + # Regression line
  labs(x="Experience", y="Wage",title="Simple Linear Regression Model")
print(p)

# Intercept = 8.37997. This is the minimum wage/hour, when the worker's experience is zero.
# Slope = 0.03614. It shows how much the wage changes when the experience increases by 1 unit. 
# When the worker gets 1 more year of experience, on average, his wage increases by 36 cents.
# R-Squared = 0.00757. This statistic shows the proportion of variation in the wage. 
# This means that experience alone can explain just 0.75% of the wage. 

fitted <- model1$fitted.values # These are the fitted values of the model. 
qplot(fitted, main = "Fitted values") # Plotting the fitted (predicted) values 
plot(fitted, CPS1985$wage) # Indeed, not a very accurate model. 

#########################################
### 1.3 MULTIVARIATE REGRESSION ###
# Use a regression model to explain determinants of wage. 
# Include as explanatory variables education, experience, ethnicity, gender and union. 

df <- data.frame() # Empty data frame
df <- CPS1985[,c(1,2,3,5,7,10)] # We only need these variables
head(df) # testing
levels(df$gender) # This is a feature type of variable 
df$gender <- as.numeric(df$gender) # transform the factorials (categoricals) to numericals
df$ethnicity <- as.numeric(df$ethnicity) # For the purpose of statistical inference
df$union <- as.numeric(df$union)
cor(df) # A correlation matrix, now possible after transforming the categorical values to numerical ones. 

model2 <- lm(wage ~ education + experience + ethnicity + gender + union , data = df)
summary(model2) # show results

# Intercept = -2.83875 (Base wage)
# Ethnicity has a corresponding p-value of 0.057, which is slightly higher than the alpha significance level of 0.5
# This renders the variable ethnicity as not statistically significant
# Does the p-value for the intercept bear any meaning (it is also : 0.5)


model3 <- lm(wage ~ education + experience + gender + union , data = df)
summary(model3) # show results

# R-Squared Adjusted: it is less than in the above model. Does this mean that by substracting the "insignificant"
# variable, we now have a less reliable model? 

anova(model2, model3)

# According to the ANOVA, the test shows an unsignificant result (0.057). 
# We should reject model 3 and stick to model 2. 

anova(model1, model2) 
# This time, clearly, model2 is superior to model1. 



 ###############################################################
##### QUESTION 2 #####

?USConsump1993
data(USConsump1993)
head(USConsump1993)
dim(USConsump1993) # 2 variables over 44 years
frequency(USConsump1993) # So, yearly, clearly
class(USConsump1993) # It is indeed a time series type. 

#########################################
### 2.1: PLOTTING EXPENDITURE TIME SERIES ###
#Plot the series for expenditure. Identify relevant features. Use different plots if needed.

expenditure = USConsump1993[,1] # Create a separate time series for expenditure. 
autoplot(expenditure, main = "Expenditure Time Series") # There is a trend of growth. 
gglagplot(expenditure, main = "Expenditure Lag Plot", lags = 12) # First three lags show strong autocorrelation.
ggAcf(expenditure, main = "Expenditure ACF", lag.max = 12) # Again, we can see a trend, with no seasonality and no cyclicity.

#########################################
### 2.2: FORECASSTING THE EXPENDITURE  ###
# Calculate 5-periods ahead forecast for the expenditure series. 
# Use average, naive and drift methods. Show your results in one graph and comment on the forecasts.
# Check the residuals of your forecast. 
# See if they present correlation and if they are normally distributed.

autoplot(expenditure) + autolayer(meanf(expenditure, h = 5), series = "Mean", PI = FALSE) +
  autolayer(naive(expenditure, h=5), series="Naive", PI=FALSE) +
  autolayer(rwf(expenditure, h=5), series="Drift", PI=FALSE) +
  ggtitle("Different types of basic forecasts") + 
  ylab("USD") + guides(color=guide_legend(title="Forecast type"))

# Drift and naive show the exact same forecast. Drift is equal to last value plus average change.
# Apaprently, the average change might just be insignificant. 

meanf <- meanf(expenditure)
autoplot(residuals(meanf))
checkresiduals(meanf, plot = TRUE) # Correlationseen in ACF plot, Distribution is somewhat normal. Mean close to 0. Variation pretty much the same across ts. Huge Q* shows autocorrelation is not from white noise ts. 
 mean(residuals(meanf))
naive <- naive(expenditure)
autoplot(residuals(naive))
checkresiduals(naive, plot = TRUE) # White noise in ACF. Distribution is right skewed. Mean not close to 0. Greaat variation in residuals..
mean(residuals(naive))
rwf <- rwf(expenditure, h = 5)
autoplot(residuals(rwf))
mean(residuals(rwf))
checkresiduals(rwf) # White noise seen in ACF. Distribution is right skewed. Most residuals are positive.  Mean not close to 0. 
resid <- residuals(rwf)
hist(resid)
mean(resid)
Box.test(resid, lag=12, fitdf=0)
Box.test(resid, lag=12, fitdf=0, type="Lj") # For Q and Q* the p values are large, the result is thus not significant. 
# Thus, the residuals are not distinguishable from a white noise series. 
qplot(resid, geom="histogram") +
  labs(title = "Historgram of Average Years of Experience") +
  labs(x ="Average Years of Experience") +
  labs(y = "Frequency") +
  geom_vline(xintercept = mean(resid), show.legend=TRUE, color="red") 
meanf <- meanf(expenditure, na.action=na.exclude)
# The mean forecasting model is not a good forecasting method, at least in this case. 
# Assumptions: 
# 1) The residuals are uncorrelated. If there are correlations between residuals, then there is information left in the residuals which should be used in computing forecasts.
# 2) The residuals have zero mean. If the residuals have a mean other than zero, then the forecasts are biased.


#########################################
### 2.3: EXPONENTIAL SMOOTHENING ###
# Split the sample into a training set and a test set. Forecast the expenditure series using at least 2 exponential smoothing methods.
# (decide how long the forecast horizon will be)
# Compare the accuracy of your chosen methods and choose the best method, motivating your answer.
autoplot(expenditure)
train <- window(expenditure, end=end(expenditure)-c(9,0))	# 20% test
test <- window(expenditure, start=end(expenditure)-c(9,0), end=end(expenditure))
fcast1 <- ses(train, h=9) # trend. no seasonality
autoplot(fcast1) + 
  autolayer(train)
fcast2 <- holt(train, h=9)
fcast3 <- holt(train, h=9, damped=TRUE, phi=0.9)
autoplot(fcast2) + 
  autolayer(expenditure)
autoplot(fcast3) + 
  autolayer(expenditure) # Visually, the Damped Holt's Method seems to give us a forecast that is closer to the truth. 
fcast4 <- holt(test, h=9, damped=TRUE, phi=0.9)
autoplot(fcast4) + 
  autolayer(train) # By specifying train as autolayer, we also get the test series, to better understand the whole picture.

# I used time series cross-validation to compare the nine-step forecast accuracy of the methods 
e1 <- tsCV(expenditure, ses, h=9)
e2 <- tsCV(expenditure, holt, h=9)
e3 <- tsCV(expenditure, holt, damped=TRUE, h=9)
# Compare MSE 
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE) # Normal Holt has lowest MSE, so it is the better model. 
# Compare MAE
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE) # Normal Holt has lowest MAE, so it is the better model. 

fcast2[["model"]] # Alpha close to 1, showing the level reacts strongly to each new observation. Beta is almost 0, showing little change in trend. 
# The forecast plot shows a narrow prediction interval, accounting for the minimal changes in the time series. 

###############################################################
##### QUESTION 3 #####

data(UKNonDurables)
?UKNonDurables
df3 <- UKNonDurables
head(df3)
frequency(df3) # Quarterly 

### 3.1: Plot the series and identify relevant features ###
autoplot(df3, main = "Cunsomption of Non-Durables Time Series") # Trend of growth, some cycles of decay followed by growth. 
ggseasonplot(df3, main = "Non-Durables Season Plot") # There are signs of seasonality .
ggsubseriesplot(df3, main = "Non-Durables Sub-Series Plot") # Continuous growth over the quarters. All quarters follow same trend over time.
gglagplot(df3, main = "Non-Durables Lag Plot", lags = 16) # lag 4, lag 8 and lag 12 show the strongest correlation. 
ggAcf(df3, main = "Non-Durables Autocorrelation Function", lag.max = 24) # There is seasonality suggested by the graph, with every 4th lag. 
ggPacf(df3, main = "Non-Durables Partial Autocorrelation Function", lag.max = 24)
# The slow decrease in the ACF as the lags increase is due to the trend, while the “scalloped” shape is due the seasonality.

# For multivariate regression model, I should find the correlation between each 2 independent variables
# In order to avoid multicolinearity 
# I should check for patterns in the scatterplot of the residuals ! Another principle

# Assumptions:
# 1) Data presents a clear growing trend, especially to the right side of the sample. SES fails to catch the trend. 
# 2) Data presents seasonality. Apply additive or multiplicative methods. 


### 3.2:  Forecast the series using exponential smoothing. Justify the choice of the mothod you use. ###
# The Holt-Winters method captures both seasonality and trend. 
# The Seasonal variations are roughly constant through the series, which leads me to choose the additive method.

train <- window(df3, start(df3), c(1979,4))
#autoplot(train)
test <- window(df3, 1980, c(1988,4))
#autoplot(test)

fit1 <- hw(train, seasonal="additive", h=36)
fit2 <- hw(train, seasonal="multiplicative", h=36) # I will also study the performance of the multiplicative method, for comparrison.
fit3 <- hw(train, damped=TRUE, seasonal="multiplicative", h=36)

RMSE = function(m, o){ # m is method, o is observed values 
  sqrt(mean(m - o)^2)
}
RMSE(fit1, df3)
RMSE(fit2, df3)
RMSE(fit3, df3)

autoplot(train) +
  autolayer(fit1, series="HW additive forecast", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecast", PI=FALSE) +
  autolayer(fit3, series="HW damped multiplicative forecast", PI=FALSE) +
  xlab("Year") + 
  ylab("Units") + 
  ggtitle("Consumption of Non-Durables in the UK") + 
  guides(colour=guide_legend(title="Forecasts"))

autoplot(test) +
  autolayer(fit1, series="HW additive forecast", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecast", PI=FALSE) +
  autolayer(fit3, series="HW damped multiplicative forecast", PI=FALSE) +
  xlab("Year") + 
  ylab("Units") + 
  ggtitle("Consumption of Non-Durables in the UK") + 
  guides(colour=guide_legend(title="Forecasts"))


### 3.3:  Estimate a Innovations State Space Model. Present the model you estimated ###
# Aditive trend, aditive seasonality => Additive Holt-Winter's Method or (A,A)

ets1 <- ets(train)
autoplot(ets1)


summary(ets1)

cbind('Residuals' = residuals(ets1),
      'Forecast errors' = residuals(ets1,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")

### 3.4:  Using the model you estimated in the previous question, produce a forecast for the series ###
ets1 %>% forecast(h=36) %>%
  autoplot() +
  ylab("Consumption of Non-Durables in the UK")
