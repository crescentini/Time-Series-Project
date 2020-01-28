rm(list=ls())

library(tseries)
library(forecast)
library(lmtest)
library(tidyr)

#### Decomposition Method ####

# read in data
sales_full <- read.csv(file = "C:/Users/Chandler/Downloads/sales.csv")

# data cleaning
colnames(sales_full) <- c("Time", "Value")
sales_full$Time <- 1:324
sales_full$Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# plot data
plot(sales_full$Time, sales_full$Value, type = "l", xlab = "Time",
     ylab = "Sales (Millions of Dollars)", main = "Retail Sales: Full Service Restaurants")

# leave off last year for prediction
sales <- sales_full[1:312,]

plot(sales$Time, sales$Value, type = "l", xlab = "Time",
     ylab = "Sales (Millions of Dollars)", main = "Retail Sales: Full Service Restaurants")

# take log transform
logsales <- log(sales$Value)

sales$log <- logsales

# plot log transform
plot(sales$Time, sales$log, type = "l", xlab = "Time",
     ylab = "Log Sales", main = "Log Sales with Cubic Trend")

# create linear, quadratic, and cubic models for trend
mod1 <- lm(log ~ Time, data = sales)
mod2 <- lm(log ~ Time + I(Time^2), data = sales)
mod3 <- lm(log ~ Time + I(Time^2) + I(Time^3), data = sales)

summary(mod1)
summary(mod2)
summary(mod3)

# plot models
lines(sales$Time, mod1$fitted.values, col = "blue")
lines(sales$Time, mod2$fitted.values, col = "green")
lines(sales$Time, mod3$fitted.values, col = "red")

# detrend the data
detrend <- sales$log - mod3$fitted.values

# plot detrended series
plot(sales$Time, detrend, type = "l", xlab = "Time",
     ylab = "Log Sales", main = "Detrended Series with 2x12 Moving Average")

# 2x12 moving averages
MA=ma(detrend, order=12)
lines(sales$Time,MA,col="blue")

MA=ma(MA, order=2)
lines(sales$Time,MA,col="red")

# decycle the series
decycle <- detrend - MA

# plot detrended, decycled series
plot(sales$Time, decycle, type = "l", xlab = "Time",
     ylab = "Log Sales", main = "Detrended, Decycled Series")

decycle <- as.data.frame(decycle)
decycle$Time <- 1:312
decycle$Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

L = 12
#try 2 and 4 harmonic
test=lm(decycle$x~0+I(cos(2*pi*Time/L)), data = decycle)
summary(test)
test2=lm(decycle$x~0+I(cos(2*pi*Time/L))+I(cos(4*pi*Time/L)), data = decycle)
summary(test2)

#best harmonic
test3 <- lm(decycle$x~0+I(cos(2*pi*Time/L))+I(cos(4*pi*Time/L)),data = decycle)
summary(test3)
lines(decycle$Time[8:305],test3$fitted.values,col="red")

#Dummy regression is better
season=lm(decycle$x~0+Month,data = decycle)
summary(season)
lines(decycle$Time[8:305],season$fitted.values,col="red")

deseason <- decycle$x[8:305] - season$fitted.values

# plot the error component
plot(sales$Time[8:305], deseason, xlab = "Time", ylab = "Residuals", main = "Error Component")

# final fit
fit_final <- lm(log ~ Time + I(Time^2) + I(Time^3) + Month, data = sales)
summary(fit_final)

# calculate AIC and BIC
AIC(fit_final)
BIC(fit_final)

# plot fitted values
plot(sales_full$Time, sales_full$Value, type = "l", xlab = "Time",
     ylab = "Sales (Millions of Dollars)", main = "Retail Sales with Fitted and Predicted Values")
lines(exp(fit_final$fitted.values), col = "red")

# predict
pred_proj <- data.frame(Time = c(313:324), Month = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
pred <- predict(fit_final, newdata = pred_proj, interval = "prediction")
pred_proj$pred <- pred
pred_proj <- as.data.frame(pred_proj)

mean((sales_full$Value[313:324] - exp(pred_proj$pred[,1]))^2)
mean((sales_full$Value[313:324] - exp(pred_proj$pred[,1])) / sales_full$Value[313:324])

# plot predicted values
points(313:324, exp(pred_proj$pred[,1]), pch = 20, col = "green")
lines(313:324, exp(pred_proj$pred[,1]), pch = 20, col = "green")
lines(313:324, exp(pred_proj$pred[,2]), col = "blue")
lines(313:324, exp(pred_proj$pred[,3]), col = "blue")


#### Box-Jenkins Analysis ####

rm(list=ls())

# read in data
sales_full <- read.csv(file = "C:/Users/Chandler/Downloads/sales.csv")

# clean data
colnames(sales_full) <- c("Time", "Value")
sales_full$Time <- 1:324
sales_full$Month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

plot(sales_full$Time, sales_full$Value, type = "l", xlab = "Time",
     ylab = "Sales (Millions of Dollars)", main = "Retail Sales: Full Service Restaurants")

# leave off last year for prediction
sales <- sales_full[1:312,]

plot(sales$Time, sales$Value, type = "l")

# run adf test
adf.test(sales$Value)

# take log transform
logsales <- log(sales$Value)

sales$log <- logsales

# plot log transformed data
plot(sales$Time, sales$log, type = "l")

# take seasonal difference
sdiff <- diff(sales$log, 12)

ts.plot(sdiff, main = "One Seasonal Difference")

# almost stationary
adf.test(sdiff)

# one more difference
diff <- diff(sdiff)

adf.test(diff)

ts.plot(diff)

# acf and pacf plots
acf(sdiff,lag.max = 60,ylim=c(-1,1), main = "ACF After Seasonal Differencing")
pacf(sdiff,lag.max = 60,ylim=c(-1,1), main = "PACF After Seasonal Differencing")

# create different arima models
model1 <- arima(sdiff, order = c(0,0,1))
BIC(model1)
model2 <- arima(sdiff, order = c(1,0,0))
BIC(model2)
model3 <- arima(sdiff, order = c(1,0,1))
BIC(model3)
model4 <- arima(sdiff, order = c(1,0,2))
BIC(model4)
model5 <- arima(sdiff, order = c(2,0,1))
BIC(model5)
model6 <- arima(sdiff, order = c(2,0,2))
BIC(model6)

# test coefficients
coeftest(model1)
coeftest(model2)
coeftest(model3)

# acf and pacf plots
acf(residuals(model3),lag.max = 60,ylim=c(-1,1), main = "ACF Plot")
pacf(residuals(model3),lag.max = 60,ylim=c(-1,1), main = "PACF Plot")

# create difference sarima models
model7 <- arima(logsales,order=c(1,0,1),seasonal = list(order = c(0,1,1), period = 12))
model8 <- arima(logsales,order=c(1,0,1),seasonal = list(order = c(1,1,0), period = 12))
model9 <- arima(logsales,order=c(1,0,1),seasonal = list(order = c(1,1,1), period = 12))
model10<- arima(logsales,order=c(1,0,1),seasonal = list(order = c(1,1,2), period = 12))
model11<- arima(logsales,order=c(1,0,1),seasonal = list(order = c(2,1,1), period = 12))
model12<- arima(logsales,order=c(1,0,1),seasonal = list(order = c(2,1,2), period = 12))

# calculate BIC values
BIC(model7)
BIC(model8)
BIC(model9)
BIC(model10)
BIC(model11)
BIC(model12)

coeftest(model12)
coeftest(model8)
print(model12) 
summary(model12)

# acf and pacf plots
acf(residuals(model12),lag.max = 60,ylim=c(-1,1), main = "ACF Plot")
pacf(residuals(model12),lag.max = 60,ylim=c(-1,1), main = "PACF Plot")

# calculate Ljung-Box test
residuals=resid(model12) 
Box.test(residuals,type="Ljung",lag=6,fitdf=0) 

# plot residuals
plot(residuals, type = "p", ylab = "Residuals", main = "Residual Plot")

# prediction
pred <- predict(model12, 12)

# plot fitted and predicted values
plot(sales_full$Time, sales_full$Value, type = "l",
     xlab = "Time", ylab = "Sales (Millions of Dollars)",
     main = "Retail Sales with Fitted and Predicted Values")
lines(sales$Time, exp(fitted(model12)), col = "red")
points(313:324, exp(pred$pred), pch = 20, col = "green")
lines(313:324, exp(pred$pred), pch = 20, col = "green")

mean((sales_full$Value[313:324] - exp(pred$pred))^2)
mean((sales_full$Value[313:324] - exp(pred$pred)) / sales_full$Value[313:324])

