# chap9.R

library(TSA)
library(forecast)

# Exhibit 9.1
data(color)
m1.color <- Arima(color,order=c(1,0,0))
m1.color 

# Exhibit 9.2
# append 2 years of missing values to the tempdub data as we want to forecast
# the temperature for two years.
data(tempdub)

tempdub1 <- ts(c(tempdub,rep(NA,24)),
               start = start(tempdub),
               freq = frequency(tempdub)) 

# creates the first pair of harmonic functions and then fit the model
har. <- harmonic(tempdub,1)
m5.tempdub <- Arima(tempdub, order = c(0,0,0), xreg = har.)
m5.tempdub
# The result is same as that from the fit using lm function.
har. <- harmonic(tempdub, 1)
model4 <- lm(tempdub ~ har.)
summary(model4)

# create the harmonic functions over the period of forecast.
newhar. <- harmonic(ts(rep(1,24), start = c(1976,1), freq = 12), 1)
# Compute and plot the forecasts.
plot(m5.tempdub, n.ahead=24, n1 = c(1972,1), 
     newxreg = newhar., type = "b",
     ylab = "Temperature", xlab = "Year")

# forecast package
fc.tempdub <- forecast(m5.tempdub, xreg = newhar.)
plot(fc.tempdub)


# Exhibit 9.3 
data(color)
m1.color <- Arima(color, order = c(1,0,0))
plot(m1.color, n.ahead = 12, type = "b", 
     xlab = "Time", ylab = "Color Property")
# add the horizontal line at the estimated mean ("intercept") 
abline(h = coef(m1.color)[names(coef(m1.color)) == "intercept"])

# forecast package
fc.color <- forecast(m1.color, h = 12)
plot(fc.color)


# Exhibit 9.4
data(hare)
m1.hare <- Arima(sqrt(hare), order = c(3,0,0))
plot(m1.hare, n.ahead = 25, type = "b", xlab = "Year",
     ylab = "Sqrt(hare)")
abline(h = coef(m1.hare)[names(coef(m1.hare))=="intercept"])

# forecast package
fc.hare <- forecast(m1.hare, h = 25)
plot(fc.hare)

# transformed data
m2.hare <- Arima(hare, order = c(3,0,0), lambda = 0.5)
fc2.hare <- forecast(m2.hare, h = 25)
plot(fc2.hare)


