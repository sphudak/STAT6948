
# This is fittingArima.R
set.seed(42)
library(forecast)

y <- arima.sim(model = list(ma = c(0.3, 0.2, 0.1)), n = 100)
Arima(y, order = c(0,0,3), include.mean = FALSE)

y <- arima.sim(model = list(ar = c(0.3, 0.2)), n = 100)
Arima(y, order = c(2,0,0), include.mean = FALSE)

y <- 5 + arima.sim(model = list(ma = c(0.3, 0.2, 0.1)), n = 100)
Arima(y, order = c(0,0,3))

y <- 21 +  arima.sim(model = list(ar = c(0.3, 0.2)), n = 100)
Arima(y, order = c(2,0,0))

y <- arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 189)
Arima(y, order = c(1,0,2), include.mean = FALSE)

y <- 11 + arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 189)
Arima(y, order = c(1,0,2))

ydiff <- arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 196)
y <- ts(cumsum(ydiff))
Arima(y, order = c(1,1,2))

ydiff <- 1.5 + arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 196)
y <- ts(cumsum(ydiff))
Arima(y, order = c(1,1,2), include.drift = TRUE)

ydiffdiff <- arima.sim(model = list(ar = 0.9, ma = 0.5), n = 175)
y <- ts(cumsum(cumsum(ydiffdiff)))
Arima(y, order = c(1,2,1))
