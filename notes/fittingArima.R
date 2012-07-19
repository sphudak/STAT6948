
# This is fittingArima.R
set.seed(42)
library(forecast)

y <- arima.sim(model = list(ma = c(0.3, 0.2, 0.1)), n = 100)

fit <- Arima(y, order = c(0,0,3), include.mean = FALSE)
tmp <- round(coef(fit), 4)

png(filename="img/ma3zm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(0,0,3), include.mean = FALSE)

fit

y <- arima.sim(model = list(ar = c(0.3, 0.2)), n = 100)

fit <- Arima(y, order = c(2,0,0), include.mean = FALSE)
tmp <- round(coef(fit), 4)

png(filename="img/ar2zm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(2,0,0), include.mean = FALSE)

fit

y <- 5 + arima.sim(model = list(ma = c(0.3, 0.2, 0.1)), n = 100)

fit <- Arima(y, order = c(0,0,3))
tmp <- round(coef(fit), 4)

png(filename="img/ma3nzm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(0,0,3))

fit

y <- 21 +  arima.sim(model = list(ar = c(0.3, 0.2)), n = 100)

fit <- Arima(y, order = c(2,0,0))
tmp <- round(coef(fit), 4)

png(filename="img/ar2nzm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(2,0,0))

fit

y <- arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 189)

fit <- Arima(y, order = c(1,0,2), include.mean = FALSE)
tmp <- round(coef(fit), 4)

png(filename="img/arma12zm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(1,0,2), include.mean = FALSE)

fit

y <- 11 + arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 189)

fit <- Arima(y, order = c(1,0,2))
tmp <- round(coef(fit), 4)

png(filename="img/arma12nzm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(1,0,2))

fit

ydiff <- arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 196)
y <- ts(cumsum(ydiff))

fit <- Arima(y, order = c(1,1,2))
tmp <- round(coef(fit), 4)

png(filename="img/arima112zm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(1,1,2))

fit

ydiff <- 1.5 + arima.sim(model = list(ar = 0.8, ma = c(0.7, 0.6)), n = 196)
y <- ts(cumsum(ydiff))

fit <- Arima(y, order = c(1,1,2), include.drift = TRUE)
tmp <- round(coef(fit), 4)

png(filename="img/arima112nzm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(1,1,2), include.drift = TRUE)

fit

ydiffdiff <- arima.sim(model = list(ar = 0.9, ma = 0.5), n = 175)
y <- ts(cumsum(cumsum(ydiffdiff)))

fit <- Arima(y, order = c(1,2,1))
tmp <- round(coef(fit), 4)

png(filename="img/arima121zm.png")
plot(y, ylab = expression(Y[t]))
dev.off()

Arima(y, order = c(1,2,1))

fit
