# Librerías ----
library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(fracdiff)

source("TS.diag.R")
source("summary.arima.R")
source("salida_TS.R")


# Carga de datos ----
### Stock total de cartera consumo.
datos <- rio::import("CMF_CONT_BANC_STO_CCS_INCUMP_AGIFI_MM$_MONT_V_old.xlsx",
                     skip = 3)
df <- rio::import("rial_imacec.xlsx", skip = 2)
colnames(datos) <- sub(".*?, ", "", colnames(datos))
datos <- na.omit(datos)

X <- datos$`Banco Santander-Chile`[-c(136:148)]
Xt <- ts(X, frequency = 12,start = c(2011,1))

plot(Xt)
datos1 <- cbind(X, "imacec" = df[-c(136:154),2])

# Calcular el numero de diferenciaciones

ndiffs(ts(X, frequency = 12)) # Se diferencia con 1
nsdiffs(ts(X, frequency = 12)) # 0

# Se trabaja con la serie diferenciada
X_dif <- diff(log(X), lag = ndiffs(ts(X, frequency = 12)))

acf(X_dif)
pacf(X_dif)

ndiffs(ts(X_dif, frequency = 12))
nsdiffs(ts(X_dif, frequency = 12))

lambda <- forecast::BoxCox.lambda(X_dif, method = "guerrero")
plot(forecast::BoxCox(Xt, lambda = lambda), col = "steelblue")

LSTS::periodogram(X_dif)



fit1 <- forecast::auto.arima(X_dif, d=0) # Modelo AR (2)
salida_TS(X_dif, fit1, fixed = c(NA, NA, NA))
TS.diag(fit1$res)

#         AR       1   2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18
fixed <- c(NA, NA)

fit2 <- forecast::Arima(X_dif, 
                        order = c(1, 0, 1),
                        fixed = fixed, include.mean = FALSE)
salida_TS(X_dif, fit2, fixed = fixed)
TS.diag(fit2$res)


# No pescar ---------------------------------------------------------------

#No sé si ta weno

par(mfrow = c(2,3))
plot(X)
plot(diff(X))
plot(X_dif)
acf(X, ylim = c(-1,+1), lag.max = 36)
acf(diff(X), ylim = c(-1,+1), lag.max = 36)
acf(X_dif, ylim = c(-1,+1), lag.max = 36)

par(mfrow = c(2,2), bty = "n", las = 1)
acf(c(X_dif), ylim = c(-1,+1), lag.max = 11)
pacf(c(X_dif), ylim = c(-1,+1), lag.max = 11, xlim = c(0,11))
acf(c(X_dif), ylim = c(-1,+1), lag.max = 60)
pacf(c(X_dif), ylim = c(-1,+1), lag.max = 60, xlim = c(0,60))

fit <- forecast::auto.arima(X_dif, d = 1, D = 1)
source("summary.arima.R")
summary(fit)
summary_arima(fit, fixed = c(NA,NA,NA,NA,NA))

fit01 <- forecast::Arima(X_dif, order = c(0,1,1), seasonal = c(1,1,2))
fit02 <- forecast::Arima(X_dif, order = c(0,1,1), seasonal = c(1,1,1))
fit03 <- forecast::Arima(X_dif, order = c(0,1,1), seasonal = c(1,1,0))
fit04 <- forecast::Arima(X_dif, order = c(0,1,1), seasonal = c(0,1,2))
fit05 <- forecast::Arima(X_dif, order = c(0,1,1), seasonal = c(0,1,1))
fit06 <- forecast::Arima(X_dif, order = c(0,1,1), seasonal = c(0,1,0))

summary(fit01)
summary(fit02)
summary(fit03)

AIC(
  fit01,
  fit02,
  fit03,
  fit04,
  fit05,
  fit06
)


fit05

LSTS::ts.diag(fit05$res, 24)

fit05.1 <- forecast::Arima(co2, order = c(1,1,2), seasonal = c(0,1,1), fixed = c(NA,0,NA,NA))
summary_arima(fit05.1, fixed = c(NA,0,NA,NA))

tsdiag(fit05.1, 24)

fit05.2 <- forecast::Arima(co2, order = c(1,1,3), seasonal = c(0,1,1), fixed = c(NA,0,NA,NA,NA))
summary_arima(fit05.2, fixed = c(NA,0,NA,NA,NA))

tsdiag(fit05.2, 24)

fit05.3 <- forecast::Arima(co2, order = c(1,1,9), seasonal = c(0,1,1), fixed = c(NA,0,NA,NA,0,0,0,0,0,NA,NA))
summary_arima(fit05.3, fixed = c(NA,0,NA,NA,0,0,0,0,0,NA,NA))

tsdiag(fit05.3, 24)
