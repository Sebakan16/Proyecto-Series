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
colnames(datos) <- sub(".*?, ", "", colnames(datos))
datos <- na.omit(datos)

X <- datos$`Banco Santander-Chile`[-c(136:148)]
Xt <- ts(X, frequency = 12)

# Calcular el numero de diferenciaciones

ndiffs(ts(X, frequency = 12)) # Se diferencia con 1
nsdiffs(ts(X, frequency = 12)) # 0

# Se trabaja con la serie diferenciada
X_dif <- diff(log(X), lag = ndiffs(ts(X, frequency = 12)))

acf(X_dif)
pacf(X_dif)


lambda <- forecast::BoxCox.lambda(X_, method = "guerrero")
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

?ARMAacf()
