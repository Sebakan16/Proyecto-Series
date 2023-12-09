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

X <- datos$`Banco Santander-Chile`

# Calcular el numero de diferenciaciones

ndiffs(ts(X, frequency = 12)) # Se diferencia con 1
nsdiffs(ts(X, frequency = 12)) # 0

# Se trabaja con la serie diferenciada
X <- diff(X, lag = ndiffs(ts(X, frequency = 12)))

acf(X)
pacf(X)

fit1 <- forecast::auto.arima(X) # Modelo AR (2)
salida_TS(X, fit1, fixed = c(NA, NA))
TS.diag(fit1$res)

#         AR       1   2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18
fixed <- c(NA, NA, 0, 0, 0, 0, NA, NA)

fit2 <- forecast::Arima(X, 
                        order = c(2, 1, 5),
                        fixed = fixed)
salida_TS(X, fit2, fixed = fixed)
TS.diag(fit2$res)
