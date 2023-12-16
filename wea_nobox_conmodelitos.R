library(tidyverse)
library(dplyr)
library(tseries)
library(forecast)
library(fracdiff)

source("TS.diag.R")
source("summary.arima.R")
source("salida_TS.R")


# Base de datos -----------------------------------------------------------


santander <- rio::import("santander.xlsx", skip = 2)
colnames(santander) <- c("fecha", "BS")
santander$fecha <- as.Date(santander$fecha)

santander$tipo <- c(rep("entrenamiento", 148-12),
                    rep("validacion",12))

Y <- santander %>%
  filter(tipo == "entrenamiento") %>% 
  # filter((fecha < as.Date("2020-07-30") |
  #           fecha > as.Date("2022-04-28"))) %>%
  select(BS) 

Y <- ts(Y$BS, frequency = 12)


plot(log(santander$BS), type = "l")

# Transformación ----------------------------------------------------------

lambda <- forecast::BoxCox.lambda(log(Y), method = "guerrero")
lambda2 <- forecast::BoxCox.lambda(log(Y), method = "loglik")
f.Y <- forecast::BoxCox(log(Y), lambda = lambda)

MASS::boxcox(lm((Y) ~ 1))

# Diferenciaciones --------------------------------------------------------

forecast::ndiffs((Y), test = "adf")
forecast::ndiffs((Y), test = "pp")
forecast::ndiffs((Y), test = "kpss")

d <- forecast::ndiffs(log(Y))

plot(diff((Y), differences = d), main = expression((1-B)*f(Y[t])), ylab = "")
acf(diff((Y), differences = d), lag.max = 136)
pacf(diff((Y), differences = d), lag.max = 136)

## Diferenciamos Estacionalmente?
s <- frequency(log(Y))

forecast::nsdiffs(diff((Y), differences = d), test = "seas")
forecast::nsdiffs(diff((Y), differences = d), test = "ocsb")
forecast::nsdiffs(diff((Y), differences = d), test = "hegy")
forecast::nsdiffs(diff((Y), differences = d), test = "ch")

D <- forecast::nsdiffs(diff(log(Y), differences = d))
# Este es cero, no necesitamos añadirlo al modelo

## Z[t] = (1-B)(1-B^s) f(Y[t]), s = 12.
Z <- diff((Y), differences = d)
plot(Z, main = expression((1-B)*(1-B^s)*f(Y[t])), ylab = "")

## ACF parte regular
par(mfrow = c(1,2))
acf(c(Z), lag.max = 11, ylim = c(-0.5,+1), main = "")
pacf(c(Z), lag.max = 11, ylim = c(-0.5,+1), xlim = c(0,11), main = "")
## p = 2, q = 5

## ACF parte estacional
par(mfrow = c(1,2))
acf(c(Z), lag.max = 136, ylim = c(-1,+1), main = "")
abline(v=seq(12, 136, by = 12), lty = 2, col = "gray")
pacf(c(Z), lag.max = 136, ylim = c(-1,+1), xlim = c(0,136), main = "")
abline(v=seq(12, 136, by = 12), lty = 2, col = "gray")
# P = 1, Q = 1


# Sarima ------------------------------------------------------------------


fixed <- c(NA, NA, # AR
           0, 0, 0, 0, NA,  # MA
           NA#,
           #0#,  # SMA
)

fit_diff <- forecast::Arima(log(Y), 
                            order = c(2, d, 5),
                            seasonal = c(1, 0, 0),
                            #lambda = lambda,
                            fixed = fixed,
                            include.mean = FALSE,
                            include.drift = FALSE
)



salida_TS(log(Y), fit_diff, fixed = fixed)

Box.Ljung.Test(fit_diff$residuals, lag = 135)
plot(forecast::forecast(fit_diff, h = 12))
TS.diag(fit_diff$residuals)





fixed2 <- c(NA, NA, # AR
            0, 0, 0, 0, NA,  # MA
            #0, # SAR
            NA,  # SMA
            NA)

fit_diff2 <- forecast::Arima(log(Y), 
                             order = c(2, d, 5),
                             seasonal = c(0, 0, 1),
                             #lambda = lambda,
                             fixed = fixed2,
                             include.mean = FALSE,
                             include.drift = TRUE
)

salida_TS(log(Y), fit_diff2, fixed = fixed2)

Box.Ljung.Test(fit_diff2$residuals, lag = 135)
plot(forecast::forecast(fit_diff2, h = 12))
TS.diag(fit_diff2$residuals)

# PROBAR A LOS DOS CTM CON VARIABLES EXÓGENAS, en volá así los weones mejoran


# Sarimax -----------------------------------------------------------------

desempleo <- rio::import("Indicador.xls", skip = 2)

desempleo$Mes <- as.Date(desempleo$Mes)

tasa <- desempleo %>% 
  filter(Mes >= as.Date("2011-01-01") & Mes <= as.Date("2023-08-01")) %>% 
  filter(!Mes %in% c(as.Date("2013-07-01"), as.Date("2015-11-01"),
                     as.Date("2015-12-01"), as.Date("2016-10-01")))

santander %>% 
  filter(fecha >= as.Date("2015-01-01") & fecha <= as.Date("2016-12-01")) 
# Falta noviembre y diciembre de 2015
# Falta julio del 2013
# Falta octubre del 2016

santander$desempleo <- tasa$Valor

fixedx <- c(NA, NA, # AR
           0, 0, 0, 0, NA,  # MA
           NA, # SAR
           #0#,  # SMA
           NA
)

fit_diffx <- forecast::Arima((Y), 
                            order = c(2, 1, 5),
                            seasonal = c(1, 0, 0),
                            fixed = fixedx,
                            xreg = santander$desempleo[1:136],
                            include.mean = FALSE,
                            include.drift = FALSE
)

salida_TS((Y), fit_diffx, fixed = fixedx)

Box.Ljung.Test(fit_diffx$residuals, lag = 135)
plot(forecast::forecast(fit_diffx, h = 12, xreg = santander$desempleo[137:148]))
TS.diag(fit_diffx$residuals)

fixedx2 <- c(NA, NA, # AR
            0, 0, 0, 0, NA,  # MA
            #0, # SAR
            NA,  # SMA
            NA)

fit_diffx2 <- forecast::Arima((Y), 
                             order = c(2, d, 5),
                             seasonal = c(0, 0, 1),
                             #lambda = lambda,
                             fixed = fixedx2,
                             include.mean = FALSE,
                             include.drift = TRUE
)

salida_TS((Y), fit_diffx2, fixed = fixedx2)

Box.Ljung.Test(fit_diff2$residuals, lag = 135)
plot(forecast::forecast(fit_diff2, h = 12))
TS.diag(fit_diff2$residuals)
