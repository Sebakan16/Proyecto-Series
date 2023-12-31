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
santander$ano <- year(santander$fecha)
santander$mes <- year(santander$fecha)

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

MASS::boxcox(lm(log(Y) ~ 1))

boxcox(lm(x ~ 1))

# Diferenciaciones --------------------------------------------------------

forecast::ndiffs((f.Y), test = "adf")
forecast::ndiffs((f.Y), test = "pp")
forecast::ndiffs((f.Y), test = "kpss")

d <- forecast::ndiffs((f.Y))

plot(diff((f.Y), differences = d), main = expression((1-B)*f(Y[t])), ylab = "")
acf(diff(f.Y, differences = d), lag.max = 136)
pacf(diff(f.Y, differences = d), lag.max = 136)

## Diferenciamos Estacionalmente?
s <- frequency((f.Y))

forecast::nsdiffs(diff((f.Y), differences = d), test = "seas")
forecast::nsdiffs(diff((f.Y), differences = d), test = "ocsb")
forecast::nsdiffs(diff((f.Y), differences = d), test = "hegy")
forecast::nsdiffs(diff((f.Y), differences = d), test = "ch")

D <- forecast::nsdiffs(diff((f.Y), differences = d), test = "hegy")
# Este es cero, no necesitamos añadirlo al modelo

## Z[t] = (1-B)(1-B^s) f(Y[t]), s = 12.
Z <- diff(diff((f.Y), differences = d, lag = 1), lag = s, differences = D)
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

# Modelo auto SARIMA ----

model_diff <- auto.arima(Y)

salida_TS(Y, model_diff, fixed = c(NA, NA))
TS.diag(model_diff$residuals)

# Jugando con el SARIMA ----

# Wea que salió bonita
# fit_diff <- forecast::Arima(Y, 
#                             order = c(2, 1, 29),
#                             seasonal = c(0, 0, 5),
#                             fixed = fixed,
#                             include.mean = FALSE,
#                             include.drift = T)



fixed <- c(0, NA, # AR
           0, NA, 0, NA, NA,  # MA
           0,
           NA#,  # SMA
           )

fit_diff <- forecast::Arima(log(Y), 
                            order = c(2, d, 5),
                            seasonal = c(1, 0, 1),
                            #lambda = lambda,
                            fixed = fixed,
                            include.mean = FALSE,
                            include.drift = FALSE
                            )

fixed2 <- c(NA, NA, # AR
            NA, 0, 0, NA, NA,  # MA
           0,
           NA#,  # SMA
)

fit_diff2 <- forecast::Arima(log(Y), 
                            order = c(2, d, 5),
                            seasonal = c(1, D, 1),
                            #lambda = lambda,
                            fixed = fixed,
                            include.mean = FALSE,
                            include.drift = FALSE
)

salida_TS(log(Y), fit_diff, fixed = fixed)
salida_TS(log(Y), fit_diff2, fixed = fixed)

Box.Ljung.Test(fit_diff$residuals, lag = 135)
plot(forecast::forecast(fit_diff, h = 12))
plot(forecast::forecast(fit_diff2, h = 12))

TS.diag(fit_diff$residuals)
