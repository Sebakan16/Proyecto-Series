# SARIMA

# Librerías ----
library(tidyverse)
library(dplyr)
library(corrplot)
library(forecast)
library(lubridate)

# Paso 1: Transformaciones----

lambda <- forecast::BoxCox.lambda(Y, method = "guerrero")

# argumentar q usar el log pq lo dice Boxcox y por la plata

# Paso 2 Diferenciación: ----

ndiffs(Y)

# direnciamos una vez para eliminar tendencia

# Paso 3 Construcción del modelo ------

acf((diff(log(Y))), lag.max = 50) # argumenta pq un MA(29)
pacf((diff(log(Y))), lag.max = 150) # argumenta pq usar un AR(2)

# Probando con auto SARIMA ----

model_diff <- auto.arima(log(Y))

salida_TS(Y, model_diff, fixed = c(NA,NA,NA))
TS.diag(model_diff$residuals)

# se recomienda un modelo ARIMA(2,1,0) pero hay coef no signficativos 
# hay q buscar formas de mejorarlo

# Probando casos a mano -----

# Opción 1: SARIMA()

fixed <- c(NA, NA, # AR
           rep(0, 28), NA,  # MA
           rep(NA, 10) # SMA
)

fit_diff <- forecast::Arima(log(Y),
                            order = c(2, 1, 29),
                            seasonal = c(0, 0, 10),
                            fixed = fixed,
                            include.mean = FALSE,
                            include.drift = F)
beepr::beep()
salida_TS(log(Y), fit_diff, fixed = fixed)
dev.off()
Box.Ljung.Test(fit_diff$residuals, lag = 135)
plot(forecast::forecast(fit_diff, h = 12))

