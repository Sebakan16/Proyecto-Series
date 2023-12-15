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

##es un excel solo con los datos que nos importan del santander para hacer la leida de datos mas sencilla
data = rio::import("santander.xlsx", skip = 2)

#se crea la serie dejando fuera lo que hay que predecir
X1 <- data$`Stock de cartera consumo en incumplimiento por institución, Banco Santander-Chile`[-c(136:148)]
Xt <- ts(X1, frequency = 12,start = c(2011,1))




### Stock total de cartera consumo.
datos <- rio::import("CMF_CONT_BANC_STO_CCS_INCUMP_AGIFI_MM$_MONT_V_old.xlsx", skip = 2)
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
X_dif <- diff((X), lag = ndiffs(ts(X, frequency = 12)))

plot(X_dif, type = "l")

acf(X)
pacf(X)

ndiffs(ts(X_dif, frequency = 12))
nsdiffs(ts(X_dif, frequency = 12))

lambda <- forecast::BoxCox.lambda(X_dif, method = "guerrero")
plot(forecast::BoxCox(Xt, lambda = lambda), col = "steelblue")

LSTS::periodogram(X_dif)



fit1 <- forecast::auto.arima(X_dif, d=0) # Modelo AR (2)
salida_TS(X_dif, fit1, fixed = c(NA, NA, NA))
TS.diag(fit1$res)

#         AR       1   2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18
fixed <- c(0, 
           NA, NA, 0, 0, rep(0, 6), NA, rep(0, 7), NA, 
           NA, NA, NA)

fit2 <- forecast::Arima(X, 
                        order = c(1, 1, 19), seasonal = list(order = c(3, 1, 0), period = 12),
                        fixed = fixed)
salida_TS(X, fit2, fixed = fixed)
 TS.diag(fit2$res)

LSTS::Box.Ljung.Test(fit2$res, lag = 30)


# Prediccion --------------------------------------------------------------
#se leen los datos a predecir
datos_pred = datos$`Banco Santander-Chile`[-c(1:136)]

val = ts(datos_pred, start = c(2022, 9), end = c(2023, 8),frequency = 12)

# aplicar las mismas transformaciones y diferenciacion que a los datos de entrenamiento
val = diff(val,differences = 1)

#se crea la prediccion, donde fit1 corresponde al modelo arma, sarima, sarimax, cambiar segun corresponda
algo = forecast(fit1, h = 12) 
# ,xreg = blabla, si se agregan variables exogenas al modelo, etc


par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
# grafica las predicciones
plot(algo) 

#grafica el valor real de los datos
lines(val, col = "orange", lwd = 2)  

# grafica los datos estimados por el modelo
lines(fit1$fitted,col ="red") 



### como ejemplo con un sarima copiando metodo de la clase 26

y2 =  diff(Xt, differences = 1)
s <- frequency(y2)
s # implica que tenemos un sarima (q,0,p)(Q,0,P)[12]
Z <- diff(y2, lag = s)
 
Z1 = auto.arima(Z)

algo = forecast(Z1, h = 12)

par(mfrow = c(1,1), bty = "n", las = 1, font.main = 1)
plot(algo) #predic
lines(val, col = "orange", lwd = 2) #real 
lines(Z1$fitted,col ="red") #estimacion modelo 

#predice mal debido a que no podemos diferenciar la series con "s"
# pork nos quedamos sin datos al ser s=12 :c


# No pescar ---------------------------------------------------------------

#No sé si ta weno

par(mfrow = c(2,3))
plot(X)
plot(diff(X))
plot(X_dif, type = "l")
acf(X, ylim = c(-1,+1), lag.max = 36)
acf(diff(X), ylim = c(-1,+1), lag.max = 36)
acf(X_dif, ylim = c(-1,+1), lag.max = 36)

par(mfrow = c(2,2), bty = "n", las = 1)
acf(c(X_dif), ylim = c(-1,+1), lag.max = 11)
pacf(c(X_dif), ylim = c(-1,+1), lag.max = 11, xlim = c(0,11))
acf(c(X_dif), ylim = c(-1,+1), lag.max = 60)
pacf(c(X_dif), ylim = c(-1,+1), lag.max = 60, xlim = c(0,60))

fit <- forecast::auto.arima(X_dif_n, d = 1, D = 12)
source("summary.arima.R")
summary(fit)
summary_arima(fit, fixed = c(NA,NA,NA,NA,NA))

fit01 <- forecast::Arima(X_dif_n, order = c(0,1,1), seasonal = c(1,1,2))
fit02 <- forecast::Arima(X_dif_n, order = c(0,1,1), seasonal = c(1,1,1))
fit03 <- forecast::Arima(X_dif_n, order = c(0,1,1), seasonal = c(1,1,0))
fit04 <- forecast::Arima(X_dif_n, order = c(0,1,1), seasonal = c(0,1,2))
fit05 <- forecast::Arima(X_dif_n, order = c(0,1,1), seasonal = c(0,1,1))
fit06 <- forecast::Arima(X_dif_n, order = c(0,1,1), seasonal = c(0,1,0))

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

q1 <- quantile(X_dif, 0.25)
q3 <- quantile(X_dif, 0.75)

iqr <- q3 - q1

lower_bound <- q1 - 1.5 * iqr
upper_bound <- q3 + 1.5 * iqr

X_dif_n <- X_dif[X_dif >= lower_bound & X_dif <= upper_bound]
plot(X_dif_n, type = "l")

boxplot(X_dif_n)

acf(X_dif_n, lag = 60)
pacf(X_dif_n, lag = 60)
