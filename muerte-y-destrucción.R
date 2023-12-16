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

# Paso de diferenciación

# Y <- diff(Y, lag = ndiffs(Y))

acf(diff(Y), lag.max = 19)
pacf(diff(Y))

acf((diff(log(Y))), lag.max = 150)
pacf((diff(log(Y))), lag.max = 150)


lambda <- forecast::BoxCox.lambda(Y, method = "guerrero")
plot(forecast::BoxCox(Y, lambda = lambda), col = "steelblue")
per <- LSTS::periodogram((diff(log(Y))))

plot(per$periodogram ~ per$lambda, type = "l", lwd = 2)

which(per$periodogram  == max(per$periodogram))

2*pi/per$lambda[4] # Periodo d=169, que es el total de datos
# No necesitamos diferenciar (?)
abline(v = 2*pi/33.75, col = "red")

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
           NA, rep(0, 18), rep(0, 9), NA,  # MA
           0, 0, 0, 0, NA # SMA
)

fit_diff <- forecast::Arima(log(Y), 
                            order = c(2, 1, 29),
                            seasonal = c(0, 1, 5),
                            # fixed = fixed,
                            include.mean = FALSE,
                            include.drift = T)

salida_TS(log(Y), fit_diff, fixed = fixed)

Box.Ljung.Test(fit_diff$residuals, lag = 135)
abline(v = 30)
plot(forecast::forecast(fit_diff, h = 12))

# TS.diag(fit_diff$residuals)