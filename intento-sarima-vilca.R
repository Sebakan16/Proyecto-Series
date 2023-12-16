# SARIMA

# Librerías ----
library(tidyverse)
library(dplyr)
library(corrplot)
library(forecast)
library(lubridate)

# Paso 1: Transformaciones----

lambda <- forecast::BoxCox.lambda(log(Y), method = "guerrero")

# argumentar q usar el log pq lo dice Boxcox y por la plata

# Paso 2 Diferenciación: ----

ndiffs(Y)

# direnciamos una vez para eliminar tendencia

# Paso 3 Construcción del modelo ------

acf((diff(log(Y))), lag.max = 50) # argumenta pq un MA(29)
pacf((diff(log(Y))), lag.max = 50) # argumenta pq usar un AR(2)

acf <- acf((diff(log(Y))), lag.max = 50, plot = F)
acf_data <- data.frame(Lag = acf$lag,
                       ACF = acf$acf)

# PACF base
pacf <- pacf((diff(log(Y))), lag.max = 50, plot = F)
pacf_data <- data.frame(Lag = pacf$lag,
                        PACF = pacf$acf)

n = length(Y)

ggplot(acf_data, aes(x = Lag, y = ACF)) +
  ylim(c(-0.5,1))+
  geom_hline(yintercept = 0,
             linetype = "dashed") + 
  geom_hline(yintercept = 1.96/sqrt(n),
             linetype = "dashed",
             col = "black") + 
  geom_hline(yintercept = -1.96/sqrt(n),
             linetype = "dashed",
             col = "black") + 
  geom_segment(aes(xend = Lag,
                   yend = 0),
               color = "#FF1A15",
               size = 1) + # Líneas verticales
  geom_point(size = 2,
             col = "#FF1A15") + # Puntos de color en el extremo de las líneas
  labs(x = "Lag",
       y = "ACF") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15),
        # axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 10,
                                  #face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 15))

pacf_data %>%
  ggplot(aes(x = Lag, y = PACF)) +
  ylim(c(-0.3,0.3))+
  geom_hline(yintercept = 0,
             linetype = "dashed") + 
  geom_hline(yintercept = 1.96/sqrt(n),
             linetype = "dashed",
             col = "black") + 
  geom_hline(yintercept = -1.96/sqrt(n),
             linetype = "dashed",
             col = "black") + 
  geom_segment(aes(xend = Lag,
                   yend = 0),
               color = "#FF1A15",
               size = 1) + # Líneas verticales
  geom_point(size = 2,
             col = "#FF1A15") + # Puntos de color en el extremo de las líneas
  labs(x = "Lag",
       y = "PACF") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 15),
        # axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 10,
                                  #face = "bold",
                                  color = "black",
                                  hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 15))

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
           NA,rep(0, 4),NA # SMA
)

fit_diff <- forecast::Arima(log(Y),
                            order = c(2, 1, 29),
                            seasonal = c(0, 0, 5),
                            fixed = fixed,
                            include.mean = FALSE,
                            include.drift = F)
beepr::beep()
salida_TS(log(Y), fit_diff, fixed = fixed)
dev.off()
Box.Ljung.Test(fit_diff$residuals, lag = 135)
plot(forecast::forecast(fit_diff, h = 12))

