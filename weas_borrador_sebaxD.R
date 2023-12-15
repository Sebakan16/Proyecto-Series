# Librerías ----
library(tidyverse)
library(dplyr)
library(corrplot)
library(forecast)

# Datos TS ----
santander <- rio::import("santander.xlsx", skip = 2)
colnames(santander) <- c("fecha", "BS")
santander$fecha <- as.Date(santander$fecha)
santander$ano <- year(santander$fecha)
santander$mes <- year(santander$fecha)

santander$tipo <- c(rep("entrenamiento", 148-12),
                    rep("validacion", 12))

# Variables exógenas ----

# UF - UTM - IVP
aux <- rio::import("UF-UTM-IVP.xlsx", skip = 2)
colnames(aux) <- c("fecha", "UF", "IVP", "UTM")
aux$fecha <- as.Date(aux$fecha)

santander <- santander %>% # Unir datos
  left_join(aux,
            by = "fecha")

# tasas
aux <- rio::import("Tasas.xls", sheet = 2, skip = 8)
aux <- aux %>%
  select(Año, Mes, Consumo, Comercial, Comex)
aux <- aux %>%
  janitor::clean_names()
aux <- aux[1:284, ]
aux <- aux[!is.na(aux$comex), ]
aux$ano <- rep(c(rep(2002:2022, each = 12),
                 rep(2023, times = 11)), each = 1)
aux$fecha <- seq(as.Date("2002-01-01"),
                 as.Date("2023-11-01"),
                 by = "1 month")
aux <- aux %>%
  select(-c(ano, mes))

santander <- santander %>% # unimos los datos
  left_join(aux,
            by = "fecha")

# imacec
aux <- rio::import("rial_imacec.xlsx", skip = 2)
colnames(aux) <- c("fecha", "imacec")
aux$fecha <- as.Date(aux$fecha)

santander <- santander %>%
  left_join(aux,
            by = "fecha")

# Correlaciones ----
matriz_correlacion <- cor(santander %>%
                            select(-c(fecha, mes, ano, tipo)))

corrplot(matriz_correlacion, method = "color")

pairs(santander %>%
        select(-c(fecha, mes, ano, tipo)))

# Modelamiento

model <- lm(BS ~ .,
            data = santander %>%
              filter(tipo == "entrenamiento") %>%
              select(-c(fecha, mes, ano, tipo)))

summary(model)

# Modelo significativo
model2 <- lm(BS ~ .,
             data = santander %>%
               filter(tipo == "entrenamiento") %>%
               select(-c(fecha, mes, ano, tipo, UF, consumo, UTM)))

summary(model2)

matriz_correlacion2 <- cor(santander %>%
                            select(-c(fecha, mes, ano, tipo,
                                      UF, consumo, UTM)))

corrplot(matriz_correlacion2, method = "color")

# Análisis residual
qqnorm(model2$residuals)
qqline(model2$residuals, col = "red")

shapiro.test(model2$residuals) # NO rechazamos la normalidad de los residuos
# como p-value > 0.05

# Modelando un SARIMAX ----
fit_auto <- auto.arima(model2$residuals)

source("TS.diag.R")
source("summary.arima.R")
source("salida_TS.R")

salida_TS(model2$residuals, fit_auto, fixed = c(NA, NA))
TS.diag(fit_auto$residuals)

# Jugando con el modelo
fixed <- c(NA, NA, # AR 
           NA # MA
)

fit1 <- forecast::Arima(model2$residuals,
                        order = c(2, 0, 1),
                        seasonal = c(0, 0, 0),
                        fixed = fixed,
                        include.mean = FALSE)

salida_TS(model2$residuals, fit1, fixed = fixed)
TS.diag(fit1$residuals)
