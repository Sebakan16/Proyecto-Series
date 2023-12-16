# Librerías ----
library(tidyverse)
library(dplyr)
library(corrplot)
library(forecast)

# 30 julio 2020 primero retiro
# 10 diciembre 2020
# 28 abril 2021

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

# Gráfico TS ----

# 30 julio 2020 primero retiro
# 10 diciembre 2020
# 28 abril 2021

grafo_TS <- santander %>%
  ggplot(aes(y = BS, x = fecha)) +
  geom_line(color = "#FF1A15", lwd = 1.5) +
  scale_x_date(date_breaks = "2 years",
               date_labels = "%Y",
               guide = guide_axis(angle = 45)) +
  geom_line(data = santander %>%
              filter((fecha > as.Date("2020-07-30") &
                        fecha < as.Date("2022-04-28"))),
            aes(y = BS, x = fecha),
            color = "#3A5FCD", lwd = 1.5) +
  geom_line(data = santander %>%
              filter((tipo == "validacion")),
            aes(y = BS, x = fecha),
            color = "green4", lwd = 1.5) +
  scale_y_continuous(labels = scales::comma_format()) +
  # theme_linedraw() +
  theme_light() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15))



ggsave("grafico_transparente.png",
       plot = grafo_TS,
       bg = "transparent",
       height = 7, width = 7)

# Boxplot serie ----

BP <- santander %>%
  ggplot(aes(x = BS)) +
  geom_boxplot(fill = "#FF1A15", varwidth = 0.5, lwd = 0.5) +
  ylim(c(-0.75, 0.75)) +
  theme_light() +
  scale_x_continuous(labels = scales::comma_format()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15))

ggsave("grafico_boxplot.png",
       plot = BP,
       bg = "transparent",
       height = 7, width = 7)

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



# SARIMA by Seba:    -------
# Intento SARIMA

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

# ?forecast::Arima

lambda <- forecast::BoxCox.lambda(Y, method = "guerrero")
plot(forecast::BoxCox(Y, lambda = lambda), col = "steelblue")
LSTS::periodogram(Y)

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

fit_diff <- forecast::Arima(Y, 
                            order = c(2, 1, 29),
                            seasonal = c(0, 0, 5),
                            fixed = fixed,   # Si no corres esto se muere el pc
                            include.mean = FALSE,
                            include.drift = T)

salida_TS(Y, fit_diff, fixed = fixed)

Box.Ljung.Test(fit_diff$residuals, lag = 40)
abline(v = 30)
plot(forecast::forecast(fit_diff, h = 12))

