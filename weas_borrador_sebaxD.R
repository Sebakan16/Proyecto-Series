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
                            select(-c(fecha, mes, ano, tipo)) %>%
                            mutate(BS = log(BS),
                                   UTM = log(UTM),
                                   UF = log(UF),
                                   IVP = log(IVP)))
matriz_correlacion[, 1]

corrplot(matriz_correlacion, method = "color")

pairs(santander %>%
        select(-c(fecha, mes, ano, tipo)))

Y <- santander %>%
  filter(tipo == "entrenamiento") %>% 
  # filter((fecha < as.Date("2020-07-30") |
  #           fecha > as.Date("2022-04-28"))) %>%
  select(BS)

Y <- ts(Y$BS, frequency = 12)

# Modelando un SARIMA ----

lambda <- forecast::BoxCox.lambda(log(Y), method = "guerrero")
f.Y <- forecast::BoxCox(log(Y), lambda = lambda)
plot(f.Y, lwd = 3, col = "gray", ylab = "", main = "Transformación Box-Cox Imacec No Minero")

## Diferenciamos?

d <- forecast::ndiffs(f.Y)
plot(diff(f.Y, differences = d), main = expression((1-B)*f(Y[t])), ylab = "")
acf(diff(f.Y, differences = d), lag.max = 120)
## Diferenciamos Estacionalmente?
s <- frequency(f.Y)
s
D <- forecast::nsdiffs(diff(f.Y, differences = d))
## Z[t] = (1-B)(1-B^s) f(Y[t]), s = 12.
Z <- diff(diff(f.Y, differences = d, lag = 1), lag = s) # differences = D = 0
plot(Z, main = expression((1-B)*(1-B^s)*f(Y[t])), ylab = "")

## ACF parte regular
par(mfrow = c(1,2))
acf(c(Z), lag.max = 11, ylim = c(-0.2,+1), main = "")
pacf(c(Z), lag.max = 11, ylim = c(-0.2,+1), xlim = c(0,11), main = "")
## p = 2, q = 2 o 5

## ACF parte estacional
par(mfrow = c(1,2))
acf(c(Z), lag.max = 48, ylim = c(-1,+1), main = "")
pacf(c(Z), lag.max = 60, ylim = c(-1,+1), xlim = c(0,60), main = "")
## p = 1, q = 1 o 4

#   autoarima
fit <- forecast::auto.arima(log(Y), d = d, D = D,
                            max.p = 2, max.q = 5,
                            max.P = 1, max.Q = 4)
fit
plot(fit)
salida_TS(log(Y), fit, fixed = c(NA,NA,NA))
TS.diag(fit$residuals)
Box.Ljung.Test(fit$residuals, lag = 40)

# jugando
fixed = c(0, NA, # AR
          NA, NA, 0, NA, # MA
          0, 0, 0, 0, NA # SMA
)

fit2 <- forecast::Arima(log(Y),
                        order = c(2, 1, 4),
                        seasonal = c(0, 0, 5),
                        include.mean = FALSE,
                        fixed = fixed)
# fit2
plot(fit2, lwd = 0.1)
salida_TS(log(Y), fit2, fixed = fixed)
TS.diag(fit2$residuals)
Box.Ljung.Test(fit2$residuals, lag = 50)

pred_profe <- forecast::forecast(fit2, h = 12)
plot(pred_profe)


# SARIMAX ----

exog_var <- santander %>%
  filter(tipo == "entrenamiento") %>%
  select(IVP) %>%
  mutate(IVP = log(IVP))
exog_var <- as.matrix(exog_var)

fixed <- c(0, NA, # AR
           0, 0, 0, 0, # MA  1, 2, 4 NA
           0, 0, 0, 0, NA, # SMA
           rep(NA, ncol(exog_var))
)

fite <- forecast::Arima(log(Y),
                        order = c(2, 1, 4),
                        seasonal = c(0, 0, 5),
                        fixed = fixed,
                        xreg = exog_var)
salida_TS(log(Y), fite, fixed = fixed)
TS.diag(fite$residuals)

pre <- forecast::forecast(fite, h = 12, xreg = log(new_exog[, 1]))
plot(pre)



# para ver
fixed <- c(0, NA, # AR
           NA, NA, 0, NA, # MA  1, 2, 4 NA
           0, 0, 0, 0, NA, # SMA
           rep(NA, ncol(exog_var))
)

fitexd <- forecast::Arima(log(Y),
                          order = c(2, 1, 4),
                          seasonal = c(0, 0, 5),
                          fixed = fixed,
                          xreg = exog_var)
plot(fitexd)
salida_TS(log(Y), fitexd, fixed = fixed)
TS.diag(fitexd$residuals)

pre <- forecast::forecast(fitexd, h = 12, xreg = log(new_exog[, 1]))
plot(pre)

# SARIMA by Seba:    -------
# Intento SARIMA



