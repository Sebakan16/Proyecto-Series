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

Y <- santander %>%
  filter(tipo == "entrenamiento") %>% 
  # filter((fecha < as.Date("2020-07-30") |
  #           fecha > as.Date("2022-04-28"))) %>%
  select(BS)

Y <- ts(Y$BS, frequency = 12)

# Gráfico TS ----

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

grafo_TS

# Boxplot serie ----

BP <- santander %>%
  ggplot(aes(x = BS)) +
  geom_boxplot(fill = "#FF1A15", alpha =0.8, varwidth = 0.5, lwd = 0.5) +
  ylim(c(-0.75, 0.75)) +
  theme_light() +
  scale_x_continuous(labels = scales::comma_format()) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15))
BP


# SARIMA ------------------------------------------------------------------

fixed2 <- c(0, NA, # AR
            NA, NA, 0, NA, 0,  # MA
            0, # SAR
            0, 0, 0, 0, NA  # SMA
)

fit_diff2 <- forecast::Arima(log(Y), 
                             order = c(2, 1, 5),
                             seasonal = c(1, 0, 5),
                             fixed = fixed2,
                             include.mean = FALSE,
                             include.drift = FALSE
)

salida_TS(log(Y), fit_diff2, fixed = fixed2)
coef(fit_diff2)

Box.Ljung.Test(fit_diff2$residuals, lag = 50)
plot(forecast::forecast(fit_diff2, h = 12))
TS.diag(fit_diff2$residuals)


# SARIMAX -----------------------------------------------------------------

pesogringo <- rio::import("dolar.xlsx", skip = 2)
colnames(pesogringo) <- c("fecha", "dolar")
santander <- santander %>% # Unir datos
  left_join(pesogringo,
            by = "fecha")
 
fixedx2 <- c(NA, NA, # AR
             0, 0, 0, 0, 0,  # MA
             0, # SAR
             0, 0, 0, 0, NA,  # SMA
             NA)


fit_diffx2 <- forecast::Arima(log(Y), 
                              order = c(2, 1, 5),
                              seasonal = c(1, 0, 5),
                              fixed = fixedx2,
                              xreg = santander$dolar[1:136],
                              include.mean = TRUE,
                              include.drift = FALSE
)

salida_TS(log(Y), fit_diffx2, fixed = fixedx2)

Box.Ljung.Test(fit_diffx2$residuals, lag = 50)
plot(forecast::forecast(fit_diffx2, h = 12, xreg = santander$dolar[137:148]))
TS.diag(fit_diffx2$residuals)
plot(fit_diffx2)