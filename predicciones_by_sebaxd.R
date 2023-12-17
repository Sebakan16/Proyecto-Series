
obs <- santander %>%
  filter(tipo == "validacion") %>%
  select(fecha, BS) #%>%
  mutate(BS = log(BS))

pred_sarima <- forecast::forecast(fit_diff2, h = 12)
pred_sarima <- as.data.frame(pred_sarima)

pred_sarimax <- forecast::forecast(fit_diffx2, h = 12, xreg = santander$dolar[137:148])
pred_sarimax <- as.data.frame(pred_sarimax)

predicciones <- obs
predicciones$sarima <- exp(pred_sarima$`Point Forecast`)
predicciones$sarimax <- exp(pred_sarimax$`Point Forecast`)
predicciones$fecha <- as.Date(predicciones$fecha)

pred_grafo <- predicciones %>%
  ggplot(aes(x = fecha, y = BS)) +
  geom_line(color = "#FF1A15", lwd = 1) +
  geom_line(aes(x = fecha, y = sarima), color = "blue", lwd = 1) +
  geom_line(aes(x = fecha, y = sarimax), color = "green4", lwd = 1) +
  geom_ribbon(aes(ymin = exp(pred_sarima$`Lo 95`),
                  ymax = exp(pred_sarima$`Hi 95`)),
              alpha = 0.2, fill = "turquoise3") +
  geom_ribbon(aes(ymin = exp(pred_sarimax$`Lo 95`),
                  ymax = exp(pred_sarimax$`Hi 95`)),
              alpha = 0.2, fill = "green3") +
  geom_line(data = santander %>%
              filter(fecha > as.Date("2020-12-31")),
            aes(x = as.Date(fecha), y = BS),
            color = "#FF1A15", lwd = 1) +
  theme_light() +
  scale_y_continuous(labels = scales::comma_format()) +
  # scale_x_date(date_breaks = "3 months",
  #              date_labels = "%M") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.text.x = element_text(size = 15))

ggsave("grafico_predicciones.png",
       plot = pred_grafo ,
       bg = "transparent",
       height = 7, width = 7)

# SARIMA ----
summary(forecast::forecast(fit_diff2, h = 12))
# SARIMAX ----
summary(forecast::forecast(fit_diffx2, h = 12, xreg = santander$dolar[137:148]))

