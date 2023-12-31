salida_TS = function(Y, fit, fixed){
print(summary(fit))
print(summary_arima(fit, fixed = fixed))

M = cbind(ks.test(scale(fit$res), "pnorm")$p.value,
lmtest::bptest(fit$res ~ time(fit$res))$p.value)
colnames(M) = c("Kolmogorov-Smirnov test", "Breusch-Pagan test")
rownames(M) = "p-value"
print(M)

r2 = cbind(1-var(Y-fit$fitted, na.rm = T)/var(Y, na.rm = T))
colnames(r2) = "Adjusted R-squared"
rownames(r2) = ""
print(r2)
}


