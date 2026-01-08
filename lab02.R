# ============================================================
# Proyecto: Análisis de series temporales - nottem (R)
# Temperaturas mensuales en Nottingham (1920-1939)
# ============================================================

# 0) Paquetes ----
if (!require(tseries)) install.packages("tseries", dependencies = TRUE)
library(tseries)

# 1) Carga del dataset y verificación ----
data("nottem")

cat("Clase del objeto:\n")
print(class(nottem))                 # Debe ser "ts"
cat("\nResumen estadístico:\n")
print(summary(nottem))
cat("\nInicio / Fin / Frecuencia:\n")
print(start(nottem))
print(end(nottem))
print(frequency(nottem))             # 12 (mensual)

# Gráfica inicial (serie temporal)
plot(nottem,
     main = "Temperaturas Mensuales en Nottingham (1920-1939)",
     xlab = "Año", ylab = "Temperatura",
     col = "blue")

# 2) Exploración: descomposición (tendencia, estacionalidad, aleatoriedad) ----
# Serie mensual con estacionalidad anual -> decompose es adecuado
decomp <- decompose(nottem, type = "additive")
plot(decomp)

# 3) Estacionariedad: ACF/PACF + Dickey-Fuller ----
par(mfrow = c(1,2))
acf(nottem, main = "ACF - Serie original")
pacf(nottem, main = "PACF - Serie original")
par(mfrow = c(1,1))

cat("\nADF test (serie original):\n")
adf_original <- adf.test(nottem)
print(adf_original)

# 4) Transformación si no es estacionaria (diferenciación) ----
# Diferencia simple (para quitar posible tendencia). Si fuese necesario, el ADF debería mejorar.
nottem_diff <- diff(nottem, differences = 1)

plot(nottem_diff,
     main = "nottem (1ª diferencia)",
     xlab = "Año", ylab = "Diferencia de temperatura",
     col = "blue")

par(mfrow = c(1,2))
acf(nottem_diff, main = "ACF - 1ª diferencia")
pacf(nottem_diff, main = "PACF - 1ª diferencia")
par(mfrow = c(1,1))

cat("\nADF test (1ª diferencia):\n")
adf_diff <- adf.test(nottem_diff)
print(adf_diff)

# 5) Detección de valores atípicos ----
# Boxplot para detección visual
boxplot(nottem,
        main = "Boxplot - nottem (detección visual de outliers)",
        ylab = "Temperatura")

# Detección simple con IQR (regla 1.5*IQR)
x <- as.numeric(nottem)
q1 <- quantile(x, 0.25)
q3 <- quantile(x, 0.75)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

out_idx <- which(x < lower | x > upper)
out_vals <- x[out_idx]

cat("\nDetección de outliers (regla IQR 1.5):\n")
cat("Límite inferior:", lower, "\n")
cat("Límite superior:", upper, "\n")
cat("Nº de outliers detectados:", length(out_idx), "\n")

# Fechas (año decimal) para localizar outliers en la serie
t_not <- time(nottem)
out_time <- t_not[out_idx]
out_df <- data.frame(time = out_time, value = out_vals)
print(out_df)

# Marcar outliers en la serie temporal
plot(nottem,
     main = "nottem con outliers destacados (IQR)",
     xlab = "Año", ylab = "Temperatura",
     col = "blue")
points(out_time, out_vals, pch = 19, col = "red")

# 6) Interpretación breve (prints) ----
cat("\n================ INTERPRETACIÓN =================\n")
cat("1) La descomposición muestra estacionalidad anual clara en temperaturas.\n")
cat("2) ACF/PACF ayudan a ver autocorrelación y patrón estacional.\n")
cat("3) ADF evalúa estacionariedad; si no lo es, la diferenciación puede mejorarla.\n")
cat("4) Outliers (IQR) se señalan en rojo; revisar si coinciden con inviernos/veranos extremos.\n")
cat("=================================================\n")
