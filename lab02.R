# ============================================================
# Proyecto: Análisis de series temporales - AirPassengers (R)
# ============================================================

# 0) Paquetes ----
if (!require(tseries)) install.packages("tseries", dependencies = TRUE)
library(tseries)

# 1) Carga del dataset y verificación ----
data("AirPassengers")

cat("Clase del objeto:\n")
print(class(AirPassengers))             # Debe ser "ts"
cat("\nResumen estadístico:\n")
print(summary(AirPassengers))
cat("\nInicio / Fin / Frecuencia:\n")
print(start(AirPassengers))
print(end(AirPassengers))
print(frequency(AirPassengers))         # 12 (mensual)

# 2) Exploración inicial ----
# Gráfico de la serie
plot(AirPassengers,
     main = "AirPassengers (1949-1960): Pasajeros mensuales",
     xlab = "Año", ylab = "Nº de pasajeros")

# Estadísticas descriptivas básicas
cat("\nEstadísticas descriptivas básicas:\n")
cat("Media:", mean(AirPassengers), "\n")
cat("Desviación estándar:", sd(AirPassengers), "\n")
cat("Mínimo:", min(AirPassengers), "\n")
cat("Máximo:", max(AirPassengers), "\n")

# 3) Tendencia y estacionalidad (descomposición) ----
decomp <- decompose(AirPassengers, type = "multiplicative")
plot(decomp)

# 4) Estacionariedad: ACF/PACF + Dickey-Fuller ----
par(mfrow = c(1,2))
acf(AirPassengers, main = "ACF - Serie original")
pacf(AirPassengers, main = "PACF - Serie original")
par(mfrow = c(1,1))

cat("\nADF test (serie original):\n")
adf_original <- adf.test(AirPassengers)
print(adf_original)

# Si no es estacionaria, diferenciar una vez
AirPassengers_diff <- diff(AirPassengers, differences = 1)

plot(AirPassengers_diff,
     main = "Serie diferenciada (1ª diferencia)",
     xlab = "Año", ylab = "Diferencia mensual (pasajeros)")

par(mfrow = c(1,2))
acf(AirPassengers_diff, main = "ACF - 1ª diferencia")
pacf(AirPassengers_diff, main = "PACF - 1ª diferencia")
par(mfrow = c(1,1))

cat("\nADF test (1ª diferencia):\n")
adf_diff <- adf.test(AirPassengers_diff)
print(adf_diff)

# 5) Detección de valores atípicos ----
# Boxplot para detectar outliers de forma visual
boxplot(AirPassengers,
        main = "Boxplot - AirPassengers (detección visual de outliers)",
        ylab = "Nº de pasajeros")

# Detección simple con IQR (sobre valores de la serie)
x <- as.numeric(AirPassengers)
q1 <- quantile(x, 0.25)
q3 <- quantile(x, 0.75)
iqr <- q3 - q1
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

outlier_idx <- which(x < lower | x > upper)
outlier_vals <- x[outlier_idx]

cat("\nDetección de outliers (regla IQR 1.5):\n")
cat("Límite inferior:", lower, "\n")
cat("Límite superior:", upper, "\n")
cat("Nº de outliers detectados:", length(outlier_idx), "\n")

# Convertir índices a fechas (año-mes) para reportar
tp <- time(AirPassengers)
outlier_time <- tp[outlier_idx]
out_df <- data.frame(time = outlier_time, value = outlier_vals)
print(out_df)

# Destacar outliers sobre la serie
plot(AirPassengers,
     main = "AirPassengers con outliers destacados (IQR)",
     xlab = "Año", ylab = "Nº de pasajeros")
points(outlier_time, outlier_vals, pch = 19)

# 6) Interpretación (prints cortos) ----
cat("\n================ INTERPRETACIÓN =================\n")
cat("1) Tendencia/estacionalidad: la serie muestra tendencia creciente y estacionalidad anual.\n")
cat("2) Estacionariedad: el ADF en la serie original suele NO rechazar estacionariedad; tras diferenciar, normalmente mejora.\n")
cat("3) Outliers: los puntos marcados suelen corresponder a picos asociados a meses de alta demanda (estacionalidad) o variaciones anómalas.\n")
cat("=================================================\n")
