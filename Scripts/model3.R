# Definir los datos
library(tidyverse)
library(here)

ALPHABET <- read.csv(here("Input","GOOGL_full.csv"))

# Filtrar datos hasta el 2025-03-05 para el benchmark
train_data <- ALPHABET %>% filter(date <= as.Date("2025-03-05"))

# Definir número de días de la simulación
dias <- as.numeric(as.Date("2025-05-06") - as.Date("2025-03-05"))
simulaciones <- 150

# 1. Extraer precios históricos ordenados (más antiguos primero)
precios <- train_data$close

# 2. Calcular retornos logarítmicos
retornos <- diff(log(precios))

# 3. Estimar media de retornos
media_ret <- mean(retornos)

# 4. Ajustar la volatilidad artificialmente
volatility_factor <- 4
new_sd <- sd(retornos) * volatility_factor
new_var <- new_sd^2

# 5. Calcular el drift corregido con la nueva varianza
drift <- media_ret - 0.5 * new_var

# 6. Precio de inicio para la simulación
precio_inicial <- tail(precios, 1)

# 7. Inicializar matriz de simulación
set.seed(42)
sim_matrix <- matrix(NA, nrow = dias, ncol = simulaciones)
sim_matrix[1, ] <- precio_inicial

# 8. Simulación Monte Carlo con drift corregido
for (i in 1:simulaciones) {
  for (t in 2:dias) {
    ruido <- rnorm(1, mean = 0, sd = new_sd)
    sim_matrix[t, i] <- sim_matrix[t - 1, i] * exp(drift + ruido)
  }
}

# 9. Convertir a DataFrame para graficar
sim_df <- data.frame(
  Dia = rep(1:dias, simulaciones),
  Precio = as.vector(sim_matrix),
  Simulacion = rep(1:simulaciones, each = dias)
)

# 10. Tabla de resultados
fecha_inicio_predicciones <- as.Date("2025-03-06")
fechas <- seq(fecha_inicio_predicciones, by = "day", length.out = nrow(sim_matrix))

output_model3 <- data.frame(
  date = fechas,
  pred = rowMeans(sim_matrix)
)

# 11. Guardar el archivo
saveRDS(output_model3, here("Output", "output_modelo3.rds"))
