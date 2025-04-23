# Definir los datos
library(tidyverse)
library(here)

# Carga de datos

ALPHABET <- read.csv(here("Input","GOOGL_2025-04-12.csv"))

# Filtrar datos hasta el 2025-03-05 para el benchmark

train_data <- ALPHABET %>% filter(date <= as.Date("2025-03-05"))


# Definir número de días de la simulación

day_diff <- as.numeric(as.Date("2025-05-06","%Y-%m-%d")-as.Date("2025-03-05","%Y-%m-%d"))

# 2. Extraer la columna de precios y ordenarlos del más antiguo al más reciente
precios <- train_data$close

# 3. Calcular retornos logarítmicos
retornos <- diff(log(precios))
media_ret <- mean(retornos)
sd_ret <- sd(retornos)



# 4. Configurar parámetros de simulación

dias <- as.numeric(as.Date("2025-05-06","%Y-%m-%d")-as.Date("2025-03-05","%Y-%m-%d"))
simulaciones <- 100000
precio_inicial <- tail(precios, 1)

# 5. Realizar simulación Monte Carlo
set.seed(42)
sim_matrix <- matrix(NA, nrow = dias, ncol = simulaciones)

for (i in 1:simulaciones) {
  ret_sim <- rnorm(dias, mean = media_ret, sd = sd_ret)
  log_precios <- cumsum(ret_sim) + log(precio_inicial)
  sim_matrix[, i] <- exp(log_precios)
}




# Tabla de resultados

fecha_inicio_predicciones <- as.Date("2025-03-06")
fechas <- seq(fecha_inicio_predicciones, by = "day", length.out = nrow(sim_matrix))

output_model2 <- data.frame(
  date = fechas,
  pred = rowMeans(sim_matrix)
)


saveRDS(output_model2, here("Output", "output_modelo2.rds"))
