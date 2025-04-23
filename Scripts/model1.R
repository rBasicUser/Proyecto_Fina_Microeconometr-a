# Definir los datos
library(tidyverse)
library(here)

here("Input/GOOGL_2025-04-12.csv" )

ALPHABET <- read.csv(here("Input","GOOGL_2025-04-12.csv"))

# Filtrar datos hasta el 2025-03-05 para el benchmark

train_data <- ALPHABET %>% filter(date <= as.Date("2025-03-05"))

# Definir número de días de la simulación

day_diff <- as.numeric(as.Date("2025-05-06","%Y-%m-%d")-as.Date("2025-03-05","%Y-%m-%d"))

# Configuración de los parámetros de simulación


precioM <- mean(train_data$close)

      ###### MEJORA SUSTANCIAL RESPECTO AL MODELO 0 AUMENTAR NÚMERO DE SIMULACIONES ####

escenarios <- 10000
dias <- day_diff  # Número de días a simular
sdAlphabet <- sd(train_data$close)

# Matriz de simulaciones
precioMatris <- matrix(nrow=dias, ncol=escenarios)
precioMatris[1,] <- precioM

# Simulaciones de Monte Carlo

set.seed(42)

for (i in 1:escenarios) {
  for (t in 2:dias) {
    precioMatris[t, i] <- precioMatris[t-1, i] + rnorm(1, mean=0, sd=sdAlphabet)
  }
}

# Visualización de los resultados
if (interactive()) {
  ts.plot(precioMatris)
}

# Tabla de resultados

fecha_inicio_predicciones <- as.Date("2025-03-06")
fechas <- seq(fecha_inicio_predicciones, by = "day", length.out = nrow(precioMatris))

output_model1 <- data.frame(
  date = fechas,
  pred = rowMeans(precioMatris)
)

saveRDS(output_model1, here("Output", "output_modelo1.rds"))