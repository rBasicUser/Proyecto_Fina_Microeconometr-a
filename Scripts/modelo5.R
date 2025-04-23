library(forecast)
library(tidyverse)
library(here)

# --- 1. Cargar datos ---
data <- read_csv(here("Input", "GOOGL_full.csv"), show_col_types = FALSE)

# --- 2. Filtrar entrenamiento hasta el 5 de marzo ---
train_data <- data %>% filter(date <= as.Date("2025-03-05"))

# --- 3. Ordenar y extraer la serie ---
serie <- train_data %>% arrange(date) %>% pull(close)
fechas <- train_data %>% arrange(date) %>% pull(date)

# --- 4. Ajustar modelo AR(1) con drift ---
modelo <- Arima(serie, order = c(1, 0, 1), include.drift = TRUE)

# --- 5. Predicción futura (30 días hábiles) ---
dias <- as.numeric(as.Date("2025-05-06") - as.Date("2025-03-05"))
forecast_result <- forecast(modelo, h = dias)

# --- 6. Generar fechas futuras hábiles ---
fechas_pred <- seq(as.Date("2025-03-06"), by = "day", length.out = dias_pred)

# --- 7. Salida como tibble ---
output_modelo5 <- tibble(
  date = fechas_pred,
  pred = as.numeric(forecast_result$mean)
)

# --- 8. Guardar el archivo de salida ---

saveRDS(output_modelo5, here("Output", "output_modelo5.rds"))



