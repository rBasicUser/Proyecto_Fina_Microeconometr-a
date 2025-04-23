library(tidyverse)
library(here)

# --- 1. Cargar datos ---
data <- read_csv(here("Input", "GOOGL_full.csv"), show_col_types = FALSE)

# --- 2. Filtrar entrenamiento hasta el 5 de marzo ---
train_data <- data %>% filter(date <= as.Date("2025-03-05"))

# --- 3. Calcular retornos logarítmicos y lag ---
train_data <- train_data %>%
  arrange(date) %>%
  mutate(
    log_return = log(close / lag(close)),
    lag1 = lag(log_return)
  ) %>%
  drop_na()

# --- 4. Ajustar modelo de regresión ---
modelo4 <- lm(log_return ~ lag1, data = train_data)

# --- 5. Simulación de retornos futuros ---
dias_pred <- as.numeric(as.Date("2025-05-06","%Y-%m-%d")-as.Date("2025-03-05","%Y-%m-%d"))
ret_sim <- numeric(dias_pred)
lag_val <- tail(train_data$log_return, 1)

for (i in 1:dias_pred) {
  ret_sim[i] <- predict(modelo4, newdata = data.frame(lag1 = lag_val))
  lag_val <- ret_sim[i]  # actualizar para la próxima predicción
}

# --- 6. Reconstruir precios a partir de retornos ---
precio_inicial <- tail(train_data$close, 1)
precios_pred <- precio_inicial * exp(cumsum(ret_sim))

# --- 7. Crear fechas futuras ---
fechas_pred <- seq(as.Date("2025-03-06"), by = "day", length.out = dias_pred)

# --- 8. Crear output ---
output_modelo4 <- tibble(
  date = fechas_pred,
  pred = precios_pred
)

# --- 9. Guardar output ---
saveRDS(output_modelo4, here("Output", "output_modelo4.rds"))
