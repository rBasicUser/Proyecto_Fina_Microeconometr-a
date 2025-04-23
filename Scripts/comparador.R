library(tidyverse)
library(here)

# --- Cargar predicciones de todos los modelos ---
archivos <- list.files(here("Output"), pattern = "output_modelo.*\\.rds", full.names = TRUE)

# --- Leer, estandarizar y combinar ---
cargar_modelo <- function(path) {
  modelo <- str_extract(basename(path), "modelo\\d")
  df <- readRDS(path) %>%
    mutate(modelo = modelo)
  return(df)
}

todas_las_preds <- map_dfr(archivos, cargar_modelo)

# --- Cargar la serie real para comparar ---
serie_real <- read_csv(here("Input", "GOOGL_full.csv"), show_col_types = FALSE) %>%
  select(date, close) %>%
  rename(real = close)

# --- Archivo de Exportación de Excel ---

todas_las_preds |>
  left_join(serie_real, by = "date")|>
  pivot_wider(id_cols = c(date, real), names_from = modelo, values_from = pred) -> export

write.csv(export,here("Output", "series.csv"))

# --- Unir predicciones con valores reales ---
todas_las_preds |>
  left_join(serie_real, by = "date")-> comparacion

write.csv(comparacion,here("Output", "comparacion.csv"))

# --- Calcular métricas por modelo ---
metricas <- comparacion %>%
  group_by(modelo) %>%
  summarise(
    MAE = mean(abs(pred - real), na.rm = TRUE),
    RMSE = sqrt(mean((pred - real)^2, na.rm = TRUE)),
    MAPE = mean(abs((pred - real) / real), na.rm = TRUE)
  ) %>%
  arrange(MAE)

# --- Guardar tabla de métricas ---
write_csv(metricas, here("Output", "metricas_comparacion.csv"))

# --- Mostrar resumen ---
print(metricas)
