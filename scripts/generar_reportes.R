library(tidyverse)
#install.packages("writexl")
library(writexl) # Para exportar a Excel

# 1. Asegurar que la carpeta de salida existe
if (!dir.exists("outputs/reportes_carreras")) {
  dir.create("outputs/reportes_carreras", recursive = TRUE)
}

# 2. Redefinir la función con una acción de escritura
generar_reporte <- function(nombre_carrera) {
  
  # Limpiar el nombre para que sea un nombre de archivo válido
  nombre_archivo <- str_replace_all(nombre_carrera, "[^[:alnum:]]", "_")
  
  # Filtrar datos
  data_subset <- ihea_final %>% 
    filter(carrera == nombre_carrera)
  
  # --- ACCIÓN DE GUARDADO ---
  # Guardamos un archivo Excel por carrera en la carpeta especificada
  ruta_salida <- paste0("outputs/reportes_carreras/IHEA_", nombre_archivo, ".xlsx")
  
  write_xlsx(data_subset, path = ruta_salida)
  # ---------------------------
  
  message(paste("✓ Archivo guardado:", ruta_salida))
}

# 3. Ejecutar nuevamente
walk(carreras, generar_reporte)