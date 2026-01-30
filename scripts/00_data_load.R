# ==============================================================================
# PROYECTO: IHEA-encuesta (UAH)
# SCRIPT: 00_data_load.R
# OBJETIVO: Consolidar excels y generar base limpia para reportabilidad
# ==============================================================================

# 1. Cargar librerías necesarias
library(tidyverse)
library(readxl)
library(janitor)
library(here) # Para manejo robusto de rutas y evitar errores de directorio

# 2. Configuración de rutas
# 'here' detecta automáticamente la raíz de tu proyecto (.Rproj)
path_data <- here("data")
archivos <- list.files(path = path_data, pattern = "\\.xlsx$", full.names = TRUE)

# 3. Función de lectura y limpieza inicial
# Saltamos 7 filas según el formato de descarga de la encuesta IHEA
leer_encuesta_uah <- function(ruta) {
  read_excel(ruta, skip = 7) %>% 
    clean_names() %>%
    # Filtramos filas vacías basándonos en el RUT
    filter(!is.na(rut)) %>% 
    mutate(
      fuente_archivo = basename(ruta),
      # Convertimos todo a character temporalmente para evitar errores de unión
      across(everything(), as.character) 
    )
}

# 4. Carga, Consolidación y Conversión de tipos
# Usamos type_convert() sin argumentos adicionales para que detecte números y fechas
ihea_raw <- archivos %>%
  map_dfr(leer_encuesta_uah) %>%
  type_convert() 

# 5. Procesamiento y Estandarización Final
ihea_final <- ihea_raw %>%
  mutate(
    # Limpieza de RUT: eliminamos puntos, guiones y el prefijo 'rt'
    rut = str_remove_all(rut, "rt|[\\.\\-]"),
    
    # Estandarizar nombres de carrera: Primera letra mayúscula y sin espacios extra
    carrera = str_trim(str_to_title(carrera_s)),
    
    # Trazabilidad de la modalidad de aplicación
    modalidad = case_when(
      str_detect(fuente_archivo, "SALA") ~ "En Sala",
      str_detect(fuente_archivo, "CENTRALIZADA") ~ "Centralizada",
      str_detect(fuente_archivo, "DIRECTA") ~ "Directa",
      TRUE ~ "Otra"
    ),
    
    # Asegurar que la fecha sea reconocida como objeto temporal
    fecha_inicio = as.POSIXct(fecha_inicio)
  ) %>%
  # Reorganizar columnas para el análisis
  select(
    rut, nombre, email, carrera, modalidad, 
    fecha_inicio, 
    preguntas_omitidas = x30, 
    starts_with("p_"), 
    fuente_archivo
  )

# 6. Exportación de Seguridad (Base de Datos Lista para Quarto)
# Se guarda en la raíz para que el template de Quarto la encuentre fácilmente
saveRDS(ihea_final, here("data_reproduction.rds"))

# Mensajes de control en consola
cat("\n--- REPORTE DE CARGA ---")
cat("\n✅ Registros totales procesados:", nrow(ihea_final))
cat("\n🎓 Carreras detectadas:", length(unique(ihea_final$carrera)))
cat("\n💾 Archivo generado:", here("data_reproduction.rds"), "\n")
glimpse(ihea_final)
