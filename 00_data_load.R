

library(tidyverse)
library(tidyverse)
library(readxl)
library(janitor)


list.files("data")


# 1. Listar solo archivos Excel
path_data <- "/home/rober/Documentos/cefau/IHEA-encuesta/data/"
files_full <- list.files(path = path_data, pattern = "\\.xlsx$", full.names = TRUE)

# 2. Crear nombres de objeto "limpios" (ej: "bd_directa" en vez del nombre largo)
# Esto evita problemas con espacios y caracteres especiales
obj_names <- files_full %>% 
  basename() %>% 
  tools::file_path_sans_ext() %>% 
  make_clean_names()

# 3. Función para cargar y limpiar nombres de columnas internamente
cargar_limpio <- function(ruta) {
  read_excel(ruta) %>% 
    clean_names()
}

# 4. Cargar al Environment
# walk2 aplica la función y 'assign' guarda el resultado con el nombre limpio
walk2(files_full, obj_names, ~ assign(.y, cargar_limpio(.x), envir = .GlobalEnv))

# Confirmación de objetos cargados
message("Objetos cargados en el Environment:")
print(obj_names)


data <- rbind(bd_encuesta_de_intereses_en_sala_28_ene, 
  bd_respuestas_encuesta_de_intereses_centralizada_28_ene,
bd_respuestas_encuesta_de_intereses_directa)


glimpse(data)







library(tidyverse)
library(readxl)
library(janitor)

# 1. Definir la ruta y archivos
path_data <- "/home/rober/Documentos/cefau/IHEA-encuesta/data/"
archivos <- list.files(path = path_data, pattern = "\\.xlsx$", full.names = TRUE)

# 2. Función ajustada para saltar el "ruido" inicial
leer_encuesta_uah <- function(ruta) {
  read_excel(ruta, skip = 7) %>% # SALTAMOS LAS PRIMERAS 7 FILAS
    clean_names() %>%
    # Filtramos filas que sean totalmente NA o que repitan el encabezado
    filter(!is.na(rut)) %>% 
    mutate(
      fuente = basename(ruta),
      across(everything(), as.character) # Previene errores de tipo al unir
    )
}

# 3. Cargar y unir todo en un solo paso
ihea_limpia <- archivos %>%
  map_dfr(leer_encuesta_uah) %>%
  # type_convert() sin argumentos adicionales intentará detectar números y fechas
  type_convert() 

# Verificación inmediata de tipos de columna
spec(ihea_limpia)

# 4. Verificación de calidad
glimpse(ihea_limpia)


library(tidyverse)
library(lubridate)

ihea_final <- ihea_limpia %>%
  # 1. Limpieza de Identificadores y Nombres
  mutate(
    # Quitar el prefijo 'rt', puntos o guiones si existieran
    rut = str_remove_all(rut, "rt|[\\.\\-]"),
    # Estandarizar carreras a Título (opcional, ayuda a visualización)
    carrera = str_to_title(carrera_s)
  ) %>%
  
  # 2. Corrección de Fechas
  mutate(
    fecha_finalizacion = as.POSIXct(fecha_finalizacion, format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  
  # 3. Renombrar columnas técnicas
  rename(
    preguntas_omitidas = x30
  ) %>%
  
  # 4. Trazabilidad: Simplificar el nombre de la fuente
  mutate(modalidad = case_when(
    str_detect(fuente, "SALA") ~ "En Sala",
    str_detect(fuente, "CENTRALIZADA") ~ "Centralizada",
    str_detect(fuente, "DIRECTA") ~ "Directa",
    TRUE ~ "Otra"
  )) %>%
  
  # 5. Ordenar columnas (Identificadores primero, luego respuestas)
  select(rut, nombre, email, carrera, modalidad, fecha_inicio, preguntas_omitidas, starts_with("p_"), fuente)

# --- CHEQUEO DE CALIDAD: DUPLICADOS ---
duplicados <- ihea_final %>%
  group_by(rut) %>%
  filter(n() > 1) %>%
  arrange(rut)

message("Número de estudiantes con más de una respuesta: ", nrow(duplicados))


glimpse(ihea_final)


library(tidyverse)
library(janitor)

# 1. Limpieza previa de la base final para asegurar trazabilidad
ihea_final <- ihea_final %>%
  mutate(carrera = str_trim(carrera)) %>% # Quita espacios accidentales
  filter(!is.na(carrera), carrera != "")   # Elimina registros sin carrera

# 2. Definición del vector de iteración
carreras <- unique(ihea_final$carrera)

# 3. Función de reporte (Asegúrate de que el filtro sea exacto)
generar_reporte <- function(nombre_carrera) {
  
  # Filtrado de datos para la carrera actual
  data_subset <- ihea_final %>% 
    filter(carrera == nombre_carrera)
  
  # Validación de seguridad
  if (nrow(data_subset) == 0) {
    warning(paste("La carrera", nombre_carrera, "no tiene datos. Saltando..."))
    return(NULL)
  }
  
  # Lógica de renderizado (ejemplo con Quarto/RMarkdown)
  # rmarkdown::render("template_reporte.Rmd", 
  #                   params = list(data = data_subset, titulo = nombre_carrera),
  #                   output_file = paste0("outputs/Reporte_", nombre_carrera, ".html"))
  
  message(paste("✓ Reporte generado para:", nombre_carrera))
}

# 4. Ejecución Masiva con purrr
message("Iniciando procesamiento de ", length(carreras), " carreras...")

# Usamos walk para efectos secundarios (guardar archivos)
walk(carreras, generar_reporte)
