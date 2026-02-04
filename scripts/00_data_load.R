# ==============================================================================
# SCRIPT: 00_data_load.R (Versión Actualizada - Lógica Binaria de Admisión)
# Objetivo:
#  (1) Cargar respuestas IHEA (DIURNO PRESENCIAL) y limpiar/estandarizar
#  (2) Expandir matrices (P12, P15, P16, P18)
#  (3) Cargar MATRÍCULAS con nueva regla de negocio:
#      - Centralizada -> "Centralizada"
#      - Todo lo demás -> "Directa" (salvo NAs)
#  (4) FILTRAR: Eliminar respuestas de estudiantes NO matriculados
#  (5) Calcular coberturas y estadísticas
#  (6) Guardar outputs en formato lista y archivos individuales
# ==============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(stringr)

# ------------------------------------------------------------------------------
# 0) Parámetros
# ------------------------------------------------------------------------------
archivo_path <- here("data", "BD respuestas encuesta IHEA 2026.xlsx")
sheet_respuestas <- "encuesta IHE"
sheet_matriculas <- "MATRIC CENTRA Y DIRECTA"

if (!file.exists(archivo_path)) {
  stop("❌ ERROR CRÍTICO: No existe el archivo Excel en: ", archivo_path)
}

# ------------------------------------------------------------------------------
# 1) Cargar Respuestas (todo como texto para evitar problemas con RUT)
# ------------------------------------------------------------------------------
message("🔄 Cargando hoja de respuestas IHEA...")

ihea_raw <- read_excel(
  archivo_path,
  sheet = sheet_respuestas,
  skip = 0,
  col_types = "text"
) %>%
  clean_names()

# ------------------------------------------------------------------------------
# 2) Procesamiento Respuestas
# ------------------------------------------------------------------------------
message("🔄 Procesando y limpiando respuestas...")

ihea_final <- ihea_raw %>%
  rename(carrera_raw = any_of(c("carrera_s", "carrera", "nombre_carrera", "x_carrera"))) %>%
  filter(!is.na(rut)) %>%
  mutate(
    rut = as.character(rut),
    rut = str_remove_all(rut, "rt|[\\.\\-\\s]"), # Limpieza RUT
    rut = str_trim(rut),

    carrera_raw = as.character(carrera_raw),
    carrera_raw = str_squish(carrera_raw),

    carrera = str_trim(str_to_title(carrera_raw)),
    modalidad = "Diurno Presencial"
  ) %>%
  filter(!is.na(rut), rut != "") %>%
  filter(!is.na(carrera), carrera != "") %>%

  # --- EXPANSIÓN DE MATRICES (P12, P15, P16, P18) ---
  separate(p_12, into = paste0("p_12_", letters[1:14]), sep = ",", fill = "right", remove = TRUE) %>%
  separate(p_15, into = paste0("p_15_", letters[1:8]),  sep = ",", fill = "right", remove = TRUE) %>%
  separate(p_16, into = paste0("p_16_", letters[1:5]),  sep = ",", fill = "right", remove = TRUE) %>%
  separate(p_18, into = paste0("p_18_", letters[1:4]),  sep = ",", fill = "right", remove = TRUE) %>%

  select(
    rut,
    any_of(c("nombres", "nombre", "nombre_completo")),
    contains("email"),
    carrera,
    modalidad,
    starts_with("p_")
  )

# ------------------------------------------------------------------------------
# 3) Cargar Matrículas con Regla de Negocio Actualizada
# ------------------------------------------------------------------------------
message("🔄 Cargando matrículas y aplicando reglas de negocio...")

matriculas_raw0 <- read_excel(
  archivo_path,
  sheet = sheet_matriculas,
  col_names = FALSE,
  col_types = "text"
) %>%
  janitor::clean_names()

# Buscar dinámicamente la fila donde aparece "RUT" (cabecera real)
header_row <- which(apply(matriculas_raw0, 1, function(r) any(str_to_upper(r) == "RUT", na.rm = TRUE)))[1]

if (is.na(header_row)) {
  stop("❌ ERROR: No encontré ninguna fila con 'RUT' en la hoja de matrículas.")
}

# Extraer nombres de columnas correctos
new_names <- matriculas_raw0 %>%
  slice(header_row) %>%
  unlist(use.names = FALSE) %>%
  as.character() %>%
  str_squish()

# Armar tabla con cabeceras correctas
matriculas_raw <- matriculas_raw0 %>%
  slice((header_row + 1):n()) %>%
  setNames(new_names) %>%
  janitor::clean_names()

# Detectar columna de carrera
if (!"carrera" %in% names(matriculas_raw)) {
  car_candidates <- names(matriculas_raw)[str_detect(names(matriculas_raw), "carrera|programa|plan|nombre")]
  if (length(car_candidates) == 0) stop("❌ ERROR: No encontré columna de carrera en matrículas.")
  matriculas_raw <- matriculas_raw %>% dplyr::rename(carrera = all_of(car_candidates[1]))
}

# Detectar columna de tipo de admisión
if (!"tipo_admision" %in% names(matriculas_raw)) {
  tipo_candidates <- names(matriculas_raw)[str_detect(names(matriculas_raw), "tipo.*admis|admis.*tipo|via.*ingreso")]
  if (length(tipo_candidates) > 0) {
    matriculas_raw <- matriculas_raw %>% dplyr::rename(tipo_admision = all_of(tipo_candidates[1]))
  }
}

# Limpieza final y CLASIFICACIÓN BINARIA (Centralizada vs Directa)
matriculas_clean <- matriculas_raw %>%
  filter(!is.na(rut)) %>%
  mutate(
    rut = as.character(rut),
    rut = str_remove_all(rut, "rt|[\\.\\-\\s]"),
    rut = str_trim(rut),
    carrera = str_trim(str_to_title(as.character(carrera))),
    
    # --- REGLA DE NEGOCIO ACTUALIZADA (BLINDADA) ---
    # Paso 1: Obtener el valor crudo, asegurando que existe la columna
    tipo_admision_raw = if ("tipo_admision" %in% names(.)) as.character(tipo_admision) else NA_character_,
    
    # Paso 2: Clasificación estricta
    tipo_admision = case_when(
      is.na(tipo_admision_raw) ~ "Sin información",
      
      # Si contiene "CENTRALIZADA" -> Centralizada
      str_detect(str_to_upper(tipo_admision_raw), "CENTRALIZADA") ~ "Centralizada",
      
      # IMPORTANTE: Todo lo demás (incluyendo "Otra", "Especial", etc.) se fuerza a "Directa"
      TRUE ~ "Directa"
    )
  ) %>%
  filter(rut != "", !is.na(carrera), carrera != "") %>%
  select(rut, carrera, tipo_admision)

# ------------------------------------------------------------------------------
# 4) FILTRAR: Eliminar respuestas de estudiantes NO matriculados
# ------------------------------------------------------------------------------
message("🧹 Filtrando respuestas según base de matrícula...")

ruts_validos <- matriculas_clean %>%
  distinct(rut, carrera)

n_antes <- nrow(ihea_final)
ihea_final <- ihea_final %>%
  semi_join(ruts_validos, by = c("rut", "carrera"))
n_despues <- nrow(ihea_final)

cat("\n--- RESULTADO FILTRO ---")
cat("\n✅ Respuestas totales (antes):", n_antes)
cat("\n✅ Respuestas de matriculados (después):", n_despues)
cat("\n❌ Eliminadas (NO matriculados):", n_antes - n_despues, "\n")

# ------------------------------------------------------------------------------
# 5) Generar Tablas de Cobertura
# ------------------------------------------------------------------------------

resp_unique <- ihea_final %>% distinct(rut, carrera)
mat_unique <- matriculas_clean %>% distinct(rut, carrera)

# Por Carrera
resp_por_carrera <- resp_unique %>% count(carrera, name = "n_respondieron")
mat_por_carrera <- mat_unique %>% count(carrera, name = "n_matriculados")

cobertura_por_carrera <- mat_por_carrera %>%
  left_join(resp_por_carrera, by = "carrera") %>%
  mutate(
    n_respondieron = replace_na(n_respondieron, 0L),
    cobertura = if_else(n_matriculados > 0, n_respondieron / n_matriculados, NA_real_)
  ) %>%
  arrange(desc(cobertura), desc(n_respondieron))

# ------------------------------------------------------------------------------
# 6) Estadísticas por Tipo de Admisión (Lógica Actualizada)
# ------------------------------------------------------------------------------

# Enriquecer respuestas con tipo de admisión
ihea_con_tipo <- ihea_final %>%
  left_join(
    matriculas_clean %>% select(rut, carrera, tipo_admision),
    by = c("rut", "carrera")
  )

# Resumen general por tipo de admisión
cobertura_tipo_admision <- matriculas_clean %>%
  count(tipo_admision, name = "n_matriculados") %>%
  left_join(
    ihea_con_tipo %>%
      count(tipo_admision, name = "n_respondieron"),
    by = "tipo_admision"
  ) %>%
  mutate(
    n_respondieron = replace_na(n_respondieron, 0L),
    cobertura = if_else(n_matriculados > 0, n_respondieron / n_matriculados, NA_real_),
    pct_matriculados = n_matriculados / sum(n_matriculados),
    pct_respondieron = n_respondieron / sum(n_respondieron)
  ) %>%
  arrange(desc(n_matriculados))

# Resumen por carrera y tipo de admisión
cobertura_carrera_tipo <- matriculas_clean %>%
  count(carrera, tipo_admision, name = "n_matriculados") %>%
  left_join(
    ihea_con_tipo %>%
      count(carrera, tipo_admision, name = "n_respondieron"),
    by = c("carrera", "tipo_admision")
  ) %>%
  mutate(
    n_respondieron = replace_na(n_respondieron, 0L),
    cobertura = if_else(n_matriculados > 0, n_respondieron / n_matriculados, NA_real_)
  ) %>%
  arrange(carrera, desc(n_matriculados))

# ------------------------------------------------------------------------------
# 7) Guardado de Outputs
# ------------------------------------------------------------------------------
message("💾 Guardando archivos...")

# Formato lista para RMarkdown/Quarto
data_completa <- list(
  ihea = ihea_final,
  cobertura = cobertura_por_carrera,
  tipo_admision = cobertura_tipo_admision,
  carrera_tipo = cobertura_carrera_tipo
)

saveRDS(data_completa, here("data_reproduction.rds"))

# Archivos individuales en carpeta outputs
out_dir <- here("outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveRDS(cobertura_por_carrera, here("outputs", "cobertura_por_carrera.rds"))
saveRDS(cobertura_tipo_admision, here("outputs", "cobertura_tipo_admision.rds"))
saveRDS(cobertura_carrera_tipo, here("outputs", "cobertura_carrera_tipo.rds"))
saveRDS(matriculas_clean, here("outputs", "matriculas_clean.rds"))

# ------------------------------------------------------------------------------
# 8) Diagnóstico Final en Consola
# ------------------------------------------------------------------------------
cat("\n" , paste(rep("=", 50), collapse = ""))
cat("\n📊 REPORTE FINAL DE CARGA")
cat("\n" , paste(rep("=", 50), collapse = ""))

cat("\n\n--- DISTRIBUCIÓN POR TIPO DE ADMISIÓN (Nueva Lógica) ---\n")
print(
  cobertura_tipo_admision %>%
    mutate(
      cobertura_pct = scales::percent(cobertura, accuracy = 0.1),
      pct_mat = scales::percent(pct_matriculados, accuracy = 0.1)
    ) %>%
    select(tipo_admision, n_matriculados, pct_mat, n_respondieron, cobertura_pct)
)

# VALIDACIÓN DE SEGURIDAD
if ("Otra" %in% cobertura_tipo_admision$tipo_admision) {
  warning("⚠️  ALERTA: La categoría 'Otra' todavía existe. Revisa la lógica de case_when.")
} else {
  cat("\n✅ Confirmado: La categoría 'Otra' ha sido absorbida por 'Directa' correctamente.")
}

cat("\n\n✅ Proceso finalizado. Ahora puedes ejecutar tu reporte.")
cat("\n📁 Archivo principal: data_reproduction.rds creado/actualizado.\n")