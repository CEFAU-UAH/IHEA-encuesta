# ==============================================================================
# SCRIPT: 00_data_load.R (Versión Final - Con Tipo de Admisión)
# Objetivo:
#  (1) Cargar respuestas IHEA (DIURNO PRESENCIAL) y limpiar/estandarizar
#  (2) Expandir matrices (P12, P15, P16, P18)
#  (3) Cargar MATRÍCULAS (MATRÍCULAS AD DIRECTA Y CENTR) con tipo de admisión
#  (4) FILTRAR: Eliminar respuestas de estudiantes NO matriculados
#  (5) Calcular cobertura por carrera: % respondieron / matriculados
#  (6) Calcular estadísticas por tipo de admisión (Centralizada vs Directa)
#  (7) Guardar:
#       - data_reproduction.rds              (lista con ihea + cobertura + tipo_admision)
#       - outputs/cobertura_por_carrera.rds  (tabla cobertura por carrera)
#       - outputs/cobertura_tipo_admision.rds (estadísticas por tipo admisión)
#       - outputs/matriculas_clean.rds       (matrículas limpias)
# ==============================================================================

library(tidyverse)
library(readxl)
library(janitor)
library(here)

# ------------------------------------------------------------------------------
# 0) Parámetros
# ------------------------------------------------------------------------------
archivo_path <- here("data", "BD respuestas encuesta IHEA 2026.xlsx")
sheet_respuestas <- "encuesta IHE"
sheet_matriculas <- "MATRIC CENTRA Y DIRECTA"

if (!file.exists(archivo_path)) {
  stop("No existe el archivo Excel en: ", archivo_path)
}

# ------------------------------------------------------------------------------
# 1) Cargar Respuestas (todo como texto para evitar problemas con RUT)
# ------------------------------------------------------------------------------
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
ihea_final <- ihea_raw %>%
  rename(carrera_raw = any_of(c("carrera_s", "carrera", "nombre_carrera", "x_carrera"))) %>%
  filter(!is.na(rut)) %>%
  mutate(
    rut = as.character(rut),
    rut = str_remove_all(rut, "rt|[\\.\\-\\s]"),
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
# 3) Cargar Matrículas con Tipo de Admisión
# ------------------------------------------------------------------------------

matriculas_raw0 <- read_excel(
  archivo_path,
  sheet = sheet_matriculas,
  col_names = FALSE,
  col_types = "text"
) %>%
  janitor::clean_names()

# Buscar la fila donde aparece "RUT"
header_row <- which(apply(matriculas_raw0, 1, function(r) any(stringr::str_to_upper(r) == "RUT", na.rm = TRUE)))[1]

if (is.na(header_row)) {
  stop("No encontré ninguna fila con 'RUT' en la hoja de matrículas.")
}

# Extraer nombres de columnas
new_names <- matriculas_raw0 %>%
  slice(header_row) %>%
  unlist(use.names = FALSE) %>%
  as.character() %>%
  stringr::str_squish()

# Armar tabla real
matriculas_raw <- matriculas_raw0 %>%
  slice((header_row + 1):n()) %>%
  setNames(new_names) %>%
  janitor::clean_names()

# Verificación
if (!"rut" %in% names(matriculas_raw)) {
  stop("Después de detectar header, igual no existe columna 'rut'.")
}

# Detectar columna de carrera
if (!"carrera" %in% names(matriculas_raw)) {
  car_candidates <- names(matriculas_raw)[stringr::str_detect(names(matriculas_raw), "carrera|programa|plan|nombre")]
  if (length(car_candidates) == 0) stop("No encontré columna de carrera en matrículas.")
  matriculas_raw <- matriculas_raw %>% dplyr::rename(carrera = all_of(car_candidates[1]))
}

# Detectar columna de tipo de admisión
if (!"tipo_admision" %in% names(matriculas_raw)) {
  tipo_candidates <- names(matriculas_raw)[stringr::str_detect(names(matriculas_raw), "tipo.*admis|admis.*tipo|via.*ingreso")]
  if (length(tipo_candidates) > 0) {
    matriculas_raw <- matriculas_raw %>% dplyr::rename(tipo_admision = all_of(tipo_candidates[1]))
  }
}

# Limpieza final incluyendo tipo de admisión
matriculas_clean <- matriculas_raw %>%
  filter(!is.na(rut)) %>%
  mutate(
    rut = as.character(rut),
    rut = stringr::str_remove_all(rut, "rt|[\\.\\-\\s]"),
    rut = stringr::str_trim(rut),
    carrera = stringr::str_trim(stringr::str_to_title(as.character(carrera))),
    
    # Procesar tipo de admisión (clasificar en Centralizada vs Directa)
    tipo_admision_raw = if ("tipo_admision" %in% names(.)) as.character(tipo_admision) else NA_character_,
    tipo_admision = case_when(
      is.na(tipo_admision_raw) ~ "Sin información",
      stringr::str_detect(stringr::str_to_upper(tipo_admision_raw), "CENTRALIZADA") ~ "Centralizada",
      stringr::str_detect(stringr::str_to_upper(tipo_admision_raw), "DIRECTA|ESPECIAL|CONDUCENCIA|HABILITACION|COMPLEMENTARIA") ~ "Directa",
      TRUE ~ "Otra"
    )
  ) %>%
  filter(rut != "", !is.na(carrera), carrera != "") %>%
  select(rut, carrera, tipo_admision)

# ------------------------------------------------------------------------------
# 4) FILTRAR: Eliminar respuestas de estudiantes NO matriculados
# ------------------------------------------------------------------------------

ruts_validos <- matriculas_clean %>%
  distinct(rut, carrera)

n_antes <- nrow(ihea_final)
ihea_final <- ihea_final %>%
  semi_join(ruts_validos, by = c("rut", "carrera"))
n_despues <- nrow(ihea_final)

cat("\n--- FILTRADO DE RESPUESTAS ---")
cat("\n✅ Respuestas totales (antes):", n_antes)
cat("\n✅ Respuestas de matriculados (después):", n_despues)
cat("\n❌ Respuestas eliminadas (NO matriculados):", n_antes - n_despues, "\n")

# ------------------------------------------------------------------------------
# 5) Cobertura por carrera
# ------------------------------------------------------------------------------

resp_unique <- ihea_final %>% distinct(rut, carrera)
mat_unique <- matriculas_clean %>% distinct(rut, carrera)

resp_por_carrera <- resp_unique %>%
  count(carrera, name = "n_respondieron")

mat_por_carrera <- mat_unique %>%
  count(carrera, name = "n_matriculados")

cobertura_por_carrera <- mat_por_carrera %>%
  left_join(resp_por_carrera, by = "carrera") %>%
  mutate(
    n_respondieron = replace_na(n_respondieron, 0L),
    cobertura = if_else(n_matriculados > 0, n_respondieron / n_matriculados, NA_real_)
  ) %>%
  arrange(desc(cobertura), desc(n_respondieron))

# ------------------------------------------------------------------------------
# 6) Estadísticas por Tipo de Admisión
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
# 7) Guardados (FORMATO LISTA para compatibilidad con templates)
# ------------------------------------------------------------------------------

# Guardar en formato lista (nuevo formato recomendado)
data_completa <- list(
  ihea = ihea_final,
  cobertura = cobertura_por_carrera,
  tipo_admision = cobertura_tipo_admision,
  carrera_tipo = cobertura_carrera_tipo
)

saveRDS(data_completa, here("data_reproduction.rds"))

# Guardar outputs auxiliares
out_dir <- here("outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveRDS(cobertura_por_carrera, here("outputs", "cobertura_por_carrera.rds"))
saveRDS(cobertura_tipo_admision, here("outputs", "cobertura_tipo_admision.rds"))
saveRDS(cobertura_carrera_tipo, here("outputs", "cobertura_carrera_tipo.rds"))
saveRDS(matriculas_clean, here("outputs", "matriculas_clean.rds"))

# ------------------------------------------------------------------------------
# 8) Diagnóstico
# ------------------------------------------------------------------------------
cat("\n--- REPORTE DE CALIDAD (RESPUESTAS - SOLO MATRICULADOS) ---")
cat("\n✅ Registros totales (respuestas válidas):", nrow(ihea_final))
cat("\n🎓 Carreras detectadas (respuestas):", n_distinct(ihea_final$carrera), "\n")
print(sort(table(ihea_final$carrera), decreasing = TRUE))

cat("\n--- REPORTE DE CALIDAD (MATRÍCULAS) ---")
cat("\n✅ Registros totales (matrículas):", nrow(matriculas_clean))
cat("\n🎓 Carreras detectadas (matrículas):", n_distinct(matriculas_clean$carrera), "\n")
print(sort(table(matriculas_clean$carrera), decreasing = TRUE))

cat("\n--- COBERTURA POR TIPO DE ADMISIÓN ---\n")
print(
  cobertura_tipo_admision %>%
    mutate(
      cobertura_pct = scales::percent(cobertura, accuracy = 0.1),
      pct_mat = scales::percent(pct_matriculados, accuracy = 0.1),
      pct_resp = scales::percent(pct_respondieron, accuracy = 0.1)
    ) %>%
    select(tipo_admision, n_matriculados, pct_mat, n_respondieron, pct_resp, cobertura_pct)
)

cat("\n--- COBERTURA (TOP 15 por % cobertura) ---\n")
print(
  cobertura_por_carrera %>%
    mutate(cobertura_pct = scales::percent(cobertura, accuracy = 0.1)) %>%
    select(carrera, n_matriculados, n_respondieron, cobertura_pct) %>%
    slice_head(n = 15)
)

cat("\n--- ARCHIVOS GENERADOS ---")
cat("\n📦 data_reproduction.rds (lista con ihea + cobertura + tipo_admision)")
cat("\n📦 outputs/cobertura_por_carrera.rds")
cat("\n📦 outputs/cobertura_tipo_admision.rds")
cat("\n📦 outputs/cobertura_carrera_tipo.rds")
cat("\n📦 outputs/matriculas_clean.rds\n")

glimpse(ihea_final)