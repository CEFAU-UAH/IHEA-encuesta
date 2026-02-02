# ==============================================================================
# SCRIPT: 00_data_load.R (Versión Final - Solo Estudiantes Matriculados)
# Objetivo:
#  (1) Cargar respuestas IHEA (DIURNO PRESENCIAL) y limpiar/estandarizar
#  (2) Expandir matrices (P12, P15, P16, P18)
#  (3) Cargar MATRÍCULAS (MATRÍCULAS AD DIRECTA Y CENTR)
#  (4) FILTRAR: Eliminar respuestas de estudiantes NO matriculados
#  (5) Calcular cobertura por carrera: % respondieron / matriculados
#  (6) Guardar:
#       - data_reproduction.rds              (ihea_final - solo estudiantes matriculados)
#       - outputs/cobertura_por_carrera.rds  (tabla cobertura por carrera)
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
# CORRECCIÓN: skip = 0 porque los headers están en la primera fila
ihea_raw <- read_excel(
  archivo_path,
  sheet = sheet_respuestas,
  skip = 0,  # Los headers están en la fila 0: 'Rut', 'Carrera(s)', etc.
  col_types = "text"
) %>%
  clean_names()

# ------------------------------------------------------------------------------
# 2) Procesamiento Respuestas
# ------------------------------------------------------------------------------
ihea_final <- ihea_raw %>%
  # Consolidar nombre de carrera aunque venga con distintos nombres
  rename(carrera_raw = any_of(c("carrera_s", "carrera", "nombre_carrera", "x_carrera"))) %>%
  filter(!is.na(rut)) %>%
  mutate(
    rut = as.character(rut),
    rut = str_remove_all(rut, "rt|[\\.\\-\\s]"),
    rut = str_trim(rut),

    carrera_raw = as.character(carrera_raw),
    carrera_raw = str_squish(carrera_raw),

    # Estandarización "bonita" (si tu institución usa un formato distinto, cámbialo aquí)
    carrera = str_trim(str_to_title(carrera_raw)),
    modalidad = "Diurno Presencial"
  ) %>%
  filter(!is.na(rut), rut != "") %>%
  filter(!is.na(carrera), carrera != "") %>%

  # --- EXPANSIÓN DE MATRICES (P12, P15, P16, P18) ---
  # Nota: fill="right" evita fallos si vienen menos items en alguna fila
  separate(p_12, into = paste0("p_12_", letters[1:14]), sep = ",", fill = "right", remove = TRUE) %>%
  separate(p_15, into = paste0("p_15_", letters[1:8]),  sep = ",", fill = "right", remove = TRUE) %>%
  separate(p_16, into = paste0("p_16_", letters[1:5]),  sep = ",", fill = "right", remove = TRUE) %>%
  separate(p_18, into = paste0("p_18_", letters[1:4]),  sep = ",", fill = "right", remove = TRUE) %>%

  # --- Selección final flexible ---
  select(
    rut,
    any_of(c("nombres", "nombre", "nombre_completo")),
    contains("email"),
    carrera,
    modalidad,
    starts_with("p_")
  )

# ------------------------------------------------------------------------------
# 3) Cargar Matrículas (BLINDADO: detecta header "RUT" aunque haya celdas raras)
# ------------------------------------------------------------------------------

matriculas_raw0 <- read_excel(
  archivo_path,
  sheet = sheet_matriculas,
  col_names = FALSE,     # 👈 clave: no asumir headers
  col_types = "text"
) %>%
  janitor::clean_names()

# Buscar la fila donde aparece "RUT" en cualquier columna (case-insensitive)
header_row <- which(apply(matriculas_raw0, 1, function(r) any(stringr::str_to_upper(r) == "RUT", na.rm = TRUE)))[1]

if (is.na(header_row)) {
  stop("No encontré ninguna fila con 'RUT' en la hoja de matrículas.")
}

# Extraer nombres de columnas desde esa fila
new_names <- matriculas_raw0 %>%
  slice(header_row) %>%
  unlist(use.names = FALSE) %>%
  as.character() %>%
  stringr::str_squish()

# Armar tabla real desde la fila siguiente
matriculas_raw <- matriculas_raw0 %>%
  slice((header_row + 1):n()) %>%
  setNames(new_names) %>%
  janitor::clean_names()

# Verificación
if (!"rut" %in% names(matriculas_raw)) {
  stop("Después de detectar header, igual no existe columna 'rut'. Revisa si el header dice 'Rut' o 'R.U.T.' etc.")
}

# Si la columna de carrera no se llama exactamente "carrera",
# intenta detectar una candidata razonable:
if (!"carrera" %in% names(matriculas_raw)) {
  car_candidates <- names(matriculas_raw)[stringr::str_detect(names(matriculas_raw), "carrera|programa|plan|nombre")]
  if (length(car_candidates) == 0) stop("No encontré columna de carrera en matrículas.")
  matriculas_raw <- matriculas_raw %>% dplyr::rename(carrera = all_of(car_candidates[1]))
}

# Limpieza final de rut y carrera (match con tus respuestas)
matriculas_clean <- matriculas_raw %>%
  filter(!is.na(rut)) %>%
  mutate(
    rut = as.character(rut),
    rut = stringr::str_remove_all(rut, "rt|[\\.\\-\\s]"),
    rut = stringr::str_trim(rut),
    carrera = stringr::str_trim(stringr::str_to_title(as.character(carrera)))
  ) %>%
  filter(rut != "", !is.na(carrera), carrera != "") %>%
  distinct(rut, carrera)



# ------------------------------------------------------------------------------
# 4) FILTRAR: Eliminar respuestas de estudiantes NO matriculados
# ------------------------------------------------------------------------------

# Identificar RUTs+carreras válidas (que están en matrículas)
ruts_validos <- matriculas_clean %>%
  distinct(rut, carrera)

# Filtrar ihea_final para mantener SOLO estudiantes matriculados
n_antes <- nrow(ihea_final)
ihea_final <- ihea_final %>%
  semi_join(ruts_validos, by = c("rut", "carrera"))
n_despues <- nrow(ihea_final)

cat("\n--- FILTRADO DE RESPUESTAS ---")
cat("\n✅ Respuestas totales (antes):", n_antes)
cat("\n✅ Respuestas de matriculados (después):", n_despues)
cat("\n❌ Respuestas eliminadas (NO matriculados):", n_antes - n_despues, "\n")

# ------------------------------------------------------------------------------
# 5) Cobertura por carrera (ahora solo estudiantes matriculados)
# ------------------------------------------------------------------------------

# Preparar datos únicos
resp_unique <- ihea_final %>% distinct(rut, carrera)
mat_unique <- matriculas_clean %>% distinct(rut, carrera)

# Contar respuestas por carrera (todas son válidas ahora)
resp_por_carrera <- resp_unique %>%
  count(carrera, name = "n_respondieron")

# Contar matriculados por carrera
mat_por_carrera <- mat_unique %>%
  count(carrera, name = "n_matriculados")

# Calcular cobertura
cobertura_por_carrera <- mat_por_carrera %>%
  left_join(resp_por_carrera, by = "carrera") %>%
  mutate(
    n_respondieron = replace_na(n_respondieron, 0L),
    cobertura = if_else(n_matriculados > 0, n_respondieron / n_matriculados, NA_real_)
  ) %>%
  arrange(desc(cobertura), desc(n_respondieron))

# ------------------------------------------------------------------------------
# 6) Guardados
# ------------------------------------------------------------------------------
# (A) Mantener compatibilidad con tus QMD actuales
saveRDS(ihea_final, here("data_reproduction.rds"))

# (B) Guardar outputs auxiliares
out_dir <- here("outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

saveRDS(cobertura_por_carrera, here("outputs", "cobertura_por_carrera.rds"))
saveRDS(matriculas_clean, here("outputs", "matriculas_clean.rds"))

# ------------------------------------------------------------------------------
# 7) Diagnóstico rápido
# ------------------------------------------------------------------------------
cat("\n--- REPORTE DE CALIDAD (RESPUESTAS - SOLO MATRICULADOS) ---")
cat("\n✅ Registros totales (respuestas válidas):", nrow(ihea_final))
cat("\n🎓 Carreras detectadas (respuestas):", n_distinct(ihea_final$carrera), "\n")
print(sort(table(ihea_final$carrera), decreasing = TRUE))

cat("\n--- REPORTE DE CALIDAD (MATRÍCULAS) ---")
cat("\n✅ Registros totales (matrículas):", nrow(matriculas_clean))
cat("\n🎓 Carreras detectadas (matrículas):", n_distinct(matriculas_clean$carrera), "\n")
print(sort(table(matriculas_clean$carrera), decreasing = TRUE))

cat("\n--- COBERTURA (TOP 15 por % cobertura) ---\n")
print(
  cobertura_por_carrera %>%
    mutate(cobertura_pct = scales::percent(cobertura, accuracy = 0.1)) %>%
    select(carrera, n_matriculados, n_respondieron, cobertura_pct) %>%
    slice_head(n = 15)
)

cat("\n--- ARCHIVOS GENERADOS ---")
cat("\n📦 data_reproduction.rds (ihea_final - solo matriculados)")
cat("\n📦 outputs/cobertura_por_carrera.rds")
cat("\n📦 outputs/matriculas_clean.rds\n")