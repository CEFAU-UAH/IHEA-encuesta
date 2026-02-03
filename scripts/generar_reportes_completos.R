# ==============================================================================
# SCRIPT: generar_reportes_completos.R
# Genera reporte general + reportes por carrera (HTML + PDF)
# ==============================================================================
library(tidyverse)
library(quarto)
library(fs)
library(here)

# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================

# Nombre seguro de archivo
make_file_safe <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:]]", "_") %>%
    iconv(to = "ASCII//TRANSLIT")
}

# PDF desde HTML usando pagedown
make_pdf_from_html <- function(html_path, pdf_path, log_path) {
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop("Falta paquete 'pagedown'. Instala con: install.packages('pagedown')")
  }
  
  res <- tryCatch({
    pagedown::chrome_print(
      input = html_path,
      output = pdf_path,
      extra_args = c("--no-sandbox", "--disable-dev-shm-usage")
    )
    TRUE
  }, error = function(e) {
    writeLines(paste("ERROR pagedown::chrome_print:", e$message), con = log_path)
    FALSE
  })
  
  res
}

# ==============================================================================
# CONFIGURACIÓN INICIAL
# ==============================================================================

# 1) Verificar existencia de datos
if (!file.exists(here("data_reproduction.rds"))) {
  stop("❌ No existe data_reproduction.rds en la raíz del proyecto.")
}

# 2) Cargar datos y obtener carreras
dat <- readRDS(here("data_reproduction.rds"))

carreras <- if (is.data.frame(dat)) {
  unique(na.omit(dat$carrera))
} else if (is.list(dat) && "ihea" %in% names(dat)) {
  unique(na.omit(dat$ihea$carrera))
} else {
  stop("❌ data_reproduction.rds tiene un formato inesperado (debe ser data.frame o lista con $ihea).")
}

message(paste0("📊 Carreras detectadas: ", length(carreras)))
message(paste0("📋 ", paste(carreras, collapse = ", ")))

# 3) Crear estructura de carpetas
out_base <- here("outputs", "reportes_quarto")
out_html <- path(out_base, "html")
out_pdf  <- path(out_base, "pdf")
out_log  <- path(out_base, "logs")

dir_create(out_html, recurse = TRUE)
dir_create(out_pdf,  recurse = TRUE)
dir_create(out_log,  recurse = TRUE)

message("✅ Carpetas de salida creadas/verificadas")

# ==============================================================================
# FUNCIÓN: RENDERIZAR REPORTE GENERAL
# ==============================================================================

render_reporte_general <- function() {
  
  message("\n" , paste(rep("=", 70), collapse = ""))
  message("🌐 GENERANDO REPORTE GENERAL")
  message(paste(rep("=", 70), collapse = ""))
  
  input_qmd <- here("template", "template_reporte_general.qmd")
  template_dir <- here("template")
  
  if (!file.exists(input_qmd)) {
    stop("❌ No se encuentra template_reporte_general.qmd en /template")
  }
  
  html_name <- "reporte_general.html"
  html_in_template <- path(template_dir, html_name)
  html_out <- path(out_html, html_name)
  pdf_out  <- path(out_pdf, "reporte_general.pdf")
  pdf_log  <- path(out_log, "pdf_general.log")
  
  tryCatch({
    
    # A) Generar HTML
    message("🔄 Renderizando HTML...")
    quarto_render(
      input = input_qmd,
      output_format = "revealjs",
      output_file = html_name,
      quiet = FALSE
    )
    
    if (!file_exists(html_in_template)) {
      stop("❌ No se generó el HTML en template/")
    }
    
    file_move(html_in_template, html_out)
    message("✅ HTML guardado: ", html_out)
    
    # B) Generar PDF
    message("🔄 Convirtiendo a PDF...")
    ok_pdf <- make_pdf_from_html(html_out, pdf_out, pdf_log)
    
    if (ok_pdf && file_exists(pdf_out)) {
      message("✅ PDF guardado:  ", pdf_out)
    } else {
      warning(
        "⚠️  No se generó el PDF del reporte general\n",
        "    Revisa log: ", pdf_log, "\n",
        "    Tip: asegúrate de tener Chromium/Chrome. Prueba: quarto install chromium"
      )
    }
    
    message("✅ Reporte general completado\n")
    
  }, error = function(e) {
    message("❌ Error crítico en reporte general: ", e$message)
  })
}

# ==============================================================================
# FUNCIÓN: RENDERIZAR REPORTE POR CARRERA
# ==============================================================================

render_reporte_carrera <- function(nom_carrera) {
  
  file_safe <- make_file_safe(nom_carrera)
  message("\n🚀 Renderizando: ", nom_carrera)
  
  input_qmd <- here("template", "template_reporte.qmd")
  template_dir <- here("template")
  
  html_name <- paste0("reporte_", file_safe, ".html")
  html_in_template <- path(template_dir, html_name)
  html_out <- path(out_html, html_name)
  pdf_out  <- path(out_pdf, paste0("reporte_", file_safe, ".pdf"))
  pdf_log  <- path(out_log, paste0("pdf_", file_safe, ".log"))
  
  tryCatch({
    
    # A) Generar HTML
    quarto_render(
      input = input_qmd,
      output_format = "revealjs",
      output_file = html_name,
      execute_params = list(carrera_sel = nom_carrera),
      quiet = FALSE
    )
    
    if (!file_exists(html_in_template)) {
      stop("No se generó el HTML en template/: ", html_in_template)
    }
    
    file_move(html_in_template, html_out)
    message("   ✅ HTML: ", basename(html_out))
    
    # B) Generar PDF
    ok_pdf <- make_pdf_from_html(html_out, pdf_out, pdf_log)
    
    if (ok_pdf && file_exists(pdf_out)) {
      message("   ✅ PDF:  ", basename(pdf_out))
    } else {
      warning("   ⚠️  PDF no generado (ver log: ", basename(pdf_log), ")")
    }
    
  }, error = function(e) {
    message("   ❌ Error en ", nom_carrera, ": ", e$message)
  })
}

# ==============================================================================
# EJECUCIÓN PRINCIPAL
# ==============================================================================

message("\n" , paste(rep("█", 70), collapse = ""))
message("📄 SISTEMA DE GENERACIÓN DE REPORTES IHEA 2026")
message(paste(rep("█", 70), collapse = ""))

# Paso 1: Reporte General
render_reporte_general()

# Paso 2: Reportes por Carrera
message("\n" , paste(rep("=", 70), collapse = ""))
message("📚 GENERANDO REPORTES POR CARRERA")
message(paste(rep("=", 70), collapse = ""))

N_TEST <- Inf  # Cambiar a un número pequeño (ej: 3) para pruebas

carreras_a_procesar <- head(carreras, N_TEST)
message(paste0("📝 Se procesarán ", length(carreras_a_procesar), " carreras\n"))

walk(carreras_a_procesar, render_reporte_carrera)

# Resumen final
message("\n" , paste(rep("█", 70), collapse = ""))
message("✅ PROCESO COMPLETADO")
message(paste(rep("█", 70), collapse = ""))
message("📂 Reportes disponibles en:")
message("   HTML: ", out_html)
message("   PDF:  ", out_pdf)
message("   Logs: ", out_log)
message(paste(rep("█", 70), collapse = ""))
