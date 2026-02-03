# ==============================================================================
# SCRIPT: generar_reporte_general.R
# Genera únicamente el reporte general IHEA 2026 (HTML + PDF)
# ==============================================================================
library(tidyverse)
library(quarto)
library(fs)
library(here)

# ==============================================================================
# FUNCIONES AUXILIARES
# ==============================================================================

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

message("\n" , paste(rep("█", 70), collapse = ""))
message("📄 GENERACIÓN DE REPORTE GENERAL IHEA 2026")
message(paste(rep("█", 70), collapse = ""))

# Verificar existencia de datos
if (!file.exists(here("data_reproduction.rds"))) {
  stop("❌ No existe data_reproduction.rds en la raíz del proyecto.")
}

# Verificar template
input_qmd <- here("template", "template_reporte_general.qmd")
if (!file.exists(input_qmd)) {
  stop("❌ No se encuentra template_reporte_general.qmd en /template")
}

# Crear carpetas de salida
out_base <- here("outputs", "reportes_quarto")
out_html <- path(out_base, "html")
out_pdf  <- path(out_base, "pdf")
out_log  <- path(out_base, "logs")

dir_create(out_html, recurse = TRUE)
dir_create(out_pdf,  recurse = TRUE)
dir_create(out_log,  recurse = TRUE)

message("✅ Carpetas de salida verificadas")

# ==============================================================================
# GENERACIÓN DEL REPORTE
# ==============================================================================

template_dir <- here("template")
html_name <- "reporte_general.html"
html_in_template <- path(template_dir, html_name)
html_out <- path(out_html, html_name)
pdf_out  <- path(out_pdf, "reporte_general.pdf")
pdf_log  <- path(out_log, "pdf_general.log")

message("\n🔄 Iniciando renderizado...")

tryCatch({
  
  # A) Generar HTML
  message("   📊 Generando presentación HTML...")
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
  message("   ✅ HTML generado: ", html_out)
  
  # B) Generar PDF
  message("\n   📄 Convirtiendo a PDF...")
  ok_pdf <- make_pdf_from_html(html_out, pdf_out, pdf_log)
  
  if (ok_pdf && file_exists(pdf_out)) {
    message("   ✅ PDF generado:  ", pdf_out)
  } else {
    warning(
      "\n   ⚠️  No se pudo generar el PDF\n",
      "       Revisa el log: ", pdf_log, "\n",
      "       Tip: asegúrate de tener Chrome/Chromium instalado\n",
      "       Ejecuta: quarto install chromium"
    )
  }
  
  # Resumen final
  message("\n" , paste(rep("█", 70), collapse = ""))
  message("✅ PROCESO COMPLETADO")
  message(paste(rep("█", 70), collapse = ""))
  message("📂 Archivos generados:")
  message("   HTML: ", html_out)
  if (file_exists(pdf_out)) {
    message("   PDF:  ", pdf_out)
  }
  message(paste(rep("█", 70), collapse = ""))
  
}, error = function(e) {
  message("\n❌ Error crítico: ", e$message)
  message("💡 Verifica:")
  message("   - Que exista data_reproduction.rds")
  message("   - Que exista outputs/cobertura_por_carrera.rds")
  message("   - Que template_reporte_general.qmd esté en /template")
  message("   - Que tengas instalados los paquetes necesarios")
})
