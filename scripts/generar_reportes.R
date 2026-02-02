# ==============================================================================
# SCRIPT: generar_reportes.R (HTML + PDF por carrera) — FIX PDF via pagedown
# ==============================================================================
library(tidyverse)
library(quarto)
library(fs)
library(here)

# 1) Cargar datos y lista de carreras (soporta df o lista)
if (!file.exists(here("data_reproduction.rds"))) {
  stop("No existe data_reproduction.rds en la raíz.")
}

dat <- readRDS(here("data_reproduction.rds"))

carreras <- if (is.data.frame(dat)) {
  unique(na.omit(dat$carrera))
} else if (is.list(dat) && "ihea" %in% names(dat)) {
  unique(na.omit(dat$ihea$carrera))
} else {
  stop("data_reproduction.rds tiene un formato inesperado (debe ser data.frame o lista con $ihea).")
}

# 2) Carpetas de salida
out_base <- here("outputs", "reportes_quarto")
out_html <- path(out_base, "html")
out_pdf  <- path(out_base, "pdf")
out_log  <- path(out_base, "logs")
dir_create(out_html, recurse = TRUE)
dir_create(out_pdf,  recurse = TRUE)
dir_create(out_log,  recurse = TRUE)

# 3) Nombre seguro
make_file_safe <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^[:alnum:]]", "_") %>%
    iconv(to = "ASCII//TRANSLIT")
}

# 4) PDF desde HTML usando pagedown
make_pdf_from_html <- function(html_path, pdf_path, log_path) {
  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop("Falta paquete 'pagedown'. Instala con: install.packages('pagedown')")
  }

  # Nota: extra_args ayuda en Linux cuando Chrome corre con restricciones
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

# 5) Render por carrera
render_slides_uah <- function(nom_carrera) {

  file_safe <- make_file_safe(nom_carrera)
  message(paste0("\n🚀 Renderizando: ", nom_carrera))

  input_qmd <- here("template", "template_reporte.qmd")
  template_dir <- here("template")

  html_name <- paste0("reporte_", file_safe, ".html")

  html_in_template <- path(template_dir, html_name)
  html_out <- path(out_html, html_name)

  pdf_out  <- path(out_pdf, paste0("reporte_", file_safe, ".pdf"))
  pdf_log  <- path(out_log, paste0("pdf_", file_safe, ".log"))

  tryCatch({

    # A) HTML (QMD -> revealjs)
    quarto_render(
      input = input_qmd,
      output_format = "revealjs",
      output_file = html_name, # SOLO nombre, sin path
      execute_params = list(carrera_sel = nom_carrera),
      quiet = FALSE
    )

    if (!file_exists(html_in_template)) {
      stop("No se generó el HTML en template/: ", html_in_template)
    }

    file_move(html_in_template, html_out)
    message("✅ HTML guardado: ", html_out)

    # B) PDF (HTML -> PDF) sin Quarto/Pandoc
    ok_pdf <- make_pdf_from_html(html_out, pdf_out, pdf_log)

    if (ok_pdf && file_exists(pdf_out)) {
      message("✅ PDF guardado:  ", pdf_out)
    } else {
      warning(
        "⚠️ No se generó el PDF para: ", nom_carrera, "\n",
        "Revisa log: ", pdf_log, "\n",
        "Tip: asegúrate de tener Chromium/Chrome. Prueba: quarto install chromium"
      )
    }

  }, error = function(e) {
    message("❌ Error crítico en ", nom_carrera, ": ", e$message)
  })
}

# 6) Ejecutar
N_TEST <- Inf  # prueba; usa Inf para todas
walk(head(carreras, N_TEST), render_slides_uah)
