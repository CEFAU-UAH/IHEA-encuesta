# ==============================================================================
# SCRIPT: generar_reportes.R (VERSIÓN BLINDADA)
# ==============================================================================
library(tidyverse)
library(quarto)
library(fs)
library(here)

# 1. Cargar datos y limpiar lista de carreras
if(!file.exists(here("data_reproduction.rds"))) stop("No existe data_reproduction.rds en la raíz.")

ihea_final <- readRDS(here("data_reproduction.rds"))
carreras <- unique(na.omit(ihea_final$carrera))

# 2. Carpeta de salida
out_dir <- here("outputs", "reportes_quarto")
if (!dir_exists(out_dir)) dir_create(out_dir, recurse = TRUE)

# 3. Función de renderizado
render_slides_uah <- function(nom_carrera) {
  
  file_safe <- nom_carrera %>% 
    str_to_lower() %>% 
    str_replace_all("[^[:alnum:]]", "_") %>% 
    iconv(to = "ASCII//TRANSLIT")
  
  message(paste("🚀 Renderizando:", nom_carrera))
  
  # Ejecutamos Quarto DESDE la carpeta template para que encuentre el logo/scss
  tryCatch({
    quarto_render(
      input = here("template", "template_reporte.qmd"),
      output_file = paste0("reporte_", file_safe, ".html"),
      execute_params = list(carrera_sel = nom_carrera),
      quiet = FALSE # Mantenemos FALSE para ver errores en consola
    )
    
    # Mover el archivo generado al output
    path_gen <- here("template", paste0("reporte_", file_safe, ".html"))
    path_dest <- path(out_dir, paste0("reporte_", file_safe, ".html"))
    
    if (file_exists(path_gen)) {
      file_move(path_gen, path_dest)
      message("✅ Guardado en outputs/")
    }
    
  }, error = function(e) {
    message(paste("❌ Error crítico en", nom_carrera, ":", e$message))
  })
}

# 4. Ejecución (Prueba primero con 2 carreras para verificar)
walk(carreras[1:2], render_slides_uah) 

# Si las primeras 2 funcionan, descomenta la siguiente línea y comenta la anterior:
# walk(carreras, render_slides_uah)