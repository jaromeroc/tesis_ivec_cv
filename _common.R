# ============================================================
# _common.R — Núcleo operativo IVEC TESIS OS++ v9
# ============================================================

# --- 1. Opciones generales de entorno -----------------------------------------
options(
  width = 80,                           # ancho legible en consola
  encoding = "UTF-8",                   # evita errores de acentos
  scipen = 999,                         # no usar notación científica
  digits = 4,                           # precisión estándar
  stringsAsFactors = FALSE
)

# --- 2. Opciones globales de knitr -------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  echo = TRUE,                          # muestra el código
  message = FALSE,                      # oculta mensajes
  warning = FALSE,                      # oculta advertencias
  fig.align = "center",
  fig.path = "figures/",
  cache = TRUE,                         # activa caché de compilación
  fig.width = 7, fig.height = 5,
  dpi = 300
)

# --- 3. Directorios estándar IVEC --------------------------------------------
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("evaluaciones", showWarnings = FALSE)

# --- 4. Registro de auditoría -------------------------------------------------
log_file <- "evaluaciones/registro_IVEC.log"
cat(paste("Compilación iniciada:", Sys.time(), "\n"), file = log_file, append = TRUE)

# --- 5. Carga de librerías básicas -------------------------------------------
required_pkgs <- c("bookdown", "knitr", "RefManageR", "DiagrammeR", 
                   "dplyr", "readr", "stringr", "ggplot2")
invisible(lapply(required_pkgs, require, character.only = TRUE))

# --- 6. Funciones de control IVEC --------------------------------------------
# Verificación de fórmulas IVEC (fragilidad, capacidad, etc.)
verify_IVEC <- function(Vi, CH, VC, CS, VE, CAD) {
  M_micro <- Vi * (1 - CH)
  M_context <- VC * (1 - CS)
  M_estruct <- VE * (1 - CAD)
  IVEC <- (M_micro * M_context * M_estruct)^(1/3)
  return(IVEC)
}

# Validación de referencias bibliográficas
check_refs <- function() {
  if (file.exists("references.bib")) {
    bib <- RefManageR::ReadBib("references.bib")
    issues <- RefManageR::CheckBibliography(bib)
    cat("Verificación bibliográfica completada.\n", file = log_file, append = TRUE)
    return(issues)
  } else {
    warning("No se encontró el archivo references.bib")
  }
}

# --- 7. Metadatos del sistema -------------------------------------------------
cat(paste("Versión IVEC TESIS OS++ v9 - Sesión:", Sys.Date(), "\n"), 
    file = log_file, append = TRUE)

# --- 8. Entorno reproducible --------------------------------------------------
set.seed(1234) # para resultados replicables

