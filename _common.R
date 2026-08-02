# ──────────────────────────────────────────────
# _common.R
# Configuración global y librerías para bookdown
# ──────────────────────────────────────────────

# ── Opciones globales de R ─────────────────────
options(
  width = 100,            # ancho máximo en consola (más cómodo que 80)
  scipen = 999,           # evita notación científica (ej. 1e+05 → 100000)
  knitr.kable.NA = "",    # muestra celdas vacías en tablas en vez de "NA"
  OutDec = ","            # separador decimal español en format()/formatC()/kbl()
)

# ── Opciones globales de knitr ─────────────────
knitr::opts_chunk$set(
  echo      = FALSE,      # oculta el código en los chunks (los scripts de pipeline se enlazan vía gh() al repositorio); cada archivo puede sobrescribirlo
  message   = FALSE,      # oculta mensajes de librerías
  warning   = FALSE,      # oculta advertencias
  fig.align = "center",   # centra todas las figuras
  dpi       = 300,        # resolución (300 ppp, calidad impresión)
  # PDF+PNG para LaTeX (vectorial+raster simultáneo); solo PNG para Word/HTML
  dev       = if (isTRUE(knitr::is_latex_output())) c("pdf", "png") else "png"
)

# ──────────────────────────────────────────────
# Librerías globales más usadas
# ──────────────────────────────────────────────
quiet_library <- function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

quiet_library("tidyverse")   # incluye dplyr, readr, tidyr y ggplot2
quiet_library("sf")          # datos espaciales (shapefiles, GIS)
quiet_library("kableExtra")  # tablas estilizadas en LaTeX/HTML
quiet_library("flextable")  # tablas estilizadas en Word
quiet_library("knitr")       # manejo de tablas y chunks
quiet_library("dplyr")       # manipulación de datos

# Helper de tablas Word (patrón tesis_tabla_word)
source(here::here("R/renderizado/tesis_tabla.R"))

# ──────────────────────────────────────────────
# Función auxiliar: ruta de figura según salida
# ──────────────────────────────────────────────
# Devuelve la ruta con extensión .pdf para LaTeX y .png para Word/HTML.
# Uso: knitr::include_graphics(fig_ext("outputs/figures/app-b/mi_figura"))
fig_ext <- function(base) {
  if (isTRUE(knitr::is_latex_output())) paste0(base, ".pdf") else paste0(base, ".png")
}

# ──────────────────────────────────────────────
# Repositorio público del código y helper de enlaces
# ──────────────────────────────────────────────
# URL base del repositorio reproducible (rama main). Cambiar AQUÍ si se
# renombra el repositorio o la rama: todos los enlaces del documento se
# actualizan automáticamente.
repo_base <- "https://github.com/jaromeroc/tesis_ivec_cv/blob/main"

# Devuelve un enlace Markdown al script del pipeline en GitHub.
# Uso en prosa: `r gh("el pipeline del módulo base", "R/ivec/01_ivec_base.R")`
# Funciona igual en PDF (XeLaTeX) y en Word: knitr sustituye el texto y
# pandoc renderiza el hipervínculo.
gh <- function(text, path) {
  sprintf("[%s](%s/%s)", text, repo_base, path)
}

# ──────────────────────────────────────────────
# Supresión de opciones no soportadas en docx
# ──────────────────────────────────────────────
# fig.align no está soportado en bookdown::word_document2.
# El hook se registra siempre, pero la condición se evalúa en tiempo de chunk
# (cuando knitr ya conoce el formato de salida), no en tiempo de _common.R.
# Así se evita que el registro prematuro nullifique fig.align en compilación PDF.
knitr::opts_hooks$set(fig.align = function(options) {
  if (!isTRUE(knitr::is_latex_output())) {
    options$fig.align <- "default"   # NULL causaría "argumento tiene longitud cero"
  }
  options
})

# ──────────────────────────────────────────────
# Semilla global
# ──────────────────────────────────────────────
set.seed(1972) # reproducibilidad

