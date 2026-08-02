# ─────────────────────────────────────────────────────────────────────────────
# Renderizar_Word.R
# Compila la tesis en formato Word (.docx) mediante bookdown.
#
# Diferencias respecto a Renderizar.R (PDF/LaTeX):
#   • Usa bookdown::word_document2 en lugar de pdf_book
#   • Los bloques {=latex} son ignorados silenciosamente por pandoc → correcto
#   • Las figuras se generan en PNG (controlado condicionalmente en _common.R)
#   • La plantilla de estilos es plantilla.docx (generada con pandoc por defecto)
#
# Uso desde RStudio:   Source o Ctrl+Shift+Enter
#      desde terminal: Rscript R/renderizado/Renderizar_Word.R
#
# El script se ancla a la raíz del proyecto mediante here::here(), de modo que
# `index.Rmd` se resuelve correctamente independientemente del directorio de
# trabajo desde el que se invoque el script.
# ─────────────────────────────────────────────────────────────────────────────

library(here)

# Anclar la raíz del proyecto como directorio de trabajo
old_wd <- getwd()
setwd(here::here())
on.exit(setwd(old_wd))

# 1. Limpiar compilaciones anteriores (opcional pero recomendado)
bookdown::clean_book(TRUE)

# 2. Compilar a Word
bookdown::render_book(
  input       = "index.Rmd",
  output_format = "bookdown::word_document2",
  clean       = TRUE
)

# 3. Mensaje de éxito con ruta al archivo
output_file <- file.path("_book", "Tesis_Vulnerabilidad.docx")
if (file.exists(output_file)) {
  message("\n✓ Compilación completada: ", normalizePath(output_file))
} else {
  warning("No se encontró el archivo de salida esperado en _book/")
}
