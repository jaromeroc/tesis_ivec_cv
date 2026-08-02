# ─────────────────────────────────────────────────────────────────────────────
# Renderizar.R
# Compila la tesis en formato PDF (XeLaTeX) y Word (.docx) mediante bookdown.
#
# Uso desde RStudio:   Source o Ctrl+Shift+Enter
#      desde consola:  source("R/renderizado/Renderizar.R")
#      desde terminal: Rscript R/renderizado/Renderizar.R
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

# Renderiza el libro en PDF
bookdown::clean_book(TRUE)
bookdown::render_book("index.Rmd", "bookdown::pdf_book")


# Renderiza el libro en Word
bookdown::render_book("index.Rmd", "bookdown::word_document2")


# ─────────────────────────────────────────────────────────────────────────────
# Versión reducida (síntesis divulgativa, version-reducida/)
# Compila SOLO el subproyecto version-reducida/ en PDF (XeLaTeX), heredando el
# formato de la tesis completa vía ../preamble.tex referenciado en su index.Rmd.
#
# IMPORTANTE: se compila DENTRO de la carpeta (setwd) porque bookdown lee el
# _bookdown.yml del DIRECTORIO DE TRABAJO; así usa SOLO los 6 archivos reducidos
# y nunca la tesis completa de la raíz. La salida queda en
# version-reducida/_book/IVEC_version_reducida.pdf .
# Comenta este bloque si en una ejecución solo quieres la tesis completa.
# ─────────────────────────────────────────────────────────────────────────────
local({
  old_red <- setwd(here::here("version-reducida"))
  on.exit(setwd(old_red))
  bookdown::render_book("index.Rmd", "bookdown::pdf_book")
})


