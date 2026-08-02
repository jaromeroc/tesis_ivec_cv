# ===========================================================
# R/renderizado/tesis_tabla.R
# Helper para tablas en salida Word (formato académico).
# Para salida LaTeX se usa kableExtra directamente en cada chunk.
# ===========================================================

#' Tabla académica para salida Word
#'
#' Envuelve un data.frame en un flextable con formato booktabs,
#' fuente de 9 pt, cabeceras en negrita y ajuste automático de columnas.
#'
#' @param df         data.frame con los datos de la tabla.
#' @param caption    Título de la tabla (character).
#' @param col_names  Vector de nombres de columna visibles.
#'                   Si es NULL, se usan los nombres del data.frame.
#' @param fontsize   Tamaño de fuente (por defecto 9).
#' @param bold_rows  Filas a marcar en negrita (vector de enteros). NULL = ninguna.
#'
#' @return Objeto flextable listo para insertar en el documento Word.
tesis_tabla_word <- function(df,
                             caption   = NULL,
                             col_names = NULL,
                             fontsize  = 9,
                             bold_rows = NULL) {

  if (!requireNamespace("flextable", quietly = TRUE))
    stop("Instala el paquete 'flextable' para generar tablas en Word.")

  # Convertir matrices u objetos similares a data.frame
  # (apply() devuelve matrix, no data.frame; flextable exige data.frame)
  if (is.matrix(df) || !is.data.frame(df)) {
    # Para matrices con rownames informativos, incorporarlos como primera columna
    if (is.matrix(df) && !is.null(rownames(df)) &&
        !all(rownames(df) == as.character(seq_len(nrow(df))))) {
      df <- data.frame(Indicador = rownames(df),
                       as.data.frame(df, stringsAsFactors = FALSE),
                       stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
    }
  }

  # Renombrar columnas visibles si se proporcionan
  if (!is.null(col_names)) {
    if (length(col_names) != ncol(df))
      stop("col_names debe tener la misma longitud que el número de columnas de df.")
    names(df) <- col_names
  }

  ft <- flextable::flextable(df)

  # Tema booktabs: líneas horizontales solo en cabecera/pie, sin líneas verticales
  ft <- flextable::theme_booktabs(ft)

  # Fuente uniforme
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = fontsize, part = "all")

  # Cabecera en negrita
  ft <- flextable::bold(ft, part = "header")

  # Alineación: cabecera centrada, cuerpo alineado a la izquierda por defecto
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, align = "left",   part = "body")

  # Filas opcionales en negrita (p. ej. filas de totales o categorías)
  if (!is.null(bold_rows)) {
    ft <- flextable::bold(ft, i = bold_rows, part = "body")
  }

  # Ajuste automático de anchos (respeta el ancho de página de la plantilla)
  ft <- flextable::autofit(ft)
  ft <- flextable::set_table_properties(ft, layout = "autofit", width = 1)

  # Título de tabla si se proporciona
  if (!is.null(caption)) {
    ft <- flextable::set_caption(ft, caption = caption)
  }

  ft
}
