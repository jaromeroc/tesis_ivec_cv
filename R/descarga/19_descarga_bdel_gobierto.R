# =============================================================================
# 19_descarga_bdel_gobierto.R — DESCARGA BDEL VÍA REPOSITORIO GOBIERTO-BUDGETS
# =============================================================================
# Módulo:   IVEC_estructural — dimensión CAD (capacidad institucional)
# Fuente:   PopulateTools/gobierto-budgets-data (GitHub)
#           Liquidaciones anuales del MHFP convertidas a SQL/PostgreSQL.
# Cobertura: 2015–2024 (10 años, ejecutadas/executed).
# Salida:   data/estructural/bdel/gobierto/{año}/{tabla}.sql.gz
#
# Tablas descargadas por año (todas en executed):
#   • tb_economica_cons.sql.gz   — gasto consolidado por capítulo económico
#                                  (capítulo 4: subvenciones; capítulo 6: inv. reales)
#   • tb_funcional_cons.sql.gz   — gasto consolidado por programa funcional
#                                  (programa 23x: servicios sociales)
#   • tb_inventario.sql.gz       — mapeo idente ↔ municipio + población oficial MHFP
#
# Indicadores CAD que estas tablas alimentan (la deuda viva municipal, cuarto
# indicador CAD, se procesa directamente dentro de R/ivec/07_ivec_estructural.R
# a partir de los XLS del MHFP depositados manualmente en data/estructural/bdel/
# con el patrón "deuda viva ayuntamientos *.xls/xlsx"):
#   1. Gasto liquidado en servicios sociales y dependencia per cápita
#   2. Inversiones reales per cápita (capítulo 6 económica)
#   3. Subvenciones concedidas por el municipio per cápita (capítulo 4 económica)
#
# El parseado de los ficheros descargados se realiza en `19b_parsear_bdel_gobierto.R`.
# =============================================================================

library(here)

# ── Configuración ────────────────────────────────────────────────────────────

ANYOS <- 2015:2024

TABLAS <- c(
  "tb_economica_cons",
  "tb_funcional_cons",
  "tb_inventario"
)

URL_BASE <- paste0(
  "https://raw.githubusercontent.com/PopulateTools/gobierto-budgets-data/",
  "master/data/presupuestos_municipales"
)

dir_destino <- here("data", "estructural", "bdel", "gobierto")
if (!dir.exists(dir_destino)) dir.create(dir_destino, recursive = TRUE)

# ── Loop de descarga (idempotente) ────────────────────────────────────────────

log_descarga <- data.frame(
  anyo    = integer(),
  tabla   = character(),
  estado  = character(),
  bytes   = integer(),
  ruta    = character(),
  stringsAsFactors = FALSE
)

for (anyo in ANYOS) {

  dir_anyo <- file.path(dir_destino, as.character(anyo))
  if (!dir.exists(dir_anyo)) dir.create(dir_anyo, recursive = TRUE)

  message("Año ", anyo)

  for (tabla in TABLAS) {

    nombre_fichero <- paste0(tabla, ".sql.gz")
    ruta_local     <- file.path(dir_anyo, nombre_fichero)
    url            <- paste0(URL_BASE, "/", anyo, "/executed/", nombre_fichero)

    if (file.exists(ruta_local) && file.size(ruta_local) > 0) {
      bytes  <- file.size(ruta_local)
      estado <- "ya_descargado"
      message("  ", tabla, ": ya descargado (", round(bytes / 1024), " KB)")
    } else {
      message("  ", tabla, ": descargando ", url, " …")
      res <- tryCatch({
        download.file(
          url      = url,
          destfile = ruta_local,
          mode     = "wb",
          quiet    = TRUE
        )
        bytes  <- file.size(ruta_local)
        estado <- "ok"
        message("    completado (", round(bytes / 1024), " KB)")
      }, error = function(e) {
        message("    ERROR: ", e$message)
        bytes  <<- NA_integer_
        estado <<- "error"
      })
    }

    log_descarga <- rbind(log_descarga, data.frame(
      anyo   = anyo,
      tabla  = tabla,
      estado = estado,
      bytes  = bytes,
      ruta   = ruta_local,
      stringsAsFactors = FALSE
    ))
  }
}

# ── Log final ─────────────────────────────────────────────────────────────────

ruta_log <- file.path(dir_destino, "log_descarga_gobierto.rds")
saveRDS(log_descarga, ruta_log)

message("")
message("RESUMEN")
message("-------")
message("Descargas OK:           ", sum(log_descarga$estado == "ok"))
message("Ya estaban descargadas: ", sum(log_descarga$estado == "ya_descargado"))
message("Errores:                ", sum(log_descarga$estado == "error"))
message("Volumen total:          ",
        round(sum(log_descarga$bytes, na.rm = TRUE) / 1024 / 1024, 1), " MB")
message("Log guardado en:        ", ruta_log)
