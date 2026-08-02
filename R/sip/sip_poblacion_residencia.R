# =============================================================================
# SIP — Población total clasificada por condición de residencia (D2_resid)
# Fuente: SIP_final.parquet (data/SIP/results/)
# Escala: Comunitat Valenciana (total)
# Fecha: 2026-03-17
# =============================================================================
# Produce una tabla resumen con:
#   - n_total   : total de registros en el SIP
#   - n         : registros por categoría de D2_resid
#   - pct       : porcentaje sobre el total
# Salida: data/SIP/results/sip_pob_residencia.csv
# =============================================================================

library(here)
library(arrow)
library(dplyr)
library(glue)

# -----------------------------------------------------------------------------
# 0. Rutas
# -----------------------------------------------------------------------------

ruta_sip <- here::here("data/SIP/results", "SIP_final.parquet")
ruta_out <- here::here("data/SIP/results", "sip_pob_residencia.csv")

# Verificación de existencia
if (!file.exists(ruta_sip)) {
  stop(
    "⚠️  No se encontró 'SIP_final.parquet'.\n",
    "    Ejecute el flujo completo de A3-sip.Rmd antes de continuar.\n",
    "    Ruta esperada: ", ruta_sip
  )
}

# -----------------------------------------------------------------------------
# 1. Carga del dataset final
# -----------------------------------------------------------------------------

df <- arrow::read_parquet(ruta_sip)

n_total <- nrow(df)
message(glue::glue("✔  Dataset cargado: {format(n_total, big.mark = '.')} registros totales."))

# -----------------------------------------------------------------------------
# 2. Verificación de columnas requeridas
# -----------------------------------------------------------------------------

col_cod  <- "D2_resid(cod)"
col_desc <- "D2_resid(desc)"

if (!col_cod %in% names(df)) {
  stop(glue::glue(
    "⚠️  Columna '{col_cod}' no encontrada en SIP_final.parquet.\n",
    "    Columnas disponibles: {paste(names(df), collapse = ', ')}"
  ))
}

# -----------------------------------------------------------------------------
# 3. Tabla resumen por condición de residencia
# -----------------------------------------------------------------------------

resumen <- df %>%
  group_by(
    .data[[col_cod]],
    .data[[col_desc]]
  ) %>%
  summarise(
    n   = n(),
    .groups = "drop"
  ) %>%
  rename(
    D2_resid_cod  = 1,
    D2_resid_desc = 2
  ) %>%
  mutate(
    n_total = n_total,
    pct     = round(100 * n / n_total, 2)
  ) %>%
  arrange(D2_resid_cod)

# -----------------------------------------------------------------------------
# 4. Impresión en consola
# -----------------------------------------------------------------------------

message(glue::glue("\n{'─' %+% strrep('─', 60)}"))
message(glue::glue("  POBLACIÓN SIP — CONDICIÓN DE RESIDENCIA (D2_resid)"))
message(glue::glue("  N total: {format(n_total, big.mark = '.')} registros"))
message(glue::glue("{'─' %+% strrep('─', 60)}"))

print(
  resumen %>%
    select(D2_resid_cod, D2_resid_desc, n, pct),
  n = Inf
)

# Verificación: la suma debe coincidir con n_total
n_clasificados <- sum(resumen$n)
n_sin_cod      <- n_total - n_clasificados

if (n_sin_cod > 0) {
  message(glue::glue(
    "\n⚠️  {format(n_sin_cod, big.mark = '.')} registros con D2_resid NA",
    " ({round(100 * n_sin_cod / n_total, 2)}% del total)."
  ))
} else {
  message(glue::glue("\n✔  Todos los registros tienen código D2_resid asignado."))
}

# -----------------------------------------------------------------------------
# 5. Guardado del resultado
# -----------------------------------------------------------------------------

dir.create(dirname(ruta_out), recursive = TRUE, showWarnings = FALSE)
write.csv(resumen, ruta_out, row.names = FALSE, fileEncoding = "UTF-8")
message(glue::glue("✔  Tabla guardada en: {ruta_out}"))
