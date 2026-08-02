# =============================================================================
# 18_descarga_estructural.R — DESCARGA AUTOMÁTICA DE FUENTES INE PARA EL MÓDULO
#                              IVEC_estructural (V1, mayo 2026)
# =============================================================================
# Módulo:   IVEC_estructural (vulnerabilidad y capacidades estructurales)
# Escala:   Municipal (542 municipios CV).
# Cobertura del script:
#   • Atlas demográfico municipal INE (tabla 30877) — fuente de variables
#     auxiliares interpretativas (edad media, % unipersonales, % menores 18,
#     tamaño medio del hogar, población). Serie 2015–2023.
#   • Atlas de Distribución de Renta de los Hogares INE (tabla 31106) — fuente
#     de la mediana de renta neta media por persona que ordena los cuatro
#     clústeres k-means para producir VE* en el módulo estructural V1.
#     Serie 2015–2023.
#
# El resto de fuentes del módulo estructural (PEGV Indicadores Demográficos
# Municipales, PEGV Estadística de Migraciones, TGSS, BDEL deuda viva,
# repositorio gobierto-budgets para los demás indicadores CAD, RMEs FISABIO)
# se obtienen manualmente o por scripts específicos (19_descarga_bdel_gobierto.R,
# 19b_parsear_bdel_gobierto.R) y se depositan directamente en data/estructural/
# con los nombres canónicos:
#   • Indicadores_demograficos.csv             (PEGV; 8 indicadores municipales)
#   • Tasa_natalidad_comarcal.csv              (PEGV; auxiliar)
#   • Tasa_mortalidad_comarcal.csv             (PEGV; auxiliar)
#   • arope_comarcal.csv                       (PEGV; auxiliar)
#   • datos_RME.csv                            (FISABIO; validación externa)
#   • Pob_2002_2009_mun.csv                    (PEGV; histórico poblacional)
#   • TGS/MUNCNAE0{prov}{año}.xlsx             (TGSS; modelo productivo)
#   • bdel/deuda viva ayuntamientos *.xlsx     (BDEL; deuda viva CAD)
#   • bdel/gobierto/{año}/...                  (gobierto-budgets-data; CAD)
#
# =============================================================================

library(here)
library(httr)


# ── Configuración global ─────────────────────────────────────────────────────

dir_estructural        <- here("data", "estructural")
dir_atlas_demografico  <- file.path(dir_estructural, "atlas_demografico")
dir_atlas_renta_muni   <- file.path(dir_estructural, "atlas_renta_municipal")

for (d in list(dir_estructural, dir_atlas_demografico, dir_atlas_renta_muni)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Log estructurado de estado por fuente
log_estado <- tibble::tibble(
  Fuente   = character(),
  Estado   = character(),
  N_files  = integer(),
  Nota     = character()
)
registrar <- function(fuente, estado, n, nota = "") {
  log_estado[nrow(log_estado) + 1, ] <<- list(fuente, estado, as.integer(n), nota)
}


# ── Helpers ──────────────────────────────────────────────────────────────────

download_if_missing <- function(url, dest, timeout_s = 180) {
  if (file.exists(dest) && file.info(dest)$size > 0) {
    return(TRUE)
  }
  res <- tryCatch(
    httr::GET(url,
              httr::user_agent("IVEC/1.0 (research; juanantonio.romerocrespo@gmail.com)"),
              httr::timeout(timeout_s),
              httr::write_disk(dest, overwrite = TRUE),
              httr::progress()),
    error = function(e) NULL
  )
  if (is.null(res) || httr::status_code(res) != 200) {
    if (file.exists(dest)) file.remove(dest)
    return(FALSE)
  }
  return(TRUE)
}


# =============================================================================
# 1 — Atlas demográfico municipal INE (tabla 30877)
# =============================================================================
# Indicadores municipales del Atlas: edad media de la población, población,
# porcentaje de hogares unipersonales, porcentaje de población de 65 y más
# años, porcentaje de población española, porcentaje de población menor de
# 18 años, tamaño medio del hogar. Cobertura: 8.139 municipios España ×
# 2015–2023. El pipeline 07_ivec_estructural.R filtra a los 542 municipios CV
# y los utiliza como variables auxiliares interpretativas.
# =============================================================================

message("\n══════════════════════════════════════════════════════════════")
message("  1 — Atlas demográfico municipal INE (tabla 30877)")
message("══════════════════════════════════════════════════════════════")

url_30877  <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30877.csv?nocab=1"
dest_30877 <- file.path(dir_atlas_demografico, "atlas_demografico_municipal_30877.csv")

ok_30877 <- download_if_missing(url_30877, dest_30877, timeout_s = 300)
if (ok_30877) {
  size_mb <- round(file.info(dest_30877)$size / 1024^2, 1)
  message(sprintf("  ✓ Tabla 30877 disponible (%.1f MB)", size_mb))
  message("    Variables auxiliares: edad media, % unipersonales, % menores 18,")
  message("    tamaño medio del hogar, población. Cobertura: 542 municipios CV × 2015–2023")
  registrar("Atlas demográfico municipal INE (30877)",
            "COMPLETO", 1L,
            "Variables auxiliares interpretativas del módulo IVEC_estructural.")
} else {
  message("  ✗ Tabla 30877 no se ha podido descargar. Reintentar manualmente desde:")
  message("    https://www.ine.es/dynt3/inebase/es/index.htm?padre=7132")
  registrar("Atlas demográfico municipal INE (30877)",
            "PENDIENTE", 0L,
            "Descarga manual desde INEbase Atlas.")
}


# =============================================================================
# 2 — Atlas de Distribución de Renta de los Hogares INE (tabla 31106)
# =============================================================================
# Renta neta media por persona y renta mediana municipal del Atlas de Renta.
# La renta neta media por persona se conserva en el módulo IVEC_estructural V1
# como variable auxiliar interpretativa (no entra al escalar ni al criterio de
# ordenamiento; el ordenamiento de los cinco clústeres del k-means usa el índice
# compuesto de severidad demográfica, VE* en {0,1; 0,3; 0,5; 0,7; 0,9}).
# Cobertura: serie 2015–2023.
# =============================================================================

message("\n══════════════════════════════════════════════════════════════")
message("  2 — Atlas de Distribución de Renta INE (tabla 31106)")
message("══════════════════════════════════════════════════════════════")

ficheros_atlas <- list.files(dir_atlas_renta_muni, pattern = "\\.csv$", full.names = TRUE)
if (length(ficheros_atlas) > 0) {
  tamanyo_total <- sum(file.info(ficheros_atlas)$size) / 1024^2
  message(sprintf("  ✓ Atlas Renta municipal presente: %d fichero(s), %.0f MB total",
                  length(ficheros_atlas), tamanyo_total))
  message("    Filtrado a municipios CV + nivel municipal + Renta neta media por persona 2023")
  message("    se realiza en el pipeline 07_ivec_estructural.R.")
  registrar("Atlas Renta INE municipal (31106)",
            "COMPLETO", length(ficheros_atlas),
            "Criterio de ordenamiento k-means para VE* en IVEC_estructural V1.")
} else {
  message("  ✗ Atlas Renta municipal ausente. Descargando tabla 31106...")
  url_31106  <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/31106.csv?nocab=1"
  dest_31106 <- file.path(dir_atlas_renta_muni, "atlas_renta_municipal_31106.csv")
  ok_31106 <- download_if_missing(url_31106, dest_31106, timeout_s = 600)
  if (ok_31106) {
    message(sprintf("  ✓ Tabla 31106 descargada (%.0f MB)",
                    file.info(dest_31106)$size / 1024^2))
    registrar("Atlas Renta INE municipal (31106)",
              "COMPLETO", 1L, "Tabla 31106 descargada.")
  } else {
    registrar("Atlas Renta INE municipal (31106)",
              "PENDIENTE", 0L, "Descarga manual desde INEbase Atlas.")
  }
}


# =============================================================================
# RESUMEN
# =============================================================================

message("\n══════════════════════════════════════════════════════════════")
message("  RESUMEN DE LA DESCARGA")
message("══════════════════════════════════════════════════════════════")
print(log_estado)

# Guardar el log para auditoría
saveRDS(log_estado, file.path(dir_estructural, "log_descarga_estructural.rds"))
message(sprintf("\n  Log guardado en %s",
                file.path(dir_estructural, "log_descarga_estructural.rds")))

fuentes_pendientes <- log_estado |>
  dplyr::filter(Estado %in% c("PENDIENTE_MANUAL", "PARCIAL", "PENDIENTE"))

if (nrow(fuentes_pendientes) > 0) {
  message("\n┌──────────────────────────────────────────────────────────────")
  message("│  DESCARGAS PENDIENTES")
  message("└──────────────────────────────────────────────────────────────")
  for (i in seq_len(nrow(fuentes_pendientes))) {
    message(sprintf("  • %s — %s",
                    fuentes_pendientes$Fuente[i],
                    fuentes_pendientes$Nota[i]))
  }
} else {
  message("\n  ✓ Atlas demográfico y Atlas Renta disponibles. El pipeline")
  message("    07_ivec_estructural.R puede ejecutarse cuando estén descargadas")
  message("    también las fuentes manuales (PEGV, TGSS, BDEL, FISABIO).")
}

message("══════════════════════════════════════════════════════════════\n")
