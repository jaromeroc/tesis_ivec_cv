# =============================================================================
# SCRIPT MAESTRO — Descarga de fuentes secundarias del IVEC (V1, mayo 2026)
# Tesis doctoral: Vulnerabilidad Estructural y Contextual — Comunitat Valenciana
# =============================================================================
# Ejecutar desde RStudio con el proyecto abierto:
#   source("R/descarga/00_descarga_maestro.R")
#
# Cada módulo tiene su propio script independiente. Este maestro los llama en
# secuencia. Puedes ejecutar solo los que necesites comentando las llamadas.
#
# REVISIÓN MAYO 2026:
#   Los scripts de descarga obsoletos (EVR, DIRCE, AEAT IRPF, VAB comarcal,
#   SEPE paro, electoral, padrón a sección/municipal, Atlas renta a sección,
#   Catastro Excel, equipamientos CKAN GVA, asociaciones) han sido retirados
#   del pipeline tras el rediseño arquitectural del IVEC documentado en
#   el cap. 5 §sec-vecad-fundamento. Las fuentes canónicas V1
#   pasan por PEGV (índices demográficos municipales + estadística de
#   migraciones), Atlas INE 2023 a nivel municipal, TGSS, BDEL/gobierto y
#   las gpkg de equipamientos descargadas por 17_descarga_contextual_cs.R.
# =============================================================================

cat("\n========================================================\n")
cat("  DESCARGA DE FUENTES SECUNDARIAS DEL IVEC (V1)\n")
cat("  Ventana de referencia: 2015-2025 (asimétrica por fuente)\n")
cat("========================================================\n\n")

# --- Directorio raíz del proyecto -------------------------------------------
# here::here() detecta automáticamente la raíz del proyecto .Rproj
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

# --- Verificar paquetes necesarios -------------------------------------------
pkgs_necesarios <- c("tidyverse", "httr", "httr2", "jsonlite", "readxl",
                     "sf", "curl", "glue", "fs")
pkgs_faltantes <- pkgs_necesarios[!pkgs_necesarios %in% installed.packages()[,"Package"]]
if (length(pkgs_faltantes) > 0) {
  message("Instalando paquetes necesarios: ", paste(pkgs_faltantes, collapse = ", "))
  install.packages(pkgs_faltantes, dependencies = TRUE,
                   repos = "https://cloud.r-project.org")
}
invisible(lapply(pkgs_necesarios, library, character.only = TRUE))

# =============================================================================
# INFRAESTRUCTURA CARTOGRÁFICA (compartida por los tres módulos)
# =============================================================================
cat("\n>>> INFRAESTRUCTURA CARTOGRÁFICA\n\n")

# La cartografía de límites administrativos (municipios, secciones, comarcas) se
# obtuvo manualmente y reside en data/mallas/malla_administrativa/
# (ver R/descarga/README.md). No requiere descarga por script.

cat("  [1/1] Malla LAEA 1×1 km — marco cartográfico del IVEC_base...\n")
tryCatch(
  source(here("R/descarga/16_malla_ign_ivecbase.R")),
  error = function(e) message("  [16_malla] Error: ", e$message)
)

# =============================================================================
# MÓDULO CONTEXTUAL (IVEC_contextual — cuadrícula 1 km² LAEA)
# Fuentes canónicas V1: Censo 2021 (tenencia), Catastro CAT+SHP (antigüedad,
# superficie, hacinamiento), Censo Anual 2025 (educativo del entorno) y
# 17_descarga_contextual_cs para la dimensión CS al completo (RECESSO +
# centros docentes + paradas OSM + centros de salud GVA + GeoJSONs cívicos).
# Los indicadores demográfico-poblacionales se reconstruyen desde el SIP
# por agregación ascendente; su descarga se gestiona en R/sip/.
# =============================================================================
cat("\n>>> MÓDULO CONTEXTUAL\n\n")

cat("  [1/2] Censo 2021 — sección censal (vc_tenencia)...\n")
source(here("R/descarga/01_censo2021_seccion.R"))

cat("  [2/2] Descarga CS contextual (RECESSO + docentes + OSM + cívicos)...\n")
tryCatch(
  source(here("R/descarga/17_descarga_contextual_cs.R")),
  error = function(e) message("  [17_contextual_cs] Error: ", e$message)
)

# =============================================================================
# MÓDULO ESTRUCTURAL (IVEC_estructural — escala municipal, 542 municipios CV)
# Fuentes canónicas V1: TGSS (modelo productivo, serie 2015-2024),
# PEGV Indicadores Demográficos Municipales (envejecimiento, maternidad,
# tendencia, serie 2015-2025), PEGV Estadística de Migraciones (saldos
# interior y exterior, serie 2021-2024), BDEL y repositorio gobierto-budgets
# (CAD, serie 2015-2023), Atlas INE 2023 (criterio de ordenamiento k-means).
# =============================================================================
cat("\n>>> MÓDULO ESTRUCTURAL\n\n")

# La afiliación TGSS del modelo productivo (cociente de localización, índice de
# Hirschman-Herfindahl y amplitud estacional) se procesa en
# R/ivec/07_ivec_estructural.R a partir de los ficheros TGS/MUNCNAE depositados
# manualmente en data/estructural/TGS/.

cat("  [1/2] Descarga estructural (Atlas demográfico INE 30877 + Atlas Renta 31106)...\n")
tryCatch(
  source(here("R/descarga/18_descarga_estructural.R")),
  error = function(e) message("  [18_estructural] Error: ", e$message)
)

cat("  [2/2] BDEL — repositorio gobierto-budgets (CAD: gasto SS, inversiones, subvenciones)...\n")
tryCatch(
  source(here("R/descarga/19_descarga_bdel_gobierto.R")),
  error = function(e) message("  [19_bdel_gobierto] Error: ", e$message)
)
tryCatch(
  source(here("R/descarga/19b_parsear_bdel_gobierto.R")),
  error = function(e) message("  [19b_parsear_bdel] Error: ", e$message)
)
# Nota: la deuda viva municipal (cuarto indicador CAD) se procesa dentro del
# pipeline R/ivec/07_ivec_estructural.R a partir de los XLS del MHFP
# depositados manualmente en data/estructural/bdel/ (deuda viva ayuntamientos *.xls/xlsx).
# Las fuentes PEGV de envejecimiento (Indicadores_demograficos.csv) y de
# migraciones (saldo_migratorio.xlsx) ya están en data/estructural/ y no
# requieren descarga adicional.

# =============================================================================
# RESUMEN DE ESTADO
# =============================================================================
cat("\n========================================================\n")
cat("  Pipeline de descarga V1 completado.\n")
cat("\n")
cat("  Datos almacenados en:\n")
cat("  - data/mallas/        (malla LAEA, cartografía administrativa manual)\n")
cat("  - data/contextual/    (Censo 2021, Catastro CAT+SHP, gpkg CS)\n")
cat("  - data/estructural/   (TGSS, PEGV, INE 30877, Atlas Renta, BDEL)\n")
cat("\n")
cat("  Ficheros con descarga manual o asistida (ver INSTRUCCIONES_*.txt):\n")
cat("  - data/estructural/tgss/\n")
cat("  - data/estructural/bdel/\n")
cat("  - data/base/cartografia/\n")
cat("  - data/mallas/malla_ign/\n")
cat("\n")
cat("  Fuentes diferidas a V2 (no incluidas en este pipeline):\n")
cat("  - Registre Associacions GVA (cs_asociativo; bloqueado por geocoding)\n")
cat("  - VAB per cápita comarcal (Ivie/PEGV)\n")
cat("  - DIRCE concentración empresarial (solapamiento con diversificación TGSS)\n")
cat("========================================================\n")
