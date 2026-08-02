# =============================================================================
# CENSO 2021 — Indicadores físico-residenciales a nivel de sección censal (CV)
# Módulo IVEC_contextual · sub-dimensión VC físico-residencial
#
#   Bloque 1 — vc_tenencia : % de viviendas principales en alquiler o cesión
#                            Fuente: fichero de indicadores a sección censal
#                            del Censo 2021 (C2021_Indicadores.csv).
#
# Fuente:  INE — Censos de Población y Viviendas 2021
#          Indicadores por sección censal: fichero ya descargado en data/contextual/
# Escala:  Sección censal (código CUSEC de 10 dígitos)
# Año ref: 2021
# Salida:  data/contextual/censo2021/
#
# NOTA SOBRE vc_superficie: la superficie media útil por vivienda NO se obtiene
# en este script. La vía Censo 2021 / API del SDC21 (tabla viv.fam, métrica
# MEDIA_SSUPERFICIE) se probó y se descartó el 14 de mayo de 2026: el SDC21 no
# difunde la superficie a escala de sección censal —la variable ID_RESIDENCIA_N5
# solo admite consulta aislada; cruzada con MEDIA_SSUPERFICIE devuelve un
# resultado vacío y con ID_SUP_VIV devuelve error 404—, presumiblemente por
# secreto estadístico. La fuente de vc_superficie queda fijada en el Catastro
# (decisión de operacionalización V1: fuente catastral).
#
# NOTA DE INTEGRACIÓN: la proyección de la sección censal a la cuadrícula de
# 1 km² LAEA NO se hace aquí (principio de separación adquisición/procesamiento);
# la realiza el pipeline R/ivec/04_ivec_contextual.R. Este script deja el
# indicador limpio a sección censal listo para esa asignación.
# =============================================================================

library(tidyverse)
library(here)

# -----------------------------------------------------------------------------
# CONFIGURACIÓN GENERAL
# -----------------------------------------------------------------------------
dir_in  <- here("data/contextual")              # ficheros crudos ya descargados
dir_out <- here("data/contextual/censo2021")    # salida de este script
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

# Provincias de la Comunitat Valenciana (código INE de 2 dígitos)
CPRO_CV <- c("03", "12", "46")   # Alacant, Castelló, València

# =============================================================================
# BLOQUE 1 — vc_tenencia
# -----------------------------------------------------------------------------
# Fichero de indicadores del Censo 2021 a sección censal. Estructura verificada:
#   claves geográficas : ccaa, cpro, cmun, dist, secc
#   indicadores de vivienda principal por régimen de tenencia (tabla T20):
#     t19_1 = viviendas principales         (= t20_1 + t20_2 + t20_3)
#     t20_1 = viviendas en propiedad
#     t20_2 = viviendas en alquiler
#     t20_3 = viviendas en otro régimen de tenencia (incluye cesión)
#
#   vc_tenencia = (t20_2 + t20_3) / t19_1
#   — proporción de viviendas principales en alquiler o cesión/otro régimen.
# =============================================================================

f_indic <- file.path(dir_in, "C2021_Indicadores.csv")

if (!file.exists(f_indic)) {
  stop("No se encuentra el fichero de indicadores del Censo 2021: ", f_indic,
       "\n  Descárgalo y deposítalo en data/contextual/ antes de ejecutar.")
}

message("BLOQUE 1 — vc_tenencia desde ", basename(f_indic))

# Las claves geográficas se leen como texto para preservar los ceros a la izquierda
indic_raw <- read_csv(
  f_indic,
  col_types = cols(
    ccaa = col_character(), cpro = col_character(), cmun = col_character(),
    dist = col_character(), secc = col_character(),
    .default = col_double()
  ),
  locale = locale(encoding = "UTF-8")
)

tenencia_seccen <- indic_raw %>%
  filter(cpro %in% CPRO_CV) %>%
  transmute(
    # código de sección censal de 10 dígitos: prov(2)+mun(3)+dist(2)+secc(3)
    cusec        = paste0(cpro, cmun, dist, secc),
    cpro, cmun,
    viv_princ    = t19_1,
    viv_alquiler = t20_2,
    viv_otro     = t20_3,
    vc_tenencia  = if_else(t19_1 > 0, (t20_2 + t20_3) / t19_1, NA_real_)
  )

n_total <- nrow(tenencia_seccen)
n_na    <- sum(is.na(tenencia_seccen$vc_tenencia))
message("  Secciones censales CV: ", n_total,
        "  (con dato: ", n_total - n_na, " · sin dato/secreto: ", n_na, ")")
message("  vc_tenencia — media: ",  round(mean(tenencia_seccen$vc_tenencia, na.rm = TRUE), 4),
        " · rango: [", round(min(tenencia_seccen$vc_tenencia, na.rm = TRUE), 4), ", ",
        round(max(tenencia_seccen$vc_tenencia, na.rm = TRUE), 4), "]")

out_tenencia <- file.path(dir_out, "vc_tenencia_seccen.csv")
write_csv(tenencia_seccen, out_tenencia)
message("  Guardado: ", out_tenencia, "\n")

# =============================================================================
# RESUMEN
# =============================================================================
message("Censo 2021 — descarga/procesamiento a sección censal completado.")
message("  · vc_tenencia  : ", file.path(dir_out, "vc_tenencia_seccen.csv"))
message("\nIntegración: actualizar 'ruta_tenencia' en ")
message("R/ivec/04_ivec_contextual.R para que apunte a este fichero.")
