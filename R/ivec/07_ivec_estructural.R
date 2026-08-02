# =============================================================================
# R/ivec/07_ivec_estructural.R — PIPELINE DEL MÓDULO IVEC_estructural (V1, mayo 2026)
# =============================================================================
# Módulo:   IVEC_estructural — vulnerabilidad estructural (VE) × capacidades
#           de adaptación y transformación del municipio (CAD).
# Escala:   Municipal (542 municipios de la Comunitat Valenciana).
# Ruta arquitectural D (ver cap. 5 §sec-vecad-fundamento):
#   • Panel de 12 indicadores (8 VE + 4 CAD) + variables auxiliares
#     interpretativas.
#   • VE* se deriva por clusterización k-means K = 5 sobre los 8 indicadores VE
#     estandarizados; los cinco clústeres se identifican con perfiles PAR
#     (agrario-interior despoblado extremo, monoproducción estacional envejecida,
#     especializado-productivo intermedio, atractor de inmigración exterior,
#     perfil diversificado de baja severidad estructural) y se ordenan por la
#     mediana del índice compuesto de severidad demográfica del clúster
#     [z(envej) + z(-matern) + z(-tend)]. El clúster de mayor severidad recibe
#     VE* = 0,9; los siguientes 0,7, 0,5, 0,3; el de menor severidad 0,1.
#   • CAD* mantiene la normalización percentílica + media equiponderada de
#     los cuatro indicadores CAD (deuda viva con dirección invertida) +
#     min-max sobre los 542 municipios.
#   • Fórmula compensadora canónica:
#         IVEC_estructural = ½·VE* + ½·(1 − CAD*)
#   • Cada municipio lleva, además del valor escalar en [0, 1], su etiqueta
#     de clúster como variable cualitativa.
#
# Series de referencia (asimétricas por fuente):
#   • TGSS  (modelo productivo)     : LQ y HHI con dato de diciembre 2015–2024;
#                                     amplitud estacional con serie mensual
#                                     completa 2022–2024 (los únicos años con
#                                     12 ficheros mensuales en data/estructural/TGS/).
#   • PEGV  (envejecimiento)         : índices envejecimiento, maternidad y
#                                     tendencia, serie 2015–2025 (último año
#                                     disponible en Indicadores_demograficos.csv).
#   • PEGV  (migración)              : saldos interior + exterior, serie
#                                     2021–2024, operacionalizados como tasas
#                                     netas por mil habitantes mediante
#                                     división por población municipal a 1 de
#                                     enero del año correspondiente.
#   • BDEL  (gobierto + deuda viva)  : serie 2015–2023 para CAD (media de
#                                     los nueve años); deuda viva 2015–2023
#                                     en miles de euros.
#
# Inputs canónicos (todos en data/estructural/):
#   • Indicadores_demograficos.csv                     (PEGV, latin-1, sin cabecera)
#   • saldo_migratorio.xlsx                            (PEGV; pivot multi-cabecera)
#   • pob_2021__2024_mun.csv                           (PEGV, latin-1, con cabecera)
#   • TGS/MUNCNAE{MM}{YY}.xlsx                         (TGSS; afiliación mensual)
#   • bdel/cad_gobierto_2015_2024.rds                  (gobierto-budgets-data)
#   • bdel/deuda viva ayuntamientos *.xls/xlsx         (MHFP; 2015–2023)
#   • atlas_demografico/atlas_demografico_municipal_30877.csv  (INE; auxiliares)
#   • atlas_renta_municipal/atlas_renta_municipal_*.csv        (INE 31106; renta)
#   • Tasa_natalidad_comarcal.csv, Tasa_mortalidad_comarcal.csv (PEGV; auxiliares)
#   • arope_comarcal.csv                               (PEGV; auxiliar)
#
# Output:
#   • data/estructural/ivec_estructural/panel_municipal.parquet
#   • data/estructural/ivec_estructural/ivec_estructural.parquet
#   • data/estructural/ivec_estructural/ivec_estructural.gpkg
#
# La validación externa del plano 2 (correlación de Spearman con las RMEs
# suavizadas FISABIO de data/estructural/datos_RME.csv) se realiza en un
# script independiente: R/ivec/08_validacion_externa_estructural.R.
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(stringr)
  library(purrr)
  library(here)
  library(sf)
})

set.seed(20260519L)   # Reproducibilidad del k-means (PASO 9)


# =============================================================================
# FASE 0 — Parámetros globales, helpers y directorios
# =============================================================================

# Universos temporales canónicos V1
ANYOS_TGSS_LQ_HHI    <- 2015:2024
ANYOS_TGSS_ESTACION  <- 2022:2024     # los únicos con cobertura mensual completa
ANYOS_PEGV_DEMO      <- 2015:2025
ANYOS_MIGRACION      <- 2021:2024
ANYOS_CAD            <- 2015:2023
ANYO_RENTA           <- 2023L          # Atlas INE 31106 — renta como variable auxiliar
ANYO_AROPE           <- 2025L          # AROPE comarcal — variable auxiliar interpretativa

PROVINCIAS_CV        <- c("03", "12", "46")
N_MUNICIPIOS_CV      <- 542L
K_CLUSTERS           <- 5L
W_VE                 <- 0.5
W_CAD                <- 0.5

# Asignación equiespaciada en [0, 1] para los K clústeres del k-means.
# K = 5 → cinco quintiles centrados {0.1, 0.3, 0.5, 0.7, 0.9} con paso 0.2.
# Migración K=4 → K=5 documentada en A4 §app-d-arope-orden tras la verificación
# empírica de mayo de 2026: con K=4, el cluster mayoritario (n=372, ~68 %
# del territorio CV) mezclaba el perfil rural-medio sano con el perfil
# urbano-metropolitano diversificado, lo cual limitaba la capacidad
# discriminante del módulo. K=5 separa esos dos perfiles.
VE_STAR_VALORES      <- c(0.1, 0.3, 0.5, 0.7, 0.9)

# Rutas
DIR_BASE <- here("data", "estructural")
DIR_OUT  <- file.path(DIR_BASE, "ivec_estructural")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)


# ─── Helpers ────────────────────────────────────────────────────────────────

#' Normaliza un código municipal a 5 dígitos (provincia 2 + municipio 3).
#' Acepta entradas numéricas y de texto con o sin ceros iniciales.
norm_cmun <- function(x) {
  s <- as.character(x)
  s <- str_extract(s, "\\d+")
  s <- str_pad(s, width = 5, side = "left", pad = "0")
  s
}

#' Extrae el código municipal de una cadena tipo "03001 - Atzúbia, l'" o
#' "03001 Atzúbia, l'".
extraer_cmun_etiqueta <- function(x) {
  s <- as.character(x)
  s <- str_extract(s, "^\\s*(\\d{5})")
  str_trim(s)
}

#' Min-max sobre un vector numérico (NA-safe).
minmax01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

#' Rank percentil en [0, 1].
pct_rank <- function(x) {
  if (all(is.na(x))) return(x)
  rank(x, na.last = "keep", ties.method = "average") / sum(!is.na(x))
}

#' Lee un CSV PEGV o INE en latin-1. El delimitador no es uniforme entre
#' ficheros: el Atlas demográfico INE 30877 y los Indicadores Demográficos
#' PEGV usan `;`, mientras que pob_2021__2024_mun.csv usa `,`. Por defecto
#' se asume `;` y se sobrescribe en los call sites donde aplique.
#' Se fija `grouping_mark = "."` para parsear correctamente cifras con punto
#' de miles (p. ej. 16.429 € de renta neta media) sin romper la lectura de
#' decimales con coma como 49,8 años de edad media.
read_pegv_csv <- function(path, delim = ";", ...) {
  read_delim(path, delim = delim,
             locale = locale(encoding = "latin1",
                             decimal_mark = ",",
                             grouping_mark = "."),
             show_col_types = FALSE, ...)
}

message("\n══════════════════════════════════════════════════════════════")
message("  PIPELINE IVEC_estructural (V1, ruta arquitectural D)")
message("══════════════════════════════════════════════════════════════")


# =============================================================================
# FASE 1 — Universo: lista canónica de los 542 municipios CV
# =============================================================================
# Fuente primaria del universo: PEGV Indicadores_demograficos.csv, que cubre
# los 547 municipios CV con dato continuo desde 2002. Esta fuente sustituye al
# Atlas demográfico INE 30877 como universo de referencia tras la verificación
# operativa de mayo de 2026: el fichero INE descargado debe contener todos los
# 8.139 municipios de España, pero la descarga depositada en el corpus puede
# venir filtrada a una sola provincia si el formulario INEbase se exporta sin
# levantar el filtro provincial; el universo PEGV es directamente CV-nativo y
# no presenta esa ambigüedad. La decisión y la incidencia del Atlas se
# documentan en A4 §app-d-atlas-30877.
# =============================================================================
message("\n[1/13] Cargando universo CV (542 municipios) desde PEGV...")

# Diagnóstico defensivo del Atlas demográfico INE — si está disponible, se
# utiliza en FASE 7 para variables auxiliares. El pipeline acepta cualquier
# CSV depositado en data/estructural/atlas_demografico/ con el esquema canónico
# de seis columnas (Municipios, Distritos, Secciones, Indicadores demográficos,
# Periodo, Total). Esto permite usar indistintamente la tabla 30877 (panel
# municipal directo) o la tabla 30832 (panel a sección censal que incluye los
# registros agregados a municipio); ambas son funcionalmente equivalentes para
# las variables auxiliares del bloque "composición del hogar" del IVEC_estructural
# (decisión documentada en A4 §app-d-atlas-30877).
dir_atlas <- file.path(DIR_BASE, "atlas_demografico")
ficheros_atlas <- list.files(dir_atlas, pattern = "\\.csv$", full.names = TRUE,
                              ignore.case = TRUE)

atlas_demo <- if (length(ficheros_atlas) >= 1) {
  ruta_atlas <- ficheros_atlas[1]
  if (length(ficheros_atlas) > 1) {
    message(sprintf("  Atlas: %d ficheros CSV detectados; usando %s.",
                    length(ficheros_atlas), basename(ruta_atlas)))
  } else {
    message(sprintf("  Atlas: usando %s.", basename(ruta_atlas)))
  }
  ad <- read_pegv_csv(
    ruta_atlas,
    col_names = c("Municipios", "Distritos", "Secciones",
                  "Indicador", "Periodo", "Valor"),
    skip = 1
  )
  provs_atlas <- unique(str_sub(ad$Municipios, 1, 2))
  provs_atlas <- provs_atlas[!is.na(provs_atlas) & nchar(provs_atlas) == 2]
  if (length(intersect(provs_atlas, PROVINCIAS_CV)) == 0) {
    message("  ⚠ Atlas no cubre municipios CV (provincias detectadas: ",
            paste(sort(provs_atlas), collapse = ", "), ").")
    message("    Re-descarga el fichero del INEbase sin filtro provincial.")
    message("    Las variables auxiliares del Atlas quedarán como NA.")
  } else {
    message(sprintf("  Atlas: %d provincias presentes (incluye CV).",
                    length(provs_atlas)))
  }
  ad
} else {
  message("  ⚠ Ningún CSV en data/estructural/atlas_demografico/. ",
          "Variables auxiliares del Atlas quedarán como NA.")
  NULL
}

# Universo CV primario: extraído de Indicadores_demograficos.csv (PEGV).
pegv_demo_universo <- read_pegv_csv(
  file.path(DIR_BASE, "Indicadores_demograficos.csv"),
  col_names = c("anyo", "Municipio", "Indicador", "Valor")
)

universo_cv <- pegv_demo_universo |>
  mutate(CMUN   = extraer_cmun_etiqueta(Municipio),
         nombre = str_trim(str_remove(Municipio, "^\\s*\\d{5}\\s*[-]?\\s*"))) |>
  filter(!is.na(CMUN), str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV) |>
  # Excluir registros históricos del PEGV (p. ej. "Gátova (hasta 1995)",
  # arrastre del cambio de provincia de Gátova en 1995): producirían un
  # municipio fantasma sin datos CAD y un NA en el escalar IVEC_estructural.
  filter(!str_detect(nombre, fixed("(hasta"))) |>
  distinct(CMUN, nombre) |>
  arrange(CMUN)

if (nrow(universo_cv) < 400) {
  stop("Universo CV obtenido del PEGV tiene menos de 400 municipios. ",
       "Revisar Indicadores_demograficos.csv: formato esperado ",
       "anyo;\"COD - Municipio\";Indicador;Valor con código de 5 dígitos al inicio del campo Municipio.")
}

message(sprintf("  Universo CV (PEGV): %d municipios (esperado %d).",
                nrow(universo_cv), N_MUNICIPIOS_CV))


# =============================================================================
# FASE 2 — VE Modelo productivo (TGSS): 3 indicadores
# =============================================================================
# Indicadores:
#   • ve_lq_sector_dominante : cociente de localización del sector CNAE
#                              dominante (mayor share de afiliación) sobre el
#                              referente provincial CV (media de los 542 muns
#                              ponderada por afiliación).
#   • ve_hhi_sectorial       : índice de Hirschman-Herfindahl de las cuotas
#                              sectoriales (CNAE-1 dígito) de cada municipio.
#   • ve_amplitud_estacional : (max − min) / media de afiliación mensual.
#
# Series:
#   • LQ y HHI: media de los datos de diciembre 2015–2024 (10 puntos).
#   • Amplitud estacional: media de la amplitud relativa mensual sobre
#                          los años 2022–2024 (cobertura mensual completa).
# =============================================================================
message("[2/13] Calculando VE modelo productivo desde TGSS...")

leer_muncnae <- function(path) {
  readxl::read_excel(path, sheet = 1, col_types = "text") |>
    select(FECHA = `FECHA`,
           cod_prov = `COD PROVINCIA`,
           cod_mun  = `COD MUNICIPIO`,
           cod_cnae = `COD CNAE`,
           afil     = last_col()) |>     # la última columna numérica es AFILIADOS
    mutate(
      FECHA    = as.integer(FECHA),
      cod_prov = str_pad(cod_prov, 2, pad = "0"),
      cod_mun  = str_pad(cod_mun, 5, pad = "0"),
      CMUN     = cod_mun,
      anyo     = FECHA %/% 100L,
      mes      = FECHA %%  100L,
      cnae1    = str_sub(str_pad(as.character(cod_cnae), 2, pad = "0"), 1, 1),
      afil     = suppressWarnings(as.numeric(afil))
    ) |>
    filter(cod_prov %in% PROVINCIAS_CV, !is.na(afil), afil > 0)
}

dir_tgss <- file.path(DIR_BASE, "TGS")
ficheros_tgss <- list.files(dir_tgss, pattern = "^MUNCNAE\\d{4}\\.xlsx$",
                             full.names = TRUE)

tgss_long <- purrr::map_dfr(ficheros_tgss, leer_muncnae)

# 2.1 — Afiliación municipal anual: dato de diciembre (cierre del año)
tgss_anual_dic <- tgss_long |>
  filter(mes == 12L, anyo %in% ANYOS_TGSS_LQ_HHI) |>
  group_by(CMUN, anyo, cnae1) |>
  summarise(afil = sum(afil, na.rm = TRUE), .groups = "drop")

# Cuotas sectoriales por municipio y año
tgss_anual_share <- tgss_anual_dic |>
  group_by(CMUN, anyo) |>
  mutate(share = afil / sum(afil, na.rm = TRUE)) |>
  ungroup()

# Cuota sectorial CV (referente del LQ): suma de afiliación CV por sector y año
tgss_cv_share <- tgss_anual_dic |>
  group_by(anyo, cnae1) |>
  summarise(afil_cv = sum(afil, na.rm = TRUE), .groups = "drop") |>
  group_by(anyo) |>
  mutate(share_cv = afil_cv / sum(afil_cv, na.rm = TRUE)) |>
  ungroup()

# 2.2 — LQ y HHI por municipio y año, luego media 2015–2024
tgss_lq_hhi <- tgss_anual_share |>
  inner_join(tgss_cv_share, by = c("anyo", "cnae1")) |>
  mutate(lq_sector = share / share_cv) |>
  group_by(CMUN, anyo) |>
  summarise(
    lq_max = max(lq_sector, na.rm = TRUE),
    hhi    = sum(share^2,   na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(CMUN) |>
  summarise(
    ve_lq_sector_dominante = mean(lq_max, na.rm = TRUE),
    ve_hhi_sectorial       = mean(hhi,    na.rm = TRUE),
    .groups = "drop"
  )

# 2.3 — Amplitud estacional: media de la amplitud mensual relativa 2022–2024
tgss_mensual <- tgss_long |>
  filter(anyo %in% ANYOS_TGSS_ESTACION) |>
  group_by(CMUN, anyo, mes) |>
  summarise(afil = sum(afil, na.rm = TRUE), .groups = "drop")

tgss_amplitud <- tgss_mensual |>
  group_by(CMUN, anyo) |>
  summarise(
    afil_max = max(afil, na.rm = TRUE),
    afil_min = min(afil, na.rm = TRUE),
    afil_med = mean(afil, na.rm = TRUE),
    amplitud_rel = (afil_max - afil_min) / pmax(afil_med, 1),
    .groups = "drop"
  ) |>
  group_by(CMUN) |>
  summarise(ve_amplitud_estacional = mean(amplitud_rel, na.rm = TRUE),
            .groups = "drop")

ve_productivo <- tgss_lq_hhi |>
  full_join(tgss_amplitud, by = "CMUN")

message(sprintf("  VE productivo: %d municipios con LQ/HHI; %d con amplitud estacional.",
                sum(!is.na(ve_productivo$ve_lq_sector_dominante)),
                sum(!is.na(ve_productivo$ve_amplitud_estacional))))


# =============================================================================
# FASE 3 — VE Envejecimiento (PEGV Indicadores_demograficos.csv): 3 indicadores
# =============================================================================
# Fuente sin cabecera: anyo;"COD - Municipio";Indicador;Valor.
# Indicadores objetivo:
#   • "Índice de envejecimiento"
#   • "Índice de maternidad"
#   • "Índice de tendencia"
# Se calcula la media municipal del indicador sobre la serie 2015–2025.
# =============================================================================
message("[3/13] Calculando VE envejecimiento desde PEGV Indicadores_demograficos.csv...")

pegv_demo_raw <- read_pegv_csv(
  file.path(DIR_BASE, "Indicadores_demograficos.csv"),
  col_names = c("anyo", "Municipio", "Indicador", "Valor")
)

# Conversión defensiva de `Valor` a numérico. El CSV PEGV lo trae entre
# comillas dobles ("57,6") y read_delim puede leerlo como caracter pese al
# decimal_mark, dado que la columna contiene mezcla de enteros y decimales.
# Forzamos as.numeric tras reemplazar "," por "." manualmente.
pegv_demo <- pegv_demo_raw |>
  mutate(
    anyo  = as.integer(anyo),
    CMUN  = extraer_cmun_etiqueta(Municipio),
    Valor = suppressWarnings(as.numeric(
      str_replace(as.character(Valor), ",", ".")
    ))
  ) |>
  filter(!is.na(CMUN), str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV,
         anyo %in% ANYOS_PEGV_DEMO)

# Detección flexible del nombre de indicador (acentos pueden venir corruptos)
indicador_norm <- function(s) {
  s |>
    tolower() |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "?") |>
    str_replace_all("[^a-z ]", "")
}

pegv_demo <- pegv_demo |>
  mutate(ind_n = indicador_norm(Indicador))

ve_envejecimiento <- pegv_demo |>
  filter(ind_n %in% c("indice de envejecimiento",
                      "indice de maternidad",
                      "indice de tendencia")) |>
  mutate(ind_canon = case_when(
    ind_n == "indice de envejecimiento" ~ "ve_indice_envejecimiento",
    ind_n == "indice de maternidad"     ~ "ve_indice_maternidad",
    ind_n == "indice de tendencia"      ~ "ve_indice_tendencia"
  )) |>
  group_by(CMUN, ind_canon) |>
  summarise(valor = mean(Valor, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = ind_canon, values_from = valor)

# Variables auxiliares: tasas de dependencia + longevidad + renovación activa
ve_aux_demo <- pegv_demo |>
  filter(ind_n %in% c("tasa de dependencia",
                      "tasa de dependencia de la poblacion mayor de  anos",
                      "tasa de dependencia de la poblacion menor de  anos",
                      "indice de longevidad",
                      "indice de renovacion de la poblacion activa")) |>
  mutate(ind_aux = case_when(
    ind_n == "tasa de dependencia"                                       ~ "aux_tasa_dependencia",
    ind_n == "tasa de dependencia de la poblacion mayor de  anos"        ~ "aux_dependencia_mayores",
    ind_n == "tasa de dependencia de la poblacion menor de  anos"        ~ "aux_dependencia_menores",
    ind_n == "indice de longevidad"                                      ~ "aux_indice_longevidad",
    ind_n == "indice de renovacion de la poblacion activa"               ~ "aux_renovacion_activa"
  )) |>
  group_by(CMUN, ind_aux) |>
  summarise(valor = mean(Valor, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = ind_aux, values_from = valor)

message(sprintf("  VE envejecimiento: %d municipios con los tres índices.",
                sum(complete.cases(ve_envejecimiento[, -1]))))


# =============================================================================
# FASE 4 — VE Migración (saldo_migratorio.xlsx + pob_2021__2024_mun.csv): 2 indicadores
# =============================================================================
# Pivot saldo_migratorio.xlsx: el dataset trae 559 filas (CV + provincias +
# comarcas + 542 municipios) con saldos exterior e interior por año
# 2021–2024. Las cabeceras están en filas 9–11, los datos empiezan en fila 12.
# Conversión a tasa neta por mil habitantes mediante división por la población
# municipal a 1 de enero del año correspondiente (pob_2021__2024_mun.csv).
# =============================================================================
message("[4/13] Calculando VE migración (saldos interior y exterior, tasas por mil hab.)...")

saldo_raw <- read_excel(
  file.path(DIR_BASE, "saldo_migratorio.xlsx"),
  sheet = "Informe", col_names = FALSE, skip = 11
)

# Reconstruir nombres: col 1 = territorio; cols 2-9 = (sext21, sint21, sext22, sint22, ...)
# distinct() defensivo por (CMUN, anyo, tipo) elimina eventuales duplicados de
# fila procedentes del XLSX original; cualquier municipio repetido en la hoja
# Informe (por ejemplo, por aparecer también como agregado parcial) queda con
# su primer valor, preservando la unicidad necesaria para el inner_join con
# población.
saldo_long <- saldo_raw |>
  rename(territorio = `...1`) |>
  rename_with(.cols = -1, .fn = function(x) {
    n <- length(x)
    anyos <- rep(2021:2024, each = 2)
    tipos <- rep(c("sext", "sint"), times = 4)
    paste0(tipos, "_", anyos)[seq_len(n)]
  }) |>
  filter(!is.na(territorio)) |>
  mutate(CMUN = extraer_cmun_etiqueta(territorio)) |>
  filter(!is.na(CMUN), str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV) |>
  pivot_longer(cols = starts_with("s"),
               names_to = c("tipo", "anyo"),
               names_sep = "_",
               values_to = "saldo") |>
  mutate(anyo = as.integer(anyo)) |>
  distinct(CMUN, anyo, tipo, .keep_all = TRUE)

# Población municipal anual (totales ambos sexos × Edat = Total).
# pob_2021__2024_mun.csv usa "," como delimitador (no ";" como otros CSV PEGV);
# las cabeceras originales están en valenciano (Període, Municipis, Sexe, Edat,
# Valor). El filtro por sexo se hace contra el valor del campo, que viene como
# "Ambdós sexes" cuando se lee con encoding latin1.
pob_anual <- read_pegv_csv(
  file.path(DIR_BASE, "pob_2021__2024_mun.csv"),
  delim = ",",
  col_names = c("Periodo", "Municipio", "Sexo", "Edad", "Valor"),
  skip = 1
) |>
  filter(str_detect(Sexo, "(?i)^ambd|^ambos"),
         Edad == "Total") |>
  mutate(
    anyo = as.integer(str_extract(Periodo, "\\d{4}")),
    CMUN = extraer_cmun_etiqueta(Municipio),
    pob_total = suppressWarnings(as.numeric(Valor))
  ) |>
  filter(!is.na(CMUN), str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV) |>
  select(CMUN, anyo, pob_total) |>
  # distinct() defensivo: el fichero PEGV puede contener varias filas por
  # (CMUN, anyo) si combina total + desglose etario o si replica el total.
  # Garantiza unicidad para el inner_join con saldo_long.
  distinct(CMUN, anyo, .keep_all = TRUE)

ve_migracion <- saldo_long |>
  inner_join(pob_anual, by = c("CMUN", "anyo")) |>
  mutate(tasa = saldo * 1000 / pob_total) |>
  group_by(CMUN, tipo) |>
  summarise(tasa_media = mean(tasa, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = tipo,
              values_from = tasa_media,
              names_glue = "ve_saldo_{tipo}_pmh") |>
  rename(ve_saldo_interior_pmh = ve_saldo_sint_pmh,
         ve_saldo_exterior_pmh = ve_saldo_sext_pmh)

message(sprintf("  VE migración: %d municipios con tasas netas calculadas.",
                sum(complete.cases(ve_migracion[, -1]))))


# =============================================================================
# FASE 5 — CAD parcial: gobierto-budgets-data (3 indicadores)
# =============================================================================
# Lee cad_gobierto_2015_2024.rds (producido por 19b_parsear_bdel_gobierto.R)
# y agrega por municipio la media de la serie 2015–2023 de los tres indicadores
# per cápita. Cobertura: ~ 512 municipios CV / 10 años → 5.123 filas.
# =============================================================================
message("[5/13] Cargando CAD parcial (gobierto-budgets-data)...")

cad_gob <- readRDS(file.path(DIR_BASE, "bdel", "cad_gobierto_2015_2024.rds")) |>
  mutate(CMUN = norm_cmun(cmun)) |>
  filter(str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV,
         anyo %in% ANYOS_CAD)

cad_gobierto <- cad_gob |>
  group_by(CMUN) |>
  summarise(
    cad_gasto_sss_pc     = mean(gasto_sss_pc,     na.rm = TRUE),
    cad_inversiones_pc   = mean(inversiones_pc,   na.rm = TRUE),
    cad_subvenciones_pc  = mean(subvenciones_pc,  na.rm = TRUE),
    aux_intereses_pc           = mean(intereses_pc,           na.rm = TRUE),
    aux_amortizacion_pc        = mean(amortizacion_pc,        na.rm = TRUE),
    aux_carga_financiera_pc    = mean(carga_financiera_pc,    na.rm = TRUE),
    .groups = "drop"
  )

message(sprintf("  CAD gobierto: %d municipios con los 3 indicadores.",
                sum(complete.cases(cad_gobierto[, c("cad_gasto_sss_pc",
                                                    "cad_inversiones_pc",
                                                    "cad_subvenciones_pc")]))))


# =============================================================================
# FASE 6 — CAD deuda viva (XLS del MHFP)
# =============================================================================
# Parseado de los XLS de "deuda viva ayuntamientos" del MHFP, serie 2015–2023.
# La hoja "Datos" empieza en fila 12; la columna 8 es "Deuda viva 31/12/YYYY
# (miles de euros)". Se construye la deuda viva per cápita anual usando la
# población oficial MHFP del panel cad_gobierto (columna `poblacion`).
# =============================================================================
message("[6/13] Parseando deuda viva XLS del MHFP...")

parse_deuda_viva <- function(path, anyo) {
  ext <- tools::file_ext(path)
  df  <- if (ext == "xlsx") {
    suppressMessages(readxl::read_excel(path, sheet = "Datos",
                                        col_names = FALSE, skip = 12))
  } else {
    suppressMessages(readxl::read_xls(path, sheet = "Datos",
                                       col_names = FALSE, skip = 12))
  }
  df <- df |>
    select(cod_prov = 4, cod_mun = 6, deuda = 8) |>
    mutate(
      cod_prov = str_pad(as.character(cod_prov), 2, pad = "0"),
      cod_mun  = str_pad(as.character(cod_mun),  3, pad = "0"),
      CMUN     = paste0(cod_prov, cod_mun),
      anyo     = anyo,
      deuda_miles_eur = suppressWarnings(as.numeric(deuda))
    ) |>
    filter(cod_prov %in% PROVINCIAS_CV, !is.na(deuda_miles_eur)) |>
    select(CMUN, anyo, deuda_miles_eur)
  df
}

ficheros_deuda <- list.files(file.path(DIR_BASE, "bdel"),
                              pattern = "(?i)^deuda.viva.+\\.(xls|xlsx)$",
                              full.names = TRUE)

# Extracción del año desde el nombre del fichero
anyo_de_deuda <- function(fname) {
  m <- str_extract(fname, "(20\\d{2})12|20\\d{2}1231|20\\d{2}__")
  if (is.na(m)) m <- str_extract(fname, "20\\d{2}")
  as.integer(str_sub(m, 1, 4))
}

# Procesa solo los ficheros del rango canónico 2015–2023
deuda_paths <- tibble(
  path = ficheros_deuda,
  anyo = vapply(ficheros_deuda, anyo_de_deuda, integer(1))
) |>
  filter(anyo %in% ANYOS_CAD) |>
  distinct(anyo, .keep_all = TRUE)

deuda_long <- purrr::map2_dfr(deuda_paths$path, deuda_paths$anyo,
                               possibly(parse_deuda_viva, otherwise = NULL))

# Población oficial MHFP del panel gobierto (mapeo CMUN × año)
pob_mhfp <- cad_gob |>
  select(CMUN, anyo, pob_mhfp = poblacion)

cad_deuda <- deuda_long |>
  left_join(pob_mhfp, by = c("CMUN", "anyo")) |>
  mutate(deuda_pc = ifelse(is.finite(pob_mhfp) & pob_mhfp > 0,
                           deuda_miles_eur * 1000 / pob_mhfp,
                           NA_real_)) |>
  group_by(CMUN) |>
  summarise(cad_deuda_viva_pc = mean(deuda_pc, na.rm = TRUE),
            .groups = "drop")

message(sprintf("  CAD deuda viva: %d municipios con dato 2015–2023.",
                sum(!is.na(cad_deuda$cad_deuda_viva_pc))))


# =============================================================================
# FASE 7 — Variables auxiliares interpretativas
# =============================================================================
# Atlas demográfico INE 30877 (último año disponible 2023):
#   edad media, % unipersonales, % menores 18, tamaño medio del hogar, población.
# =============================================================================
message("[7/13] Cargando variables auxiliares (Atlas INE 30877 + PEGV)...")

# El Atlas INE 30877 puede no estar disponible o cubrir solo una provincia
# (incidencia documentada en A4 §app-d-atlas-30877). En ese caso se produce
# un tibble vacío con las columnas esperadas para que los left_join posteriores
# no rompan; las variables auxiliares del Atlas quedarán como NA en el panel.
columnas_atlas <- c("CMUN", "aux_edad_media", "aux_poblacion",
                    "aux_pct_unipersonales", "aux_pct_menores_18",
                    "aux_tam_hogar")

aux_atlas <- if (is.null(atlas_demo) ||
                 sum(str_sub(atlas_demo$Municipios, 1, 2) %in% PROVINCIAS_CV) == 0) {
  message("  Atlas INE 30877 sin cobertura CV: variables auxiliares quedan NA.")
  tibble::tibble(!!!setNames(rep(list(NA), length(columnas_atlas)), columnas_atlas))[0, ]
} else {
  atlas_demo |>
    filter(is.na(Distritos) | Distritos == "",
           is.na(Secciones) | Secciones == "",
           Periodo == as.character(max(suppressWarnings(as.integer(Periodo)),
                                       na.rm = TRUE))) |>
    mutate(CMUN = extraer_cmun_etiqueta(Municipios)) |>
    filter(!is.na(CMUN), str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV) |>
    mutate(ind_n = indicador_norm(Indicador)) |>
    filter(ind_n %in% c("edad media de la poblacion",
                        "poblacion",
                        "porcentaje de hogares unipersonales",
                        "porcentaje de poblacion menor de  anos",
                        "tamano medio del hogar")) |>
    mutate(ind_aux = case_when(
      ind_n == "edad media de la poblacion"             ~ "aux_edad_media",
      ind_n == "poblacion"                              ~ "aux_poblacion",
      ind_n == "porcentaje de hogares unipersonales"    ~ "aux_pct_unipersonales",
      ind_n == "porcentaje de poblacion menor de  anos" ~ "aux_pct_menores_18",
      ind_n == "tamano medio del hogar"                 ~ "aux_tam_hogar"
    )) |>
    mutate(valor = suppressWarnings(as.numeric(str_replace(
      as.character(Valor), ",", ".")))) |>
    select(CMUN, ind_aux, valor) |>
    distinct(CMUN, ind_aux, .keep_all = TRUE) |>
    pivot_wider(names_from = ind_aux, values_from = valor)
}

message(sprintf("  Variables auxiliares del Atlas: %d municipios.",
                nrow(aux_atlas)))


# =============================================================================
# FASE 8 — Construcción del panel municipal (542 × 12 + auxiliares)
# =============================================================================
message("[8/13] Construyendo panel municipal final...")

panel <- universo_cv |>
  left_join(ve_productivo,     by = "CMUN") |>
  left_join(ve_envejecimiento, by = "CMUN") |>
  left_join(ve_migracion,      by = "CMUN") |>
  left_join(cad_gobierto,      by = "CMUN") |>
  left_join(cad_deuda,         by = "CMUN") |>
  left_join(ve_aux_demo,       by = "CMUN") |>
  left_join(aux_atlas,         by = "CMUN")

# Identificadores de los 8 indicadores VE y 4 CAD
COLS_VE <- c("ve_lq_sector_dominante", "ve_hhi_sectorial", "ve_amplitud_estacional",
             "ve_indice_envejecimiento", "ve_indice_maternidad", "ve_indice_tendencia",
             "ve_saldo_interior_pmh",  "ve_saldo_exterior_pmh")

COLS_CAD <- c("cad_deuda_viva_pc", "cad_gasto_sss_pc",
              "cad_inversiones_pc", "cad_subvenciones_pc")

# Comprobación de cobertura
cobertura <- panel |>
  summarise(across(all_of(c(COLS_VE, COLS_CAD)), ~ sum(!is.na(.x))))
message("  Cobertura municipal por indicador:")
print(cobertura)

# Persistir el panel intermedio
write_parquet(panel, file.path(DIR_OUT, "panel_municipal.parquet"))
message(sprintf("  Panel guardado en %s", file.path(DIR_OUT, "panel_municipal.parquet")))


# =============================================================================
# FASE 9 — Estandarización + k-means K = 5 sobre los 8 indicadores VE
# =============================================================================
# Estandarización z-score sobre los 542 municipios. Se imputa la media del
# indicador para municipios con NA puntual antes del k-means (los NA suelen
# concentrarse en amplitud estacional de municipios pequeños sin datos
# mensuales completos en 2022-2024).
# =============================================================================
message("[9/13] Estandarizando VE y aplicando k-means K=5...")

X <- panel |> select(CMUN, all_of(COLS_VE))

# Diagnóstico previo: cobertura efectiva de cada indicador VE
diag_ve <- vapply(COLS_VE, function(cn) {
  v <- X[[cn]]
  c(n_no_na = sum(!is.na(v)),
    pct_no_na = round(100 * mean(!is.na(v)), 1),
    varianza = if (sum(!is.na(v)) >= 2) var(v, na.rm = TRUE) else NA_real_)
}, numeric(3))
message("  Cobertura por indicador VE (no-NA / % / varianza):")
print(t(diag_ve))

cols_problematicas <- COLS_VE[diag_ve["n_no_na", ] < 50]
if (length(cols_problematicas) > 0) {
  message("  ⚠ ALERTA: las siguientes columnas VE tienen cobertura < 50 municipios:")
  for (cn in cols_problematicas) {
    message(sprintf("    - %s: %d municipios con valor",
                    cn, diag_ve["n_no_na", cn]))
  }
  message("  Revisa las fases 2-4 antes de continuar.")
}
cols_vacias <- COLS_VE[diag_ve["n_no_na", ] == 0]
if (length(cols_vacias) > 0) {
  stop("Las siguientes columnas VE no tienen ningún valor válido: ",
       paste(cols_vacias, collapse = ", "),
       ". La imputación a la media produciría NaN y el k-means abortaría. ",
       "Revisa la fuente de origen del indicador antes de reintentar.")
}

# Imputación a la media (decisión documentada en A4); usamos mediana como
# fallback más robusto a cobertura escasa.
for (cn in COLS_VE) {
  v <- X[[cn]]
  if (sum(!is.na(v)) == 0) next
  mu <- median(v, na.rm = TRUE)
  if (!is.finite(mu)) mu <- mean(v, na.rm = TRUE)
  X[[cn]][is.na(X[[cn]])] <- mu
}

# Sanity check: no debería quedar ningún NaN/Inf tras la imputación
nan_post <- sapply(COLS_VE, function(cn) sum(!is.finite(X[[cn]])))
if (any(nan_post > 0)) {
  message("  ⚠ Tras imputar quedan valores no finitos:")
  print(nan_post[nan_post > 0])
  stop("Imputación incompleta. Revisa la cobertura de las fuentes VE.")
}

X_mat <- as.matrix(X[, COLS_VE])
X_z   <- scale(X_mat)

# Comprobación final antes del k-means
filas_distintas <- nrow(unique(X_z))
message(sprintf("  Matriz para k-means: %d municipios × %d indicadores; %d filas distintas.",
                nrow(X_z), ncol(X_z), filas_distintas))
if (filas_distintas < K_CLUSTERS) {
  stop(sprintf("Solo %d filas distintas en la matriz estandarizada (< K = %d). ",
               filas_distintas, K_CLUSTERS),
       "Probablemente una o varias columnas VE tienen muy pocos valores ",
       "originales y la imputación masiva ha colapsado la varianza. ",
       "Revisa el diagnóstico previo (cobertura por indicador VE).")
}

km <- kmeans(X_z, centers = K_CLUSTERS, nstart = 50, iter.max = 100,
             algorithm = "Hartigan-Wong")

panel$cluster_id <- km$cluster
message(sprintf("  K-means convergido. Tamaños de los 5 clústeres: %s",
                paste(table(panel$cluster_id), collapse = ", ")))


# =============================================================================
# FASE 10 — Ordenamiento de clústeres por mediana del índice compuesto de
#           severidad demográfica del clúster:
#               severidad = z(envejecimiento) + z(-maternidad) + z(-tendencia)
#           Asignación VE* equiespaciada en {0,1; 0,3; 0,5; 0,7; 0,9}.
# =============================================================================
# Decisión metodológica (mayo de 2026, irrevocable): el criterio canónico de
# ordenamiento de los cinco clústeres del k-means es la mediana del índice
# compuesto de severidad demográfica del clúster, construido como combinación
# z-score de los tres indicadores VE del eje envejecimiento: envejecimiento
# (peso +1), maternidad (peso −1, invertido porque mayor maternidad → menor
# severidad) y tendencia (peso −1, invertido porque mayor tendencia → menor
# severidad estructural). La justificación se desarrolla en A4 §app-d-arope-orden:
#   • Capturamos directamente el mecanismo PAR demográfico (regresión
#     estructural por colapso reproductivo y ausencia de relevo generacional),
#     que es la dimensión sustantiva de severidad relevante para el módulo.
#   • La circularidad parcial con el k-means (los tres componentes del criterio
#     también entran al clustering) es defendible y documentada: el clustering
#     identifica los perfiles socio-territoriales globales, y el ordenamiento
#     jerarquiza esos perfiles por su severidad en la dimensión demográfica
#     específica. La técnica es equivalente a la "asignación tipológica
#     interpretativa" de la literatura de índices compuestos territoriales.
#   • Mantiene independencia respecto a las RMEs FISABIO del plano 2 de
#     validación externa (FISABIO mide mortalidad estandarizada por edad,
#     constructo distinto del envejecimiento estructural del territorio).
# El AROPE comarcal proyectado y la renta neta media municipal se conservan en
# el panel como variables auxiliares interpretativas (no canónicas), siguiendo
# el protocolo de variables auxiliares documentado en A5 §app-e-auxiliares.
# La adopción del compuesto demográfico tras la verificación empírica de mayo
# de 2026 con AROPE y renta como criterios primarios queda documentada en A4.
# =============================================================================
message("[10/13] Ordenando clústeres por índice compuesto de severidad demográfica...")

# 10.1 — Lectura del AROPE comarcal y filtrado al año canónico.
# El fichero arope_comarcal.csv usa "," como delimitador (no ";" como otros
# CSV PEGV), con cabecera "Años","Comarcas","Grupos","Estimación y CV","Valor".
arope_raw <- read_pegv_csv(
  file.path(DIR_BASE, "arope_comarcal.csv"),
  delim = ",",
  col_names = c("anyo", "Comarca", "Grupo", "Tipo", "Valor"),
  skip = 1
)

arope_2025 <- arope_raw |>
  mutate(anyo = as.integer(anyo),
         valor = suppressWarnings(as.numeric(str_replace(
           as.character(Valor), ",", ".")))) |>
  filter(anyo == ANYO_AROPE,
         str_detect(Tipo, "(?i)estimaci"),
         !Comarca %in% c("Comunitat Valenciana",
                          "Provincia de Alicante",
                          "Provincia de Castellón",
                          "Provincia de Valencia")) |>
  select(comarca_arope = Comarca, arope = valor)

# 10.2 — Normalización de nombres de comarca para emparejar GPKG ↔ AROPE.
# Diferencias sistemáticas detectadas y resueltas por la normalización:
#   • capitalización del artículo: "el Baix" ↔ "El Baix"
#   • espacios alrededor de "/": "Maestrat/La" ↔ "Maestrat / La"
#   • apóstrofo curly/straight: "l'Alt" (GPKG) ↔ "L'Alt" (AROPE)
#   • tildes: "serranía" (GPKG) ↔ "serranos" (AROPE, sin tilde) tras alias
#   • artículo intermedio detrás de "/": "alt vinalopó/el alto" (GPKG)
#                                         ↔ "alt vinalopó/alto" (AROPE)
# El GPKG no separa L'Horta Oest de L'Horta Sud, por lo que la proyección
# agrega ambas en una sola comarca (los muns de Oest reciben el AROPE de Sud).
normalizar_comarca <- function(s) {
  s |>
    tolower() |>
    str_replace_all("’", "'") |>          # apóstrofo curly → straight
    # Translit a ASCII para igualar tildes (serranía → serrania, etc.)
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT", sub = "?") |>
    str_replace_all("\\s*/\\s*", "/") |>      # quita espacios alrededor de /
    # Quita artículos al inicio (el, la, els, les, l', los, las)
    str_replace("^(el|la|els|les|l'|los|las)\\s+", "") |>
    # Quita artículos también tras "/" (alt vinalopo/el alto vinalopo → alt vinalopo/alto vinalopo)
    str_replace("/(el|la|els|les|l'|los|las)\\s+", "/") |>
    str_trim()
}

# Alias manual mínimo para discrepancias no resolubles por normalización.
# "serrania" (GPKG, post-TRANSLIT) ↔ "serranos" (AROPE): nombres oficiales distintos.
alias_comarca <- list(
  "serrania" = "serranos"   # GPKG: La Serranía → AROPE: Los Serranos
)

aplicar_alias <- function(s) {
  vapply(s, function(x) if (x %in% names(alias_comarca)) alias_comarca[[x]] else x,
         character(1))
}

# 10.3 — Carga del mapeo municipio ↔ comarca desde el GeoPackage municipal
geom_mun_completa <- st_read(
  file.path(DIR_BASE, "..", "mallas", "malla_administrativa",
            "Delimitaciones_municipios.gpkg"),
  layer = "ICV.Municipios", quiet = TRUE
) |>
  sf::st_drop_geometry() |>
  transmute(CMUN = norm_cmun(cod_ine_mun),
            comarca_gpkg = comarca,
            comarca_key  = aplicar_alias(normalizar_comarca(comarca)))

# Comarca AROPE con la misma clave normalizada para el join
arope_join <- arope_2025 |>
  mutate(comarca_key = normalizar_comarca(comarca_arope))

# 10.4 — Proyección municipio ← AROPE comarcal
mun_arope <- geom_mun_completa |>
  left_join(arope_join, by = "comarca_key") |>
  select(CMUN, comarca = comarca_gpkg, arope)

n_sin_arope <- sum(is.na(mun_arope$arope))
if (n_sin_arope > 0) {
  comarcas_no_match <- mun_arope |>
    filter(is.na(arope)) |>
    distinct(comarca) |>
    pull(comarca)
  message(sprintf(
    "  ⚠ %d municipios sin AROPE comarcal asignado (comarcas: %s)",
    n_sin_arope, paste(comarcas_no_match, collapse = ", ")))
  message("    Se imputará la mediana CV del AROPE comarcal a esos municipios.")
  mediana_arope_cv <- median(mun_arope$arope, na.rm = TRUE)
  mun_arope <- mun_arope |>
    mutate(arope = ifelse(is.na(arope), mediana_arope_cv, arope))
}

message(sprintf("  AROPE comarcal proyectado: %d municipios con valor.",
                sum(!is.na(mun_arope$arope))))

# Eliminar columnas previas potencialmente añadidas en una ejecución anterior
# de FASE 10 (idempotencia: la fase puede re-ejecutarse sin reconstruir el panel).
panel <- panel |>
  select(-any_of(c("comarca", "arope", "renta_neta_media",
                    "mediana_arope_cluster", "mediana_arope_cluster_aux",
                    "mediana_renta_cluster_aux", "mediana_severidad_cluster",
                    "grupo_arope", "ve_star",
                    "z_envejecimiento", "z_maternidad_inv", "z_tendencia_inv",
                    "severidad_demo_mun"))) |>
  left_join(mun_arope, by = "CMUN")

# 10.5 — Renta neta media municipal (Atlas INE 31106) como variable auxiliar
# interpretativa. Se incorpora al panel para la caracterización de cluster
# en cap. 6 §sec-res-estructural pero no entra al criterio de ordenamiento.
leer_atlas_renta_robusto <- function(path) {
  df <- read_pegv_csv(path)
  candidatos_ind <- c("Indicadores de renta media", "Indicador", "Indicadores",
                       "Distribución por fuente de ingresos",
                       "Distribución de la renta por unidad de consumo")
  col_ind <- intersect(candidatos_ind, names(df))[1]
  if (is.na(col_ind)) return(NULL)
  df |>
    dplyr::rename(Indicador = !!rlang::sym(col_ind)) |>
    mutate(ind_n = indicador_norm(Indicador)) |>
    filter(ind_n == "renta neta media por persona")
}

ficheros_renta <- list.files(file.path(DIR_BASE, "atlas_renta_municipal"),
                              pattern = "\\.csv$", full.names = TRUE)

aux_renta <- purrr::map_dfr(ficheros_renta, leer_atlas_renta_robusto) |>
  filter(is.na(Distritos) | Distritos == "",
         is.na(Secciones) | Secciones == "") |>
  mutate(CMUN  = extraer_cmun_etiqueta(Municipios),
         valor = suppressWarnings(as.numeric(Total))) |>
  filter(str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV,
         as.integer(Periodo) == ANYO_RENTA) |>
  distinct(CMUN, .keep_all = TRUE) |>
  select(CMUN, renta_neta_media = valor)

panel <- panel |>
  select(-any_of("renta_neta_media")) |>
  left_join(aux_renta, by = "CMUN")

# 10.6 — Construcción del índice compuesto de severidad demográfica municipal.
# Tres z-scores estandarizados sobre los 542 municipios:
#   z(envejecimiento) → mayor envejecimiento = mayor severidad (+1)
#   z(maternidad)     → mayor maternidad = menor severidad (peso −1, invertido)
#   z(tendencia)      → mayor tendencia = mayor relevo = menor severidad (peso −1)
# Suma simple de los tres z-scores con los signos indicados.
panel <- panel |>
  mutate(
    z_envejecimiento     = as.numeric(scale(ve_indice_envejecimiento)),
    z_maternidad_inv     = -as.numeric(scale(ve_indice_maternidad)),
    z_tendencia_inv      = -as.numeric(scale(ve_indice_tendencia)),
    severidad_demo_mun   = z_envejecimiento + z_maternidad_inv + z_tendencia_inv
  )

# 10.7 — Mediana del índice de severidad demográfica por clúster y asignación VE*
orden_clusters <- panel |>
  group_by(cluster_id) |>
  summarise(
    mediana_severidad      = median(severidad_demo_mun,   na.rm = TRUE),
    mediana_envejecimiento = median(ve_indice_envejecimiento, na.rm = TRUE),
    mediana_maternidad     = median(ve_indice_maternidad,     na.rm = TRUE),
    mediana_tendencia      = median(ve_indice_tendencia,      na.rm = TRUE),
    mediana_arope_aux      = median(arope,             na.rm = TRUE),
    mediana_renta_aux      = median(renta_neta_media,  na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  # Orden descendente por severidad demográfica: el clúster con mayor severidad
  # demográfica (envejecimiento extremo + maternidad colapsada + tendencia
  # regresiva) recibe VE* = 0.9.
  arrange(desc(mediana_severidad))

orden_clusters$ve_star <- VE_STAR_VALORES |> rev()   # 0.9, 0.7, 0.5, 0.3, 0.1

message("  Mapeo cluster → VE* (severidad demográfica descendente):")
print(orden_clusters)

panel <- panel |>
  left_join(select(orden_clusters, cluster_id,
                    mediana_severidad_cluster = mediana_severidad,
                    mediana_arope_cluster_aux = mediana_arope_aux,
                    mediana_renta_cluster_aux = mediana_renta_aux,
                    ve_star),
            by = "cluster_id")


# =============================================================================
# FASE 11 — Normalización CAD: percentilización + media + min-max + inversión deuda
# =============================================================================
# La deuda viva tiene dirección invertida: mayor deuda → menor CAD. Se invierte
# la percentilización (1 − pct) antes de la agregación. Los otros tres CAD se
# percentilizan directamente.
# =============================================================================
message("[11/13] Construyendo CAD* (percentil + media + min-max)...")

panel <- panel |>
  mutate(
    pct_deuda_viva     = 1 - pct_rank(cad_deuda_viva_pc),     # dirección invertida
    pct_gasto_sss      = pct_rank(cad_gasto_sss_pc),
    pct_inversiones    = pct_rank(cad_inversiones_pc),
    pct_subvenciones   = pct_rank(cad_subvenciones_pc),
    cad_media = rowMeans(
      cbind(pct_deuda_viva, pct_gasto_sss, pct_inversiones, pct_subvenciones),
      na.rm = TRUE
    ),
    CAD_star = minmax01(cad_media)
  )


# =============================================================================
# FASE 12 — Fórmula compensadora: IVEC_estructural = ½·VE* + ½·(1 − CAD*)
# =============================================================================
message("[12/13] Calculando IVEC_estructural compensador...")

panel <- panel |>
  mutate(IVEC_estructural = round(W_VE * ve_star + W_CAD * (1 - CAD_star), 6))

message("  Distribución IVEC_estructural:")
print(summary(panel$IVEC_estructural))


# =============================================================================
# FASE 13 — Exportar parquet + GeoPackage
# =============================================================================
message("[13/13] Exportando parquet y GeoPackage del IVEC_estructural...")

write_parquet(panel, file.path(DIR_OUT, "ivec_estructural.parquet"))
message(sprintf("  Parquet: %s", file.path(DIR_OUT, "ivec_estructural.parquet")))

# Geometría municipal canónica: capa ICV.Municipios del GeoPackage del Institut
# Cartogràfic Valencià depositado en data/mallas/malla_administrativa/. La
# capa expone 542 municipios CV en EPSG:25830 con identificador `cod_ine_mun`
# de cinco dígitos. La unión con el panel se hace por CMUN tras normalizar el
# nombre del campo.
ruta_geom_mun <- here("data", "mallas", "malla_administrativa",
                      "Delimitaciones_municipios.gpkg")

if (file.exists(ruta_geom_mun)) {
  geom <- st_read(ruta_geom_mun, layer = "ICV.Municipios", quiet = TRUE) |>
    mutate(CMUN = norm_cmun(cod_ine_mun)) |>
    select(CMUN, nom_mun, comarca, provincia, area_ha)
  ivec_geo <- geom |>
    inner_join(panel, by = "CMUN")
  st_write(ivec_geo, file.path(DIR_OUT, "ivec_estructural.gpkg"),
            delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("  GeoPackage: %s (%d municipios con geometría).",
                  file.path(DIR_OUT, "ivec_estructural.gpkg"),
                  nrow(ivec_geo)))
} else {
  message("  ⚠ No se ha encontrado Delimitaciones_municipios.gpkg en ",
          "data/mallas/malla_administrativa/. El parquet del IVEC sí ",
          "se ha exportado; el GeoPackage queda pendiente.")
}

message("\n══════════════════════════════════════════════════════════════")
message("  PIPELINE IVEC_estructural COMPLETADO.")
message("══════════════════════════════════════════════════════════════")
message("  Productos en data/estructural/ivec_estructural/:")
message("    • panel_municipal.parquet   — panel de 12 indicadores + auxiliares")
message("    • ivec_estructural.parquet  — panel + cluster_id, VE*, CAD*, IVEC")
message("    • ivec_estructural.gpkg     — capa cartográfica (si geom disponible)")
message("\n  Siguiente paso: validación externa con RMEs FISABIO")
message("    Script: R/ivec/08_validacion_externa_estructural.R\n")
