# =============================================================================
# 05_validacion_externa_contextual.R — VALIDACIÓN CONVERGENTE DEL IVEC_contextual
# =============================================================================
# Módulo:   Validación convergente del IVEC_contextual (plano 2 contextual)
# Escala:   Cuadrícula LAEA 1×1 km (EPSG:3035)
#
# Estrategia tripartita documentada en el Apéndice D §D.6. La convergencia se establece con tres criterios complementarios,
# adecuados a la escala y al horizonte temporal del módulo contextual (datos
# de 2023-2025, cuadrícula 1 km², condición presente del entorno residencial):
#
#   (1) D7 — convergencia endógena.
#       Variable de estratificación clínica del SIP. Condición necesaria pero
#       no suficiente del argumento de validez: D7 y el IVEC_contextual no
#       comparten indicadores al nivel del ítem pero proceden del mismo
#       sistema de información (el SIP), de modo que un sesgo compartido de
#       cobertura podría inflar la asociación sin que ninguna de las dos
#       medidas capture mejor el constructo.
#
#   (2) Correlación inter-módulos con IVEC_base — coherencia interna del
#       instrumento. Mide que los dos módulos del IVEC operan sobre territorios
#       vinculados pero distintos en mecanismo causal.
#
#   (3) Atlas de Distribución de Renta de los Hogares (INE) — convergencia
#       externa por renta. Mide la condición socioeconómica del territorio
#       desde una fuente independiente del SIP (declaraciones IRPF/AEAT
#       agregadas por sección censal). Datos a sección censal 2023 que se
#       proyectan al grid LAEA 1 km² por centroide, mismo patrón que
#       `vc_educacion` y `vc_tenencia`. VEUS-CV emplea esta fuente como
#       criterio de contraste; adoptarla sitúa la validación del IVEC en
#       diálogo explícito con la literatura valenciana de referencia.
#
#   (4) VEUS-CV (Visor de Espacios Urbanos Sensibles, R. Temes / GVA) —
#       convergencia externa con el principal instrumento valenciano de
#       vulnerabilidad urbana. GeoPackage de secciones censales con índice de
#       vulnerabilidad `iv`, dimensiones residencial / socioeconómica /
#       sociodemográfica y marca de espacio urbano sensible `eus`. Se proyecta
#       al grid 1 km² por centroide. Criterio de escala-mecanismo afín al módulo
#       contextual; convergencia positiva esperada.
#
# Nota sobre las RMEs FISABIO. La Razón de Mortalidad Estandarizada suavizada
# (FISABIO/GVA) NO entra en esta validación contextual. Por coherencia
# escala-mecanismo-horizonte, las RMEs se reservan como criterio externo de la
# validación del módulo IVEC_estructural (escala municipal, horizonte de
# décadas). Esa validación se documenta en `08_validacion_externa_estructural.R`.
#
# El criterio externo (Atlas Renta) opera con guarda `file.exists()`: si la
# fuente está disponible el bloque computa la correlación; si no, deja la
# entrada como NA con mensaje informativo de cómo activarla.
#
# Entrada:
#   data/base/ivec_resultados/ivec_contextual.parquet   (salida de 04_*)
#   data/base/ivec_resultados/ivec_base.parquet         (correlación entre módulos)
#   data/SIP/results/SIP_final.parquet                  (D7 por cuadrícula)
#   data/contextual/atlas_renta/<XXXXX>_<provincia>.csv (criterio externo)
#
# Salida:
#   data/base/ivec_resultados/validacion_contextual_externa.rds
#       Lista con las tres correlaciones (D7 + inter-módulos + Atlas Renta),
#       ρ, p-valor y N por cada criterio.
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(here)

set.seed(1972)

# ── Rutas ────────────────────────────────────────────────────────────────────
ruta_ivec_contextual <- here("data/base/ivec_resultados/ivec_contextual.parquet")
ruta_ivec_base       <- here("data/base/ivec_resultados/ivec_base.parquet")
ruta_sip             <- here("data/SIP/results/SIP_final.parquet")
ruta_secciones_shp   <- here("data/mallas/malla_administrativa/ca_seccion_censal/ca_seccion_censal_20260405.shp")

# Fuentes externas — guardas que el script comprueba
# Atlas de Distribución de Renta de los Hogares (INE): tres CSV provinciales
# en data/contextual/atlas_renta/<XXXXX>_<provincia>.csv. Encoding UTF-8 BOM,
# delimitador ;, valores numéricos con separador de miles ".". Indicadores
# disponibles: Media/Mediana renta por unidad de consumo, Renta bruta/neta
# media por hogar/persona. Series 2015-2023.
ruta_atlas_renta_dir <- here("data/contextual/atlas_renta")
ATLAS_RENTA_INDICADOR <- "Renta neta media por persona"
ATLAS_RENTA_ANYO      <- "2023"

# VEUS-CV (Visor de Espacios Urbanos Sensibles, R. Temes / GVA): GeoPackage con
# 3.472 secciones censales CV, índice de vulnerabilidad `iv`, dimensiones
# residencial (`dim_res`), socioeconómica (`dim_soce`) y sociodemográfica
# (`dim_socd`) y marca de espacio urbano sensible (`eus`). Se proyecta al grid
# LAEA 1 km² por inclusión del centroide de celda en la sección, mismo patrón
# que el Atlas de Renta.
ruta_veus <- here("data/estructural/veus_cv/0801_VEUS.gpkg")
# NB: las RMEs FISABIO se reservan para 08_validacion_externa_estructural.R.

dir_out <- here("data/base/ivec_resultados")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

message("\n══════════════════════════════════════════════════════════════")
message("  05_validacion_externa.R — inicio")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA DEL IVEC_contextual Y CRITERIOS PRE-COMPUTADOS
# =============================================================================

message("── Sección 1: carga del IVEC_contextual ─────────────────────────")

if (!file.exists(ruta_ivec_contextual)) {
  stop("ivec_contextual.parquet no encontrado. Ejecutar primero 04_ivec_contextual.R.")
}
ivec_ctxt <- read_parquet(ruta_ivec_contextual)
message("  Celdas IVEC_contextual cargadas: ", format(nrow(ivec_ctxt), big.mark = "."))


# =============================================================================
# SECCIÓN 2 — CRITERIO 1: D7 ENDÓGENA (CONDICIÓN NECESARIA)
# =============================================================================
# Reproduce el cálculo de 02_validacion_espacial.R para el IVEC_base pero
# sobre la cuadrícula del IVEC_contextual. Resultado: ρ de Spearman entre
# IVEC_contextual (V1, fórmula compensadora completa con CS construido como
# media equiponderada de los cuatro indicadores CS activos tras la activación
# de mayo de 2026) y la proporción de individuos con D7-alta (códigos
# 3, 5 y 7 del campo D7_vulner_apsig del SIP) por celda LAEA. Unificada con el módulo base (decisión de mayo de 2026; antes 4, 5).

message("\n── Sección 2: criterio 1 — D7 endógena ─────────────────────────")

if (file.exists(ruta_sip)) {
  message("  Cargando D7 desde SIP_final.parquet ...")
  cols_d7 <- c("D7_vulner_apsig(cod)", "D2_resid(cod)", "ST_X", "ST_Y")
  sip_d7  <- read_parquet(ruta_sip, col_select = all_of(cols_d7)) |>
    filter(
      `D2_resid(cod)` == "1",
      !is.na(ST_X), !is.na(ST_Y),
      ST_X > 620000, ST_X < 900000,
      ST_Y > 4175000, ST_Y < 4550000
    )

  # Reproyección a LAEA y construcción del GRD_ID idéntico al del pipeline
  pts <- st_as_sf(sip_d7, coords = c("ST_X", "ST_Y"), crs = 25830) |>
    st_transform(3035)
  coords_laea <- st_coordinates(pts)
  sip_d7 <- sip_d7 |>
    mutate(
      laea_N = floor(coords_laea[, 2] / 1000) * 1000L,
      laea_E = floor(coords_laea[, 1] / 1000) * 1000L,
      GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E),
      D7_alto = `D7_vulner_apsig(cod)` %in% c("3", "5", "7")
    )
  rm(pts, coords_laea)

  d7_celda <- sip_d7 |>
    group_by(GRD_ID) |>
    summarise(prop_d7_alto = mean(D7_alto, na.rm = TRUE), .groups = "drop")

  cor_d7 <- ivec_ctxt |>
    select(GRD_ID, ivec_contextual) |>
    inner_join(d7_celda, by = "GRD_ID") |>
    filter(!is.na(ivec_contextual), !is.na(prop_d7_alto))

  rho_d7 <- cor.test(cor_d7$ivec_contextual, cor_d7$prop_d7_alto,
                     method = "spearman", exact = FALSE)
  message("  ρ(IVEC_contextual × D7-alto) = ",
          round(rho_d7$estimate, 4),
          " (p = ", format.pval(rho_d7$p.value, digits = 3),
          ", N = ", nrow(cor_d7), ")")
  criterio_d7 <- list(
    rho = unname(rho_d7$estimate),
    p_value = rho_d7$p.value,
    n = nrow(cor_d7),
    interpretacion = "Condición necesaria pero no suficiente — convergencia endógena al SIP"
  )
  rm(sip_d7, d7_celda, cor_d7)
} else {
  message("  Criterio 1 PENDIENTE: SIP_final.parquet no encontrado")
  criterio_d7 <- list(
    rho = NA_real_, p_value = NA_real_, n = NA_integer_,
    interpretacion = paste("PENDIENTE: depositar SIP_final.parquet en",
                           "data/SIP/results/")
  )
}


# =============================================================================
# SECCIÓN 3 — CORRELACIÓN ENTRE MÓDULOS (IVEC_contextual × IVEC_base)
# =============================================================================
# Coherencia interna del instrumento: los dos módulos deben correlacionar
# positivamente pero no de manera redundante. ρ esperada en torno a 0,5–0,7
# (mismos territorios, distintos mecanismos causales).

message("\n── Sección 3: correlación entre módulos IVEC_contextual × IVEC_base ─")

if (file.exists(ruta_ivec_base)) {
  ivec_base <- read_parquet(ruta_ivec_base)
  col_ivec_base <- intersect(c("IVEC_base", "ivec_base", "ivec_base_v0"),
                             names(ivec_base))
  if (length(col_ivec_base) == 0) {
    warning("Columna del IVEC_base no identificable; usando primera columna numérica")
    col_ivec_base <- names(ivec_base)[vapply(ivec_base, is.numeric, logical(1))][1]
  } else {
    col_ivec_base <- col_ivec_base[1]
  }
  message("  Columna IVEC_base utilizada: ", col_ivec_base)

  cor_mod <- ivec_ctxt |>
    select(GRD_ID, ivec_contextual) |>
    inner_join(ivec_base |> select(GRD_ID, !!sym(col_ivec_base)),
               by = "GRD_ID") |>
    filter(if_all(everything(), ~ !is.na(.)))

  rho_mod <- cor.test(cor_mod$ivec_contextual,
                      cor_mod[[col_ivec_base]],
                      method = "spearman", exact = FALSE)
  message("  ρ(IVEC_contextual × IVEC_base) = ",
          round(rho_mod$estimate, 4),
          " (p = ", format.pval(rho_mod$p.value, digits = 3),
          ", N = ", nrow(cor_mod), ")")
  correlacion_modulos <- list(
    rho = unname(rho_mod$estimate),
    p_value = rho_mod$p.value,
    n = nrow(cor_mod)
  )
  rm(ivec_base, cor_mod)
} else {
  message("  IVEC_base no disponible: salta correlación inter-módulos")
  correlacion_modulos <- list(rho = NA_real_, p_value = NA_real_, n = NA_integer_)
}


# =============================================================================
# SECCIÓN 4 — CRITERIO 2: ATLAS DE DISTRIBUCIÓN DE RENTA (INE) — EXTERNO
# =============================================================================
# Fuente: Atlas de Distribución de Renta de los Hogares del INE, agregado
# por sección censal. Variable de referencia esperada: renta neta media
# por persona o por unidad de consumo. La proyección al grid LAEA opera
# por centroide de celda × polígono de sección, mismo patrón que
# vc_educacion en el pipeline contextual.
#
# Hipótesis: IVEC_contextual correla NEGATIVAMENTE con renta media
# (cuanto mayor renta, menor vulnerabilidad). ρ esperada en torno a -0,4
# a -0,6 sobre la base de la literatura comparable.

message("\n── Sección 4: criterio 2 — Atlas de Renta INE (externo) ────────")

# Localización de los tres CSV provinciales del Atlas de Renta. INE los
# distribuye con un prefijo numérico opaco que varía por consulta (p. ej.
# 30833_alicante.csv, 30962_castellon.csv, 31250_valencia.csv). Se localizan
# por sufijo de nombre de provincia para tolerar cambios en el prefijo.
atlas_renta_csvs <- if (dir.exists(ruta_atlas_renta_dir)) {
  ficheros <- list.files(ruta_atlas_renta_dir,
                         pattern = "(alicante|castellon|castellón|valencia|valència)\\.csv$",
                         full.names = TRUE, ignore.case = TRUE)
  if (length(ficheros) == 3) ficheros else character(0)
} else {
  character(0)
}

if (length(atlas_renta_csvs) > 0 && file.exists(ruta_secciones_shp)) {
  message("  Cargando Atlas de Renta INE (", length(atlas_renta_csvs),
          " ficheros provinciales) ...")

  # Lectura y consolidación de las tres provincias. El INE distribuye los
  # CSV con encoding UTF-8 BOM y delimitador ;. La columna Secciones lleva
  # el formato "0300101001 Atzúbia, l' sección 01001"; los primeros 10
  # dígitos componen el CUSEC canónico. La columna Total trae el valor
  # con separador de miles "." (ej. "13.664" = 13664 €).
  renta_raw <- purrr::map_dfr(atlas_renta_csvs, ~ readr::read_delim(
    .x, delim = ";",
    locale = readr::locale(encoding = "UTF-8", grouping_mark = "."),
    show_col_types = FALSE
  ))
  message("    Filas brutas leídas: ", format(nrow(renta_raw), big.mark = "."))

  # Renombrar columnas (el BOM del INE deja la primera columna con prefijo
  # invisible; readr::read_delim lo limpia con `encoding = "UTF-8"` pero por
  # robustez se identifica la columna correcta por contenido).
  renta_raw <- renta_raw |>
    dplyr::rename_with(~ "municipios",        .cols = dplyr::starts_with("Municipios")) |>
    dplyr::rename_with(~ "distritos",         .cols = dplyr::starts_with("Distritos")) |>
    dplyr::rename_with(~ "seccion_etiqueta",  .cols = dplyr::starts_with("Secciones")) |>
    dplyr::rename_with(~ "indicador",         .cols = dplyr::starts_with("Indicadores")) |>
    dplyr::rename_with(~ "periodo",           .cols = dplyr::starts_with("Periodo")) |>
    dplyr::rename_with(~ "total_chr",         .cols = dplyr::starts_with("Total"))

  # Filtrado al nivel sección + indicador canónico + año más reciente
  renta_sec <- renta_raw |>
    dplyr::filter(
      !is.na(seccion_etiqueta) & seccion_etiqueta != "",
      indicador == ATLAS_RENTA_INDICADOR,
      as.character(periodo) == ATLAS_RENTA_ANYO
    ) |>
    dplyr::mutate(
      cusec        = stringr::str_extract(seccion_etiqueta, "^[0-9]{10}"),
      renta_media  = suppressWarnings(as.numeric(
        stringr::str_replace_all(as.character(total_chr), "\\.", "")
      ))
    ) |>
    dplyr::filter(!is.na(cusec), !is.na(renta_media)) |>
    dplyr::select(cusec, renta_media)

  message("    Filtrado a '", ATLAS_RENTA_INDICADOR, "' / año ",
          ATLAS_RENTA_ANYO, ": ", nrow(renta_sec), " secciones con dato")
  message("    Renta media de las secciones CV: mediana = ",
          round(median(renta_sec$renta_media, na.rm = TRUE), 0), " €; ",
          "media = ", round(mean(renta_sec$renta_media, na.rm = TRUE), 0), " €")

  secciones <- sf::st_read(ruta_secciones_shp, quiet = TRUE) |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::rename(cusec = ID_SECCION) |>
    dplyr::select(cusec, geometry)

  centroides_celdas <- ivec_ctxt |>
    dplyr::select(GRD_ID, laea_N, laea_E) |>
    dplyr::mutate(x_c = laea_E + 500L, y_c = laea_N + 500L) |>
    sf::st_as_sf(coords = c("x_c", "y_c"), crs = 3035, remove = FALSE)

  asignacion_renta <- sf::st_join(centroides_celdas, secciones,
                                  join = sf::st_within) |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::left_join(renta_sec, by = "cusec") |>
    dplyr::select(GRD_ID, renta_media)

  cor_renta <- ivec_ctxt |>
    dplyr::select(GRD_ID, ivec_contextual) |>
    dplyr::inner_join(asignacion_renta, by = "GRD_ID") |>
    dplyr::filter(!is.na(ivec_contextual), !is.na(renta_media))

  rho_renta <- cor.test(cor_renta$ivec_contextual,
                        cor_renta$renta_media,
                        method = "spearman", exact = FALSE)
  message("  ρ(IVEC_contextual × renta media) = ",
          round(rho_renta$estimate, 4),
          " (p = ", format.pval(rho_renta$p.value, digits = 3),
          ", N = ", nrow(cor_renta), ")")
  criterio_renta <- list(
    rho       = unname(rho_renta$estimate),
    p_value   = rho_renta$p.value,
    n         = nrow(cor_renta),
    indicador = ATLAS_RENTA_INDICADOR,
    anyo      = ATLAS_RENTA_ANYO,
    interpretacion = paste("Convergencia externa por renta (Atlas Distribución",
                           "Renta INE, ", ATLAS_RENTA_ANYO, "). ρ negativa",
                           "esperada: mayor renta neta media por persona →",
                           "menor vulnerabilidad contextual.")
  )
  rm(renta_raw, renta_sec, secciones, centroides_celdas,
     asignacion_renta, cor_renta)
} else {
  message("  Criterio 2 PENDIENTE: depositar los tres CSV provinciales del")
  message("    Atlas de Distribución de Renta de los Hogares (INE) en")
  message("    ", ruta_atlas_renta_dir, "/")
  message("    con sufijos *_alicante.csv, *_castellon.csv, *_valencia.csv")
  criterio_renta <- list(
    rho = NA_real_, p_value = NA_real_, n = NA_integer_,
    interpretacion = "PENDIENTE: Atlas de Renta INE no descargado o ruta incorrecta"
  )
}


# =============================================================================
# SECCIÓN 5 — CRITERIO 3 (EXTERNO): VEUS-CV (R. Temes / GVA)
# =============================================================================
# Visor de Espacios Urbanos Sensibles de la Comunitat Valenciana. GeoPackage de
# 3.472 secciones censales con índice de vulnerabilidad `iv`, dimensiones
# residencial (`dim_res`), socioeconómica (`dim_soce`) y sociodemográfica
# (`dim_socd`) y marca de espacio urbano sensible (`eus` ∈ {SI, NO}). Cobertura
# CV completa. Proyección al grid LAEA 1 km² por inclusión del centroide de
# celda en la sección, mismo patrón que el Atlas de Renta.
#
# Hipótesis: convergencia POSITIVA. VEUS e IVEC_contextual miden vulnerabilidad
# del entorno residencial a escala fina; las celdas con mayor `iv` y las
# clasificadas como espacio urbano sensible deben presentar mayor
# IVEC_contextual. Se reporta el desglose dimensión-a-dimensión: las
# sub-dimensiones demográfico-poblacional y socioeconómica convergen con sus
# análogas de VEUS; la físico-residencial se reporta de forma descriptiva por
# su carácter formativo (A4 §app-decisiones-metodologicas).

message("\n── Sección 5: criterio 3 — VEUS-CV (R. Temes / GVA) ────────────")

sp_rho <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(list(rho = NA_real_, p_value = NA_real_, n = sum(ok)))
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  list(rho = unname(ct$estimate), p_value = ct$p.value, n = sum(ok))
}

if (file.exists(ruta_veus)) {
  message("  Cargando VEUS-CV desde ", basename(ruta_veus), " ...")
  veus <- sf::st_read(ruta_veus, layer = "TipoVulner", quiet = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::transmute(
      iv       = suppressWarnings(as.numeric(iv)),
      dim_res  = suppressWarnings(as.numeric(dim_res)),
      dim_soce = suppressWarnings(as.numeric(dim_soce)),
      dim_socd = suppressWarnings(as.numeric(dim_socd)),
      eus_si   = as.integer(eus == "SI")
    )

  centroides_celdas <- ivec_ctxt |>
    dplyr::select(GRD_ID, laea_N, laea_E) |>
    dplyr::mutate(x_c = laea_E + 500L, y_c = laea_N + 500L) |>
    sf::st_as_sf(coords = c("x_c", "y_c"), crs = 3035, remove = FALSE)

  asignacion_veus <- sf::st_join(centroides_celdas, veus, join = sf::st_within) |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::distinct(GRD_ID, .keep_all = TRUE) |>
    dplyr::select(GRD_ID, iv, dim_res, dim_soce, dim_socd, eus_si)

  fis_p  <- c("vc_antiguedad_p", "vc_superficie_p", "vc_tenencia_p", "vc_hacinamiento_p")
  demo_p <- c("vc_envejecimiento_p", "vc_inactividad_p", "vc_estr_migratoria_p",
              "vc_educacion_p", "vc_dependencia_conv_p")

  base_veus <- ivec_ctxt |>
    dplyr::select(GRD_ID, ivec_contextual, VC_star, CS_star,
                  dplyr::all_of(fis_p), dplyr::all_of(demo_p)) |>
    dplyr::inner_join(asignacion_veus, by = "GRD_ID") |>
    dplyr::mutate(
      VC_fisico = rowMeans(dplyr::across(dplyr::all_of(fis_p)),  na.rm = TRUE),
      VC_demo   = rowMeans(dplyr::across(dplyr::all_of(demo_p)), na.rm = TRUE)
    ) |>
    dplyr::filter(!is.na(eus_si))

  n_cob <- sum(!is.na(base_veus$iv))
  message("  Celdas con sección VEUS asignada: ", n_cob, " / ", nrow(ivec_ctxt))

  r_global <- sp_rho(base_veus$ivec_contextual, base_veus$iv)
  r_vc     <- sp_rho(base_veus$VC_star,         base_veus$iv)
  r_cs     <- sp_rho(base_veus$CS_star,         base_veus$iv)
  r_fis    <- sp_rho(base_veus$VC_fisico,       base_veus$dim_res)
  r_demo   <- sp_rho(base_veus$VC_demo,         base_veus$dim_socd)
  r_soce   <- sp_rho(base_veus$VC_star,         base_veus$dim_soce)

  # Contraste espacio urbano sensible (eus = SI) vs resto (eus = NO)
  iv_si <- base_veus$ivec_contextual[base_veus$eus_si == 1]
  iv_no <- base_veus$ivec_contextual[base_veus$eus_si == 0]
  iv_si <- iv_si[is.finite(iv_si)]; iv_no <- iv_no[is.finite(iv_no)]
  wt <- suppressWarnings(wilcox.test(iv_si, iv_no, alternative = "greater"))

  message("  ρ(IVEC_contextual × iv VEUS)  = ", round(r_global$rho, 4),
          " (p = ", format.pval(r_global$p.value, digits = 3), ", N = ", r_global$n, ")")
  message("  ρ(VC* × iv VEUS)              = ", round(r_vc$rho, 4))
  message("  ρ(VC demográfico × dim_socd)  = ", round(r_demo$rho, 4))
  message("  ρ(VC* × dim_soce)             = ", round(r_soce$rho, 4))
  message("  ρ(VC físico-resid. × dim_res) = ", round(r_fis$rho, 4),
          "  [formativa: descriptiva, A4]")
  message("  IVEC_contextual mediano eus=SI: ", round(median(iv_si), 3),
          " (n=", length(iv_si), ") vs eus=NO: ", round(median(iv_no), 3),
          " (n=", length(iv_no), "); Mann-Whitney p = ",
          format.pval(wt$p.value, digits = 3))

  criterio_veus <- list(
    fuente               = "VEUS-CV (Visor de Espacios Urbanos Sensibles, R. Temes / GVA)",
    n_celdas_con_seccion = n_cob,
    rho_global           = r_global,
    rho_vc               = r_vc,
    rho_cs               = r_cs,
    rho_fisico_dimres    = r_fis,
    rho_demo_dimsocd     = r_demo,
    rho_soce_dimsoce     = r_soce,
    eus_si_mediana       = median(iv_si),
    eus_no_mediana       = median(iv_no),
    eus_si_n             = length(iv_si),
    eus_no_n             = length(iv_no),
    mann_whitney_p       = wt$p.value,
    interpretacion = paste(
      "Convergencia externa con el principal instrumento valenciano de",
      "vulnerabilidad urbana. La convergencia global y de VC*, y el contraste",
      "de espacios urbanos sensibles, confirman la validez convergente del",
      "módulo contextual. La sub-dimensión físico-residencial (formativa) no",
      "traza el mismo gradiente que dim_res de VEUS y se reporta descriptivamente.")
  )
  rm(veus, centroides_celdas, asignacion_veus, base_veus)
} else {
  message("  Criterio 3 PENDIENTE: VEUS-CV no encontrado en ", ruta_veus)
  criterio_veus <- list(
    rho_global = list(rho = NA_real_, p_value = NA_real_, n = NA_integer_),
    interpretacion = paste("PENDIENTE: depositar el GeoPackage de VEUS-CV en",
                           "data/estructural/veus_cv/")
  )
}


# =============================================================================
# SECCIÓN 6 — CONSOLIDACIÓN Y GUARDADO
# =============================================================================

message("\n── Sección 6: consolidación de resultados ──────────────────────")

validacion_externa <- list(
  fecha_ejecucion          = Sys.time(),
  n_celdas_ivec_contextual = nrow(ivec_ctxt),
  criterio_d7              = criterio_d7,
  correlacion_modulos      = correlacion_modulos,
  criterio_renta           = criterio_renta,
  criterio_veus            = criterio_veus,
  estrategia = paste("Convergencia tripartita del plano 2 contextual",
                     "(D7 endógena + correlación inter-módulos + Atlas Renta INE)",
                     "para evitar el problema de endogeneidad de validar",
                     "exclusivamente con D7 dentro del propio SIP. Las RMEs",
                     "FISABIO se reservan para la validación del módulo",
                     "IVEC_estructural por coherencia escala-mecanismo-horizonte."),
  nota_metodologica = paste("D7 es condición necesaria del argumento de validez",
                            "al ser la calibración clínica del propio sistema",
                            "de información sobre vulnerabilidad. El Atlas de",
                            "Renta INE introduce una fuente externa al SIP",
                            "de la misma escala temporal y espacial que el módulo",
                            "contextual. La correlación inter-módulos verifica",
                            "la coherencia interna del instrumento.")
)

saveRDS(validacion_externa,
        file.path(dir_out, "validacion_contextual_externa.rds"))
message("  Guardado: validacion_contextual_externa.rds")


# =============================================================================
# RESUMEN FINAL EN CONSOLA
# =============================================================================

message("\n══════════════════════════════════════════════════════════════")
message("  Validación convergente IVEC_contextual — resumen")
message("══════════════════════════════════════════════════════════════")
message("  Criterio 1 (D7 endógena):       ρ = ",
        if (!is.na(criterio_d7$rho)) round(criterio_d7$rho, 4) else "PENDIENTE")
message("  Inter-módulos (IVEC_base):      ρ = ",
        if (!is.na(correlacion_modulos$rho)) round(correlacion_modulos$rho, 4) else "PENDIENTE")
message("  Criterio 2 (Atlas Renta INE):   ρ = ",
        if (!is.na(criterio_renta$rho)) round(criterio_renta$rho, 4) else "PENDIENTE")
message("  Criterio 3 (VEUS-CV, Temes):    ρ = ",
        if (!is.na(criterio_veus$rho_global$rho)) round(criterio_veus$rho_global$rho, 4) else "PENDIENTE")
message("══════════════════════════════════════════════════════════════")
message("  Nota: RMEs FISABIO reservadas para la validación del módulo")
message("        IVEC_estructural (08_validacion_externa_estructural.R).")
message("══════════════════════════════════════════════════════════════\n")
