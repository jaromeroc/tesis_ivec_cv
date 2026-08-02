# =============================================================================
# 08_validacion_externa_estructural.R — VALIDACIÓN DEL PLANO 2 DEL IVEC_estructural
# =============================================================================
# Módulo:   Validación externa del IVEC_estructural (plano 2 estructural).
# Escala:   Municipal (542 municipios de la Comunitat Valenciana).
#
# NATURALEZA DEL PLANO 2 ESTRUCTURAL: VALIDEZ DISCRIMINANTE.
# A diferencia de los módulos micro (base y contextual), cuyo plano 2 es
# CONVERGENTE (D7, renta, VEUS-CV), el módulo IVEC_estructural mide un
# constructo ---regresión histórico-estructural y demográfica del municipio---
# sustantivamente DISTINTO de los criterios externos disponibles. La
# verificación empírica de mayo de 2026 sobre dos criterios externos
# independientes y reconocidos confirma esa distinción:
#
#   (1) RMEs FISABIO (mortalidad estandarizada por edad, proyecto MEDEA). La
#       mortalidad estandarizada NO converge con la vulnerabilidad estructural:
#       el exceso de mortalidad se concentra en zonas urbano-industriales,
#       mientras que la severidad estructural del módulo se concentra en el
#       interior rural despoblado y envejecido, geografías opuestas en la CV.
#       Resultado observado: ρ negativa (VE*) o nula (IVEC) — divergencia.
#
#   (2) VEUS-CV (vulnerabilidad urbana de sección, R. Temes / GVA) agregado a
#       municipio. La vulnerabilidad urbana de grano fino no converge con la
#       regresión estructural macro: resultado observado ρ ≈ 0 — ortogonalidad.
#
# La divergencia / ortogonalidad frente a AMBOS criterios NO es un fallo de
# validación: es evidencia de validez DISCRIMINANTE, coherente con R2 (los tres
# módulos del IVEC miden mecanismos causales distintos) y con H2 (el IVEC capta
# diferenciación territorial que los indicadores convencionales no recogen). El
# módulo estructural mide precisamente la dimensión que ni el criterio sanitario
# estándar ni el principal instrumento de vulnerabilidad urbana capturan. La
# ausencia de un criterio externo CONVERGENTE fuerte para el módulo macro se
# declara abiertamente como límite de V1 y agenda V2 (Apéndice D
# §app-d-clustering-metodo: incorporación de VAB, DIRCE, dependencia de transferencias).
#
# Entrada:
#   data/estructural/ivec_estructural/ivec_estructural.parquet  (salida de 07_*)
#   data/estructural/datos_RME.csv                              (RMEs FISABIO)
#   data/estructural/veus_cv/0801_VEUS.gpkg                     (VEUS-CV)
#
# Salida:
#   data/estructural/ivec_estructural/validacion_estructural_externa.rds
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(stringr)
  library(sf)
  library(here)
})

set.seed(20260519L)

PROVINCIAS_CV <- c("03", "12", "46")

ruta_ivec_estr <- here("data/estructural/ivec_estructural/ivec_estructural.parquet")
ruta_rme       <- here("data/estructural/datos_RME.csv")
ruta_veus      <- here("data/estructural/veus_cv/0801_VEUS.gpkg")
dir_out        <- here("data/estructural/ivec_estructural")

#' ρ de Spearman robusto a NA/Inf (devuelve lista rho/p_value/n).
sp_rho <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(list(rho = NA_real_, p_value = NA_real_, n = sum(ok)))
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman", exact = FALSE))
  list(rho = unname(ct$estimate), p_value = ct$p.value, n = sum(ok))
}

message("\n══════════════════════════════════════════════════════════════")
message("  08_validacion_externa_estructural.R — plano 2 (validez discriminante)")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA DEL IVEC_estructural
# =============================================================================
if (!file.exists(ruta_ivec_estr)) {
  stop("ivec_estructural.parquet no encontrado. Ejecutar primero 07_ivec_estructural.R.")
}
ivec <- read_parquet(ruta_ivec_estr) |>
  filter(!str_detect(coalesce(nombre, ""), fixed("(hasta"))) |>   # fantasma Gátova
  filter(!is.na(IVEC_estructural))
message("  Municipios IVEC_estructural: ", nrow(ivec))


# =============================================================================
# SECCIÓN 2 — CRITERIO EXTERNO 1: RMEs FISABIO (DISCRIMINANTE)
# =============================================================================
# Razones de mortalidad estandarizada suavizadas FISABIO/MEDEA. Cruce por
# código municipal de 5 dígitos. Se reportan: ρ(IVEC × RMEs), ρ(VE* × RMEs),
# diferencias de RMEs entre clústeres (Kruskal-Wallis) y la mediana de RMEs
# por clúster ordenada por VE* descendente, que evidencia el gradiente inverso.
# =============================================================================
message("\n── Sección 2: criterio externo 1 — RMEs FISABIO (discriminante) ─")
if (file.exists(ruta_rme)) {
  rme <- read_csv(ruta_rme, show_col_types = FALSE,
                  col_types = cols(codigo_muni = col_character())) |>
    mutate(CMUN = str_pad(codigo_muni, 5, pad = "0")) |>
    filter(str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV) |>
    select(CMUN, RMEs)

  base_rme <- ivec |> inner_join(rme, by = "CMUN")
  r_ivec <- sp_rho(base_rme$IVEC_estructural, base_rme$RMEs)
  r_ve   <- sp_rho(base_rme$ve_star,          base_rme$RMEs)
  kw     <- suppressWarnings(kruskal.test(RMEs ~ factor(cluster_id), data = base_rme))
  rme_por_cluster <- base_rme |>
    group_by(cluster_id) |>
    summarise(ve_star = first(ve_star), n = dplyr::n(),
              rme_mediana = median(RMEs, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(ve_star))

  message("  ρ(IVEC_estructural × RMEs) = ", round(r_ivec$rho, 4),
          " (p = ", format.pval(r_ivec$p_value, digits = 3), ", N = ", r_ivec$n, ")")
  message("  ρ(VE* × RMEs)             = ", round(r_ve$rho, 4),
          " (p = ", format.pval(r_ve$p_value, digits = 3), ")")
  message("  Kruskal-Wallis RMEs entre clústeres: H = ", round(kw$statistic, 2),
          " (p = ", format.pval(kw$p.value, digits = 3), ")")
  message("  Mediana de RMEs por clúster (VE* descendente):")
  print(rme_por_cluster)

  criterio_rme <- list(
    fuente       = "RMEs FISABIO/MEDEA (mortalidad estandarizada por edad, 2014)",
    rho_ivec     = r_ivec,
    rho_ve_star  = r_ve,
    kruskal_H    = unname(kw$statistic),
    kruskal_p    = kw$p.value,
    rme_por_cluster = rme_por_cluster,
    interpretacion = paste(
      "VALIDEZ DISCRIMINANTE. La mortalidad estandarizada por edad no converge",
      "con la vulnerabilidad estructural: el clúster de despoblamiento extremo",
      "(VE* = 0,9) presenta la RME mediana más baja y el residual diversificado",
      "(VE* = 0,1) la más alta, gradiente inverso al de VE*. La divergencia es",
      "coherente con la geografía MEDEA (exceso de mortalidad urbano-industrial)",
      "y confirma que el módulo estructural mide un constructo distinto del",
      "resultado en salud (R2).")
  )
  rm(rme, base_rme)
} else {
  message("  Criterio 1 PENDIENTE: datos_RME.csv no encontrado en ", ruta_rme)
  criterio_rme <- list(
    rho_ivec = list(rho = NA_real_, p_value = NA_real_, n = NA_integer_),
    interpretacion = "PENDIENTE: depositar datos_RME.csv en data/estructural/")
}


# =============================================================================
# SECCIÓN 3 — CRITERIO EXTERNO 2: VEUS-CV AGREGADO A MUNICIPIO (DISCRIMINANTE)
# =============================================================================
# Media del índice de vulnerabilidad urbana `iv` de VEUS por municipio. Cruce
# por código INE de 5 dígitos. Resultado esperado: ρ ≈ 0 (ortogonalidad entre
# vulnerabilidad urbana de grano fino y regresión estructural macro).
# =============================================================================
message("\n── Sección 3: criterio externo 2 — VEUS-CV municipal (discriminante) ─")
if (file.exists(ruta_veus)) {
  veus_mun <- sf::st_read(ruta_veus, layer = "TipoVulner", quiet = TRUE) |>
    sf::st_drop_geometry() |>
    mutate(CMUN = str_pad(str_extract(as.character(cod_ine), "\\d+"), 5, pad = "0"),
           iv   = suppressWarnings(as.numeric(iv))) |>
    filter(str_sub(CMUN, 1, 2) %in% PROVINCIAS_CV) |>
    group_by(CMUN) |>
    summarise(iv_municipal = mean(iv, na.rm = TRUE), .groups = "drop")

  base_veus <- ivec |> inner_join(veus_mun, by = "CMUN")
  r_ivec_v <- sp_rho(base_veus$IVEC_estructural, base_veus$iv_municipal)
  r_ve_v   <- sp_rho(base_veus$ve_star,          base_veus$iv_municipal)

  message("  ρ(IVEC_estructural × iv VEUS municipal) = ", round(r_ivec_v$rho, 4),
          " (p = ", format.pval(r_ivec_v$p_value, digits = 3), ", N = ", r_ivec_v$n, ")")
  message("  ρ(VE* × iv VEUS municipal)              = ", round(r_ve_v$rho, 4))

  criterio_veus_estr <- list(
    fuente      = "VEUS-CV (R. Temes / GVA) agregado a municipio (media del iv de sección)",
    rho_ivec    = r_ivec_v,
    rho_ve_star = r_ve_v,
    interpretacion = paste(
      "VALIDEZ DISCRIMINANTE. El índice de vulnerabilidad urbana de VEUS,",
      "agregado a municipio, es ortogonal al IVEC_estructural (ρ ≈ 0): la",
      "vulnerabilidad urbana de grano fino y la regresión estructural macro son",
      "constructos independientes. Refuerza la distinción del módulo (R2).")
  )
  rm(veus_mun, base_veus)
} else {
  message("  Criterio 2 PENDIENTE: VEUS-CV no encontrado en ", ruta_veus)
  criterio_veus_estr <- list(
    rho_ivec = list(rho = NA_real_, p_value = NA_real_, n = NA_integer_),
    interpretacion = "PENDIENTE: depositar 0801_VEUS.gpkg en data/estructural/veus_cv/")
}


# =============================================================================
# SECCIÓN 4 — CONSOLIDACIÓN Y GUARDADO
# =============================================================================
message("\n── Sección 4: consolidación ────────────────────────────────────")
validacion_estructural <- list(
  fecha_ejecucion = Sys.time(),
  n_municipios    = nrow(ivec),
  naturaleza      = paste(
    "Validez DISCRIMINANTE (no convergente): el módulo estructural mide un",
    "constructo distinto de los criterios externos disponibles."),
  criterio_rme    = criterio_rme,
  criterio_veus   = criterio_veus_estr,
  nota_limite_v1  = paste(
    "El módulo IVEC_estructural carece en V1 de un criterio externo CONVERGENTE",
    "fuerte. Los dos criterios externos disponibles (RMEs FISABIO y VEUS-CV)",
    "divergen o son ortogonales, lo que establece validez discriminante pero no",
    "convergente. Límite documentado en el Apéndice D §app-d-clustering-metodo,",
    "con agenda V2 (incorporación de VAB per cápita comarcal, DIRCE y dependencia de",
    "transferencias como ejes económico-estructurales).")
)

saveRDS(validacion_estructural, file.path(dir_out, "validacion_estructural_externa.rds"))
message("  Guardado: validacion_estructural_externa.rds")


# =============================================================================
# RESUMEN FINAL EN CONSOLA
# =============================================================================
message("\n══════════════════════════════════════════════════════════════")
message("  Validación plano 2 IVEC_estructural — resumen (validez discriminante)")
message("══════════════════════════════════════════════════════════════")
message("  RMEs FISABIO:  ρ(IVEC) = ",
        if (!is.na(criterio_rme$rho_ivec$rho)) round(criterio_rme$rho_ivec$rho, 4) else "PEND",
        " | ρ(VE*) = ",
        if (!is.null(criterio_rme$rho_ve_star)) round(criterio_rme$rho_ve_star$rho, 4) else "PEND")
message("  VEUS-CV mun.:  ρ(IVEC) = ",
        if (!is.na(criterio_veus_estr$rho_ivec$rho)) round(criterio_veus_estr$rho_ivec$rho, 4) else "PEND")
message("  Lectura: validez DISCRIMINANTE — constructo distinto (R2 / H2).")
message("  Límite V1: sin convergente externa fuerte; agenda V2.")
message("══════════════════════════════════════════════════════════════\n")
