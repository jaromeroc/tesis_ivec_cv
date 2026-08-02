# =============================================================================
# A5_pipeline_decisiones_operativas.R
# Pipeline reproducible: implementación operativa de las decisiones
# metodológicas del IVEC (asignación al grid LAEA de 1 km², agregación a celda,
# normalización dimensional, fórmula IVEC_base y análisis de sensibilidad).
#
# Tesis: Índice de Vulnerabilitat Estructural i Contextual (IVEC-CV)
# Autor: Joan A. Romero Crespo
# Corresponde a: Apéndice E (A5-decisiones-operativas.Rmd)
#
# INSTRUCCIONES DE USO:
#   1. Establecer el directorio de trabajo en la raíz del proyecto
#      (donde se encuentra Tesis-Vulnerabilidad.Rproj).
#   2. Ejecutar este script DESPUÉS de haber materializado SIP_final.parquet
#      con el pipeline reproducible R/apendices/A3_pipeline_sip.R.
#   3. Inputs esperados:
#        - data/SIP/results/SIP_final.parquet        (extracción jun. 2025)
#        - data/Literatura/GRID/CV_shapefiles/CV_grid_1km.shp
#   4. Outputs generados:
#        - data/base/ivec_base/ivec_base_celdas.parquet
#        - data/base/ivec_base/ivec_base_celdas.gpkg
#
# Este script implementa la fase E.1 del apéndice (pipeline IVEC_base). Las
# fases E.2 (decisiones de escala territorial) y E.3 (protocolos de imputación
# y datos faltantes) están documentadas en prosa en el .Rmd y se ejecutan
# desde los pipelines específicos R/ivec/04_ivec_contextual.R y
# R/ivec/07_ivec_estructural.R.
# =============================================================================

# =============================================================================
# E.1 — Pipeline completo de construcción del IVEC_base (v2 — corregida)
# Módulo: vulnerabilidad individual (Vi) × composición del hogar (CH)
# Unidad de salida: cuadrículas CV_grid_1km.shp (GRD_ID, ETRS89-LAEA EPSG:3035;
#                   shapefile almacenado en EPSG:25830 por compatibilidad cartográfica)
# Fuente: SIP_final.parquet — Conselleria de Sanitat (extracción jun. 2025)
#
# CORRECCIONES RESPECTO A LA VERSIÓN ANTERIOR:
#   [C1] Fuente: SIP_final.parquet (incluye f_nacim desde FASE 9 del pipeline SIP)
#   [C2] Filtro previo D2_resid == '1' (residentes CV únicamente)
#   [C3] Exclusión D1 sobre strings con cero inicial ('02','06',...), no enteros
#   [C4] f_nacim en formato YYYYMMDD string → as.Date(., "%Y%m%d")
#   [C5] D4_lab: códigos letra (C,B,D,...) con escala ordinal 0–3 diferenciada
#          según capacidad funcional y garantía de prestación
#   [C6] D9_raf_renta: escala ordinal 0–5 no monotónica en el código numérico;
#          el código '60' (SF-100%) no es renta alta sino exención por sin recursos
#   [C7] D3_migr '40' bifurcado: mutualistas (C3) → ord=0; resto → ord=1
#   [C8] D8_comp_res '3' (adulto solo) y '4' (monoparental) corregidos
#          a sus ordinales correctos según el diccionario verificado
#   [C9] Fórmula IVEC_base: CH entra INVERTIDA (1 − CH*) porque es capacidad
#          compensatoria — CH* alto indica mayor capacidad, no mayor vulnerabilidad
#   [C10] Unión espacial individual→polígono con CV_grid_1km.shp (GRD_ID)
#          en lugar del floor aritmético, que no es válido para un grid LAEA
#          proyectado a UTM30 (celdas no alineadas con el retículo UTM)
# =============================================================================

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(lubridate)
  library(sf)
  library(here)
})

# ── Parámetros globales ───────────────────────────────────────────────────────
FECHA_REF  <- as.Date("2025-06-30")   # fecha de extracción SIP
UMBRAL_D2  <- 15L                      # mínimo de individuos por celda (decisión D2)
W_Vi       <- 0.5                      # peso dimensión Vi en IVEC_base
W_CH       <- 0.5                      # peso dimensión CH en IVEC_base

RUTA_SIP   <- here("data/SIP/results/SIP_final.parquet")   # [C1]
RUTA_GRID  <- here("data/Literatura/GRID/CV_shapefiles/CV_grid_1km.shp")
DIR_OUT    <- here("data/base/ivec_base")
dir.create(DIR_OUT, showWarnings = FALSE, recursive = TRUE)

# Códigos institucionales a excluir (decisión D1) — strings con cero inicial [C3]
TIPOS_INSTIT <- c("02", "06", "27", "30", "31")

# =============================================================================
# PASO 0 — Carga, renombrado y filtros previos
# Los campos APSIG en SIP_final.parquet tienen el sufijo "(cod)"
# que R no puede referenciar sin backticks; se renombran en la carga.
# =============================================================================
message("[0/10] Cargando SIP_final y aplicando filtros previos...")

sip_raw <- read_parquet(RUTA_SIP) |>
  rename(
    f_nacim    = f_nacim,
    D2_resid   = `D2_resid(cod)`,
    D3_migr    = `D3_migr(cod)`,
    D4_lab     = `D4_lab(cod)`,
    D5_aseg    = `D5_aseg_subgr(cod)`,
    D7         = `D7_vulner_apsig(cod)`,
    D8_tipo    = `D8_tipo_res(cod)`,
    D8_comp    = `D8_comp_res(cod)`,
    D8_tam     = `D8_tam_res(cod)`,
    D9_renta   = `D9_raf_renta(cod)`,
    cronicidad = `cronicidad(cod)`
  ) |>
  filter(
    D2_resid == "1",                              # [C2] solo residentes CV
    !D8_tipo %in% TIPOS_INSTIT | is.na(D8_tipo)  # [C3] exclusión D1
  )

n_tras_filtros <- nrow(sip_raw)
message("  Registros tras filtros previos: ", format(n_tras_filtros, big.mark = "."))

# =============================================================================
# PASO 1 — Cálculo de edad y grupo etario
# [C4] f_nacim es string YYYYMMDD — as.Date(., "%Y%m%d"), no as.Date(.) directo
# =============================================================================
message("[1/10] Calculando edad desde f_nacim (formato YYYYMMDD)...")

sip_edad <- sip_raw |>
  mutate(
    f_nac_date = as.Date(as.character(f_nacim), format = "%Y%m%d"),
    edad_anios = as.integer(interval(f_nac_date, FECHA_REF) / years(1)),
    ord_edad   = case_when(
      edad_anios >= 0  & edad_anios <= 14 ~ 1L,   # infancia
      edad_anios >= 15 & edad_anios <= 17 ~ 2L,   # adolescencia
      edad_anios >= 18 & edad_anios <= 49 ~ 3L,   # adulto joven
      edad_anios >= 50 & edad_anios <= 64 ~ 4L,   # adulto maduro
      edad_anios >= 65 & edad_anios <= 74 ~ 5L,   # mayor
      edad_anios >= 75 & edad_anios <= 79 ~ 6L,   # mayor avanzado
      edad_anios >= 80                    ~ 7L,   # edad avanzada
      TRUE                                ~ NA_integer_
    )
  )

# =============================================================================
# PASO 2 — Codificaciones ordinales (versión corregida)
#
# Todos los campos APSIG son strings en el parquet. Ninguna comparación
# con enteros funcionará; todas las comparaciones son sobre caracteres.
# =============================================================================
message("[2/10] Aplicando escalas ordinales corregidas...")

sip_ord <- sip_edad |>
  mutate(

    # ── Vi: cronicidad (0–3, directamente ordinal; sin transformación)
    cronic_int = as.integer(cronicidad),

    # ── Vi: posición socioeconómica relativa [C6]
    # Escala 0–5: 0 = rentas más altas (menor vulnerab.) → 5 = sin recursos (máx.)
    # La escala NO es monotónica en el código numérico del campo:
    # '60' (SF-100%) = exención total por falta de recursos, no renta alta.
    D9_renta_ord = case_when(
      D9_renta %in% c("50", "53")          ~ 0L,  # AC/PN 60%: rentas más altas
      D9_renta == "40"                      ~ 1L,  # AC 50%: renta media-alta
      D9_renta == "30"                      ~ 2L,  # AC 40%: renta media (mayoritario)
      D9_renta %in% c("20", "21", "22")    ~ 3L,  # PN 10%: pensionistas renta baja
      D9_renta %in% c("10", "63")          ~ 4L,  # PN 0% / NA-40%: mínimos
      D9_renta %in% c("60", "65")          ~ 5L,  # SF-100% / NA-100%: sin recursos
      D9_renta %in% c("99", "DA") | is.na(D9_renta) ~ NA_integer_
    ),

    # ── Vi: situación laboral [C5]
    # Escala 0–3: la incapacidad permanente (B) supera en vulnerabilidad al
    # desempleo activo (D) porque implica limitación funcional permanente que
    # reduce directamente la capacidad de respuesta ante perturbaciones,
    # con independencia de la prestación económica reconocida.
    D4_lab_ord = case_when(
      D4_lab %in% c("C", "Q")             ~ 0L,  # trabaja (con/sin discapacidad)
      D4_lab %in% c("D", "E", "O")        ~ 1L,  # desempleo activo u otras sit.
      D4_lab %in% c("B", "R", "S", "T")  ~ 2L,  # incapacidad / desempleo+discap.
      D4_lab == "P"                        ~ 3L,  # incapacidad permanente+discap.
      TRUE                                 ~ NA_integer_   # 'A' (<1‰), NA, código 9
    ),

    # ── Vi: trayectoria migratoria [C7]
    # D3_migr=='40' ('Cualquier otra situación', 62.352 casos) es heterogéneo:
    #   - Subgrupo C3 (22,5%): mutualistas civiles (MUFACE/ISFAS/MUGEJU).
    #     El código '40' es artefacto del registro, no situación migratoria real.
    #     → ordinal 0 (como no migrante).
    #   - Resto (77,5%): residentes con estado migratorio no clasificable.
    #     → ordinal 1 (incertidumbre moderada, similar a migración interna).
    D3_migr_ord = case_when(
      D3_migr == "10"                         ~ 0L,  # no migrante
      D3_migr == "40" & D5_aseg == "C3"      ~ 0L,  # mutualistas → como no migrante
      D3_migr %in% c("31", "32", "33")       ~ 1L,  # migr. interna (antigua/media/reciente)
      D3_migr == "40"                         ~ 1L,  # resto '40': incertidumbre media
      D3_migr == "23"                         ~ 2L,  # extranjer. antiguo
      D3_migr == "22"                         ~ 3L,  # extranjer. estancia media
      D3_migr == "21"                         ~ 4L,  # extranjer. reciente (máx. vuln.)
      TRUE                                    ~ NA_integer_   # 'H1' (2 registros), NA
    ),

    # ── Vi: situación de aseguramiento sanitario
    # Escala 0–3: proxy de integración administrativa en el sistema de protección
    D5_aseg_ord = case_when(
      D5_aseg %in% c("A1", "A2", "A3")   ~ 0L,  # asegurados plenos SS/convenio
      D5_aseg %in% c("B2", "B4", "C3")   ~ 1L,  # cobertura subsidiaria o privada
      D5_aseg %in% c("B1", "C1", "C2")   ~ 2L,  # cobertura precaria o transitoria
      D5_aseg == "C5"                      ~ 3L,  # extranjeros irregulares (máx.)
      TRUE                                 ~ NA_integer_
    ),

    # ── Vi: interacción edad × cronicidad
    # (+1) evita la anulación del término cuando cronicidad = 0.
    # Rango resultante: 1 (infancia sin cronicidad) → 28 (≥80 años, cronicidad 3).
    edad_cronic = ord_edad * (cronic_int + 1L),

    # ── CH: composición del hogar [C8]
    # Escala 0–5: 0 = máxima capacidad redistributiva → 5 = mínima (menor sin adulto).
    # Códigos '3' (adulto solo) y '4' (adulto con menores) verificados contra
    # el diccionario: son configuraciones de vulnerabilidad alta, no media.
    D8_comp_ord = case_when(
      D8_comp %in% c("7", "8")            ~ 0L,  # >2 adultos (sin/con menores)
      D8_comp %in% c("5", "6")            ~ 1L,  # 2 adultos (sin/con menores)
      D8_comp == "3"                       ~ 2L,  # adulto solo [corregido desde VERIFICAR]
      D8_comp == "4"                       ~ 3L,  # monoparental [corregido desde VERIFICAR]
      D8_comp == "0"                       ~ 4L,  # sin unidad familiar reconocida
      D8_comp %in% c("1", "2")            ~ 5L,  # menor(es) sin adulto (máx. fragilidad)
      TRUE                                 ~ NA_integer_
    ),

    # ── CH: tamaño del hogar
    # Escala 0–3: 0 = hogar grande (mayor capacidad interna) → 3 = sin unidad
    D8_tam_ord = case_when(
      D8_tam == "3"                        ~ 0L,  # >4 personas
      D8_tam == "2"                        ~ 1L,  # 3–4 personas
      D8_tam == "1"                        ~ 2L,  # <3 personas
      D8_tam == "0"                        ~ 3L,  # sin unidad familiar
      TRUE                                 ~ NA_integer_
    )
  )

# =============================================================================
# PASO 3 — Filtro de georreferenciación y unión espacial con el grid CV [C10]
#
# La cuadrícula CV_grid_1km.shp procede de una proyección desde EPSG:3035
# (ETRS89-LAEA) a EPSG:25830 (UTM30N). Las celdas NO están alineadas con el
# retículo UTM, por lo que el floor aritmético (floor(ST_X/1000)*1000) asignaría
# incorrectamente a un número relevante de individuos en los bordes de celda.
# La unión espacial por punto-en-polígono (st_within) es el método correcto.
# =============================================================================
message("[3/10] Unión espacial individuos → grid CV (st_within)...")

sip_georref <- sip_ord |>
  filter(!is.na(ST_X), !is.na(ST_Y))

n_georref    <- nrow(sip_georref)
n_no_georref <- n_tras_filtros - n_georref
message("  Georreferenciados: ", format(n_georref, big.mark = "."),
        " | Sin coords: ", format(n_no_georref, big.mark = "."),
        " (", round(100 * n_no_georref / n_tras_filtros, 1), "%)")

sip_sf <- sip_georref |>
  st_as_sf(coords = c("ST_X", "ST_Y"), crs = 25830, remove = FALSE)

grid_sf <- st_read(RUTA_GRID, quiet = TRUE) |>
  select(GRD_ID, geometry)

# st_within: solo los individuos que caen dentro de un polígono quedan asignados.
# Los puntos en el límite de la CV que no intersecten ninguna celda se descartan.
sip_grid <- st_join(sip_sf, grid_sf, join = st_within, left = FALSE) |>
  st_drop_geometry()

message("  Asignados a celda: ", format(nrow(sip_grid), big.mark = "."),
        " | Fuera del grid CV: ",
        format(n_georref - nrow(sip_grid), big.mark = "."))

# =============================================================================
# PASO 4 — Percentilización individual (universo CV completo)
# rank() / N convierte cada ordinal en un percentil continuo en [0, 1]
# que preserva el ordenamiento relativo sin asumir equidistancia entre categorías.
# El universo es el total de individuos georreferenciados y asignados al grid,
# antes de aplicar el umbral D2 por celda.
# =============================================================================
message("[4/10] Calculando percentiles individuales (universo CV)...")

N_univ <- nrow(sip_grid)

sip_pct <- sip_grid |>
  mutate(
    # Dimensión Vi (5 indicadores) — rank directo: mayor ordinal = mayor pct = más vulnerable
    pct_cronic = rank(edad_cronic,   ties.method = "average", na.last = "keep") / N_univ,
    pct_renta  = rank(D9_renta_ord,  ties.method = "average", na.last = "keep") / N_univ,
    pct_lab    = rank(D4_lab_ord,    ties.method = "average", na.last = "keep") / N_univ,
    pct_migr   = rank(D3_migr_ord,   ties.method = "average", na.last = "keep") / N_univ,
    pct_aseg   = rank(D5_aseg_ord,   ties.method = "average", na.last = "keep") / N_univ,
    # Dimensión CH (2 indicadores) — rank INVERTIDO [C11]:
    # D8_comp_ord = 0 → hogar con máxima capacidad redistributiva (>2 adultos).
    # Con rank directo, estos individuos obtendrían la pct MÁS BAJA → CH_star bajo
    # → (1 − CH_star) alto → contribución elevada a IVEC_base. Dirección errónea.
    # La inversión (1 − rank/N) hace que el hogar más protector obtenga pct_comp = 1,
    # CH_star alto, (1 − CH_star) bajo → contribución mínima a IVEC_base. Correcto.
    # Esta inversión es consistente con la especificación de cap. 5 (§sec-agregacion-grid):
    # "valores altos de CH* indican hogares con mayor capacidad interna de redistribución".
    pct_comp   = 1 - rank(D8_comp_ord, ties.method = "average", na.last = "keep") / N_univ,
    pct_tam    = 1 - rank(D8_tam_ord,  ties.method = "average", na.last = "keep") / N_univ
  )

# =============================================================================
# PASO 5 — Agregación por celda: media de percentiles individuales
# =============================================================================
message("[5/10] Agregando percentiles por celda (GRD_ID)...")

grid_agg <- sip_pct |>
  group_by(GRD_ID) |>
  summarise(
    n_individuos = n(),
    vi_cronic    = mean(pct_cronic, na.rm = TRUE),
    vi_renta     = mean(pct_renta,  na.rm = TRUE),
    vi_lab       = mean(pct_lab,    na.rm = TRUE),
    vi_migr      = mean(pct_migr,   na.rm = TRUE),
    vi_aseg      = mean(pct_aseg,   na.rm = TRUE),
    ch_comp      = mean(pct_comp,   na.rm = TRUE),
    ch_tam       = mean(pct_tam,    na.rm = TRUE),
    .groups = "drop"
  )

message("  Celdas antes de filtro D2: ", nrow(grid_agg))

# =============================================================================
# PASO 6 — Exclusión D2: umbral mínimo de 15 individuos por celda
# Garantiza estabilidad estadística de las medias y protección de identidad.
# =============================================================================
message("[6/10] Aplicando exclusión D2 (n >= ", UMBRAL_D2, ")...")

grid_d2 <- grid_agg |>
  filter(n_individuos >= UMBRAL_D2)

message("  Celdas excluidas D2: ", nrow(grid_agg) - nrow(grid_d2),
        " | Celdas activas: ", nrow(grid_d2))

# =============================================================================
# PASO 7 — Medias dimensionales y normalización min-max sobre celdas activas
# Vi_media: media aritmética de los 5 indicadores percentilizados de Vi
# CH_media: media aritmética de los 2 indicadores percentilizados de CH
# Normalización min-max referida al conjunto de celdas activas (post-D2)
# =============================================================================
message("[7/10] Calculando Vi, CH y normalizando (min-max, celdas activas)...")

minmax <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

grid_norm <- grid_d2 |>
  mutate(
    Vi_media = rowMeans(cbind(vi_cronic, vi_renta, vi_lab, vi_migr, vi_aseg),
                        na.rm = TRUE),
    CH_media = rowMeans(cbind(ch_comp, ch_tam), na.rm = TRUE),
    Vi_star  = minmax(Vi_media),
    CH_star  = minmax(CH_media)
  )

# =============================================================================
# PASO 8 — Fórmula IVEC_base [C9]
#
# IVEC_base_i = W_Vi × Vi_i* + W_CH × (1 − CH_i*)
#
# CH entra INVERTIDA porque es una dimensión de capacidad compensatoria:
# CH_i* alto → hogar con mayor capacidad redistributiva → menor vulnerabilidad.
# La inversión (1 − CH*) convierte la capacidad en contribución a la
# vulnerabilidad neta, manteniendo coherencia con la dirección de Vi*.
# =============================================================================
message("[8/10] Calculando IVEC_base con CH invertida...")

ivec_base <- grid_norm |>
  mutate(
    IVEC_base = round(W_Vi * Vi_star + W_CH * (1 - CH_star), 6)
  ) |>
  arrange(desc(IVEC_base))

message("  Estadísticos IVEC_base:")
print(summary(ivec_base$IVEC_base))

# =============================================================================
# PASO 9 — Análisis de sensibilidad (3 especificaciones alternativas)
# Todas mantienen la inversión de CH para coherencia con el modelo base.
# =============================================================================
p75_Vi <- quantile(ivec_base$Vi_star, 0.75, na.rm = TRUE)
p75_CH <- quantile(ivec_base$CH_star, 0.75, na.rm = TRUE)
ALPHA  <- 0.10   # incremento de co-vulnerabilidad; rango ensayado: 0.05–0.20

ivec_base <- ivec_base |>
  mutate(
    # Alt. A: agregación multiplicativa (penaliza más la co-vulnerabilidad alta)
    IVEC_base_mult = sqrt(Vi_star * (1 - CH_star)),
    # Alt. B: ponderaciones asimétricas ±20 pp
    IVEC_base_w73  = 0.7 * Vi_star + 0.3 * (1 - CH_star),
    IVEC_base_w37  = 0.3 * Vi_star + 0.7 * (1 - CH_star),
    # Alt. C: término de acumulación para celdas con alta co-vulnerabilidad
    covuln_alta    = (Vi_star > p75_Vi) & (CH_star > p75_CH),
    IVEC_base_acc  = IVEC_base + ALPHA * as.integer(covuln_alta)
  )

rho_sens <- cor(
  ivec_base[, c("IVEC_base", "IVEC_base_mult", "IVEC_base_w73",
                "IVEC_base_w37", "IVEC_base_acc")],
  method = "spearman", use = "pairwise.complete.obs"
)
message("  Correlaciones de Spearman entre especificaciones:")
print(round(rho_sens, 3))

# =============================================================================
# PASO 10 — Exportar: parquet analítico + GeoPackage vinculado al grid CV
#
# El GeoPackage une los valores del IVEC_base con la geometría poligonal del
# grid CV_grid_1km.shp (EPSG:25830), produciendo un archivo directamente
# cartografiable en QGIS, R (sf) o Python (geopandas) sin unión posterior.
# La clave de vinculación entre ambos productos es GRD_ID.
# =============================================================================
message("[10/10] Exportando parquet y GeoPackage...")

tab_export <- ivec_base |>
  select(GRD_ID, n_individuos,
         vi_cronic, vi_renta, vi_lab, vi_migr, vi_aseg,
         ch_comp, ch_tam,
         Vi_media, CH_media, Vi_star, CH_star,
         IVEC_base, IVEC_base_mult, IVEC_base_w73, IVEC_base_w37, IVEC_base_acc)

ruta_pq <- file.path(DIR_OUT, "ivec_base_celdas.parquet")
write_parquet(tab_export, ruta_pq)
message("  Parquet: ", ruta_pq)

# GeoPackage: unión de resultados con geometría del grid
grid_geo <- grid_sf |>
  inner_join(tab_export, by = "GRD_ID")

ruta_gpkg <- file.path(DIR_OUT, "ivec_base_celdas.gpkg")
st_write(grid_geo, ruta_gpkg, delete_dsn = TRUE, quiet = TRUE)
message("  GeoPackage: ", ruta_gpkg)
message("  Total celdas activas IVEC_base: ", nrow(grid_geo))
message("\nCorrelaciones de Spearman entre especificaciones IVEC_base:")
print(round(rho_sens, 3))
# Una correlación ρ > 0.95 entre el modelo base y todas las alternativas
# acredita la estabilidad ordinal del instrumento (robustez a la ponderación).
