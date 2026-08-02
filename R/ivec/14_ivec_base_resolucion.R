# =============================================================================
# 01_ivec_base_resolucion.R — IVEC_base parametrizado por RESOLUCIÓN y UMBRAL
# =============================================================================
# Versión de 01_ivec_base.R con dos parámetros expuestos y encadenada hasta el
# recuento de "celdas de divergencia" (Sección 4 de 11_mapas_base.R), de modo
# que una sola ejecución produce TODO: índice, parquet(s), figura y el número.
#
# Qué cambia respecto a tu 01_ivec_base.R (y NADA más):
#   · RES_M      — lado de la celda en metros. La malla y el GRD_ID se construyen
#                  con este valor (Sección 6). Todo lo anterior (indicadores,
#                  percentiles, agregación intrahogar) es idéntico e independiente
#                  de la resolución.
#   · UMBRALES   — vector de umbrales mínimos de individuos por celda. El índice
#                  se recalcula para cada umbral (la normalización min-max depende
#                  del conjunto de celdas activas, así que el umbral SÍ afecta a
#                  Vi*, CH* e ivec_base). A menor lado de celda, menos población
#                  por celda: por eso conviene reportar >1 umbral.
#
# Validación: con RES_M = 1000L y UMBRAL 15L reproduce exactamente el índice de
# la tesis y, si existe el cruce de 12_cruce_grd_municipio.R, sus 183/78.
#
# Salidas (dir_out):
#   ivec_base_<RES_M>m_u<umbral>.parquet     (uno por umbral)
#   validacion_d7_<RES_M>m_u<umbral>.rds
#   outputs/figures/cap-6/fig_divergencia_municipal_<RES_M>m_u<umbral>.pdf|png
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(here)
library(psych)

set.seed(1972)
FECHA_EXTRACCION <- as.Date("2025-06-01")

# ── PARÁMETROS DEL EXPERIMENTO ───────────────────────────────────────────────
RES_M    <- 500L              # 1000L = malla canónica; 500L = experimento 0,5 km²
UMBRALES <- c(15L, 5L)        # umbral(es) de individuos/celda. 15 = criterio tesis;
                              # 5 = capa complementaria (más celdas rurales, con
                              # advertencia de inestabilidad estadística).

# ── Rutas ────────────────────────────────────────────────────────────────────
ruta_sip_final <- here("data/SIP/results/SIP_final.parquet")
dir_out        <- here("data/base/ivec_resultados")
dir_fig        <- here("outputs/figures/cap-6")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

# Para el cruce celda->municipio del recuento de divergencia:
#   a) si RES_M==1000 y existe el cruce de 12_cruce_grd_municipio.R, se usa (=> 183/78).
#   b) si no, se construye por unión espacial contra la capa de municipios.
ruta_cruce_1km <- here("data/mallas/malla_laea/cruce_GRD_ID_seccion_municipio.rds")
CAPA_MUNI      <- here("data/mallas/malla_administrativa/Delimitaciones_municipios.gpkg")
CAMPO_CMUN     <- "CMUN"      # nombre de la columna con el código de municipio

COL_ROJO <- "#d7191c"

message("\n══════════════════════════════════════════════════════════════")
message("  IVEC_base — RES_M = ", RES_M, " m | umbrales = ", paste(UMBRALES, collapse = ", "))
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA
# =============================================================================
message("── Sección 1: carga de datos ──")
cols_final <- c(
  "UCO_CODI", "N_SIP", "f_nacim",
  "cronicidad(cod)", "D9_raf_renta(cod)", "D4_lab(cod)",
  "D3_migr(cod)", "D5_aseg_subgr(cod)",
  "empadronamiento(cod)", "D1_fin_cov(cod)",
  "D7_vulner_apsig(cod)",
  "D8_tipo_res(cod)", "D8_comp_res(cod)", "D8_tam_res(cod)",
  "D2_resid(cod)", "ST_X", "ST_Y"
)
sip <- read_parquet(ruta_sip_final, col_select = all_of(cols_final))
message("  Registros cargados: ", format(nrow(sip), big.mark = "."))


# =============================================================================
# SECCIÓN 2 — FILTROS DE INCLUSIÓN
# =============================================================================
message("\n── Sección 2: filtros de universo ──")
n_total <- nrow(sip)
sip <- sip |> filter(`D2_resid(cod)` == "1")
n_d2 <- nrow(sip)
message("  Tras D2 (residentes CV): ", format(n_d2, big.mark = "."))

sip_geo0 <- sip |>
  filter(!is.na(ST_X), !is.na(ST_Y),
         ST_X > 620000, ST_X < 900000,
         ST_Y > 4175000, ST_Y < 4550000)
n_nulos_geo  <- sum(is.na(sip$ST_X) | is.na(sip$ST_Y))
n_fuera_bbox <- nrow(sip) - n_nulos_geo - nrow(sip_geo0)
message("  Georreferenciados válidos: ", format(nrow(sip_geo0), big.mark = "."))
rm(sip_geo0)


# =============================================================================
# SECCIÓN 3 — INDICADORES Vi  (idéntico a 01_ivec_base.R)
# =============================================================================
message("\n── Sección 3: indicadores Vi ──")

# 3.1 vi_cronicidad
cronicidad_map <- c("0" = 0, "1" = 1/3, "2" = 2/3, "3" = 1)
sip <- sip |>
  mutate(vi_cronicidad = cronicidad_map[`cronicidad(cod)`],
         vi_cronicidad = replace_na(vi_cronicidad, 0))

# 3.2 vi_renta
renta_map <- c(
  "10" = 1.00, "20" = 0.90, "21" = 0.80, "22" = 0.75,
  "63" = 0.65, "65" = 0.60, "30" = 0.40, "40" = 0.30,
  "50" = 0.20, "53" = 0.18, "60" = 0.10, "64" = 0.50,
  "DA" = NA_real_, "99" = NA_real_
)
sip <- sip |>
  mutate(vi_renta = renta_map[`D9_raf_renta(cod)`],
         vi_renta = replace_na(vi_renta, median(vi_renta, na.rm = TRUE)))
mediana_renta <- median(sip$vi_renta, na.rm = TRUE)
sip <- sip |> mutate(vi_renta = if_else(is.na(vi_renta), mediana_renta, vi_renta))

# 3.3 vi_labor
labor_map <- c(
  "P" = 1.00, "R" = 0.90, "B" = 0.85, "T" = 0.75, "D" = 0.60,
  "Q" = 0.50, "E" = 0.40, "O" = 0.35, "C" = 0.10, "S" = 0.35,
  "9" = NA_real_, "0" = NA_real_, "2" = NA_real_
)
sip <- sip |> mutate(vi_labor = labor_map[`D4_lab(cod)`])
mediana_labor <- median(sip$vi_labor, na.rm = TRUE)
sip <- sip |> mutate(vi_labor = if_else(is.na(vi_labor), mediana_labor, vi_labor))

# 3.4 vi_migr
migr_map <- c(
  "21" = 1.00, "22" = 0.80, "31" = 0.70, "32" = 0.50, "23" = 0.40,
  "33" = 0.30, "40" = 0.30, "10" = 0.10,
  "H1" = NA_real_, "51" = NA_real_, "96" = NA_real_, "00" = NA_real_
)
sip <- sip |> mutate(vi_migr = migr_map[`D3_migr(cod)`])
mediana_migr <- median(sip$vi_migr, na.rm = TRUE)
sip <- sip |> mutate(vi_migr = if_else(is.na(vi_migr), mediana_migr, vi_migr))

# 3.5 vi_aseg
aseg_map <- c(
  "C5" = 1.00, "B2" = 0.90, "C1" = 0.70, "C2" = 0.65, "B4" = 0.50,
  "A1" = 0.30, "A2" = 0.20, "C3" = 0.15, "A3" = 0.10,
  "73" = NA_real_, "75" = NA_real_, "64" = NA_real_
)
sip <- sip |> mutate(vi_aseg = aseg_map[`D5_aseg_subgr(cod)`])
mediana_aseg <- median(sip$vi_aseg, na.rm = TRUE)
sip <- sip |> mutate(vi_aseg = if_else(is.na(vi_aseg), mediana_aseg, vi_aseg))

# 3.6 vi_padronal
padronal_map <- c("1" = 0.00, "2" = 1.00, "3" = 1.00)
sip <- sip |> mutate(vi_padronal = padronal_map[`empadronamiento(cod)`])
mediana_padronal <- median(sip$vi_padronal, na.rm = TRUE)
sip <- sip |> mutate(vi_padronal = if_else(is.na(vi_padronal), mediana_padronal, vi_padronal))

# 3.7 vi_d1cobertura
d1cobertura_map <- c(
  "10" = 0.00, "30" = 0.00, "40" = 0.00, "51" = 0.00,
  "20" = 0.50, "60" = 1.00, "52" = NA_real_, "99" = NA_real_
)
sip <- sip |> mutate(vi_d1cobertura = d1cobertura_map[`D1_fin_cov(cod)`])
mediana_d1cob <- median(sip$vi_d1cobertura, na.rm = TRUE)
sip <- sip |> mutate(vi_d1cobertura = if_else(is.na(vi_d1cobertura), mediana_d1cob, vi_d1cobertura))

# 3.8 vi_edad
sip <- sip |>
  mutate(
    f_nacim_date = as.Date(as.character(f_nacim), format = "%Y%m%d"),
    vi_edad_raw  = as.numeric(difftime(FECHA_EXTRACCION, f_nacim_date, units = "days")) / 365.25,
    vi_edad_raw  = if_else(vi_edad_raw < 0 | vi_edad_raw > 125, NA_real_, vi_edad_raw)
  )


# =============================================================================
# SECCIÓN 4 — INDICADORES CH  (idéntico a 01_ivec_base.R)
# =============================================================================
message("\n── Sección 4: indicadores CH ──")

# 4.1 ch_tipo_res
sip <- sip |> mutate(ch_tipo_res = if_else(`D8_tipo_res(cod)` == "01", 0.0, 1.0))

# 4.2 ch_comp_res
comp_res_map <- c(
  "1" = 1.00, "2" = 1.00, "0" = 0.95, "4" = 0.85, "3" = 0.75,
  "6" = 0.45, "8" = 0.35, "5" = 0.25, "7" = 0.10, "9" = 0.50, "O" = NA_real_
)
sip <- sip |> mutate(ch_comp_res = comp_res_map[`D8_comp_res(cod)`])
mediana_comp <- median(sip$ch_comp_res, na.rm = TRUE)
sip <- sip |> mutate(ch_comp_res = if_else(is.na(ch_comp_res), mediana_comp, ch_comp_res))

# 4.3 ch_tam_res
tam_res_map <- c("0" = 0.90, "1" = 0.70, "2" = 0.30, "3" = 0.40, "C" = NA_real_)
sip <- sip |> mutate(ch_tam_res = tam_res_map[`D8_tam_res(cod)`])
mediana_tam <- median(sip$ch_tam_res, na.rm = TRUE)
sip <- sip |> mutate(ch_tam_res = if_else(is.na(ch_tam_res), mediana_tam, ch_tam_res))

# 4.4 ch_ratio_dep
message("  Calculando ratio dependientes por UCO_CODI...")
sip_ratio <- sip |>
  select(UCO_CODI, f_nacim_date = f_nacim_date,
         cronicidad_cod = `cronicidad(cod)`, labor_cod = `D4_lab(cod)`) |>
  mutate(
    edad_calc = as.numeric(difftime(FECHA_EXTRACCION, f_nacim_date, units = "days")) / 365.25,
    es_dependiente = case_when(
      is.na(edad_calc)                                 ~ NA,
      edad_calc < 18                                   ~ TRUE,
      edad_calc >= 75 & cronicidad_cod %in% c("2","3") ~ TRUE,
      labor_cod %in% c("P","B","R")                    ~ TRUE,
      TRUE                                             ~ FALSE
    )
  ) |>
  group_by(UCO_CODI) |>
  summarise(n_miembros = n(),
            n_dependientes = sum(es_dependiente == TRUE, na.rm = TRUE),
            ratio_dep = n_dependientes / n_miembros, .groups = "drop")
sip <- sip |>
  left_join(sip_ratio |> select(UCO_CODI, ch_ratio_dep = ratio_dep), by = "UCO_CODI")
mediana_ratio <- median(sip$ch_ratio_dep, na.rm = TRUE)
sip <- sip |> mutate(ch_ratio_dep = if_else(is.na(ch_ratio_dep), mediana_ratio, ch_ratio_dep))
rm(sip_ratio)


# =============================================================================
# SECCIÓN 5 — NORMALIZACIÓN PERCENTÍLICA INDIVIDUAL  (idéntico)
# =============================================================================
message("\n── Sección 5: percentiles individuales ──")
prank <- function(x) {
  n  <- sum(!is.na(x))
  rx <- rank(x, ties.method = "average", na.last = "keep")
  (rx - 0.5) / n
}
indicadores_vi <- c("vi_cronicidad", "vi_renta", "vi_labor", "vi_migr",
                    "vi_aseg", "vi_padronal", "vi_d1cobertura", "vi_edad_raw")
indicadores_ch_basicos <- c("ch_tipo_res", "ch_comp_res", "ch_tam_res", "ch_ratio_dep")
for (ind in c(indicadores_vi, indicadores_ch_basicos)) {
  sip[[paste0(ind, "_p")]] <- prank(sip[[ind]])
}
sip <- sip |> rename(vi_edad_p = vi_edad_raw_p)
indicadores_vi_p <- c("vi_cronicidad_p", "vi_renta_p", "vi_labor_p", "vi_migr_p",
                      "vi_aseg_p", "vi_padronal_p", "vi_d1cobertura_p", "vi_edad_p")


# =============================================================================
# SECCIÓN 5.5 — CH DERIVADOS INTRAHOGAR  (idéntico)
# =============================================================================
message("\n── Sección 5.5: CH intrahogar ──")
sip <- sip |>
  mutate(vi_individual_score = (vi_cronicidad_p + vi_renta_p + vi_labor_p +
                                vi_migr_p + vi_aseg_p + vi_padronal_p +
                                vi_d1cobertura_p + vi_edad_p) / 8)
sip_hogar_vi <- sip |>
  group_by(UCO_CODI) |>
  summarise(vi_medio_hogar_raw = mean(vi_individual_score, na.rm = TRUE),
            vi_het_hogar_raw = if_else(n() > 1, sd(vi_individual_score, na.rm = TRUE), 0),
            .groups = "drop")
sip <- sip |>
  left_join(sip_hogar_vi |> select(UCO_CODI, vi_medio_hogar_raw, vi_het_hogar_raw),
            by = "UCO_CODI") |>
  mutate(vi_het_hogar_raw = replace_na(vi_het_hogar_raw, 0))
sip$ch_vi_medio_p <- prank(sip$vi_medio_hogar_raw)
sip$ch_vi_het_p   <- prank(-sip$vi_het_hogar_raw)
indicadores_ch_p <- c("ch_tipo_res_p", "ch_comp_res_p", "ch_tam_res_p",
                      "ch_ratio_dep_p", "ch_vi_medio_p", "ch_vi_het_p")
rm(sip_hogar_vi)


# =============================================================================
# SECCIÓN 6 — ASIGNACIÓN A CUADRÍCULA LAEA (PARAMETRIZADA POR RES_M)
# =============================================================================
message("\n── Sección 6: asignación a malla de ", RES_M, " m ──")
cols_para_geo <- c("UCO_CODI", "N_SIP", "ST_X", "ST_Y",
                   "D7_vulner_apsig(cod)", indicadores_vi_p, indicadores_ch_p)
sip_geo <- sip |>
  filter(!is.na(ST_X), !is.na(ST_Y),
         ST_X > 620000, ST_X < 900000,
         ST_Y > 4175000, ST_Y < 4550000) |>
  select(all_of(cols_para_geo))

pts_laea <- st_as_sf(sip_geo, coords = c("ST_X", "ST_Y"), crs = 25830, remove = FALSE) |>
  st_transform(3035)
coords_laea <- st_coordinates(pts_laea)
sip_geo <- sip_geo |>
  mutate(
    laea_N = floor(coords_laea[, 2] / RES_M) * RES_M,   # <- RES_M (antes 1000)
    laea_E = floor(coords_laea[, 1] / RES_M) * RES_M,   # <- RES_M (antes 1000)
    GRD_ID = paste0("CRS3035RES", RES_M, "mN", laea_N, "E", laea_E)  # <- RES_M
  )
rm(pts_laea, coords_laea)
message("  Celdas únicas antes del umbral: ",
        format(n_distinct(sip_geo$GRD_ID), big.mark = "."))


# =============================================================================
# SECCIÓN 7 — AGREGACIÓN A CELDA (medias por celda; sin umbral aún)
# =============================================================================
message("\n── Sección 7: agregación a celda ──")
sip_celda_all <- sip_geo |>
  group_by(GRD_ID, laea_N, laea_E) |>
  summarise(
    n_individuos = n(),
    vi_cronicidad  = mean(vi_cronicidad_p,  na.rm = TRUE),
    vi_renta       = mean(vi_renta_p,       na.rm = TRUE),
    vi_labor       = mean(vi_labor_p,       na.rm = TRUE),
    vi_migr        = mean(vi_migr_p,        na.rm = TRUE),
    vi_aseg        = mean(vi_aseg_p,        na.rm = TRUE),
    vi_padronal    = mean(vi_padronal_p,    na.rm = TRUE),
    vi_d1cobertura = mean(vi_d1cobertura_p, na.rm = TRUE),
    vi_edad        = mean(vi_edad_p,        na.rm = TRUE),
    ch_tipo_res    = mean(ch_tipo_res_p,    na.rm = TRUE),
    ch_comp_res    = mean(ch_comp_res_p,    na.rm = TRUE),
    ch_tam_res     = mean(ch_tam_res_p,     na.rm = TRUE),
    ch_ratio_dep   = mean(ch_ratio_dep_p,   na.rm = TRUE),
    ch_vi_medio    = mean(ch_vi_medio_p,    na.rm = TRUE),
    ch_vi_het      = mean(ch_vi_het_p,      na.rm = TRUE),
    d7_alto_prop   = mean(`D7_vulner_apsig(cod)` %in% c("3","5","7"), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Vi_celda = (vi_cronicidad + vi_renta + vi_labor + vi_migr + vi_aseg +
                vi_padronal + vi_d1cobertura + vi_edad) / 8,
    CH_celda = 1 - (ch_tipo_res + ch_comp_res + ch_tam_res +
                    ch_ratio_dep + ch_vi_medio + ch_vi_het) / 6
  )


# ── Funciones auxiliares para el bloque por-umbral ────────────────────────────
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

construir_grid <- function(df, res, crs = 3035) {
  df |>
    distinct(GRD_ID, laea_N, laea_E) |>
    rowwise() |>
    mutate(geometry = list(st_polygon(list(matrix(
      c(laea_E, laea_N,  laea_E + res, laea_N,  laea_E + res, laea_N + res,
        laea_E, laea_N + res,  laea_E, laea_N),
      ncol = 2, byrow = TRUE))))) |>
    ungroup() |>
    st_as_sf(crs = crs)
}

# Cruce celda->municipio (una vez por RES_M): rds existente si procede, o unión espacial.
obtener_cruce <- function(sip_celda, res) {
  if (res == 1000 && file.exists(ruta_cruce_1km)) {
    message("  Cruce: usando ", basename(ruta_cruce_1km))
    return(readRDS(ruta_cruce_1km) |> distinct(GRD_ID, .keep_all = TRUE) |> select(GRD_ID, CMUN))
  }
  if (!file.exists(CAPA_MUNI))
    stop("Falta la capa de municipios para el cruce: ", CAPA_MUNI)
  message("  Cruce: unión espacial (centroide de celda -> municipio)")
  muni <- st_read(CAPA_MUNI, quiet = TRUE) |> st_transform(3035)
  muni <- muni[, CAMPO_CMUN] |> rename(CMUN = all_of(CAMPO_CMUN))
  grid <- construir_grid(sip_celda, res)
  st_join(st_centroid(grid), muni, join = st_within) |>
    st_drop_geometry() |> distinct(GRD_ID, .keep_all = TRUE) |> select(GRD_ID, CMUN)
}


# =============================================================================
# SECCIONES 8–12 + DIVERGENCIA, POR CADA UMBRAL
# =============================================================================
resumen_final <- list()

for (UMBRAL_CELDA in UMBRALES) {
  tag <- sprintf("%dm_u%d", RES_M, UMBRAL_CELDA)
  message("\n=============================================================")
  message("  UMBRAL = ", UMBRAL_CELDA, " individuos/celda   (", tag, ")")
  message("=============================================================")

  # Sección 7 (umbral) + 8 (min-max) + 9 (fórmula) --------------------------
  sip_celda <- sip_celda_all |> filter(n_individuos >= UMBRAL_CELDA)
  message("  Celdas activas tras el umbral: ", format(nrow(sip_celda), big.mark = "."))

  sip_celda <- sip_celda |>
    mutate(Vi_star = minmax(Vi_celda),
           CH_star = minmax(CH_celda),
           ivec_base = 0.5 * Vi_star + 0.5 * (1 - CH_star))
  message("  IVEC_base: media = ", round(mean(sip_celda$ivec_base), 3),
          " ; dt = ", round(sd(sip_celda$ivec_base), 3))

  # Sección 10 — coherencia interna (alfa/omega) ----------------------------
  mat_vi <- sip_celda |>
    select(vi_cronicidad, vi_renta, vi_labor, vi_migr, vi_aseg,
           vi_padronal, vi_d1cobertura, vi_edad) |> as.matrix()
  mat_ch <- sip_celda |>
    select(ch_tipo_res, ch_comp_res, ch_tam_res, ch_ratio_dep,
           ch_vi_medio, ch_vi_het) |> as.matrix()
  alpha_vi <- tryCatch(psych::alpha(mat_vi, check.keys = FALSE)$total$raw_alpha, error = function(e) NA_real_)
  alpha_ch <- tryCatch(psych::alpha(mat_ch, check.keys = FALSE)$total$raw_alpha, error = function(e) NA_real_)
  cor_ivec_d7 <- cor(sip_celda$ivec_base, sip_celda$d7_alto_prop, use = "complete.obs")
  message("  Vi: alfa = ", round(alpha_vi, 3), " | CH: alfa = ", round(alpha_ch, 3),
          " | cor(ivec, d7) = ", round(cor_ivec_d7, 3))

  # Sección 12 — guardar índice + validación --------------------------------
  ruta_parquet_out <- file.path(dir_out, paste0("ivec_base_", tag, ".parquet"))
  write_parquet(sip_celda, ruta_parquet_out)
  saveRDS(list(alpha_vi = alpha_vi, alpha_ch = alpha_ch, cor_ivec_d7 = cor_ivec_d7,
               res_m = RES_M, umbral = UMBRAL_CELDA, n_celdas = nrow(sip_celda)),
          file.path(dir_out, paste0("validacion_d7_", tag, ".rds")))
  message("  Guardado: ", basename(ruta_parquet_out))

  # ── DIVERGENCIA (Sección 4 de 11_mapas_base.R), a esta resolución ─────────
  cruce <- obtener_cruce(sip_celda, RES_M)
  breaks_ivec <- quantile(sip_celda$ivec_base, probs = seq(0, 1, 0.2), na.rm = TRUE)
  div_df <- sip_celda |>
    select(GRD_ID, ivec_base) |>
    left_join(cruce, by = "GRD_ID")
  div_df$q_celda <- as.integer(cut(div_df$ivec_base, breaks = breaks_ivec,
                                   include.lowest = TRUE, labels = 1:5))
  mun_agg <- div_df |> filter(!is.na(CMUN)) |> group_by(CMUN) |>
    summarise(ivec_mun = mean(ivec_base, na.rm = TRUE), .groups = "drop")
  br_mun <- quantile(mun_agg$ivec_mun, probs = seq(0, 1, 0.2), na.rm = TRUE)
  mun_agg$q_mun <- as.integer(cut(mun_agg$ivec_mun, breaks = br_mun,
                                  include.lowest = TRUE, labels = 1:5))
  div_df <- left_join(div_df, select(mun_agg, CMUN, q_mun), by = "CMUN")
  div_df$divergencia <- !is.na(div_df$q_celda) & !is.na(div_df$q_mun) &
    div_df$q_celda >= 4L & div_df$q_mun <= 2L
  n_div     <- sum(div_df$divergencia, na.rm = TRUE)
  n_mun_div <- length(unique(div_df$CMUN[div_df$divergencia]))

  message("  Celdas de divergencia (celda Q4-Q5 en municipio Q1-Q2): ",
          format(n_div, big.mark = "."), "  en ", n_mun_div, " municipios")

  # Figura
  grid_geom <- construir_grid(sip_celda, RES_M)
  div_sf <- left_join(grid_geom, select(div_df, GRD_ID, divergencia), by = "GRD_ID")
  p <- ggplot(div_sf) +
    geom_sf(aes(fill = divergencia), color = NA) +
    scale_fill_manual(values = c(`TRUE` = COL_ROJO, `FALSE` = "grey85"),
                      labels = c(`TRUE` = "Divergencia", `FALSE` = "Resto"),
                      name = NULL, na.value = "grey85") +
    theme_void(base_size = 11) + theme(legend.position = "bottom") +
    labs(title = paste0("Celdas de divergencia — malla ", RES_M, " m (umbral ", UMBRAL_CELDA, ")"),
         subtitle = paste0(n_div, " celdas en ", n_mun_div, " municipios"))
  ggsave(file.path(dir_fig, paste0("fig_divergencia_municipal_", tag, ".pdf")), p, width = 7, height = 8)
  ggsave(file.path(dir_fig, paste0("fig_divergencia_municipal_", tag, ".png")), p, dpi = 300, width = 7, height = 8, bg = "white")

  resumen_final[[tag]] <- data.frame(res_m = RES_M, umbral = UMBRAL_CELDA,
                                     n_celdas = nrow(sip_celda), n_div = n_div, n_mun = n_mun_div)
}

# =============================================================================
# RESUMEN
# =============================================================================
message("\n══════════════════════════════════════════════════════════════")
message("  RESUMEN — RES_M = ", RES_M, " m")
res_tab <- do.call(rbind, resumen_final)
print(res_tab, row.names = FALSE)
message("══════════════════════════════════════════════════════════════\n")
