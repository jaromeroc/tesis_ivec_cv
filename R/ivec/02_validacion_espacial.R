# =============================================================================
# 02_validacion_espacial.R — VALIDACIÓN ESPACIAL DEL IVEC_base
# =============================================================================
# Módulo:   Validación del IVEC_base (planos 2 y 3 de la estrategia)
# Escala:   Cuadrícula LAEA 1×1 km (EPSG:3035)
#
# Plano 2 — Validez convergente (H1):
#   Correlación de Spearman entre IVEC_base y proporción D7-alta por cuadrícula.
#   Resultado: estadístico ρ, p-valor, IC 95%, diagrama de dispersión.
#
# Plano 2b — Autocorrelación espacial global (I de Moran):
#   Confirma que la distribución del IVEC_base no es aleatoria en el espacio.
#   Resultado: I de Moran, E[I], p-valor (permutaciones).
#
# Plano 3 — Diferenciación territorial (H2) + superposición PATRICOVA:
#   Clústeres LISA (HH/LL/HL/LH), comparación con indicadores municipales
#   convencionales, y superposición con zonas de peligrosidad PATRICOVA.
#
# Entrada:
#   data/base/ivec_resultados/ivec_base.parquet   (salida de 01_ivec_base.R)
#   data/SIP/results/SIP_final.parquet            (para D7 por cuadrícula)
#   data/mallas/malla_laea/CV_grid_1km_laea.gpkg  (geometría del grid)
#   data/Literatura/GRID/PATRICOVA/               (capa riesgo inundación, si disponible)
#
# Salida:
#   data/base/ivec_resultados/validacion_d7.rds         (plano 2: correlación D7)
#   data/base/ivec_resultados/moran_global.rds           (plano 2b: I de Moran)
#   data/base/ivec_resultados/lisa_clusters.gpkg         (plano 3: clústeres LISA)
#   data/base/ivec_resultados/superposicion_patricova.rds (plano 3: PATRICOVA, si hay datos)
#   outputs/figures/cap-6/                               (figuras del cap. 6)
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(spdep)
library(here)

# ── Reproducibilidad ──────────────────────────────────────────────────────────
set.seed(1972)

# ── Directorios de salida ────────────────────────────────────────────────────
dir_out     <- here("data/base/ivec_resultados")
dir_figuras <- here("outputs/figures/cap-6")
dir.create(dir_out,     showWarnings = FALSE, recursive = TRUE)
dir.create(dir_figuras, showWarnings = FALSE, recursive = TRUE)

message("\n══════════════════════════════════════════════════════════════")
message("  02_validacion_espacial.R — inicio")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA DE DATOS
# =============================================================================

message("── Sección 1: carga de datos ──────────────────────────────────")

ruta_ivecbase <- here("data/base/ivec_resultados/ivec_base.parquet")
ruta_sip      <- here("data/SIP/results/SIP_final.parquet")
ruta_grid     <- here("data/mallas/malla_laea/CV_grid_1km_laea.gpkg")

if (!file.exists(ruta_ivecbase)) {
  stop("ivec_base.parquet no encontrado. Ejecutar primero 01_ivec_base.R.")
}

# IVEC_base por cuadrícula
ivec_base <- read_parquet(ruta_ivecbase)
message("  Cuadrículas activas cargadas: ", format(nrow(ivec_base), big.mark = "."))
message("  Columnas disponibles: ", paste(names(ivec_base), collapse = ", "))

# D7 individual desde SIP_final (misma fuente que 01_ivec_base.R)
# Garantiza que el denominador del contraste H1 usa exactamente los mismos
# individuos georreferenciados que construyeron las celdas del IVEC_base.
message("  Cargando D7 desde SIP_final.parquet...")
cols_d7 <- c("N_SIP", "D7_vulner_apsig(cod)", "D2_resid(cod)", "ST_X", "ST_Y")
sip_d7  <- read_parquet(ruta_sip, col_select = all_of(cols_d7))

# Filtrar residentes CV georreferenciados
sip_d7 <- sip_d7 |>
  filter(
    `D2_resid(cod)` == "1",
    !is.na(ST_X), !is.na(ST_Y),
    ST_X > 620000, ST_X < 900000,   # bbox corregido: incluye interior occidental de la CV
    ST_Y > 4175000, ST_Y < 4550000
  )
message("  Registros con D7 y coordenadas válidas: ",
        format(nrow(sip_d7), big.mark = "."))


# =============================================================================
# SECCIÓN 2 — ASIGNACIÓN DE D7 AL GRID (para validez convergente H1)
# =============================================================================

message("\n── Sección 2: asignación D7 → GRD_ID ─────────────────────────")

# Convertir a sf para reproyectar a LAEA
pts_utm <- st_as_sf(sip_d7,
                    coords = c("ST_X", "ST_Y"),
                    crs    = 25830,
                    remove = FALSE)
pts_laea <- st_transform(pts_utm, 3035)
coords_laea <- st_coordinates(pts_laea)

sip_d7 <- sip_d7 |>
  mutate(
    laea_N = floor(coords_laea[, 2] / 1000) * 1000L,
    laea_E = floor(coords_laea[, 1] / 1000) * 1000L,
    GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E)
  )

rm(pts_utm, pts_laea, coords_laea)

# Definir niveles "altos" de D7 según diccionario_etiquetas_sip.xlsx
# D7_vulner_apsig es una clasificación CATEGÓRICA (no ordinal):
#   0 = Sin riesgo                              ← excluido
#   1 = Desempleados                            ← vulnerabilidad moderada
#   2 = Extranjeros irregulares (titular)       ← vulnerabilidad moderada
#   3 = Sin recursos                            ← ALTA vulnerabilidad
#   4 = Indefinidos (No clasificables)          ← categoría residual, excluido
#   5 = Extranjeros irregulares                 ← ALTA vulnerabilidad
#   6 = Indefinidos Tutelados                   ← frecuencia 0.03%, excluido
#   7 = Desempleados en riesgo objetivo         ← ALTA vulnerabilidad
# Criterio: alta vulnerabilidad = 3 + 5 + 7 (misma definición que 01_ivec_base.R)
NIVELES_D7_ALTA <- c("3", "5", "7")

d7_grid <- sip_d7 |>
  rename(d7_cod = `D7_vulner_apsig(cod)`) |>
  group_by(GRD_ID) |>
  summarise(
    n_d7_total = n(),
    n_d7_alta  = sum(d7_cod %in% NIVELES_D7_ALTA, na.rm = TRUE),
    pct_D7_alta = n_d7_alta / n_d7_total,
    .groups = "drop"
  )

message("  Cuadrículas con datos D7: ", format(nrow(d7_grid), big.mark = "."))
message("  Distribución de D7 (todos niveles):")
print(table(sip_d7$`D7_vulner_apsig(cod)`, useNA = "ifany"))

rm(sip_d7)


# =============================================================================
# SECCIÓN 3 — PLANO 2: VALIDEZ CONVERGENTE (H1)
# =============================================================================

message("\n── Sección 3: validez convergente IVEC_base × D7 ──────────────")

conv_df <- ivec_base |>
  inner_join(d7_grid, by = "GRD_ID") |>
  filter(!is.na(pct_D7_alta), !is.na(ivec_base))

message("  Cuadrículas con IVEC_base y D7: ", format(nrow(conv_df), big.mark = "."))

# ── Correlaciones de Spearman ─────────────────────────────────────────────────
r_ivecbase <- cor.test(conv_df$ivec_base, conv_df$pct_D7_alta,
                       method = "spearman", exact = FALSE)
r_vi       <- cor.test(conv_df$Vi_star,   conv_df$pct_D7_alta,
                       method = "spearman", exact = FALSE)
r_ch       <- cor.test(conv_df$CH_star,   conv_df$pct_D7_alta,
                       method = "spearman", exact = FALSE)

cat(sprintf("\n  IVEC_base × D7:  ρ = %.4f  (p = %s)\n",
            r_ivecbase$estimate,
            format.pval(r_ivecbase$p.value, digits = 3, eps = 0.001)))
cat(sprintf("  Vi*     × D7:  ρ = %.4f  (p = %s)\n",
            r_vi$estimate,
            format.pval(r_vi$p.value, digits = 3, eps = 0.001)))
cat(sprintf("  CH*     × D7:  ρ = %.4f  (p = %s)\n",
            r_ch$estimate,
            format.pval(r_ch$p.value, digits = 3, eps = 0.001)))

validacion_d7 <- list(
  n_celdas    = nrow(conv_df),
  r_ivecbase  = r_ivecbase,
  r_vi        = r_vi,
  r_ch        = r_ch,
  tabla = tibble(
    Variable  = c("IVEC_base", "Vi*", "CH*"),
    rho       = c(r_ivecbase$estimate, r_vi$estimate, r_ch$estimate),
    p_valor   = c(r_ivecbase$p.value,  r_vi$p.value,  r_ch$p.value)
    # cor.test con method="spearman" no devuelve IC: conf.int es NULL
  )
)

print(validacion_d7$tabla)

# ── Diagrama de dispersión ────────────────────────────────────────────────────
p_conv <- ggplot(conv_df, aes(x = pct_D7_alta, y = ivec_base)) +
  geom_point(alpha = 0.25, size = 0.6, color = "#2c5f8a") +
  geom_smooth(method = "loess", se = TRUE,
              color = "#b0320a", linewidth = 0.8, fill = "#f4a582") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     name   = "Proporción individuos D7-alta (por cuadrícula)") +
  scale_y_continuous(name = "IVEC_base (cuadrícula)") +
  labs(
    title   = sprintf("Validez convergente IVEC_base x D7  (rho = %.3f)",
                      r_ivecbase$estimate),
    caption = sprintf("N = %s cuadrículas activas (umbral ≥ 15 individuos)",
                      format(nrow(conv_df), big.mark = "."))
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(size = 10))

ggsave(file.path(dir_figuras, "fig_convergente_ivecbase_d7.pdf"),
       p_conv, width = 12, height = 9, units = "cm")
ggsave(file.path(dir_figuras, "fig_convergente_ivecbase_d7.png"),
       p_conv, width = 12, height = 9, units = "cm", dpi = 300)
message("  Figura de dispersión guardada.")


# =============================================================================
# SECCIÓN 4 — PLANO 2b: AUTOCORRELACIÓN ESPACIAL GLOBAL (I DE MORAN)
# =============================================================================

message("\n── Sección 4: I de Moran global ───────────────────────────────")

# Cargar geometría del grid
if (!file.exists(ruta_grid)) {
  warning("CV_grid_1km_laea.gpkg no encontrado. Omitiendo análisis espacial.\n",
          "  Generar con R/descarga/16_malla_ign_ivecbase.R")
  moran_global   <- NULL
  ivec_base_sf   <- NULL
} else {
  message("  Cargando geometría del grid...")
  grid_sf <- st_read(ruta_grid, quiet = TRUE)
  message("  Celdas en el grid: ", format(nrow(grid_sf), big.mark = "."))

  # Unir IVEC_base a la geometría
  ivec_base_sf <- grid_sf |>
    inner_join(ivec_base, by = "GRD_ID") |>
    filter(!is.na(ivec_base))

  message("  Celdas activas con geometría: ",
          format(nrow(ivec_base_sf), big.mark = "."))

  # ── Matriz de vecindad queen (8 vecinos en grid regular) ─────────────────────
  message("  Construyendo matriz de vecindad (queen contiguity)...")
  nb_queen <- poly2nb(ivec_base_sf, queen = TRUE)
  lw_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

  n_sin_vecinos <- sum(card(nb_queen) == 0)
  if (n_sin_vecinos > 0)
    message("  Advertencia: ", n_sin_vecinos, " celdas sin vecinos (celdas aisladas).")

  # ── I de Moran global ─────────────────────────────────────────────────────────
  message("  Calculando I de Moran global (999 permutaciones)...")
  moran_res <- moran.mc(ivec_base_sf$ivec_base,
                        lw_queen,
                        nsim        = 999,
                        zero.policy = TRUE,
                        alternative = "greater")

  cat(sprintf(
    "\n  I de Moran: I = %.4f | E[I] = %.6f | p (permutaciones) = %.4f\n",
    moran_res$statistic,
    mean(moran_res$res),
    moran_res$p.value
  ))

  moran_global <- list(
    I       = moran_res$statistic,
    E_I     = mean(moran_res$res),
    p_value = moran_res$p.value,
    nsim    = 999,
    res_obj = moran_res
  )
}


# =============================================================================
# SECCIÓN 5 — PLANO 3: CLÚSTERES LISA
# =============================================================================

message("\n── Sección 5: clústeres LISA ───────────────────────────────────")

if (!is.null(ivec_base_sf)) {

  message("  Calculando estadísticos LISA locales...")
  lisa_res <- localmoran(ivec_base_sf$ivec_base,
                         lw_queen,
                         zero.policy = TRUE,
                         alternative = "two.sided")

  ivec_base_sf <- ivec_base_sf |>
    mutate(
      Ii      = lisa_res[, "Ii"],
      p_Ii    = lisa_res[, "Pr(z != E(Ii))"],
      sig_05  = p_Ii < 0.05,
      IVEC_z  = as.numeric(scale(ivec_base)),
      lag_z   = lag.listw(lw_queen, IVEC_z, zero.policy = TRUE),
      cuadrante = case_when(
        sig_05 & IVEC_z > 0 & lag_z > 0 ~ "HH",
        sig_05 & IVEC_z < 0 & lag_z < 0 ~ "LL",
        sig_05 & IVEC_z > 0 & lag_z < 0 ~ "HL",
        sig_05 & IVEC_z < 0 & lag_z > 0 ~ "LH",
        TRUE                             ~ "NS"
      ),
      cuadrante = factor(cuadrante, levels = c("HH","LL","HL","LH","NS"))
    )

  tabla_lisa <- ivec_base_sf |>
    st_drop_geometry() |>
    count(cuadrante) |>
    mutate(pct = round(100 * n / sum(n), 2))

  message("  Distribución de clústeres LISA:")
  print(tabla_lisa)

  # ── Mapa LISA ─────────────────────────────────────────────────────────────────
  colores_lisa <- c(
    "HH" = "#b2182b",
    "LL" = "#2166ac",
    "HL" = "#f4a582",
    "LH" = "#92c5de",
    "NS" = "#d9d9d9"
  )

  p_lisa <- ggplot(ivec_base_sf) +
    geom_sf(aes(fill = cuadrante), color = NA, size = 0) +
    scale_fill_manual(
      values = colores_lisa,
      name   = "Clúster LISA",
      labels = c("HH — co-vulner. alta",
                 "LL — co-vulner. baja",
                 "HL — outlier alto",
                 "LH — outlier bajo",
                 "NS — no significativo")
    ) +
    labs(
      title   = sprintf("Clústeres LISA del IVEC_base (I de Moran = %.4f)",
                        moran_global$I),
      caption = sprintf("N = %s cuadrículas activas · α = 0,05 · 999 permutaciones",
                        format(nrow(ivec_base_sf), big.mark = "."))
    ) +
    theme_void(base_size = 10) +
    theme(legend.position = "right",
          plot.title = element_text(size = 9))

  # Capa de delimitación administrativa (CV + provincias), común a todos los
  # mapas de la tesis. El color del mapa de prueba se mantiene; lo que se
  # unifica es la presencia del contorno autonómico y provincial.
  .ruta_cv   <- here("data/mallas/malla_administrativa/Delimitaciones_CV.gpkg")
  .ruta_prov <- here("data/mallas/malla_administrativa/Delimitaciones_provincias.gpkg")
  if (file.exists(.ruta_prov)) {
    p_lisa <- p_lisa + geom_sf(data = st_transform(st_read(.ruta_prov, quiet = TRUE),
                                                   st_crs(ivec_base_sf)),
                               fill = NA, color = "grey40", linewidth = 0.3)
  }
  if (file.exists(.ruta_cv)) {
    p_lisa <- p_lisa + geom_sf(data = st_transform(st_read(.ruta_cv, quiet = TRUE),
                                                   st_crs(ivec_base_sf)),
                               fill = NA, color = "black", linewidth = 0.5)
  }

  ggsave(file.path(dir_figuras, "fig_lisa_ivecbase.pdf"),
         p_lisa, width = 16, height = 14, units = "cm")
  ggsave(file.path(dir_figuras, "fig_lisa_ivecbase.png"),
         p_lisa, width = 16, height = 14, units = "cm", dpi = 300)
  message("  Mapa LISA guardado.")

  # Exportar gpkg con resultados LISA
  ruta_lisa_gpkg <- file.path(dir_out, "lisa_clusters.gpkg")
  st_write(
    ivec_base_sf |> select(GRD_ID, ivec_base, Vi_star, CH_star,
                            Ii, p_Ii, cuadrante),
    ruta_lisa_gpkg,
    delete_dsn = TRUE,
    quiet      = TRUE
  )
  message("  lisa_clusters.gpkg exportado.")

} else {
  message("  LISA omitido: geometría del grid no disponible.")
  tabla_lisa <- NULL
}


# =============================================================================
# SECCIÓN 6 — PLANO 3: SUPERPOSICIÓN CON PATRICOVA (si la capa está disponible)
# =============================================================================

message("\n── Sección 6: superposición PATRICOVA ─────────────────────────")

ruta_patricova_dir <- here("data/mallas/malla_patricova")
capas_patricova    <- list.files(ruta_patricova_dir,
                                 pattern = "\\.(gpkg|shp|geojson)$",
                                 recursive = TRUE,
                                 full.names = TRUE)

if (length(capas_patricova) == 0 || is.null(ivec_base_sf)) {
  message("  PATRICOVA no disponible o sin geometría. Guardando placeholder.")
  superposicion_patricova <- list(
    disponible = FALSE,
    nota       = "Capa PATRICOVA no encontrada en data/mallas/malla_patricova/."
  )
} else {
  message("  Cargando capa PATRICOVA: ", basename(capas_patricova[1]))
  patricova_sf <- st_read(capas_patricova[1], quiet = TRUE) |>
    st_transform(crs = st_crs(ivec_base_sf))

  campos_pat <- names(patricova_sf)
  message("  Campos disponibles en PATRICOVA: ", paste(campos_pat, collapse = ", "))

  # ── Clasificación de peligrosidad ────────────────────────────────────────────
  # En PATRICOVA, n_pelig va de 1 (máxima peligrosidad: frec. alta + calado alto)
  # a 6 (mínima peligrosidad hidráulica: frec. baja + calado bajo).
  # n_pelig = 7 corresponde a peligrosidad geomorfológica (vaguadas, abanicos, etc.)
  # Clasificación operativa para el análisis:
  #   Alta (1–3): frecuencia alta o media + calado relevante → prioridad máxima
  #   Media (4–5): frecuencia baja o media + calado bajo
  #   Baja hidráulica (6): frecuencia baja + calado bajo
  #   Geomorfológica (7): riesgo estructural de inundación
  if ("n_pelig" %in% campos_pat) {
    patricova_sf <- patricova_sf |>
      mutate(
        pelig_clase = case_when(
          n_pelig %in% 1:3 ~ "alta",
          n_pelig %in% 4:5 ~ "media",
          n_pelig == 6     ~ "baja_hidraulica",
          n_pelig == 7     ~ "geomorfologica",
          TRUE             ~ "sin_clasificar"
        ),
        pelig_clase = factor(pelig_clase,
                             levels = c("alta", "media",
                                        "baja_hidraulica", "geomorfologica",
                                        "sin_clasificar"))
      )
  }

  # ── Unión espacial ───────────────────────────────────────────────────────────
  # Una cuadrícula puede intersectar varios polígonos PATRICOVA de distinto nivel.
  # Se conserva el nivel más peligroso (n_pelig mínimo) por celda.
  # Las celdas sin intersección reciben n_pelig = NA (fuera de zona PATRICOVA).
  ivec_pat_raw <- st_join(
    ivec_base_sf |> select(GRD_ID, ivec_base, cuadrante),
    patricova_sf |> select(n_pelig, pelig_clase, d_pelig),
    join = st_intersects,
    left = TRUE
  ) |>
  st_drop_geometry()

  # Agregar: para cada cuadrícula, quedarse con el polígono de mayor peligrosidad
  ivec_pat <- ivec_pat_raw |>
    group_by(GRD_ID) |>
    slice_min(n_pelig, with_ties = FALSE, na_rm = TRUE) |>
    ungroup() |>
    # Las celdas sin ninguna intersección aparecen con NA: reincorporarlas
    bind_rows(
      ivec_pat_raw |>
        filter(is.na(n_pelig)) |>
        distinct(GRD_ID, .keep_all = TRUE)
    ) |>
    distinct(GRD_ID, .keep_all = TRUE)

  # Estadísticos de resumen para el capítulo
  n_en_patricova  <- sum(!is.na(ivec_pat$n_pelig))
  n_pelig_alta    <- sum(ivec_pat$pelig_clase == "alta",    na.rm = TRUE)
  n_pelig_media   <- sum(ivec_pat$pelig_clase == "media",   na.rm = TRUE)
  n_pelig_geomorf <- sum(ivec_pat$pelig_clase == "geomorfologica", na.rm = TRUE)

  message(sprintf("  Celdas en zona PATRICOVA:        %d (%.1f%%)",
                  n_en_patricova,
                  100 * n_en_patricova / nrow(ivec_pat)))
  message(sprintf("  Celdas peligrosidad alta (1–3):  %d (%.1f%%)",
                  n_pelig_alta,
                  100 * n_pelig_alta / nrow(ivec_pat)))
  message(sprintf("  Celdas peligrosidad media (4–5): %d (%.1f%%)",
                  n_pelig_media,
                  100 * n_pelig_media / nrow(ivec_pat)))
  message(sprintf("  Celdas peligrosidad geomorf (7): %d (%.1f%%)",
                  n_pelig_geomorf,
                  100 * n_pelig_geomorf / nrow(ivec_pat)))

  # Cruce HH × peligrosidad alta (el dato clave para cap. 6 y cap. 7)
  hh_pelig_alta <- ivec_pat |>
    filter(cuadrante == "HH", pelig_clase == "alta") |>
    nrow()
  message(sprintf("  Celdas HH con peligrosidad alta: %d (%.1f%% de los HH)",
                  hh_pelig_alta,
                  100 * hh_pelig_alta / sum(ivec_pat$cuadrante == "HH", na.rm = TRUE)))

  superposicion_patricova <- list(
    disponible      = TRUE,
    n_celdas        = nrow(ivec_pat),
    n_en_patricova  = n_en_patricova,
    n_pelig_alta    = n_pelig_alta,
    n_pelig_media   = n_pelig_media,
    n_pelig_geomorf = n_pelig_geomorf,
    hh_pelig_alta   = hh_pelig_alta,
    campos_pat      = campos_pat,
    datos           = ivec_pat
  )

  message("  Superposición completada: ", nrow(ivec_pat), " cuadrículas.")
}


# =============================================================================
# SECCIÓN 7 — GUARDADO DE RESULTADOS
# =============================================================================

message("\n── Sección 7: guardado de resultados ──────────────────────────")

# ── Enriquecer el objeto validacion_d7.rds creado por 01_ivec_base.R ────────
# Script 01 guarda el esqueleto (coherencia_interna, tabla_contraste_h1 pendiente).
# Script 02 añade los resultados de Spearman y actualiza tabla_contraste_h1.
# Este patrón evita que cap. 6 pierda los campos de coherencia interna al
# ejecutar el script 02.
ruta_vd7 <- file.path(dir_out, "validacion_d7.rds")
if (file.exists(ruta_vd7)) {
  vd7_existente <- readRDS(ruta_vd7)
} else {
  warning("validacion_d7.rds no encontrado — el objeto se crea desde cero.",
          "\n  Ejecutar primero 01_ivec_base.R para preservar coherencia_interna.")
  vd7_existente <- list()
}

# Añadir o actualizar campos con los resultados de Spearman
vd7_existente$plano2_spearman    <- validacion_d7$tabla
vd7_existente$r_ivecbase_spearman <- validacion_d7$r_ivecbase
vd7_existente$r_vi_spearman       <- validacion_d7$r_vi
vd7_existente$r_ch_spearman       <- validacion_d7$r_ch
vd7_existente$n_celdas_plano2     <- validacion_d7$n_celdas

# Actualizar tabla_contraste_h1 con los valores reales de Spearman (sustituye "— pendiente —")
vd7_existente$tabla_contraste_h1 <- tibble::tibble(
  Estadístico = c(
    "Correlación de Spearman (IVEC_base ~ D7_alto_prop)",
    "Vi* ~ D7_alto_prop",
    "CH* ~ D7_alto_prop",
    "p-valor IVEC_base × D7 (bilateral)",
    "Decisión sobre H1"
  ),
  Valor = c(
    as.character(round(validacion_d7$r_ivecbase$estimate, 4)),
    as.character(round(validacion_d7$r_vi$estimate,       4)),
    as.character(round(validacion_d7$r_ch$estimate,       4)),
    format.pval(validacion_d7$r_ivecbase$p.value, digits = 3, eps = 0.001),
    ifelse(validacion_d7$r_ivecbase$p.value < 0.05, "H1 SOPORTADA", "H1 no soportada")
  )
)
# Nota: cor.test con method="spearman" no devuelve IC asintóticos.
# Si se requieren IC bootstrap, usar boot::boot() sobre el vector de rangos.

saveRDS(vd7_existente, ruta_vd7)
message("  validacion_d7.rds actualizado (campos coherencia_interna preservados).")

if (!is.null(moran_global)) {
  saveRDS(moran_global,         file.path(dir_out, "moran_global.rds"))
  message("  moran_global.rds guardado.")
}

saveRDS(superposicion_patricova, file.path(dir_out, "superposicion_patricova.rds"))
message("  superposicion_patricova.rds guardado.")

# Tabla resumen de validación
resumen_validacion <- list(
  fecha_ejecucion = Sys.time(),
  n_celdas_ivecbase = nrow(ivec_base),
  plano2_convergente = validacion_d7$tabla,
  plano2b_moran = if (!is.null(moran_global)) {
    tibble(I = moran_global$I, E_I = moran_global$E_I, p = moran_global$p_value)
  } else NULL,
  plano3_lisa = tabla_lisa,
  patricova_disponible = superposicion_patricova$disponible
)
saveRDS(resumen_validacion, file.path(dir_out, "resumen_validacion.rds"))
message("  resumen_validacion.rds guardado.")

message("\n══════════════════════════════════════════════════════════════")
message("  02_validacion_espacial.R — completado")
message("══════════════════════════════════════════════════════════════\n")
