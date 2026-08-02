# =============================================================================
# 03_analisis_sensibilidad.R — ANÁLISIS DE SENSIBILIDAD DEL IVEC_base
# =============================================================================
# Módulo:   Sensibilidad y robustez del IVEC_base (R6)
# Escala:   Cuadrícula LAEA 1×1 km (EPSG:3035)
#
# Implementa las tres especificaciones alternativas descritas en §sec-sensibilidad
# del capítulo quinto:
#
#   Especificación 0 (base):
#     ivec_base   = 0.5 · Vi* + 0.5 · (1 − CH*)
#
#   Especificación 1 — Operador multiplicativo (complementariedad estricta):
#     ivec_mult   = sqrt(Vi* · (1 − CH*))
#
#   Especificación 2a — Pesos 0.7 / 0.3 (mayor peso en fragilidad individual):
#     ivec_w70    = 0.7 · Vi* + 0.3 · (1 − CH*)
#
#   Especificación 2b — Pesos 0.3 / 0.7 (mayor peso en ausencia de capacidad):
#     ivec_w30    = 0.3 · Vi* + 0.7 · (1 − CH*)
#
#   Especificación 3 — Término de acumulación dimensional (α = 0.10):
#     ivec_acum   = ivec_base + α · I[Vi* > p75(Vi*)] · I[(1−CH*) > p75(1−CH*)]
#     donde α se explora en {0.05, 0.10, 0.20}
#
# Para cada especificación se calcula la clasificación en quintiles.
# El análisis de concordancia identifica las celdas cuyo quintil varía entre
# especificaciones (mapa de incertidumbre diagnóstica).
#
# Entrada:
#   data/base/ivec_resultados/ivec_base.parquet       (salida de 01_ivec_base.R)
#   data/mallas/malla_laea/CV_grid_1km_laea.gpkg      (geometría del grid)
#
# Salida:
#   data/base/ivec_resultados/sensibilidad_tabla.parquet   — tabla de celdas ×
#                                                            especificaciones
#   data/base/ivec_resultados/sensibilidad_spearman.rds    — matriz Spearman
#   data/base/ivec_resultados/sensibilidad_incertidumbre.gpkg — mapa incertidumbre
#   outputs/figures/cap-6/                                  — figuras del cap. 6
#
# Secuencia del pipeline IVEC:
#   01_ivec_base.R → 02_validacion_espacial.R → [03_analisis_sensibilidad.R]
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(here)

# ── Reproducibilidad ──────────────────────────────────────────────────────────
set.seed(1972)

# ── Directorios ───────────────────────────────────────────────────────────────
dir_out     <- here("data/base/ivec_resultados")
dir_figuras <- here("outputs/figures/cap-6")
dir.create(dir_out,     showWarnings = FALSE, recursive = TRUE)
dir.create(dir_figuras, showWarnings = FALSE, recursive = TRUE)

message("\n══════════════════════════════════════════════════════════════")
message("  03_analisis_sensibilidad.R — inicio")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA DE DATOS
# =============================================================================

message("── Sección 1: carga de ivec_base.parquet ───────────────────────")

ivec <- read_parquet(here("data/base/ivec_resultados/ivec_base.parquet"))

message("  Celdas activas cargadas: ", format(nrow(ivec), big.mark = "."))
message("  Columnas disponibles: ", paste(names(ivec), collapse = ", "))

# Verificar columnas necesarias
cols_req <- c("GRD_ID", "Vi_star", "CH_star", "ivec_base")
if (!all(cols_req %in% names(ivec))) {
  stop("Columnas requeridas no encontradas: ",
       paste(setdiff(cols_req, names(ivec)), collapse = ", "))
}


# =============================================================================
# SECCIÓN 2 — ESPECIFICACIONES ALTERNATIVAS
# =============================================================================

message("\n── Sección 2: cálculo de especificaciones alternativas ──────────")

# Parámetros del término de acumulación (explorados en §sec-sensibilidad)
ALPHA_VALORES <- c(0.05, 0.10, 0.20)
ALPHA_BASE    <- 0.10   # valor de referencia para el mapa de incertidumbre

# Percentiles p75 para el término de acumulación
p75_vi   <- quantile(ivec$Vi_star,        0.75, na.rm = TRUE)
p75_1mch <- quantile(1 - ivec$CH_star,    0.75, na.rm = TRUE)

message("  p75(Vi*)       = ", round(p75_vi, 4))
message("  p75(1 − CH*)   = ", round(p75_1mch, 4))

ivec <- ivec |>
  mutate(
    # ── Especificación 0: base (ya calculada en 01_ivec_base.R, reproducida) ──
    ivec_base_v   = 0.5 * Vi_star + 0.5 * (1 - CH_star),

    # ── Especificación 1: operador multiplicativo ────────────────────────────
    # Complementariedad estricta: sqrt(Vi* · (1 − CH*))
    # Si Vi* ≈ 0, el índice colapsa hacia 0 con independencia de (1-CH*)
    ivec_mult     = sqrt(Vi_star * (1 - CH_star)),

    # ── Especificación 2a: pesos 0.7 Vi / 0.3 (1-CH) ───────────────────────
    ivec_w70      = 0.7 * Vi_star + 0.3 * (1 - CH_star),

    # ── Especificación 2b: pesos 0.3 Vi / 0.7 (1-CH) ───────────────────────
    ivec_w30      = 0.3 * Vi_star + 0.7 * (1 - CH_star),

    # ── Especificación 3: término de acumulación (α = 0.10) ─────────────────
    # Identifica y bonifica celdas con alta fragilidad simultánea en ambas dim.
    acum_flag     = as.integer(Vi_star > p75_vi & (1 - CH_star) > p75_1mch),
    ivec_acum     = ivec_base_v + ALPHA_BASE * acum_flag,

    # ── Variantes del término de acumulación ─────────────────────────────────
    ivec_acum_005 = ivec_base_v + 0.05 * acum_flag,
    ivec_acum_020 = ivec_base_v + 0.20 * acum_flag
  )

# Verificar coherencia con ivec_base original
r_check <- cor(ivec$ivec_base, ivec$ivec_base_v, use = "complete.obs")
message("  Correlación ivec_base (original) vs. ivec_base_v (reproducido): ",
        round(r_check, 6), "  [debe ser ≈ 1]")
if (abs(r_check - 1) > 1e-6) warning("  AVISO: diferencia inesperada en la reproducción del IVEC_base base.")

message("  Celdas con acum_flag = 1: ",
        sum(ivec$acum_flag), " (",
        round(100 * mean(ivec$acum_flag), 2), "%)")


# =============================================================================
# SECCIÓN 3 — CLASIFICACIÓN EN QUINTILES
# =============================================================================

message("\n── Sección 3: clasificación en quintiles ───────────────────────")

# Función auxiliar: quintil robusto (cortes sobre el conjunto completo de celdas)
asignar_quintil <- function(x) {
  cuts <- quantile(x, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
  # Asegurar que los límites extremos incluyan todos los valores
  cuts[1]   <- -Inf
  cuts[6]   <- Inf
  as.integer(cut(x, breaks = cuts, labels = 1:5, include.lowest = TRUE))
}

specs <- c("ivec_base_v", "ivec_mult", "ivec_w70", "ivec_w30", "ivec_acum")
nombres_legibles <- c(
  "ivec_base_v"  = "Base (0.5 / 0.5)",
  "ivec_mult"    = "Multiplicativo",
  "ivec_w70"     = "Pesos 0.7 / 0.3",
  "ivec_w30"     = "Pesos 0.3 / 0.7",
  "ivec_acum"    = "Acumulación (α = 0.10)"
)

for (sp in specs) {
  col_q <- paste0("q_", sp)
  ivec[[col_q]] <- asignar_quintil(ivec[[sp]])
  message("  ", sp, ": quintiles asignados. Distribución: ",
          paste(table(ivec[[col_q]]), collapse = " | "))
}


# =============================================================================
# SECCIÓN 4 — CONCORDANCIA Y MAPA DE INCERTIDUMBRE DIAGNÓSTICA
# =============================================================================

message("\n── Sección 4: concordancia entre especificaciones ──────────────")

# Columnas de quintil de las 5 especificaciones
cols_q <- paste0("q_", specs)

# Para cada celda: rango de quintiles entre especificaciones
# Un rango = 0 significa que todas las especificaciones asignan el mismo quintil
# Un rango = 4 significa máxima discordancia (quintil 1 en una, 5 en otra)
ivec <- ivec |>
  mutate(
    q_min    = pmin(!!!syms(cols_q), na.rm = TRUE),
    q_max    = pmax(!!!syms(cols_q), na.rm = TRUE),
    q_rango  = q_max - q_min,
    # Celda de alta incertidumbre diagnóstica: rango >= 2 (cambia de quintil
    # en al menos 2 posiciones entre la especificación más baja y la más alta)
    incertidumbre_alta = q_rango >= 2L
  )

message("  Distribución del rango de quintiles entre especificaciones:")
print(table(ivec$q_rango, dnn = "Rango"))
message("  Celdas con incertidumbre alta (rango ≥ 2): ",
        sum(ivec$incertidumbre_alta), " (",
        round(100 * mean(ivec$incertidumbre_alta), 2), "%)")

# Quintil más frecuente (moda) por celda como estimación robusta
moda_quintil <- function(x) {
  as.integer(names(sort(table(x), decreasing = TRUE))[1])
}
ivec <- ivec |>
  rowwise() |>
  mutate(q_moda = moda_quintil(c_across(all_of(cols_q)))) |>
  ungroup()


# =============================================================================
# SECCIÓN 5 — MATRIZ DE CORRELACIONES SPEARMAN
# =============================================================================

message("\n── Sección 5: matriz de correlaciones de Spearman ──────────────")

# Correlaciones entre las puntuaciones continuas (no los quintiles)
mat_spearman <- cor(
  ivec |> select(all_of(specs)),
  method = "spearman",
  use    = "complete.obs"
)
colnames(mat_spearman) <- nombres_legibles[specs]
rownames(mat_spearman) <- nombres_legibles[specs]

message("  Matriz de correlaciones de Spearman:")
print(round(mat_spearman, 4))

# Correlaciones entre quintiles (tau de Kendall como verificación)
mat_kendall <- cor(
  ivec |> select(all_of(cols_q)),
  method = "kendall",
  use    = "complete.obs"
)
colnames(mat_kendall) <- nombres_legibles[specs]
rownames(mat_kendall) <- nombres_legibles[specs]

# Guardar matrices
sensibilidad_correlaciones <- list(
  spearman = mat_spearman,
  kendall  = mat_kendall,
  p75_vi   = p75_vi,
  p75_1mch = p75_1mch,
  alpha    = ALPHA_BASE,
  n_celdas = nrow(ivec),
  n_acum   = sum(ivec$acum_flag)
)
saveRDS(sensibilidad_correlaciones,
        file.path(dir_out, "sensibilidad_spearman.rds"))
message("  Guardado: sensibilidad_spearman.rds")


# =============================================================================
# SECCIÓN 6 — FIGURAS PARA EL CAPÍTULO 6
# =============================================================================

message("\n── Sección 6: figuras para el capítulo 6 ───────────────────────")

library(ggplot2)

# ── 6.1 Histogramas comparados de las 5 especificaciones ──────────────────────
ivec_long <- ivec |>
  select(GRD_ID, all_of(specs)) |>
  pivot_longer(cols = all_of(specs),
               names_to  = "especificacion",
               values_to = "valor") |>
  mutate(especificacion = nombres_legibles[especificacion])

p_hist_comp <- ggplot(ivec_long, aes(x = valor, fill = especificacion)) +
  geom_histogram(bins = 40, color = "white", linewidth = 0.2) +
  facet_wrap(~ especificacion, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(
    "Base (0.5 / 0.5)"      = "#2c6fad",
    "Multiplicativo"         = "#e07b28",
    "Pesos 0.7 / 0.3"       = "#3a9b5c",
    "Pesos 0.3 / 0.7"       = "#9b3a3a",
    "Acumulación (α = 0.10)" = "#7a3a9b"
  )) +
  labs(
    title   = "Distribución del IVEC_base bajo cinco especificaciones alternativas",
    x       = "Puntuación IVEC_base",
    y       = "Frecuencia (celdas)",
    caption = sprintf("N = %s celdas activas (≥ 15 residentes). Cuadrícula LAEA 1×1 km.",
                      format(nrow(ivec), big.mark = "."))
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 9))

ggsave(file.path(dir_figuras, "fig-sens-histogramas.png"),
       p_hist_comp, width = 7, height = 10, dpi = 300)
message("  Guardado: fig-sens-histogramas.png")

# ── 6.2 Mapa de incertidumbre diagnóstica (requiere geometría del grid) ───────

ruta_grid_laea <- here("data/mallas/malla_laea/CV_grid_1km_laea.gpkg")
ruta_grid_alt  <- here("data/Literatura/GRID/CV_shapefiles/CV_grid_1km.shp")

grid_path <- if (file.exists(ruta_grid_laea)) {
  ruta_grid_laea
} else if (file.exists(ruta_grid_alt)) {
  ruta_grid_alt
} else {
  NULL
}

if (!is.null(grid_path)) {
  message("  Cargando geometría del grid desde: ", grid_path)
  grid_sf <- st_read(grid_path, quiet = TRUE)

  # Unir resultados con geometría
  # El join se hace por GRD_ID (clave estándar Eurostat)
  id_col <- if ("GRD_ID" %in% names(grid_sf)) "GRD_ID" else names(grid_sf)[1]
  sens_sf <- grid_sf |>
    inner_join(
      ivec |> select(GRD_ID, all_of(specs), all_of(cols_q),
                     q_rango, q_moda, incertidumbre_alta, acum_flag),
      by = setNames("GRD_ID", id_col)
    )

  # Proyección para visualización (ETRS89-LAEA o UTM30N)
  if (st_crs(sens_sf)$epsg != 25830) {
    sens_sf <- st_transform(sens_sf, 25830)
  }

  # Capa de delimitación administrativa (CV + provincias), común a todos los
  # mapas de la tesis. El color de los mapas de prueba se mantiene; lo que se
  # unifica es la presencia del contorno autonómico y provincial.
  .ruta_cv   <- here("data/mallas/malla_administrativa/Delimitaciones_CV.gpkg")
  .ruta_prov <- here("data/mallas/malla_administrativa/Delimitaciones_provincias.gpkg")
  .cv_lim   <- if (file.exists(.ruta_cv))   st_transform(st_read(.ruta_cv,   quiet = TRUE), st_crs(sens_sf)) else NULL
  .prov_lim <- if (file.exists(.ruta_prov)) st_transform(st_read(.ruta_prov, quiet = TRUE), st_crs(sens_sf)) else NULL
  .add_admin <- function(p) {
    if (!is.null(.prov_lim)) p <- p + geom_sf(data = .prov_lim, fill = NA, color = "grey40", linewidth = 0.3)
    if (!is.null(.cv_lim))   p <- p + geom_sf(data = .cv_lim,   fill = NA, color = "black", linewidth = 0.5)
    p
  }

  # Mapa de incertidumbre
  p_incertidumbre <- ggplot(sens_sf) +
    geom_sf(aes(fill = factor(q_rango)), color = NA) +
    scale_fill_manual(
      name   = "Rango de\nquintiles",
      values = c(
        "0" = "#d4e6f1",
        "1" = "#85c1e9",
        "2" = "#e67e22",
        "3" = "#cb4335",
        "4" = "#7b241c"
      ),
      labels = c("0 — diagnóstico estable",
                 "1 — variación menor",
                 "2 — incertidumbre moderada",
                 "3 — incertidumbre alta",
                 "4 — máxima discordancia")
    ) +
    labs(
      title   = "Mapa de incertidumbre diagnóstica del IVEC_base",
      subtitle = "Rango de clasificación en quintiles entre 5 especificaciones alternativas",
      caption = sprintf("N = %s celdas activas. Cuadrícula LAEA 1×1 km. EPSG:25830.",
                        format(nrow(ivec), big.mark = "."))
    ) +
    theme_void(base_size = 10) +
    theme(legend.position = "right",
          plot.title    = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, color = "grey40"))

  p_incertidumbre <- .add_admin(p_incertidumbre)
  ggsave(file.path(dir_figuras, "fig-sens-incertidumbre.png"),
         p_incertidumbre, width = 8, height = 7, dpi = 300)
  message("  Guardado: fig-sens-incertidumbre.png")

  # Mapa de celdas de acumulación dimensional
  p_acumulacion <- ggplot(sens_sf) +
    geom_sf(aes(fill = factor(acum_flag)), color = NA) +
    scale_fill_manual(
      name   = "Acumulación\ndimensional",
      values = c("0" = "#eaecee", "1" = "#922b21"),
      labels = c("No — fragilidad no acumulada",
                 "Sí — Vi* > p75 Y (1−CH*) > p75")
    ) +
    labs(
      title   = "Celdas con acumulación dimensional en el IVEC_base",
      subtitle = paste0("N = ", sum(ivec$acum_flag),
                        " celdas (", round(100*mean(ivec$acum_flag), 1),
                        "% del total activo). α = 0.10"),
      caption = "Cuadrícula LAEA 1×1 km. EPSG:25830."
    ) +
    theme_void(base_size = 10) +
    theme(legend.position = "right",
          plot.title    = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(size = 9, color = "grey40"))

  p_acumulacion <- .add_admin(p_acumulacion)
  ggsave(file.path(dir_figuras, "fig-sens-acumulacion.png"),
         p_acumulacion, width = 8, height = 7, dpi = 300)
  message("  Guardado: fig-sens-acumulacion.png")

  # Guardar GeoPackage de incertidumbre
  st_write(
    sens_sf |> select(GRD_ID = all_of(id_col), all_of(specs),
                      all_of(cols_q), q_rango, q_moda,
                      incertidumbre_alta, acum_flag),
    file.path(dir_out, "sensibilidad_incertidumbre.gpkg"),
    delete_dsn = TRUE,
    quiet      = TRUE
  )
  message("  Guardado: sensibilidad_incertidumbre.gpkg")

} else {
  message("  AVISO: no se encontró la geometría del grid.")
  message("         Los mapas de incertidumbre se omiten.")
  message("         Rutas buscadas:")
  message("           ", ruta_grid_laea)
  message("           ", ruta_grid_alt)
}


# =============================================================================
# SECCIÓN 7 — TABLA HEATMAP DE LA MATRIZ SPEARMAN (para el capítulo 6)
# =============================================================================

message("\n── Sección 7: heatmap de la matriz Spearman ────────────────────")

mat_df <- as.data.frame(mat_spearman) |>
  rownames_to_column("Especificación_y") |>
  pivot_longer(-Especificación_y,
               names_to  = "Especificación_x",
               values_to = "rho")

p_heatmap <- ggplot(mat_df, aes(x = Especificación_x,
                                 y = Especificación_y,
                                 fill = rho)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.3f", rho)),
            size = 3.5, fontface = "bold",
            color = ifelse(mat_df$rho > 0.97, "white", "black")) +
  scale_fill_gradientn(
    name   = "ρ Spearman",
    colors = c("#2166ac", "#d1e5f0", "#f7f7f7", "#fddbc7", "#b2182b"),
    limits = c(0.8, 1),
    oob    = scales::squish
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title   = "Concordancia entre especificaciones del IVEC_base",
    subtitle = sprintf("Correlaciones de Spearman sobre puntuaciones continuas (N = %s celdas)",
                       format(nrow(ivec), big.mark = ".")),
    x = NULL, y = NULL
  ) +
  coord_fixed() +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 0, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    plot.title    = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(size = 9, color = "grey40")
  )

ggsave(file.path(dir_figuras, "fig-sens-heatmap-spearman.png"),
       p_heatmap, width = 8, height = 6.5, dpi = 300)
message("  Guardado: fig-sens-heatmap-spearman.png")


# =============================================================================
# SECCIÓN 8 — GUARDAR TABLA COMPLETA
# =============================================================================

message("\n── Sección 8: guardando tabla de resultados ────────────────────")

write_parquet(
  ivec |> select(GRD_ID, Vi_star, CH_star,
                 all_of(specs), all_of(cols_q),
                 q_rango, q_moda, incertidumbre_alta, acum_flag,
                 ivec_acum_005, ivec_acum_020),
  file.path(dir_out, "sensibilidad_tabla.parquet")
)
message("  Guardado: sensibilidad_tabla.parquet  (",
        nrow(ivec), " celdas × ",
        length(c(specs, cols_q, "q_rango", "q_moda",
                 "incertidumbre_alta", "acum_flag")), " columnas)")


# =============================================================================
# SECCIÓN 9 — RESUMEN FINAL
# =============================================================================

message("\n══════════════════════════════════════════════════════════════")
message("  03_analisis_sensibilidad.R — completado")
message("══════════════════════════════════════════════════════════════")
message("  Celdas analizadas:              ", format(nrow(ivec), big.mark = "."))
message("  Celdas con incertidumbre alta:  ",
        sum(ivec$incertidumbre_alta), " (",
        round(100 * mean(ivec$incertidumbre_alta), 1), "%)")
message("  Celdas con acumulación dim.:    ",
        sum(ivec$acum_flag), " (",
        round(100 * mean(ivec$acum_flag), 1), "%)")
message("")
message("  Correlación Spearman mínima entre especificaciones:")
message("  ", round(min(mat_spearman[mat_spearman < 1]), 4),
        "  (entre '", rownames(which(mat_spearman == min(mat_spearman[mat_spearman < 1]),
                                     arr.ind = TRUE))[1],
        "' y '", colnames(which(mat_spearman == min(mat_spearman[mat_spearman < 1]),
                                arr.ind = TRUE))[1], "')")
message("")
message("  Archivos generados:")
message("    data/base/ivec_resultados/sensibilidad_tabla.parquet")
message("    data/base/ivec_resultados/sensibilidad_spearman.rds")
if (!is.null(grid_path)) {
  message("    data/base/ivec_resultados/sensibilidad_incertidumbre.gpkg")
}
message("    outputs/figures/cap-6/fig-sens-histogramas.png")
message("    outputs/figures/cap-6/fig-sens-heatmap-spearman.png")
if (!is.null(grid_path)) {
  message("    outputs/figures/cap-6/fig-sens-incertidumbre.png")
  message("    outputs/figures/cap-6/fig-sens-acumulacion.png")
}
message("")
message("  Siguiente paso: integrar figuras en 06-resultados.Rmd")
message("  Sección de referencia: §sec-sensibilidad (cap. 5)")
