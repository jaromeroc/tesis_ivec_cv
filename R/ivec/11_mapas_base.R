# =============================================================================
# 11_mapas_base.R — Mapas del módulo IVEC_base con el formato cartográfico
#                   canónico de la tesis (idéntico a 06_mapas_tablas_contextual.R
#                   y 09_mapas_tablas_estructural.R).
# -----------------------------------------------------------------------------
# Motivación: hasta esta versión los mapas del IVEC_base se generaban inline en
# los chunks de 06-resultados.Rmd con un formato propio (theme_void base 10,
# capa de límite CV_boundary.shp sin provincias, barra continua con el máximo
# arriba, barra de escala manual). Este script unifica el módulo base con el
# resto del instrumento: misma capa administrativa (Delimitaciones_CV +
# Delimitaciones_provincias de data/mallas), mismo tema, misma paleta semáforo,
# mismo bbox canónico desde cv_lim + 2 km y mismo tamaño de salida (7×8).
#
# Salidas (outputs/figures/cap-6/):
#   fig_mapa_ivec_base.pdf|png   Choropleth del IVEC_base por quintiles (dir 1)
#   fig_mapa_vi.pdf|png          Mapa continuo de Vi* (dir 1, barra menor→mayor)
#
# Los chunks fig-ivecbase-mapa y fig-vi-mapa de 06-resultados.Rmd se sustituyen
# por knitr::include_graphics() sobre estos ficheros, igual que ya hacen los
# mapas del módulo contextual y estructural.
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(arrow)
  library(dplyr)
  library(sf)
  library(ggplot2)
})

message("\n══════════════════════════════════════════════════════════════")
message("  11_mapas_base.R — inicio")
message("══════════════════════════════════════════════════════════════\n")

# ── Rutas ────────────────────────────────────────────────────────────────────
ruta_ivecbase  <- here("data/base/ivec_resultados/ivec_base.parquet")
ruta_grid_laea <- here("data/mallas/malla_laea/CV_grid_1km_laea.gpkg")
ruta_cv_lim    <- here("data/mallas/malla_administrativa/Delimitaciones_CV.gpkg")
ruta_prov_lim  <- here("data/mallas/malla_administrativa/Delimitaciones_provincias.gpkg")

dir_figuras <- here("outputs/figures/cap-6")
dir.create(dir_figuras, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# SECCIÓN 1 — CARGA DE DATOS
# =============================================================================

message("── Sección 1: carga de datos ──────────────────────────────────")

ivec_base <- read_parquet(ruta_ivecbase)
message("  Celdas cargadas: ", format(nrow(ivec_base), big.mark = "."))

# Geometría del grid LAEA. Si el gpkg no está disponible se reconstruye desde
# laea_N y laea_E (esquina suroeste, lado 1000 m), idéntico a 06_mapas_*.
if (file.exists(ruta_grid_laea)) {
  message("  Cargando geometría del grid desde ", basename(ruta_grid_laea), " ...")
  grid_geom <- st_read(ruta_grid_laea, quiet = TRUE) |>
    st_zm(drop = TRUE) |>
    st_transform(3035)
} else {
  message("  Reconstruyendo geometría del grid desde laea_N, laea_E ...")
  grid_geom <- ivec_base |>
    distinct(GRD_ID, laea_N, laea_E) |>
    rowwise() |>
    mutate(
      geometry = list(st_polygon(list(matrix(
        c(laea_E,          laea_N,
          laea_E + 1000L,  laea_N,
          laea_E + 1000L,  laea_N + 1000L,
          laea_E,          laea_N + 1000L,
          laea_E,          laea_N),
        ncol = 2, byrow = TRUE
      ))))
    ) |>
    ungroup() |>
    st_as_sf(crs = 3035)
}

if (!"GRD_ID" %in% names(grid_geom)) {
  warning("El grid no tiene columna GRD_ID; intentando reconstruirla")
  coords_grid <- st_coordinates(st_centroid(grid_geom))
  grid_geom <- grid_geom |>
    mutate(
      laea_N = floor(coords_grid[, 2] / 1000) * 1000L,
      laea_E = floor(coords_grid[, 1] / 1000) * 1000L,
      GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E)
    )
}

base_sf <- grid_geom |>
  select(GRD_ID, any_of("geom"), any_of("geometry")) |>
  inner_join(ivec_base, by = "GRD_ID")
message("  Celdas con geometría tras join: ",
        format(nrow(base_sf), big.mark = "."))

# Capas administrativas para el contexto cartográfico
cv_lim <- if (file.exists(ruta_cv_lim)) {
  st_read(ruta_cv_lim, quiet = TRUE) |> st_transform(3035)
} else NULL
prov_lim <- if (file.exists(ruta_prov_lim)) {
  st_read(ruta_prov_lim, quiet = TRUE) |> st_transform(3035)
} else NULL


# =============================================================================
# SECCIÓN 2 — TEMA Y PALETA CANÓNICAS  (idénticos a 06/09)
# =============================================================================

tema_mapa <- theme_void(base_size = 11) +
  theme(
    plot.title       = element_text(hjust = 0, face = "bold", size = 12,
                                    margin = margin(b = 6)),
    plot.subtitle    = element_text(hjust = 0, size = 10,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(hjust = 0, size = 8, color = "grey30",
                                    margin = margin(t = 8)),
    legend.position  = "right",
    legend.title     = element_text(size = 9, face = "bold"),
    legend.text      = element_text(size = 8),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(1.0, "cm")
  )

# Paleta semáforo canónica (RdYlGn 5 clases ColorBrewer); rojo = mayor riesgo.
COL_ROJO         <- "#d7191c"
COL_NARANJA      <- "#fdae61"
COL_AMARILLO     <- "#ffffbf"
COL_VERDE_CLARO  <- "#a6d96a"
COL_VERDE_OSCURO <- "#1a9641"

# IVEC_base y Vi son indicadores de vulnerabilidad/riesgo: dirección 1
# (mayor valor = más rojo = Q5).
paleta_riesgo_alto_es_alto <- c(
  "Q1 (más bajo)" = COL_VERDE_OSCURO,
  "Q2"            = COL_VERDE_CLARO,
  "Q3"            = COL_AMARILLO,
  "Q4"            = COL_NARANJA,
  "Q5 (más alto)" = COL_ROJO
)

# Bbox canónico de la CV en EPSG:3035 con margen de 2 km: idéntico encuadre,
# relación de aspecto y tamaño de archivo en todos los mapas de la tesis.
if (!is.null(cv_lim)) {
  bbox_canonico <- sf::st_bbox(cv_lim)
} else {
  bbox_canonico <- sf::st_bbox(base_sf)
}
BBOX_MARGEN <- 2000
bbox_xlim <- c(bbox_canonico["xmin"] - BBOX_MARGEN,
               bbox_canonico["xmax"] + BBOX_MARGEN)
bbox_ylim <- c(bbox_canonico["ymin"] - BBOX_MARGEN,
               bbox_canonico["ymax"] + BBOX_MARGEN)

# Capas de contorno administrativo, comunes a todos los mapas
capas_admin <- function(p) {
  if (!is.null(prov_lim)) {
    p <- p + geom_sf(data = prov_lim, fill = NA, color = "grey40",
                     linewidth = 0.3)
  }
  if (!is.null(cv_lim)) {
    p <- p + geom_sf(data = cv_lim, fill = NA, color = "black",
                     linewidth = 0.5)
  }
  p
}

guardar_mapa <- function(p, nombre, w = 7, h = 8) {
  ggsave(file.path(dir_figuras, paste0(nombre, ".pdf")), p,
         device = cairo_pdf, width = w, height = h)
  ggsave(file.path(dir_figuras, paste0(nombre, ".png")), p,
         dpi = 600, width = w, height = h, bg = "white")
  message("  Guardado: ", nombre, ".pdf|png")
}


# =============================================================================
# SECCIÓN 3 — MAPAS
# =============================================================================

message("\n── Sección 3: generación de mapas ──────────────────────────────")

# ── Mapa 1: IVEC_base por quintiles (categórico, dirección 1) ─────────────────
breaks_ivec <- quantile(base_sf$ivec_base, probs = seq(0, 1, 0.2), na.rm = TRUE)
base_sf$cat_ivec <- cut(base_sf$ivec_base, breaks = breaks_ivec,
                        include.lowest = TRUE,
                        labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4",
                                   "Q5 (más alto)"))

mapa_ivec <- ggplot(base_sf) +
  geom_sf(aes(fill = cat_ivec), color = NA) +
  scale_fill_manual(values = paleta_riesgo_alto_es_alto,
                    name = "Quintil\nIVEC_base",
                    na.value = "grey85", drop = FALSE)
mapa_ivec <- capas_admin(mapa_ivec) +
  coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE,
           crs = sf::st_crs(3035)) +
  tema_mapa
guardar_mapa(mapa_ivec, "fig_mapa_ivec_base")

# ── Mapa 2: dimensión Vi* (continuo, dirección 1) ─────────────────────────────
# Gradiente verde→rojo equivalente a la paleta semáforo. La barra de color se
# invierte (guide reverse = TRUE) para que se lea de menor (verde, arriba) a
# mayor (rojo, abajo), en coherencia con el orden Q1→Q5 de los mapas
# categóricos del instrumento.
mapa_vi <- ggplot(base_sf) +
  geom_sf(aes(fill = Vi_star), color = NA) +
  scale_fill_gradientn(
    colours = c(COL_VERDE_OSCURO, COL_VERDE_CLARO, COL_AMARILLO,
                COL_NARANJA, COL_ROJO),
    name   = "Vi*\nnormalizada",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25),
    labels = c("0", "0,25", "0,50", "0,75", "1"),
    na.value = "grey85",
    guide  = guide_colorbar(reverse = TRUE)
  )
mapa_vi <- capas_admin(mapa_vi) +
  coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE,
           crs = sf::st_crs(3035)) +
  tema_mapa
guardar_mapa(mapa_vi, "fig_mapa_vi")

# ── Mapa 3: dimensión CH* (quintiles, dirección -1) ───────────────────────────
# CH es un indicador de capacidad: mayor CH* = mayor capacidad protectora del
# hogar = menor vulnerabilidad. Dirección cromática invertida respecto a Vi:
# Q1 (menor capacidad, más vulnerable) = rojo; Q5 (mayor capacidad) = verde.
paleta_riesgo_alto_es_bajo <- c(
  "Q1 (más bajo)" = COL_ROJO,
  "Q2"            = COL_NARANJA,
  "Q3"            = COL_AMARILLO,
  "Q4"            = COL_VERDE_CLARO,
  "Q5 (más alto)" = COL_VERDE_OSCURO
)

breaks_ch <- quantile(base_sf$CH_star, probs = seq(0, 1, 0.2), na.rm = TRUE)
base_sf$cat_ch <- cut(base_sf$CH_star, breaks = breaks_ch,
                      include.lowest = TRUE,
                      labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4",
                                 "Q5 (más alto)"))

mapa_ch <- ggplot(base_sf) +
  geom_sf(aes(fill = cat_ch), color = NA) +
  scale_fill_manual(values = paleta_riesgo_alto_es_bajo,
                    name = "Quintil\nCH*",
                    na.value = "grey85", drop = FALSE)
mapa_ch <- capas_admin(mapa_ch) +
  coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE,
           crs = sf::st_crs(3035)) +
  tema_mapa
guardar_mapa(mapa_ch, "fig_mapa_ch_star")

# =============================================================================
# SECCIÓN 4 — MAPA DE DIVERGENCIA CUADRÍCULA ↔ MUNICIPIO (evidencia H2)
# -----------------------------------------------------------------------------
# Identifica las celdas donde la lectura a 1 km² del IVEC_base detecta alta
# vulnerabilidad (Q4-Q5 de la distribución celular) que la lectura de agregación
# municipal ---media del IVEC_base de las celdas del municipio, quintilada sobre
# los municipios con celdas activas--- no captura, porque sitúa al municipio en
# los quintiles bajos (Q1-Q2). Es la demostración cartográfica del problema de la
# unidad espacial modificable (PUAM) y de la segunda hipótesis: el municipio
# promedio oculta concentraciones intramunicipales de vulnerabilidad que la
# resolución de cuadrícula sí revela.
# =============================================================================

message("\n── Sección 4: mapa de divergencia cuadrícula–municipio ─────────")

ruta_cruce_mun <- here("data/mallas/malla_laea/cruce_GRD_ID_seccion_municipio.rds")

if (file.exists(ruta_cruce_mun)) {
  cruce_mun <- readRDS(ruta_cruce_mun)        # GRD_ID, CMUN, nombre, comarca

  # Tabla celda → (ivec_base, municipio)
  div_df <- base_sf |>
    sf::st_drop_geometry() |>
    select(GRD_ID, ivec_base) |>
    left_join(select(cruce_mun, GRD_ID, CMUN), by = "GRD_ID")

  # Quintil de celda (mismos cortes que el mapa categórico de la Sección 3)
  div_df$q_celda <- as.integer(cut(div_df$ivec_base, breaks = breaks_ivec,
                                   include.lowest = TRUE, labels = 1:5))

  # Indicador municipal convencional: media del IVEC_base por municipio,
  # quintilada sobre los municipios con celdas activas.
  mun_agg <- div_df |>
    filter(!is.na(CMUN)) |>
    group_by(CMUN) |>
    summarise(ivec_mun = mean(ivec_base, na.rm = TRUE), .groups = "drop")
  br_mun <- quantile(mun_agg$ivec_mun, probs = seq(0, 1, 0.2), na.rm = TRUE)
  mun_agg$q_mun <- as.integer(cut(mun_agg$ivec_mun, breaks = br_mun,
                                  include.lowest = TRUE, labels = 1:5))
  div_df <- left_join(div_df, select(mun_agg, CMUN, q_mun), by = "CMUN")

  # Celda de divergencia: alta a escala de cuadrícula (Q4-Q5) en municipio cuya
  # lectura agregada es baja (Q1-Q2).
  div_df$divergencia <- !is.na(div_df$q_celda) & !is.na(div_df$q_mun) &
                        div_df$q_celda >= 4L & div_df$q_mun <= 2L
  n_div     <- sum(div_df$divergencia, na.rm = TRUE)
  n_mun_div <- length(unique(div_df$CMUN[div_df$divergencia]))
  message("  Celdas de divergencia (celda Q4-Q5 en municipio Q1-Q2): ",
          format(n_div, big.mark = "."), "  en ", n_mun_div, " municipios")

  # Exporta las dos cifras para que el capítulo 6 (chunk `divergencia-stats`)
  # las cite de forma reproducible en lugar de dejarlas solo en consola.
  ruta_div_stats <- here("data/base/ivec_resultados/divergencia_municipal_stats.rds")
  saveRDS(list(n_div = n_div, n_mun_div = n_mun_div), ruta_div_stats)
  message("  Guardado: ", basename(ruta_div_stats))

  div_sf <- left_join(base_sf, select(div_df, GRD_ID, divergencia),
                      by = "GRD_ID")
  div_sf$cat_div <- factor(
    ifelse(div_sf$divergencia,
           "Divergencia: celda Q4–Q5 en municipio Q1–Q2",
           "Resto de celdas activas"),
    levels = c("Divergencia: celda Q4–Q5 en municipio Q1–Q2",
               "Resto de celdas activas"))

  mapa_div <- ggplot(div_sf) +
    geom_sf(aes(fill = cat_div), color = NA) +
    scale_fill_manual(
      values = c("Divergencia: celda Q4–Q5 en municipio Q1–Q2" = COL_ROJO,
                 "Resto de celdas activas"                     = "grey85"),
      name = NULL, na.value = "grey85", drop = FALSE)
  mapa_div <- capas_admin(mapa_div) +
    coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE,
             crs = sf::st_crs(3035)) +
    tema_mapa +
    theme(legend.position = "bottom")
  guardar_mapa(mapa_div, "fig_divergencia_municipal")
} else {
  message("  AVISO: ", basename(ruta_cruce_mun), " no disponible; ejecuta antes ",
          "12_cruce_grd_municipio.R. Mapa de divergencia omitido.")
}

message("\n══════════════════════════════════════════════════════════════")
message("  11_mapas_base.R — fin")
message("══════════════════════════════════════════════════════════════\n")
