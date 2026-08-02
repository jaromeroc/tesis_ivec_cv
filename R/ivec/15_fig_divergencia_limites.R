# =============================================================================
# fig_divergencia_limites.R — Mapa de divergencia a 0,5 km CON contorno CV+provincias
# -----------------------------------------------------------------------------
# Reproduce el canon cartográfico de 11_mapas_base.R (Sección 4): celdas de
# divergencia en rojo semáforo sobre el resto en gris, con las capas
# administrativas (provincias en gris, CV en negro), mismo bbox (CV + 2 km) y
# mismo CRS (LAEA 3035). Lee el índice a 0,5 km que ya generaste; el cruce
# celda->municipio lo hereda de la celda madre de 1 km (regla de la tesis).
# =============================================================================
suppressPackageStartupMessages({
  library(here); library(arrow); library(dplyr); library(sf); library(ggplot2)
})

# ── Parámetros ────────────────────────────────────────────────────────────────
RES_M  <- 500L
UMBRAL <- 15L                     # cambia a 5 para la variante rural
COL_ROJO <- "#d7191c"

ruta_ivec     <- here(sprintf("data/base/ivec_resultados/ivec_base_%dm_u%d.parquet", RES_M, UMBRAL))
ruta_cruce1km <- here("data/mallas/malla_laea/cruce_GRD_ID_seccion_municipio.rds")
ruta_cv_lim   <- here("data/mallas/malla_administrativa/Delimitaciones_CV.gpkg")
ruta_prov_lim <- here("data/mallas/malla_administrativa/Delimitaciones_provincias.gpkg")
dir_fig       <- here("outputs/figures/cap-6")
dir.create(dir_fig, showWarnings = FALSE, recursive = TRUE)

# ── Helper: polígonos de celda desde laea_N/laea_E ────────────────────────────
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

# ── Datos e indicador de divergencia (Sección 4, idéntico) ────────────────────
ivec <- read_parquet(ruta_ivec)
cruce1 <- readRDS(ruta_cruce1km) |> distinct(GRD_ID, .keep_all = TRUE) |>
  transmute(GRD_ID_1km = GRD_ID, CMUN)

div_df <- ivec |>
  transmute(GRD_ID, ivec_base,
            GRD_ID_1km = paste0("CRS3035RES1000mN",
                                floor(laea_N/1000)*1000, "E", floor(laea_E/1000)*1000)) |>
  left_join(cruce1, by = "GRD_ID_1km")

breaks_ivec <- quantile(ivec$ivec_base, probs = seq(0, 1, 0.2), na.rm = TRUE)
div_df$q_celda <- as.integer(cut(div_df$ivec_base, breaks = breaks_ivec,
                                 include.lowest = TRUE, labels = 1:5))
mun <- div_df |> filter(!is.na(CMUN)) |> group_by(CMUN) |>
  summarise(ivec_mun = mean(ivec_base, na.rm = TRUE), .groups = "drop")
br_mun <- quantile(mun$ivec_mun, probs = seq(0, 1, 0.2), na.rm = TRUE)
mun$q_mun <- as.integer(cut(mun$ivec_mun, breaks = br_mun,
                            include.lowest = TRUE, labels = 1:5))
div_df <- left_join(div_df, select(mun, CMUN, q_mun), by = "CMUN")
div_df$divergencia <- !is.na(div_df$q_celda) & !is.na(div_df$q_mun) &
  div_df$q_celda >= 4L & div_df$q_mun <= 2L
message("Celdas de divergencia: ", sum(div_df$divergencia, na.rm = TRUE),
        " en ", length(unique(div_df$CMUN[div_df$divergencia])), " municipios")

# ── Geometría + capas administrativas ─────────────────────────────────────────
grid   <- construir_grid(ivec, RES_M)
div_sf <- left_join(grid, select(div_df, GRD_ID, divergencia), by = "GRD_ID")
lab_div <- "Divergencia: celda Q4–Q5 en municipio Q1–Q2"
div_sf$cat <- factor(ifelse(div_sf$divergencia, lab_div, "Resto de celdas activas"),
                     levels = c(lab_div, "Resto de celdas activas"))

cv_lim   <- st_read(ruta_cv_lim,   quiet = TRUE) |> st_transform(3035)
prov_lim <- st_read(ruta_prov_lim, quiet = TRUE) |> st_transform(3035)

bb <- st_bbox(cv_lim); M <- 2000
xlim <- c(bb["xmin"] - M, bb["xmax"] + M)
ylim <- c(bb["ymin"] - M, bb["ymax"] + M)

# ── Mapa (mismo tema/canon que 11_mapas_base.R) ───────────────────────────────
p <- ggplot(div_sf) +
  geom_sf(aes(fill = cat), color = NA) +
  geom_sf(data = prov_lim, fill = NA, color = "grey40", linewidth = 0.3) +
  geom_sf(data = cv_lim,   fill = NA, color = "black",  linewidth = 0.5) +
  scale_fill_manual(values = c(COL_ROJO, "grey85"),
                    breaks = c(lab_div, "Resto de celdas activas"),
                    name = NULL, na.value = "grey85", drop = FALSE) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, crs = sf::st_crs(3035)) +
  theme_void(base_size = 11) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.4, "cm"))

nombre <- sprintf("fig_divergencia_municipal_%dm_u%d_limites", RES_M, UMBRAL)
ggsave(file.path(dir_fig, paste0(nombre, ".pdf")), p, device = cairo_pdf, width = 7, height = 8)
ggsave(file.path(dir_fig, paste0(nombre, ".png")), p, dpi = 600, width = 7, height = 8, bg = "white")
message("Guardado: ", nombre, ".pdf|png  en  ", dir_fig)
