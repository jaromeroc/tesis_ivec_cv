# =============================================================================
# 09_mapas_tablas_estructural.R — MAPAS Y TABLAS DEL IVEC_estructural
# =============================================================================
# Módulo:   Generación de figuras y tablas del módulo IVEC_estructural para
#           el capítulo sexto (§sec-res-estructural) y para los Apéndices D y F.
# Escala:   Municipal (542 municipios de la Comunitat Valenciana).
# Versión:  V1 (mayo 2026, ruta arquitectural D: 12 indicadores activos,
#               k-means K = 5, ordenamiento de clústeres por mediana del índice
#               compuesto de severidad demográfica del clúster ---z(envejecim.)
#               + z(-maternidad) + z(-tendencia)---; asignación VE* ∈
#               {0,1; 0,3; 0,5; 0,7; 0,9}. AROPE comarcal y renta neta
#               media municipal se conservan como auxiliares interpretativas.
#
# Salidas principales (outputs/figures/cap-6/ y outputs/tables/cap-6/):
#
#   FIGURAS:
#     fig_mapa_ivec_estructural.pdf|png      Choropleth municipal del compuesto
#                                            (cinco clases discretas)
#     fig_mapa_cad_star.pdf|png              Choropleth municipal de CAD*
#                                            (quintiles, dirección invertida)
#     fig_mapa_cluster.pdf|png               Choropleth municipal de cluster_id
#                                            (cinco categorías PAR)
#     fig_histograma_ivec_estructural.pdf|png  Distribución global del compuesto
#     fig_dispersion_renta_ivec_estr.pdf|png   IVEC × renta INE 2023 (validación
#                                              preliminar; la validación externa
#                                              formal con RMEs FISABIO se realiza
#                                              en 08_validacion_externa_estructural.R)
#
#   TABLAS:
#     tab_distribucion_indicadores_estr.rds  Min, q25, mediana, media, q75, max,
#                                            sd de los 12 indicadores VE+CAD.
#     tab_caracterizacion_clusters.rds       Caracterización rica de cada uno de
#                                            los cinco clústeres con variables
#                                            auxiliares (mediana renta, edad media,
#                                            % unipersonales, tasa dependencia,
#                                            longevidad, carga financiera, etc.)
#                                            + medianas de los 8 VE y 4 CAD por
#                                            clúster, para apoyar la asignación
#                                            de etiquetas PAR cualitativas en
#                                            cap. 6 §sec-res-estructural.
#
# Notas:
#   • El script se ejecuta una sola vez tras `07_ivec_estructural.R`. Los
#     productos se incorporan al cap. 6 desde chunks bookdown que los cargan
#     con `readRDS()` para tablas e `include_graphics()` para figuras.
#
#   • Estilo cromático: la paleta semáforo canónica de la tesis es RdYlGn de
#     5 clases (ColorBrewer), idéntica a la del módulo contextual y del módulo
#     base, manteniendo la convención "rojo = mayor riesgo" coherente entre los
#     tres módulos del IVEC. La orientación de la paleta (qué quintil queda en
#     rojo) se controla con el parámetro `direccion` de mapa_choropleth():
#       direccion = 1  → rojo = Q5 = mayor IVEC = mayor vulnerabilidad.
#                        Aplica a IVEC_estructural y a VE*.
#       direccion = -1 → rojo = Q1 = menor CAD = menor capacidad.
#                        Aplica a CAD* (capacidad, no riesgo).
#
#   • Convención categórica para clúster: paleta Dark2 (ColorBrewer 5 categorías)
#     con etiquetas neutras "Clúster A — VE muy alta" ... "Clúster E — VE muy baja"
#     ordenadas por el valor VE* del clúster (descendente: A = VE* 0,9 → E =
#     0,1). Esta etiqueta inicial es operativa y se sustituye en cap. 6 por
#     los nombres cualitativos PAR (agrario-interior despoblado extremo,
#     monoproducción estacional envejecida, especializado-productivo intermedio,
#     atractor de inmigración exterior, perfil diversificado de baja severidad
#     estructural) tras inspeccionar la tabla `tab_caracterizacion_clusters.rds`.
#
#   • Uniformidad geométrica: todos los mapas usan el mismo bbox calculado
#     desde cv_lim con un margen de 2 km, idéntica relación de aspecto y mismas
#     dimensiones de archivo (7 × 8 pulgadas), garantizando que las
#     delimitaciones administrativas sean visualmente comparables entre
#     figuras y entre módulos del IVEC.
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(here)

set.seed(1972)

# ── Rutas ────────────────────────────────────────────────────────────────────
# Geometría municipal canónica: capa ICV.Municipios de Delimitaciones_municipios.gpkg
# (Institut Cartogràfic Valencià, EPSG:25830, 542 municipios, campo identificador
# cod_ine_mun de 5 dígitos). Capas administrativas auxiliares: Delimitaciones_CV
# (límite de la comunidad autónoma) y Delimitaciones_provincias (3 provincias).
ruta_ivec_estr   <- here("data/estructural/ivec_estructural/ivec_estructural.parquet")
ruta_municipios  <- here("data/mallas/malla_administrativa/Delimitaciones_municipios.gpkg")
ruta_cv_lim      <- here("data/mallas/malla_administrativa/Delimitaciones_CV.gpkg")
ruta_prov_lim    <- here("data/mallas/malla_administrativa/Delimitaciones_provincias.gpkg")

dir_figuras <- here("outputs/figures/cap-6")
dir_tablas  <- here("outputs/tables/cap-6")
dir.create(dir_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_tablas,  showWarnings = FALSE, recursive = TRUE)

message("\n══════════════════════════════════════════════════════════════")
message("  09_mapas_tablas_estructural.R — inicio")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA DE DATOS
# =============================================================================

message("── Sección 1: carga de datos ──────────────────────────────────")

ivec_estr <- read_parquet(ruta_ivec_estr)
message("  Municipios cargados: ", format(nrow(ivec_estr), big.mark = "."))

# Geometría municipal de la CV: capa ICV.Municipios del GeoPackage canónico del
# ICV en data/mallas/malla_administrativa/. La capa expone 542 municipios CV
# con identificador `cod_ine_mun` de 5 dígitos. La detección del campo es
# defensiva (acepta variantes posibles si el fichero se actualiza en el futuro).
if (!file.exists(ruta_municipios)) {
  stop("No se ha encontrado Delimitaciones_municipios.gpkg en ",
       "data/mallas/malla_administrativa/.")
}

municipios_geom <- st_read(ruta_municipios, layer = "ICV.Municipios",
                            quiet = TRUE) |>
  st_zm(drop = TRUE) |>
  st_transform(3035)

# Detección del campo identificador municipal. El fichero canónico ICV usa
# `cod_ine_mun` (5 dígitos), pero se aceptan alias por robustez.
cmun_field <- intersect(c("cod_ine_mun", "CMUN", "CODIGO", "COD_INE", "NATCODE",
                          "MUNICIPIO_CODE", "MUN_CODE", "cod_ine"),
                        names(municipios_geom))[1]
if (is.na(cmun_field)) {
  stop("La geometría municipal no expone un campo identificador reconocible. ",
       "Campos disponibles: ", paste(names(municipios_geom), collapse = ", "))
}

municipios_geom <- municipios_geom |>
  mutate(CMUN = stringr::str_pad(stringr::str_extract(as.character(.data[[cmun_field]]),
                                                     "\\d+"),
                                 width = 5, side = "left", pad = "0")) |>
  filter(stringr::str_sub(CMUN, 1, 2) %in% c("03", "12", "46"))

# Unión geometría × panel IVEC
ivec_sf <- municipios_geom |>
  select(CMUN) |>
  inner_join(ivec_estr, by = "CMUN")

message("  Municipios con geometría tras join: ", nrow(ivec_sf))

# Capas administrativas para el contexto cartográfico
cv_lim <- if (file.exists(ruta_cv_lim)) {
  st_read(ruta_cv_lim, quiet = TRUE) |> st_transform(3035)
} else NULL
prov_lim <- if (file.exists(ruta_prov_lim)) {
  st_read(ruta_prov_lim, quiet = TRUE) |> st_transform(3035)
} else NULL


# =============================================================================
# SECCIÓN 2 — TEMA Y PALETAS CANÓNICAS
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

# Paleta semáforo canónica (RdYlGn 5 clases ColorBrewer)
COL_ROJO         <- "#d7191c"
COL_NARANJA      <- "#fdae61"
COL_AMARILLO     <- "#ffffbf"
COL_VERDE_CLARO  <- "#a6d96a"
COL_VERDE_OSCURO <- "#1a9641"

paleta_riesgo_alto_es_alto <- c(
  "Q1 (más bajo)" = COL_VERDE_OSCURO,
  "Q2"            = COL_VERDE_CLARO,
  "Q3"            = COL_AMARILLO,
  "Q4"            = COL_NARANJA,
  "Q5 (más alto)" = COL_ROJO
)

paleta_riesgo_alto_es_bajo <- c(
  "Q1 (más bajo)" = COL_ROJO,
  "Q2"            = COL_NARANJA,
  "Q3"            = COL_AMARILLO,
  "Q4"            = COL_VERDE_CLARO,
  "Q5 (más alto)" = COL_VERDE_OSCURO
)

get_paleta <- function(direccion) {
  if (direccion == 1) paleta_riesgo_alto_es_alto
  else                paleta_riesgo_alto_es_bajo
}

# Paleta específica para VE* (cinco clases discretas en {0,1; 0,3; 0,5; 0,7; 0,9}).
# El orden semántico es el mismo que en la paleta semáforo: a mayor VE*, mayor
# vulnerabilidad estructural, así que el más vulnerable (0,9) queda en rojo.
paleta_ve_star <- c(
  "0,1 (VE muy baja)"    = COL_VERDE_OSCURO,
  "0,3 (VE baja)"        = COL_VERDE_CLARO,
  "0,5 (VE moderada)"    = COL_AMARILLO,
  "0,7 (VE alta)"        = COL_NARANJA,
  "0,9 (VE muy alta)"    = COL_ROJO
)

# Paleta de cinco categorías para cluster_id. Usa la MISMA paleta semáforo
# canónica que el mapa de VE* (fig. 6.21), de modo que ambos mapas comparten
# cromática: a cada clúster se le asigna el color semáforo de su nivel de VE*.
# Las etiquetas operativas se ordenan por VE* ASCENDENTE (A = menos vulnerable,
# VE* 0,1, verde oscuro → E = más vulnerable, VE* 0,9, rojo), idéntico al orden
# de la leyenda de fig. 6.21. Tras inspeccionar tab_caracterizacion_clusters.rds
# estas etiquetas se sustituyen en cap. 6 §sec-res-estructural por los nombres
# cualitativos PAR.
paleta_cluster <- c(
  "A — VE muy baja (0,1)" = COL_VERDE_OSCURO,
  "B — VE baja (0,3)"     = COL_VERDE_CLARO,
  "C — VE moderada (0,5)" = COL_AMARILLO,
  "D — VE alta (0,7)"     = COL_NARANJA,
  "E — VE muy alta (0,9)" = COL_ROJO
)

# Bbox canónico
if (!is.null(cv_lim)) {
  bbox_canonico <- sf::st_bbox(cv_lim)
} else {
  bbox_canonico <- sf::st_bbox(ivec_sf)
}
BBOX_MARGEN <- 2000
bbox_xlim <- c(bbox_canonico["xmin"] - BBOX_MARGEN,
               bbox_canonico["xmax"] + BBOX_MARGEN)
bbox_ylim <- c(bbox_canonico["ymin"] - BBOX_MARGEN,
               bbox_canonico["ymax"] + BBOX_MARGEN)


# =============================================================================
# SECCIÓN 3 — HELPERS DE MAPAS
# =============================================================================

#' Choropleth municipal con paleta semáforo y opción de quintiles o cinco
#' clases discretas (caso especial para VE*).
mapa_choropleth <- function(sf_data, columna, titulo, subtitulo, caption,
                            etiqueta_leyenda = NULL,
                            modo = c("quintiles", "ve_star", "cluster"),
                            direccion = 1) {
  modo <- match.arg(modo)
  if (is.null(etiqueta_leyenda)) etiqueta_leyenda <- columna

  if (modo == "quintiles") {
    breaks <- quantile(sf_data[[columna]], probs = seq(0, 1, 0.2),
                       na.rm = TRUE)
    sf_data$cat <- cut(sf_data[[columna]], breaks = breaks,
                       include.lowest = TRUE,
                       labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4",
                                  "Q5 (más alto)"))
    valores_paleta <- get_paleta(direccion)
  } else if (modo == "ve_star") {
    sf_data$cat <- factor(
      sprintf("%0.1f", round(sf_data[[columna]], 1)),
      levels = c("0.1", "0.3", "0.5", "0.7", "0.9"),
      labels = c("0,1 (VE muy baja)", "0,3 (VE baja)",
                 "0,5 (VE moderada)", "0,7 (VE alta)",
                 "0,9 (VE muy alta)")
    )
    valores_paleta <- paleta_ve_star
  } else {   # cluster
    # cluster_id es entero 1..K_CLUSTERS. Lo emparejamos con el VE* del clúster
    # (que ya viene en el parquet) para producir etiquetas operativas A..E.
    etiquetas_cluster <- c(
      "A — VE muy baja (0,1)",
      "B — VE baja (0,3)",
      "C — VE moderada (0,5)",
      "D — VE alta (0,7)",
      "E — VE muy alta (0,9)"
    )
    cluster_orden <- sf_data |>
      sf::st_drop_geometry() |>
      distinct(cluster_id, ve_star) |>
      arrange(ve_star) |>
      mutate(etiqueta_cluster = etiquetas_cluster[seq_len(dplyr::n())])
    sf_data <- sf_data |>
      left_join(cluster_orden, by = c("cluster_id", "ve_star"))
    sf_data$cat <- factor(sf_data$etiqueta_cluster,
                          levels = names(paleta_cluster))
    valores_paleta <- paleta_cluster
  }

  p <- ggplot(sf_data) +
    geom_sf(aes(fill = cat), color = "grey80", linewidth = 0.05) +
    scale_fill_manual(values = valores_paleta,
                      name = etiqueta_leyenda,
                      na.value = "grey85",
                      drop = FALSE)

  if (!is.null(prov_lim)) {
    p <- p + geom_sf(data = prov_lim, fill = NA, color = "grey40",
                     linewidth = 0.3)
  }
  if (!is.null(cv_lim)) {
    p <- p + geom_sf(data = cv_lim, fill = NA, color = "black",
                     linewidth = 0.5)
  }
  p +
    coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE,
             crs = sf::st_crs(3035)) +
    labs(title = titulo, subtitle = subtitulo, caption = caption) +
    tema_mapa
}

guardar_mapa <- function(p, nombre, w = 7, h = 8) {
  ggsave(file.path(dir_figuras, paste0(nombre, ".pdf")), p,
         device = cairo_pdf, width = w, height = h)
  ggsave(file.path(dir_figuras, paste0(nombre, ".png")), p,
         dpi = 600, width = w, height = h, bg = "white")
  message("  Guardado: ", nombre, ".pdf|png")
}


# =============================================================================
# SECCIÓN 4 — MAPAS PRINCIPALES (4 figuras)
# =============================================================================

message("\n── Sección 4: generación de mapas ──────────────────────────────")

# Mapa 1: IVEC_estructural compuesto (fórmula compensadora ½·VE* + ½·(1 − CAD*))
mapa_ivec <- mapa_choropleth(
  ivec_sf,
  columna = "IVEC_estructural",
  titulo = "IVEC_estructural (V1)",
  subtitulo = paste0("Quintiles del compuesto estructural sobre los ",
                     nrow(ivec_sf), " municipios CV"),
  caption = paste0("Fórmula: IVEC_estructural = ½·VE* + ½·(1 − CAD*). ",
                   "Rojo = mayor vulnerabilidad estructural compensada por CAD."),
  etiqueta_leyenda = "Quintil",
  modo = "quintiles",
  direccion = 1
)
guardar_mapa(mapa_ivec, "fig_mapa_ivec_estructural")

# Mapa 2 (VE* discreto) suprimido: era idéntico al mapa de clústeres (la
# tipología por k-means y VE* son la misma partición); se conserva solo el
# mapa de la tipología (fig_mapa_cluster) con VE* explícito en la leyenda.

# Mapa 3: CAD* (quintiles, dirección invertida: rojo = baja capacidad)
mapa_cad <- mapa_choropleth(
  ivec_sf,
  columna = "CAD_star",
  titulo = "CAD* — capacidad institucional financiera",
  subtitulo = paste0("Quintiles de CAD* (normalización percentílica + media + ",
                     "min-max sobre los 4 indicadores CAD)"),
  caption = paste0("Rojo = Q1 = menor capacidad institucional. La deuda viva ",
                   "entra invertida en la agregación (mayor deuda = menor CAD)."),
  etiqueta_leyenda = "Quintil",
  modo = "quintiles",
  direccion = -1
)
guardar_mapa(mapa_cad, "fig_mapa_cad_star")

# Mapa 4: cluster_id (cinco categorías PAR; etiquetas operativas A..E)
mapa_cluster <- mapa_choropleth(
  ivec_sf,
  columna = "cluster_id",
  titulo = "Tipología municipal — clústeres del k-means VE",
  subtitulo = paste0("Cinco perfiles obtenidos por k-means K = 5 sobre los ",
                     "8 indicadores VE estandarizados"),
  caption = paste0("Etiquetas operativas ordenadas por VE* descendente (A = ",
                   "más vulnerable). La asignación cualitativa a los perfiles ",
                   "PAR canónicos (agrario-interior despoblado extremo, ",
                   "monoproducción estacional envejecida, especializado-productivo ",
                   "intermedio, atractor de inmigración exterior, diversificado de ",
                   "baja severidad) se documenta en cap. 6 §sec-res-estructural ",
                   "tras inspeccionar la tabla de caracterización rica."),
  etiqueta_leyenda = "Clúster",
  modo = "cluster"
)
guardar_mapa(mapa_cluster, "fig_mapa_cluster")


# =============================================================================
# SECCIÓN 5 — HISTOGRAMA + DISPERSIÓN DE VALIDACIÓN PRELIMINAR
# =============================================================================

message("\n── Sección 5: histograma + dispersión renta × IVEC ─────────────")

# Histograma del compuesto
hist_p <- ggplot(ivec_sf, aes(x = IVEC_estructural)) +
  geom_histogram(bins = 25, fill = COL_NARANJA, color = "white", linewidth = 0.3) +
  geom_vline(xintercept = median(ivec_sf$IVEC_estructural, na.rm = TRUE),
             color = COL_ROJO, linetype = "dashed", linewidth = 0.7) +
  labs(
    title    = "Distribución del IVEC_estructural en los 542 municipios CV",
    subtitle = paste0("Línea roja: mediana del compuesto. Bins = 25."),
    x        = "IVEC_estructural",
    y        = "N municipios"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(dir_figuras, "fig_histograma_ivec_estructural.pdf"), hist_p,
       device = cairo_pdf, width = 7, height = 5)
ggsave(file.path(dir_figuras, "fig_histograma_ivec_estructural.png"), hist_p,
       dpi = 600, width = 7, height = 5, bg = "white")
message("  Guardado: fig_histograma_ivec_estructural.pdf|png")

# Dispersión IVEC × renta neta media INE 2023 (validación preliminar).
# El parquet 07 ya trae la columna `renta_neta_media` por municipio.
disp_p <- ivec_sf |>
  sf::st_drop_geometry() |>
  filter(!is.na(renta_neta_media), !is.na(IVEC_estructural)) |>
  ggplot(aes(x = renta_neta_media, y = IVEC_estructural)) +
  geom_point(alpha = 0.55, color = COL_NARANJA, size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, color = COL_ROJO, linewidth = 0.8) +
  labs(
    title    = "Validación preliminar: IVEC_estructural × Renta neta media municipal",
    subtitle = paste0("Atlas de Distribución de Renta de los Hogares INE 2023 ",
                       "(tabla 31106, indicador 'Renta neta media por persona')"),
    caption  = paste0("La correlación esperada es negativa: mayor renta ",
                       "→ menor vulnerabilidad estructural. ",
                       "La validación externa formal con RMEs FISABIO se documenta ",
                       "en 08_validacion_externa_estructural.R."),
    x        = "Renta neta media por persona (€, 2023)",
    y        = "IVEC_estructural"
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(dir_figuras, "fig_dispersion_renta_ivec_estr.pdf"), disp_p,
       device = cairo_pdf, width = 7, height = 5)
ggsave(file.path(dir_figuras, "fig_dispersion_renta_ivec_estr.png"), disp_p,
       dpi = 600, width = 7, height = 5, bg = "white")
message("  Guardado: fig_dispersion_renta_ivec_estr.pdf|png")


# =============================================================================
# SECCIÓN 6 — TABLAS
# =============================================================================

message("\n── Sección 6: tablas ───────────────────────────────────────────")

# 6.1 — Distribución descriptiva de los 12 indicadores
COLS_VE <- c("ve_lq_sector_dominante", "ve_hhi_sectorial", "ve_amplitud_estacional",
             "ve_indice_envejecimiento", "ve_indice_maternidad", "ve_indice_tendencia",
             "ve_saldo_interior_pmh",  "ve_saldo_exterior_pmh")

COLS_CAD <- c("cad_deuda_viva_pc", "cad_gasto_sss_pc",
              "cad_inversiones_pc", "cad_subvenciones_pc")

resumen_descriptivo <- function(x) {
  c(
    n        = sum(!is.na(x)),
    min      = suppressWarnings(min(x, na.rm = TRUE)),
    q25      = unname(quantile(x, 0.25, na.rm = TRUE)),
    mediana  = median(x, na.rm = TRUE),
    media    = mean(x, na.rm = TRUE),
    q75      = unname(quantile(x, 0.75, na.rm = TRUE)),
    max      = suppressWarnings(max(x, na.rm = TRUE)),
    sd       = sd(x, na.rm = TRUE)
  )
}

panel_plano <- sf::st_drop_geometry(ivec_sf)

tab_distribucion <- bind_rows(
  purrr::map_dfr(COLS_VE,  ~ tibble::tibble(indicador = .x,
                                            dimension = "VE",
                                            !!! as.list(resumen_descriptivo(panel_plano[[.x]])))),
  purrr::map_dfr(COLS_CAD, ~ tibble::tibble(indicador = .x,
                                            dimension = "CAD",
                                            !!! as.list(resumen_descriptivo(panel_plano[[.x]]))))
)

saveRDS(tab_distribucion, file.path(dir_tablas, "tab_distribucion_indicadores_estr.rds"))
message("  Guardado: tab_distribucion_indicadores_estr.rds (",
        nrow(tab_distribucion), " filas).")

# 6.2 — Caracterización rica de cada clúster con variables auxiliares.
#
# Para cada clúster (etiqueta operativa A..E ordenada por VE* descendente)
# se reportan: n municipios; medianas de los 8 VE estandarizados y los
# 4 CAD; mediana del índice compuesto de severidad demográfica (criterio
# canónico de ordenamiento k-means); medianas del AROPE comarcal proyectado
# y de la renta neta media municipal (ambas variables auxiliares
# interpretativas); medianas de las variables auxiliares restantes
# disponibles en el panel (edad media, % unipersonales, % menores 18,
# tamaño medio hogar, tasa de dependencia, longevidad, renovación activa,
# carga financiera). Esta tabla es el insumo para la asignación cualitativa
# de los nombres PAR.

cluster_orden_tabla <- panel_plano |>
  distinct(cluster_id, ve_star) |>
  arrange(ve_star) |>
  mutate(etiqueta_cluster = c(
    "A — VE muy baja (0,1)",
    "B — VE baja (0,3)",
    "C — VE moderada (0,5)",
    "D — VE alta (0,7)",
    "E — VE muy alta (0,9)"
  )[seq_len(dplyr::n())])

panel_marcado <- panel_plano |>
  left_join(cluster_orden_tabla, by = c("cluster_id", "ve_star"))

vars_auxiliares <- intersect(
  c("arope", "renta_neta_media",
    "aux_edad_media", "aux_pct_unipersonales", "aux_pct_menores_18",
    "aux_tam_hogar", "aux_poblacion",
    "aux_tasa_dependencia", "aux_dependencia_mayores",
    "aux_dependencia_menores", "aux_indice_longevidad",
    "aux_renovacion_activa",
    "aux_intereses_pc", "aux_amortizacion_pc", "aux_carga_financiera_pc"),
  names(panel_marcado)
)

cols_tabla <- c(COLS_VE, COLS_CAD, vars_auxiliares)

tab_caracterizacion <- panel_marcado |>
  group_by(etiqueta_cluster) |>
  summarise(
    n_municipios = dplyr::n(),
    across(any_of(cols_tabla), ~ median(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  arrange(etiqueta_cluster)

saveRDS(tab_caracterizacion, file.path(dir_tablas, "tab_caracterizacion_clusters.rds"))
message("  Guardado: tab_caracterizacion_clusters.rds (",
        nrow(tab_caracterizacion), " filas × ",
        ncol(tab_caracterizacion), " columnas).")


# =============================================================================
# RESUMEN FINAL
# =============================================================================

message("\n══════════════════════════════════════════════════════════════")
message("  09_mapas_tablas_estructural.R — completado.")
message("══════════════════════════════════════════════════════════════")
message("  Figuras en ", dir_figuras, ":")
message("    • fig_mapa_ivec_estructural.{pdf,png}")
message("    • fig_mapa_cad_star.{pdf,png}")
message("    • fig_mapa_cluster.{pdf,png}")
message("    • fig_histograma_ivec_estructural.{pdf,png}")
message("    • fig_dispersion_renta_ivec_estr.{pdf,png}")
message("  Tablas en ", dir_tablas, ":")
message("    • tab_distribucion_indicadores_estr.rds")
message("    • tab_caracterizacion_clusters.rds")
message("\n  Próximo paso: inspeccionar tab_caracterizacion_clusters.rds")
message("  para asignar etiquetas cualitativas PAR a los cinco clústeres,")
message("  y a continuación ejecutar el script de validación externa con")
message("  RMEs FISABIO: R/ivec/08_validacion_externa_estructural.R.\n")
