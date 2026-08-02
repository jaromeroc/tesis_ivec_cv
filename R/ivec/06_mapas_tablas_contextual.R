# =============================================================================
# 06_mapas_tablas_contextual.R — MAPAS Y TABLAS DEL IVEC_contextual
# =============================================================================
# Módulo:   Generación de figuras y tablas del módulo IVEC_contextual para
#           el capítulo sexto (§sec-res-contextual) y para los Apéndices D y F.
# Escala:   Cuadrícula LAEA 1×1 km (EPSG:3035)
#
# Versión:  V1 (mayo 2026, arquitectura 13 indicadores activos / 14 tabla
#               operativa: 4 VC físico-residencial + 5 VC demográfico-
#               poblacional + 4 CS activos; cs_asociativo diferido a V2).
#
# Salidas principales (outputs/figures/cap-6/ y outputs/tables/cap-6/):
#
#   FIGURAS:
#     fig_mapa_ivec_contextual.pdf|png       Choropleth del compuesto IVEC_contextual
#     fig_mapa_vc_star.pdf|png               Choropleth de VC* (vulnerabilidad agregada)
#     fig_mapa_cs_star.pdf|png               Choropleth de CS* (soporte agregado)
#     fig_mapa_vc_fisico.pdf|png             Choropleth sub-dim físico-residencial
#     fig_mapa_vc_demografico.pdf|png        Choropleth sub-dim demográfico-poblacional
#     fig_mapa_cs_proximidad.pdf|png         Densidad de servicios de proximidad
#     fig_mapa_cs_transporte.pdf|png         Tiempo a parada de transporte público
#     fig_mapa_cs_sociosanitario.pdf|png     Tiempo a equipamientos sociosanitarios
#     fig_mapa_cs_nodos_civicos.pdf|png      Densidad de nodos cívicos
#     fig_mapa_cobertura_vc.pdf|png          N de indicadores VC con dato por celda
#     fig_mapa_cobertura_cs.pdf|png          N de indicadores CS con dato por celda
#     fig_histograma_ivec_contextual.pdf|png Distribución global del compuesto
#     fig_dispersion_renta_ivec.pdf|png      Dispersión IVEC_contextual × renta INE 2023
#                                            (validación externa, plano 2 contextual)
#
#   TABLAS:
#     tab_distribucion_indicadores.rds       Min, q25, mediana, media, q75, max, sd
#                                            (los 13 indicadores activos en V1)
#     tab_coherencia_subdim.rds              α, ω, N, interpretación por sub-dimensión
#                                            (incluye CS con 4 indicadores activos)
#     tab_correlaciones_intersubdim.rds      Matrices ρ inter-indicador
#                                            (físico 4×4, demográfico 5×5, CS 4×4)
#     tab_resumen_modulo.rds                 Indicadores activos, cobertura, fórmula
#
# Notas:
#   El script se ejecuta una sola vez tras `04_ivec_contextual.R`. Los
#   ficheros producidos se incorporan al cap. 6 mediante chunks de bookdown
#   que los cargan con `readRDS()` o `include_graphics()` según el caso.
#   El estilo cromático sigue la paleta semáforo canónica de la tesis
#   (RdYlGn 5 clases de ColorBrewer: rojo → naranja → amarillo → verde
#   claro → verde oscuro). La orientación cromática se controla con el
#   parámetro `direccion` de mapa_choropleth() según el mecanismo causal
#   de cada indicador, manteniendo en todos los casos la convención
#   "rojo = mayor riesgo" coherente entre módulos.
#
#   Convención cromática: VC (vulnerabilidad) → dirección 1 (rojo = Q5
#   = alta vulnerabilidad). Indicadores CS de tipo "tiempo" donde un
#   valor alto significa peor accesibilidad (transporte, sociosanitario)
#   → dirección 1 (rojo = Q5 = mucho tiempo = peor capacidad). Indicadores
#   CS de tipo "densidad" donde un valor alto significa más capacidad
#   protectora (proximidad, nodos cívicos, CS* agregado) → dirección -1
#   (rojo = Q1 = baja densidad = menor capacidad). En los dos indicadores
#   CS con ceros estructurales (cs_proximidad, cs_nodos_civicos) la clase
#   "0 (sin punto en radio)" se separa explícitamente y recibe el color
#   rojo por interpretarse como ausencia efectiva del recurso.
#
#   Uniformidad geométrica: todos los mapas usan el mismo bbox calculado
#   desde cv_lim con un margen de 2 km, idéntica relación de aspecto y
#   mismas dimensiones de archivo (7 × 8 pulgadas), garantizando que las
#   delimitaciones administrativas y la posición de la Comunitat Valenciana
#   sean visualmente comparables entre figuras.
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(here)

set.seed(1972)

# ── Rutas ────────────────────────────────────────────────────────────────────
ruta_ivec_contextual <- here("data/base/ivec_resultados/ivec_contextual.parquet")
ruta_validacion      <- here("data/base/ivec_resultados/validacion_contextual.rds")
ruta_grid_laea       <- here("data/mallas/malla_laea/CV_grid_1km_laea.gpkg")
ruta_cv_lim          <- here("data/mallas/malla_administrativa/Delimitaciones_CV.gpkg")
ruta_prov_lim        <- here("data/mallas/malla_administrativa/Delimitaciones_provincias.gpkg")

# Rutas para reconstruir la figura de dispersión renta × IVEC_contextual.
# El bloque al final de la sección 4 reconstruye el join sección censal →
# centroide LAEA → renta neta media por persona del Atlas de Distribución
# de Renta INE 2023, en paralelo a lo que produce 05_validacion_externa_contextual.R.
ruta_secciones_shp   <- here("data/mallas/malla_administrativa/ca_seccion_censal/ca_seccion_censal_20260405.shp")
ruta_atlas_renta_dir <- here("data/contextual/atlas_renta")
ATLAS_RENTA_INDICADOR <- "Renta neta media por persona"
ATLAS_RENTA_ANYO      <- "2023"

dir_figuras <- here("outputs/figures/cap-6")
dir_tablas  <- here("outputs/tables/cap-6")
dir.create(dir_figuras, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_tablas,  showWarnings = FALSE, recursive = TRUE)

message("\n══════════════════════════════════════════════════════════════")
message("  06_mapas_tablas_contextual.R — inicio")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA DE DATOS
# =============================================================================

message("── Sección 1: carga de datos ──────────────────────────────────")

ivec_ctxt <- read_parquet(ruta_ivec_contextual)
message("  Celdas cargadas: ", format(nrow(ivec_ctxt), big.mark = "."))

validacion <- if (file.exists(ruta_validacion)) {
  readRDS(ruta_validacion)
} else {
  warning("validacion_contextual.rds no encontrado. Re-ejecutar 04_ivec_contextual.R.")
  list()
}

# Geometría del grid LAEA: lo asociamos a las celdas activas para producir
# los mapas. Si el fichero gpkg no está disponible se reconstruye la
# geometría desde laea_N y laea_E (esquina suroeste, lado 1000 m).
if (file.exists(ruta_grid_laea)) {
  message("  Cargando geometría del grid desde ", basename(ruta_grid_laea), " ...")
  grid_geom <- st_read(ruta_grid_laea, quiet = TRUE) |>
    st_zm(drop = TRUE) |>
    st_transform(3035)
} else {
  message("  Reconstruyendo geometría del grid desde laea_N, laea_E ...")
  grid_geom <- ivec_ctxt |>
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

# Asegurar la columna identificadora de unión
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

# Unir indicadores a geometría
ivec_sf <- grid_geom |>
  select(GRD_ID, any_of("geom"), any_of("geometry")) |>
  inner_join(ivec_ctxt, by = "GRD_ID")
message("  Celdas con geometría tras join: ",
        format(nrow(ivec_sf), big.mark = "."))

# Capas administrativas para el contexto cartográfico
cv_lim <- if (file.exists(ruta_cv_lim)) {
  st_read(ruta_cv_lim, quiet = TRUE) |> st_transform(3035)
} else NULL
prov_lim <- if (file.exists(ruta_prov_lim)) {
  st_read(ruta_prov_lim, quiet = TRUE) |> st_transform(3035)
} else NULL


# =============================================================================
# SECCIÓN 2 — TEMA Y PALETA CANÓNICAS
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

# Paleta semáforo canónica (RdYlGn 5 clases ColorBrewer):
#   rojo intenso → naranja → amarillo → verde claro → verde oscuro
# Convención cromática del módulo: rojo = mayor riesgo. La orientación
# (qué quintil queda en rojo) depende del mecanismo causal del indicador:
#   direccion = 1  → rojo = Q5 (más alto = mayor riesgo)   para VC y para
#                    indicadores CS de tipo "tiempo" donde un valor alto
#                    significa peor accesibilidad (transporte, sociosanitario).
#   direccion = -1 → rojo = Q1 (más bajo = mayor riesgo)   para indicadores
#                    CS de tipo "densidad" donde valor alto = más capacidad
#                    protectora (proximidad, nodos cívicos) y para CS*.
COL_ROJO         <- "#d7191c"
COL_NARANJA      <- "#fdae61"
COL_AMARILLO     <- "#ffffbf"
COL_VERDE_CLARO  <- "#a6d96a"
COL_VERDE_OSCURO <- "#1a9641"

paleta_dir_vc <- 1
paleta_dir_cs <- -1

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

# Paletas para indicadores con ceros estructurales (cs_proximidad y
# cs_nodos_civicos) cuya rama "breaks no únicos" produce un factor con la
# clase "0 (sin punto en radio)" y entre 2 y 4 categorías de positivos.
# El cero estructural corresponde a "ausencia de recurso" = mayor riesgo.
paleta_cs_ceros_riesgo_alto_es_bajo <- c(
  "0 (sin punto en radio)" = COL_ROJO,
  "Q2"                     = COL_NARANJA,
  "Q3"                     = COL_AMARILLO,
  "Q4"                     = COL_VERDE_CLARO,
  "Q5 (más alto)"          = COL_VERDE_OSCURO,
  "Bajo > 0"               = COL_NARANJA,
  "Medio"                  = COL_AMARILLO,
  "Alto"                   = COL_VERDE_OSCURO
)

# Selector de paleta categórica según la dirección
get_paleta <- function(direccion) {
  if (direccion == 1) paleta_riesgo_alto_es_alto
  else                paleta_riesgo_alto_es_bajo
}

# Bbox canónico de la Comunitat Valenciana en EPSG:3035, calculado una sola
# vez desde cv_lim y reutilizado por todos los mapas para asegurar idéntico
# encuadre, idéntica relación de aspecto y tamaño uniforme del archivo PDF/PNG.
# Si cv_lim no está disponible, se cae a un bbox derivado del propio grid.
if (!is.null(cv_lim)) {
  bbox_canonico <- sf::st_bbox(cv_lim)
} else {
  bbox_canonico <- sf::st_bbox(ivec_sf)
}
# Margen de 2 km alrededor para que las geometrías costeras no toquen el borde
BBOX_MARGEN <- 2000
bbox_xlim <- c(bbox_canonico["xmin"] - BBOX_MARGEN,
               bbox_canonico["xmax"] + BBOX_MARGEN)
bbox_ylim <- c(bbox_canonico["ymin"] - BBOX_MARGEN,
               bbox_canonico["ymax"] + BBOX_MARGEN)


# =============================================================================
# SECCIÓN 3 — CONSTRUCCIÓN DE SUB-DIMENSIONES AGREGADAS
# =============================================================================
# Cada sub-dimensión se construye como media de los percentiles de los
# indicadores activos en ella. Las nuevas listas reflejan la arquitectura
# V1 tras la reasignación de mayo de 2026: VC físico-residencial = 4
# indicadores (vc_acceso_eq sale a CS bajo el nombre cs_sanitario).

cols_fisico_p <- c("vc_antiguedad_p", "vc_superficie_p", "vc_tenencia_p",
                   "vc_hacinamiento_p")
cols_demo_p   <- c("vc_envejecimiento_p", "vc_inactividad_p",
                   "vc_estr_migratoria_p", "vc_educacion_p",
                   "vc_dependencia_conv_p")
cols_cs_p     <- c("cs_proximidad_p", "cs_transporte_p",
                   "cs_sanitario_p", "cs_nodos_civicos_p")

ivec_sf <- ivec_sf |>
  rowwise() |>
  mutate(
    vc_fisico_celda = mean(c_across(any_of(cols_fisico_p)), na.rm = TRUE),
    vc_demo_celda   = mean(c_across(any_of(cols_demo_p)),   na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    vc_fisico_celda = ifelse(is.nan(vc_fisico_celda), NA_real_, vc_fisico_celda),
    vc_demo_celda   = ifelse(is.nan(vc_demo_celda),   NA_real_, vc_demo_celda)
  )


# =============================================================================
# SECCIÓN 4 — MAPAS PRINCIPALES
# =============================================================================

message("\n── Sección 4: generación de mapas ──────────────────────────────")

mapa_choropleth <- function(sf_data, columna, titulo, subtitulo, caption,
                            etiqueta_leyenda = NULL,
                            quintiles = TRUE,
                            direccion = 1) {
  if (is.null(etiqueta_leyenda)) etiqueta_leyenda <- columna
  if (quintiles) {
    breaks <- quantile(sf_data[[columna]], probs = seq(0, 1, 0.2),
                       na.rm = TRUE)
    breaks_unicos <- unique(breaks)
    # Indicadores con muchos ceros estructurales (p. ej. cs_proximidad,
    # cs_nodos_civicos) producen quintiles colapsados a 0. Si los breaks no
    # son únicos, se separa la clase "0 (sin punto en radio)" del resto y
    # se cuantilizan los valores positivos en hasta cuatro categorías
    # adicionales, con etiquetas adaptadas al número efectivo de cortes.
    es_caso_ceros <- length(breaks_unicos) < 6
    if (es_caso_ceros) {
      es_cero <- !is.na(sf_data[[columna]]) & sf_data[[columna]] == 0
      valores_pos <- sf_data[[columna]][!is.na(sf_data[[columna]]) &
                                          sf_data[[columna]] > 0]
      n_clases_pos <- min(4, length(unique(valores_pos)))
      if (n_clases_pos >= 2) {
        probs_pos <- seq(0, 1, length.out = n_clases_pos + 1)
        breaks_pos <- unique(quantile(valores_pos, probs = probs_pos,
                                      na.rm = TRUE))
        cat_pos <- cut(sf_data[[columna]], breaks = breaks_pos,
                       include.lowest = TRUE)
        n_lvl <- length(levels(cat_pos))
        if (n_lvl >= 4) {
          labels_pos <- c("Q2", "Q3", "Q4", "Q5 (más alto)")[seq_len(n_lvl)]
        } else if (n_lvl == 3) {
          labels_pos <- c("Bajo > 0", "Medio", "Alto")
        } else {
          labels_pos <- c("Bajo > 0", "Alto")
        }
        levels(cat_pos) <- labels_pos
        cat_pos <- as.character(cat_pos)
      } else {
        cat_pos <- rep(NA_character_, nrow(sf_data))
        labels_pos <- character(0)
      }
      cat_final <- ifelse(es_cero, "0 (sin punto en radio)", cat_pos)
      niveles <- c("0 (sin punto en radio)", labels_pos)
      sf_data$cat <- factor(cat_final, levels = niveles)
    } else {
      sf_data$cat <- cut(sf_data[[columna]], breaks = breaks,
                         include.lowest = TRUE,
                         labels = c("Q1 (más bajo)", "Q2", "Q3", "Q4",
                                    "Q5 (más alto)"))
    }
    # Selección de paleta nombrada según dirección y tipo de caso
    if (es_caso_ceros) {
      valores_paleta <- paleta_cs_ceros_riesgo_alto_es_bajo
    } else {
      valores_paleta <- get_paleta(direccion)
    }
    fill_layer <- geom_sf(aes(fill = cat), color = NA)
    scale_layer <- scale_fill_manual(values = valores_paleta,
                                     name = etiqueta_leyenda,
                                     na.value = "grey85",
                                     drop = FALSE)
  } else {
    # Modo continuo (rara vez usado en este script): gradiente rojo-verde
    # equivalente, con dirección controlada por el signo.
    fill_layer <- geom_sf(aes(fill = .data[[columna]]), color = NA)
    if (direccion == 1) {
      scale_layer <- scale_fill_gradientn(
        colours = c(COL_VERDE_OSCURO, COL_VERDE_CLARO, COL_AMARILLO,
                    COL_NARANJA, COL_ROJO),
        name = etiqueta_leyenda, na.value = "grey85"
      )
    } else {
      scale_layer <- scale_fill_gradientn(
        colours = c(COL_ROJO, COL_NARANJA, COL_AMARILLO,
                    COL_VERDE_CLARO, COL_VERDE_OSCURO),
        name = etiqueta_leyenda, na.value = "grey85"
      )
    }
  }
  p <- ggplot(sf_data) +
    fill_layer +
    scale_layer
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

# Mapa 1: IVEC_contextual compuesto (fórmula compensadora completa)
mapa_ivec <- mapa_choropleth(
  ivec_sf,
  columna = "ivec_contextual",
  titulo = "IVEC_contextual (V1)",
  subtitulo = paste0("Quintiles del compuesto contextual, ",
                     format(nrow(ivec_sf), big.mark = "."),
                     " celdas activas en cuadrícula LAEA 1 km²"),
  caption = paste("Fórmula: ½·VC* + ½·(1−CS*). Trece indicadores activos en V1.",
                  "Fuentes: SIP (envejecimiento, inactividad, estructura migratoria,",
                  "dependencia convivencial), Censo Anual 2025 INE (educación),",
                  "Censo 2021 INE (tenencia), Observatorio Hábitat GVA (antigüedad),",
                  "Catastro Inmobiliario CAT+SHP (superficie, hacinamiento),",
                  "RECESSO + centros docentes 0-3 (proximidad), OSM (transporte),",
                  "Conselleria de Sanitat + RECESSO (sociosanitario),",
                  "centros docentes públicos 3+ + cívicos consolidados (nodos cívicos).",
                  "Elaboración propia."),
  etiqueta_leyenda = "IVEC_contextual\n(quintiles)",
  direccion = paleta_dir_vc
)
guardar_mapa(mapa_ivec, "fig_mapa_ivec_contextual")

# Mapa 1b: VC* agregado (sin compensar)
mapa_vc_star <- mapa_choropleth(
  ivec_sf,
  columna = "VC_star",
  titulo = "VC* — Vulnerabilidad contextual agregada",
  subtitulo = paste0("Min-max sobre las ", format(nrow(ivec_sf), big.mark = "."),
                     " celdas activas. Nueve indicadores VC en dos sub-dimensiones."),
  caption = "Sub-dimensiones: físico-residencial (4 indicadores formativos) + demográfico-poblacional (5 indicadores reflectivos).",
  etiqueta_leyenda = "VC*\n(quintiles)",
  direccion = paleta_dir_vc
)
guardar_mapa(mapa_vc_star, "fig_mapa_vc_star")

# Mapa 1c: CS* agregado
mapa_cs_star <- mapa_choropleth(
  ivec_sf,
  columna = "CS_star",
  titulo = "CS* — Capacidades de soporte social agregadas",
  subtitulo = paste0("Min-max sobre las ", format(nrow(ivec_sf), big.mark = "."),
                     " celdas activas. Cuatro indicadores CS activos en V1."),
  caption = paste("Indicadores: cs_proximidad (RECESSO + infantil 0-3),",
                  "cs_transporte (OSM), cs_sanitario (Centros Salud + RECESSO),",
                  "cs_nodos_civicos (docentes públicos + cívicos consolidados).",
                  "cs_asociativo diferido a V2."),
  etiqueta_leyenda = "CS*\n(quintiles)",
  direccion = paleta_dir_cs
)
guardar_mapa(mapa_cs_star, "fig_mapa_cs_star")

# Mapa 2: Sub-dimensión físico-residencial (4 indicadores)
mapa_fisico <- mapa_choropleth(
  ivec_sf,
  columna = "vc_fisico_celda",
  titulo = "VC físico-residencial",
  subtitulo = "Sub-dimensión físico-residencial (4 indicadores activos): media de percentiles",
  caption = paste("Sub-dimensión formativa: antigüedad del parque, superficie útil media,",
                  "régimen de tenencia, hacinamiento residencial.",
                  "α y ω se reportan descriptivamente por la naturaleza formativa del constructo."),
  etiqueta_leyenda = "VC físico\n(quintiles)",
  direccion = paleta_dir_vc
)
guardar_mapa(mapa_fisico, "fig_mapa_vc_fisico")

# Mapa 3: Sub-dimensión demográfico-poblacional (5 indicadores)
mapa_demo <- mapa_choropleth(
  ivec_sf,
  columna = "vc_demo_celda",
  titulo = "VC demográfico-poblacional",
  subtitulo = "Sub-dimensión demográfico-poblacional (5 indicadores activos): media de percentiles",
  caption = paste("Sub-dimensión reflectiva: envejecimiento territorial, concentración de inactividad,",
                  "estructura migratoria, dependencia convivencial, nivel educativo del entorno.",
                  "α = 0,639; ω = 0,673 (N = 2.811)."),
  etiqueta_leyenda = "VC demográfico\n(quintiles)",
  direccion = paleta_dir_vc
)
guardar_mapa(mapa_demo, "fig_mapa_vc_demografico")

# Mapas individuales de los cuatro indicadores CS activos
# Cada uno se mapea por quintiles del valor bruto (no del percentil) para
# preservar la lectura cuantitativa propia del indicador.

mapa_cs_proximidad <- mapa_choropleth(
  ivec_sf,
  columna = "cs_proximidad",
  titulo = "CS — Proximidad de servicios de cuidado cotidiano",
  subtitulo = paste0("Número de puntos por km² (centros de día RECESSO + infantil 0-3). ",
                     "Mediana: ", round(median(ivec_sf$cs_proximidad, na.rm = TRUE), 1)),
  caption = paste("Composición V1 parcial: farmacias pendientes (V2).",
                  "Valores 0 frecuentes en celdas rurales: distribución asimétrica con cola larga urbana."),
  etiqueta_leyenda = "Puntos/km²\n(quintiles)",
  direccion = paleta_dir_cs
)
guardar_mapa(mapa_cs_proximidad, "fig_mapa_cs_proximidad")

mapa_cs_transporte <- mapa_choropleth(
  ivec_sf,
  columna = "cs_transporte",
  titulo = "CS — Accesibilidad al transporte público",
  subtitulo = paste0("Tiempo a pie a la parada OSM más próxima (minutos). ",
                     "Mediana: ", round(median(ivec_sf$cs_transporte, na.rm = TRUE), 1),
                     " min"),
  caption = paste("Fuente: paradas OSM (bus + ferrocarril + tranvía + metro).",
                  "Velocidad peatonal 4,5 km/h; factor de sinuosidad 1,30.",
                  "Cobertura íntegra de la red en V1."),
  etiqueta_leyenda = "Tiempo (min)\n(quintiles)",
  direccion = paleta_dir_vc   # más tiempo = peor capacidad = rojo
)
guardar_mapa(mapa_cs_transporte, "fig_mapa_cs_transporte")

mapa_cs_sociosanitario <- mapa_choropleth(
  ivec_sf,
  columna = "cs_sanitario",
  titulo = "CS — Accesibilidad sociosanitaria",
  subtitulo = paste0("Tiempo medio a Centros de Salud y RECESSO (minutos). ",
                     "Mediana: ", round(median(ivec_sf$cs_sanitario, na.rm = TRUE), 1),
                     " min"),
  caption = paste("Fuente: Centros de Salud GVA (15_) + RECESSO GVA (19_).",
                  "Indicador reasignado a CS en mayo de 2026 por mecanismo causal",
                  "(soporte institucional del entorno, no condición material)."),
  etiqueta_leyenda = "Tiempo (min)\n(quintiles)",
  direccion = paleta_dir_vc   # más tiempo = peor capacidad = rojo
)
guardar_mapa(mapa_cs_sociosanitario, "fig_mapa_cs_sociosanitario")

mapa_cs_nodos_civicos <- mapa_choropleth(
  ivec_sf,
  columna = "cs_nodos_civicos",
  titulo = "CS — Densidad de nodos cívicos públicos",
  subtitulo = paste0("Número de puntos por km² (docentes públicos 3+ + cívicos consolidados). ",
                     "Mediana: ", round(median(ivec_sf$cs_nodos_civicos, na.rm = TRUE), 1)),
  caption = paste("Composición V1 parcial: bibliotecas pendientes (V2).",
                  "Fuente: Registre de Centres Docents Valencià + GeoJSONs DipCas/DipAli/VLC + OSM.",
                  "Mecanismo causal: capilaridad institucional + socialización intergeneracional."),
  etiqueta_leyenda = "Puntos/km²\n(quintiles)",
  direccion = paleta_dir_cs
)
guardar_mapa(mapa_cs_nodos_civicos, "fig_mapa_cs_nodos_civicos")

# Mapa de cobertura VC por celda (n_indic_vc, máximo = 9)
# Convención cromática: la cobertura es información de control de calidad,
# no medida de riesgo. Se usa una paleta secuencial neutra (YlGnBu de
# ColorBrewer) para no confundir al lector con la paleta semáforo del módulo.
mapa_cobertura_vc <- ggplot(ivec_sf) +
  geom_sf(aes(fill = factor(n_indic_vc)), color = NA) +
  scale_fill_brewer(palette = "YlGnBu", direction = 1,
                    name = "Indicadores\ncon dato",
                    na.value = "grey85")
if (!is.null(prov_lim)) {
  mapa_cobertura_vc <- mapa_cobertura_vc +
    geom_sf(data = prov_lim, fill = NA, color = "grey40", linewidth = 0.3)
}
if (!is.null(cv_lim)) {
  mapa_cobertura_vc <- mapa_cobertura_vc +
    geom_sf(data = cv_lim, fill = NA, color = "black", linewidth = 0.5)
}
mapa_cobertura_vc <- mapa_cobertura_vc +
  coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE,
           crs = sf::st_crs(3035)) +
  labs(
    title    = "Cobertura del módulo VC por celda activa",
    subtitle = paste0("Número de indicadores VC con dato sobre 9 posibles. ",
                      "Media: ", round(mean(ivec_sf$n_indic_vc, na.rm = TRUE), 2),
                      "/9"),
    caption  = "Celdas con cobertura plena (9/9): núcleo del módulo. Celdas con cobertura parcial: limitación documentada."
  ) +
  tema_mapa
guardar_mapa(mapa_cobertura_vc, "fig_mapa_cobertura_vc")

# Mapa de cobertura CS por celda (n_indic_cs, máximo = 5)
# Se computa sobre los 5 slots teóricos, no sobre los 4 activos, para que
# la lectura del mapa refleje la limitación efectiva (cs_asociativo en NA).
mapa_cobertura_cs <- ggplot(ivec_sf) +
  geom_sf(aes(fill = factor(n_indic_cs)), color = NA) +
  scale_fill_brewer(palette = "YlGnBu", direction = 1,
                    name = "Indicadores\ncon dato",
                    na.value = "grey85")
if (!is.null(prov_lim)) {
  mapa_cobertura_cs <- mapa_cobertura_cs +
    geom_sf(data = prov_lim, fill = NA, color = "grey40", linewidth = 0.3)
}
if (!is.null(cv_lim)) {
  mapa_cobertura_cs <- mapa_cobertura_cs +
    geom_sf(data = cv_lim, fill = NA, color = "black", linewidth = 0.5)
}
mapa_cobertura_cs <- mapa_cobertura_cs +
  coord_sf(xlim = bbox_xlim, ylim = bbox_ylim, expand = FALSE,
           crs = sf::st_crs(3035)) +
  labs(
    title    = "Cobertura del módulo CS por celda activa",
    subtitle = paste0("Número de indicadores CS con dato sobre 5 posibles. ",
                      "Media: ", round(mean(ivec_sf$n_indic_cs, na.rm = TRUE), 2),
                      "/5 (cs_asociativo diferido a V2: 4/5 estructural)"),
    caption  = "Cobertura uniforme 4/5 en todas las celdas activas: los cuatro CS activos están definidos en todo el grid LAEA por las redes y catálogos territoriales sin restricción seccional."
  ) +
  tema_mapa
guardar_mapa(mapa_cobertura_cs, "fig_mapa_cobertura_cs")

# Histograma del IVEC_contextual
histograma <- ggplot(ivec_sf, aes(x = ivec_contextual)) +
  geom_histogram(bins = 60, fill = "#21908C", color = "white", linewidth = 0.2) +
  geom_vline(xintercept = median(ivec_sf$ivec_contextual, na.rm = TRUE),
             linetype = "dashed", color = "grey20") +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  labs(
    title    = "Distribución del IVEC_contextual en la cuadrícula activa",
    subtitle = paste0("Mediana = ",
                      round(median(ivec_sf$ivec_contextual, na.rm = TRUE), 3),
                      "; media = ",
                      round(mean(ivec_sf$ivec_contextual, na.rm = TRUE), 3),
                      "; desviación típica = ",
                      round(sd(ivec_sf$ivec_contextual, na.rm = TRUE), 3),
                      "; N = ", format(sum(!is.na(ivec_sf$ivec_contextual)),
                                       big.mark = ".")),
    x        = "IVEC_contextual = ½·VC* + ½·(1−CS*)",
    y        = "Número de celdas",
    caption  = paste("Línea discontinua: mediana. Fórmula compensadora completa con VC* y CS* tras la activación",
                     "de los cuatro indicadores CS en mayo de 2026. La concentración en torno a la mediana",
                     "(sd = 0,114) refleja la operación compensadora entre vulnerabilidad y soporte territorial.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey20"),
    plot.caption  = element_text(size = 8, color = "grey30", hjust = 0)
  )

ggsave(file.path(dir_figuras, "fig_histograma_ivec_contextual.pdf"),
       histograma, device = cairo_pdf, width = 8, height = 5)
ggsave(file.path(dir_figuras, "fig_histograma_ivec_contextual.png"),
       histograma, dpi = 600, width = 8, height = 5, bg = "white")
message("  Guardado: fig_histograma_ivec_contextual.pdf|png")


# ── Figura de dispersión renta × IVEC_contextual ─────────────────────────────
# Reconstrucción autosuficiente del join renta × IVEC para producir la figura
# de validez convergente externa (paralela a fig_convergente_ivecbase_d7 del
# módulo base). Replica el procedimiento de 05_validacion_externa_contextual.R
# (lectura de los tres CSV provinciales del Atlas de Renta INE, filtrado al
# indicador "Renta neta media por persona" 2023, asignación al centroide LAEA
# de cada celda mediante st_within sobre las secciones censales) para no
# depender del estado de memoria de 05 y permitir la ejecución aislada de 06.
atlas_renta_csvs <- if (dir.exists(ruta_atlas_renta_dir)) {
  list.files(ruta_atlas_renta_dir,
             pattern = "(alicante|castellon|castellón|valencia|valència)\\.csv$",
             full.names = TRUE, ignore.case = TRUE)
} else {
  character(0)
}

if (length(atlas_renta_csvs) >= 1 && file.exists(ruta_secciones_shp)) {
  message("  Construyendo figura de dispersión renta × IVEC_contextual ...")
  renta_raw <- purrr::map_dfr(atlas_renta_csvs, ~ readr::read_delim(
    .x, delim = ";",
    locale = readr::locale(encoding = "UTF-8", grouping_mark = "."),
    show_col_types = FALSE
  )) |>
    dplyr::rename_with(~ "municipios",       .cols = dplyr::starts_with("Municipios")) |>
    dplyr::rename_with(~ "distritos",        .cols = dplyr::starts_with("Distritos")) |>
    dplyr::rename_with(~ "seccion_etiqueta", .cols = dplyr::starts_with("Secciones")) |>
    dplyr::rename_with(~ "indicador",        .cols = dplyr::starts_with("Indicadores")) |>
    dplyr::rename_with(~ "periodo",          .cols = dplyr::starts_with("Periodo")) |>
    dplyr::rename_with(~ "total_chr",        .cols = dplyr::starts_with("Total"))

  renta_sec <- renta_raw |>
    dplyr::filter(
      !is.na(seccion_etiqueta) & seccion_etiqueta != "",
      indicador == ATLAS_RENTA_INDICADOR,
      as.character(periodo) == ATLAS_RENTA_ANYO
    ) |>
    dplyr::mutate(
      cusec       = stringr::str_extract(seccion_etiqueta, "^[0-9]{10}"),
      renta_media = suppressWarnings(as.numeric(
        stringr::str_replace_all(as.character(total_chr), "\\.", "")
      ))
    ) |>
    dplyr::filter(!is.na(cusec), !is.na(renta_media)) |>
    dplyr::select(cusec, renta_media)

  secciones <- sf::st_read(ruta_secciones_shp, quiet = TRUE) |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::rename(cusec = ID_SECCION) |>
    dplyr::select(cusec, geometry)

  centroides_celdas <- ivec_ctxt |>
    dplyr::select(GRD_ID, laea_N, laea_E, ivec_contextual) |>
    dplyr::mutate(x_c = laea_E + 500L, y_c = laea_N + 500L) |>
    sf::st_as_sf(coords = c("x_c", "y_c"), crs = 3035, remove = FALSE)

  cor_renta <- sf::st_join(centroides_celdas, secciones,
                           join = sf::st_within) |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::left_join(renta_sec, by = "cusec") |>
    dplyr::filter(!is.na(ivec_contextual), !is.na(renta_media)) |>
    dplyr::select(GRD_ID, ivec_contextual, renta_media)

  # Estadísticos del ajuste
  rho_sp <- cor.test(cor_renta$ivec_contextual, cor_renta$renta_media,
                     method = "spearman", exact = FALSE)
  rho_v  <- round(rho_sp$estimate, 4)
  n_cel  <- nrow(cor_renta)
  label_rho <- paste0("ρ = ", gsub("\\.", ",", sprintf("%.4f", rho_v)),
                      "  (p < 0,001)")
  med_renta <- median(cor_renta$renta_media,     na.rm = TRUE)
  med_ivec  <- median(cor_renta$ivec_contextual, na.rm = TRUE)

  fig_dispersion_renta <- ggplot(cor_renta,
                                 aes(x = renta_media, y = ivec_contextual)) +
    geom_vline(xintercept = med_renta, colour = "grey70",
               linetype = "dotted", linewidth = 0.5) +
    geom_hline(yintercept = med_ivec, colour = "grey70",
               linetype = "dotted", linewidth = 0.5) +
    geom_point(alpha = 0.20, size = 0.7, colour = "#2c5f8a", stroke = 0) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                colour = "grey55", linetype = "dashed", linewidth = 0.6) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE,
                colour = "#b0320a", fill = "#f4a582", alpha = 0.25,
                linewidth = 0.9, span = 0.75) +
    annotate("label", x = quantile(cor_renta$renta_media, 0.88, na.rm = TRUE),
             y = 0.92, label = label_rho, size = 3.2, colour = "#1F497D",
             fontface = "italic", fill = "white", label.size = 0.3,
             label.padding = unit(0.2, "lines")) +
    scale_x_continuous(
      name   = "Renta neta media por persona en la sección censal (€, Atlas INE 2023)",
      labels = function(x) format(x, big.mark = ".", decimal.mark = ",",
                                   scientific = FALSE)
    ) +
    scale_y_continuous(
      name   = "IVEC_contextual (valor compuesto por cuadrícula)",
      limits = c(0, 1),
      breaks = seq(0, 1, 0.2),
      labels = function(x) gsub("\\.", ",", x)
    ) +
    labs(
      caption = paste0(
        "N = ", format(n_cel, big.mark = "."),
        " celdas con renta seccional asignada · ",
        "Líneas grises punteadas = medianas de ambas variables · ",
        "Curva roja = LOESS (span = 0,75) · ",
        "Línea gris discontinua = regresión lineal · ",
        "Fuente: Atlas de Distribución de Renta de los Hogares INE, año 2023."
      )
    ) +
    theme_minimal(base_size = 10.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey93"),
      axis.title       = element_text(size = 9),
      axis.text        = element_text(size = 8),
      plot.caption     = element_text(size = 6.8, colour = "grey50", hjust = 0)
    )

  ggsave(file.path(dir_figuras, "fig_dispersion_renta_ivec.pdf"),
         fig_dispersion_renta, device = cairo_pdf, width = 8, height = 5)
  ggsave(file.path(dir_figuras, "fig_dispersion_renta_ivec.png"),
         fig_dispersion_renta, dpi = 600, width = 8, height = 5, bg = "white")
  message("  Guardado: fig_dispersion_renta_ivec.pdf|png (N = ", n_cel,
          " celdas, ρ = ", rho_v, ")")
  rm(renta_raw, renta_sec, secciones, centroides_celdas, cor_renta,
     fig_dispersion_renta)
} else {
  message("  fig_dispersion_renta_ivec: PENDIENTE (Atlas Renta o secciones no encontrados).")
}


# =============================================================================
# SECCIÓN 5 — TABLAS DE RESULTADOS
# =============================================================================

message("\n── Sección 5: generación de tablas ─────────────────────────────")

# Tabla 1: Distribución de cada indicador del módulo (13 activos V1)
indicadores_modulo <- c(
  # VC físico-residencial (4)
  "vc_antiguedad", "vc_superficie", "vc_tenencia", "vc_hacinamiento",
  # VC demográfico-poblacional (5)
  "vc_envejecimiento", "vc_inactividad", "vc_estr_migratoria",
  "vc_educacion", "vc_dependencia_conv",
  # CS (4 activos)
  "cs_proximidad", "cs_transporte", "cs_sanitario",
  "cs_nodos_civicos"
)

resumen_indicador <- function(x, etiqueta) {
  tibble(
    Indicador = etiqueta,
    N_validos = sum(!is.na(x)),
    Cobertura = sum(!is.na(x)) / length(x),
    Minimo    = min(x, na.rm = TRUE),
    Q25       = quantile(x, 0.25, na.rm = TRUE),
    Mediana   = median(x, na.rm = TRUE),
    Media     = mean(x, na.rm = TRUE),
    Q75       = quantile(x, 0.75, na.rm = TRUE),
    Maximo    = max(x, na.rm = TRUE),
    Desv_tip  = sd(x, na.rm = TRUE)
  )
}

tabla_distribucion <- map_dfr(indicadores_modulo, function(ind) {
  if (!ind %in% names(ivec_ctxt)) return(NULL)
  resumen_indicador(ivec_ctxt[[ind]], ind)
})

saveRDS(tabla_distribucion,
        file.path(dir_tablas, "tab_distribucion_indicadores.rds"))
message("  Guardado: tab_distribucion_indicadores.rds")

# Tabla 2: Coherencia interna por sub-dimensión (incluye CS activa)
tabla_coherencia <- tibble(
  Sub_dimension = c("VC demográfico-poblacional", "VC físico-residencial", "CS"),
  Tipo_constructo = c("Reflectivo",
                      "Formativo (Bollen & Lennox 1991)",
                      "Formativo (Bollen & Lennox 1991)"),
  N_indicadores = c(validacion$n_activos_demo   %||% NA_integer_,
                    validacion$n_activos_fisico %||% NA_integer_,
                    validacion$n_activos_cs     %||% NA_integer_),
  Alpha_Cronbach = c(validacion$alpha_demo_v1   %||% NA_real_,
                     validacion$alpha_fisico_v1 %||% NA_real_,
                     validacion$alpha_cs_v1     %||% NA_real_),
  Omega_McDonald = c(validacion$omega_demo_v1   %||% NA_real_,
                     validacion$omega_fisico_v1 %||% NA_real_,
                     validacion$omega_cs_v1     %||% NA_real_),
  Interpretacion = c(
    "α y ω en el umbral (α = 0,639; ω = 0,673). Constructo reflectivo válido con cinco mecanismos demográficos coherentes a nivel de celda.",
    "α y ω descriptivos: la sub-dimensión es formativa (cuatro mecanismos materiales heterogéneos: antigüedad, superficie, tenencia, hacinamiento); los criterios reflectivos no aplican.",
    "α y ω descriptivos: el módulo CS mezcla densidad de puntos (proximidad, nodos cívicos) con accesibilidad temporal (transporte, sociosanitario); ω = 0,648 refleja covariación residual entre los cuatro indicadores activos, α = 0,315 confirma la heterogeneidad de mecanismos."
  )
)
saveRDS(tabla_coherencia, file.path(dir_tablas, "tab_coherencia_subdim.rds"))
message("  Guardado: tab_coherencia_subdim.rds")

# Tabla 3: Correlaciones inter-indicador por sub-dimensión
# Las tres matrices: físico 4×4, demográfico 5×5, CS 4×4.
matriz_fisico <- ivec_ctxt |>
  select(any_of(c("vc_antiguedad", "vc_superficie", "vc_tenencia",
                  "vc_hacinamiento"))) |>
  cor(method = "spearman", use = "pairwise.complete.obs")
matriz_demo <- ivec_ctxt |>
  select(any_of(c("vc_envejecimiento", "vc_inactividad",
                  "vc_estr_migratoria", "vc_educacion",
                  "vc_dependencia_conv"))) |>
  cor(method = "spearman", use = "pairwise.complete.obs")
matriz_cs <- ivec_ctxt |>
  select(any_of(c("cs_proximidad", "cs_transporte",
                  "cs_sanitario", "cs_nodos_civicos"))) |>
  cor(method = "spearman", use = "pairwise.complete.obs")

tabla_correlaciones <- list(
  fisico_residencial      = matriz_fisico,
  demografico_poblacional = matriz_demo,
  capacidades_soporte     = matriz_cs
)
saveRDS(tabla_correlaciones,
        file.path(dir_tablas, "tab_correlaciones_intersubdim.rds"))
message("  Guardado: tab_correlaciones_intersubdim.rds")

# Tabla 4: Resumen del módulo
tabla_resumen <- tibble(
  Concepto = c("Celdas activas",
               "Indicadores en tabla operativa",
               "Indicadores activos en pipeline V1",
               "Cobertura VC media",
               "Cobertura CS media",
               "VC* (media; desviación típica)",
               "CS* (media; desviación típica)",
               "IVEC_contextual medio",
               "IVEC_contextual mediana",
               "IVEC_contextual desviación típica",
               "Fórmula aplicada V1",
               "Inmuebles residenciales Catastro asignados al grid"),
  Valor = c(
    format(nrow(ivec_ctxt), big.mark = "."),
    "14 (4 VC físico-residencial + 5 VC demográfico-poblacional + 5 CS)",
    paste0(validacion$n_indicadores_activos_v1 %||% NA, " / 14 (",
           validacion$n_activos_fisico %||% NA, "/4 físico-residencial; ",
           validacion$n_activos_demo   %||% NA, "/5 demográfico-poblacional; ",
           validacion$n_activos_cs     %||% NA, "/5 CS)"),
    paste0(round(mean(ivec_ctxt$n_indic_vc, na.rm = TRUE), 2), " / 9"),
    paste0(round(mean(ivec_ctxt$n_indic_cs, na.rm = TRUE), 2), " / 5"),
    paste0(round(mean(ivec_ctxt$VC_star, na.rm = TRUE), 3), "; ",
           round(sd(ivec_ctxt$VC_star, na.rm = TRUE), 3)),
    paste0(round(mean(ivec_ctxt$CS_star, na.rm = TRUE), 3), "; ",
           round(sd(ivec_ctxt$CS_star, na.rm = TRUE), 3)),
    round(mean(ivec_ctxt$ivec_contextual, na.rm = TRUE), 3),
    round(median(ivec_ctxt$ivec_contextual, na.rm = TRUE), 3),
    round(sd(ivec_ctxt$ivec_contextual, na.rm = TRUE), 3),
    validacion$formula_aplicada %||% "—",
    "3.085.808 (Alicante: 1.260.478; Castellón: 414.323; Valencia: 1.411.007)"
  )
)
saveRDS(tabla_resumen, file.path(dir_tablas, "tab_resumen_modulo.rds"))
message("  Guardado: tab_resumen_modulo.rds")


# =============================================================================
# SECCIÓN 6 — RESUMEN EN CONSOLA
# =============================================================================

message("\n══════════════════════════════════════════════════════════════")
message("  Mapas y tablas del IVEC_contextual generados")
message("══════════════════════════════════════════════════════════════")
message("  Figuras en: ", dir_figuras)
message("    - fig_mapa_ivec_contextual.pdf|png")
message("    - fig_mapa_vc_star.pdf|png")
message("    - fig_mapa_cs_star.pdf|png")
message("    - fig_mapa_vc_fisico.pdf|png")
message("    - fig_mapa_vc_demografico.pdf|png")
message("    - fig_mapa_cs_proximidad.pdf|png")
message("    - fig_mapa_cs_transporte.pdf|png")
message("    - fig_mapa_cs_sociosanitario.pdf|png")
message("    - fig_mapa_cs_nodos_civicos.pdf|png")
message("    - fig_mapa_cobertura_vc.pdf|png")
message("    - fig_mapa_cobertura_cs.pdf|png")
message("    - fig_histograma_ivec_contextual.pdf|png")
message("")
message("  Tablas en: ", dir_tablas)
message("    - tab_distribucion_indicadores.rds  (13 indicadores activos)")
message("    - tab_coherencia_subdim.rds         (3 sub-dimensiones)")
message("    - tab_correlaciones_intersubdim.rds (matrices 4×4, 5×5, 4×4)")
message("    - tab_resumen_modulo.rds")
message("══════════════════════════════════════════════════════════════\n")
