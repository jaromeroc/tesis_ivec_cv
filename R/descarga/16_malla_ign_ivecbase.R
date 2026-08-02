# =============================================================================
# CUADRÍCULA LAEA 1×1 km — MARCO CARTOGRÁFICO DE REFERENCIA DEL IVEC_base
# Variable: rejilla ETRS89-LAEA (EPSG:3035) de 1×1 km para la Comunitat Valenciana
# Fuente: Eurostat / GEOSTAT — Census Grid 2021 (JRC, @schiavina2023)
# Escala: Cuadrícula 1 km² (GRD_ID formato CRS3035RES1000mN{N}E{E})
#
# ENTRADA (descarga MANUAL): el shapefile canónico CV_grid_1km.shp se obtuvo a
#   mano de Eurostat/GEOSTAT y se colocó en data/Literatura/GRID/CV_shapefiles/.
#   Este script lo verifica, lo procesa y construye los derivados; no realiza
#   descarga automática (ver R/descarga/README.md).
# =============================================================================
# NOTA sobre CRS:
#   — Sistema de referencia del instrumento: ETRS89-LAEA (EPSG:3035)
#   — Identificadores de celda (GRD_ID): coordenadas LAEA redondeadas al km
#       Formato: CRS3035RES1000mN{N}E{E}
#       Ejemplo: CRS3035RES1000mN1707000E3369000
#   — Geometría almacenada en el shapefile: ETRS89 UTM huso 30N (EPSG:25830)
#       por compatibilidad cartográfica con capas ICV y cartografía GVA.
#       Las capas externas (PATRICOVA, CV05) se reprojectan a LAEA antes
#       de cualquier operación de superposición con el IVEC.
# =============================================================================
# Campos del shapefile CV_grid_1km.shp (data/Literatura/GRID/CV_shapefiles/):
#   GRD_ID      — identificador único LAEA (campo de cruce principal)
#   obs_value_  — población residente (Eurostat Census 2021)
#   Shape_Leng  — perímetro (metros, CRS del fichero)
#   Shape_Area  — superficie (m², CRS del fichero)
#   OBJECTID    — clave interna del shapefile (no usar como identificador)
# =============================================================================
# Fuente del shapefile canónico:
#   Eurostat Census Grid 2021 (GEOSTAT/JRC):
#   https://ec.europa.eu/eurostat/web/gisco/geodata/population-distribution/geostat
#   Producto: CENSUS_INS21ES_A_IT_2021 — España
#   CRS nativo: ETRS89-LAEA (EPSG:3035) → almacenado aquí en EPSG:25830
# =============================================================================

library(tidyverse)
library(sf)
library(arrow)
library(here)

# Ruta al shapefile canónico ya disponible en el proyecto
ruta_grid_canon <- here("data/Literatura/GRID/CV_shapefiles/CV_grid_1km.shp")

# Directorio de salida para derivados procesados
dir_out <- here("data/mallas/malla_laea")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)


# =============================================================================
# SECCIÓN 1 — VERIFICAR Y CARGAR EL SHAPEFILE CANÓNICO
# =============================================================================
# El fichero CV_grid_1km.shp ya está disponible en el proyecto.
# Contiene las 8.713 celdas activas de la CV con identificadores GRD_ID
# en formato LAEA (CRS3035RES1000mN{N}E{E}) y población del Census 2021.
#
# Si necesitas regenerar el fichero desde la fuente Eurostat:
#   1. Ir a: https://ec.europa.eu/eurostat/web/gisco/geodata/population-distribution/geostat
#   2. Descargar: CENSUS_INS21ES_A_IT_2021_0000_TOTAL_POPULATION (shapefile)
#   3. Filtrar por NUTS2 = ES52 (Comunitat Valenciana)
#   4. Guardar en: data/Literatura/GRID/CV_shapefiles/CV_grid_1km.shp
# =============================================================================

if (!file.exists(ruta_grid_canon)) {
  stop(
    "Shapefile canónico no encontrado: ", ruta_grid_canon, "\n",
    "Ver instrucciones de descarga en la cabecera de este script."
  )
}

message("  Cargando cuadrícula LAEA desde: ", basename(ruta_grid_canon), "...")
grid_cv <- st_read(ruta_grid_canon, quiet = TRUE)
message("  Celdas cargadas: ", nrow(grid_cv))
message("  CRS del fichero: ", st_crs(grid_cv)$input)
message("  Campos: ", paste(names(grid_cv), collapse = ", "))

# Verificar que GRD_ID existe y tiene el formato LAEA esperado
if (!"GRD_ID" %in% names(grid_cv)) {
  stop("El campo GRD_ID no existe en el shapefile. Verificar la fuente.")
}
ejemplo_grd <- head(grid_cv$GRD_ID, 1)
if (!grepl("^CRS3035RES1000m", ejemplo_grd)) {
  warning("GRD_ID no sigue el formato canónico CRS3035RES1000mN{N}E{E}. ",
          "Primer valor: ", ejemplo_grd)
}
message("  Ejemplo GRD_ID: ", ejemplo_grd)

# Guardar copia procesada en GeoPackage (formato más eficiente)
ruta_grid_gpkg <- file.path(dir_out, "CV_grid_1km_laea.gpkg")
st_write(grid_cv, ruta_grid_gpkg, delete_dsn = TRUE, quiet = TRUE)
message("  Grid guardado en: ", ruta_grid_gpkg)


# =============================================================================
# SECCIÓN 2 — VERIFICACIÓN DEL CRS Y METADATOS
# =============================================================================
# La geometría está almacenada en EPSG:25830 (UTM30N) por compatibilidad
# cartográfica. Los identificadores GRD_ID referencian EPSG:3035 (LAEA).
# No se modifica el CRS de almacenamiento: las operaciones de superposición
# con capas externas (ICV, PATRICOVA) exigen reprojectar esas capas a LAEA,
# no el grid a UTM30.
# =============================================================================

epsg_actual <- st_crs(grid_cv)$epsg

if (!is.na(epsg_actual) && epsg_actual == 3035) {
  message("  CRS: ETRS89-LAEA (EPSG:3035) — coherente con GRD_ID.")
} else if (!is.na(epsg_actual) && epsg_actual == 25830) {
  message("  CRS geometría: ETRS89 UTM30N (EPSG:25830) — almacenamiento cartográfico.")
  message("  Identificadores GRD_ID en LAEA (EPSG:3035) — correcto según protocolo IVEC.")
} else {
  warning("  CRS inesperado: ", st_crs(grid_cv)$input,
          ". Verificar coherencia con el pipeline del IVEC_base.")
}

# Estadísticas básicas de cobertura
message("\n  Resumen de cobertura:")
message("  Total celdas: ", nrow(grid_cv))
if ("obs_value_" %in% names(grid_cv)) {
  pop_total <- sum(grid_cv$obs_value_, na.rm = TRUE)
  message("  Población total (Census 2021): ", format(pop_total, big.mark = "."))
  message("  Celdas con población > 0: ", sum(grid_cv$obs_value_ > 0, na.rm = TRUE))
}


# =============================================================================
# SECCIÓN 3 — FALLBACK: REJILLA DESDE COORDENADAS SIP (si no hay shapefile)
# Genera GRD_ID canónicos reprojectando ST_X/ST_Y (UTM30N) a LAEA (EPSG:3035)
# antes de construir el identificador. Usar solo si CV_grid_1km.shp no está.
# =============================================================================

construir_rejilla_desde_sip <- function(ruta_sip_parquet) {

  message("  Construyendo rejilla desde coordenadas SIP (fallback)...")
  df_sip <- read_parquet(ruta_sip_parquet,
                          col_select = c("id_individuo", "ST_X", "ST_Y"))

  # Filtrar coordenadas válidas en bbox CV (ETRS89 UTM30N, EPSG:25830)
  # CV real: X [620000, 900000], Y [4175000, 4550000]
  # Límite occidental 620 000 para incluir Rincón de Ademuz, Los Serranos
  # y Serranía (centroides UTM30N en rango X [634 000–695 000])
  df_validos <- df_sip %>%
    filter(!is.na(ST_X), !is.na(ST_Y),
           ST_X > 620000, ST_X < 900000,
           ST_Y > 4175000, ST_Y < 4550000)

  message("  Registros con coordenadas válidas en CV: ",
          format(nrow(df_validos), big.mark = "."))

  # Convertir a sf en UTM30N
  pts_utm <- df_validos %>%
    st_as_sf(coords = c("ST_X", "ST_Y"), crs = 25830, remove = FALSE)

  # Reprojectar a LAEA (EPSG:3035) para construir GRD_ID canónico
  pts_laea <- pts_utm %>% st_transform(3035)
  coords_laea <- st_coordinates(pts_laea)

  df_validos <- df_validos %>%
    mutate(
      laea_N = floor(coords_laea[, 2] / 1000) * 1000L,
      laea_E = floor(coords_laea[, 1] / 1000) * 1000L,
      GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E),
      # Vértice suroeste en UTM30N (para compatibilidad con el grid almacenado)
      c_x_utm = floor(ST_X / 1000) * 1000L,
      c_y_utm = floor(ST_Y / 1000) * 1000L
    )

  # Agregar por celda LAEA
  celdas <- df_validos %>%
    st_drop_geometry() %>%
    group_by(GRD_ID, laea_N, laea_E, c_x_utm, c_y_utm) %>%
    summarise(
      n_individuos = n(),
      centroide_x  = first(c_x_utm) + 500L,
      centroide_y  = first(c_y_utm) + 500L,
      .groups = "drop"
    ) %>%
    filter(n_individuos >= 15)  # umbral densidad mínima (D2)

  message("  Celdas activas con >= 15 individuos: ", nrow(celdas))

  # Convertir a sf con geometría en UTM30N (compatibilidad cartográfica)
  celdas_sf <- celdas %>%
    st_as_sf(coords = c("centroide_x", "centroide_y"), crs = 25830)

  celdas_sf
}

# Activar el fallback solo si el shapefile canónico no está disponible
ruta_sip <- here("data/SIP/results/SIP_final.parquet")

if (!file.exists(ruta_grid_canon) && file.exists(ruta_sip)) {
  message("\n  Shapefile canónico no disponible — activando fallback SIP...")
  malla_sip <- construir_rejilla_desde_sip(ruta_sip)
  ruta_malla_sip <- file.path(dir_out, "CV_grid_1km_desde_sip.gpkg")
  st_write(malla_sip, ruta_malla_sip, delete_dsn = TRUE, quiet = TRUE)
  message("  Rejilla fallback guardada: ", nrow(malla_sip), " celdas activas")
  message("  AVISO: usar solo como aproximación provisional.")
  message("  GRD_ID generados desde SIP son compatibles con el formato LAEA canónico.")
}


# =============================================================================
# SECCIÓN 4 — CRUCE DEL GRID CON SECCIONES CENSALES Y MUNICIPIOS
# Necesario para integrar IVEC_base (cuadrícula) con IVEC_contextual
# (sección censal) e IVEC_estructural (municipio/comarca).
# La tabla de cruce permite asignar a cada celda GRD_ID su CUSEC y CMUN.
# =============================================================================

cruzar_grid_con_divisiones <- function(ruta_grid,
                                        ruta_secciones_censales,
                                        ruta_municipios) {
  grid      <- st_read(ruta_grid, quiet = TRUE)
  secciones <- st_read(ruta_secciones_censales, quiet = TRUE) %>%
    select(CUSEC, CMUN, geometry) %>%
    st_transform(st_crs(grid))   # reprojectar al CRS del grid (25830)
  municipios <- st_read(ruta_municipios, quiet = TRUE) %>%
    select(CMUN, geometry) %>%
    st_transform(st_crs(grid))

  # El centroide de cada celda determina su sección censal y municipio
  centroides_grid <- grid %>% st_centroid()

  # Unión espacial: celda → sección censal (por centroide)
  grid_con_seccion <- centroides_grid %>%
    st_join(secciones %>% select(CUSEC, CMUN), join = st_within) %>%
    st_drop_geometry() %>%
    select(GRD_ID, CUSEC, CMUN)

  n_asignadas <- sum(!is.na(grid_con_seccion$CUSEC))
  n_total     <- nrow(grid_con_seccion)
  message("  Cruce grid-secciones: ", n_asignadas, " / ", n_total,
          " celdas asignadas (",
          round(100 * n_asignadas / n_total, 1), "%)")

  grid_con_seccion
}

ruta_secciones  <- here("data/contextual/cartografia/secciones_censales_cv.gpkg")
ruta_municipios <- here("data/estructural/cartografia/municipios_cv.gpkg")

if (file.exists(ruta_grid_gpkg) &&
    file.exists(ruta_secciones) &&
    file.exists(ruta_municipios)) {

  message("\n  Ejecutando cruce grid-secciones-municipios...")
  tabla_cruce <- cruzar_grid_con_divisiones(
    ruta_grid_gpkg, ruta_secciones, ruta_municipios
  )
  ruta_cruce <- file.path(dir_out, "cruce_GRD_ID_seccion_municipio.rds")
  saveRDS(tabla_cruce, ruta_cruce)
  message("  Tabla de cruce guardada: ", nrow(tabla_cruce), " celdas → ", ruta_cruce)

} else {
  mensaje_pendiente <- character(0)
  if (!file.exists(ruta_grid_gpkg))    mensaje_pendiente <- c(mensaje_pendiente, "grid LAEA")
  if (!file.exists(ruta_secciones))    mensaje_pendiente <- c(mensaje_pendiente, "secciones censales")
  if (!file.exists(ruta_municipios))   mensaje_pendiente <- c(mensaje_pendiente, "municipios")
  message("  PENDIENTE: no disponible(s): ", paste(mensaje_pendiente, collapse = ", "))
  message("  Cartografía INE: https://www.ine.es/ss/Satellite?",
          "c=Page&p=1259952026632&pagename=ProductosYServicios/PYSLayout&",
          "cid=1259952026632")
}
