# =============================================================================
# 12_cruce_grd_municipio.R — Construcción del cruce celda LAEA 1 km² → municipio
# -----------------------------------------------------------------------------
# Produce el fichero de correspondencia GRD_ID → CMUN que las tablas de
# superposición del capítulo 6 (tab-configuraciones y tab-nucleos-coincidencia)
# necesitan para agregar las celdas a escala municipal y para incorporar el
# CAD_star municipal del módulo estructural a cada celda.
#
# Método: asignación de cada celda al municipio que contiene su CENTROIDE
# (asignación unívoca celda→municipio), con la misma derivación canónica del
# código municipal de 5 dígitos que usa 09_mapas_tablas_estructural.R.
#
# Entradas:
#   data/mallas/malla_laea/CV_grid_1km_laea.gpkg
#   data/mallas/malla_administrativa/Delimitaciones_municipios.gpkg (ICV.Municipios)
#   data/estructural/ivec_estructural/ivec_estructural.parquet  (nombre, comarca)
#
# Salida:
#   data/mallas/malla_laea/cruce_GRD_ID_seccion_municipio.rds
#     columnas: GRD_ID, CMUN, nombre, comarca
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(sf)
  library(dplyr)
  library(stringr)
  library(arrow)
})

message("\n── 12_cruce_grd_municipio.R — inicio ──────────────────────────")

ruta_grid       <- here("data/mallas/malla_laea/CV_grid_1km_laea.gpkg")
ruta_municipios <- here("data/mallas/malla_administrativa/Delimitaciones_municipios.gpkg")
ruta_estr       <- here("data/estructural/ivec_estructural/ivec_estructural.parquet")
ruta_salida     <- here("data/mallas/malla_laea/cruce_GRD_ID_seccion_municipio.rds")

# ── Grid: centroides en EPSG:3035 ─────────────────────────────────────────────
grid <- st_read(ruta_grid, quiet = TRUE) |>
  st_zm(drop = TRUE) |>
  st_transform(3035)
if (!"GRD_ID" %in% names(grid)) {
  stop("El grid no expone la columna GRD_ID. Campos: ",
       paste(names(grid), collapse = ", "))
}
grid_cent <- st_centroid(grid[, "GRD_ID"])
message("  Celdas en el grid: ", format(nrow(grid_cent), big.mark = "."))

# ── Municipios: CMUN de 5 dígitos, filtrado a las tres provincias CV ──────────
municipios <- st_read(ruta_municipios, layer = "ICV.Municipios", quiet = TRUE) |>
  st_zm(drop = TRUE) |>
  st_transform(3035)

cmun_field <- intersect(c("cod_ine_mun", "CMUN", "CODIGO", "COD_INE", "NATCODE",
                          "MUNICIPIO_CODE", "MUN_CODE", "cod_ine"),
                        names(municipios))[1]
if (is.na(cmun_field)) {
  stop("La geometría municipal no expone un campo identificador reconocible. ",
       "Campos: ", paste(names(municipios), collapse = ", "))
}
municipios <- municipios |>
  mutate(CMUN = str_pad(str_extract(as.character(.data[[cmun_field]]), "\\d+"),
                        width = 5, side = "left", pad = "0")) |>
  filter(str_sub(CMUN, 1, 2) %in% c("03", "12", "46")) |>
  select(CMUN)

# ── Asignación celda → municipio por contención del centroide ─────────────────
cruce_sf <- st_join(grid_cent, municipios, join = st_within, left = TRUE) |>
  distinct(GRD_ID, .keep_all = TRUE)

# Las celdas litorales/de frontera cuyo centroide cae fuera de todo polígono
# municipal (centroide sobre el mar) quedan con CMUN NA. Se reasignan al municipio
# MÁS CERCANO mediante st_nearest_feature, para que ninguna celda activa quede sin
# municipio en las tablas y mapas por municipio (corrección de mayo de 2026; antes
# producían una fila anónima en tab-nucleos-coincidencia y celdas HH sin asignar).
sin_mun <- is.na(cruce_sf$CMUN)
if (any(sin_mun)) {
  idx_near <- st_nearest_feature(cruce_sf[sin_mun, ], municipios)
  cruce_sf$CMUN[sin_mun] <- municipios$CMUN[idx_near]
  message("  Celdas costeras/frontera reasignadas al municipio más cercano: ",
          format(sum(sin_mun), big.mark = "."))
}

cruce <- cruce_sf |>
  st_drop_geometry() |>
  distinct(GRD_ID, .keep_all = TRUE)

n_sin <- sum(is.na(cruce$CMUN))
message("  Celdas asignadas a municipio: ",
        format(sum(!is.na(cruce$CMUN)), big.mark = "."),
        " | sin municipio (frontera/costa): ", n_sin)

# ── Nombre y comarca desde el panel estructural ───────────────────────────────
if (file.exists(ruta_estr)) {
  estr <- read_parquet(ruta_estr) |>
    mutate(CMUN = str_pad(str_extract(as.character(CMUN), "\\d+"),
                          width = 5, side = "left", pad = "0")) |>
    select(CMUN, nombre, comarca)
  cruce <- left_join(cruce, estr, by = "CMUN")
} else {
  warning("ivec_estructural.parquet no encontrado: el cruce no llevará nombre ni comarca.")
  cruce$nombre  <- NA_character_
  cruce$comarca <- NA_character_
}

saveRDS(cruce, ruta_salida)
message("  Guardado: ", ruta_salida, "  (", nrow(cruce), " celdas)")
message("── 12_cruce_grd_municipio.R — fin ─────────────────────────────\n")
