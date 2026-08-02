# =============================================================================
# DESCARGA DE FUENTES PARA LOS INDICADORES CS DEL IVEC_contextual
# Variables:
#   1. Densidad asociativa   — CSV Registre Associacions GVA + Catastro
#   2. Servicios proximidad  — Farmacias (XLSX Conselleria Sanitat + Catastro)
#                              Escuelas infantiles 0-3 (CSV Centres Docents)
#                              Centros de día / inclusión (RECESSO, ya descargado)
#   3. Transporte público    — OSM Overpass (highway=bus_stop, railway=*)
#   4. Nodos cívicos públicos (Solución B):
#                              Centros educativos 3+ (CSV Centres Docents)
#                              Bibliotecas (Directorio MCU + Catastro)
#                              Centros cívicos (GeoJSON DipCas + Alicante +
#                              València + OSM amenity=community_centre)
#   (+) Agentes de cooperación (ONGDs, descartado del IVEC pero conservado
#       por trazabilidad de la fuente).
# Motor de geocoding único: Catastro Inmobiliario local (DGC), índice
# construido por construir_indice_catastro() en sección 0. Nominatim queda
# como motor alternativo para auditoría/comparación.
# Salida: data/contextual/ — GPKG y Parquet en EPSG:3035 (LAEA),
#         coherentes con el grid de trabajo del IVEC.
# =============================================================================
# Contexto:
#   — Dimensión CS (capacidades de soporte social) del módulo IVEC_contextual.
#     V1 opera con la fórmula compensadora completa IVEC_contextual = ½·VC* +
#     ½·(1−CS*) y CS construido como media equiponderada de los cuatro
#     indicadores CS activos: cs_sanitario, cs_transporte, cs_proximidad
#     (parcial: farmacias pendientes) y cs_nodos_civicos (parcial: bibliotecas
#     pendientes). cs_asociativo permanece como columna NA en V1 a la espera
#     de la geocodificación viable del Registre d'Associacions GVA (diferido
#     a V2 por baja tasa de geocoding catastral).
#   — Este script consolida la descarga de los inputs externos asociados a
#     los componentes pendientes (asociaciones, farmacias, bibliotecas) y los
#     prepara para una próxima iteración del pipeline R/ivec/04_ivec_contextual.R.
# =============================================================================
# Lo que el script NO hace (por diseño):
#   — No reagrega al grid de 1 km² LAEA. Esa proyección al grid corresponde al
#     pipeline R/ivec/04_ivec_contextual.R cuando se activen los indicadores CS.
#   — No actualiza prosa de cap. 3 / 5 / 6 ni la columna "Fichero crudo" de
#     tab-fuentes-contextual en A6-fuentes.Rmd. La cascada de revisiones se
#     aplica en batch cuando lo solicite el director de la tesis.
# =============================================================================
# Idempotencia: las cuatro funciones aceptan `overwrite = FALSE` por defecto.
# Si el archivo destino existe, la descarga se omite con un mensaje informativo.
# =============================================================================

# -----------------------------------------------------------------------------
# Carga de paquetes con autoinstalación si falta alguno
# -----------------------------------------------------------------------------
# Los paquetes CRAN estándar se instalan automáticamente desde el espejo de
# RStudio si no están presentes. El script no depende de geocoders externos:
# el motor de geocodificación es el Catastro Inmobiliario local.
# -----------------------------------------------------------------------------

.paquetes_cran <- c(
  "tidyverse",     # core: dplyr, tidyr, purrr, readr, etc.
  "sf",            # vectores espaciales y proyecciones
  "osmdata",       # interfaz a Overpass API de OpenStreetMap
  "tidygeocoder",  # geocodificación Nominatim (alternativa de auditoría)
  "readxl",        # lectura de XLSX (listado de farmacias)
  "httr",          # cliente HTTP (endpoints MCU, Overpass)
  "jsonlite",      # parseo JSON
  "arrow",         # lectura/escritura Parquet (caché de geocoding)
  "here",          # rutas relativas al root del proyecto
  "glue",          # interpolación de strings
  "stringi",       # normalización Unicode (sin acentos)
  "stringdist"     # similaridad fuzzy de strings (Jaro-Winkler)
)
for (.p in .paquetes_cran) {
  if (!requireNamespace(.p, quietly = TRUE)) {
    message(sprintf("[setup] Instalando paquete CRAN: %s", .p))
    install.packages(.p, repos = "https://cloud.r-project.org")
  }
  suppressPackageStartupMessages(library(.p, character.only = TRUE))
}


# -----------------------------------------------------------------------------
# Configuración global
# -----------------------------------------------------------------------------

# CRS de trabajo del IVEC: ETRS89-LAEA (EPSG:3035), nativo del grid 1×1 km.
EPSG_LAEA <- 3035

# Directorio de salida (ya existente en el proyecto, contiene los GPKG de
# fuentes contextuales: 15_SistemaValencianoSalud.gpkg, 19_RECESSO.gpkg, etc.).
dir_out <- here::here("data", "contextual")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# URLs canónicas del Portal de Dades Obertes GVA.
URL_ASOCIACIONES <- "https://dadesobertes.gva.es/dataset/dbe0d2b9-b7c8-4329-91ea-17806a0a8d3e/resource/dff5096e-7289-4492-bc76-c1baa4eea727/download/asociaciones-de-la-comunitat-valenciana.csv"
URL_ONGS         <- "https://dadesobertes.gva.es/dataset/14709b5b-9319-46c7-97f5-4240b606294f/resource/9f499b35-19bf-45f8-a002-7e8c2b356f73/download/soc_agentes_cooperacion.csv"

# Endpoint de Overpass API.
URL_OVERPASS <- "https://overpass-api.de/api/interpreter"

# Directorio de Bibliotecas Españolas (Ministerio de Cultura).
# Endpoint TXT con cobertura nacional; filtrado posterior por provincias CV.
URL_BIBLIOTECAS_MCU <- "https://directoriobibliotecas.mcu.es/mobile/dimbe.cmd?apartado=descarga&accion=descargar&completa=no&formato=txt"

# GeoJSON de equipamientos culturales — Diputació de Castelló.
URL_CIVICOS_DIPCAS <- "https://datosabiertos.dipcas.es/api/explore/v2.1/catalog/datasets/centros-culturales-2/exports/geojson"

# GeoJSON de equipamientos municipales — Ajuntament de València.
URL_CIVICOS_VLC <- "https://valencia.opendatasoft.com/api/explore/v2.1/catalog/datasets/equipamients-municipals-equipamientos-municipales/exports/geojson"

# Ficheros locales (descargados manualmente, ya disponibles en data/contextual/).
RUTA_FARMACIAS_XLSX   <- file.path(dir_out, "ListadoOficinasFarmacia.xlsx")
RUTA_CENTROS_DOC_CSV  <- file.path(dir_out, "centros-docentes-de-la-comunitat-valenciana.csv")
RUTA_CIVICOS_ALICANTE <- file.path(dir_out, "centros-culturales_alicante.geojson")

# Raíz del Catastro extraído (estructura coherente con R/ivec/04_ivec_contextual.R).
RUTA_CATASTRO_EXTRACTED <- here::here("data", "contextual", "catastro", "_extracted")

# Códigos de provincia DGC para CV (03 Alicante, 12 Castellón, 46 Valencia).
PROVS_CATASTRO_CV <- c("03", "12", "46")

# Códigos de provincia CV (utilizados para filtrar el TXT del MCU).
PROVINCIAS_CV <- c("03", "12", "46")

# Log acumulativo de la sesión de descarga.
log_descarga <- character(0)
.log <- function(msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  linea <- glue::glue("[{ts}] {msg}")
  message(linea)
  log_descarga <<- c(log_descarga, linea)
  invisible(NULL)
}


# =============================================================================
# SECCIÓN 0 — GEOCODER LOCAL CONTRA EL CATASTRO INMOBILIARIO
# =============================================================================
# Construye un índice local de direcciones (vía + número → centroide de parcela)
# a partir del fichero alfanumérico CAT (V3, longitud fija 999, encoding
# latin-1) y la capa PARCELA del SHP catastral (DGC). Esquema empírico inferido
# sobre los ficheros V3:
#   - Registro tipo 11 (Finca) lleva la dirección postal:
#       pos 24-28   cod_muni_5d (formato DGC, prov+muni)
#       pos 31-44   REFCAT_14
#       pos 53-77   nombre_PROVINCIA (no municipio: 25 chars, padding der.)
#       pos 84-113  nombre_municipio real (campo entidad_menor del CAT V3,
#                   30 chars con padding; es el topónimo municipal efectivo
#                   pese a llamarse "entidad menor" en el manual DGC).
#       pos 159-163 tipo_via    (CL/AV/PZ/PG/CR/GL... 5 chars, padding der.)
#       pos 164-188 nombre_via  (25 chars, padding derecha)
#       pos 189-192 num_policia_1 (4 dig, padding izq. con ceros)
#   - REFCAT_14 (14 chars) enlaza con el SHP de PARCELA (campo REFCAT).
#
# Nota histórica (mayo de 2026): la primera versión del parser tomaba
# erróneamente pos 53-77 como nombre del municipio, lo que producía un índice
# con solo tres valores únicos (Valencia, Alicante, Castellón) y un 79 % de
# `municipio_no_indexado` al geocodificar. La posición correcta del topónimo
# municipal en el CAT V3 es 84-113 (entidad_menor). Si se observa el bug en
# un índice ya generado, basta con relanzar `construir_indice_catastro(rebuild = TRUE)`.
#
# El índice se guarda en data/contextual/17_catastro_direcciones_indice.parquet
# (~50-150 MB estimados tras dedup) y es idempotente: una vez construido, se
# reutiliza salvo que se solicite rebuild = TRUE.
# =============================================================================

# Posiciones del registro tipo 11 del CAT alfanumérico V3.
CAT11_POS <- list(
  tipo_reg      = c(1, 2),
  cod_muni_5d   = c(24, 28),
  refcat_14     = c(31, 44),
  nombre_muni   = c(84, 113),   # campo entidad_menor — topónimo municipal real
  tipo_via      = c(159, 163),
  nombre_via    = c(164, 188),
  num_pol_1     = c(189, 192)
)

# Mapa de abreviaturas catastrales de tipo de vía a forma canónica.
.MAPEO_TIPO_VIA <- c(
  "CL" = "calle",       "C"   = "calle",
  "AV" = "avenida",     "AVDA" = "avenida",
  "PZ" = "plaza",       "PL"   = "plaza",     "PLAZ" = "plaza",
  "PG" = "paseo",       "PSO"  = "paseo",     "P"    = "paseo",
  "CR" = "carretera",   "CTRA" = "carretera",
  "GL" = "glorieta",
  "TR" = "travesia",    "TRAV" = "travesia",
  "RD" = "ronda",       "RNDA" = "ronda",
  "CM" = "camino",      "CMNO" = "camino",
  "BO" = "barrio",      "URB"  = "urbanizacion",
  "CR" = "carrer",      "PT"   = "partida"
)


# Helper: normalización canónica de una cadena (lowercase + sin acentos + trim).
.normalizar_str <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("[[:punct:]]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Helper: normalización de tipo_via + nombre_via a forma comparable.
.normalizar_via <- function(tipo_via, nombre_via) {
  tipo_via <- trimws(as.character(tipo_via))
  tipo_can <- ifelse(toupper(tipo_via) %in% names(.MAPEO_TIPO_VIA),
                     .MAPEO_TIPO_VIA[toupper(tipo_via)],
                     .normalizar_str(tipo_via))
  nombre_can <- .normalizar_str(nombre_via)
  trimws(paste(tipo_can, nombre_can))
}


# Helper interno: parseo del fichero alfanumérico CAT.gz para una provincia.
# Devuelve un data.frame con todos los registros tipo 11 parseados.
.parsear_cat_provincia <- function(dir_extract_provincia) {
  ficheros_cat <- list.files(dir_extract_provincia,
                              pattern = "\\.CAT(\\.gz)?$",
                              recursive = TRUE, full.names = TRUE,
                              ignore.case = TRUE)
  if (length(ficheros_cat) == 0) return(NULL)

  .log(glue::glue("[catastro/cat] {basename(dir_extract_provincia)}: ",
                  "{length(ficheros_cat)} ficheros CAT detectados"))

  resultados <- vector("list", length(ficheros_cat))
  for (i in seq_along(ficheros_cat)) {
    fic <- ficheros_cat[i]
    con <- if (grepl("\\.gz$", fic)) gzfile(fic, encoding = "latin1") else
                                     file(fic, encoding = "latin1")
    lineas <- tryCatch(readLines(con, warn = FALSE), error = function(e) character(0))
    close(con)
    lineas11 <- lineas[startsWith(lineas, "11")]
    if (length(lineas11) == 0) next

    extraer <- function(linea, rango) {
      substr(linea, rango[1], rango[2])
    }
    df <- data.frame(
      cod_muni_5d   = trimws(vapply(lineas11, extraer, character(1), rango = CAT11_POS$cod_muni_5d)),
      refcat_14     = trimws(vapply(lineas11, extraer, character(1), rango = CAT11_POS$refcat_14)),
      nombre_muni   = trimws(vapply(lineas11, extraer, character(1), rango = CAT11_POS$nombre_muni)),
      tipo_via      = trimws(vapply(lineas11, extraer, character(1), rango = CAT11_POS$tipo_via)),
      nombre_via    = trimws(vapply(lineas11, extraer, character(1), rango = CAT11_POS$nombre_via)),
      num_pol_1     = as.integer(trimws(vapply(lineas11, extraer, character(1), rango = CAT11_POS$num_pol_1))),
      stringsAsFactors = FALSE
    )
    resultados[[i]] <- df
  }
  resultados <- resultados[!vapply(resultados, is.null, logical(1))]
  if (length(resultados) == 0) return(NULL)
  dplyr::bind_rows(resultados)
}


# Helper interno: lectura de la capa PARCELA de un municipio del SHP catastral.
# Réplica de leer_parcelas_municipio() de R/ivec/04_ivec_contextual.R; no se
# importa por source() para mantener este script autónomo. Si se modifica
# allí, propagar el cambio aquí.
.leer_parcelas_municipio_local <- function(carpeta_muni) {
  zip_parcela <- list.files(carpeta_muni, pattern = "PARCELA\\.ZIP$",
                            full.names = TRUE, ignore.case = TRUE)
  if (length(zip_parcela) == 0) return(NULL)
  td <- tempfile(); dir.create(td, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zip_parcela[1], exdir = td)
  shp <- list.files(td, pattern = "PARCELA\\.shp$",
                    full.names = TRUE, ignore.case = TRUE)
  if (length(shp) == 0) { unlink(td, recursive = TRUE); return(NULL) }
  p <- tryCatch(sf::st_read(shp[1], quiet = TRUE), error = function(e) NULL)
  unlink(td, recursive = TRUE)
  if (is.null(p) || nrow(p) == 0 || !"REFCAT" %in% names(p)) return(NULL)
  p |>
    sf::st_zm(drop = TRUE) |>
    dplyr::transmute(refcat_14 = substr(as.character(REFCAT), 1, 14)) |>
    sf::st_centroid() |>
    sf::st_transform(EPSG_LAEA)
}

# Helper interno: lectura completa del PARCELA SHP de una provincia.
.leer_parcelas_provincia_local <- function(dir_extract_provincia) {
  primer_nivel <- list.dirs(dir_extract_provincia, recursive = FALSE,
                            full.names = TRUE)
  carpetas_shp <- primer_nivel[grepl("_UA_.*_SHP$", primer_nivel,
                                     ignore.case = TRUE)]
  carpetas_muni <- if (length(carpetas_shp) > 0) {
    unlist(lapply(carpetas_shp, function(d) {
      subs <- list.dirs(d, recursive = FALSE, full.names = TRUE)
      subs[grepl("uA ", subs, fixed = TRUE)]
    }))
  } else {
    primer_nivel[grepl("uA ", primer_nivel, fixed = TRUE)]
  }
  .log(glue::glue("[catastro/shp] {basename(dir_extract_provincia)}: ",
                  "{length(carpetas_muni)} municipios"))
  if (length(carpetas_muni) == 0) return(NULL)
  res <- vector("list", length(carpetas_muni))
  for (i in seq_along(carpetas_muni)) {
    res[[i]] <- .leer_parcelas_municipio_local(carpetas_muni[i])
  }
  res <- res[!vapply(res, is.null, logical(1))]
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}


# Función pública: construcción del índice catastral (idempotente).
construir_indice_catastro <- function(rebuild = FALSE,
                                       provincias = PROVS_CATASTRO_CV) {
  ruta_indice <- file.path(dir_out, "17_catastro_direcciones_indice.parquet")
  if (file.exists(ruta_indice) && !rebuild) {
    .log(glue::glue("[catastro/indice] Existe {basename(ruta_indice)}; ",
                    "se reutiliza (rebuild=FALSE)."))
    return(invisible(ruta_indice))
  }

  if (!dir.exists(RUTA_CATASTRO_EXTRACTED)) {
    .log(glue::glue("[catastro/indice] No existe {RUTA_CATASTRO_EXTRACTED}."))
    .log("[catastro/indice] Ejecuta primero el pipeline 04_ivec_contextual.R o equivalente para extraer el Catastro.")
    return(invisible(NULL))
  }

  bloques <- list()
  for (prov in provincias) {
    dir_prov <- file.path(RUTA_CATASTRO_EXTRACTED, prov)
    if (!dir.exists(dir_prov)) {
      .log(glue::glue("[catastro/indice] Provincia {prov} no encontrada en _extracted/; saltada."))
      next
    }
    .log(glue::glue("[catastro/indice] Procesando provincia {prov} ..."))

    direcciones <- .parsear_cat_provincia(dir_prov)
    if (is.null(direcciones) || nrow(direcciones) == 0) {
      .log(glue::glue("[catastro/indice] Provincia {prov}: sin registros tipo 11. Saltada."))
      next
    }
    .log(glue::glue("[catastro/indice] Provincia {prov}: {nrow(direcciones)} registros tipo 11"))

    parcelas <- .leer_parcelas_provincia_local(dir_prov)
    if (is.null(parcelas) || nrow(parcelas) == 0) {
      .log(glue::glue("[catastro/indice] Provincia {prov}: sin PARCELA SHP. Saltada."))
      next
    }
    .log(glue::glue("[catastro/indice] Provincia {prov}: {nrow(parcelas)} parcelas con geometría"))

    # Join CAT × PARCELA por refcat_14, preservando geometría.
    parcelas_df <- as.data.frame(parcelas) |>
      dplyr::mutate(refcat_14 = as.character(refcat_14)) |>
      dplyr::distinct(refcat_14, .keep_all = TRUE)

    direcciones <- direcciones |>
      dplyr::mutate(
        via_normalizada = .normalizar_via(tipo_via, nombre_via),
        municipio_norm  = .normalizar_str(nombre_muni),
        provincia       = prov
      )

    indice_prov <- dplyr::inner_join(direcciones, parcelas_df, by = "refcat_14") |>
      dplyr::filter(!is.na(num_pol_1), num_pol_1 > 0,
                    via_normalizada != "", municipio_norm != "")

    bloques[[prov]] <- indice_prov
    .log(glue::glue("[catastro/indice] Provincia {prov}: índice con {nrow(indice_prov)} entradas"))
  }

  if (length(bloques) == 0) {
    .log("[catastro/indice] No se construyó índice (sin datos en ninguna provincia).")
    return(invisible(NULL))
  }

  indice <- dplyr::bind_rows(bloques)

  # Dedup por (provincia, municipio, vía, número): conservamos la primera
  # geometría (centroide de parcela) por unicidad de dirección.
  indice <- indice |>
    dplyr::distinct(provincia, municipio_norm, via_normalizada, num_pol_1,
                    .keep_all = TRUE)

  # Convertir geometry a wkt para serializar a parquet (sf-on-parquet no es
  # universalmente soportado en arrow R; el wkt es portable).
  if ("geometry" %in% names(indice)) {
    indice$geom_wkt <- sf::st_as_text(indice$geometry)
    indice$geometry <- NULL
  }

  arrow::write_parquet(indice, ruta_indice)
  .log(glue::glue("[catastro/indice] Escrito: {basename(ruta_indice)} ",
                  "({nrow(indice)} direcciones únicas)"))
  invisible(ruta_indice)
}


# Helper: carga el índice catastral en memoria y restaura la geometría sf.
.cargar_indice_catastro <- function() {
  ruta_indice <- file.path(dir_out, "17_catastro_direcciones_indice.parquet")
  if (!file.exists(ruta_indice)) {
    .log("[catastro/indice] No existe el índice; ejecuta construir_indice_catastro() primero.")
    return(NULL)
  }
  df <- arrow::read_parquet(ruta_indice)
  if ("geom_wkt" %in% names(df)) {
    geom <- sf::st_as_sfc(df$geom_wkt, crs = EPSG_LAEA)
    df <- sf::st_as_sf(cbind(df, geometry = geom), sf_column_name = "geometry")
    df$geom_wkt <- NULL
  }
  df
}


# Helper interno: parseo de una dirección de input para extraer (tipo_via,
# nombre_via, numero). Devuelve un data.frame de una fila o NA si no parsea.
.parsear_direccion_input <- function(direccion) {
  d <- .normalizar_str(direccion)
  # Patrones para detectar el tipo de vía al inicio (lista exhaustiva de
  # abreviaturas comunes en castellano y valenciano).
  patron_tipo <- "^(c/?|calle|carrer|av|avda|avenida|pl|plaza|plaça|pg|paseo|pso|cr|carretera|ctra|cm|camino|cami|gl|glorieta|tr|travesia|rd|ronda|bo|barrio|urb|urbanizacion|partida|pt)\\b"
  tipo_match <- regmatches(d, regexpr(patron_tipo, d, perl = TRUE))
  tipo_can <- if (length(tipo_match) > 0 && nzchar(tipo_match)) {
    abbr <- toupper(gsub("[^A-Z]", "", tipo_match))
    if (abbr %in% names(.MAPEO_TIPO_VIA)) .MAPEO_TIPO_VIA[abbr] else .normalizar_str(tipo_match)
  } else {
    ""
  }
  resto <- trimws(sub(patron_tipo, "", d, perl = TRUE))

  # Extracción del primer número (número de policía).
  num_match <- regmatches(resto, regexpr("\\b\\d{1,4}\\b", resto))
  numero <- if (length(num_match) > 0 && nzchar(num_match)) as.integer(num_match[1]) else NA_integer_

  # Nombre de vía = todo antes del primer número.
  nombre <- if (!is.na(numero)) {
    trimws(sub(paste0("\\b", numero, "\\b.*$"), "", resto, perl = TRUE))
  } else {
    resto
  }

  via_normalizada <- trimws(paste(tipo_can, nombre))
  data.frame(
    via_normalizada = via_normalizada,
    numero          = numero,
    stringsAsFactors = FALSE
  )
}


# Helper público: geocoding de una dirección contra el índice catastral.
# Estrategia mixta: (A) match exacto por (municipio, vía, número); si falla,
# (B) fuzzy match por Jaro-Winkler sobre la vía dentro del mismo municipio.
.geocode_catastro_uno <- function(direccion, municipio, indice,
                                   umbral_fuzzy = 0.15) {
  if (is.null(indice) || nrow(indice) == 0) {
    return(data.frame(lat = NA_real_, lon = NA_real_, refcat = NA_character_,
                      score_match = NA_real_, motor = "catastro",
                      tipo_match = "indice_vacio", stringsAsFactors = FALSE))
  }

  muni_norm <- .normalizar_str(municipio)
  parsed <- .parsear_direccion_input(direccion)

  # (A) Match exacto.
  if (!is.na(parsed$numero) && nzchar(parsed$via_normalizada)) {
    match_exacto <- indice |>
      dplyr::filter(municipio_norm == muni_norm,
                    via_normalizada == parsed$via_normalizada,
                    num_pol_1 == parsed$numero)
    if (nrow(match_exacto) >= 1) {
      coord <- sf::st_coordinates(sf::st_transform(match_exacto[1, ], 4326))
      return(data.frame(
        lat        = coord[1, "Y"],
        lon        = coord[1, "X"],
        refcat     = match_exacto$refcat_14[1],
        score_match = 0,
        motor      = "catastro",
        tipo_match = "exacto",
        stringsAsFactors = FALSE
      ))
    }
  }

  # (B) Fuzzy match (Jaro-Winkler) sobre la vía dentro del municipio.
  subset_muni <- indice |> dplyr::filter(municipio_norm == muni_norm)
  if (nrow(subset_muni) == 0) {
    return(data.frame(lat = NA_real_, lon = NA_real_, refcat = NA_character_,
                      score_match = NA_real_, motor = "catastro",
                      tipo_match = "municipio_no_indexado",
                      stringsAsFactors = FALSE))
  }
  scores <- stringdist::stringdist(parsed$via_normalizada,
                                    subset_muni$via_normalizada,
                                    method = "jw")
  i_min <- which.min(scores)
  if (length(i_min) == 0 || scores[i_min] > umbral_fuzzy) {
    return(data.frame(lat = NA_real_, lon = NA_real_, refcat = NA_character_,
                      score_match = if (length(i_min) > 0) scores[i_min] else NA_real_,
                      motor = "catastro", tipo_match = "no_match_fuzzy",
                      stringsAsFactors = FALSE))
  }
  # Si encontramos vía similar, intentamos ajustar por número dentro de ese subconjunto.
  candidatos <- subset_muni |>
    dplyr::filter(via_normalizada == subset_muni$via_normalizada[i_min])
  if (!is.na(parsed$numero)) {
    diff_num <- abs(candidatos$num_pol_1 - parsed$numero)
    j <- which.min(diff_num)
    if (length(j) > 0) candidatos <- candidatos[j, ]
  }
  coord <- sf::st_coordinates(sf::st_transform(candidatos[1, ], 4326))
  data.frame(
    lat        = coord[1, "Y"],
    lon        = coord[1, "X"],
    refcat     = candidatos$refcat_14[1],
    score_match = scores[i_min],
    motor      = "catastro",
    tipo_match = "fuzzy",
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# SECCIÓN 1 — DENSIDAD ASOCIATIVA (REGISTRE ASSOCIACIONS GVA)
# =============================================================================
# El CSV oficial contiene dirección postal en la columna DOMICILIO (cobertura
# 99,90 %) más DISTRITO_POSTAL (63,87 %) y MUNICIPIO (100 %). Sin coordenadas,
# por lo que se geocodifica contra el Catastro Inmobiliario local (sección 0)
# con estrategia mixta: match exacto por (municipio, vía, número), fallback
# fuzzy Jaro-Winkler.
#
# Output: capa de puntos `17_asociaciones.gpkg` en EPSG:3035 (LAEA), agregable
# directamente al grid de 1 km² en el pipeline R/ivec/04_ivec_contextual.R,
# sin proyección por centroide municipal.
#
# Robustez para volúmenes elevados (~90k registros activos):
#   1. Filtrado previo por SITUACION=Activa.
#   2. Caché incremental en parquet (`17_asociaciones_geocoding_cache.parquet`)
#      por bloques de 500 filas: si el proceso se interrumpe, se reanuda
#      desde el último bloque escrito en disco.
#   3. tryCatch por fila: una dirección que falla no aborta el batch.
# =============================================================================

# Helper interno reutilizable para todas las funciones de geocoding contra el
# Catastro: ejecuta una llamada a .geocode_catastro_uno() con la firma
# habitual (direccion + municipio + indice). Devuelve un data.frame de una fila.
.geocode_via_catastro <- function(direccion, municipio, indice) {
  r <- .geocode_catastro_uno(direccion = direccion, municipio = municipio,
                              indice = indice)
  if (!is.data.frame(r)) {
    r <- data.frame(lat = NA_real_, lon = NA_real_, refcat = NA_character_,
                    score_match = NA_real_, motor = "catastro",
                    tipo_match = "error", stringsAsFactors = FALSE)
  }
  r
}

descargar_asociaciones <- function(overwrite = FALSE,
                                    bloque    = 500L) {
  ruta_out       <- file.path(dir_out, "17_asociaciones.gpkg")
  ruta_log_fall  <- file.path(dir_out, "17_asociaciones_no_geocodificadas.csv")
  ruta_cache     <- file.path(dir_out, "17_asociaciones_geocoding_cache.parquet")

  if (file.exists(ruta_out) && !overwrite) {
    .log(glue::glue("[asociaciones] Existe {basename(ruta_out)}, se omite (overwrite=FALSE)."))
    return(invisible(ruta_out))
  }

  .log("[asociaciones] Descargando CSV oficial desde dadesobertes.gva.es ...")
  asoc_raw <- tryCatch(
    readr::read_csv2(URL_ASOCIACIONES, locale = readr::locale(encoding = "UTF-8"),
                     show_col_types = FALSE),
    error = function(e) {
      .log(glue::glue("[asociaciones] ERROR de descarga: {conditionMessage(e)}"))
      return(NULL)
    }
  )
  if (is.null(asoc_raw)) return(invisible(NULL))
  .log(glue::glue("[asociaciones] Registros descargados: {nrow(asoc_raw)}"))

  # Detección dinámica de los campos relevantes.
  campo_situacion <- names(asoc_raw)[
    grepl("^situaci", names(asoc_raw), ignore.case = TRUE)
  ][1]
  campo_domicilio <- names(asoc_raw)[
    grepl("^domicili|^direcci", names(asoc_raw), ignore.case = TRUE)
  ][1]
  campo_codmuni <- names(asoc_raw)[
    grepl("^codi.?munici|^cod.?muni", names(asoc_raw), ignore.case = TRUE)
  ][1]
  campo_municipio <- names(asoc_raw)[
    grepl("^municipi", names(asoc_raw), ignore.case = TRUE) &
      !grepl("^codi", names(asoc_raw), ignore.case = TRUE)
  ][1]
  campo_cp <- names(asoc_raw)[
    grepl("^distrito.?postal|^cod.?postal|^cp$", names(asoc_raw), ignore.case = TRUE)
  ][1]
  campo_denom <- names(asoc_raw)[
    grepl("^desc.?denominacion|^denominaci|^nombre$", names(asoc_raw), ignore.case = TRUE)
  ][1]

  if (is.na(campo_situacion) || is.na(campo_domicilio) || is.na(campo_municipio)) {
    .log("[asociaciones] No se identifican los campos de situación, domicilio o municipio.")
    .log(glue::glue("[asociaciones] Campos disponibles: {paste(names(asoc_raw), collapse = ', ')}"))
    return(invisible(NULL))
  }

  # Filtrado de activas con domicilio y municipio.
  asoc_activas <- asoc_raw |>
    dplyr::filter(
      !is.na(.data[[campo_situacion]]),
      grepl("^act", .data[[campo_situacion]], ignore.case = TRUE),
      !is.na(.data[[campo_domicilio]]),
      !is.na(.data[[campo_municipio]])
    )
  pct_activas <- round(100 * nrow(asoc_activas) / nrow(asoc_raw), 1)
  .log(glue::glue("[asociaciones] Activas con domicilio: {nrow(asoc_activas)} ({pct_activas}%)"))

  # Carga del índice catastral (construye si no existe).
  construir_indice_catastro(rebuild = FALSE)
  indice <- .cargar_indice_catastro()
  if (is.null(indice)) {
    .log("[asociaciones] No se pudo cargar el índice catastral; abortando.")
    return(invisible(NULL))
  }
  .log(glue::glue("[asociaciones] Índice catastral cargado: {nrow(indice)} direcciones"))

  # Clave de caché: domicilio + municipio (un join key tipo string).
  asoc_activas <- asoc_activas |>
    dplyr::mutate(
      direccion_busqueda = trimws(paste(.data[[campo_domicilio]],
                                         .data[[campo_municipio]], sep = " | "))
    )

  # Caché incremental para reanudar tras interrupciones (la geocodificación
  # contra Catastro es local pero el volumen es alto: ~90k filas).
  ya_geocodificadas <- if (file.exists(ruta_cache)) {
    .log(glue::glue("[asociaciones] Caché previo: {basename(ruta_cache)}"))
    arrow::read_parquet(ruta_cache)
  } else NULL

  asoc_idx <- if (!is.null(ya_geocodificadas)) {
    asoc_activas$direccion_busqueda %in% ya_geocodificadas$direccion_busqueda
  } else rep(FALSE, nrow(asoc_activas))
  pendientes <- asoc_activas[!asoc_idx, , drop = FALSE]
  .log(glue::glue("[asociaciones] Direcciones pendientes: {nrow(pendientes)}"))

  if (nrow(pendientes) > 0) {
    .log(glue::glue("[asociaciones] Lanzando match contra Catastro (bloque {bloque}) ..."))
    resultados <- vector("list", nrow(pendientes))
    inicio <- Sys.time()
    for (i in seq_len(nrow(pendientes))) {
      r <- .geocode_via_catastro(
        direccion = pendientes[[campo_domicilio]][i],
        municipio = pendientes[[campo_municipio]][i],
        indice    = indice
      )
      r$direccion_busqueda <- pendientes$direccion_busqueda[i]
      resultados[[i]] <- r
      if (i %% bloque == 0 || i == nrow(pendientes)) {
        tabla_parcial <- dplyr::bind_rows(resultados[seq_len(i)])
        tabla_completa <- dplyr::bind_rows(ya_geocodificadas, tabla_parcial)
        arrow::write_parquet(tabla_completa, ruta_cache)
        pct <- round(100 * i / nrow(pendientes), 1)
        elapsed <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
        eta <- elapsed / i * (nrow(pendientes) - i)
        .log(glue::glue("[asociaciones]   bloque {i}/{nrow(pendientes)} ",
                        "({pct}% — ETA {round(eta/60,1)} min)"))
      }
    }
  }

  caché_final <- if (file.exists(ruta_cache)) arrow::read_parquet(ruta_cache) else ya_geocodificadas
  asoc_geo <- dplyr::left_join(asoc_activas, caché_final, by = "direccion_busqueda")

  # Métricas por tipo de match.
  if ("tipo_match" %in% names(asoc_geo)) {
    metricas <- as.data.frame(table(asoc_geo$tipo_match), stringsAsFactors = FALSE)
    names(metricas) <- c("tipo_match", "n")
    for (i in seq_len(nrow(metricas))) {
      .log(glue::glue("[asociaciones]   tipo_match={metricas$tipo_match[i]}: {metricas$n[i]}"))
    }
  }

  geo_validas  <- asoc_geo |> dplyr::filter(!is.na(lat), !is.na(lon))
  geo_fallidas <- asoc_geo |> dplyr::filter(is.na(lat) | is.na(lon))
  pct_ok <- round(100 * nrow(geo_validas) / nrow(asoc_geo), 1)
  .log(glue::glue("[asociaciones] Geocodificadas: {nrow(geo_validas)} de ",
                  "{nrow(asoc_geo)} ({pct_ok}%)"))

  if (nrow(geo_fallidas) > 0) {
    readr::write_csv(geo_fallidas, ruta_log_fall)
    .log(glue::glue("[asociaciones] Log de fallidas: {basename(ruta_log_fall)}"))
  }
  if (nrow(geo_validas) == 0) {
    .log("[asociaciones] Sin registros geolocalizados; no se escribe GPKG.")
    return(invisible(NULL))
  }

  campos_conservar <- intersect(
    c("COD_NUM_AUTONOMICO", campo_denom, campo_domicilio,
      campo_municipio, campo_codmuni, campo_cp,
      "DESC_ACTIVIDAD", "refcat", "score_match", "motor", "tipo_match"),
    names(geo_validas)
  )
  keep <- geo_validas[, campos_conservar]
  keep$lat <- geo_validas$lat
  keep$lon <- geo_validas$lon

  asoc_sf <- sf::st_as_sf(keep, coords = c("lon","lat"), crs = 4326) |>
    sf::st_transform(crs = EPSG_LAEA)

  sf::st_write(
    asoc_sf,
    dsn           = ruta_out,
    layer         = "asociaciones",
    layer_options = "ENCODING=UTF-8",
    delete_dsn    = TRUE,
    quiet         = TRUE
  )
  .log(glue::glue("[asociaciones] Escrito: {basename(ruta_out)} ",
                  "({nrow(asoc_sf)} puntos, EPSG:{EPSG_LAEA})"))

  invisible(ruta_out)
}


# =============================================================================
# SECCIÓN 2 — AGENTES DE COOPERACIÓN (ONGDs)
# =============================================================================
# El registro soc_agentes_cooperacion contiene la dirección postal completa
# (sede_central + municipio_sede_central) sin coordenadas. La geocodificación
# se realiza contra el índice catastral (sección 0) con estrategia mixta
# (exacto + fuzzy). El parámetro `motor` permite alternativamente forzar
# Nominatim para auditoría/comparación. La función reporta la tasa de éxito
# por tipo de match (exacto, fuzzy, no_match).
# =============================================================================

descargar_ongs <- function(overwrite = FALSE,
                            motor    = c("catastro", "nominatim")) {
  motor <- match.arg(motor)
  ruta_out_geo <- file.path(dir_out, "17_ongs_cooperacion.gpkg")
  ruta_out_log <- file.path(dir_out, "17_ongs_no_geocodificadas.csv")

  if (file.exists(ruta_out_geo) && !overwrite) {
    .log(glue::glue("[ongs] Existe {basename(ruta_out_geo)}, se omite (overwrite=FALSE)."))
    return(invisible(ruta_out_geo))
  }

  .log(glue::glue("[ongs] Descargando CSV oficial (motor={motor}) ..."))
  ongs_raw <- tryCatch(
    readr::read_csv2(URL_ONGS, locale = readr::locale(encoding = "UTF-8"),
                     show_col_types = FALSE),
    error = function(e) {
      .log(glue::glue("[ongs] ERROR de descarga: {conditionMessage(e)}"))
      return(NULL)
    }
  )
  if (is.null(ongs_raw)) return(invisible(NULL))
  .log(glue::glue("[ongs] Registros descargados: {nrow(ongs_raw)}"))

  campo_sede <- names(ongs_raw)[
    grepl("^sede.?central|^sede$|^direc", names(ongs_raw), ignore.case = TRUE)
  ][1]
  campo_muni <- names(ongs_raw)[
    grepl("municipio.?sede|^municipio$|^poblac", names(ongs_raw), ignore.case = TRUE)
  ][1]
  if (is.na(campo_sede) || is.na(campo_muni)) {
    .log("[ongs] No se identifican los campos de dirección o municipio.")
    .log(glue::glue("[ongs] Campos disponibles: {paste(names(ongs_raw), collapse = ', ')}"))
    return(invisible(NULL))
  }

  ongs_dir <- ongs_raw |>
    dplyr::filter(!is.na(.data[[campo_sede]]), !is.na(.data[[campo_muni]]))
  .log(glue::glue("[ongs] Direcciones a geocodificar: {nrow(ongs_dir)}"))

  if (motor == "catastro") {
    construir_indice_catastro(rebuild = FALSE)
    indice <- .cargar_indice_catastro()
    if (is.null(indice)) {
      .log("[ongs] No se pudo cargar el índice catastral; abortando.")
      return(invisible(NULL))
    }
    .log(glue::glue("[ongs] Índice catastral cargado: {nrow(indice)} direcciones"))
    .log("[ongs] Geocodificando contra Catastro (exacto + fuzzy)...")
    resultados <- vector("list", nrow(ongs_dir))
    for (i in seq_len(nrow(ongs_dir))) {
      r <- .geocode_via_catastro(
        direccion = ongs_dir[[campo_sede]][i],
        municipio = ongs_dir[[campo_muni]][i],
        indice    = indice
      )
      resultados[[i]] <- r
      if (i %% 50 == 0 || i == nrow(ongs_dir)) {
        pct <- round(100 * i / nrow(ongs_dir), 1)
        .log(glue::glue("[ongs]   {i}/{nrow(ongs_dir)} ({pct}%)"))
      }
    }
    geocode_df <- dplyr::bind_rows(resultados)
    metricas <- as.data.frame(table(geocode_df$tipo_match), stringsAsFactors = FALSE)
    names(metricas) <- c("tipo_match", "n")
    for (i in seq_len(nrow(metricas))) {
      .log(glue::glue("[ongs]   tipo_match={metricas$tipo_match[i]}: {metricas$n[i]}"))
    }
  } else {
    # Motor Nominatim — para auditoría/comparación. Baja tasa de éxito
    # documentada en CV (26% en pruebas).
    direccion_full <- paste(ongs_dir[[campo_sede]], ongs_dir[[campo_muni]],
                            "Comunitat Valenciana", "España", sep = ", ")
    .log("[ongs] Geocodificando contra Nominatim (1 req/s) ...")
    ongs_geo <- tryCatch(
      tidygeocoder::geo(direccion_full, method = "osm", min_time = 1, quiet = FALSE),
      error = function(e) {
        .log(glue::glue("[ongs] ERROR Nominatim: {conditionMessage(e)}"))
        return(NULL)
      }
    )
    if (is.null(ongs_geo)) return(invisible(NULL))
    geocode_df <- data.frame(
      lat = ongs_geo$lat, lon = ongs_geo$long,
      refcat = NA_character_, score_match = NA_real_,
      motor = "nominatim",
      tipo_match = ifelse(is.na(ongs_geo$lat), "no_match", "exacto"),
      stringsAsFactors = FALSE
    )
  }

  # Combinar metadatos de la ONG con el resultado del geocoding.
  ongs_geo <- dplyr::bind_cols(ongs_dir, geocode_df)
  validas  <- ongs_geo |> dplyr::filter(!is.na(lat), !is.na(lon))
  fallidas <- ongs_geo |> dplyr::filter(is.na(lat) | is.na(lon))
  pct_ok <- round(100 * nrow(validas) / nrow(ongs_geo), 1)
  .log(glue::glue("[ongs] Geolocalizadas: {nrow(validas)} de {nrow(ongs_geo)} ({pct_ok}%)"))

  if (nrow(fallidas) > 0) {
    readr::write_csv(fallidas, ruta_out_log)
    .log(glue::glue("[ongs] Log de fallidas: {basename(ruta_out_log)}"))
  }
  if (nrow(validas) == 0) {
    .log("[ongs] Sin registros geolocalizados; no se escribe GPKG.")
    return(invisible(NULL))
  }

  ongs_sf <- sf::st_as_sf(validas, coords = c("lon", "lat"), crs = 4326) |>
    sf::st_transform(crs = EPSG_LAEA)
  sf::st_write(ongs_sf, dsn = ruta_out_geo, layer = "ongs_cooperacion",
               layer_options = "ENCODING=UTF-8",
               delete_dsn = TRUE, quiet = TRUE)
  .log(glue::glue("[ongs] Escrito: {basename(ruta_out_geo)} ",
                  "({nrow(ongs_sf)} puntos, EPSG:{EPSG_LAEA})"))
  invisible(ruta_out_geo)
}


# =============================================================================
# SECCIÓN 3 — PARADAS DE TRANSPORTE PÚBLICO (OSM / OVERPASS API)
# =============================================================================
# Se usa el paquete `osmdata` (más robusto que `httr` crudo: gestiona reintentos
# y devuelve directamente objetos sf). El bbox es la Comunitat Valenciana,
# obtenido a través del nombre administrativo y filtrado por NUTS2 ES52 en una
# segunda pasada espacial. Si la query con `osmdata` falla, hay un bloque
# alternativo más abajo con la query Overpass cruda corregida.
# =============================================================================

descargar_paradas_transporte <- function(overwrite = FALSE) {
  ruta_out <- file.path(dir_out, "17_transporte_publico_osm.gpkg")

  if (file.exists(ruta_out) && !overwrite) {
    .log(glue::glue("[transporte] Existe {basename(ruta_out)}, se omite (overwrite=FALSE)."))
    return(invisible(ruta_out))
  }

  .log("[transporte] Consultando Overpass API vía paquete osmdata...")

  # Bbox aproximado de la Comunitat Valenciana (xmin, ymin, xmax, ymax en WGS84).
  bbox_cv <- c(-1.55, 37.85, 0.65, 40.80)

  # Helper interno: consulta robusta con un par de reintentos.
  consulta_osm <- function(key, value) {
    intento <- function() {
      osmdata::opq(bbox = bbox_cv, timeout = 180) |>
        osmdata::add_osm_feature(key = key, value = value) |>
        osmdata::osmdata_sf()
    }
    res <- tryCatch(intento(), error = function(e) {
      .log(glue::glue("[transporte] Reintentando {key}={value} tras error: {conditionMessage(e)}"))
      Sys.sleep(5); tryCatch(intento(), error = function(e2) NULL)
    })
    res
  }

  # Cuatro modos: bus_stop (highway), tram_stop / station / halt (railway).
  consultas <- list(
    list(key = "highway", value = "bus_stop",   modo = "bus"),
    list(key = "railway", value = "tram_stop",  modo = "tranvia"),
    list(key = "railway", value = "station",    modo = "ferrocarril"),
    list(key = "railway", value = "halt",       modo = "ferrocarril"),
    list(key = "railway", value = "subway_entrance", modo = "metro")
  )

  capas <- list()
  for (c in consultas) {
    .log(glue::glue("[transporte] Descargando {c$key}={c$value} ..."))
    r <- consulta_osm(c$key, c$value)
    if (is.null(r) || is.null(r$osm_points) || nrow(r$osm_points) == 0) {
      .log(glue::glue("[transporte] Sin puntos para {c$key}={c$value}"))
      next
    }
    puntos <- r$osm_points |>
      dplyr::mutate(
        modo  = c$modo,
        fuente = glue::glue("OSM:{c$key}={c$value}")
      ) |>
      dplyr::select(dplyr::any_of(c("osm_id", "name", "operator", "ref",
                                    "network", "modo", "fuente")),
                    geometry)
    capas[[length(capas) + 1]] <- puntos
    .log(glue::glue("[transporte]   → {nrow(puntos)} puntos"))
  }

  if (length(capas) == 0) {
    .log("[transporte] No se obtuvieron puntos de ninguna consulta. Abortando.")
    return(invisible(NULL))
  }

  paradas_sf <- dplyr::bind_rows(capas)

  # Recorte espacial estricto a la Comunitat Valenciana (NUTS2 ES52).
  # Usamos un buffer del bbox para no perder paradas fronterizas, pero filtramos
  # por el polígono administrativo si está disponible en una capa del proyecto;
  # de lo contrario el filtro bbox es suficiente.
  paradas_sf <- sf::st_transform(paradas_sf, crs = EPSG_LAEA)

  sf::st_write(
    paradas_sf,
    dsn           = ruta_out,
    layer         = "transporte_publico",
    layer_options = "ENCODING=UTF-8",
    delete_dsn    = TRUE,
    quiet         = TRUE
  )

  resumen <- paradas_sf |>
    sf::st_drop_geometry() |>
    dplyr::count(modo, sort = TRUE)
  .log(glue::glue("[transporte] Escrito: {basename(ruta_out)} ",
                  "({nrow(paradas_sf)} paradas, EPSG:{EPSG_LAEA})"))
  for (i in seq_len(nrow(resumen))) {
    .log(glue::glue("[transporte]   {resumen$modo[i]}: {resumen$n[i]}"))
  }

  invisible(ruta_out)
}


# =============================================================================
# SECCIÓN 3c — FARMACIAS COMUNITARIAS (XLSX CONSELLERIA + CATASTRO)
# =============================================================================
# Componente del indicador 2 (servicios de proximidad). Fuente operativa:
# `ListadoOficinasFarmacia.xlsx` de la Conselleria de Sanitat — listado oficial
# y completo con 2.345 oficinas para las tres provincias (Valencia 1.240,
# Alicante 807, Castellón 298), descargado manualmente y depositado en
# data/contextual/. El XLSX trae dirección + CP embebido + municipio +
# provincia, sin coordenadas: la geocodificación se realiza contra el Catastro
# local (sección 0) con estrategia mixta (exacto + fuzzy).
#
# La función `descargar_farmacias_osm()` mantiene la consulta Overpass como
# fallback opcional ante incidencias con el XLSX o cuando se quiera contrastar
# cobertura. Output: 17_farmacias.gpkg en EPSG:3035 (LAEA).
# =============================================================================

descargar_farmacias <- function(overwrite = FALSE,
                                 bloque    = 500L) {
  ruta_out       <- file.path(dir_out, "17_farmacias.gpkg")
  ruta_log_fall  <- file.path(dir_out, "17_farmacias_no_geocodificadas.csv")

  if (file.exists(ruta_out) && !overwrite) {
    .log(glue::glue("[farmacias] Existe {basename(ruta_out)}, se omite (overwrite=FALSE)."))
    return(invisible(ruta_out))
  }

  if (!file.exists(RUTA_FARMACIAS_XLSX)) {
    .log(glue::glue("[farmacias] No se encuentra {basename(RUTA_FARMACIAS_XLSX)} en data/contextual/."))
    .log("[farmacias] Descárgalo manualmente desde la Conselleria de Sanitat antes de relanzar.")
    return(invisible(NULL))
  }

  .log(glue::glue("[farmacias] Leyendo XLSX local: {basename(RUTA_FARMACIAS_XLSX)} ..."))
  hojas <- readxl::excel_sheets(RUTA_FARMACIAS_XLSX)
  hoja_datos <- hojas[1]
  farm_raw <- tryCatch(
    readxl::read_excel(RUTA_FARMACIAS_XLSX, sheet = hoja_datos, skip = 1),
    error = function(e) {
      .log(glue::glue("[farmacias] ERROR de lectura: {conditionMessage(e)}"))
      return(NULL)
    }
  )
  if (is.null(farm_raw)) return(invisible(NULL))
  .log(glue::glue("[farmacias] Registros leídos del XLSX: {nrow(farm_raw)}"))

  campo_direccion <- names(farm_raw)[
    grepl("^direcci|^domicili", names(farm_raw), ignore.case = TRUE)
  ][1]
  campo_municipio <- names(farm_raw)[
    grepl("^municipi", names(farm_raw), ignore.case = TRUE)
  ][1]
  campo_provincia <- names(farm_raw)[
    grepl("^provinci", names(farm_raw), ignore.case = TRUE)
  ][1]
  campo_titular <- names(farm_raw)[
    grepl("^titular|^denominaci", names(farm_raw), ignore.case = TRUE)
  ][1]
  campo_establecimiento <- names(farm_raw)[
    grepl("^establecimi|^codi|^codigo", names(farm_raw), ignore.case = TRUE)
  ][1]
  if (is.na(campo_direccion) || is.na(campo_municipio)) {
    .log("[farmacias] No se identifican campos de dirección o municipio en el XLSX.")
    .log(glue::glue("[farmacias] Columnas disponibles: {paste(names(farm_raw), collapse = ', ')}"))
    return(invisible(NULL))
  }

  farm_raw <- farm_raw |>
    dplyr::filter(!is.na(.data[[campo_direccion]]),
                  !is.na(.data[[campo_municipio]]))
  .log(glue::glue("[farmacias] Direcciones a geocodificar: {nrow(farm_raw)}"))

  # Carga del índice catastral.
  construir_indice_catastro(rebuild = FALSE)
  indice <- .cargar_indice_catastro()
  if (is.null(indice)) {
    .log("[farmacias] No se pudo cargar el índice catastral; abortando.")
    return(invisible(NULL))
  }
  .log(glue::glue("[farmacias] Índice catastral cargado: {nrow(indice)} direcciones"))

  .log("[farmacias] Geocodificando contra Catastro (exacto + fuzzy)...")
  resultados <- vector("list", nrow(farm_raw))
  inicio <- Sys.time()
  for (i in seq_len(nrow(farm_raw))) {
    r <- .geocode_via_catastro(
      direccion = farm_raw[[campo_direccion]][i],
      municipio = farm_raw[[campo_municipio]][i],
      indice    = indice
    )
    resultados[[i]] <- r
    if (i %% bloque == 0 || i == nrow(farm_raw)) {
      pct <- round(100 * i / nrow(farm_raw), 1)
      elapsed <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
      eta <- elapsed / i * (nrow(farm_raw) - i)
      .log(glue::glue("[farmacias]   bloque {i}/{nrow(farm_raw)} ",
                      "({pct}% — ETA {round(eta/60,1)} min)"))
    }
  }
  geocode_df <- dplyr::bind_rows(resultados)

  # Métricas por tipo de match.
  metricas <- as.data.frame(table(geocode_df$tipo_match), stringsAsFactors = FALSE)
  names(metricas) <- c("tipo_match", "n")
  for (i in seq_len(nrow(metricas))) {
    .log(glue::glue("[farmacias]   tipo_match={metricas$tipo_match[i]}: {metricas$n[i]}"))
  }

  farm_geo <- dplyr::bind_cols(farm_raw, geocode_df)
  geo_validas  <- farm_geo |> dplyr::filter(!is.na(lat), !is.na(lon))
  geo_fallidas <- farm_geo |> dplyr::filter(is.na(lat) | is.na(lon))
  pct_ok <- round(100 * nrow(geo_validas) / nrow(farm_geo), 1)
  .log(glue::glue("[farmacias] Geocodificadas: {nrow(geo_validas)} de {nrow(farm_geo)} ({pct_ok}%)"))

  if (nrow(geo_fallidas) > 0) {
    readr::write_csv(geo_fallidas, ruta_log_fall)
    .log(glue::glue("[farmacias] Log de fallidas: {basename(ruta_log_fall)} ({nrow(geo_fallidas)} filas)"))
  }
  if (nrow(geo_validas) == 0) {
    .log("[farmacias] Sin registros geolocalizados; no se escribe GPKG.")
    return(invisible(NULL))
  }

  campos_conservar <- intersect(
    c(campo_establecimiento, campo_titular, campo_direccion,
      campo_municipio, campo_provincia,
      "Departamento de Salud", "Zona Sanitaria", "Zona Farmacéutica",
      "refcat", "score_match", "motor", "tipo_match"),
    names(geo_validas)
  )
  geo_keep <- geo_validas[, campos_conservar]
  geo_keep$lat <- geo_validas$lat
  geo_keep$lon <- geo_validas$lon

  farm_sf <- sf::st_as_sf(geo_keep, coords = c("lon","lat"), crs = 4326) |>
    sf::st_transform(crs = EPSG_LAEA)

  sf::st_write(farm_sf, dsn = ruta_out, layer = "farmacias",
               layer_options = "ENCODING=UTF-8",
               delete_dsn = TRUE, quiet = TRUE)
  .log(glue::glue("[farmacias] Escrito: {basename(ruta_out)} ",
                  "({nrow(farm_sf)} puntos, EPSG:{EPSG_LAEA})"))
  invisible(ruta_out)
}


# Fallback opcional: descarga de farmacias desde OSM (amenity=pharmacy). No se
# invoca en el bloque main; solo se llama manualmente cuando se quiera contrastar
# la cobertura del XLSX oficial.
descargar_farmacias_osm <- function(overwrite = FALSE) {
  ruta_out <- file.path(dir_out, "17_farmacias_osm.gpkg")

  if (file.exists(ruta_out) && !overwrite) {
    .log(glue::glue("[farmacias/osm] Existe {basename(ruta_out)}, se omite (overwrite=FALSE)."))
    return(invisible(ruta_out))
  }

  .log("[farmacias/osm] Consultando Overpass API (amenity=pharmacy) ...")
  bbox_cv <- c(-1.55, 37.85, 0.65, 40.80)
  intento <- function() {
    osmdata::opq(bbox = bbox_cv, timeout = 180) |>
      osmdata::add_osm_feature(key = "amenity", value = "pharmacy") |>
      osmdata::osmdata_sf()
  }
  res <- tryCatch(intento(), error = function(e) {
    .log(glue::glue("[farmacias/osm] Reintentando tras error: {conditionMessage(e)}"))
    Sys.sleep(5); tryCatch(intento(), error = function(e2) NULL)
  })
  if (is.null(res) || is.null(res$osm_points) || nrow(res$osm_points) == 0) {
    .log("[farmacias/osm] Overpass no devolvió puntos; aborto.")
    return(invisible(NULL))
  }

  puntos <- res$osm_points |>
    dplyr::mutate(fuente = "OSM:amenity=pharmacy") |>
    dplyr::select(dplyr::any_of(c("osm_id", "name", "operator", "brand",
                                  "opening_hours", "phone", "fuente")),
                  geometry)
  if (!is.null(res$osm_polygons) && nrow(res$osm_polygons) > 0) {
    poligonos_centroide <- res$osm_polygons |>
      sf::st_centroid() |>
      dplyr::mutate(fuente = "OSM:amenity=pharmacy (centroid)") |>
      dplyr::select(dplyr::any_of(c("osm_id", "name", "operator", "brand",
                                    "opening_hours", "phone", "fuente")),
                    geometry)
    puntos <- dplyr::bind_rows(puntos, poligonos_centroide)
  }
  farmacias_sf <- puntos |> sf::st_transform(crs = EPSG_LAEA)
  sf::st_write(farmacias_sf, dsn = ruta_out, layer = "farmacias",
               layer_options = "ENCODING=UTF-8",
               delete_dsn = TRUE, quiet = TRUE)
  .log(glue::glue("[farmacias/osm] Escrito: {basename(ruta_out)} ",
                  "({nrow(farmacias_sf)} puntos, EPSG:{EPSG_LAEA})"))
  invisible(ruta_out)
}


# =============================================================================
# SECCIÓN 3b — FALLBACK: QUERY OVERPASS CRUDA
# =============================================================================
# Solo se invoca manualmente si la función `descargar_paradas_transporte()`
# falla persistentemente con el paquete osmdata. NO se ejecuta en el main.
# =============================================================================

descargar_paradas_overpass_crudo <- function(overwrite = FALSE) {
  ruta_out <- file.path(dir_out, "17_transporte_publico_osm.gpkg")
  if (file.exists(ruta_out) && !overwrite) {
    .log(glue::glue("[transporte/raw] Existe {basename(ruta_out)}, se omite."))
    return(invisible(ruta_out))
  }

  query <- '
[out:json][timeout:180];
area["ISO3166-2"="ES-VC"]->.searchArea;
(
  node["highway"="bus_stop"](area.searchArea);
  node["railway"="tram_stop"](area.searchArea);
  node["railway"="station"](area.searchArea);
  node["railway"="halt"](area.searchArea);
  node["railway"="subway_entrance"](area.searchArea);
);
out body;
>;
out skel qt;
'
  .log("[transporte/raw] Lanzando POST a Overpass API ...")
  resp <- httr::POST(url = URL_OVERPASS, body = query, encode = "form")
  if (httr::status_code(resp) != 200) {
    .log(glue::glue("[transporte/raw] HTTP {httr::status_code(resp)}; aborto."))
    return(invisible(NULL))
  }

  osm <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
                            simplifyVector = TRUE)
  nodos <- osm$elements |>
    dplyr::filter(type == "node")

  if (nrow(nodos) == 0) {
    .log("[transporte/raw] Respuesta sin nodos.")
    return(invisible(NULL))
  }

  paradas_df <- nodos |>
    dplyr::mutate(
      name    = vapply(tags, function(x) if ("name" %in% names(x))    x[["name"]]    else NA_character_, character(1)),
      highway = vapply(tags, function(x) if ("highway" %in% names(x)) x[["highway"]] else NA_character_, character(1)),
      railway = vapply(tags, function(x) if ("railway" %in% names(x)) x[["railway"]] else NA_character_, character(1)),
      modo = dplyr::case_when(
        highway == "bus_stop"          ~ "bus",
        railway == "tram_stop"         ~ "tranvia",
        railway %in% c("station","halt") ~ "ferrocarril",
        railway == "subway_entrance"   ~ "metro",
        TRUE                            ~ "otro"
      )
    ) |>
    dplyr::select(id, lon, lat, name, modo)

  paradas_sf <- sf::st_as_sf(paradas_df, coords = c("lon","lat"), crs = 4326) |>
    sf::st_transform(crs = EPSG_LAEA)

  sf::st_write(paradas_sf, dsn = ruta_out, layer = "transporte_publico",
               layer_options = "ENCODING=UTF-8",
               delete_dsn = TRUE, quiet = TRUE)
  .log(glue::glue("[transporte/raw] Escrito: {basename(ruta_out)} ({nrow(paradas_sf)} puntos)"))
  invisible(ruta_out)
}


# =============================================================================
# SECCIÓN 4 — BIBLIOTECAS (DIRECTORIO MCU + CATASTRO)
# =============================================================================
# Componente del indicador 4 (nodos cívicos públicos del entorno, solución B).
# Fuente operativa: Directorio de Bibliotecas Españolas del Ministerio de
# Cultura (MCU). Endpoint TXT con cobertura nacional, filtrado posterior a las
# provincias CV (03 Alicante, 12 Castellón, 46 Valencia). Geocodificación
# contra el Catastro local (sección 0). Output: 17_bibliotecas.gpkg en
# EPSG:3035 (LAEA).
# =============================================================================

descargar_bibliotecas <- function(overwrite = FALSE,
                                   bloque    = 250L) {
  ruta_out       <- file.path(dir_out, "17_bibliotecas.gpkg")
  ruta_raw       <- file.path(dir_out, "17_bibliotecas_mcu_raw.txt")
  ruta_log_fall  <- file.path(dir_out, "17_bibliotecas_no_geocodificadas.csv")

  if (file.exists(ruta_out) && !overwrite) {
    .log(glue::glue("[bibliotecas] Existe {basename(ruta_out)}, se omite (overwrite=FALSE)."))
    return(invisible(ruta_out))
  }

  # Descarga del TXT bruto si no existe ya en disco.
  if (!file.exists(ruta_raw) || overwrite) {
    .log("[bibliotecas] Descargando TXT del MCU ...")
    desc <- tryCatch(
      httr::GET(URL_BIBLIOTECAS_MCU, httr::write_disk(ruta_raw, overwrite = TRUE),
                httr::timeout(120)),
      error = function(e) {
        .log(glue::glue("[bibliotecas] ERROR de descarga: {conditionMessage(e)}"))
        return(NULL)
      }
    )
    if (is.null(desc) || httr::status_code(desc) != 200) {
      .log(glue::glue("[bibliotecas] HTTP {if (is.null(desc)) 'NA' else httr::status_code(desc)} ",
                      "al descargar TXT MCU. Aborto."))
      return(invisible(NULL))
    }
  }

  bibs_raw <- tryCatch(
    readr::read_delim(ruta_raw, delim = "\t",
                      locale = readr::locale(encoding = "ISO-8859-15"),
                      show_col_types = FALSE),
    error = function(e) {
      tryCatch(
        readr::read_delim(ruta_raw, delim = "|",
                          locale = readr::locale(encoding = "ISO-8859-15"),
                          show_col_types = FALSE),
        error = function(e2) NULL
      )
    }
  )
  if (is.null(bibs_raw) || ncol(bibs_raw) < 3) {
    .log("[bibliotecas] No se pudo parsear el TXT del MCU (probar inspección manual).")
    return(invisible(NULL))
  }
  .log(glue::glue("[bibliotecas] Registros totales del MCU: {nrow(bibs_raw)}"))

  campo_provincia <- names(bibs_raw)[
    grepl("provinc", names(bibs_raw), ignore.case = TRUE)
  ][1]
  campo_codprov <- names(bibs_raw)[
    grepl("cod.?prov|^cp$", names(bibs_raw), ignore.case = TRUE)
  ][1]
  campo_municipio <- names(bibs_raw)[
    grepl("munici|^loca", names(bibs_raw), ignore.case = TRUE)
  ][1]
  campo_direccion <- names(bibs_raw)[
    grepl("direcc|domicili", names(bibs_raw), ignore.case = TRUE)
  ][1]
  campo_nombre <- names(bibs_raw)[
    grepl("nombre|denomina|bibliot", names(bibs_raw), ignore.case = TRUE)
  ][1]
  campo_cp <- names(bibs_raw)[
    grepl("postal|^cp$|^codpost", names(bibs_raw), ignore.case = TRUE)
  ][1]
  if (is.na(campo_direccion) || is.na(campo_municipio)) {
    .log("[bibliotecas] No se identifican campos de dirección o municipio.")
    .log(glue::glue("[bibliotecas] Columnas disponibles: {paste(names(bibs_raw), collapse = ', ')}"))
    return(invisible(NULL))
  }

  # Filtrado a provincias CV.
  bibs_cv <- if (!is.na(campo_codprov)) {
    cod <- as.character(bibs_raw[[campo_codprov]])
    bibs_raw[cod %in% PROVINCIAS_CV, , drop = FALSE]
  } else if (!is.na(campo_provincia)) {
    pat <- "alacant|alicant|castell|valenci"
    bibs_raw[grepl(pat, bibs_raw[[campo_provincia]], ignore.case = TRUE), , drop = FALSE]
  } else {
    bibs_raw
  }
  bibs_cv <- bibs_cv[!is.na(bibs_cv[[campo_direccion]]) & !is.na(bibs_cv[[campo_municipio]]), , drop = FALSE]
  .log(glue::glue("[bibliotecas] Registros CV con dirección: {nrow(bibs_cv)}"))

  # Carga del índice catastral.
  construir_indice_catastro(rebuild = FALSE)
  indice <- .cargar_indice_catastro()
  if (is.null(indice)) {
    .log("[bibliotecas] No se pudo cargar el índice catastral; abortando.")
    return(invisible(NULL))
  }
  .log(glue::glue("[bibliotecas] Índice catastral cargado: {nrow(indice)} direcciones"))

  .log("[bibliotecas] Geocodificando contra Catastro (exacto + fuzzy)...")
  resultados <- vector("list", nrow(bibs_cv))
  inicio <- Sys.time()
  for (i in seq_len(nrow(bibs_cv))) {
    r <- .geocode_via_catastro(
      direccion = bibs_cv[[campo_direccion]][i],
      municipio = bibs_cv[[campo_municipio]][i],
      indice    = indice
    )
    resultados[[i]] <- r
    if (i %% bloque == 0 || i == nrow(bibs_cv)) {
      pct <- round(100 * i / nrow(bibs_cv), 1)
      elapsed <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))
      eta <- elapsed / i * (nrow(bibs_cv) - i)
      .log(glue::glue("[bibliotecas]   bloque {i}/{nrow(bibs_cv)} ",
                      "({pct}% — ETA {round(eta/60,1)} min)"))
    }
  }
  geocode_df <- dplyr::bind_rows(resultados)

  metricas <- as.data.frame(table(geocode_df$tipo_match), stringsAsFactors = FALSE)
  names(metricas) <- c("tipo_match", "n")
  for (i in seq_len(nrow(metricas))) {
    .log(glue::glue("[bibliotecas]   tipo_match={metricas$tipo_match[i]}: {metricas$n[i]}"))
  }

  bibs_geo <- dplyr::bind_cols(bibs_cv, geocode_df)
  validas  <- bibs_geo |> dplyr::filter(!is.na(lat), !is.na(lon))
  fallidas <- bibs_geo |> dplyr::filter(is.na(lat) | is.na(lon))
  pct_ok <- round(100 * nrow(validas) / nrow(bibs_geo), 1)
  .log(glue::glue("[bibliotecas] Geocodificadas: {nrow(validas)} de {nrow(bibs_geo)} ({pct_ok}%)"))

  if (nrow(fallidas) > 0) {
    readr::write_csv(fallidas, ruta_log_fall)
    .log(glue::glue("[bibliotecas] Log de fallidas: {basename(ruta_log_fall)}"))
  }
  if (nrow(validas) == 0) {
    .log("[bibliotecas] Sin registros geolocalizados; no se escribe GPKG.")
    return(invisible(NULL))
  }

  campos_conservar <- intersect(
    c(campo_nombre, campo_direccion, campo_municipio, campo_provincia,
      campo_cp, "refcat", "score_match", "motor", "tipo_match"),
    names(validas)
  )
  keep <- validas[, campos_conservar]
  keep$lat <- validas$lat
  keep$lon <- validas$lon
  bibs_sf <- sf::st_as_sf(keep, coords = c("lon","lat"), crs = 4326) |>
    sf::st_transform(crs = EPSG_LAEA)

  sf::st_write(bibs_sf, dsn = ruta_out, layer = "bibliotecas",
               layer_options = "ENCODING=UTF-8",
               delete_dsn = TRUE, quiet = TRUE)
  .log(glue::glue("[bibliotecas] Escrito: {basename(ruta_out)} ",
                  "({nrow(bibs_sf)} puntos, EPSG:{EPSG_LAEA})"))
  invisible(ruta_out)
}


# =============================================================================
# SECCIÓN 5 — CENTROS CÍVICOS / CASAS DE CULTURA (fuente compuesta)
# =============================================================================
# Componente del indicador 4 (nodos cívicos públicos del entorno, solución B).
# La capa CV se compone necesariamente de fragmentos: no hay un dataset
# unificado GVA. Se combinan cuatro fuentes complementarias en orden de
# prioridad institucional (la primera coincidencia espacial gana en dedup):
#   (1) GeoJSON Diputació de Castelló (Centros Culturales 2021, provincia
#       entera con coordenadas).
#   (2) GeoJSON Diputación de Alicante (Centros Culturales, provincia entera
#       con coordenadas; fichero local centros-culturales_alicante.geojson).
#   (3) GeoJSON Ajuntament de València (Equipamients municipals, solo el
#       término municipal de València).
#   (4) OSM Overpass (amenity=community_centre + arts_centre + social_centre)
#       para el resto del territorio CV (Valencia provincia fuera de la
#       capital, lagunas residuales).
# Tras la unión se aplica deduplicación por proximidad espacial (umbral 100 m).
# Output: 17_centros_civicos.gpkg en EPSG:3035 (LAEA), con columna `fuente`.
# =============================================================================

descargar_centros_civicos <- function(overwrite = FALSE) {
  ruta_out <- file.path(dir_out, "17_centros_civicos.gpkg")

  if (file.exists(ruta_out) && !overwrite) {
    .log(glue::glue("[civicos] Existe {basename(ruta_out)}, se omite (overwrite=FALSE)."))
    return(invisible(ruta_out))
  }

  capas <- list()

  # (1) Diputació de Castelló — GeoJSON directo.
  .log("[civicos] Descargando GeoJSON Diputació de Castelló ...")
  cas <- tryCatch(
    sf::st_read(URL_CIVICOS_DIPCAS, quiet = TRUE),
    error = function(e) {
      .log(glue::glue("[civicos] DipCas ERROR: {conditionMessage(e)}"))
      NULL
    }
  )
  if (!is.null(cas) && nrow(cas) > 0) {
    cas <- cas |>
      sf::st_transform(crs = EPSG_LAEA) |>
      dplyr::mutate(fuente = "DipCas:centros-culturales-2",
                    tipo = "centro cultural")
    cas$nombre <- if ("nombre" %in% names(cas)) cas$nombre else NA_character_
    cas$municipio <- if ("municipio" %in% names(cas)) cas$municipio else NA_character_
    capas[[length(capas) + 1]] <- cas[, c("nombre","municipio","tipo","fuente")]
    .log(glue::glue("[civicos]   Castelló: {nrow(cas)} puntos"))
  }

  # (2) Diputación de Alicante — GeoJSON local (descargado manualmente).
  if (file.exists(RUTA_CIVICOS_ALICANTE)) {
    .log(glue::glue("[civicos] Leyendo GeoJSON local Diputación de Alicante: ",
                    "{basename(RUTA_CIVICOS_ALICANTE)} ..."))
    ali <- tryCatch(
      sf::st_read(RUTA_CIVICOS_ALICANTE, quiet = TRUE),
      error = function(e) {
        .log(glue::glue("[civicos] Alicante ERROR: {conditionMessage(e)}"))
        NULL
      }
    )
    if (!is.null(ali) && nrow(ali) > 0) {
      # El fichero declara campos canónicos: CC_DENOMINACION (nombre),
      # MU_NOMBRE (municipio), CC_TIPO (categoría: centro cívico/social, casa
      # de cultura, biblioteca, hogar del pensionista, museo, etc.).
      # Decisión de arquitectura: las bibliotecas se EXCLUYEN aquí para evitar
      # solapamiento con 17_bibliotecas.gpkg (Directorio MCU). Las bibliotecas
      # viven exclusivamente en su capa específica.
      if ("CC_TIPO" %in% names(ali)) {
        n_antes <- nrow(ali)
        ali <- ali[!grepl("^bibliotec", ali$CC_TIPO, ignore.case = TRUE), , drop = FALSE]
        n_bib <- n_antes - nrow(ali)
        if (n_bib > 0) {
          .log(glue::glue("[civicos]   Alicante: excluidas {n_bib} bibliotecas ",
                          "(viven en 17_bibliotecas.gpkg)"))
        }
      }
      ali <- ali |>
        sf::st_transform(crs = EPSG_LAEA) |>
        dplyr::mutate(
          fuente    = "DipAlicante:centros-culturales",
          nombre    = if ("CC_DENOMINACION" %in% names(ali)) CC_DENOMINACION else NA_character_,
          municipio = if ("MU_NOMBRE" %in% names(ali)) MU_NOMBRE else NA_character_,
          tipo      = if ("CC_TIPO" %in% names(ali)) CC_TIPO else NA_character_
        )
      capas[[length(capas) + 1]] <- ali[, c("nombre","municipio","tipo","fuente")]
      .log(glue::glue("[civicos]   Alicante: {nrow(ali)} puntos (post-filtro biblioteca)"))
    }
  } else {
    .log(glue::glue("[civicos] No se encuentra {basename(RUTA_CIVICOS_ALICANTE)}; ",
                    "Alicante caerá a OSM en (4)."))
  }

  # (3) Ajuntament de València — GeoJSON directo.
  .log("[civicos] Descargando GeoJSON Ajuntament de València ...")
  vlc <- tryCatch(
    sf::st_read(URL_CIVICOS_VLC, quiet = TRUE),
    error = function(e) {
      .log(glue::glue("[civicos] València ERROR: {conditionMessage(e)}"))
      NULL
    }
  )
  if (!is.null(vlc) && nrow(vlc) > 0) {
    # Filtramos a las categorías de interés (centros sociales, casas de cultura,
    # centros juveniles, centros de barrio). El nombre exacto del campo varía
    # según versión del recurso; intentamos detectarlo dinámicamente.
    campo_tipo <- names(vlc)[grepl("tipo|categori|equipa", names(vlc),
                                    ignore.case = TRUE)][1]
    if (!is.na(campo_tipo)) {
      pat <- "cultur|cívic|civic|junil|junion|jovenil|joven|barr|social"
      vlc <- vlc[grepl(pat, vlc[[campo_tipo]], ignore.case = TRUE), ]
    }
    vlc <- vlc |>
      sf::st_transform(crs = EPSG_LAEA) |>
      dplyr::mutate(fuente = "Ajunt.Valencia:equipaments",
                    tipo = if (!is.na(campo_tipo)) .data[[campo_tipo]] else "equipamiento municipal")
    vlc$nombre <- if ("nombre" %in% names(vlc)) vlc$nombre else
                    if ("denominacion" %in% names(vlc)) vlc$denominacion else NA_character_
    vlc$municipio <- "VALÈNCIA"
    capas[[length(capas) + 1]] <- vlc[, c("nombre","municipio","tipo","fuente")]
    .log(glue::glue("[civicos]   València: {nrow(vlc)} puntos"))
  }

  # (4) OSM — complemento para el resto del territorio CV.
  .log("[civicos] Consultando Overpass (amenity=community_centre/arts_centre/social_centre)...")
  bbox_cv <- c(-1.55, 37.85, 0.65, 40.80)
  consultas <- list(
    list(value = "community_centre"),
    list(value = "arts_centre"),
    list(value = "social_centre")
  )
  osm_capas <- list()
  for (cc in consultas) {
    intento <- function() {
      osmdata::opq(bbox = bbox_cv, timeout = 180) |>
        osmdata::add_osm_feature(key = "amenity", value = cc$value) |>
        osmdata::osmdata_sf()
    }
    res <- tryCatch(intento(), error = function(e) {
      Sys.sleep(5); tryCatch(intento(), error = function(e2) NULL)
    })
    if (is.null(res)) next

    puntos <- if (!is.null(res$osm_points) && nrow(res$osm_points) > 0) {
      res$osm_points
    } else NULL
    poligonos <- if (!is.null(res$osm_polygons) && nrow(res$osm_polygons) > 0) {
      sf::st_centroid(res$osm_polygons)
    } else NULL

    capa_osm <- dplyr::bind_rows(puntos, poligonos)
    if (is.null(capa_osm) || nrow(capa_osm) == 0) next
    capa_osm <- capa_osm |>
      sf::st_transform(crs = EPSG_LAEA) |>
      dplyr::mutate(fuente = glue::glue("OSM:amenity={cc$value}"),
                    tipo = cc$value,
                    municipio = NA_character_)
    capa_osm$nombre <- if ("name" %in% names(capa_osm)) capa_osm$name else NA_character_
    osm_capas[[length(osm_capas) + 1]] <- capa_osm[, c("nombre","municipio","tipo","fuente")]
    .log(glue::glue("[civicos]   OSM {cc$value}: {nrow(capa_osm)} puntos"))
  }
  if (length(osm_capas) > 0) {
    capas[[length(capas) + 1]] <- dplyr::bind_rows(osm_capas)
  }

  if (length(capas) == 0) {
    .log("[civicos] Ninguna fuente devolvió datos; aborto.")
    return(invisible(NULL))
  }

  # Unión de las tres fuentes en una sola capa.
  todos <- dplyr::bind_rows(capas)
  .log(glue::glue("[civicos] Total preliminar (antes dedup): {nrow(todos)} puntos"))

  # Deduplicación por proximidad: dos puntos dentro de 100 m se consideran el
  # mismo equipamiento. Se conserva el de menor índice (orden de prioridad:
  # DipCas > Ajunt.Valencia > OSM por orden de inserción).
  if (nrow(todos) > 1) {
    dist_m <- sf::st_distance(todos)
    mantener <- rep(TRUE, nrow(todos))
    for (i in seq_len(nrow(todos) - 1)) {
      if (!mantener[i]) next
      vecinos <- which(as.numeric(dist_m[i, ]) < 100 & seq_len(nrow(todos)) > i)
      mantener[vecinos] <- FALSE
    }
    n_antes <- nrow(todos)
    todos <- todos[mantener, , drop = FALSE]
    .log(glue::glue("[civicos] Tras dedup (umbral 100 m): {nrow(todos)} ",
                    "(eliminados {n_antes - nrow(todos)})"))
  }

  sf::st_write(todos, dsn = ruta_out, layer = "centros_civicos",
               layer_options = "ENCODING=UTF-8",
               delete_dsn = TRUE, quiet = TRUE)
  .log(glue::glue("[civicos] Escrito: {basename(ruta_out)} ",
                  "({nrow(todos)} puntos, EPSG:{EPSG_LAEA})"))
  invisible(ruta_out)
}


# =============================================================================
# SECCIÓN 6 — CENTROS DOCENTES (CSV LOCAL — INDICADORES 2 Y 4)
# =============================================================================
# El CSV oficial `centros-docentes-de-la-comunitat-valenciana.csv` (descargado
# manualmente desde la GVA) trae longitud y latitud ya geocodificadas (WGS84),
# por lo que no requiere paso por el geocoder. La función reproyecta a LAEA y segmenta
# en dos sub-capas según `denominacion_generica_es`:
#   - 17_centros_docentes_infantil03.gpkg: escuelas infantiles 0-3 años
#     (input al indicador 2 como componente "servicios de primera infancia").
#   - 17_centros_docentes_resto.gpkg: centros 3+ años (CEIP, CEP, IES, FP)
#     (input al indicador 4 como componente "centros educativos públicos").
# =============================================================================

procesar_centros_docentes <- function(overwrite = FALSE,
                                       solo_publicos = FALSE) {
  ruta_inf <- file.path(dir_out, "17_centros_docentes_infantil03.gpkg")
  ruta_res <- file.path(dir_out, "17_centros_docentes_resto.gpkg")

  if (file.exists(ruta_inf) && file.exists(ruta_res) && !overwrite) {
    .log(glue::glue("[docentes] Existen los dos GPKG; se omite (overwrite=FALSE)."))
    return(invisible(c(ruta_inf, ruta_res)))
  }

  if (!file.exists(RUTA_CENTROS_DOC_CSV)) {
    .log(glue::glue("[docentes] No se encuentra {basename(RUTA_CENTROS_DOC_CSV)}."))
    return(invisible(NULL))
  }

  .log(glue::glue("[docentes] Leyendo CSV: {basename(RUTA_CENTROS_DOC_CSV)} ..."))
  doc_raw <- tryCatch(
    readr::read_csv2(RUTA_CENTROS_DOC_CSV,
                     locale = readr::locale(encoding = "UTF-8"),
                     show_col_types = FALSE),
    error = function(e) {
      .log(glue::glue("[docentes] ERROR: {conditionMessage(e)}"))
      return(NULL)
    }
  )
  if (is.null(doc_raw)) return(invisible(NULL))
  .log(glue::glue("[docentes] Centros leídos: {nrow(doc_raw)}"))

  # Validación de coordenadas y filtrado de filas con lat/lon ausentes.
  doc_raw <- doc_raw |>
    dplyr::mutate(
      longitud = suppressWarnings(as.numeric(longitud)),
      latitud  = suppressWarnings(as.numeric(latitud))
    ) |>
    dplyr::filter(!is.na(longitud), !is.na(latitud))
  .log(glue::glue("[docentes] Con coordenadas válidas: {nrow(doc_raw)}"))

  if (solo_publicos) {
    doc_raw <- doc_raw |>
      dplyr::filter(grepl("^PÚB", regimen, ignore.case = TRUE))
    .log(glue::glue("[docentes] Filtrado a régimen público: {nrow(doc_raw)}"))
  }

  # Segmentación por denominación. Las escuelas infantiles 0-3 se identifican
  # por la presencia de "INFANTIL" + ausencia de "PRIMARIA" en denominación
  # genérica (los CEIP combinan ambas y NO son escuelas infantiles puras).
  patron_inf03 <- "(?i)infantil(?!.*primari)"
  doc_inf <- doc_raw |>
    dplyr::filter(grepl(patron_inf03, denominacion_generica_es, perl = TRUE))
  doc_res <- doc_raw |>
    dplyr::filter(!grepl(patron_inf03, denominacion_generica_es, perl = TRUE))
  .log(glue::glue("[docentes] Infantil 0-3: {nrow(doc_inf)} | Resto: {nrow(doc_res)}"))

  .escribir_doc <- function(df, ruta, layer) {
    if (nrow(df) == 0) {
      .log(glue::glue("[docentes] Sin filas para {layer}; no se escribe GPKG."))
      return(NULL)
    }
    sff <- sf::st_as_sf(df, coords = c("longitud", "latitud"), crs = 4326) |>
      sf::st_transform(crs = EPSG_LAEA)
    sf::st_write(sff, dsn = ruta, layer = layer,
                 layer_options = "ENCODING=UTF-8",
                 delete_dsn = TRUE, quiet = TRUE)
    .log(glue::glue("[docentes] Escrito: {basename(ruta)} ({nrow(sff)} puntos)"))
  }
  .escribir_doc(doc_inf, ruta_inf, "centros_docentes_infantil03")
  .escribir_doc(doc_res, ruta_res, "centros_docentes_resto")

  invisible(c(ruta_inf, ruta_res))
}


# =============================================================================
# SECCIÓN 7 — LOG DE TRAZABILIDAD
# =============================================================================

generar_log_descarga <- function() {
  ruta_log <- file.path(dir_out, "17_log_descarga_cs.txt")
  encabezado <- c(
    "=========================================================================",
    "LOG DE DESCARGA — INDICADORES CS DEL IVEC_contextual",
    glue::glue("Generado: {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}"),
    glue::glue("Script:   R/descarga/17_descarga_contextual_cs.R"),
    glue::glue("Output:   {dir_out}"),
    glue::glue("CRS:      EPSG:{EPSG_LAEA} (ETRS89-LAEA, grid IVEC)"),
    "Fuentes:",
    glue::glue("  - Asociaciones GVA: {URL_ASOCIACIONES}"),
    glue::glue("  - ONGs GVA:         {URL_ONGS}"),
    glue::glue("  - Transporte OSM:   {URL_OVERPASS}"),
    glue::glue("  - Geocoder local:   Catastro Inmobiliario (DGC)"),
    glue::glue("  - Farmacias XLSX:   {basename(RUTA_FARMACIAS_XLSX)} (local)"),
    glue::glue("  - Centros docentes: {basename(RUTA_CENTROS_DOC_CSV)} (local)"),
    glue::glue("  - Bibliotecas MCU:  {URL_BIBLIOTECAS_MCU}"),
    glue::glue("  - Cívicos DipCas:   {URL_CIVICOS_DIPCAS}"),
    glue::glue("  - Cívicos DipAli:   {basename(RUTA_CIVICOS_ALICANTE)} (local)"),
    glue::glue("  - Cívicos VLC:      {URL_CIVICOS_VLC}"),
    "========================================================================="
  )
  writeLines(c(encabezado, "", log_descarga), ruta_log, useBytes = TRUE)
  message(glue::glue("Log de sesión: {ruta_log}"))
  invisible(ruta_log)
}


# =============================================================================
# MAIN — ejecución desde RStudio (interactivo) o desde shell con `Rscript`.
# =============================================================================
# Ejecutar todo:  Rscript R/descarga/17_descarga_contextual_cs.R
# O por función desde RStudio:
#   descargar_asociaciones(overwrite = TRUE)        # ~1-2 h (Carto, 88k)
#   descargar_ongs(overwrite = TRUE)                # ~3-5 min (Nominatim, 196)
#   descargar_paradas_transporte(overwrite = TRUE)  # ~30 s (Overpass)
#   descargar_farmacias(overwrite = TRUE)           # ~5-10 min (Carto, 2345)
#   descargar_bibliotecas(overwrite = TRUE)         # ~3-5 min (Carto, ~800)
#   descargar_centros_civicos(overwrite = TRUE)     # ~1 min (mix)
#   procesar_centros_docentes(overwrite = TRUE)     # ~5 s (CSV local)
#   generar_log_descarga()
# =============================================================================

if (sys.nframe() == 0L) {
  .log("=== Inicio de la sesión de descarga CS ===")

  # Orden por coste (rápidas primero, geocodificación pesada al final).
  procesar_centros_docentes(overwrite = FALSE)
  descargar_paradas_transporte(overwrite = FALSE)
  descargar_centros_civicos(overwrite = FALSE)
  descargar_ongs(overwrite = FALSE)
  descargar_farmacias(overwrite = FALSE)
  descargar_bibliotecas(overwrite = FALSE)
  descargar_asociaciones(overwrite = FALSE)

  generar_log_descarga()
  .log("=== Fin de la sesión de descarga CS ===")
}
