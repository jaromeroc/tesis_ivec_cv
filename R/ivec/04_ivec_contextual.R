# =============================================================================
# 04_ivec_contextual.R — CONSTRUCCIÓN DEL MÓDULO IVEC_contextual
# =============================================================================
# Módulo:   IVEC_contextual (vulnerabilidad y capacidades contextuales)
# Escala:   Cuadrícula LAEA 1×1 km (EPSG:3035, GRD_ID estándar Eurostat)
# Fórmula canónica:  IVEC_contextual_i = ½·VC_i* + ½·(1 − CS_i*)
#
# JERARQUÍA DE CIFRAS (distinguir las dos lecturas, irrevocable):
#   - Arquitectura ideal del módulo (cap. 3 §sec-indicadores-contextual-cap3):
#     abierta, con 15 indicadores teóricos justificados por mecanismo causal
#     (4 VC físico-residencial + 5 VC demográfico-poblacional + 5 CS). La
#     cifra ideal puede ampliarse si entra un nuevo indicador con justificación
#     causal sin que ello rompa la coherencia del módulo.
#   - Diseño operativo V1 (cap. 5 §sec-modulo-vccs, cap. 6 §sec-res-contextual,
#     pipeline): 13 indicadores efectivamente operacionalizados, dentro de una
#     tabla operativa de 14 entradas en la que cs_asociativo queda reservado
#     como slot pendiente para V2. La cifra canónica para metodología y
#     resultados es 13.
#
# Dimensión VC — Vulnerabilidad contextual (9 indicadores teóricos):
#   Sub-dimensión físico-residencial (4):
#     vc_antiguedad         → densidad edif. > 50 años por km²    (Observatorio GVA)
#     vc_superficie         → media superficie útil vivienda      (Catastro CAT, USO=V)
#     vc_tenencia           → % alquiler + otro régimen           (Censo 2021 INE)
#     vc_hacinamiento       → m² útiles residenciales per cápita  (Catastro CAT + SIP)
#   Sub-dimensión demográfico-poblacional (5):
#     vc_envejecimiento     → % > 65 años por celda                (SIP, f_nacim)
#     vc_inactividad        → % en cat. no-activas D4_lab          (SIP, D4_lab)
#     vc_estr_migratoria    → índice compuesto D3_migr             (SIP, D3_migr)
#     vc_educacion          → % adultos sin estudios/primarios    (Censo 2021 INE)
#     vc_dependencia_conv   → proxy convivencia intergeneracional (SIP, UCO×edad×D4_lab)
#
# Dimensión CS — Capacidades de soporte social (5 indicadores teóricos):
#     cs_asociativo            → asociaciones / 1000 hab               (DIFERIDO V2)
#     cs_proximidad            → centros de día + escuelas infantiles 0-3 (ACTIVO V1 parcial; farmacias V2)
#     cs_transporte            → tiempo a parada transporte público OSM   (ACTIVO V1 íntegro)
#     cs_sanitario → distancia a Centros Salud + RECESSO   (ACTIVO V1 íntegro, 15_ + 19_ gpkg)
#     cs_nodos_civicos         → centros docentes 3+ + GeoJSONs DipCas/DipAli/VLC + OSM (ACTIVO V1 parcial; bibliotecas V2)
#
# OPERACIONALIZACIÓN V1 (esta versión del pipeline):
#   Activos en pipeline:  13 indicadores (4 físico-residencial + 5 demográfico-poblacional + 4 CS)
#   Diferidos a V2:       1 indicador (cs_asociativo) + 2 componentes parciales (farmacias en cs_proximidad,
#                         bibliotecas en cs_nodos_civicos)
#
# Decisiones de fuente documentadas en el Apéndice D (A4-decisiones-metodologicas):
#   §D.9  — vc_educacion: Censo Anual 2025 (proyección seccional)
#   §D.10 — vc_hacinamiento: Catastro CAT Type 15 USO=V + PARCELA SHP + SIP
#   §D.11 — vc_tenencia: Censo 2021 INE (proyección seccional)
#   §D.12 — vc_superficie: Catastro CAT Type 15 USO=V (media)
#   §D.13 — cs_sanitario: red Centros Salud + RECESSO, distancia mínima
#
# Nota sobre la fórmula en V1: la fórmula canónica del módulo
# IVEC_contextual = ½·VC* + ½·(1-CS*) opera en V1 con CS construido como media
# equiponderada de los cuatro indicadores CS activos (cs_sanitario,
# cs_transporte, cs_proximidad parcial, cs_nodos_civicos parcial). El quinto
# slot (cs_asociativo) está reservado en la tabla operativa para activación
# en V2, sin que la arquitectura del módulo requiera ninguna modificación.
#
# Reasignación de mayo 2026: el indicador de accesibilidad a equipamientos
# sanitarios y sociales, inicialmente denominado cs_sanitario y clasificado en
# VC físico-residencial, ha sido renombrado a cs_sanitario y
# reasignado a CS por el principio de mecanismo causal: la accesibilidad a
# centros de salud y a centros de servicios sociales es un recurso institucional
# del entorno que compensa la fragilidad individual (mecanismo CS de soporte),
# no una condición material del entorno construido (mecanismo VC físico-residencial).
# Decisión documentada en cap. 3 §sec-indicadores-contextual-cap3 y en A4 §D.13.
#
# Reclasificación de mayo 2026: el indicador de convivencia intergeneracional,
# inicialmente denominado cs_cohesion_conv y clasificado en CS, ha sido
# renombrado a vc_dependencia_conv y reasignado a VC demográfico-poblacional
# tras la validación empírica que evidenció correlación positiva con D7-alto
# (ρ = +0,20), incompatible con la interpretación de "capacidad protectora"
# de CS. El indicador capta presencia de dependencia funcional intergeneracional,
# no soporte. Esta decisión se documenta en cap. 5 §sec-vccs-decisiones y en
# A4 §D.dependencia-conv.
#
# Normalización:
#   Paso 1 — Percentil por celda: cada indicador se transforma en rango
#             percentílico midpoint sobre el conjunto de celdas activas.
#   Paso 2 — Min-max dimensional: VC* y CS* se normalizan al rango [0,1].
#
# Fuentes (V1):
#   data/SIP/results/SIP_final.parquet           — microdatos SIP geocodificados
#   data/contextual/0801_GESIEE_antiguedad.gpkg  — edif. ≥ 50 años (Observatorio)
#
# Salida:
#   data/base/ivec_resultados/ivec_contextual.parquet
#   data/base/ivec_resultados/tabla_indicadores_contextual.rds
#   data/base/ivec_resultados/validacion_contextual.rds
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(here)
library(psych)       # alfa de Cronbach, omega de McDonald

# ── Reproducibilidad ─────────────────────────────────────────────────────────
set.seed(1972)

# ── Fechas de referencia ──────────────────────────────────────────────────────
FECHA_EXTRACCION <- as.Date("2025-06-01")   # SIP
ANYO_ANTIGUEDAD  <- 1975L                    # umbral > 50 años a 2025

# ── Umbrales ─────────────────────────────────────────────────────────────────
UMBRAL_CELDA <- 15L   # mínimo de individuos por celda (decisión D2)
                       # Aplicado en simetría con IVEC_base.

# ── Rutas ────────────────────────────────────────────────────────────────────
ruta_sip_final      <- here("data/SIP/results/SIP_final.parquet")
ruta_antiguedad     <- here("data/contextual/0801_GESIEE_antiguedad.gpkg")
dir_out             <- here("data/base/ivec_resultados")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

# Rutas de fuentes externas. Las que están disponibles llevan el nombre real;
# las pendientes llevan prefijo PENDIENTE_ y se activan al depositar el dato.
ruta_educacion_csv  <- c(
  here("data/contextual/Educacion_alicante.csv"),
  here("data/contextual/Educacion_valencia.csv"),
  here("data/contextual/Educacion_castellon.csv")
)
ruta_actividad_csv  <- here("data/contextual/Actividad.csv")
ruta_pais_csv       <- here("data/contextual/Pais_nacimiento.csv")
ruta_pais_act_csv   <- here("data/contextual/Pais_nacimiento_actividad.csv")
ruta_secciones_shp  <- here("data/mallas/malla_administrativa/ca_seccion_censal/ca_seccion_censal_20260405.shp")
ruta_delim_comarcas <- here("data/mallas/malla_administrativa/Delimitaciones_comarcas.gpkg")
ruta_delim_munic    <- here("data/mallas/malla_administrativa/Delimitaciones_municipios.gpkg")
ruta_delim_prov     <- here("data/mallas/malla_administrativa/Delimitaciones_provincias.gpkg")
ruta_delim_cv       <- here("data/mallas/malla_administrativa/Delimitaciones_CV.gpkg")
ruta_delim_dana     <- here("data/mallas/malla_administrativa/Delimitaciones_dana2024.gpkg")
# Rutas de las fuentes activas en V1
ruta_catastro_dir   <- here("data/contextual/catastro")
ruta_tenencia_csv   <- here("data/contextual/C2021_Indicadores.csv")
ruta_acceso_salud   <- here("data/contextual/15_SistemaValencianoSalud.gpkg")
ruta_acceso_sssoc   <- here("data/contextual/19_RECESSO.gpkg")
# Rutas de fuentes CS pendientes
ruta_asociaciones   <- here("data/contextual/17_asociaciones.gpkg")        # V2: diferido
ruta_recesso        <- here("data/contextual/19_RECESSO.gpkg")              # cs_proximidad y cs_sanitario
ruta_centros_docentes <- here("data/contextual/centros-docentes-de-la-comunitat-valenciana.csv")
ruta_transporte_osm <- here("data/contextual/17_transporte_publico_osm.gpkg")  # cs_transporte (activo V1)
ruta_civicos        <- here("data/contextual/17_centros_civicos.gpkg")      # cs_nodos_civicos (compuesto)
ruta_bibliotecas    <- here("data/contextual/17_bibliotecas.gpkg")          # ampliación pendiente (V2 nodos cívicos)
ruta_farmacias      <- here("data/contextual/17_farmacias.gpkg")            # ampliación pendiente (V2 proximidad)
ruta_electoral      <- here("data/contextual/PENDIENTE_electoral_2023.csv")
# Directorio de trabajo para extracción del Catastro (cacheado)
ruta_catastro_work  <- here("data/contextual/catastro/_extracted")
# Provincias del Catastro
PROVINCIAS_CAT <- c("03", "12", "46")  # Alicante, Castellón, Valencia
# Constantes Catastro
CAT_USO_VIVIENDA <- "V"                # Filtro USO Type 15 = Vivienda
CAT_POS_USO      <- 428                # Posición 1-indexed del USO en Type 15
CAT_POS_REFCAT   <- c(31, 44)          # Posiciones del refcat de la parcela
CAT_POS_SUP      <- c(442, 451)        # Posiciones de la superficie construida (m², 10 chars)
# Indicadores que requieren inversión de signo antes del rank percentílico
# para mantener la convención de su dimensión:
#   - VC (vulnerabilidad): vc_superficie y vc_hacinamiento tienen dirección
#     natural inversa (más m2/persona = menos vulnerabilidad) -> se invierten
#     para que alto percentil = alta VC.
#   - CS (capacidad): cs_sanitario y cs_transporte son tiempos de desplazamiento
#     (más minutos = MENOS capacidad de soporte del entorno) -> se invierten para
#     que alto percentil = alta capacidad CS, coherente con su entrada en la
#     fórmula como (1 - CS*). Los indicadores CS de densidad (cs_proximidad,
#     cs_nodos_civicos) NO se invierten: más puntos = más capacidad.
INDICADORES_INVERSOS <- c("vc_superficie", "vc_hacinamiento",
                          "cs_sanitario", "cs_transporte")
# Parámetros de acceso a equipamientos
VELOCIDAD_PIE_KMH <- 4.5                # Velocidad de marcha estándar
FACTOR_SINUOSIDAD <- 1.30               # Corrección distancia recta → trayecto urbano

ANYO_REF_CENSO <- "2021"  # año canónico del Censo para nivel educativo

# ── Control de cache (checkpoints intermedios) ────────────────────────────────
# Dos checkpoints estratégicos para evitar recalcular las dos secciones más
# costosas del pipeline cuando se itera sobre los bloques 4.x:
#
#   (1) sip_indic_post_sec3.parquet  → sip_indic tras secciones 1-3
#       (carga SIP, reproyección UTM→LAEA, agregación de los 4 indicadores
#       SIP-derivados por celda). Ahorra ~5-10 min por ejecución.
#
#   (2) catastro_celdas.parquet      → catastro_celdas tras bloque 4.1bis
#       (join SHP×CAT con REFCAT para los 3.085.808 inmuebles residenciales
#       de las tres provincias). Ahorra ~15-25 min por ejecución.
#
# Para forzar recálculo: poner el flag a FALSE, o borrar el .parquet del
# directorio cache. Los checkpoints se invalidan manualmente cuando cambien
# las fuentes: si renuevas el SIP, borrar (1); si renuevas el Catastro,
# borrar (2).
USAR_CACHE_SIP      <- TRUE   # cache de sip_indic tras sección 3
USAR_CACHE_CATASTRO <- TRUE   # cache de catastro_celdas tras 4.1bis

ruta_cache_dir          <- here("data/base/ivec_resultados/_cache")
ruta_cache_sip_post_s3  <- file.path(ruta_cache_dir, "sip_indic_post_sec3.parquet")
ruta_cache_catastro_cel <- file.path(ruta_cache_dir, "catastro_celdas.parquet")
dir.create(ruta_cache_dir, showWarnings = FALSE, recursive = TRUE)

message("\n══════════════════════════════════════════════════════════════")
message("  IVEC_contextual — inicio del pipeline")
message("  Versión: V1 — 13 indicadores activos en pipeline (cifra canónica para cap. 5 y cap. 6); cs_asociativo diferido a V2; arquitectura ideal cap. 3 = 15 indicadores teóricos")
message("  Cache SIP:      ", if (USAR_CACHE_SIP) "ACTIVADO" else "desactivado",
        " (", basename(ruta_cache_sip_post_s3),
        if (file.exists(ruta_cache_sip_post_s3)) ": presente" else ": NO presente", ")")
message("  Cache Catastro: ", if (USAR_CACHE_CATASTRO) "ACTIVADO" else "desactivado",
        " (", basename(ruta_cache_catastro_cel),
        if (file.exists(ruta_cache_catastro_cel)) ": presente" else ": NO presente", ")")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# CHECKPOINT 1 — Carga sip_indic desde cache si existe y está activado.
# Si no, ejecutamos las secciones 1, 2 y 3 normalmente y guardamos al final.
# =============================================================================
sip_indic_desde_cache <- FALSE
if (USAR_CACHE_SIP && file.exists(ruta_cache_sip_post_s3)) {
  message("── Checkpoint 1: cargando sip_indic desde cache ───────────────")
  message("  Fichero: ", basename(ruta_cache_sip_post_s3))
  sip_indic <- arrow::read_parquet(ruta_cache_sip_post_s3)
  sip_indic_desde_cache <- TRUE
  message("  Cargadas ", format(nrow(sip_indic), big.mark = "."),
          " celdas. Saltando secciones 1, 2 y 3.")
}

if (!sip_indic_desde_cache) {

# =============================================================================
# SECCIÓN 1 — CARGA Y FILTROS DE LOS MICRODATOS DEL SIP
# =============================================================================

message("── Sección 1: carga de microdatos del SIP ─────────────────────")

cols_final <- c(
  "UCO_CODI", "N_SIP",
  "f_nacim",                    # edad → envejecimiento territorial
  "D4_lab(cod)",                # inactividad → concentración inactividad
  "D3_migr(cod)",               # trayectoria migratoria → estructura migratoria
  "empadronamiento(cod)",       # anclaje padronal → rotación residencial proxy
  "D2_resid(cod)",              # filtro residencia CV
  "ST_X", "ST_Y"
)

sip <- read_parquet(ruta_sip_final, col_select = all_of(cols_final))
message("  Registros cargados: ", format(nrow(sip), big.mark = "."))

# Filtro D2: residentes en la CV (mismo criterio que IVEC_base)
n_total <- nrow(sip)
sip <- sip |> filter(`D2_resid(cod)` == "1")
message("  Tras D2 (residentes CV): ", format(nrow(sip), big.mark = "."))

# Filtro geográfico: bbox CV en UTM30N (mismo criterio que IVEC_base)
sip <- sip |>
  filter(
    !is.na(ST_X), !is.na(ST_Y),
    ST_X > 620000, ST_X < 900000,
    ST_Y > 4175000, ST_Y < 4550000
  )
message("  Georreferenciados válidos: ", format(nrow(sip), big.mark = "."))


# =============================================================================
# SECCIÓN 2 — ASIGNACIÓN DE INDIVIDUOS AL GRID LAEA
# =============================================================================
# Mismo procedimiento que el módulo base: reproyección UTM30N → LAEA y
# construcción del GRD_ID por floor sobre coordenadas LAEA divididas entre 1000.

message("\n── Sección 2: asignación al grid LAEA ─────────────────────────")

pts_utm  <- st_as_sf(sip, coords = c("ST_X", "ST_Y"), crs = 25830,
                     remove = FALSE)
pts_laea <- st_transform(pts_utm, 3035)
coords_laea <- st_coordinates(pts_laea)

sip <- sip |>
  mutate(
    laea_N = floor(coords_laea[, 2] / 1000) * 1000L,
    laea_E = floor(coords_laea[, 1] / 1000) * 1000L,
    GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E)
  )

rm(pts_utm, pts_laea, coords_laea)
gc(verbose = FALSE)

n_celdas_brutas <- n_distinct(sip$GRD_ID)
message("  Celdas con al menos un residente: ", format(n_celdas_brutas, big.mark = "."))


# =============================================================================
# SECCIÓN 3 — INDICADORES SIP-DERIVADOS A NIVEL DE CELDA
# =============================================================================
# Los cuatro indicadores que se construyen por agregación ascendente del SIP
# directamente a la celda, sin paso por fuentes externas.

message("\n── Sección 3: indicadores SIP-derivados ────────────────────────")

# Edad calculada
sip <- sip |>
  mutate(
    f_nacim_date = as.Date(as.character(f_nacim), format = "%Y%m%d"),
    edad         = as.numeric(difftime(FECHA_EXTRACCION, f_nacim_date,
                                       units = "days")) / 365.25,
    edad         = if_else(edad < 0 | edad > 125, NA_real_, edad),
    es_mayor_65  = edad >= 65,
    es_activo_edad = edad >= 18 & edad < 65
  )

# Indicador-3.1: envejecimiento territorial — % > 65 años por celda
# Indicador-3.2: concentración inactividad — % en cat. no-activas D4_lab
# Indicador-3.3: estructura migratoria — índice compuesto ponderado D3_migr
# Indicador-3.4: convivencia intergeneracional (proxy cohesión por celda)

# Las categorías de inactividad de D4_lab agregan: A (discapacitado), B
# (no puede trabajar), D (desempleo), P/Q/R/S/T (combinaciones con
# discapacidad). Trabajan solo C (trabaja) y E/O (situaciones activas).
CAT_INACTIVIDAD <- c("A","B","D","P","Q","R","S","T")

# Pesos del índice de estructura migratoria por subgrupo de D3_migr.
# Justificación en cap. 5 §sec-vccs-fundamento y A4 §D.0.
PESOS_MIGR <- c(
  "21" = 1.00,   # Inmigrante reciente extranjero (≤ 2 años)
  "22" = 0.75,   # Inmigrante estancia media extranjero
  "23" = 0.40,   # Inmigrante antiguo extranjero
  "31" = 0.55,   # Reciente otra CCAA
  "32" = 0.35,   # Estancia media otra CCAA
  "33" = 0.20,   # Antiguo otra CCAA
  "10" = 0.00,   # No migrante
  "40" = NA_real_  # Otra situación (excluida del índice)
)

# Convivencia intergeneracional a nivel hogar (UCO_CODI):
# condiciones = hogar tiene ≥1 miembro > 65 años Y ≥1 miembro en edad 18-65.
message("  Calculando condiciones de convivencia intergeneracional por hogar...")
conv_hogar <- sip |>
  group_by(UCO_CODI) |>
  summarise(
    tiene_mayor   = any(es_mayor_65, na.rm = TRUE),
    tiene_adulto  = any(es_activo_edad, na.rm = TRUE),
    conv_intergen = tiene_mayor & tiene_adulto,
    .groups = "drop"
  )
sip <- sip |>
  left_join(conv_hogar |> select(UCO_CODI, conv_intergen), by = "UCO_CODI")
rm(conv_hogar)

# Agregación por celda
message("  Agregando los cuatro indicadores SIP por celda...")
sip_indic <- sip |>
  mutate(
    peso_migr = PESOS_MIGR[`D3_migr(cod)`]
  ) |>
  group_by(GRD_ID, laea_N, laea_E) |>
  summarise(
    n_individuos     = n(),
    # 3.1 envejecimiento
    vc_envejecimiento = mean(es_mayor_65, na.rm = TRUE),
    # 3.2 inactividad
    vc_inactividad    = mean(`D4_lab(cod)` %in% CAT_INACTIVIDAD, na.rm = TRUE),
    # 3.3 estructura migratoria (índice compuesto ponderado)
    vc_estr_migratoria = mean(peso_migr, na.rm = TRUE),
    # 3.4 dependencia convivencial intergeneracional
    #     (mayor + adulto cohabitan en misma UCO; captura presencia de
    #     dependencia funcional, no de capacidad protectora — reclasificado
    #     a VC demográfico-poblacional, ver cabecera del script)
    vc_dependencia_conv = mean(conv_intergen, na.rm = TRUE),
    .groups = "drop"
  )

message("  vc_envejecimiento:    media celda = ",
        round(mean(sip_indic$vc_envejecimiento, na.rm = TRUE), 3))
message("  vc_inactividad:       media celda = ",
        round(mean(sip_indic$vc_inactividad, na.rm = TRUE), 3))
message("  vc_estr_migratoria:   media celda = ",
        round(mean(sip_indic$vc_estr_migratoria, na.rm = TRUE), 3))
message("  vc_dependencia_conv:  media celda = ",
        round(mean(sip_indic$vc_dependencia_conv, na.rm = TRUE), 3))

# ── Guardar checkpoint 1 (sip_indic tras sección 3) ──────────────────────────
if (USAR_CACHE_SIP) {
  arrow::write_parquet(sip_indic, ruta_cache_sip_post_s3)
  message("── Checkpoint 1 guardado: ", basename(ruta_cache_sip_post_s3),
          " (", format(nrow(sip_indic), big.mark = "."), " celdas)")
}

}  # fin del if (!sip_indic_desde_cache); secciones 1, 2 y 3 cubiertas


# =============================================================================
# SECCIÓN 3.B — HELPERS DEL CATASTRO Y DE LAS REDES DE EQUIPAMIENTOS
# =============================================================================
# Funciones auxiliares utilizadas por los bloques 4.2 (vc_superficie),
# 4.4 (vc_hacinamiento) y 4.5 (cs_sanitario). Se concentran aquí para evitar
# repetición y permitir su reutilización si se añaden más indicadores
# derivados del Catastro Inmobiliario.

# 3.B.1 — Extracción de la cartografía vectorial del Catastro por provincia.
# Maneja correctamente los zip multipart (.zip + .z01) combinando primero
# las partes mediante `zip -s 0` (la herramienta zip estándar en Linux/macOS
# y Git Bash) y extrayendo después el archivo combinado. La operación se
# cachea: si la carpeta de destino ya contiene la estructura extraída no
# vuelve a ejecutarse.
extract_catastro_shp <- function(ruta_zip_principal, dir_destino) {
  dir.create(dir_destino, showWarnings = FALSE, recursive = TRUE)
  # Detectar si la extracción ya está hecha (por ejecución previa del pipeline
  # o por descompresión manual del usuario): basta con que existan carpetas
  # de municipio con el patrón "uA " característico del Catastro.
  marca <- file.path(dir_destino, "_extracted.ok")
  contenido_actual <- list.dirs(dir_destino, recursive = TRUE)
  ya_extraido <- length(contenido_actual) > 1 &&
    any(grepl("uA ", contenido_actual, fixed = TRUE))
  if (ya_extraido) {
    # Crear la marca si no existe, para acelerar futuras invocaciones
    if (!file.exists(marca)) file.create(marca)
    return(invisible(dir_destino))
  }
  # Si quedó un intento previo a medias (marca presente pero sin carpetas
  # de municipio), limpiar antes de reintentar
  if (file.exists(marca) && !ya_extraido) {
    message("    Limpiando extracción previa incompleta de ", basename(dir_destino), " ...")
    unlink(list.files(dir_destino, full.names = TRUE,
                      include.dirs = TRUE, all.files = TRUE,
                      no.. = TRUE),
           recursive = TRUE, force = TRUE)
  }
  # Detectar partes adicionales del zip (.z01, .z02, ...)
  partes <- list.files(dirname(ruta_zip_principal),
                       pattern = paste0("^",
                                        sub("\\.zip$", "", basename(ruta_zip_principal)),
                                        "\\.z[0-9]+$"),
                       full.names = TRUE, ignore.case = TRUE)
  combined <- file.path(dir_destino, "_combined.zip")
  if (length(partes) > 0) {
    if (!file.exists(combined)) {
      message("    Combinando multipart zip (", length(partes) + 1, " partes) ...")
      # Intento 1: 7-Zip en la ubicación estándar de Windows
      siete_zip_win <- "C:/Program Files/7-Zip/7z.exe"
      # Intento 2: 7z disponible en PATH (Linux/macOS con p7zip o Windows con
      # 7-Zip añadido al PATH)
      siete_zip_path <- Sys.which("7z")
      siete_zip <- if (file.exists(siete_zip_win)) siete_zip_win
                   else if (nzchar(siete_zip_path)) siete_zip_path
                   else ""
      ok <- FALSE
      if (nzchar(siete_zip)) {
        message("    Usando 7-Zip: ", siete_zip)
        # 7-Zip extrae directamente las multipart desde la parte .zip
        # principal, encontrando las .z01, .z02 ... automáticamente
        res <- tryCatch(
          suppressWarnings(system2(
            siete_zip,
            args = c("x", shQuote(ruta_zip_principal),
                     paste0("-o", shQuote(dir_destino)),
                     "-y"),
            stdout = FALSE, stderr = FALSE
          )),
          error = function(e) -1L
        )
        ok <- isTRUE(res == 0)
      }
      # Intento 3: `zip -s 0` (Linux/macOS/Git Bash). Combina las partes en
      # un único .zip "single part" que después se extrae con utils::unzip.
      if (!ok) {
        res2 <- tryCatch(
          suppressWarnings(system2(
            "zip", args = c("-s", "0", shQuote(ruta_zip_principal),
                            "--out", shQuote(combined)),
            stdout = FALSE, stderr = FALSE
          )),
          error = function(e) -1L
        )
        if (isTRUE(res2 == 0) && file.exists(combined) &&
            file.info(combined)$size > 0) {
          utils::unzip(combined, exdir = dir_destino)
          ok <- TRUE
        }
      }
      if (!ok) {
        stop("No se ha podido descomprimir el multipart zip ",
             basename(ruta_zip_principal), ". Instala 7-Zip ",
             "(https://www.7-zip.org/) o ejecuta manualmente: ",
             "7z x \"", ruta_zip_principal, "\" -o\"", dir_destino, "\"")
      }
    } else {
      # combined existía: queda del intento previo de `zip -s 0`
      utils::unzip(combined, exdir = dir_destino)
    }
  } else {
    utils::unzip(ruta_zip_principal, exdir = dir_destino)
  }
  file.create(marca)
  invisible(dir_destino)
}

# 3.B.2 — Parseo del fichero alfanumérico CAT de una provincia.
# Lee el zip provincial XX_U_*_CAT.zip que contiene un .CAT.gz por municipio;
# para cada municipio decomprime, filtra registros Tipo 15 (Bien Inmueble) por
# USO = "V" (Vivienda) y extrae refcat14 (parcela) y superficie construida.
# Las posiciones de los campos se definen como constantes al inicio del script
# y siguen el formato canónico de la Dirección General del Catastro.
parse_cat_provincia <- function(ruta_zip_cat) {
  message("    Procesando CAT de ", basename(ruta_zip_cat))
  zip_contents <- utils::unzip(ruta_zip_cat, list = TRUE)
  cat_gz_files <- zip_contents$Name[grepl("\\.CAT\\.gz$", zip_contents$Name,
                                          ignore.case = TRUE)]
  td <- tempfile()
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(ruta_zip_cat, files = cat_gz_files, exdir = td)
  archivos <- file.path(td, cat_gz_files)
  resultados <- vector("list", length(archivos))
  for (i in seq_along(archivos)) {
    con <- gzfile(archivos[i], encoding = "latin1")
    lineas <- readLines(con, warn = FALSE)
    close(con)
    if (length(lineas) == 0) next
    mascara_t15 <- substr(lineas, 1, 2) == "15"
    t15 <- lineas[mascara_t15]
    if (length(t15) == 0) next
    refcat14 <- substr(t15, CAT_POS_REFCAT[1], CAT_POS_REFCAT[2])
    uso      <- substr(t15, CAT_POS_USO, CAT_POS_USO)
    sup      <- suppressWarnings(
      as.numeric(substr(t15, CAT_POS_SUP[1], CAT_POS_SUP[2]))
    )
    resultados[[i]] <- tibble(
      refcat14   = refcat14,
      uso        = uso,
      superficie = sup
    )
  }
  unlink(td, recursive = TRUE)
  dplyr::bind_rows(resultados) |>
    dplyr::filter(uso == CAT_USO_VIVIENDA, !is.na(superficie), superficie > 0)
}

# 3.B.3 — Lectura de la capa PARCELA de un municipio extraído del SHP.
# Cada municipio está dentro de una carpeta cuya capa PARCELA viene en un
# .ZIP interno; se decomprime a tempfile, se lee con sf::st_read y se
# devuelve un sf con refcat14 (primeros 14 chars de REFCAT) y geometría
# del centroide reproyectada a LAEA.
leer_parcelas_municipio <- function(carpeta_municipio) {
  zip_parcela <- list.files(carpeta_municipio,
                            pattern = "PARCELA\\.ZIP$",
                            full.names = TRUE, ignore.case = TRUE)
  if (length(zip_parcela) == 0) return(NULL)
  td <- tempfile()
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  utils::unzip(zip_parcela[1], exdir = td)
  shp <- list.files(td, pattern = "PARCELA\\.shp$",
                    full.names = TRUE, ignore.case = TRUE)
  if (length(shp) == 0) {
    unlink(td, recursive = TRUE)
    return(NULL)
  }
  p <- tryCatch(
    sf::st_read(shp[1], quiet = TRUE),
    error = function(e) NULL
  )
  unlink(td, recursive = TRUE)
  if (is.null(p) || nrow(p) == 0) return(NULL)
  # El campo REFCAT del PARCELA SHP tiene 14 chars; coincide con refcat14
  p <- p |> sf::st_zm(drop = TRUE)
  if (!"REFCAT" %in% names(p)) return(NULL)
  p |>
    dplyr::transmute(refcat14 = substr(as.character(REFCAT), 1, 14)) |>
    sf::st_centroid() |>
    sf::st_transform(3035)
}

# 3.B.4 — Lectura completa del Catastro de una provincia (todas las parcelas).
# Itera sobre las carpetas-municipio extraídas en `dir_extract` y concatena
# los centroides de parcela con su refcat14, listos para join con el CAT.
leer_parcelas_provincia <- function(dir_extract_provincia) {
  # Estructura típica del Catastro descomprimido:
  #   <dir_extract_provincia>/XX_UA_23012026_SHP/XX001uA Atzubia/PARCELA.ZIP
  # Primer nivel: carpeta provincial XX_UA_*_SHP. Segundo nivel: municipios.
  primer_nivel <- list.dirs(dir_extract_provincia, recursive = FALSE,
                            full.names = TRUE)
  carpetas_shp <- primer_nivel[grepl("_UA_.*_SHP$", primer_nivel,
                                     ignore.case = TRUE)]
  if (length(carpetas_shp) > 0) {
    carpetas_muni <- unlist(lapply(carpetas_shp, function(d) {
      subs <- list.dirs(d, recursive = FALSE, full.names = TRUE)
      subs[grepl("uA ", subs, fixed = TRUE)]
    }))
  } else {
    # Caso alternativo: los municipios están en el primer nivel directamente
    carpetas_muni <- primer_nivel[grepl("uA ", primer_nivel, fixed = TRUE)]
  }
  message("    Leyendo PARCELA de ", length(carpetas_muni), " municipios ...")
  if (length(carpetas_muni) == 0) return(NULL)
  res <- vector("list", length(carpetas_muni))
  for (i in seq_along(carpetas_muni)) {
    res[[i]] <- leer_parcelas_municipio(carpetas_muni[i])
  }
  res <- res[!vapply(res, is.null, logical(1))]
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}


# =============================================================================
# SECCIÓN 4 — INDICADORES DE FUENTES EXTERNAS
# =============================================================================
# Cada bloque tiene una guarda de existencia de fichero: el indicador se
# computa solo si la fuente está disponible. Cuando una fuente externa llegue,
# basta con depositar el archivo en data/contextual/ con la ruta esperada y
# eliminar el guardián para activar el bloque.

message("\n── Sección 4: indicadores de fuentes externas ──────────────────")

# Inicialización: todas las celdas del grid con NA en los indicadores externos
sip_indic <- sip_indic |>
  mutate(
    vc_antiguedad     = NA_real_,
    vc_superficie     = NA_real_,
    vc_tenencia       = NA_real_,
    vc_hacinamiento  = NA_real_,
    cs_sanitario      = NA_real_,
    vc_educacion      = NA_real_,
    cs_asociativo     = NA_real_,
    cs_proximidad     = NA_real_,
    cs_transporte     = NA_real_,
    cs_nodos_civicos  = NA_real_
  )


# ── 4.1 vc_antiguedad — densidad edificios ≥ 50 años por km² ─────────────────
# Fuente: 0801_GESIEE_antiguedad.gpkg (Observatorio Hábitat GVA).
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3): la fragilidad
# física del parque construido amplifica el daño sobre los habitantes más
# frágiles ante perturbaciones que interrumpen servicios básicos o deterioran
# las condiciones habitacionales. En V1 se opera la densidad por km² en lugar
# del % sobre el total porque el dataset publicado por el Observatorio no
# incluye el denominador; la limitación se documenta en A4 §D.1.
if (file.exists(ruta_antiguedad)) {
  message("  4.1 vc_antiguedad: cargando edificios antiguos del Observatorio...")
  edif <- st_read(ruta_antiguedad, quiet = TRUE)
  # Compatibilidad de nombre de columna de geometría: el gpkg del Observatorio
  # publica la geometría bajo `geom` (no `geometry`); como objeto sf, la columna
  # de geometría se preserva automáticamente al hacer select sobre atributos
  # sin nombrarla. Se aplica st_zm() para drop de la dimensión Z si la hubiera.
  edif <- edif |>
    st_zm(drop = TRUE) |>
    select(refcat, anyo_const_inm_masantiguo)

  # Centroide del polígono y reproyección a LAEA
  edif_pts <- edif |>
    st_centroid() |>
    st_transform(3035)
  coords_e <- st_coordinates(edif_pts)
  edif_grid <- tibble(
    refcat = edif$refcat,
    anyo   = edif$anyo_const_inm_masantiguo,
    laea_N = floor(coords_e[, 2] / 1000) * 1000L,
    laea_E = floor(coords_e[, 1] / 1000) * 1000L
  ) |>
    mutate(GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E)) |>
    # Filtrar al universo de antiguos (≤ 1975 = > 50 años a 2025)
    filter(!is.na(anyo) & anyo <= ANYO_ANTIGUEDAD)

  rm(edif, edif_pts, coords_e)

  # Conteo por celda
  antiguos_cell <- edif_grid |>
    count(GRD_ID, name = "n_edif_antiguos")
  rm(edif_grid)

  # Join con la tabla de indicadores
  sip_indic <- sip_indic |>
    select(-vc_antiguedad) |>
    left_join(antiguos_cell, by = "GRD_ID") |>
    mutate(
      vc_antiguedad = replace_na(n_edif_antiguos, 0L)
      # Celdas sin edificios antiguos reciben 0: ausencia efectiva de parque
      # antiguo en el km² (interpretación coherente con la hipótesis causal).
    ) |>
    select(-n_edif_antiguos)
  rm(antiguos_cell)

  message("  vc_antiguedad: media celda = ",
          round(mean(sip_indic$vc_antiguedad, na.rm = TRUE), 1),
          " edif./km²; máx = ", max(sip_indic$vc_antiguedad, na.rm = TRUE))
  message("  vc_antiguedad: celdas con ≥ 1 edif. antiguo = ",
          sum(sip_indic$vc_antiguedad > 0, na.rm = TRUE),
          " (", round(100 * mean(sip_indic$vc_antiguedad > 0, na.rm = TRUE), 1), "%)")
} else {
  message("  4.1 vc_antiguedad: PENDIENTE (fuente no encontrada en ",
          basename(ruta_antiguedad), ")")
}

# ── 4.1bis — Carga conjunta de fuentes del Catastro (SHP + CAT) ─────────────
# Operación pesada que se ejecuta una sola vez y cuyo resultado alimenta los
# bloques 4.2 (vc_superficie) y 4.4 (vc_hacinamiento). Recorre las tres
# provincias del Catastro (Alicante 03, Castellón 12, Valencia 46), extrae las
# cartografías vectoriales si no lo están ya, parsea los CAT alfanuméricos
# para los Type 15 con USO='V' y une cada inmueble residencial a la geometría
# de su parcela mediante REFCAT_14. Decisión documentada en A4 §D.10 y §D.12.
#
# CHECKPOINT 2 — Si el resultado ya está cacheado y el flag USAR_CACHE_CATASTRO
# está activado, se carga del disco y se salta todo el bloque de SHP+CAT.

catastro_celdas_desde_cache <- FALSE
if (USAR_CACHE_CATASTRO && file.exists(ruta_cache_catastro_cel)) {
  message("── Checkpoint 2: cargando catastro_celdas desde cache ────────")
  message("  Fichero: ", basename(ruta_cache_catastro_cel))
  catastro_celdas <- arrow::read_parquet(ruta_cache_catastro_cel)
  catastro_celdas_desde_cache <- TRUE
  message("  Cargados ", format(nrow(catastro_celdas), big.mark = "."),
          " inmuebles residenciales. Saltando bloque 4.1bis (SHP + CAT).")
}

# Localización de los ficheros del Catastro: pueden estar en la raíz del
# directorio (descarga reciente, sin descomprimir) o ya movidos a la carpeta
# de extracción cacheada `_extracted/<prov>/` (situación típica tras una
# descompresión manual del usuario). Se busca en ambas ubicaciones.
get_catastro_path <- function(prov, sufijo) {
  candidatos <- c(
    file.path(ruta_catastro_dir, paste0(prov, sufijo)),
    file.path(ruta_catastro_dir, "_extracted", prov, paste0(prov, sufijo))
  )
  existentes <- candidatos[file.exists(candidatos)]
  if (length(existentes) == 0) return(NA_character_)
  existentes[1]
}

catastro_disponible <- all(vapply(PROVINCIAS_CAT, function(p) {
  !is.na(get_catastro_path(p, "_UA_23012026_SHP.zip")) &&
    !is.na(get_catastro_path(p, "_U_23012026_CAT.zip"))
}, logical(1)))

if (!catastro_celdas_desde_cache && catastro_disponible) {
  message("  4.1bis Catastro: cargando SHP + CAT de las tres provincias ...")
  dir.create(ruta_catastro_work, showWarnings = FALSE, recursive = TRUE)
  catastro_celdas_lista <- vector("list", length(PROVINCIAS_CAT))
  for (k in seq_along(PROVINCIAS_CAT)) {
    prov <- PROVINCIAS_CAT[k]
    ruta_shp_prov <- get_catastro_path(prov, "_UA_23012026_SHP.zip")
    ruta_cat_prov <- get_catastro_path(prov, "_U_23012026_CAT.zip")
    dir_extract   <- file.path(ruta_catastro_work, prov)
    extract_catastro_shp(ruta_shp_prov, dir_extract)
    parcelas <- leer_parcelas_provincia(dir_extract)
    if (is.null(parcelas) || nrow(parcelas) == 0) {
      warning("Provincia ", prov, ": parcelas no leídas")
      next
    }
    coords_p <- sf::st_coordinates(parcelas)
    parcelas_tab <- tibble::tibble(
      refcat14 = parcelas$refcat14,
      laea_N   = floor(coords_p[, 2] / 1000) * 1000L,
      laea_E   = floor(coords_p[, 1] / 1000) * 1000L
    ) |>
      dplyr::mutate(
        GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E)
      ) |>
      dplyr::distinct(refcat14, .keep_all = TRUE)
    cat_p <- parse_cat_provincia(ruta_cat_prov)
    if (nrow(cat_p) == 0) {
      warning("Provincia ", prov, ": CAT sin Type 15 residenciales")
      next
    }
    catastro_celdas_lista[[k]] <- cat_p |>
      dplyr::inner_join(parcelas_tab |> dplyr::select(refcat14, GRD_ID),
                        by = "refcat14")
    message("    Provincia ", prov, ": ",
            format(nrow(catastro_celdas_lista[[k]]), big.mark = "."),
            " inmuebles residenciales asignados a celda")
    rm(parcelas, coords_p, parcelas_tab, cat_p)
    gc(verbose = FALSE)
  }
  catastro_celdas <- dplyr::bind_rows(catastro_celdas_lista)
  rm(catastro_celdas_lista)
  message("    Total inmuebles residenciales en grid LAEA: ",
          format(nrow(catastro_celdas), big.mark = "."))

  # Guardar checkpoint 2
  if (USAR_CACHE_CATASTRO) {
    arrow::write_parquet(catastro_celdas, ruta_cache_catastro_cel)
    message("── Checkpoint 2 guardado: ", basename(ruta_cache_catastro_cel),
            " (", format(nrow(catastro_celdas), big.mark = "."), " inmuebles)")
  }
} else if (!catastro_celdas_desde_cache) {
  # Caso: no estaba cacheado y tampoco están los ficheros zip provinciales
  message("  4.1bis Catastro: PENDIENTE (faltan ficheros zip provinciales en ",
          basename(ruta_catastro_dir), ")")
  catastro_celdas <- tibble::tibble(refcat14 = character(0),
                                    uso = character(0),
                                    superficie = numeric(0),
                                    GRD_ID = character(0))
}
# Si catastro_celdas_desde_cache es TRUE, catastro_celdas ya está cargado
# desde el checkpoint y no se entra a ninguna de las dos ramas anteriores.


# ── 4.2 vc_superficie — superficie útil media de las viviendas por celda ────
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3, A4 §D.12): la
# superficie media reducida de las viviendas de la celda indica una provisión
# estructural de espacio habitable empobrecida que opera como condición
# material del entorno residencial con independencia de la ocupación efectiva.
# Operacionalización: media aritmética de la superficie útil de los inmuebles
# residenciales (Type 15 con USO='V' del CAT) cuya parcela cae en cada celda.
if (nrow(catastro_celdas) > 0) {
  message("  4.2 vc_superficie: agregando media de superficie útil por celda ...")
  superficie_cell <- catastro_celdas |>
    dplyr::group_by(GRD_ID) |>
    dplyr::summarise(vc_superficie_new = mean(superficie, na.rm = TRUE),
                     .groups = "drop")
  sip_indic <- sip_indic |>
    dplyr::select(-vc_superficie) |>
    dplyr::left_join(superficie_cell, by = "GRD_ID") |>
    dplyr::rename(vc_superficie = vc_superficie_new)
  n_cubiertas <- sum(!is.na(sip_indic$vc_superficie))
  message("    Celdas con vc_superficie: ", n_cubiertas,
          " (", round(100 * n_cubiertas / nrow(sip_indic), 1), "%)",
          "; media celda = ", round(mean(sip_indic$vc_superficie, na.rm = TRUE), 1),
          " m²; mediana = ", round(median(sip_indic$vc_superficie, na.rm = TRUE), 1),
          " m²")
  rm(superficie_cell)
} else {
  message("  4.2 vc_superficie: PENDIENTE (Catastro no cargado)")
}


# ── 4.3 vc_tenencia — % de viviendas no en propiedad por celda ──────────────
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3, A4 §D.11): la
# alta proporción de hogares en alquiler o en otro régimen distinto de la
# propiedad indica menor estabilidad residencial, menor capacidad de
# inversión en condiciones habitacionales y mayor exposición a perturbaciones
# económicas. Operacionalización: (t20_2 + t20_3) / t19_1 del Censo 2021 INE
# a sección censal proyectado al centroide de cada celda. Decisión de fuente
# (Censo 2021 frente al Censo Anual 2025) documentada en A4 §D.11.
if (file.exists(ruta_tenencia_csv) && file.exists(ruta_secciones_shp)) {
  message("  4.3 vc_tenencia: construyendo desde Censo 2021 a sección censal ...")
  # Las columnas administrativas (ccaa, cpro, cmun, dist, secc) se leen como
  # carácter para preservar el padding de ceros a la izquierda; el resto se
  # parsean por defecto. Forzar a integer con sprintf("%02d", ...) también
  # funcionaría pero la conversión explícita a character es más robusta frente
  # a posibles cambios de tipo en versiones futuras del CSV.
  c21 <- readr::read_csv(
    ruta_tenencia_csv, show_col_types = FALSE,
    col_types = readr::cols(
      ccaa = readr::col_character(),
      cpro = readr::col_character(),
      cmun = readr::col_character(),
      dist = readr::col_character(),
      secc = readr::col_character(),
      .default = readr::col_double()
    ),
    locale = readr::locale(encoding = "UTF-8")
  )
  # CUSEC canónico INE = cpro(2) + cmun(3) + dist(2) + secc(3) = 10 dígitos.
  # No incluye ccaa. Los campos del CSV vienen ya con padding de ceros a la
  # izquierda al haberse forzado a carácter en col_types; el paste0 directo
  # produce el cusec correcto.
  c21 <- c21 |>
    dplyr::mutate(
      cusec = paste0(cpro, cmun, dist, secc),
      vc_tenencia_sec = dplyr::if_else(
        t19_1 > 0, (t20_2 + t20_3) / t19_1, NA_real_
      )
    ) |>
    dplyr::filter(!is.na(vc_tenencia_sec), nchar(cusec) == 10) |>
    dplyr::select(cusec, vc_tenencia_sec)
  secciones <- sf::st_read(ruta_secciones_shp, quiet = TRUE) |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(3035) |>
    dplyr::rename(cusec = ID_SECCION) |>
    dplyr::select(cusec, geometry)
  centroides_celdas <- sip_indic |>
    dplyr::select(GRD_ID, laea_N, laea_E) |>
    dplyr::mutate(x_c = laea_E + 500L, y_c = laea_N + 500L) |>
    sf::st_as_sf(coords = c("x_c", "y_c"), crs = 3035, remove = FALSE)
  asignacion_ten <- sf::st_join(centroides_celdas, secciones,
                                join = sf::st_within) |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::left_join(c21, by = "cusec") |>
    dplyr::select(GRD_ID, vc_tenencia_new = vc_tenencia_sec)
  sip_indic <- sip_indic |>
    dplyr::select(-vc_tenencia) |>
    dplyr::left_join(asignacion_ten, by = "GRD_ID") |>
    dplyr::rename(vc_tenencia = vc_tenencia_new)
  n_cubiertas <- sum(!is.na(sip_indic$vc_tenencia))
  message("    Celdas con vc_tenencia: ", n_cubiertas,
          " (", round(100 * n_cubiertas / nrow(sip_indic), 1), "%)",
          "; media celda = ", round(mean(sip_indic$vc_tenencia, na.rm = TRUE), 3))
  rm(c21, secciones, centroides_celdas, asignacion_ten)
} else {
  message("  4.3 vc_tenencia: PENDIENTE (sin C2021_Indicadores.csv o secciones shp)")
}


# ── 4.4 vc_hacinamiento — superficie útil residencial per cápita por celda ──
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3, A4 §D.10): la
# limitación del espacio habitable por persona amplifica la susceptibilidad
# ante perturbaciones térmicas, epidémicas, de salud mental ante confinamiento
# y de capacidad de evacuación. Operacionalización: suma de las superficies
# útiles de los inmuebles residenciales (Type 15 con USO='V' del CAT) cuya
# parcela cae en cada celda, dividida por el número de residentes SIP en la
# celda. La normalización percentílica posterior invierte la dirección para
# que valores altos del indicador agregado correspondan a baja superficie per
# cápita y, por tanto, a alta vulnerabilidad contextual.
if (nrow(catastro_celdas) > 0) {
  message("  4.4 vc_hacinamiento: agregando superficie residencial / residentes ...")
  hacin_cell <- catastro_celdas |>
    dplyr::group_by(GRD_ID) |>
    dplyr::summarise(superf_res_celda = sum(superficie, na.rm = TRUE),
                     .groups = "drop")
  sip_indic <- sip_indic |>
    dplyr::select(-vc_hacinamiento) |>
    dplyr::left_join(hacin_cell, by = "GRD_ID") |>
    dplyr::mutate(
      vc_hacinamiento = dplyr::if_else(
        n_individuos > 0 & !is.na(superf_res_celda),
        superf_res_celda / n_individuos,
        NA_real_
      )
    ) |>
    dplyr::select(-superf_res_celda)
  n_cubiertas <- sum(!is.na(sip_indic$vc_hacinamiento))
  message("    Celdas con vc_hacinamiento: ", n_cubiertas,
          " (", round(100 * n_cubiertas / nrow(sip_indic), 1), "%)",
          "; mediana = ", round(median(sip_indic$vc_hacinamiento, na.rm = TRUE), 1),
          " m²/persona; máximo = ",
          round(max(sip_indic$vc_hacinamiento, na.rm = TRUE), 1), " m²/persona")
  rm(hacin_cell)
} else {
  message("  4.4 vc_hacinamiento: PENDIENTE (Catastro no cargado)")
}


# ── 4.5 cs_sanitario — distancia-tiempo a equipamientos sanitarios y sociales
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3, A4 §D.13): la
# lejanía a los equipamientos institucionales de primer nivel amplifica el
# daño sobre los individuos con mayor dependencia de esos servicios cuando
# una perturbación interrumpe la accesibilidad habitual. Operacionalización
# V1 (versión mínima sobre red OSM): distancia recta entre el centroide LAEA
# de la celda y el punto más próximo de cada red, corregida por un factor de
# sinuosidad típico de trayectos urbanos y convertida a minutos a velocidad
# de marcha estándar. El indicador final es la media simple del tiempo a
# las dos redes (centros de salud y RECESSO). La sustitución por routing
# completo sobre OSM con catálogos GTFS para transporte público queda como
# agenda V2 documentada en A4 §D.13.
if (file.exists(ruta_acceso_salud) && file.exists(ruta_acceso_sssoc)) {
  message("  4.5 cs_sanitario: cargando redes de equipamientos ...")
  centros_salud <- sf::st_read(ruta_acceso_salud, quiet = TRUE) |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(3035)
  centros_ssoc <- sf::st_read(ruta_acceso_sssoc, quiet = TRUE) |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(3035)
  message("    Centros de salud: ", nrow(centros_salud),
          " | RECESSO: ", nrow(centros_ssoc))
  centroides_celdas <- sip_indic |>
    dplyr::select(GRD_ID, laea_N, laea_E) |>
    dplyr::mutate(x_c = laea_E + 500L, y_c = laea_N + 500L) |>
    sf::st_as_sf(coords = c("x_c", "y_c"), crs = 3035, remove = FALSE)
  idx_salud <- sf::st_nearest_feature(centroides_celdas, centros_salud)
  idx_ssoc  <- sf::st_nearest_feature(centroides_celdas, centros_ssoc)
  d_salud   <- as.numeric(sf::st_distance(centroides_celdas,
                                          centros_salud[idx_salud, ],
                                          by_element = TRUE))
  d_ssoc    <- as.numeric(sf::st_distance(centroides_celdas,
                                          centros_ssoc[idx_ssoc, ],
                                          by_element = TRUE))
  # Conversión: distancia (m) → trayecto sinuoso (m) → tiempo a pie (minutos)
  km_por_min <- VELOCIDAD_PIE_KMH / 60   # km recorridos por minuto
  t_salud <- (d_salud * FACTOR_SINUOSIDAD) / 1000 / km_por_min
  t_ssoc  <- (d_ssoc  * FACTOR_SINUOSIDAD) / 1000 / km_por_min
  t_compuesto <- (t_salud + t_ssoc) / 2
  sip_indic <- sip_indic |>
    dplyr::select(-cs_sanitario) |>
    dplyr::mutate(cs_sanitario = t_compuesto)
  message("    Celdas con cs_sanitario: ",
          sum(!is.na(sip_indic$cs_sanitario)),
          " (", round(100 * mean(!is.na(sip_indic$cs_sanitario)), 1), "%)",
          "; mediana = ",
          round(median(sip_indic$cs_sanitario, na.rm = TRUE), 1), " min;",
          " p90 = ", round(quantile(sip_indic$cs_sanitario, 0.9, na.rm = TRUE), 1),
          " min")
  rm(centros_salud, centros_ssoc, centroides_celdas,
     idx_salud, idx_ssoc, d_salud, d_ssoc, t_salud, t_ssoc, t_compuesto)
} else {
  message("  4.5 cs_sanitario: PENDIENTE (sin 15_ o 19_ gpkg en ",
          basename(ruta_acceso_salud), ")")
}

# ── 4.6 vc_educacion — nivel educativo medio del entorno ─────────────────────
# Fuente: Censo 2021 INE a sección censal. El indicador se construye como
# porcentaje de adultos con "Educación primaria e inferior" sobre el total
# de adultos por sección. La proyección a celda LAEA opera por spatial join
# entre el centroide de la celda y los polígonos de sección, con
# documentación de aproximación (cap. 5 §sec-vccs-fundamento y A4 §D.1).
#
# Cobertura V1: solo provincia 03 Alicante (1.243 secciones). Castellón (12)
# y Valencia (46) reciben NA hasta que se descarguen los CSV equivalentes
# del INE. La limitación territorial queda documentada y la dimensión VC se
# agrega con la cobertura disponible por celda.
#
# PRERREQUISITO: shapefile de secciones censales con código CUSEC de 10
# dígitos coincidente con el formato del CSV. Descargable de INE
# (Cartografía Censo 2021) o CartoCiudad/IGN. Depositar en
# `data/mallas/malla_administrativa/ca_seccion_censal/`.

if (all(file.exists(ruta_educacion_csv)) && file.exists(ruta_secciones_shp)) {
  message("  4.6 vc_educacion: construyendo desde Censo 2021...")

  # 4.6.1 — Lectura y filtrado de los tres CSV provinciales
  # Las tres provincias se publican como ficheros separados en el portal del
  # INE (Censo 2021 a sección censal). Se concatenan antes del filtrado para
  # operar sobre el universo CV completo (~3.515 secciones).
  edu <- purrr::map_dfr(ruta_educacion_csv, ~ readr::read_delim(
    .x, delim = ";", locale = readr::locale(encoding = "latin1"),
    show_col_types = FALSE
  )) |>
    rename(
      sexo      = Sexo,
      provincia = Provincias,
      municipio = Municipios,
      seccion   = Secciones,
      nivel     = `Nivel de formación alcanzado`,
      periodo   = Periodo,
      total     = Total
    ) |>
    filter(
      !is.na(seccion) & seccion != "",
      periodo == ANYO_REF_CENSO,
      sexo == "Total"
    ) |>
    mutate(
      cusec  = stringr::str_extract(seccion, "^[0-9]{10}"),
      total  = as.numeric(stringr::str_replace_all(total, "\\.", ""))
    ) |>
    filter(!is.na(cusec), !is.na(total))

  message("    Filas educación filtradas (CV 2021): ", nrow(edu),
          " | secciones únicas: ", dplyr::n_distinct(edu$cusec))

  # 4.6.2 — Cálculo del % primaria-o-inferior por sección
  vc_edu_sec <- edu |>
    group_by(cusec) |>
    summarise(
      total_adultos = sum(total, na.rm = TRUE),
      primaria_inf  = sum(total[nivel == "Educación primaria e inferior"],
                          na.rm = TRUE),
      pct_primaria  = primaria_inf / total_adultos,
      .groups = "drop"
    ) |>
    filter(total_adultos > 0)

  message("    Secciones con dato válido: ", nrow(vc_edu_sec),
          "; media % primaria = ",
          round(100 * mean(vc_edu_sec$pct_primaria), 2), "%")

  # 4.6.3 — Carga del shapefile de secciones y reproyección a LAEA
  # El shapefile `ca_seccion_censal_20260405.shp` cubre toda la CV (3.515
  # polígonos) en EPSG:25830. La columna `ID_SECCION` contiene el código
  # canónico CUSEC de 10 dígitos, coincidente con los primeros 10 caracteres
  # del campo Seccion del CSV.
  secciones <- st_read(ruta_secciones_shp, quiet = TRUE) |>
    st_zm(drop = TRUE) |>           # elimina componente M/Z si la tuviera
    st_transform(3035) |>
    rename(cusec = ID_SECCION) |>
    select(cusec, geometry)

  # 4.6.4 — Spatial join con los centroides de las celdas activas
  # Construimos el centroide LAEA de cada celda y lo cruzamos con los
  # polígonos de sección. Se asigna a cada celda el % primaria de la sección
  # que contiene su centroide.
  centroides_celdas <- sip_indic |>
    select(GRD_ID, laea_N, laea_E) |>
    mutate(
      x_c = laea_E + 500L,   # centroide LAEA: esquina + 500 m
      y_c = laea_N + 500L
    ) |>
    st_as_sf(coords = c("x_c", "y_c"), crs = 3035, remove = FALSE)

  asignacion_edu <- st_join(centroides_celdas, secciones, join = st_within) |>
    st_drop_geometry() |>
    as_tibble() |>
    left_join(vc_edu_sec |> select(cusec, pct_primaria), by = "cusec") |>
    select(GRD_ID, vc_educacion_new = pct_primaria)

  sip_indic <- sip_indic |>
    select(-vc_educacion) |>
    left_join(asignacion_edu, by = "GRD_ID") |>
    rename(vc_educacion = vc_educacion_new)

  n_cubiertas <- sum(!is.na(sip_indic$vc_educacion))
  message("    Celdas LAEA con vc_educacion asignado: ", n_cubiertas,
          " (", round(100 * n_cubiertas / nrow(sip_indic), 1),
          "% del total activo)")
  rm(edu, vc_edu_sec, secciones, centroides_celdas, asignacion_edu)

} else if (any(file.exists(ruta_educacion_csv)) && !file.exists(ruta_secciones_shp)) {
  message("  4.6 vc_educacion: CSV(s) disponibles pero falta shapefile de secciones")
  message("    Depositar `secciones_censales.gpkg` (INE Censo 2021 / CartoCiudad)")
  message("    en `data/contextual/` con columna CUSEC para activar el indicador.")
} else if (!all(file.exists(ruta_educacion_csv))) {
  faltan <- ruta_educacion_csv[!file.exists(ruta_educacion_csv)]
  message("  4.6 vc_educacion: faltan CSV provinciales: ",
          paste(basename(faltan), collapse = ", "))
} else {
  message("  4.6 vc_educacion: PENDIENTE (sin CSV ni shapefile)")
}

# ── 4.7 cs_asociativo — densidad asociativa ──────────────────────────────────
# DIFERIDO A V2. La geocodificación del Registre d'Associacions GVA contra el
# índice catastral local produjo una tasa de éxito incompatible con la
# cobertura territorial exigida por el indicador (geocodificación catastral
# insuficiente; diferido a V2). El indicador permanece como
# columna NA_real_ en V1; la decisión de mantener una columna NA con
# inicialización explícita preserva la arquitectura de cinco indicadores CS
# en el módulo y facilita la activación futura en V2 sin reorganizar la
# estructura del data.frame de salida.
if (file.exists(ruta_asociaciones)) {
  message("  4.7 cs_asociativo: fichero detectado pero diferido a V2 ",
          "(geocoding insuficiente).")
} else {
  message("  4.7 cs_asociativo: diferido a V2 (no operacionalizado en V1).")
}

# ── 4.8 cs_proximidad — servicios de cuidado cotidiano ───────────────────────
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3): la disponibilidad
# de servicios de cuidado cotidiano de pequeña escala amortigua la fragilidad
# individual cuando los recursos del propio hogar son insuficientes.
# Operacionalización V1 (composición parcial): número de puntos por celda
# combinando dos fuentes con georreferenciación nativa: (1) centros de día
# para mayores filtrados desde el RECESSO de la Conselleria d'Igualtat y
# (2) escuelas infantiles de primer ciclo (0-3 años) filtradas desde el
# Registre de Centres Docents Valencià. La incorporación de las farmacias del
# catálogo oficial de la Conselleria de Sanitat queda como ampliación V2
# (geocodificación catastral pendiente).
if (file.exists(ruta_recesso) && file.exists(ruta_centros_docentes)) {
  message("  4.8 cs_proximidad: cargando RECESSO + centros docentes (filtrado 0-3) ...")
  recesso_raw <- sf::st_read(ruta_recesso, quiet = TRUE) |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(3035)
  # Filtrado del RECESSO para cs_proximidad: estrategia de inclusión amplia
  # basada en el mecanismo causal del indicador (red densa de servicios no
  # residenciales de atención cotidiana que amortiguan la fragilidad
  # individual). Incluye centros de día y atención diurna (mayores +
  # discapacidad + salud mental), centros ocupacionales, atención temprana
  # (CAT/CDIAT), centros de rehabilitación e integración social (CRIS, CEEM,
  # CRAPPS), centros integrales y especializados de mayores (CIMS, CEA),
  # hogares y clubs para mayores, centros sociales y centros sociales de
  # atención primaria, comedores sociales, talleres prelaborales, puntos de
  # encuentro, centros mujer y centros de valoración y orientación. Excluye
  # explícitamente los puntos de alojamiento (residencias, viviendas
  # tuteladas/supervisadas/asistidas, albergues, residencias de recepción de
  # niños), cuyo mecanismo causal es alojamiento permanente o emergencia, no
  # soporte cotidiano externo del entorno. La normalización a Latin-ASCII
  # antes del match evita el problema del acento en "DÍA" del catálogo
  # RECESSO. Decisión documentada en cap. 3 §sec-indicadores-contextual-cap3
  # y A4 §D.contextual; recuento empírico: ~1.000 puntos RECESSO sobre 1.719
  # totales (≈ 58% del catálogo), el resto son alojamiento.
  campo_tipo_recesso <- grep("(?i)tipo|categori|servei|servicio",
                              names(recesso_raw), value = TRUE)[1]
  if (!is.na(campo_tipo_recesso)) {
    # Normalización del campo tipo_centro para neutralizar inconsistencias
    # tipográficas del catálogo RECESSO:
    #   (1) toupper(): los valores ya vienen en mayúsculas sostenidas, pero
    #       garantizamos uniformidad si alguna fila viene en minúscula.
    #   (2) stri_trans_general Latin-ASCII: convierte "DÍA" → "DIA",
    #       "ATENCIÓN" → "ATENCION", "DESARROLLO INFANTIL" → idem, etc.,
    #       homogeneizando con las variantes sin acento que aparecen
    #       intermitentemente en el catálogo administrativo.
    #   (3) gsub("\\s+", " ", trimws(.)): colapsa dobles espacios entre
    #       palabras a uno solo y recorta espacios envolventes, para que
    #       patrones con espacios literales (p.ej., "ATENCION TEMPRANA")
    #       matcheen también filas con espaciado anómalo.
    campo_norm <- stringi::stri_trans_general(
      toupper(recesso_raw[[campo_tipo_recesso]]),
      "Latin-ASCII"
    )
    campo_norm <- gsub("\\s+", " ", trimws(campo_norm))
    patrones_incluir <- paste(
      "\\bDIA\\b", "\\bDIAS\\b",      # CENTRO DE DÍA / CENTROS DE DÍA
      "DIURN",                          # ATENCIÓN DIURNA
      "OCUPACIONAL",                    # centros ocupacionales discapacidad
      "ATENCION TEMPRANA",              # CAT
      "DESARROLLO INFANTIL",            # CDIAT
      "REHABILITACION",                 # CRIS, CRAPPS
      "AUTONOMIA PERSONAL",             # CRAPPS
      "CEEM", "CRIS", "CRAPPS", "CIMS",
      "ENVEJECIMIENTO ACTIVO",
      "HOGAR.*MAYOR", "CLUB.*MAYOR", "CLUBS DE CONVIVENCIA",
      "ESPECIALIZADOS DE ATENCION", "ESPECIALIZADO DE ATENCION",
      "INTEGRALES DE MAYORES",
      "CENTRO SOCIAL", "CENTROS SOCIALES",
      "COMEDOR",
      "TALLER PRELABORAL", "INSERCION SOCIAL",
      "ATENCION TEMPORAL", "EMERGENCIAS SOCIALES",
      "PUNTO DE ENCUENTRO",
      "CENTRO MUJER", "MUJER.*HORAS",
      "VALORACION Y ORIENTACION", "CENTRO BASE",
      "ACOGIDA",
      sep = "|"
    )
    patrones_excluir <- paste(
      "RESIDENCIA", "RESIDENCIAL", "RESIDENCIAS",
      "VIVIENDA", "VIVIENDAS",
      "ALBERGUE", "ALBERGUES",
      "RECEPCION DE NIN",   # RECEPCIÓN DE NIÑOS, NIÑAS Y ADOLESCENTES
      sep = "|"
    )
    match_incluir <- grepl(patrones_incluir, campo_norm, perl = TRUE)
    match_excluir <- grepl(patrones_excluir, campo_norm, perl = TRUE)
    centros_dia <- recesso_raw[match_incluir & !match_excluir, ]
    message("    RECESSO total: ", nrow(recesso_raw),
            " | incluidos por mecanismo causal: ", sum(match_incluir & !match_excluir),
            " (", round(100 * sum(match_incluir & !match_excluir) / nrow(recesso_raw), 1), "%)")
    # Distribución por sector dentro del subconjunto retenido (auditoría)
    if ("sector" %in% names(recesso_raw)) {
      reparto <- as.data.frame(
        table(recesso_raw[match_incluir & !match_excluir, ][["sector"]]),
        stringsAsFactors = FALSE
      )
      names(reparto) <- c("sector", "n")
      for (i in seq_len(nrow(reparto))) {
        message("      ", reparto$sector[i], ": ", reparto$n[i])
      }
    }
  } else {
    # Fallback: usar todo el RECESSO (sobre-inclusivo)
    centros_dia <- recesso_raw
    message("    Aviso: campo de tipo no detectado en RECESSO; ",
            "se usa todo el catálogo.")
  }
  message("    Puntos RECESSO retenidos para cs_proximidad: ", nrow(centros_dia))
  # Escuelas infantiles de primer ciclo (0-3 años) desde centros docentes.
  # Lectura con read_delim explicitando decimal_mark = "." y delim = ";": el
  # CSV oficial GVA tiene separador de campos ';' pero las coordenadas usan
  # punto como separador decimal (no coma); read_csv2 asume el formato
  # decimal español (coma) y parsea las longitudes/latitudes como enteros
  # gigantes (separador de miles), tirando los puntos a coordenadas absurdas
  # fuera de CV tras la reproyección a LAEA.
  docentes_raw <- readr::read_delim(
    ruta_centros_docentes,
    delim = ";",
    locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ""),
    show_col_types = FALSE
  ) |>
    dplyr::mutate(
      longitud = suppressWarnings(as.numeric(longitud)),
      latitud  = suppressWarnings(as.numeric(latitud))
    ) |>
    dplyr::filter(!is.na(longitud), !is.na(latitud))
  # Diagnóstico de rango para verificar la lectura: los centros docentes CV
  # deberían tener longitud en torno a [-1.5, 0.6] y latitud en torno a
  # [37.8, 40.8] (grados decimales WGS84). Si los rangos están fuera de
  # estos intervalos, el CSV se ha parseado mal y los puntos no intersectarán
  # con el grid LAEA de la CV.
  message("    Rango longitud (centros docentes): [",
          round(min(docentes_raw$longitud, na.rm = TRUE), 4), ", ",
          round(max(docentes_raw$longitud, na.rm = TRUE), 4), "]")
  message("    Rango latitud (centros docentes):  [",
          round(min(docentes_raw$latitud, na.rm = TRUE), 4), ", ",
          round(max(docentes_raw$latitud, na.rm = TRUE), 4), "]")
  # Patrón: "INFANTIL" sin "PRIMARIA". Excluye centros 3+ que llevan
  # INFANTIL Y PRIMARIA, INFANTIL PRIMARIA O SECUNDARIA, etc.
  infantil_03 <- docentes_raw[
    grepl("(?i)infantil", docentes_raw$denominacion_generica_es) &
    !grepl("(?i)primari", docentes_raw$denominacion_generica_es), ]
  message("    Escuelas infantiles 0-3: ", nrow(infantil_03))
  infantil_03_sf <- sf::st_as_sf(infantil_03,
                                 coords = c("longitud", "latitud"),
                                 crs = 4326) |>
    sf::st_transform(3035)
  # Construir polígonos de celda desde laea_N/laea_E (esquina inferior-izquierda
  # del km²) para st_intersects. La columna activa se llama "geometry" para
  # garantizar compatibilidad con sf::st_intersects sin necesidad de
  # sf_column_name explícito (sintaxis menos frágil que la versión anterior).
  grid_celdas <- sf::st_sf(
    sip_indic |> dplyr::select(GRD_ID, laea_N, laea_E),
    geometry = sf::st_sfc(
      lapply(seq_len(nrow(sip_indic)), function(i) {
        N <- sip_indic$laea_N[i]; E <- sip_indic$laea_E[i]
        sf::st_polygon(list(matrix(c(
          E,       N,
          E+1000L, N,
          E+1000L, N+1000L,
          E,       N+1000L,
          E,       N
        ), ncol = 2, byrow = TRUE)))
      }),
      crs = 3035
    )
  )
  # Conteo separado por fuente: evita problemas de bind_rows entre sf de
  # estructuras heterogéneas (algunos GPKG mezclan POINT y POLYGON, otros
  # tienen CRS NA tras la lectura, etc.). Cada fuente se interseca de forma
  # independiente y se suman los conteos por celda.
  conteo_recesso <- lengths(sf::st_intersects(grid_celdas, centros_dia))
  conteo_infantil <- lengths(sf::st_intersects(grid_celdas, infantil_03_sf))
  conteo_prox <- conteo_recesso + conteo_infantil
  sip_indic <- sip_indic |>
    dplyr::select(-cs_proximidad) |>
    dplyr::mutate(cs_proximidad = conteo_prox)
  message("    Celdas con al menos un punto de proximidad: ",
          sum(sip_indic$cs_proximidad > 0),
          " (", round(100 * mean(sip_indic$cs_proximidad > 0), 1), "%)",
          "; reparto: RECESSO ", sum(conteo_recesso > 0),
          " celdas / infantil 0-3 ", sum(conteo_infantil > 0), " celdas")
  rm(recesso_raw, centros_dia, docentes_raw, infantil_03, infantil_03_sf,
     conteo_recesso, conteo_infantil, conteo_prox)
} else {
  message("  4.8 cs_proximidad: PENDIENTE (faltan ", basename(ruta_recesso),
          " o ", basename(ruta_centros_docentes), ")")
}

# ── 4.9 cs_transporte — accesibilidad al transporte público ─────────────────
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3): la accesibilidad
# al transporte público es un recurso colectivo que compensa las limitaciones
# de movilidad individual. Operacionalización V1: tiempo a pie desde el
# centroide LAEA de la celda al punto más próximo de la red de paradas
# extraída de OpenStreetMap (10.507 puntos: bus_stop, tram_stop, station,
# halt, subway_entrance), aplicando el mismo factor de sinuosidad y la misma
# velocidad peatonal estándar que el indicador cs_sanitario para
# garantizar comparabilidad metodológica entre los dos indicadores de
# accesibilidad institucional de la dimensión CS.
if (file.exists(ruta_transporte_osm)) {
  message("  4.9 cs_transporte: cargando red OSM de transporte público ...")
  paradas_tp <- sf::st_read(ruta_transporte_osm, quiet = TRUE) |>
    sf::st_zm(drop = TRUE) |>
    sf::st_transform(3035)
  message("    Paradas cargadas: ", nrow(paradas_tp))
  centroides_celdas <- sip_indic |>
    dplyr::select(GRD_ID, laea_N, laea_E) |>
    dplyr::mutate(x_c = laea_E + 500L, y_c = laea_N + 500L) |>
    sf::st_as_sf(coords = c("x_c", "y_c"), crs = 3035, remove = FALSE)
  idx_tp  <- sf::st_nearest_feature(centroides_celdas, paradas_tp)
  d_tp    <- as.numeric(sf::st_distance(centroides_celdas,
                                         paradas_tp[idx_tp, ],
                                         by_element = TRUE))
  km_por_min <- VELOCIDAD_PIE_KMH / 60
  t_tp <- (d_tp * FACTOR_SINUOSIDAD) / 1000 / km_por_min
  sip_indic <- sip_indic |>
    dplyr::select(-cs_transporte) |>
    dplyr::mutate(cs_transporte = t_tp)
  message("    Celdas con cs_transporte: ",
          sum(!is.na(sip_indic$cs_transporte)),
          " (", round(100 * mean(!is.na(sip_indic$cs_transporte)), 1), "%)",
          "; mediana = ",
          round(median(sip_indic$cs_transporte, na.rm = TRUE), 1), " min;",
          " p90 = ", round(quantile(sip_indic$cs_transporte, 0.9, na.rm = TRUE), 1),
          " min")
  rm(paradas_tp, centroides_celdas, idx_tp, d_tp, t_tp)
} else {
  message("  4.9 cs_transporte: PENDIENTE (sin ",
          basename(ruta_transporte_osm), ")")
}

# ── 4.10 cs_nodos_civicos — densidad de nodos cívicos públicos del entorno ──
# Hipótesis causal (cap. 3 §sec-indicadores-contextual-cap3): la presencia de
# nodos cívicos públicos en el entorno produce simultáneamente socialización
# intergeneracional, capilaridad institucional de la administración pública
# territorial hacia el barrio y observación comunitaria del bienestar.
# Operacionalización V1 (composición parcial, Solución B): número de puntos
# por celda combinando tres fuentes con georreferenciación nativa: (1) centros
# docentes públicos 3+ (infantil 2.º ciclo + primaria + secundaria) filtrados
# desde el Registre de Centres Docents Valencià, (2) GeoJSONs de equipamientos
# culturales de la Diputació de Castelló + Diputación de Alicante + Ajuntament
# de València + OSM (amenity=community_centre|arts_centre|social_centre)
# unificados y deduplicados por proximidad espacial en el script
# 17_descarga_contextual_cs.R. La incorporación de la red de bibliotecas
# públicas del Directorio MCU queda como ampliación V2
# (geocodificación catastral pendiente).
if (file.exists(ruta_civicos) && file.exists(ruta_centros_docentes)) {
  message("  4.10 cs_nodos_civicos: cargando centros docentes 3+ + cívicos ...")
  # Centros docentes públicos 3+ (régimen público + sin "INFANTIL solo").
  # Lectura con read_delim explicitando decimal_mark = "." (mismo fix que el
  # bloque 4.8): el CSV oficial GVA tiene separador de campos ';' pero las
  # coordenadas usan punto como separador decimal, contrario a la suposición
  # de read_csv2.
  if (!exists("docentes_raw")) {
    docentes_raw <- readr::read_delim(
      ruta_centros_docentes,
      delim = ";",
      locale = readr::locale(encoding = "UTF-8", decimal_mark = ".", grouping_mark = ""),
      show_col_types = FALSE
    ) |>
      dplyr::mutate(
        longitud = suppressWarnings(as.numeric(longitud)),
        latitud  = suppressWarnings(as.numeric(latitud))
      ) |>
      dplyr::filter(!is.na(longitud), !is.na(latitud))
    message("    Rango longitud (centros docentes): [",
            round(min(docentes_raw$longitud, na.rm = TRUE), 4), ", ",
            round(max(docentes_raw$longitud, na.rm = TRUE), 4), "]")
    message("    Rango latitud (centros docentes):  [",
            round(min(docentes_raw$latitud, na.rm = TRUE), 4), ", ",
            round(max(docentes_raw$latitud, na.rm = TRUE), 4), "]")
  }
  # Filtro: régimen público + denominación distinta de "INFANTIL puro" (0-3
  # ya se usó en cs_proximidad). Aceptamos los centros con denominación que
  # contenga "PRIMARIA", "SECUNDARIA", "BACHILLER", "FORMACION PROFESIONAL"
  # o "EDUCACION INFANTIL Y PRIMARIA" (centros mixtos 3-12).
  docentes_publ_3mas <- docentes_raw[
    grepl("(?i)^PÚB", docentes_raw$regimen) &
    grepl("(?i)primari|secundari|bachiller|formacion.?profes|^infantil.?y",
          docentes_raw$denominacion_generica_es), ]
  message("    Centros docentes públicos 3+: ", nrow(docentes_publ_3mas))
  docentes_sf <- sf::st_as_sf(docentes_publ_3mas,
                              coords = c("longitud", "latitud"),
                              crs = 4326) |>
    sf::st_transform(3035)
  # Cívicos consolidados (DipCas + DipAli + VLC + OSM, deduplicados). Defensa
  # de CRS: si el GPKG fue escrito sin CRS asignado (caso raro pero posible
  # tras bind_rows de capas heterogéneas en el script de descarga), forzar
  # EPSG:3035; si trae CRS, reproyectar a 3035 para asegurar coincidencia con
  # el grid. Defensa de tipo geométrico: si alguna fuente del consolidado
  # aportó polígonos (algunos catálogos cívicos publican edificios, no
  # puntos), se centroidiza para mantener la consistencia con el resto del
  # módulo (un nodo cívico = un punto en una celda).
  civicos_raw <- sf::st_read(ruta_civicos, quiet = TRUE) |>
    sf::st_zm(drop = TRUE)
  if (is.na(sf::st_crs(civicos_raw))) {
    sf::st_crs(civicos_raw) <- 3035
    message("    Aviso: ", basename(ruta_civicos), " sin CRS; asumido EPSG:3035.")
  } else {
    civicos_raw <- sf::st_transform(civicos_raw, 3035)
  }
  tipos_geom <- as.character(sf::st_geometry_type(civicos_raw))
  if (any(tipos_geom %in% c("POLYGON", "MULTIPOLYGON"))) {
    n_poly <- sum(tipos_geom %in% c("POLYGON", "MULTIPOLYGON"))
    message("    Aviso: ", n_poly, " polígonos en cívicos; centroidizados.")
    civicos_raw <- suppressWarnings(sf::st_centroid(civicos_raw))
  }
  message("    Centros cívicos consolidados: ", nrow(civicos_raw))
  # Reconstrucción del grid si no existe (el bloque 4.8 lo deja en memoria
  # para reutilizar; si por la razón que sea no está, se construye igual).
  if (!exists("grid_celdas")) {
    grid_celdas <- sf::st_sf(
      sip_indic |> dplyr::select(GRD_ID, laea_N, laea_E),
      geometry = sf::st_sfc(
        lapply(seq_len(nrow(sip_indic)), function(i) {
          N <- sip_indic$laea_N[i]; E <- sip_indic$laea_E[i]
          sf::st_polygon(list(matrix(c(
            E,       N,
            E+1000L, N,
            E+1000L, N+1000L,
            E,       N+1000L,
            E,       N
          ), ncol = 2, byrow = TRUE)))
        }),
        crs = 3035
      )
    )
  }
  # Conteo separado por fuente (mismo patrón que 4.8 para robustez frente a
  # heterogeneidad entre fuentes y evitar dependencia de bind_rows sobre sf).
  conteo_docentes <- lengths(sf::st_intersects(grid_celdas, docentes_sf))
  conteo_civicos_pts <- lengths(sf::st_intersects(grid_celdas, civicos_raw))
  conteo_total <- conteo_docentes + conteo_civicos_pts
  sip_indic <- sip_indic |>
    dplyr::select(-cs_nodos_civicos) |>
    dplyr::mutate(cs_nodos_civicos = conteo_total)
  message("    Celdas con al menos un nodo cívico: ",
          sum(sip_indic$cs_nodos_civicos > 0),
          " (", round(100 * mean(sip_indic$cs_nodos_civicos > 0), 1), "%)",
          "; reparto: docentes 3+ ", sum(conteo_docentes > 0),
          " celdas / cívicos consolidados ", sum(conteo_civicos_pts > 0), " celdas")
  rm(docentes_publ_3mas, docentes_sf, civicos_raw,
     conteo_docentes, conteo_civicos_pts, conteo_total)
  if (exists("docentes_raw")) rm(docentes_raw)
  if (exists("grid_celdas"))  rm(grid_celdas)
} else {
  message("  4.10 cs_nodos_civicos: PENDIENTE (faltan ",
          basename(ruta_civicos), " o ", basename(ruta_centros_docentes), ")")
}


# =============================================================================
# SECCIÓN 5 — UNIVERSO DE CELDAS ACTIVAS Y UMBRAL MÍNIMO
# =============================================================================

message("\n── Sección 5: umbral mínimo de población por celda ─────────────")

n_antes <- nrow(sip_indic)
sip_indic <- sip_indic |> filter(n_individuos >= UMBRAL_CELDA)
n_excluidas <- n_antes - nrow(sip_indic)
message("  Celdas con < ", UMBRAL_CELDA, " individuos excluidas: ",
        format(n_excluidas, big.mark = "."),
        " (", round(100 * n_excluidas / n_antes, 1), "%)")
message("  Celdas activas tras umbral: ", format(nrow(sip_indic), big.mark = "."))


# =============================================================================
# SECCIÓN 6 — NORMALIZACIÓN PERCENTÍLICA DE CADA INDICADOR
# =============================================================================
# Función prank: percentil rank midpoint, en [0, 1]. Cada indicador se
# rankea sobre el conjunto de celdas activas con valor válido.

message("\n── Sección 6: normalización percentílica ───────────────────────")

prank <- function(x) {
  n  <- sum(!is.na(x))
  rx <- rank(x, ties.method = "average", na.last = "keep")
  (rx - 0.5) / n
}

# Indicadores teóricos del módulo, organizados por sub-dimensión.
# Los que no tienen dato disponible (NA) se excluyen del rank y de la
# agregación dimensional automáticamente.
indicadores_vc_fisico <- c("vc_antiguedad", "vc_superficie", "vc_tenencia",
                           "vc_hacinamiento")
indicadores_vc_demo   <- c("vc_envejecimiento", "vc_inactividad",
                           "vc_estr_migratoria", "vc_educacion",
                           "vc_dependencia_conv")
indicadores_vc        <- c(indicadores_vc_fisico, indicadores_vc_demo)
indicadores_cs        <- c("cs_asociativo", "cs_proximidad", "cs_transporte",
                           "cs_sanitario", "cs_nodos_civicos")

# Inicialización de slots CS pendientes (NA en V1; se activan al disponer de
# fuente). Tras la activación de los tres indicadores adicionales de mayo de
# 2026 (cs_transporte sobre OSM, cs_proximidad y cs_nodos_civicos en
# composición parcial), cuatro de los cinco slots CS quedan poblados por
# los bloques 4.5, 4.8, 4.9 y 4.10. Solo cs_asociativo sigue como NA en V1
# (diferido a V2 por baja tasa de geocoding contra el Catastro local).
slots_cs_pendientes <- c("cs_asociativo")
for (col in slots_cs_pendientes) {
  if (!col %in% names(sip_indic)) sip_indic[[col]] <- NA_real_
}

for (ind in c(indicadores_vc, indicadores_cs)) {
  if (all(is.na(sip_indic[[ind]]))) {
    sip_indic[[paste0(ind, "_p")]] <- NA_real_
    next
  }
  x <- sip_indic[[ind]]
  # Los indicadores listados en INDICADORES_INVERSOS se rankean sobre el
  # opuesto del valor, de modo que el percentil mantenga la convención de su
  # dimensión: alto percentil = alta VC en los indicadores de vulnerabilidad
  # y alta capacidad CS en los indicadores de tiempo de desplazamiento.
  if (ind %in% INDICADORES_INVERSOS) x <- -x
  sip_indic[[paste0(ind, "_p")]] <- prank(x)
}


# =============================================================================
# SECCIÓN 7 — AGREGACIÓN DIMENSIONAL VC y CS
# =============================================================================
# Cada dimensión se agrega como media aritmética de los indicadores percentílicos
# con valor disponible (los NA se ignoran). La cobertura por celda se reporta
# como número de indicadores no-NA contribuyentes.

message("\n── Sección 7: agregación dimensional ───────────────────────────")

cols_vc_p <- paste0(indicadores_vc, "_p")
cols_cs_p <- paste0(indicadores_cs, "_p")

sip_indic <- sip_indic |>
  rowwise() |>
  mutate(
    n_indic_vc = sum(!is.na(c_across(all_of(cols_vc_p)))),
    n_indic_cs = sum(!is.na(c_across(all_of(cols_cs_p)))),
    VC_celda   = mean(c_across(all_of(cols_vc_p)), na.rm = TRUE),
    CS_celda   = mean(c_across(all_of(cols_cs_p)), na.rm = TRUE)
  ) |>
  ungroup()

message("  Cobertura VC (n indicadores con dato): media = ",
        round(mean(sip_indic$n_indic_vc, na.rm = TRUE), 2),
        " / ", length(cols_vc_p))
message("  Cobertura CS (n indicadores con dato): media = ",
        round(mean(sip_indic$n_indic_cs, na.rm = TRUE), 2),
        " / ", length(cols_cs_p))


# =============================================================================
# SECCIÓN 8 — NORMALIZACIÓN MIN-MAX Y FÓRMULA IVEC_CONTEXTUAL (V1)
# =============================================================================
# Fórmula canónica del módulo (cap. 5 §sec-vccs-agregacion):
#     IVEC_contextual_i = ½ · VC_i* + ½ · (1 − CS_i*)
#
# Tras la reasignación dimensional de mayo de 2026 (vc_acceso_eq →
# cs_sanitario), la dimensión CS opera en V1 con un único
# indicador activo y la fórmula compensadora completa entra en vigor sin
# suspensión. Los cuatro indicadores CS restantes se irán activando con la
# consolidación de las fuentes correspondientes.

message("\n── Sección 8: min-max dimensional y fórmula IVEC_contextual ───")

minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

cs_calculable <- any(!is.na(sip_indic$CS_celda))

sip_indic <- sip_indic |>
  mutate(
    VC_star = minmax(VC_celda),
    CS_star = if (cs_calculable) minmax(CS_celda) else NA_real_,
    ivec_contextual = if (cs_calculable) {
      0.5 * VC_star + 0.5 * (1 - CS_star)
    } else {
      VC_star
    }
  )

message("  VC*: media = ", round(mean(sip_indic$VC_star, na.rm = TRUE), 3),
        " ; dt = ", round(sd(sip_indic$VC_star, na.rm = TRUE), 3))
if (cs_calculable) {
  message("  CS*: media = ", round(mean(sip_indic$CS_star, na.rm = TRUE), 3),
          " ; dt = ", round(sd(sip_indic$CS_star, na.rm = TRUE), 3))
  message("  IVEC_contextual (½·VC* + ½·(1-CS*)): media = ",
          round(mean(sip_indic$ivec_contextual, na.rm = TRUE), 3),
          " ; mediana = ", round(median(sip_indic$ivec_contextual, na.rm = TRUE), 3))
} else {
  message("  CS*: NO COMPUTABLE (cero indicadores CS activos; situación anómala tras la reasignación)")
  message("  ivec_contextual = VC_star (fallback de seguridad): media = ",
          round(mean(sip_indic$ivec_contextual, na.rm = TRUE), 3),
          " ; mediana = ", round(median(sip_indic$ivec_contextual, na.rm = TRUE), 3))
}


# =============================================================================
# SECCIÓN 9 — COHERENCIA INTERNA (PARCIAL, V1)
# =============================================================================
# α de Cronbach y ω de McDonald por dimensión, sobre las celdas con cobertura
# completa de los indicadores ACTIVOS en V1. Los valores son provisionales y
# tendrán que recalcularse cuando se incorporen los indicadores pendientes.

message("\n── Sección 9: coherencia interna por sub-dimensión ────────────")

# La coherencia interna se reporta dentro de cada sub-dimensión, no para VC
# globalmente, porque los dos mecanismos causales (físico-residencial y
# demográfico-poblacional) no comparten estructura factorial reflectiva.
# El α global del módulo VC con indicadores mezclados de las dos sub-dim
# carece de interpretación válida.

# Lista canónica de indicadores por sub-dimensión. La detección de cuáles
# están efectivamente activos en esta ejecución se hace inspeccionando la
# columna correspondiente de sip_indic: si tiene al menos una celda no-NA,
# se considera activo y entra al cómputo de α/ω de su sub-dimensión.
cols_demo_canon   <- c("vc_envejecimiento", "vc_inactividad",
                       "vc_estr_migratoria", "vc_educacion",
                       "vc_dependencia_conv")
cols_fisico_canon <- c("vc_antiguedad", "vc_superficie", "vc_tenencia",
                       "vc_hacinamiento")
cols_cs_canon     <- c("cs_asociativo", "cs_proximidad", "cs_transporte",
                       "cs_sanitario", "cs_nodos_civicos")

calc_alpha_omega <- function(cols, etiqueta) {
  activos <- cols[vapply(cols,
                         function(x) any(!is.na(sip_indic[[x]])),
                         logical(1))]
  if (length(activos) < 2) {
    message("  ", etiqueta, " (", length(activos),
            " indicador activo en pipeline): α/ω no calculable con menos de dos ",
            "indicadores.")
    return(list(alpha = NA_real_, omega = NA_real_, n_activos = length(activos),
                n = NA_integer_, activos = activos))
  }
  # Construir la matriz invirtiendo el signo de los indicadores cuya
  # dirección natural es inversa a la dimensión (INDICADORES_INVERSOS) para
  # que todos midan en el mismo sentido del constructo. Sin esta inversión,
  # α de Cronbach sería negativo o nulo y ω quedaría sesgado a la baja, no
  # porque la sub-dimensión carezca de coherencia reflectiva sino porque la
  # matriz de covarianzas tendría signos mezclados.
  mat_tbl <- sip_indic |>
    dplyr::select(dplyr::all_of(activos))
  inversos_presentes <- intersect(activos, INDICADORES_INVERSOS)
  for (col in inversos_presentes) {
    mat_tbl[[col]] <- -mat_tbl[[col]]
  }
  mat <- mat_tbl |>
    dplyr::filter(dplyr::if_all(dplyr::everything(), ~ !is.na(.))) |>
    as.matrix()
  alpha_val <- tryCatch(
    psych::alpha(mat, check.keys = FALSE)$total$raw_alpha,
    error = function(e) NA_real_
  )
  omega_val <- tryCatch(
    psych::omega(mat, nfactors = 1, plot = FALSE)$omega.tot,
    error = function(e) NA_real_
  )
  message("  ", etiqueta, " (", length(activos),
          " indicadores activos, N = ", nrow(mat), "): α = ",
          round(alpha_val, 3), " ; ω = ", round(omega_val, 3))
  list(alpha = alpha_val, omega = omega_val, n_activos = length(activos),
       n = nrow(mat), activos = activos)
}

coh_demo   <- calc_alpha_omega(cols_demo_canon,   "VC demográfico-poblacional")
coh_fisico <- calc_alpha_omega(cols_fisico_canon, "VC físico-residencial")
coh_cs     <- calc_alpha_omega(cols_cs_canon,     "CS")

# Variables expuestas al bloque de guardado de validación
alpha_demo   <- coh_demo$alpha;   omega_demo   <- coh_demo$omega
alpha_fisico <- coh_fisico$alpha; omega_fisico <- coh_fisico$omega
alpha_cs     <- coh_cs$alpha;     omega_cs     <- coh_cs$omega


# =============================================================================
# SECCIÓN 10 — GUARDAR RESULTADOS
# =============================================================================

message("\n── Sección 10: guardando resultados ────────────────────────────")

# Guardar el dataset completo de indicadores por celda
write_parquet(sip_indic, file.path(dir_out, "ivec_contextual.parquet"))
message("  Guardado: ivec_contextual.parquet (", nrow(sip_indic),
        " celdas × ", ncol(sip_indic), " columnas)")

# Tabla operativa V1 de indicadores con su estado (activo / pendiente) y
# fuente. Contiene 14 filas (4 VC fís.-res. + 5 VC demogr. + 5 CS) de las
# cuales 13 son activos en V1 y 1 reservado para V2 (cs_asociativo). La
# arquitectura ideal documentada en cap. 3 §sec-indicadores-contextual-cap3
# contempla 15 indicadores teóricos justificados por mecanismo causal, abierta
# a ampliación; las dos cifras son distintas por construcción y no deben
# confundirse: 13 es la cifra canónica para metodología y resultados V1, 15
# es la cifra de horizonte ideal del cap. 3.
tabla_indicadores <- tibble::tibble(
  Dimension  = c(rep("VC físico-residencial", 4),
                 rep("VC demográfico-poblacional", 5),
                 rep("CS", 5)),
  Indicador  = c("vc_antiguedad", "vc_superficie", "vc_tenencia",
                 "vc_hacinamiento",
                 "vc_envejecimiento", "vc_inactividad",
                 "vc_estr_migratoria", "vc_educacion",
                 "vc_dependencia_conv",
                 "cs_asociativo", "cs_proximidad", "cs_transporte",
                 "cs_sanitario", "cs_nodos_civicos"),
  Fuente     = c("Observatorio Hábitat GVA (0801_GESIEE_antiguedad.gpkg)",
                 "Catastro CAT Type 15 USO=V (media superficie útil)",
                 "Censo 2021 INE (t20_2+t20_3 / t19_1, proyección seccional)",
                 "Catastro CAT Type 15 USO=V × SIP (m² útil / residentes)",
                 "SIP (f_nacim, % > 65)",
                 "SIP (D4_lab, % inactivos)",
                 "SIP (D3_migr, índice compuesto)",
                 "Censo Anual de Población 2025 INE (proyección seccional)",
                 "SIP (UCO_CODI × edad × D4_lab)",
                 "Registre Associacions GVA (DIFERIDO V2)",
                 "RECESSO (centros de día) + centros docentes infantil 0-3 (farmacias PENDIENTE)",
                 "Paradas OSM bus + ferrocarril + tranvía + metro (isócrono peatonal)",
                 "Centros de Salud GVA (15_) + RECESSO GVA (19_)",
                 "Centros docentes públicos 3+ + cívicos consolidados (bibliotecas PENDIENTE)")
) |>
  dplyr::mutate(
    Estado_V1 = dplyr::case_when(
      Indicador %in% c("vc_antiguedad", "vc_envejecimiento", "vc_inactividad",
                        "vc_estr_migratoria", "vc_educacion",
                        "vc_dependencia_conv") ~ "ACTIVO",
      Indicador %in% c("vc_superficie", "vc_tenencia", "vc_hacinamiento",
                        "cs_sanitario", "cs_proximidad",
                        "cs_transporte", "cs_nodos_civicos") ~ dplyr::if_else(
        vapply(Indicador, function(x) any(!is.na(sip_indic[[x]])), logical(1)),
        "ACTIVO", "PENDIENTE"
      ),
      TRUE ~ "PENDIENTE"
    )
  ) |>
  dplyr::select(Dimension, Indicador, Estado_V1, Fuente)
saveRDS(tabla_indicadores,
        file.path(dir_out, "tabla_indicadores_contextual.rds"))
message("  Guardado: tabla_indicadores_contextual.rds")

# Validación de la ejecución
validacion_contextual <- list(
  fecha_ejecucion              = Sys.time(),
  n_celdas_activas             = nrow(sip_indic),
  n_indicadores_arquitectura   = nrow(tabla_indicadores),
  n_indicadores_activos_v1     = sum(tabla_indicadores$Estado_V1 == "ACTIVO"),
  n_indicadores_pendientes_v1  = sum(tabla_indicadores$Estado_V1 == "PENDIENTE"),
  alpha_demo_v1                = alpha_demo,
  omega_demo_v1                = omega_demo,
  alpha_fisico_v1              = alpha_fisico,
  omega_fisico_v1              = omega_fisico,
  alpha_cs_v1                  = alpha_cs,
  omega_cs_v1                  = omega_cs,
  n_activos_demo               = coh_demo$n_activos,
  n_activos_fisico             = coh_fisico$n_activos,
  n_activos_cs                 = coh_cs$n_activos,
  cobertura_vc_media           = mean(sip_indic$n_indic_vc, na.rm = TRUE),
  cobertura_cs_media           = mean(sip_indic$n_indic_cs, na.rm = TRUE),
  cs_activable                 = cs_calculable,
  formula_aplicada             = if (cs_calculable) {
    "IVEC_contextual = 1/2 * VC_star + 1/2 * (1 - CS_star)"
  } else {
    "IVEC_contextual = VC_star (fallback de seguridad; CS sin indicadores activos)"
  },
  nota = paste("V1 (mayo 2026): 13 indicadores activos en pipeline sobre tabla operativa de 14",
               "(VC fisico-residencial 4/4 + VC demografico-poblacional 5/5 + CS 4/5).",
               "CS opera con cs_sanitario, cs_transporte, cs_proximidad parcial y",
               "cs_nodos_civicos parcial; cs_asociativo reservado como slot pendiente para V2",
               "(diferido por baja tasa de geocoding catastral del Registre Associacions GVA).",
               "Componentes pendientes dentro de indicadores activos: farmacias en cs_proximidad",
               "y bibliotecas en cs_nodos_civicos. Formula compensadora completa en vigor con CS",
               "construido como media equiponderada de los cuatro indicadores CS activos.",
               "Arquitectura ideal del cap. 3: 15 indicadores teoricos (abierta a ampliacion).")
)
saveRDS(validacion_contextual,
        file.path(dir_out, "validacion_contextual.rds"))
message("  Guardado: validacion_contextual.rds")

# =============================================================================
# DATASETS DE VALIDACIÓN CRUZADA (NO se incorporan al índice)
# =============================================================================
# Los siguientes ficheros INE están disponibles en `data/contextual/` pero NO
# entran como indicadores del IVEC_contextual porque sus equivalentes ya se
# construyen desde el SIP a celda nativa (mejor resolución, sin proyección).
# Su utilidad metodológica es validación convergente entre el indicador SIP
# y el dato censal a sección, para argumentos de validez en el Apéndice D y
# en la sección §sec-res-h1 del cap. 6.
#
# - data/contextual/Actividad.csv
#     INE 2021-2024 por sección × actividad. Validación cruzada de
#     `vc_inactividad` (SIP D4_lab). Permite estimar el ρ de Spearman entre
#     el % de inactivos del SIP a celda y el % censal a sección.
#
# - data/contextual/Pais_nacimiento.csv
#     INE 2021-2024 por sección × educación × país (binario España/Extranjero).
#     Validación cruzada parcial de `vc_estr_migratoria` (SIP D3_migr), con
#     la limitación de que la fuente solo distingue binariamente y el SIP
#     desglosa por trayectoria y antigüedad.
#
# - data/contextual/Pais_nacimiento_actividad.csv
#     ECP 2024 por sección × país × actividad. Documenta que la ECP es la
#     fuente que sustituiría al Padrón en versiones futuras del módulo
#     y permite cruces complementarios para análisis exploratorio.
#
# Cobertura territorial: los cuatro ficheros cubren solo provincia 03
# Alicante. La extensión a Castellón (12) y Valencia (46) requiere descarga
# adicional del INE.
# =============================================================================

if (file.exists(ruta_actividad_csv)) {
  message("\nDataset de validación cruzada disponible: Actividad.csv")
}
if (file.exists(ruta_pais_csv)) {
  message("Dataset de validación cruzada disponible: Pais_nacimiento.csv")
}
if (file.exists(ruta_pais_act_csv)) {
  message("Dataset de validación cruzada disponible: Pais_nacimiento_actividad.csv")
}

message("\n══════════════════════════════════════════════════════════════")
message("  IVEC_contextual — pipeline V1 completado")
message("  Celdas activas: ", format(nrow(sip_indic), big.mark = "."))
message("  Indicadores activos: ",
        sum(tabla_indicadores$Estado_V1 == "ACTIVO"), " de ",
        nrow(tabla_indicadores),
        "  (físico-residencial: ", coh_fisico$n_activos,
        "/4  demográfico-poblacional: ", coh_demo$n_activos,
        "/5  CS: ", coh_cs$n_activos, "/5)")
message("  Indicadores pendientes: ",
        sum(tabla_indicadores$Estado_V1 == "PENDIENTE"), " de ",
        nrow(tabla_indicadores))
message("══════════════════════════════════════════════════════════════\n")
