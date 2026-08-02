# =============================================================================
# 19b_parsear_bdel_gobierto.R — PARSEADO Y AGREGACIÓN CAD BDEL (REPO GOBIERTO)
# =============================================================================
# Módulo:   IVEC_estructural — dimensión CAD (capacidad institucional)
# Entrada:  data/estructural/bdel/gobierto/{año}/{tb_economica_cons,
#           tb_funcional_cons,tb_inventario}.sql.gz  (descargados por 19_*.R)
# Salida:   data/estructural/bdel/cad_gobierto_2015_2024.rds
#
# Estrategia operativa:
#   - Los ficheros son dumps SQL/PostgreSQL con sentencias INSERT por fila.
#     Se parsean por línea con expresiones regulares, sin necesidad de cargar
#     PostgreSQL ni paquetes pesados.
#   - El repositorio gobierto cambia el schema de tb_economica_cons entre años:
#       * Versión 4 cols: (id, tipreig, cdcta, importe)
#       * Versión 7 cols: (id, tipreig, cdcta, imported, importer, importel, importec)
#     El parser detecta dinámicamente el schema leyendo el CREATE TABLE y
#     extrae siempre el importe de obligaciones reconocidas netas (importel
#     en el schema extendido, importe en el simple).
#   - El cruce con tb_inventario se hace por `id` (no por `idente`) porque
#     las tablas *_cons agregan al municipio principal (sufijo AA000 del
#     codbdgel); idente identifica organismos autónomos en las tablas no
#     consolidadas.
#
# Indicadores producidos por (CMUN, anyo):
#
#   Indicadores CAD canónicos del IVEC_estructural V1 (entran al clustering
#   y al escalar CAD):
#   • gasto_sss_total       — suma de `importe` en tb_funcional_cons con
#                             cdfgr comenzando por «23»
#   • inversiones_total     — suma de obligaciones reconocidas netas en
#                             tb_economica_cons con tipreig='G' y cdcta
#                             comenzando por «6»
#   • subvenciones_total    — ídem para cdcta comenzando por «4»
#
#   Variables auxiliares interpretativas (NO entran al clustering ni al
#   escalar CAD; se conservan en el panel final para enriquecer la lectura
#   en cap. 6 §sec-res-estructural y los análisis de robustez en cap. 7;
#   la decisión metodológica se documenta en A4 §app-d-deuda-cad):
#   • intereses_total       — suma de obligaciones reconocidas netas en
#                             tb_economica_cons con tipreig='G' y cdcta
#                             comenzando por «3» (gastos financieros)
#   • amortizacion_total    — ídem para cdcta comenzando por «9»
#                             (pasivos financieros)
#   • carga_financiera_total = intereses_total + amortizacion_total
#                             (servicio anual de la deuda)
#
#   Población:
#   • poblacion             — del tb_inventario (oficial MHFP)
#
#   Versiones per cápita:
#   • gasto_sss_pc, inversiones_pc, subvenciones_pc       — CAD canónicos
#   • intereses_pc, amortizacion_pc, carga_financiera_pc — auxiliares
# =============================================================================

library(here)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)

# ── Configuración ────────────────────────────────────────────────────────────

ANYOS    <- 2015:2024
PROVS_CV <- c("03", "12", "46")  # Alicante, Castellón, Valencia

dir_gobierto <- here("data", "estructural", "bdel", "gobierto")
dir_bdel     <- here("data", "estructural", "bdel")

ruta_salida  <- file.path(dir_bdel, "cad_gobierto_2015_2024.rds")

# ── Lectura del .sql.gz completo ──────────────────────────────────────────────

leer_sql_gz <- function(ruta) {
  con <- gzfile(ruta, "rt", encoding = "UTF-8")
  on.exit(close(con))
  readLines(con, warn = FALSE)
}

# ── Detección del schema ──────────────────────────────────────────────────────
# Lee las primeras líneas del fichero (CREATE TABLE ... ;) y devuelve el
# vector de nombres de columna en el orden en que aparecen en los INSERT.

detectar_columnas <- function(lineas) {
  inicio <- which(str_detect(lineas, "CREATE TABLE"))[1]
  fin    <- which(str_detect(lineas, "^\\);"))[1]
  if (is.na(inicio) || is.na(fin)) return(character())
  cols_brutas <- lineas[(inicio + 1):(fin - 1)]
  cols <- str_match(cols_brutas, '"([^"]+)"')[, 2]
  cols[!is.na(cols)]
}

# ── Parseo de los VALUES ──────────────────────────────────────────────────────
# Tokenizador genérico que respeta las comillas simples en los campos VARCHAR.
# Devuelve una matriz character con tantas columnas como campos.

tokenizar_values <- function(s) {
  # Reemplazos seguros: separar por comas que no estén entre comillas.
  # Usamos un parser secuencial con estado.
  tokens <- character()
  buf    <- ""
  dentro <- FALSE
  chars  <- strsplit(s, "", fixed = TRUE)[[1]]
  for (ch in chars) {
    if (ch == "'") {
      dentro <- !dentro
      buf <- paste0(buf, ch)
    } else if (ch == "," && !dentro) {
      tokens <- c(tokens, buf)
      buf <- ""
    } else {
      buf <- paste0(buf, ch)
    }
  }
  tokens <- c(tokens, buf)
  str_trim(tokens)
}

extraer_filas <- function(lineas, n_cols) {
  m <- str_match(lineas, "VALUES\\s*\\((.+)\\);")
  vals <- m[, 2]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(matrix(character(), 0, n_cols))

  filas <- lapply(vals, tokenizar_values)
  long_ok <- lengths(filas) == n_cols
  if (any(!long_ok)) {
    message("    aviso: ", sum(!long_ok), " filas con longitud ",
            "inconsistente — descartadas")
  }
  filas <- filas[long_ok]

  do.call(rbind, filas)
}

# Limpia un token devuelto por tokenizar_values:
#   - Si está entre comillas, devuelve el contenido sin las comillas
#   - Si es numérico, lo devuelve tal cual
limpiar_token <- function(x) {
  ifelse(str_detect(x, "^'.*'$"), str_sub(x, 2, -2), x)
}

# ── Parsers específicos por tabla ─────────────────────────────────────────────

parsear_economica_cons <- function(ruta) {
  lineas <- leer_sql_gz(ruta)
  cols   <- detectar_columnas(lineas)
  if (length(cols) == 0) {
    warning("Schema no detectado en ", ruta)
    return(tibble())
  }

  mat <- extraer_filas(lineas, length(cols))
  if (nrow(mat) == 0) return(tibble())

  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- cols
  df[] <- lapply(df, limpiar_token)

  # Detectar columna de importe consolidado
  if ("importel" %in% cols) {
    col_importe <- "importel"   # schema extendido: obligaciones reconocidas netas
  } else if ("importe" %in% cols) {
    col_importe <- "importe"    # schema simple: único campo de importe
  } else {
    stop("No se reconoce columna de importe en tb_economica_cons (", ruta, ")")
  }

  tibble(
    id      = as.numeric(df$id),
    tipreig = str_trim(df$tipreig),
    cdcta   = str_trim(df$cdcta),
    importe = as.numeric(df[[col_importe]])
  )
}

parsear_funcional_cons <- function(ruta) {
  lineas <- leer_sql_gz(ruta)
  cols   <- detectar_columnas(lineas)
  if (length(cols) == 0) {
    warning("Schema no detectado en ", ruta)
    return(tibble())
  }

  mat <- extraer_filas(lineas, length(cols))
  if (nrow(mat) == 0) return(tibble())

  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- cols
  df[] <- lapply(df, limpiar_token)

  tibble(
    id      = as.numeric(df$id),
    cdcta   = if ("cdcta" %in% cols) str_trim(df$cdcta) else NA_character_,
    cdfgr   = str_trim(df$cdfgr),
    importe = as.numeric(df$importe)
  )
}

parsear_inventario <- function(ruta) {
  lineas <- leer_sql_gz(ruta)
  cols   <- detectar_columnas(lineas)
  if (length(cols) == 0) {
    warning("Schema no detectado en ", ruta)
    return(tibble())
  }

  mat <- extraer_filas(lineas, length(cols))
  if (nrow(mat) == 0) return(tibble())

  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(df) <- cols
  df[] <- lapply(df, limpiar_token)

  tibble(
    id         = as.numeric(df$id),
    codbdgel   = str_trim(df$codbdgel),
    nombreppal = str_trim(df$nombreppal),
    poblacion  = as.numeric(df$poblacion),
    estado     = str_trim(df$estado)
  ) %>%
    mutate(
      cmun    = str_sub(codbdgel, 1, 5),
      cd_prov = str_sub(codbdgel, 1, 2)
    )
}

# ── Procesamiento por año ────────────────────────────────────────────────────

procesar_anyo <- function(anyo_proc) {

  message("Año ", anyo_proc, " …")
  dir_anyo <- file.path(dir_gobierto, as.character(anyo_proc))

  r_inv  <- file.path(dir_anyo, "tb_inventario.sql.gz")
  r_econ <- file.path(dir_anyo, "tb_economica_cons.sql.gz")
  r_func <- file.path(dir_anyo, "tb_funcional_cons.sql.gz")

  if (!all(file.exists(c(r_inv, r_econ, r_func)))) {
    message("  ficheros incompletos, año omitido")
    return(NULL)
  }

  # 1. Inventario: filtrar municipios CV. Las *_cons usan `id` como clave.
  #    Deduplicar por id principal (un municipio aparece tantas filas como
  #    organismos autónomos tenga; conservamos uno por municipio).
  #
  #    Filtro adicional `poblacion > 0`: el inventario provincial CV recoge
  #    ayuntamientos (sufijo AA000), diputaciones (DD000), mancomunidades
  #    (MM000) y entidades locales menores (AE001/AE002). Solo los
  #    ayuntamientos tienen `poblacion > 0`; el resto se imputa a `poblacion
  #    = 0` en el BDEL porque la población se asigna al municipio matriz
  #    para evitar duplicación. Por tanto el filtro `poblacion > 0` deja
  #    exclusivamente los ayuntamientos principales (entre 475 y 534 por
  #    año, dependiendo de cuántos hayan remitido liquidación), que es la
  #    unidad de análisis canónica del IVEC_estructural (542 municipios CV
  #    objetivo, cobertura efectiva variable por año). La problemática de
  #    incorporar transferencias de capital procedentes de diputaciones
  #    (código económico 761) y de mancomunidades (códigos 763, 764, 765,
  #    767) como dimensión de capacidad institucional supramunicipal queda
  #    documentada como agenda V2 del instrumento.
  inv <- parsear_inventario(r_inv) %>%
    filter(cd_prov %in% PROVS_CV) %>%
    group_by(id) %>%
    slice(1) %>%
    ungroup() %>%
    filter(poblacion > 0)
  message("  inventario CV (ayuntamientos AA000): ", nrow(inv), " municipios")

  # 2. Económica consolidada:
  #    Capítulos canónicos CAD: 4 (subvenciones) y 6 (inversiones reales).
  #    Capítulos auxiliares:    3 (intereses) y 9 (amortización de deuda)
  #                             — componen la variable auxiliar `carga
  #                             financiera`, documentada en A4 §app-d-deuda-cad.
  econ_raw <- parsear_economica_cons(r_econ)
  econ <- econ_raw %>%
    filter(tipreig == "G", id %in% inv$id) %>%
    mutate(cap_eco = str_sub(cdcta, 1, 1)) %>%
    filter(cap_eco %in% c("3", "4", "6", "9"))
  message("  económica G filtrada (cap. 3/4/6/9): ", nrow(econ), " filas")

  econ_agr <- econ %>%
    group_by(id, cap_eco) %>%
    summarise(importe = sum(importe, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(
      names_from   = cap_eco,
      values_from  = importe,
      values_fill  = 0,
      names_prefix = "cap_"
    )

  # Garantizamos que las columnas existan aunque en algún año no haya filas
  for (cap in c("cap_3", "cap_4", "cap_6", "cap_9")) {
    if (!cap %in% names(econ_agr)) econ_agr[[cap]] <- 0
  }

  econ_agr <- econ_agr %>%
    rename(intereses_total    = cap_3,    # auxiliar (cap. 3 — gastos financieros)
           subvenciones_total = cap_4,    # CAD canónico
           inversiones_total  = cap_6,    # CAD canónico
           amortizacion_total = cap_9)    # auxiliar (cap. 9 — pasivos financieros)

  # 3. Funcional consolidado: programa 23x (servicios sociales)
  func_raw <- parsear_funcional_cons(r_func)
  func <- func_raw %>%
    filter(id %in% inv$id, str_starts(cdfgr, "23"))
  message("  funcional 23x filtrada: ", nrow(func), " filas")

  func_agr <- func %>%
    group_by(id) %>%
    summarise(gasto_sss_total = sum(importe, na.rm = TRUE), .groups = "drop")

  # 4. Unión y composición de la carga financiera auxiliar
  resultado <- inv %>%
    select(id, cmun, nombreppal, poblacion) %>%
    left_join(econ_agr, by = "id") %>%
    left_join(func_agr, by = "id") %>%
    mutate(
      across(c(gasto_sss_total, inversiones_total, subvenciones_total,
               intereses_total, amortizacion_total),
             ~ replace_na(.x, 0)),
      carga_financiera_total = intereses_total + amortizacion_total,
      anyo                   = anyo_proc,
      # Indicadores CAD canónicos per cápita
      gasto_sss_pc           = ifelse(poblacion > 0, gasto_sss_total        / poblacion, NA_real_),
      inversiones_pc         = ifelse(poblacion > 0, inversiones_total      / poblacion, NA_real_),
      subvenciones_pc        = ifelse(poblacion > 0, subvenciones_total     / poblacion, NA_real_),
      # Variables auxiliares interpretativas (no entran al clustering CAD)
      intereses_pc           = ifelse(poblacion > 0, intereses_total        / poblacion, NA_real_),
      amortizacion_pc        = ifelse(poblacion > 0, amortizacion_total     / poblacion, NA_real_),
      carga_financiera_pc    = ifelse(poblacion > 0, carga_financiera_total / poblacion, NA_real_)
    ) %>%
    rename(nombre = nombreppal) %>%
    select(cmun, anyo, nombre, poblacion,
           # — CAD canónicos —
           gasto_sss_total,        gasto_sss_pc,
           inversiones_total,      inversiones_pc,
           subvenciones_total,     subvenciones_pc,
           # — auxiliares (carga financiera) —
           intereses_total,        intereses_pc,
           amortizacion_total,     amortizacion_pc,
           carga_financiera_total, carga_financiera_pc)

  message("  municipios CV agregados: ", nrow(resultado))
  resultado
}

# ── Loop principal ────────────────────────────────────────────────────────────

cad_panel <- bind_rows(lapply(ANYOS, procesar_anyo))

# ── Reporte de cobertura ──────────────────────────────────────────────────────

message("")
message("RESUMEN PANEL CAD GOBIERTO")
message("==========================")
message("Filas totales (municipio × año):     ", nrow(cad_panel))
message("Años cubiertos:                       ",
        paste(sort(unique(cad_panel$anyo)), collapse = ", "))
message("Municipios CV únicos:                 ", n_distinct(cad_panel$cmun))
message("")
message("Cobertura por año:")
print(cad_panel %>%
        group_by(anyo) %>%
        summarise(
          n_municipios       = n(),
          # — CAD canónicos —
          con_gasto_sss      = sum(gasto_sss_total    > 0, na.rm = TRUE),
          con_inversiones    = sum(inversiones_total  > 0, na.rm = TRUE),
          con_subvenciones   = sum(subvenciones_total > 0, na.rm = TRUE),
          # — auxiliares (carga financiera) —
          con_intereses      = sum(intereses_total    > 0, na.rm = TRUE),
          con_amortizacion   = sum(amortizacion_total > 0, na.rm = TRUE),
          con_carga_fin      = sum(carga_financiera_total > 0, na.rm = TRUE),
          .groups = "drop"
        ))

# ── Guardado ─────────────────────────────────────────────────────────────────

saveRDS(cad_panel, ruta_salida)
message("")
message("Salida guardada en: ", ruta_salida)
