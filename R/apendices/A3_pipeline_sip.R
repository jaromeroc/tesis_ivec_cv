# =============================================================================
# A3_pipeline_sip.R
# Pipeline reproducible del Apéndice C: Tratamiento de la base APSIG del SIP
# =============================================================================
#
# Este script ejecuta en secuencia los módulos de depuración, transformación y
# enriquecimiento de la base APSIG que en el Rmd (A3-sip.Rmd) se documentan
# con chunks eval=FALSE (Módulos 1–5 y 9). Los módulos 6–8, que producen
# salidas visibles en el PDF (función de calidad, tablas kable y figura
# ggplot2), se ejecutan en tiempo de compilación del Rmd y no se reproducen
# aquí.
#
# Ejecutar este script ANTES de compilar A3-sip.Rmd para que los checkpoints
# y archivos de salida estén disponibles en data/SIP/.
#
# Archivos de entrada requeridos:
#   data/SIP/SIP.txt                              (fichero fuente APSIG)
#   data/SIP/diccionario_etiquetas_sip.xlsx       (diccionario semántico)
#
# Archivos de salida generados:
#   data/SIP/processed_data/SIP_base.parquet      (FASE 0)
#   data/SIP/processed_data/SIP_importado.parquet (FASE 3)
#   data/SIP/processed_data/SIP_separadas.parquet (FASE 4)
#   data/SIP/results/SIP_etiquetado.parquet       (FASE 5)
#   data/SIP/results/df_final_raw.rds             (FASE 5)
#   data/SIP/results/SIP_final.parquet            (FASE 9)
#
# Correspondencia con A3-sip.Rmd:
#   FASE 0 → Preprocesamiento inicial (script externo 00_preprocesa_SIP.R)
#   FASE 1 → Módulo 1: Preparación del entorno y carga de datos
#   FASE 2 → Módulo 2: Normalización y extracción de coordenadas
#   FASE 3 → Módulo 3: Imputación de coordenadas geográficas
#   FASE 4 → Módulo 4: Ingeniería de características desde APSI46
#   FASE 5 → Módulo 5: Etiquetado semántico de variables
#   FASE 9 → Módulo 9: Limpieza final de columnas y dataset para análisis
#
# Autor: Joan Antonio Romero Crespo
# Tesis: Instrument de Valoració de la Vulnerabilitat Estructural i Contextual (IVEC-CV)
# Fecha: 2026
# =============================================================================

# ── Semilla global ────────────────────────────────────────────────────────────
set.seed(1972)

# ── Librerías ─────────────────────────────────────────────────────────────────
suppressPackageStartupMessages({
  library(here)
  library(arrow)
  library(dplyr)
  library(stringr)
  library(glue)
  library(purrr)
  library(readxl)
  library(rlang)
  library(tidyr)
})

# =============================================================================
# FASE 0: Preprocesamiento inicial — equivalente a 00_preprocesa_SIP.R
# Transforma SIP.txt → SIP_base.parquet (formato columnar optimizado)
# =============================================================================
message("\n── FASE 0: Preprocesamiento inicial ──────────────────────────────────────")

ruta_txt     <- here::here("data/SIP", "SIP.txt")
ruta_parquet <- here::here("data/SIP/processed_data", "SIP_base.parquet")

if (file.exists(ruta_parquet)) {
  message("  ✓ SIP_base.parquet ya existe. Se omite el reprocesamiento.")
} else {
  if (!file.exists(ruta_txt)) {
    stop(glue::glue(
      "✗ No se encontró 'SIP.txt' en {dirname(ruta_txt)}. ",
      "Verifique la ruta del fichero fuente."
    ))
  }

  message("  · Leyendo SIP.txt ...")
  df_raw <- read.delim(
    ruta_txt,
    sep       = "|",
    colClasses = "character",
    na.strings = c("", "****")
  )

  # Eliminar columnas completamente vacías
  df_raw <- df_raw[, colSums(!is.na(df_raw)) > 0]

  # Crear carpeta de salida si no existe
  dir.create(dirname(ruta_parquet), recursive = TRUE, showWarnings = FALSE)

  # Guardar en formato parquet
  arrow::write_parquet(df_raw, ruta_parquet)
  message(glue::glue(
    "  ✓ SIP_base.parquet generado: {nrow(df_raw)} registros, ",
    "{ncol(df_raw)} columnas."
  ))
  rm(df_raw)
  invisible(gc())
}

# =============================================================================
# FASE 1: Preparación del entorno y carga de datos
# Carga SIP_base.parquet en memoria y verifica consistencia APSI
# =============================================================================
message("\n── FASE 1: Carga de datos ────────────────────────────────────────────────")

if (!file.exists(ruta_parquet)) {
  stop(glue::glue(
    "✗ No se encontró 'SIP_base.parquet'. ",
    "Ejecute la FASE 0 antes de continuar."
  ))
}

df <- arrow::read_parquet(ruta_parquet)
message(glue::glue(
  "  ✓ Dataset cargado: {nrow(df)} registros, {ncol(df)} columnas."
))

# Verificación de consistencia entre código de asignación y residencia en APSI
df_verif <- df %>%
  mutate(
    dep_asig_crudo  = str_sub(APSI, 12, 13),
    dep_resid_crudo = str_sub(APSI, 43, 44),
    coincide        = dep_asig_crudo == dep_resid_crudo
  )

prop_coincide <- mean(df_verif$coincide, na.rm = TRUE)
message(glue::glue(
  "  · Coincidencia código asignación / residencia: ",
  "{round(100 * prop_coincide, 2)}%"
))
rm(df_verif)
invisible(gc())

# =============================================================================
# FASE 2: Normalización y extracción de coordenadas
# Estandariza UCO_CODI y N_SIP; construye ST_X, ST_Y desde APSI
# Produce: df_stand
# =============================================================================
message("\n── FASE 2: Normalización e identificadores ───────────────────────────────")

df_stand <- df %>%
  mutate(
    # Normalización de identificadores (padding con ceros)
    UCO_CODI = str_pad(UCO_CODI, 6, pad = "0"),
    N_SIP    = str_pad(N_SIP, 8, pad = "0"),

    # Subcadenas de APSI
    apsilen  = nchar(APSI),
    APSI_46  = str_pad(str_sub(APSI, 1, 46), 46, pad = "0"),
    APSI_72  = if_else(
      apsilen >= 72,
      str_sub(APSI, 1, 72),
      NA_character_
    ),
    xy_raw   = if_else(
      apsilen >= 72,
      str_sub(APSI, 47, 72),
      NA_character_
    ),

    # Conversión y consolidación de coordenadas (fuente SIP o fuente APSI)
    ST_X = coalesce(
      na_if(as.numeric(ST_X), 0),
      na_if(as.numeric(str_sub(xy_raw, 1, 13)), 0)
    ),
    ST_Y = coalesce(
      na_if(as.numeric(ST_Y), 0),
      na_if(as.numeric(str_sub(xy_raw, 14, 26)), 0)
    )
  ) %>%
  # Eliminar variables auxiliares intermedias
  select(-xy_raw, -apsilen)

message(glue::glue(
  "  ✓ df_stand: {nrow(df_stand)} registros. ",
  "ST_X válidas: {sum(!is.na(df_stand$ST_X))} ",
  "({round(100*mean(!is.na(df_stand$ST_X)),1)}%)."
))

rm(df)
invisible(gc())

# =============================================================================
# FASE 3: Imputación de coordenadas geográficas
# Para cada UCO_CODI, asigna las coordenadas de cualquier conviviente
# con ubicación válida a los restantes sin coordenadas.
# Produce: df_imputado + checkpoint SIP_importado.parquet
# =============================================================================
message("\n── FASE 3: Imputación por UCO_CODI ──────────────────────────────────────")

# Coordenadas de referencia por unidad de convivencia
df_coords_referencia <- df_stand %>%
  group_by(UCO_CODI) %>%
  summarise(
    ST_X_ref = first(ST_X[!is.na(ST_X)]),
    ST_Y_ref = first(ST_Y[!is.na(ST_Y)]),
    .groups  = "drop"
  ) %>%
  filter(!is.na(ST_X_ref) | !is.na(ST_Y_ref))

# Unión e imputación
df_imputado <- df_stand %>%
  left_join(df_coords_referencia, by = "UCO_CODI") %>%
  mutate(
    ST_X = coalesce(ST_X, ST_X_ref),
    ST_Y = coalesce(ST_Y, ST_Y_ref)
  ) %>%
  select(-ST_X_ref, -ST_Y_ref)

message(glue::glue(
  "  ✓ Tras imputación — ST_X válidas: {sum(!is.na(df_imputado$ST_X))} ",
  "({round(100*mean(!is.na(df_imputado$ST_X)),1)}%)."
))

# Checkpoint condicional
ruta_checkpoint <- here::here(
  "data/SIP/processed_data",
  "SIP_importado.parquet"
)

if (!file.exists(ruta_checkpoint)) {
  dir.create(dirname(ruta_checkpoint), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df_imputado, ruta_checkpoint)
  message("  ✓ Checkpoint guardado: SIP_importado.parquet")
} else {
  message("  · Checkpoint SIP_importado.parquet ya existe. Se omite.")
}

invisible({
  rm(df_stand, df_coords_referencia)
  gc()
})

# =============================================================================
# FASE 4: Ingeniería de características desde APSI_46
# Descompone APSI_46 en variables demográficas, socioeconómicas,
# territoriales y de aseguramiento. Construye zona_asig, zona_resid y ASI.
# Produce: df_sep + checkpoint SIP_separadas.parquet
# =============================================================================
message("\n── FASE 4: Feature engineering APSI_46 ──────────────────────────────────")

df_sep <- df_imputado %>%
  mutate(
    # Dimensiones demográficas y de adscripción
    empadronamiento = str_sub(APSI_46, 1, 1),
    nacionalidad    = str_sub(APSI_46, 2, 2),
    sexo            = str_sub(APSI_46, 3, 3),
    f_nacim         = str_sub(APSI_46, 4, 11),
    dep_asig        = str_sub(APSI_46, 12, 13),

    # Dimensiones de cobertura y situación socioeconómica
    D1_fin_cov      = str_sub(APSI_46, 14, 15),
    D2_resid        = str_sub(APSI_46, 16, 16),
    D3_migr         = str_sub(APSI_46, 17, 18),
    D4_lab          = str_sub(APSI_46, 19, 19),
    D5_aseg_subgr   = str_sub(APSI_46, 20, 21),
    D5_aseg_gr      = str_sub(APSI_46, 20, 20),

    # Dimensiones geográficas
    D6_geo_cv       = str_sub(APSI_46, 22, 22),
    D6_geo_onu      = str_sub(APSI_46, 23, 25),

    # Dimensiones de vulnerabilidad y residencia
    D7_vulner_apsig = str_sub(APSI_46, 26, 26),
    D8_tipo_res     = str_sub(APSI_46, 27, 28),
    D8_comp_res     = str_sub(APSI_46, 29, 29),
    D8_tam_res      = str_sub(APSI_46, 30, 30),

    # Renta y cronicidad
    D9_raf_renta    = str_sub(APSI_46, 33, 34),
    cronicidad      = str_sub(APSI_46, 37, 37),

    # Identificadores territoriales
    centro_asig     = str_sub(APSI_46, 38, 42),
    dep_resid       = str_sub(APSI_46, 43, 44),

    # Variables combinadas de zona
    zona_asig = paste0(
      str_pad(dep_asig, 2, pad = "0"),
      str_pad(str_sub(APSI_46, 35, 36), 2, pad = "0")
    ),
    zona_resid = paste0(
      str_pad(dep_resid, 2, pad = "0"),
      str_pad(str_sub(APSI_46, 45, 46), 2, pad = "0")
    ),

    # Variable ASI: agrupación de departamentos en 8 áreas sanitarias
    ASI = case_when(
      dep_asig %in% c("01", "02", "03")       ~ "1",
      dep_asig %in% c("04", "05", "12")       ~ "2",
      dep_asig %in% c("06", "07")             ~ "3",
      dep_asig %in% c("08", "09", "23")       ~ "4",
      dep_asig %in% c("10", "11", "14")       ~ "5",
      dep_asig %in% c("13", "16", "17")       ~ "6",
      dep_asig %in% c("15", "18", "19")       ~ "7",
      dep_asig %in% c("20", "21", "22", "24") ~ "8",
      TRUE ~ NA_character_
    )
  )

message(glue::glue(
  "  ✓ df_sep: {nrow(df_sep)} registros, {ncol(df_sep)} columnas.",
  " Áreas sanitarias (ASI): {n_distinct(df_sep$ASI, na.rm=TRUE)} valores."
))

# Checkpoint condicional
output_path <- here::here("data/SIP/processed_data", "SIP_separadas.parquet")

if (!file.exists(output_path)) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(df_sep, output_path)
  message("  ✓ Checkpoint guardado: SIP_separadas.parquet")
} else {
  message("  · Checkpoint SIP_separadas.parquet ya existe. Se omite.")
}

rm(df_imputado)
invisible(gc())

# =============================================================================
# FASE 5: Etiquetado semántico de variables
# Incorpora descripciones legibles desde diccionario Excel.
# Genera pares código–descripción mediante add_etiqueta_dual().
# Produce: df_final_raw + SIP_etiquetado.parquet + df_final_raw.rds
# =============================================================================
message("\n── FASE 5: Etiquetado semántico + df_final_raw ───────────────────────────")

ruta_diccionario <- here::here("data/SIP", "diccionario_etiquetas_sip.xlsx")

if (!file.exists(ruta_diccionario)) {
  stop(glue::glue(
    "✗ No se encontró el diccionario de etiquetas en:\n  {ruta_diccionario}"
  ))
}

# Lectura de hojas del diccionario
hojas <- readxl::excel_sheets(ruta_diccionario)

leer_diccionario <- function(sheet) {
  tabla <- readxl::read_excel(ruta_diccionario, sheet = sheet)
  if (!all(c("valor", "etiqueta") %in% names(tabla))) return(NULL)
  tabla %>% mutate(across(everything(), as.character))
}

# Limpieza de caracteres especiales en etiquetas
limpiar_etiquetas <- function(x) {
  x %>%
    str_replace_all("[\\r\\n]+", " ") %>%
    str_replace_all("[\u201c\u201d]", "\"") %>%
    str_replace_all("[\u2018\u2019]", "'") %>%
    str_replace_all("[\u2013\u2014]", "-") %>%
    str_squish()
}

safe_results  <- map(hojas, safely(leer_diccionario))
keep_mask     <- map_lgl(safe_results, ~ !is.null(.x$result))
diccionarios  <- safe_results[keep_mask] %>%
  map("result") %>%
  set_names(hojas[keep_mask]) %>%
  map(~ mutate(.x, etiqueta = limpiar_etiquetas(etiqueta)))

# Ajuste de padding en claves de alta cardinalidad
pad_config <- list(
  centro_asig = 5, D8_tipo_res = 2, D6_geo_onu = 3,
  dep_asig = 2, zona_asig = 4, dep_resid = 2, zona_resid = 4
)

for (v in names(pad_config)) {
  if (v %in% names(diccionarios)) {
    diccionarios[[v]] <- diccionarios[[v]] %>%
      mutate(valor = str_pad(valor, pad_config[[v]], pad = "0"))
  }
}

# Función de etiquetado dual (código → código + descripción)
add_etiqueta_dual <- function(df, var) {
  if (!(var %in% names(diccionarios)) || !(var %in% names(df))) return(df)
  var_cod  <- paste0(var, "(cod)")
  var_desc <- paste0(var, "(desc)")
  df %>%
    rename(!!var_cod := !!sym(var)) %>%
    left_join(diccionarios[[var]], by = setNames("valor", var_cod)) %>%
    rename(!!var_desc := etiqueta)
}

# Aplicación del etiquetado a todas las variables disponibles
vars_etiquetar <- intersect(names(diccionarios), names(df_sep))

df_etiquetado <- if (length(vars_etiquetar) == 0) {
  df_sep
} else {
  reduce(vars_etiquetar, add_etiqueta_dual, .init = df_sep)
}

df_final_raw <- df_etiquetado

message(glue::glue(
  "  ✓ df_final_raw: {nrow(df_final_raw)} registros, ",
  "{ncol(df_final_raw)} columnas. ",
  "Variables etiquetadas: {length(vars_etiquetar)}."
))

# Persistencia en disco (condicional)
output_etiquetado <- here::here("data/SIP/results", "SIP_etiquetado.parquet")
output_rds        <- here::here("data/SIP/results", "df_final_raw.rds")

dir.create(dirname(output_etiquetado), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(output_etiquetado)) {
  arrow::write_parquet(df_final_raw, output_etiquetado)
  message("  ✓ SIP_etiquetado.parquet guardado.")
} else {
  message("  · SIP_etiquetado.parquet ya existe. Se omite.")
}

saveRDS(df_final_raw, output_rds)
message(glue::glue("  ✓ df_final_raw.rds guardado en {dirname(output_rds)}"))

# Liberación de objetos intermedios
invisible({
  rm(df_sep, df_etiquetado, diccionarios, hojas, pad_config,
     leer_diccionario, limpiar_etiquetas, add_etiqueta_dual, vars_etiquetar)
  gc()
})

# =============================================================================
# NOTA: Módulos 6, 7 y 8 se ejecutan en tiempo de compilación del Rmd
# ─────────────────────────────────────────────────────────────────────────────
# Módulo 6 (eval=TRUE):  Define evaluar_calidad() + carga df_final_raw.rds
# Módulo 7 (eval=TRUE):  Tablas kable — calidad antes de la limpieza
# Módulo 8 (eval=TRUE):  Figura ggplot2 — calidad después de la limpieza
#                        También genera df_final_clean (interno al Rmd)
# =============================================================================

# =============================================================================
# FASE 9: Limpieza final de columnas y dataset para análisis
# Construye df_final desde df_final_raw depurando APSI_46.
# Conserva: UCO_CODI, N_SIP, variables (cod)/(desc), ST_X, ST_Y.
# Produce: SIP_final.parquet
# =============================================================================
message("\n── FASE 9: Consolidación df_final + SIP_final.parquet ───────────────────")

# Aplicar la misma depuración que el Módulo 8 del Rmd
df_final_clean <- df_final_raw %>%
  filter(!is.na(APSI_46), APSI_46 != "0")

message(glue::glue(
  "  · Registros tras depuración APSI_46: {nrow(df_final_clean)} ",
  "(eliminados: {nrow(df_final_raw) - nrow(df_final_clean)}, ",
  "{round(100*(1 - nrow(df_final_clean)/nrow(df_final_raw)), 2)}%)."
))

# Construcción del dataset final (selección de columnas)
# f_nacim se retiene porque es insumo directo de vi_edad y ch_ratio_dep
# en el pipeline del IVEC_base (R/ivec/01_ivec_base.R).
df_final <- df_final_clean %>%
  select(
    # Identificadores clave
    UCO_CODI, N_SIP,
    # Fecha de nacimiento (necesaria para vi_edad y ch_ratio_dep en IVEC_base)
    f_nacim,
    # Variables codificadas
    matches("\\(cod\\)$"),
    # Variables descriptivas (etiquetas semánticas)
    matches("\\(desc\\)$"),
    # Coordenadas georreferenciadas
    ST_X, ST_Y
  )

message(glue::glue(
  "  ✓ df_final: {nrow(df_final)} registros, {ncol(df_final)} columnas."
))

# Registros sin coordenadas (métrica de cobertura geográfica)
n_sin_coord  <- sum(is.na(df_final$ST_X) | is.na(df_final$ST_Y))
pct_sin_coord <- round(100 * n_sin_coord / nrow(df_final), 1)
message(glue::glue(
  "  · Registros sin coordenadas válidas: {n_sin_coord} ({pct_sin_coord}%)"
))

# Persistencia en disco (siempre sobreescribe para reflejar cambios en el schema)
output_final <- here::here("data/SIP/results", "SIP_final.parquet")

dir.create(dirname(output_final), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(df_final, output_final)
message(glue::glue("  ✓ SIP_final.parquet guardado en {dirname(output_final)}"))

# =============================================================================
# RESUMEN FINAL DEL PIPELINE
# =============================================================================
message("\n═══════════════════════════════════════════════════════════════════════════")
message("  PIPELINE A3 COMPLETADO")
message(glue::glue("  Registros finales (df_final):  {nrow(df_final)}"))
message(glue::glue("  Columnas finales:               {ncol(df_final)}"))
message(glue::glue("  Sin coordenadas:                {n_sin_coord} ({pct_sin_coord}%)"))
message("  Archivos generados:")
message("    · data/SIP/processed_data/SIP_base.parquet")
message("    · data/SIP/processed_data/SIP_importado.parquet")
message("    · data