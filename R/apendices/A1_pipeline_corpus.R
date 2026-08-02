# =============================================================================
# A1_pipeline_corpus.R
# Pipeline reproducible: construcción, depuración y validación del corpus
# bibliográfico sobre vulnerabilidad (RefWoS_full)
#
# Tesis: Instrument de Valoració de la Vulnerabilitat Estructural i Contextual (IVEC-CV)
# Autor: Joan A. Romero Crespo
# Corresponde a: Apéndice A (A1-corpus.Rmd)
#
# INSTRUCCIONES DE USO:
#   1. Establecer el directorio de trabajo en la raíz del proyecto
#      (donde se encuentra Tesis-Vulnerabilidad.Rproj).
#   2. Ejecutar este script ANTES de compilar A1-corpus.Rmd.
#   3. Los ficheros exportados desde WoS deben estar en data/WoS/.
#   4. El script genera el objeto RefWoS_full.RData en data/WoS/ y los
#      artefactos de auditoría en outputs/corpus/.
# =============================================================================

library(bibliometrix)   # convert2df, biblioAnalysis
library(dplyr)
library(stringr)
library(readr)
library(tibble)

# ---- Parámetros globales -----------------------------------------------------

YEAR_MIN_CORPUS <- 1950L
YEAR_MAX_CORPUS <- 2024L
WOS_DIR         <- file.path("data", "WoS")
OUT_CORPUS      <- file.path("outputs", "corpus")

dir.create(WOS_DIR,    recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_CORPUS, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# FASE 1. Delimitación y descarga del universo documental
# ==============================================================================
# Descripción: los ficheros descargados desde Web of Science Core Collection
# (formato EndNote Desktop / WoS tagged) deben estar en data/WoS/.
# Esta fase no ejecuta código de descarga (la descarga es manual o mediante
# API); su función es documentar los parámetros de la consulta y verificar
# la presencia de los ficheros.
#
# Cadena de búsqueda (aplicada el 16-07-2025):
#   TS=(vulnerability OR "social vulnerability" OR "social exclusion" OR
#       precarity OR "risk of exclusion" OR "structural poverty" OR
#       "social disadvantage" OR "economic insecurity" OR
#       "livelihood insecurity" OR "socioeconomic vulnerability" OR
#       "urban marginality")
#   AND DT=(Article)
#   AND WC=("Sociology" OR "Urban Studies" OR "Environmental Studies" OR
#           "Geography" OR "Development Studies" OR "Social Work" OR
#           "Planning & Development")
#   AND PY=(1950–2024)
# Resultado esperado: 22.492 registros descargados en 45 ficheros.

cat("=== FASE 1: Delimitación y descarga ===\n")
wos_files <- list.files(WOS_DIR, pattern = "\\.ciw$|\\.txt$",
                        full.names = TRUE)
if (length(wos_files) == 0) {
  stop("No se encontraron ficheros WoS en ", WOS_DIR,
       "\nDescargue los registros desde Web of Science y deposítelos allí.")
}
cat("Ficheros WoS detectados:", length(wos_files), "\n")


# ==============================================================================
# FASE 2. Consolidación del corpus bibliográfico
# ==============================================================================
# Descripción: integra todos los lotes de exportación en un único objeto de
# trabajo (M_raw) mediante bibliometrix::convert2df(), preservando la
# correspondencia con los archivos originales y generando un log de proceso.

cat("\n=== FASE 2: Consolidación del corpus ===\n")

log_lines <- character(0)
M_raw     <- NULL

for (f in wos_files) {
  tryCatch({
    tmp <- convert2df(f, dbsource = "wos", format = "plaintext")
    n   <- nrow(tmp)
    log_lines <- c(log_lines, sprintf("OK | %s | %d registros", basename(f), n))
    M_raw <- if (is.null(M_raw)) tmp else bind_rows(M_raw, tmp)
    cat("  Cargado:", basename(f), "(", n, "docs)\n")
  }, error = function(e) {
    log_lines <<- c(log_lines, sprintf("ERROR | %s | %s", basename(f), e$message))
    warning("Error procesando ", f, ": ", e$message)
  })
}

writeLines(log_lines, file.path(WOS_DIR, "consolidacion_log.txt"))
cat("Corpus consolidado:", nrow(M_raw), "registros\n")
cat("Log guardado en", file.path(WOS_DIR, "consolidacion_log.txt"), "\n")

# Verificación preliminar de integridad
campos_criticos <- c("AU", "TI", "SO", "PY", "TC", "CR", "DE")
campos_presentes <- campos_criticos[campos_criticos %in% names(M_raw)]
campos_ausentes  <- setdiff(campos_criticos, names(M_raw))
cat("Campos críticos presentes:", paste(campos_presentes, collapse = ", "), "\n")
if (length(campos_ausentes) > 0)
  warning("Campos críticos AUSENTES: ", paste(campos_ausentes, collapse = ", "))


# ==============================================================================
# FASE 3. Depuración estructural del corpus documental
# ==============================================================================
# Descripción: (i) elimina duplicados mediante jerarquía de identificadores
# DOI > UT > clave compuesta (TI + PY + SO); (ii) filtra registros fuera
# del rango temporal 1950–2024.

cat("\n=== FASE 3: Depuración estructural ===\n")
n_raw <- nrow(M_raw)

M_dedup <- M_raw %>%
  mutate(PY = suppressWarnings(as.integer(PY)))

# --- Deduplicación por DOI ---
if ("DI" %in% names(M_dedup)) {
  doi_ok   <- !is.na(M_dedup$DI) & nchar(trimws(M_dedup$DI)) > 0
  M_doi    <- M_dedup[doi_ok, ] %>%
    mutate(DI_norm = toupper(trimws(DI))) %>%
    distinct(DI_norm, .keep_all = TRUE)
  M_nodoi  <- M_dedup[!doi_ok, ]
} else {
  M_doi   <- M_dedup[0, ]
  M_nodoi <- M_dedup
}

# --- Deduplicación por UT (sin DOI) ---
if ("UT" %in% names(M_nodoi)) {
  ut_ok     <- !is.na(M_nodoi$UT) & nchar(trimws(M_nodoi$UT)) > 0
  M_ut      <- M_nodoi[ut_ok, ] %>%
    mutate(UT_norm = toupper(trimws(UT))) %>%
    distinct(UT_norm, .keep_all = TRUE)
  M_rest    <- M_nodoi[!ut_ok, ]
} else {
  M_ut   <- M_nodoi[0, ]
  M_rest <- M_nodoi
}

# --- Deduplicación por clave compuesta TI + PY + SO (sin DOI ni UT) ---
if (nrow(M_rest) > 0 && all(c("TI", "SO") %in% names(M_rest))) {
  M_rest <- M_rest %>%
    mutate(
      clave = paste0(
        toupper(str_sub(str_squish(TI), 1, 80)), "|",
        as.character(PY), "|",
        toupper(str_sub(str_squish(SO), 1, 40))
      )
    ) %>%
    distinct(clave, .keep_all = TRUE)
}

M_dedup <- bind_rows(
  select(M_doi,  -any_of(c("DI_norm"))),
  select(M_ut,   -any_of(c("UT_norm"))),
  select(M_rest, -any_of(c("clave")))
)

cat("Registros antes de deduplicación:", n_raw, "\n")
cat("Registros tras deduplicación    :", nrow(M_dedup), "\n")
cat("Duplicados eliminados           :", n_raw - nrow(M_dedup), "\n")

# --- Filtro temporal ---
n_pre_temporal <- nrow(M_dedup)
M_dedup <- M_dedup %>%
  filter(!is.na(PY), PY >= YEAR_MIN_CORPUS, PY <= YEAR_MAX_CORPUS)
cat("Registros fuera de rango temporal eliminados:",
    n_pre_temporal - nrow(M_dedup), "\n")
cat("Corpus tras depuración:", nrow(M_dedup), "documentos\n")


# ==============================================================================
# FASE 4. Optimización y validación operativa del corpus
# ==============================================================================
# Descripción: construye RefWoS_slim seleccionando las variables necesarias,
# garantiza compatibilidad con bibliometrix (campos DB, SR), y cuantifica
# la completitud de los campos críticos.

cat("\n=== FASE 4: Optimización y validación ===\n")

campos_slim <- intersect(
  c("AU", "TI", "SO", "PY", "TC", "CR", "DE", "ID",
    "AB", "UT", "DI", "DT", "C1", "DB", "SR",
    "WC", "JI", "VL", "IS", "BP", "EP", "PN"),
  names(M_dedup)
)

RefWoS_slim <- M_dedup %>% select(all_of(campos_slim))

# Garantizar presencia de DB y SR (requeridos por bibliometrix)
if (!"DB" %in% names(RefWoS_slim))
  RefWoS_slim <- RefWoS_slim %>% mutate(DB = "ISI")
if (!"SR" %in% names(RefWoS_slim)) {
  RefWoS_slim <- RefWoS_slim %>%
    mutate(
      SR = paste0(
        str_extract(AU, "^[^;]+"),
        ", ", PY,
        ", ", str_sub(SO, 1, 30)
      )
    )
}

cat("Campos en RefWoS_slim:", paste(names(RefWoS_slim), collapse = ", "), "\n")

# Completitud de campos
campos_validar <- intersect(
  c("AU", "SO", "PY", "DT", "TC", "CR", "TI", "DE", "ID"),
  names(RefWoS_slim)
)
completitud <- sapply(campos_validar, function(col) {
  n_na  <- sum(is.na(RefWoS_slim[[col]]) | nchar(trimws(RefWoS_slim[[col]])) == 0)
  round(n_na / nrow(RefWoS_slim) * 100, 2)
})
cat("Porcentaje de valores ausentes por campo crítico:\n")
print(sort(completitud))


# ==============================================================================
# FASE 5. Normalización heurística mediante tesauros
# ==============================================================================
# Descripción: aplica los tesauros construidos inductivamente sobre AU, DE e ID
# para reducir la fragmentación formal de autoría y vocabulario temático.
# Los tesauros se almacenan en data/WoS/tesauros/ como ficheros CSV con
# columnas 'label' y 'replace_by'.

cat("\n=== FASE 5: Normalización heurística ===\n")

TESAURO_DIR <- file.path(WOS_DIR, "tesauros")

# ---- 5.1 Construcción del campo KW_norm (DE + ID combinados) ----------------
RefWoS_slim <- RefWoS_slim %>%
  mutate(
    KW_raw = case_when(
      !is.na(DE) & !is.na(ID) & nchar(trimws(DE)) > 0 & nchar(trimws(ID)) > 0
        ~ paste(DE, ID, sep = "; "),
      !is.na(DE) & nchar(trimws(DE)) > 0 ~ DE,
      !is.na(ID) & nchar(trimws(ID)) > 0 ~ ID,
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    KW_norm = str_squish(toupper(KW_raw)),
    KW_norm = str_replace_all(KW_norm, "[,/|]+", ";"),
    KW_norm = str_replace_all(KW_norm, ";\\s*", "; "),
    KW_norm = str_replace_all(KW_norm, "\\s+;", ";"),
    KW_norm = trimws(KW_norm)
  )

# Aplicar tesauro de palabras clave (si existe)
tesauro_kw_path <- file.path(TESAURO_DIR, "tesauro_kw.csv")
if (file.exists(tesauro_kw_path)) {
  tesauro_kw <- read_csv(tesauro_kw_path, show_col_types = FALSE)
  for (i in seq_len(nrow(tesauro_kw))) {
    pattern  <- paste0("(?<![A-Z0-9])",
                       str_replace_all(tesauro_kw$label[[i]], "([\\(\\)\\[\\]\\{\\}])", "\\\\\\1"),
                       "(?![A-Z0-9])")
    replace  <- tesauro_kw$replace_by[[i]]
    RefWoS_slim$KW_norm <- str_replace_all(RefWoS_slim$KW_norm, pattern, replace)
  }
  cat("Tesauro KW aplicado:", nrow(tesauro_kw), "sustituciones definidas\n")
} else {
  warning("Tesauro KW no encontrado en ", tesauro_kw_path)
}

# ---- 5.2 Normalización de AU_norm -------------------------------------------
RefWoS_slim <- RefWoS_slim %>%
  mutate(
    AU_norm = str_squish(toupper(AU)),
    AU_norm = str_replace_all(AU_norm, "[^A-Z0-9;,\\- ]", ""),
    AU_norm = str_replace_all(AU_norm, ";\\s*", "; ")
  )

# Aplicar tesauro de autoría (si existe)
tesauro_au_path <- file.path(TESAURO_DIR, "tesauro_au.csv")
if (file.exists(tesauro_au_path)) {
  tesauro_au <- read_csv(tesauro_au_path, show_col_types = FALSE)
  for (i in seq_len(nrow(tesauro_au))) {
    RefWoS_slim$AU_norm <- str_replace_all(
      RefWoS_slim$AU_norm,
      fixed(tesauro_au$label[[i]]),
      tesauro_au$replace_by[[i]]
    )
  }
  cat("Tesauro AU aplicado:", nrow(tesauro_au), "sustituciones definidas\n")
} else {
  warning("Tesauro AU no encontrado en ", tesauro_au_path)
}

cat("Cardinalidad KW_norm:", n_distinct(
  unlist(str_split(na.omit(RefWoS_slim$KW_norm), "; "))
), "términos únicos\n")
cat("Cardinalidad AU_norm:", n_distinct(
  unlist(str_split(na.omit(RefWoS_slim$AU_norm), "; "))
), "autores únicos\n")


# ==============================================================================
# FASE 6. Descomposición analítica de referencias citadas (CR)
# ==============================================================================
# Descripción: descompone CR en dos representaciones relacionales:
#   - CR_norm_author_year: autor + año (nivel intelectual)
#   - CR_norm_obra: obra/título normalizado (nivel de obra)
# Conserva CR original en CR_raw.

cat("\n=== FASE 6: Descomposición de referencias citadas ===\n")

RefWoS_slim <- RefWoS_slim %>%
  mutate(
    CR_raw = CR,

    # CR_norm_author_year: extraer "APELLIDO YYYY" de cada referencia
    CR_norm_author_year = map_chr(CR, function(cr_field) {
      if (is.na(cr_field) || nchar(trimws(cr_field)) == 0) return(NA_character_)
      refs <- str_split(cr_field, "; ?")[[1]]
      parsed <- sapply(refs, function(ref) {
        # Patrón WoS: "APELLIDO I, YYYY, ..."
        m <- str_match(ref, "^([A-Z][A-Z\\-\\s']+),\\s*(\\d{4})")
        if (!is.na(m[1, 1]))
          return(paste0(str_squish(m[1, 2]), " ", m[1, 3]))
        # Patrón alternativo: extraer cualquier apellido + año
        m2 <- str_match(ref, "(\\d{4})")
        year <- if (!is.na(m2[1, 1])) m2[1, 1] else ""
        author_part <- str_squish(str_extract(ref, "^[^,]+"))
        if (!is.na(author_part) && nchar(author_part) > 0 && nchar(year) > 0)
          return(paste0(author_part, " ", year))
        return(NA_character_)
      })
      paste(na.omit(parsed), collapse = "; ")
    }),

    # CR_norm_obra: mantener las primeras 3 tokens de cada referencia
    CR_norm_obra = map_chr(CR, function(cr_field) {
      if (is.na(cr_field) || nchar(trimws(cr_field)) == 0) return(NA_character_)
      refs <- str_split(cr_field, "; ?")[[1]]
      parsed <- sapply(refs, function(ref) {
        parts <- str_split(ref, ", ?")[[1]]
        str_squish(paste(parts[seq_len(min(3, length(parts)))], collapse = ", "))
      })
      paste(parsed, collapse = "; ")
    })
  )

cat("CR_norm_author_year: tokens únicos =",
    n_distinct(unlist(str_split(na.omit(RefWoS_slim$CR_norm_author_year), "; "))), "\n")


# ==============================================================================
# FASE 7. Sanitización de entidades relacionales
# ==============================================================================
# Descripción: elimina tokens no informativos (NA, vacíos, ANONYMOUS,
# identificadores numéricos puros) en los campos relacionales derivados.

cat("\n=== FASE 7: Sanitización de entidades relacionales ===\n")

stopwords_rel <- c(
  "NA", "N/A", "ANONYMOUS", "ANON", "[ANONYMOUS]",
  "NULL", "UNKNOWN", "NO AUTHOR"
)

sanitize_tokens <- function(field) {
  map_chr(field, function(x) {
    if (is.na(x) || nchar(trimws(x)) == 0) return(NA_character_)
    tokens <- str_split(x, "; ?")[[1]]
    tokens <- str_squish(tokens)
    # Eliminar vacíos, stopwords y tokens puramente numéricos
    tokens <- tokens[
      nchar(tokens) > 0 &
      !tokens %in% stopwords_rel &
      !str_detect(tokens, "^\\d+$")
    ]
    if (length(tokens) == 0) return(NA_character_)
    paste(tokens, collapse = "; ")
  })
}

RefWoS_slim <- RefWoS_slim %>%
  mutate(
    AU_norm             = sanitize_tokens(AU_norm),
    KW_norm             = sanitize_tokens(KW_norm),
    CR_norm_author_year = sanitize_tokens(CR_norm_author_year),
    CR_norm_obra        = sanitize_tokens(CR_norm_obra)
  )

cat("Sanitización completada.\n")


# ==============================================================================
# FASE 8. Auditoría de validación del corpus final (RefWoS_full)
# ==============================================================================
# Descripción: construye RefWoS_full y ejecuta la auditoría de validación
# sobre completitud, coherencia y operabilidad de los campos relacionales.

cat("\n=== FASE 8: Auditoría de validación (RefWoS_full) ===\n")

RefWoS_full <- RefWoS_slim

# ---- 8.1 Identidad del corpus -----------------------------------------------
n_docs   <- nrow(RefWoS_full)
yr_range <- range(RefWoS_full$PY, na.rm = TRUE)
cat("RefWoS_full:", n_docs, "documentos | Rango PY:", yr_range[1], "–", yr_range[2], "\n")

n_out_range <- sum(RefWoS_full$PY < YEAR_MIN_CORPUS | RefWoS_full$PY > YEAR_MAX_CORPUS,
                   na.rm = TRUE)
n_py_na     <- sum(is.na(RefWoS_full$PY))
stopifnot(n_out_range == 0, n_py_na == 0)

# ---- 8.2 Cobertura de variables -----------------------------------------------
campos_audit <- c("AU_norm", "KW_norm", "CR_norm_author_year", "CR_norm_obra",
                  "TC", "PY", "SO")
audit_completitud <- sapply(campos_audit, function(col) {
  if (!col %in% names(RefWoS_full)) return(NA_real_)
  n_na <- sum(is.na(RefWoS_full[[col]]) | nchar(trimws(RefWoS_full[[col]])) == 0)
  round(n_na / n_docs * 100, 3)
})
cat("Porcentaje de ausencia por campo (auditoría):\n")
print(audit_completitud)

# ---- 8.3 Impacto y concentración -----------------------------------------------
if ("TC" %in% names(RefWoS_full)) {
  tc <- suppressWarnings(as.numeric(RefWoS_full$TC))
  tc <- tc[!is.na(tc)]
  cat("TC — media:", round(mean(tc), 1),
      "| mediana:", median(tc),
      "| p90:", quantile(tc, 0.9), "\n")
}

# ---- 8.4 Auditoría de KW_norm -----------------------------------------------
kw_tokens <- unlist(str_split(na.omit(RefWoS_full$KW_norm), "; "))
kw_tokens <- kw_tokens[nchar(trimws(kw_tokens)) > 0]
cat("KW_norm: tokens totales =", length(kw_tokens),
    "| únicos =", n_distinct(kw_tokens), "\n")

# ---- 8.5 Auditoría de CR_norm -----------------------------------------------
cr_tokens <- unlist(str_split(na.omit(RefWoS_full$CR_norm_author_year), "; "))
cr_tokens <- cr_tokens[nchar(trimws(cr_tokens)) > 0]
cat("CR_norm_author_year: tokens totales =", length(cr_tokens),
    "| únicos =", n_distinct(cr_tokens), "\n")

# ---- 8.6 Operabilidad técnica -----------------------------------------------
stopifnot(
  is.character(RefWoS_full$AU_norm),
  is.character(RefWoS_full$KW_norm),
  is.character(RefWoS_full$CR_norm_author_year),
  is.character(RefWoS_full$CR_norm_obra)
)
cat("Operabilidad técnica: OK (todos los campos relacionales son character)\n")

# ---- 8.7 Exportar auditoría -----------------------------------------------
overview_df <- data.frame(
  n_docs                  = n_docs,
  yr_min                  = yr_range[1],
  yr_max                  = yr_range[2],
  n_out_range             = n_out_range,
  n_PY_missing            = n_py_na,
  ausencia_AU_norm_pct    = audit_completitud["AU_norm"],
  ausencia_KW_norm_pct    = audit_completitud["KW_norm"],
  ausencia_CR_ay_pct      = audit_completitud["CR_norm_author_year"],
  ausencia_CR_obra_pct    = audit_completitud["CR_norm_obra"],
  kw_tokens_total         = length(kw_tokens),
  kw_tokens_unicos        = n_distinct(kw_tokens),
  cr_tokens_total         = length(cr_tokens),
  cr_tokens_unicos        = n_distinct(cr_tokens)
)
write_csv(overview_df,
          file.path(OUT_CORPUS, "A8_overview_RefWoS_full.csv"))
cat("Auditoría guardada en", file.pat