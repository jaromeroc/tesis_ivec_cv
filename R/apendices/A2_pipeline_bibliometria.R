# =============================================================================
# A2_pipeline_bibliometria.R
# Pipeline reproducible: análisis bibliométrico del corpus RefWoS_full
#
# Tesis: Instrument de Valoración de la Vulnerabilitat Estructural i Contextual (IVEC-CV)
# Autor: Joan A. Romero Crespo
# Corresponde a: Apéndice B (A2-biblio.Rmd)
#
# INSTRUCCIONES DE USO:
#   1. Establecer el directorio de trabajo en la raíz del proyecto.
#   2. Ejecutar A1_pipeline_corpus.R primero para generar RefWoS_full.RData.
#   3. Ejecutar este script ANTES de compilar A2-biblio.Rmd.
#   4. Los mapas VOSviewer (clusters de KW y CR) deben guardarse en
#      outputs/vosviewer/ antes de ejecutar la Fase 5B.
#
# FASES DEL SCRIPT:
#   Fase 1  - Dinámica temporal y periodificación operativa
#   Fase 2  - Segmentación del corpus en subcorpus por periodos
#   Fase 3  - Estructura semántica (redes de coocurrencias KW_norm)
#   Fase 4  - Estructura intelectual (redes de cocitación CR_norm)
#   Fase 5A - Integración por periodos (auditoría + producción + redes)
#   Fase 5B - Integración cruzada KW↔CR (alineamiento tema–tradición)
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(zoo)
library(igraph)
library(readr)
library(tibble)
library(purrr)

# ---- Parámetros globales -----------------------------------------------------

YEAR_MIN   <- 1980L
YEAR_MAX   <- 2024L
N_CUTS     <- 4L
TOP_N      <- 1000L
MIN_FREQ   <- 2L     # apariciones mínimas de una entidad (nodo)
MIN_COOC   <- 3L     # co-presencias mínimas (arista)
SEP        <- ";"

OUT_CORPUS    <- file.path("outputs", "corpus")
OUT_SERIES    <- file.path("outputs", "bibliometria", "series_temporales")
OUT_COOC      <- file.path("outputs", "bibliometria", "coocurrencias")
OUT_COCIT     <- file.path("outputs", "bibliometria", "cocitacion")
OUT_INTEG     <- file.path("outputs", "bibliometria", "integrado")
OUT_INTEG2    <- file.path("outputs", "integracion", "global_1980_2024")
OUT_VOS_COOC  <- file.path("outputs", "vosviewer", "coocurrencias")
OUT_VOS_COCIT <- file.path("outputs", "vosviewer", "cocitacion")

for (d in c(OUT_CORPUS, OUT_SERIES, OUT_COOC, OUT_COCIT, OUT_INTEG, OUT_INTEG2,
            OUT_VOS_COOC, OUT_VOS_COCIT)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---- Cargar corpus -----------------------------------------------------------

load("data/WoS/RefWoS_full.RData")   # → RefWoS_full

M_final <- RefWoS_full %>%
  mutate(
    PY = suppressWarnings(as.integer(PY)),
    TC = suppressWarnings(as.numeric(TC))
  ) %>%
  filter(!is.na(PY), PY >= YEAR_MIN, PY <= YEAR_MAX)

cat("RefWoS_full cargado:", nrow(M_final), "documentos (", YEAR_MIN, "–", YEAR_MAX, ")\n")


# ==============================================================================
# FASE 1. Dinámica temporal y periodificación operativa
# ==============================================================================
# Genera: series_temporales_anual_1980_2024.csv
#          cuts_selected_1980_2024.{rds,csv}
#          subperiodos_con_produccion_y_CPPY_MA3_1980_2024.csv

cat("\n=== FASE 1: Dinámica temporal y periodificación ===\n")

# ---- 1.1 Serie anual ---------------------------------------------------------
pubs_anuales <- M_final %>%
  count(PY, name = "N_publicaciones") %>%
  arrange(PY) %>%
  mutate(
    MA3 = rollmean(N_publicaciones, k = 3, fill = NA, align = "center"),
    pubs_dy      = N_publicaciones - lag(N_publicaciones),
    pubs_dy_pct  = if_else(
      !is.na(lag(N_publicaciones)) & lag(N_publicaciones) > 0,
      100 * (N_publicaciones - lag(N_publicaciones)) / lag(N_publicaciones),
      NA_real_
    )
  )

impacto_anual <- M_final %>%
  mutate(edad = YEAR_MAX - PY + 1L) %>%
  group_by(PY) %>%
  summarise(
    N_docs  = n(),
    TC_sum  = sum(TC, na.rm = TRUE),
    edad    = first(edad),
    expo    = N_docs * edad,
    CPPY    = if_else(expo > 0, TC_sum / expo, NA_real_),
    .groups = "drop"
  )

datos_series <- pubs_anuales %>%
  left_join(impacto_anual, by = "PY") %>%
  arrange(PY) %>%
  mutate(
    cum_pubs = cumsum(coalesce(as.integer(N_publicaciones), 0L)),
    cum_TC   = cumsum(coalesce(as.numeric(TC_sum), 0))
  )

write_csv(datos_series,
          file.path(OUT_SERIES, "series_temporales_anual_1980_2024.csv"))

# ---- 1.2 Selección de cortes operativos (ponderación volumen + CPPY) ---------
MIN_PUBS      <- 20L
ROLL_K_CPPY   <- 5L
MAX_CUT_YEAR  <- 2021L
MIN_GAP       <- 5L
Q_PUBS        <- 0.80
Q_CPPY        <- 0.80
W_PUBS        <- 0.60
W_CPPY        <- 0.40

tmp <- datos_series %>%
  arrange(PY) %>%
  mutate(
    pubs_dy_pct  = if_else(
      !is.na(lag(N_publicaciones)) & lag(N_publicaciones) > 0,
      100 * (N_publicaciones - lag(N_publicaciones)) / lag(N_publicaciones),
      NA_real_
    ),
    cppy_roll_sd = rollapply(CPPY, width = ROLL_K_CPPY, FUN = sd,
                             fill = NA_real_, align = "center", na.rm = TRUE)
  ) %>%
  filter(!is.na(PY), !is.na(N_publicaciones),
         N_publicaciones >= MIN_PUBS, PY <= MAX_CUT_YEAR)

thr_pubs <- quantile(abs(tmp$pubs_dy_pct), probs = Q_PUBS, na.rm = TRUE)
thr_cppy <- quantile(tmp$cppy_roll_sd,     probs = Q_CPPY, na.rm = TRUE)
if (!is.finite(thr_pubs) || thr_pubs <= 0) thr_pubs <- 1
if (!is.finite(thr_cppy) || thr_cppy <= 0) thr_cppy <- 1

tmp <- tmp %>%
  mutate(
    z_pubs = abs(pubs_dy_pct) / thr_pubs,
    z_cppy = if_else(is.na(cppy_roll_sd), 0, cppy_roll_sd / thr_cppy),
    score  = W_PUBS * z_pubs + W_CPPY * z_cppy,
    crit_pubs = abs(pubs_dy_pct) >= thr_pubs,
    crit_cppy = if_else(is.na(cppy_roll_sd), FALSE, cppy_roll_sd >= thr_cppy)
  )

candidatos <- tmp %>%
  filter(crit_pubs | crit_cppy) %>%
  arrange(desc(score))

CUTS_selected <- integer(0)
for (y in candidatos$PY) {
  if (length(CUTS_selected) == 0 || all(abs(y - CUTS_selected) >= MIN_GAP))
    CUTS_selected <- c(CUTS_selected, y)
  if (length(CUTS_selected) >= N_CUTS) break
}
CUTS_selected <- sort(as.integer(CUTS_selected))
stopifnot(length(CUTS_selected) == N_CUTS)

saveRDS(CUTS_selected, file.path(OUT_SERIES, "cuts_selected_1980_2024.rds"))
write_csv(tibble(cut_year = CUTS_selected),
          file.path(OUT_SERIES, "cuts_selected_1980_2024.csv"))

cat("Cortes operativos seleccionados:", paste(CUTS_selected, collapse = ", "), "\n")

# ---- 1.3 Serie con métricas de subperiodo -----------------------------------
breaks_cut    <- c(YEAR_MIN - 1L, CUTS_selected, YEAR_MAX)
labels_periodo <- c("1980_1992","1993_1998","1999_2008","2009_2020","2021_2024")

datos_con_periodo <- datos_series %>%
  mutate(
    periodo = as.character(
      cut(PY, breaks = breaks_cut, include.lowest = TRUE, right = TRUE,
          labels = labels_periodo)
    )
  )

subperiodos_cppy <- datos_con_periodo %>%
  filter(!is.na(periodo)) %>%
  group_by(periodo) %>%
  summarise(
    pubs_periodo = sum(N_publicaciones, na.rm = TRUE),
    TC_periodo   = sum(TC_sum, na.rm = TRUE),
    expo_periodo = sum(expo, na.rm = TRUE),
    MA3_media    = mean(MA3, na.rm = TRUE),
    CPPY         = if_else(expo_periodo > 0, TC_periodo / expo_periodo, NA_real_),
    .groups = "drop"
  )

write_csv(subperiodos_cppy,
          file.path(OUT_SERIES, "subperiodos_con_produccion_y_CPPY_MA3_1980_2024.csv"))
cat("Fase 1 completada.\n")


# ==============================================================================
# FASE 2. Segmentación del corpus en subcorpus por periodos
# ==============================================================================
# Genera: subcorpus_periodos.RData en data/WoS/
#          subperiodos_cortes_operativos_1980_2024.csv

cat("\n=== FASE 2: Segmentación en subcorpus ===\n")

starts <- c(YEAR_MIN, CUTS_selected[-length(CUTS_selected)] + 1L,
            tail(CUTS_selected, 1) + 1L)
ends   <- c(CUTS_selected, YEAR_MAX)

periodos_df <- tibble(inicio = starts, fin = ends) %>%
  mutate(periodo = paste0("P", row_number(), "_", inicio, "_", fin))

M_periodificado <- M_final %>%
  mutate(
    periodo = as.character(
      cut(PY, breaks = breaks_cut, include.lowest = TRUE, right = TRUE,
          labels = periodos_df$periodo)
    )
  ) %>%
  filter(!is.na(periodo))

subcorpus_periodos <- split(M_periodificado, M_periodificado$periodo)

tabla_control <- periodos_df %>%
  mutate(
    n_docs  = as.integer(sapply(periodo, function(p) nrow(subcorpus_periodos[[p]]))),
    periodo = gsub("^P\\d+_", "", periodo)
  )

fila_total <- tabla_control %>%
  summarise(inicio = YEAR_MIN, fin = YEAR_MAX,
            periodo = "TOTAL", n_docs = sum(n_docs))
tabla_export <- bind_rows(tabla_control, fila_total)

write_csv(tabla_export,
          file.path(OUT_SERIES, "subperiodos_cortes_operativos_1980_2024.csv"))
save(subcorpus_periodos, file = "data/WoS/subcorpus_periodos.RData")

cat("Subcorpus creados:", length(subcorpus_periodos), "periodos\n")
cat("Fase 2 completada.\n")


# ==============================================================================
# FASE 3. Estructura semántica (redes de coocurrencias de KW_norm)
# ==============================================================================
# Genera: métricas topológicas por red (global + periodos) en OUT_COOC
#          ficheros VOSviewer (network_*.txt, cluster_*.txt) en OUT_VOS_COOC

cat("\n=== FASE 3: Redes de coocurrencias (KW_norm) ===\n")

# ---- Función: construir red de coocurrencias --------------------------------
build_cooc_network <- function(df, campo = "KW_norm",
                               top_n = TOP_N, min_freq = MIN_FREQ,
                               min_cooc = MIN_COOC, sep = SEP) {

  # Tokenizar
  tokens_list <- str_split(df[[campo]], fixed(sep))
  tokens_list <- lapply(tokens_list, function(x) str_squish(x[nchar(str_squish(x)) > 0]))

  # Frecuencia de entidades
  freq_tab <- sort(table(unlist(tokens_list)), decreasing = TRUE)
  vocab    <- names(freq_tab)[freq_tab >= min_freq]
  vocab    <- head(vocab, top_n)

  if (length(vocab) < 2) return(NULL)

  # Matriz de coocurrencias
  cooc_mat <- matrix(0L, nrow = length(vocab), ncol = length(vocab),
                     dimnames = list(vocab, vocab))
  for (tks in tokens_list) {
    tks_in <- intersect(tks, vocab)
    if (length(tks_in) < 2) next
    for (i in seq_along(tks_in)) {
      for (j in seq_along(tks_in)) {
        if (i >= j) next
        cooc_mat[tks_in[i], tks_in[j]] <- cooc_mat[tks_in[i], tks_in[j]] + 1L
        cooc_mat[tks_in[j], tks_in[i]] <- cooc_mat[tks_in[j], tks_in[i]] + 1L
      }
    }
  }

  # Filtrar por min_cooc
  cooc_mat[cooc_mat < min_cooc] <- 0L

  # Crear grafo igraph
  g <- graph_from_adjacency_matrix(cooc_mat, mode = "undirected",
                                   weighted = TRUE, diag = FALSE)
  g <- simplify(g)
  g <- induced_subgraph(g, V(g)[degree(g) > 0])

  list(
    g        = g,
    freq_tab = freq_tab[vocab]
  )
}

# ---- Función: calcular métricas topológicas ---------------------------------
compute_topo_metrics <- function(g) {
  if (is.null(g) || vcount(g) == 0) return(NULL)
  m         <- ecount(g)
  n         <- vcount(g)
  densidad  <- if (n > 1) 2 * m / (n * (n - 1)) else NA_real_
  grado_med <- if (n > 0) mean(degree(g)) else NA_real_
  clust     <- transitivity(g, type = "global")
  mod_try   <- tryCatch(
    modularity(cluster_louvain(g)),
    error = function(e) NA_real_
  )
  tibble(nodos = n, aristas = m, densidad = densidad,
         grado_medio = grado_med, clustering = clust, modularidad = mod_try)
}

# ---- Función: exportar VOSviewer --------------------------------------------
export_vosviewer <- function(g, freq_tab, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # network file (id, label, weight<Links>)
  vnames <- V(g)$name
  net_df <- tibble(
    id     = seq_along(vnames),
    label  = vnames,
    weight = as.integer(freq_tab[vnames])
  )
  edges   <- igraph::as_data_frame(g, what = "edges") %>%
    mutate(from_id = match(from, vnames), to_id = match(to, vnames)) %>%
    select(from_id, to_id, weight)
  write_tsv(net_df,  file.path(out_dir, "network.txt"))
  write_tsv(edges,   file.path(out_dir, "edges.txt"))
}

# ---- Red global -------------------------------------------------------------
res_global <- build_cooc_network(M_final)
if (!is.null(res_global)) {
  m_global <- compute_topo_metrics(res_global$g) %>%
    mutate(periodo = "TOTAL", campo = "KW_norm")
  export_vosviewer(res_global$g, res_global$freq_tab,
                   file.path(OUT_VOS_COOC, "global_1980_2024"))
} else {
  m_global <- tibble()
}

# ---- Redes por periodo -------------------------------------------------------
metricas_cooc_list <- list(m_global)
for (nm in names(subcorpus_periodos)) {
  sub  <- subcorpus_periodos[[nm]]
  res  <- build_cooc_network(sub)
  if (!is.null(res)) {
    m_p <- compute_topo_metrics(res$g) %>%
      mutate(periodo = gsub("^P\\d+_", "", nm), campo = "KW_norm")
    export_vosviewer(res$g, res$freq_tab,
                     file.path(OUT_VOS_COOC, nm))
    metricas_cooc_list <- c(metricas_cooc_list, list(m_p))
  }
  cat("  Coocurrencias -", nm, ": OK\n")
}

metricas_cooc <- bind_rows(metricas_cooc_list) %>%
  rename(nodos_cooc     = nodos,
         aristas_cooc   = aristas,
         densidad_cooc  = densidad,
         grado_medio_cooc = grado_medio,
         clustering_cooc  = clustering,
         modularidad_cooc = modularidad)

write_csv(metricas_cooc, file.path(OUT_COOC, "metrics_all_cooc_kw_norm.csv"))
cat("Métricas coocurrencias guardadas en", file.path(OUT_COOC, "metrics_all_cooc_kw_norm.csv"), "\n")
cat("Fase 3 completada.\n")


# ==============================================================================
# FASE 4. Estructura intelectual (redes de cocitación CR_norm)
# ==============================================================================
# Genera: métricas topológicas por red (global + periodos) en OUT_COCIT
#          ficheros VOSviewer en OUT_VOS_COCIT

cat("\n=== FASE 4: Redes de cocitación (CR_norm_author_year) ===\n")

# Reutilizamos build_cooc_network con campo = "CR_norm_author_year"
# (la cocitación es co-presencia de referencias en un mismo documento)

res_cocit_global <- build_cooc_network(M_final, campo = "CR_norm_author_year")
if (!is.null(res_cocit_global)) {
  m_cocit_global <- compute_topo_metrics(res_cocit_global$g) %>%
    mutate(periodo = "TOTAL", campo = "CR_norm_author_year")
  export_vosviewer(res_cocit_global$g, res_cocit_global$freq_tab,
                   file.path(OUT_VOS_COCIT, "global_1980_2024"))
} else {
  m_cocit_global <- tibble()
}

metricas_cocit_list <- list(m_cocit_global)
for (nm in names(subcorpus_periodos)) {
  sub  <- subcorpus_periodos[[nm]]
  res  <- build_cooc_network(sub, campo = "CR_norm_author_year")
  if (!is.null(res)) {
    m_p <- compute_topo_metrics(res$g) %>%
      mutate(periodo = gsub("^P\\d+_", "", nm), campo = "CR_norm_author_year")
    export_vosviewer(res$g, res$freq_tab,
                     file.path(OUT_VOS_COCIT, nm))
    metricas_cocit_list <- c(metricas_cocit_list, list(m_p))
  }
  cat("  Cocitación -", nm, ": OK\n")
}

metricas_cocit <- bind_rows(metricas_cocit_list) %>%
  rename(nodos_cocit_autores       = nodos,
         aristas_cocit_autores     = aristas,
         densidad_cocit_autores    = densidad,
         grado_medio_cocit_autores = grado_medio,
         clustering_cocit_autores  = clustering,
         modularidad_cocit_autores = modularidad)

write_csv(metricas_cocit, file.path(OUT_COCIT, "metrics_all_cocit_cr_norm.csv"))
cat("Métricas cocitación guardadas en", file.path(OUT_COCIT, "metrics_all_cocit_cr_norm.csv"), "\n")
cat("Fase 4 completada.\n")


# ==============================================================================
# FASE 5A. Integración por periodos
# ==============================================================================

cat("\n=== FASE 5A: Integración por periodos ===\n")

# ---- 5A.1 Auditoría periodificada -------------------------------------------

orden_periodos <- c("1980_1992","1993_1998","1999_2008","2009_2020","2021_2024","TOTAL")

compute_audit_periodo <- function(df, periodo_label) {
  tc    <- suppressWarnings(as.numeric(df$TC))
  n     <- nrow(df)

  # Edad media de documentos
  edad_media_doc <- mean(YEAR_MAX - df$PY + 1, na.rm = TRUE)

  # Refs por documento
  refs_por_doc <- mean(
    sapply(df$CR_norm_author_year,
           function(x) if (is.na(x) || nchar(trimws(x)) == 0) 0L
                       else length(str_split(x, "; ?")[[1]])),
    na.rm = TRUE
  )

  # KW tokens por documento
  kw_por_doc <- mean(
    sapply(df$KW_norm,
           function(x) if (is.na(x) || nchar(trimws(x)) == 0) 0L
                       else length(str_split(x, "; ?")[[1]])),
    na.rm = TRUE
  )

  # Gini de citas
  tc_clean <- tc[!is.na(tc)]
  gini_citas <- if (length(tc_clean) > 1) {
    n2 <- length(tc_clean)
    tc_sorted <- sort(tc_clean)
    2 * sum((seq_along(tc_sorted) * tc_sorted)) / (n2 * sum(tc_sorted)) - (n2 + 1) / n2
  } else NA_real_

  # Edad de referencias
  ref_ages <- unlist(lapply(seq_len(nrow(df)), function(i) {
    py  <- df$PY[i]
    cr  <- df$CR_norm_author_year[i]
    if (is.na(py) || is.na(cr) || nchar(trimws(cr)) == 0) return(NULL)
    refs <- str_split(cr, "; ?")[[1]]
    years <- suppressWarnings(as.integer(str_extract(refs, "\\d{4}")))
    years <- years[!is.na(years) & years >= 1900 & years <= YEAR_MAX]
    py - years
  }))

  tibble(
    periodo              = periodo_label,
    n_docs               = n,
    edad_media_doc       = round(edad_media_doc, 2),
    refs_por_doc_media   = round(refs_por_doc, 2),
    kw_tokens_por_doc_media = round(kw_por_doc, 2),
    gini_citas           = round(gini_citas, 4),
    edad_media_referencias = round(mean(ref_ages, na.rm = TRUE), 2),
    ref_age_mediana      = round(median(ref_ages, na.rm = TRUE), 2),
    ref_age_p90          = round(quantile(ref_ages, 0.9, na.rm = TRUE), 2)
  )
}

audit_list <- lapply(names(subcorpus_periodos), function(nm) {
  lbl <- gsub("^P\\d+_", "", nm)
  compute_audit_periodo(subcorpus_periodos[[nm]], lbl)
})
audit_total <- compute_audit_periodo(M_final, "TOTAL")
audit_df    <- bind_rows(c(audit_list, list(audit_total)))

write_csv(audit_df,
          file.path(OUT_CORPUS, "A8_overview_RefWoS_full_periodificada.csv"))
cat("Auditoría periodificada guardada.\n")

# ---- 5A.2 Integración de las cuatro fuentes ----------------------------------

f_aud      <- file.path(OUT_CORPUS, "A8_overview_RefWoS_full_periodificada.csv")
f_temporal <- file.path(OUT_SERIES, "subperiodos_con_produccion_y_CPPY_MA3_1980_2024.csv")
f_cooc     <- file.path(OUT_COOC,   "metrics_all_cooc_kw_norm.csv")
f_coci     <- file.path(OUT_COCIT,  "metrics_all_cocit_cr_norm.csv")

stopifnot(file.exists(f_aud), file.exists(f_temporal),
          file.exists(f_cooc), file.exists(f_coci))

norm_periodo <- function(x) x %>% str_trim() %>%
  str_replace_all("–", "_") %>% str_replace_all("-", "_")

aud      <- read_csv(f_aud,      show_col_types = FALSE) %>%
  mutate(periodo = norm_periodo(periodo))
temporal <- read_csv(f_temporal, show_col_types = FALSE) %>%
  mutate(periodo = norm_periodo(periodo))
cooc     <- read_csv(f_cooc,     show_col_types = FALSE) %>%
  mutate(periodo = norm_periodo(periodo))
coci     <- read_csv(f_coci,     show_col_types = FALSE) %>%
  mutate(periodo = norm_periodo(periodo))

tabla_integrada_v2 <- aud %>%
  full_join(temporal, by = "periodo") %>%
  full_join(select(cooc, -campo), by = "periodo") %>%
  full_join(select(coci, -campo), by = "periodo") %>%
  mutate(periodo = factor(periodo, levels = union(orden_periodos, unique(periodo)))) %>%
  arrange(periodo)

dir.create(OUT_INTEG, recursive = TRUE, showWarnings = FALSE)
write_csv2(tabla_integrada_v2,
           file.path(OUT_INTEG, "tabla_integrada_4_fuentes_1980_2024_v2.csv"))
cat("Tabla integrada guardada en", OUT_INTEG, "\n")
cat("Fase 5A completada.\n")


# ==============================================================================
# FASE 5B. Integración cruzada KW↔CR (alineamiento tema–tradición)
# ==============================================================================
# PRERREQUISITO: Los mapas VOSviewer con asignaciones de cluster deben estar
# disponibles en outputs/vosviewer/coocurrencias/ y
# outputs/vosviewer/cocitacion/ (ficheros cluster_*.txt o map_*.txt).
#
# Esta fase construye la matriz W_ij de acoplamiento documental KW×CR y
# deriva distribuciones condicionales, entropías e Información Mutua.

cat("\n=== FASE 5B: Integración cruzada KW↔CR ===\n")

# ---- 5B.1 Carga de mapas de cluster (VOSviewer) -------------------------------
# Carga un mapa de cluster exportado desde VOSviewer.
# Admite dos formatos:
#   - TSV (.txt) export directo de VOSviewer: columnas id, x, y, cluster, weight<Links>, ...
#     (puede llevar BOM UTF-8 y labels entrecomillados, ej. "ADGER, 2006")
#   - CSV con columnas label, cluster
# Devuelve un tibble con columnas 'label' y 'cluster'.
load_vos_map <- function(path) {
  if (!file.exists(path)) {
    warning("Fichero de mapa VOSviewer no encontrado: ", path)
    return(NULL)
  }
  # read_tsv maneja BOM UTF-8 y campos entrecomillados automáticamente
  df <- if (grepl("\\.txt$", path, ignore.case = TRUE)) {
    read_tsv(path, show_col_types = FALSE, name_repair = "minimal")
  } else {
    read_csv(path, show_col_types = FALSE, name_repair = "minimal")
  }
  # Limpiar posible BOM residual en nombres de columna
  names(df) <- str_replace_all(names(df), "^﻿", "")
  # VOSviewer usa 'id' como etiqueta del nodo; normalizar a 'label'
  if ("id" %in% names(df) && !"label" %in% names(df)) {
    df <- df %>% rename(label = id)
  }
  if (!all(c("label", "cluster") %in% names(df))) {
    warning("El fichero ", path, " no contiene columnas 'label'/'id' y 'cluster'.")
    return(NULL)
  }
  df %>%
    select(label, cluster) %>%
    mutate(label   = str_squish(toupper(as.character(label))),
           cluster = as.integer(cluster))
}

# Rutas de los mapas de cluster exportados desde VOSviewer
# (fichero .txt TAB-separado con columnas: id, x, y, cluster, weight<Links>, ...)
kw_map_path <- file.path(OUT_VOS_COOC, "global_1980_2024", "cooc_kw_map.txt")
cr_map_path <- file.path(OUT_VOS_COCIT, "global_1980_2024", "autores", "cocit_cr_map.txt")

kw_map2 <- load_vos_map(kw_map_path)
cr_map2 <- load_vos_map(cr_map_path)

if (is.null(kw_map2) || is.null(cr_map2)) {
  cat("AVISO: Mapas VOSviewer no disponibles. Fase 5B omitida.\n")
  cat("       KW esperado en:", kw_map_path, "\n")
  cat("       CR esperado en:", cr_map_path, "\n")
} else {

  kw_clusters <- kw_map2 %>% distinct(label, cluster) %>%
    rename(kw_label = label, kw_cluster = cluster)
  cr_clusters <- cr_map2 %>% distinct(label, cluster) %>%
    rename(cr_label = label, cr_cluster = cluster)

  # ---- 5B.2 Construcción de la matriz W (normalización fraccional) -----------
  df0 <- M_final %>%
    mutate(doc_id = row_number())

  # Expansión de tokens KW
  kw_expanded <- df0 %>%
    select(doc_id, KW_norm) %>%
    filter(!is.na(KW_norm)) %>%
    mutate(token = str_split(KW_norm, "; ?")) %>%
    unnest(token) %>%
    mutate(token = str_squish(toupper(token))) %>%
    filter(nchar(token) > 0) %>%
    left_join(kw_clusters, by = c("token" = "kw_label")) %>%
    filter(!is.na(kw_cluster))

  # Expansión de tokens CR
  cr_expanded <- df0 %>%
    select(doc_id, CR_norm_author_year) %>%
    filter(!is.na(CR_norm_author_year)) %>%
    mutate(token = str_split(CR_norm_author_year, "; ?")) %>%
    unnest(token) %>%
    mutate(token = str_squish(toupper(token))) %>%
    filter(nchar(token) > 0) %>%
    left_join(cr_clusters, by = c("token" = "cr_label")) %>%
    filter(!is.na(cr_cluster))

  # Contribución fraccional por documento
  kw_frac <- kw_expanded %>%
    group_by(doc_id) %>%
    mutate(n_kw_total = n()) %>%
    ungroup() %>%
    group_by(doc_id, kw_cluster) %>%
    summarise(c_di = n() / first(n_kw_total), .groups = "drop")

  cr_frac <- cr_expanded %>%
    group_by(doc_id) %>%
    mutate(n_cr_total = n()) %>%
    ungroup() %>%
    group_by(doc_id, cr_cluster) %>%
    summarise(r_dj = n() / first(n_cr_total), .groups = "drop")

  # Cobertura
  docs_with_kw  <- unique(kw_expanded$doc_id)
  docs_with_cr  <- unique(cr_expanded$doc_id)
  docs_with_both <- intersect(docs_with_kw, docs_with_cr)
  n_total <- nrow(df0)

  coverage_df <- tibble(
    n_docs_total    = n_total,
    n_docs_kw       = length(docs_with_kw),
    n_docs_cr       = length(docs_with_cr),
    n_docs_both     = length(docs_with_both),
    share_kw        = length(docs_with_kw)  / n_total,
    share_cr        = length(docs_with_cr)  / n_total,
    share_both      = length(docs_with_both) / n_total
  )

  # Token match rates
  all_kw_tokens <- unlist(str_split(na.omit(df0$KW_norm), "; ?"))
  all_kw_tokens <- str_squish(toupper(all_kw_tokens[nchar(str_squish(all_kw_tokens)) > 0]))
  all_cr_tokens <- unlist(str_split(na.omit(df0$CR_norm_author_year), "; ?"))
  all_cr_tokens <- str_squish(toupper(all_cr_tokens[nchar(str_squish(all_cr_tokens)) > 0]))

  coverage_df <- coverage_df %>%
    mutate(
      kw_token_match_rate = mean(all_kw_tokens %in% kw_clusters$kw_label),
      cr_token_match_rate = mean(all_cr_tokens %in% cr_clusters$cr_label)
    )

  # Matriz W
  W_5b <- kw_frac %>%
    inner_join(cr_frac, by = "doc_id") %>%
    group_by(kw_cluster, cr_cluster) %>%
    summarise(w_ij = sum(c_di * r_dj), .groups = "drop") %>%
    left_join(
      kw_clusters %>% distinct(kw_cluster, kw_label) %>%
        group_by(kw_cluster) %>% slice(1),
      by = "kw_cluster"
    ) %>%
    left_join(
      cr_clusters %>% distinct(cr_cluster, cr_label) %>%
        group_by(cr_cluster) %>% slice(1),
      by = "cr_cluster"
    )

  # ---- 5B.3 Distribuciones condicionales y entropías -------------------------
  kw_totals <- W_5b %>%
    group_by(kw_cluster) %>%
    summarise(w_kw_total = sum(w_ij))

  W_cond <- W_5b %>%
    left_join(kw_totals, by = "kw_cluster") %>%
    mutate(p_cr_given_kw = w_ij / w_kw_total)

  entropy_by_kw <- W_cond %>%
    group_by(kw_cluster, kw_label) %>%
    summarise(
      H        = -sum(p_cr_given_kw * log2(pmax(p_cr_given_kw, 1e-12))),
      n_cr     = n(),
      H_norm   = if_else(n_cr > 1, H / log2(n_cr), 0),
      K_eff    = 2^H,
      dominant = max(p_cr_given_kw),
      .groups = "drop"
    )

  # Información Mutua global
  w_total <- sum(W_5b$w_ij)
  p_kw    <- W_5b %>% group_by(kw_cluster) %>% summarise(p_kw = sum(w_ij) / w_total)
  p_cr    <- W_5b %>% group_by(cr_cluster) %>% summarise(p_cr = sum(w_ij) / w_total)

  MI_df <- W_5b %>%
    mutate(p_joint = w_ij / w_total) %>%
    left_join(p_kw, by = "kw_cluster") %>%
    left_join(p_cr, by = "cr_cluster") %>%
    filter(p_joint > 0, p_kw > 0, p_cr > 0) %>%
    summarise(MI_bits = sum(p_joint * log2(p_joint / (p_kw * p_cr))))

  H_kw  <- -sum(p_kw$p_kw * log2(pmax(p_kw$p_kw, 1e-12)))
  H_cr  <- -sum(p_cr$p_cr * log2(pmax(p_cr$p_cr, 1e-12)))
  MI    <- MI_df$MI_bits

  summary_5b <- coverage_df %>%
    mutate(
      MI_bits  = MI,
      NMI_min  = MI / max(H_kw, H_cr),
      NMI_sqrt = MI / sqrt(H_kw * H_cr)
    )

  # ---- Guardar outputs -------------------------------------------------------
  dir.create(OUT_INTEG2, recursive = TRUE, showWarnings = FALSE)
  write_csv(summary_5b,    file.path(OUT_INTEG2, "summary_5b_global.csv"))
  write_csv(W_5b,          file.path(OUT_INTEG2, "W_matrix_5b_global.csv"))
  write_csv(W_cond,        file.path(OUT_INTEG2, "W_conditional_5b_global.csv"))
  write_csv(entropy_by_kw, file.path(OUT_INTEG2, "entropy_by_kw_5b_global.csv"))
  save(W_5b, W_cond, entropy_by_kw, summary_5b,
       kw_clusters, cr_clusters, kw_map2, cr_map2,
       file = file.path(OUT_INTEG2, "outputs_5b_global.RData"))

  # ---- Exportar con los nombres y columnas que espera A2-biblio.Rmd -----------
  # chunk app2-load-acoplamiento carga:
  #   W_long.csv               → W_labeled / W_5b  (columnas: kw_cluster, cr_cluster, w_ij, kw_label, cr_label)
  #   p_cr_given_kw_long.csv   → p_cond             (columnas: kw_cluster, cr_cluster, p)
  write_csv(W_5b, file.path(OUT_INTEG2, "W_long.csv"))
  p_cond_export <- W_cond %>%
    select(kw_cluster, cr_cluster, p = p_cr_given_kw)
  write_csv(p_cond_export, file.path(OUT_INTEG2, "p_cr_given_kw_long.csv"))
  cat("Archivos para A2-biblio.Rmd guardados (W_long.csv, p_cr_given_kw_long.csv).
")

  cat("MI =", round(MI, 4), "bits | NMI_min =",
      round(MI / max(H_kw, H_cr), 4), "\n")
  cat("Outputs Fase 5B guardados en", OUT_INTEG2, "\n")
  cat("Fase 5B completada.\n")

  # ============================================================================
  # FIGURAS DE VISUALIZACIÓN (formato base R/ggplot2)
  # ============================================================================
  # Este bloque genera las figuras del Apéndice B:
  #   • bubble_kw_cr_1980_2024.{pdf,png}   — mapa de burbujas KW × CR
  #   • alluvial_kw_cr_1980_2024.{pdf,png} — diagrama alluvial KW → CR
  #
  # Las figuras se guardan en outputs/figures/app-b/ y son cargadas en el Knit
  # mediante include_graphics (A2-biblio.Rmd y 02-estado-cuestion.Rmd).
  #
  # VERSIONES MEJORADAS (tipografía, etiquetas sin solapamiento, leyenda limpia):
  # Ejecutar a continuación R/apendices/A2_figuras_python.py, que genera los
  # mismos ficheros con matplotlib y los pisa. El Knit cargará entonces las
  # versiones Python sin necesidad de cambiar ninguna ruta en los Rmd.
  #   Uso:  python R/apendices/A2_figuras_python.py
  # ============================================================================

  library(ggplot2)
  library(ggalluvial)
  library(stringr)
  library(dplyr)

  out_fig_dir <- file.path("outputs", "figures", "app-b")
  dir.create(out_fig_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Bubble chart KW × CR ────────────────────────────────────────────────────
  plot_data_fig <- W_5b %>%
    left_join(W_cond %>% select(kw_cluster, cr_cluster, p),
              by = c("kw_cluster", "cr_cluster")) %>%
    mutate(
      kw_label      = enc2utf8(kw_label),
      cr_label      = enc2utf8(cr_label),
      kw_label_wrap = str_wrap(kw_label, width = 26),
      cr_label_wrap = str_wrap(cr_label, width = 12)
    )

  p_bubble_fig <- ggplot(plot_data_fig,
                         aes(x = factor(cr_cluster), y = factor(kw_cluster))) +
    geom_point(aes(size = w_ij, color = p), alpha = 0.85) +
    scale_y_discrete(
      breaks = sort(unique(plot_data_fig$kw_cluster)),
      labels = plot_data_fig %>%
        distinct(kw_cluster, kw_label_wrap) %>%
        arrange(kw_cluster) %>%
        pull(kw_label_wrap)
    ) +
    scale_x_discrete(
      breaks = sort(unique(plot_data_fig$cr_cluster)),
      labels = plot_data_fig %>%
        distinct(cr_cluster, cr_label_wrap) %>%
        arrange(cr_cluster) %>%
        pull(cr_label_wrap)
    ) +
    scale_size_continuous(range = c(1.5, 12), name = "Volumen (w_ij)") +
    scale_color_viridis_c(option = "magma", direction = -1,
                          name = "Alineamiento (p)") +
    labs(title = "", x = "Polos intelectuales (CR)",
         y = "Áreas temáticas (KW)") +
    theme_bw() +
    theme(
      plot.title      = element_text(size = 13, face = "bold"),
      axis.title.x    = element_text(size = 13, margin = margin(t = 12)),
      axis.title.y    = element_text(size = 13, margin = margin(r = 12)),
      axis.text.y     = element_text(size = 12, lineheight = 0.95, hjust = 1),
      axis.text.x     = element_text(size = 12, angle = 0, hjust = 0.5,
                                     vjust = 1, lineheight = 0.95),
      legend.text     = element_text(size = 12),
      legend.title    = element_text(size = 13),
      legend.position = "bottom",
      legend.box      = "vertical",
      plot.margin     = margin(10, 10, 10, 24, "pt")
    )

  ggsave(file.path(out_fig_dir, "bubble_kw_cr_1980_2024.pdf"),
         plot = p_bubble_fig, width = 16, height = 12,
         units = "in", device = cairo_pdf)
  ggsave(file.path(out_fig_dir, "bubble_kw_cr_1980_2024.png"),
         plot = p_bubble_fig, width = 16, height = 12,
         units = "in", dpi = 600)

  # ── Alluvial KW → CR ────────────────────────────────────────────────────────
  df_alluvial_fig <- W_5b %>%
    filter(w_ij > quantile(w_ij, 0.25, na.rm = TRUE)) %>%
    mutate(
      kw_label_wrap = str_wrap(enc2utf8(kw_label), width = 26),
      cr_label_wrap = str_wrap(enc2utf8(cr_label), width = 26)
    )

  p_alluvial_fig <- ggplot(df_alluvial_fig,
         aes(y = w_ij, axis1 = kw_label_wrap, axis2 = cr_label_wrap)) +
    geom_alluvium(aes(fill = factor(kw_cluster)), width = 1/18, alpha = 0.65) +
    geom_stratum(width = 1/10, fill = "grey92", color = "grey35") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)),
              size = 2.2, lineheight = 0.85) +
    scale_fill_viridis_d(option = "turbo", guide = "none") +
    labs(y = "Peso del acoplamiento (w_ij)") +
    scale_x_discrete(limits = c("Área temática (KW)", "Polo intelectual (CR)"),
                     expand = c(0.05, 0.05)) +
    theme_minimal() +
    theme(
      panel.grid   = element_blank(),
      axis.text.y  = element_blank(),
      axis.title.x = element_blank(),
      plot.margin  = margin(6, 28, 6, 28, "pt")
    )

  ggsave(file.path(out_fig_dir, "alluvial_kw_cr_1980_2024.pdf"),
         plot = p_alluvial_fig, width = 10, height = 10,
         units = "in", device = cairo_pdf)
  ggsave(file.path(out_fig_dir, "alluvial_kw_cr_1980_2024.png"),
         plot = p_alluvial_fig, width = 10, height = 10,
         units = "in", dpi = 600)

  cat("Figuras (formato R) guardadas en", out_fig_dir, "\n")
  cat("Para versiones mejoradas ejecutar: python R/apendices/A2_figuras_python.py\n")
}
