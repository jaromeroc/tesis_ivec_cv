# =============================================================================
# A4_pipeline_decisiones_metodologicas.R — CÓDIGO REPRODUCIBLE DEL APÉNDICE D
# =============================================================================
# Reúne el código reproducible de las decisiones metodológicas del Apéndice D
# que requieren cómputo sobre microdatos del SIP o sobre el panel del módulo
# base:
#
#   • §D.3 — Análisis de sesgo en la georreferenciación del SIP: cuatro cruces
#            de contingencia (no georreferenciado × D7 / D3_migr / D5_aseg_subgr
#            / D9_raf_renta) con prueba chi-cuadrado y V de Cramér.
#   • §D.4 — Coeficientes de correlación inter-indicador y coherencia interna
#            del IVEC_base (matrices de Spearman + alfa de Cronbach) y
#            validación por ACP del esquema de ponderación equiponderada.
#
# Los resultados se reportan en la tesis: el perfil del dato perdido en
# cap. 5 §sec-perfil-dato-perdido, y la coherencia interna (alfa de Cronbach y
# omega de McDonald por dimensión) en cap. 6 §sec-res-coherencia. Este script
# es el registro reproducible del procedimiento, en paralelo al pipeline
# R/ivec/01_ivec_base.R —que produce `validacion_d7.rds` con la tabla de
# coherencia interna— y al tratamiento del SIP en R/apendices/A3_pipeline_sip.R.
#
# Requiere los microdatos del SIP (SIP_final.parquet) para §D.3 y el panel de
# celdas del módulo base (ivec_base.parquet) para §D.4; ambos sujetos al
# protocolo de acceso restringido documentado en cap. 5 (marco ético).
# =============================================================================


# =============================================================================
# §D.3 — ANÁLISIS DE SESGO EN LA GEORREFERENCIACIÓN DEL SIP
# =============================================================================
# Objetivo: evaluar si los individuos sin coordenadas válidas (no georref.)
# presentan un perfil de vulnerabilidad sistemáticamente distinto.
# Variables cruzadas: estado_georref × D7 / D3_migr / D5_aseg_subgr / D9_raf_renta
# =============================================================================

library(tidyverse)
library(arrow)

# ── Función auxiliar: tabla de contingencia + chi-cuadrado + V de Cramér ──────
tabla_sesgo <- function(df, var_fila, etiqueta_fila) {
  df_clean <- df %>%
    filter(!is.na(.data[[var_fila]])) %>%
    mutate(
      georref = factor(
        ifelse(coord_valida, "Georreferenciado", "No georreferenciado"),
        levels = c("Georreferenciado", "No georreferenciado")
      )
    )

  tbl <- table(df_clean[[var_fila]], df_clean$georref)

  # Guard: si la tabla es degenerada (sin filas válidas, una sola categoría,
  # columna vacía o todo ceros), no se puede calcular el chi-cuadrado.
  # Suele indicar que la codificación de la variable no coincide con el mapeo.
  if (nrow(tbl) < 2 || ncol(tbl) < 2 || sum(tbl) == 0 ||
      any(rowSums(tbl) == 0) || any(colSums(tbl) == 0)) {
    message("  [aviso] tabla degenerada para '", var_fila,
            "': revisa la codificación de la variable. Se omite el chi-cuadrado.")
    return(list(
      etiqueta = etiqueta_fila, tabla_rel = NA, chi2 = NA_real_,
      df = NA_integer_, p_valor = NA_real_, cramer_v = NA_real_,
      n_total = sum(tbl)
    ))
  }

  chi_res  <- chisq.test(tbl, correct = FALSE)
  cramer_v <- sqrt(chi_res$statistic / (sum(tbl) * (min(dim(tbl)) - 1)))

  list(
    etiqueta  = etiqueta_fila,
    tabla_rel = prop.table(tbl, margin = 2) %>% round(4),
    chi2      = round(chi_res$statistic, 1),
    df        = chi_res$parameter,
    p_valor   = signif(chi_res$p.value, 3),
    cramer_v  = round(cramer_v, 3),
    n_total   = sum(tbl)
  )
}

# ── 1. Carga del SIP y construcción del indicador de georreferenciación ───────
# Columnas con el sufijo (cod) del diccionario APSIG del SIP.
sip_activo <- arrow::read_parquet(
  here::here("data/SIP/results/SIP_final.parquet"),
  col_select = c("D2_resid(cod)", "D7_vulner_apsig(cod)", "D3_migr(cod)",
                 "D5_aseg_subgr(cod)", "D9_raf_renta(cod)", "ST_X", "ST_Y")
) %>%
  rename(D7 = `D7_vulner_apsig(cod)`, D3_migr = `D3_migr(cod)`,
         D5_aseg_subgr = `D5_aseg_subgr(cod)`, D9_raf_renta = `D9_raf_renta(cod)`) %>%
  filter(`D2_resid(cod)` == "1")

# Bbox canónico de la CV en ETRS89 UTM30N: X > 620000 (no 695000) para incluir
# el interior de Valencia (Rincón de Ademuz, Los Serranos, Serranía).
sip_georref <- sip_activo %>%
  mutate(
    coord_valida = !is.na(ST_X) & !is.na(ST_Y) &
      ST_X > 620000 & ST_X < 900000 &
      ST_Y > 4175000 & ST_Y < 4550000
  )

message("Total SIP activo: ",     nrow(sip_georref))
message("Georreferenciados: ",    sum(sip_georref$coord_valida))
message("No georreferenciados: ", sum(!sip_georref$coord_valida))

# ── 2. Cruce con D7 (clasificación de vulnerabilidad APSIG) ───────────────────
res_d7 <- tabla_sesgo(sip_georref, "D7", "Vulnerabilidad APSIG (D7)")

# ── 3. Cruce con D3_migr (situación migratoria) ───────────────────────────────
sip_georref <- sip_georref %>%
  mutate(d3_migr_grp = case_when(
    D3_migr == "10"            ~ "Nacido en España",
    D3_migr %in% c("31","32","33") ~ "Extranjero/interno antiguo",
    D3_migr == "23"            ~ "Extranjero 2-5 años",
    D3_migr == "22"            ~ "Extranjero 1-2 años",
    D3_migr == "21"            ~ "Extranjero <1 año",
    TRUE                       ~ NA_character_
  ))
res_d3 <- tabla_sesgo(sip_georref, "d3_migr_grp", "Situación migratoria (D3)")

# ── 4. Cruce con D5_aseg_subgr (subgrupo de aseguramiento) ────────────────────
sip_georref <- sip_georref %>%
  mutate(d5_macrogrp = case_when(
    str_starts(D5_aseg_subgr, "A") ~ "Grupo A (contributivo)",
    str_starts(D5_aseg_subgr, "B") ~ "Grupo B (subsidiario)",
    str_starts(D5_aseg_subgr, "C") ~ "Grupo C (no contributivo)",
    TRUE                            ~ NA_character_
  ))
res_d5 <- tabla_sesgo(sip_georref, "d5_macrogrp", "Aseguramiento (D5)")

# ── 5. Cruce con D9_raf_renta (nivel de renta vía aportación farmacéutica) ────
# Los códigos de D9_raf_renta son los tramos del régimen de aportación
# farmacéutica (RDL 16/2012), de dos dígitos ---no una escala 1-7---. La
# agrupación se alinea con la interpretación canónica del renta_map de
# R/ivec/01_ivec_base.R (a menor copago → menor renta → mayor vulnerabilidad).
# Códigos observados en el SIP (extracción jun-2025): 10/20/21/22 (pensionista
# exento o de renta baja), 63/64/65 (no asegurado, sin acreditar), 30/40
# (activo de renta media), 50/53/60 (renta alta). El código 41 (n = 1) es
# residual y queda como NA.
message("Códigos observados en D9_raf_renta (frecuencia):")
print(table(sip_georref$D9_raf_renta, useNA = "ifany"))

sip_georref <- sip_georref %>%
  mutate(d9_renta_grp = case_when(
    D9_raf_renta %in% c("10", "20", "21", "22") ~ "Pensionista exento o renta baja",
    D9_raf_renta %in% c("63", "64", "65")       ~ "No asegurado (sin acreditar)",
    D9_raf_renta %in% c("30", "40")             ~ "Activo de renta media",
    D9_raf_renta %in% c("50", "53", "60")       ~ "Renta alta",
    TRUE                                         ~ NA_character_
  ))
res_d9 <- tabla_sesgo(sip_georref, "d9_renta_grp", "Nivel de renta (D9)")

# ── 6. Tabla resumen y guardado del rds para el Apéndice D §D.3 ───────────────
resumen_sesgo <- tibble(
  Variable  = c(res_d7$etiqueta, res_d3$etiqueta, res_d5$etiqueta, res_d9$etiqueta),
  Chi2      = c(res_d7$chi2,     res_d3$chi2,     res_d5$chi2,     res_d9$chi2),
  GL        = c(res_d7$df,       res_d3$df,        res_d5$df,       res_d9$df),
  p_valor   = c(res_d7$p_valor,  res_d3$p_valor,  res_d5$p_valor,  res_d9$p_valor),
  Cramer_V  = c(res_d7$cramer_v, res_d3$cramer_v, res_d5$cramer_v, res_d9$cramer_v),
  N         = c(res_d7$n_total,  res_d3$n_total,  res_d5$n_total,  res_d9$n_total)
)
saveRDS(resumen_sesgo,
        here::here("data/base/ivec_resultados/sesgo_georref.rds"))
print(resumen_sesgo)
# Nota interpretativa: V de Cramér < 0.10 → efecto despreciable;
# 0.10–0.30 → pequeño; > 0.30 → moderado (requiere discusión del sesgo).


# =============================================================================
# §D.4 — CORRELACIÓN INTER-INDICADOR, COHERENCIA INTERNA Y ACP DEL IVEC_base
# =============================================================================
# Nivel de análisis: cuadrícula 1 km² (panel ivec_base.parquet). Las matrices
# de correlación de Spearman entre los 8 indicadores Vi y los 6 indicadores CH
# se guardan en correlaciones_base.rds para el Apéndice D §D.4; los alfa/omega
# por dimensión los produce R/ivec/01_ivec_base.R en validacion_d7.rds.
# =============================================================================

library(psych)     # alfa de Cronbach / omega de McDonald

ivec_base <- arrow::read_parquet(
  here::here("data/base/ivec_resultados/ivec_base.parquet")
)

vars_vi <- c("vi_cronicidad", "vi_renta", "vi_labor", "vi_migr", "vi_aseg",
             "vi_padronal", "vi_d1cobertura", "vi_edad")
vars_ch <- c("ch_tipo_res", "ch_comp_res", "ch_tam_res", "ch_ratio_dep",
             "ch_vi_medio", "ch_vi_het")

cor_vi <- cor(ivec_base[, vars_vi], method = "spearman",
              use = "pairwise.complete.obs")
cor_ch <- cor(ivec_base[, vars_ch], method = "spearman",
              use = "pairwise.complete.obs")

saveRDS(list(vi = round(cor_vi, 2), ch = round(cor_ch, 2)),
        here::here("data/base/ivec_resultados/correlaciones_base.rds"))

# Pares con |rho| > 0.70 (colinealidad a vigilar)
for (nm in c("Vi", "CH")) {
  m <- if (nm == "Vi") cor_vi else cor_ch
  altos <- which(abs(m) > 0.70 & upper.tri(m), arr.ind = TRUE)
  if (nrow(altos) == 0) message("Ningún par |rho|>0.70 en ", nm)
}

# ── Validación ACP del esquema de ponderación equiponderada ───────────────────
# Comprueba los tres criterios de cap. 5 §sec-validacion-acp: (1) varianza de
# PC1 > 40%; (2) loadings de PC1 todos positivos; (3) rho de Spearman entre el
# esquema equiponderado y PC1 > 0,95. Añade el ACP por dimensión (Vi, CH).
acp_df <- ivec_base[, c(vars_vi, vars_ch)]
acp_df <- acp_df[complete.cases(acp_df), ]
acp_full <- prcomp(acp_df, center = TRUE, scale. = FALSE)
imp_full <- summary(acp_full)$importance
print(round(imp_full[1:3, 1:min(10, ncol(acp_df))], 3))
# PC1 > 40% → dimensionalidad dominante compatible con la agregación escalar.

# Diagnóstico 2: loadings de PC1 (esperados todos positivos).
loadings_pc1 <- sort(acp_full$rotation[, 1])
print(round(loadings_pc1, 3))

# Diagnóstico 3: rho de Spearman entre el esquema equiponderado y PC1.
pc1_scores  <- acp_full$x[, 1]
idx_completo <- complete.cases(ivec_base[, c(vars_vi, vars_ch)])
rho_equi_pc1 <- abs(cor(rowMeans(acp_df), pc1_scores, method = "spearman"))
rho_ivec_pc1 <- abs(cor(ivec_base$ivec_base[idx_completo], pc1_scores,
                        method = "spearman"))

# ACP por dimensión (Vi con 8 indicadores, CH con 6).
pc1_var_dim <- sapply(list(Vi = vars_vi, CH = vars_ch), function(vv) {
  dd <- ivec_base[, vv]; dd <- dd[complete.cases(dd), ]
  summary(prcomp(dd, center = TRUE, scale. = FALSE))$importance[2, 1]
})

acp_ponderacion <- list(
  n_celdas     = nrow(acp_df),
  var_pc1      = imp_full[2, 1],
  var_pc1_pc2  = imp_full[3, 2],
  loadings_pc1 = loadings_pc1,
  rho_equi_pc1 = rho_equi_pc1,
  rho_ivec_pc1 = rho_ivec_pc1,
  var_pc1_vi   = pc1_var_dim[["Vi"]],
  var_pc1_ch   = pc1_var_dim[["CH"]]
)
saveRDS(acp_ponderacion,
        here::here("data/base/ivec_resultados/acp_ponderacion.rds"))
message(sprintf("ACP: PC1=%.1f%%, rho(equi,PC1)=%.3f, rho(IVEC,PC1)=%.3f",
                acp_ponderacion$var_pc1 * 100, rho_equi_pc1, rho_ivec_pc1))
