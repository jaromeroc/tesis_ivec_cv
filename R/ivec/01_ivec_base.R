# =============================================================================
# 01_ivec_base.R — CONSTRUCCIÓN DEL MÓDULO IVEC_base
# =============================================================================
# Módulo:   IVEC_base (vulnerabilidad individual-hogar)
# Escala:   Cuadrícula LAEA 1×1 km (EPSG:3035, GRD_ID estándar Eurostat)
# Fórmula:  IVEC_base_i = ½·Vi_i* + ½·(1 − CH_i*)
#
# Dimensión Vi — 8 indicadores operativos (V1):
#   vi_cronicidad  → cronicidad(cod)       carga de morbilidad crónica
#   vi_renta       → D9_raf_renta(cod)     posición relativa al umbral de pobreza
#   vi_labor       → D4_lab(cod)           situación laboral y discapacidad
#   vi_migr        → D3_migr(cod)          trayectoria migratoria
#   vi_aseg        → D5_aseg_subgr(cod)    subgrupo de aseguramiento sanitario
#   vi_padronal    → empadronamiento(cod)  anclaje padronal (binario)
#   vi_d1cobertura → D1_fin_cov(cod)       fragilidad econ-institucional cobertura
#   vi_edad        → f_nacim               edad (continua, percentil)
#
# Dimensión CH — 6 indicadores operativos (V1):
#   ch_tipo_res    → D8_tipo_res(cod)      tipo residencial (familiar vs. no familiar)
#   ch_comp_res    → D8_comp_res(cod)      composición del hogar (9 categorías)
#   ch_tam_res     → D8_tam_res(cod)       tamaño del hogar
#   ch_ratio_dep   → UCO_CODI + f_nacim    ratio dependientes / total hogar
#   ch_vi_medio    → UCO_CODI + Vi indiv.  Vi medio del hogar (composición sust.)
#   ch_vi_het      → UCO_CODI + Vi indiv.  heterogeneidad de Vi en el hogar
#
# Normalización:
#   Paso 1 — Percentil individual: cada indicador se transforma en su rango
#             percentílico dentro del conjunto de individuos georreferenciados
#             de la Comunitat Valenciana (midpoint percentile rank).
#   Paso 2 — Min-max dimensional a nivel de celda: Vi_celda y CH_celda se
#             normalizan al rango [0,1] sobre el conjunto de celdas activas.
#
# Fuentes:
#   data/SIP/results/SIP_final.parquet      — registro depurado con f_nacim
#   data/Literatura/GRID/CV_shapefiles/CV_grid_1km.shp — cuadrícula LAEA
#
# Salida:
#   data/base/ivec_resultados/ivec_base.parquet
#   data/base/ivec_resultados/tabla_depuracion.rds
#   data/base/ivec_resultados/validacion_d7.rds   (estadísticos plano 1 y 2)
# =============================================================================

library(tidyverse)
library(arrow)
library(sf)
library(here)
library(psych)       # alfa de Cronbach, omega de McDonald

# ── Reproducibilidad ─────────────────────────────────────────────────────────
set.seed(1972)

# ── Fecha de extracción (para cálculo de edad) ────────────────────────────────
FECHA_EXTRACCION <- as.Date("2025-06-01")

# ── Umbrales ─────────────────────────────────────────────────────────────────
UMBRAL_CELDA <- 15L   # mínimo de individuos por celda para el producto estadístico.
                       # Para uso operativo (emergencias, políticas de proximidad en
                       # zonas rurales dispersas), puede reducirse a 1L para producir
                       # una capa complementaria con todas las celdas habitadas,
                       # advirtiendo explícitamente de la inestabilidad estadística.

# ── Rutas ──────────────────────────────────────────────────────────────────
ruta_sip_final    <- here("data/SIP/results/SIP_final.parquet")
ruta_grid         <- here("data/Literatura/GRID/CV_shapefiles/CV_grid_1km.shp")
dir_out           <- here("data/base/ivec_resultados")
dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

message("\n══════════════════════════════════════════════════════════════")
message("  IVEC_base — inicio del pipeline")
message("══════════════════════════════════════════════════════════════\n")


# =============================================================================
# SECCIÓN 1 — CARGA Y UNIÓN DE DATOS
# =============================================================================

message("── Sección 1: carga de datos ──────────────────────────────────")

# Columnas necesarias de SIP_final
# f_nacim está incluida desde FASE 9 del pipeline SIP (A3_pipeline_sip.R)
cols_final <- c(
  "UCO_CODI", "N_SIP",
  "f_nacim",                       # fecha nacimiento: vi_edad y ch_ratio_dep
  "cronicidad(cod)", "D9_raf_renta(cod)", "D4_lab(cod)",
  "D3_migr(cod)", "D5_aseg_subgr(cod)",
  "empadronamiento(cod)",          # vi_padronal: anclaje padronal
  "D1_fin_cov(cod)",               # vi_d1cobertura: financiación cobertura sanitaria
  "D7_vulner_apsig(cod)",          # solo para validación, NO es indicador IVEC
  "D8_tipo_res(cod)", "D8_comp_res(cod)", "D8_tam_res(cod)",
  "D2_resid(cod)",                 # filtro residencia CV
  "ST_X", "ST_Y"
)

message("  Cargando SIP_final.parquet...")
sip <- read_parquet(ruta_sip_final, col_select = all_of(cols_final))
message("  Registros cargados: ", format(nrow(sip), big.mark = "."))

n_sin_nacim <- sum(is.na(sip$f_nacim))
message("  Registros sin f_nacim: ",
        format(n_sin_nacim, big.mark = "."),
        " (", round(100 * n_sin_nacim / nrow(sip), 2), "%)")


# =============================================================================
# SECCIÓN 2 — FILTROS DE INCLUSIÓN (reproducción de la lógica SIP_final)
# =============================================================================

message("\n── Sección 2: filtros de universo de análisis ──────────────────")

n_total <- nrow(sip)
message("  N inicial: ", format(n_total, big.mark = "."))

# D2: residentes en la CV
sip <- sip |> filter(`D2_resid(cod)` == "1")
message("  Tras D2 (residentes CV): ", format(nrow(sip), big.mark = "."),
        " (excl. ", format(n_total - nrow(sip), big.mark = "."), ")")
n_d2 <- nrow(sip)

# Registros con ST_X y ST_Y válidos (dentro del bbox de la CV en UTM30N)
# CV real: X [620 000, 900 000], Y [4 175 000, 4 550 000]
# NOTA: el límite occidental se establece en 620 000 (no 695 000) para incluir
# los municipios del interior de Valencia (Rincón de Ademuz, Los Serranos,
# Serranía) cuyos centroides UTM30N se sitúan en el rango X [634 000–695 000].
# El bbox ajustado recupera 344 221 residentes previamente excluidos por error
# de especificación (verificado con análisis de distribución de X, mayo 2025).
sip_geo <- sip |>
  filter(
    !is.na(ST_X), !is.na(ST_Y),
    ST_X > 620000, ST_X < 900000,
    ST_Y > 4175000, ST_Y < 4550000
  )
n_nulos_geo    <- sum(is.na(sip$ST_X) | is.na(sip$ST_Y))
n_fuera_bbox   <- nrow(sip) - n_nulos_geo - nrow(sip_geo)
message("  Sin coordenadas (ST_X/ST_Y nulos):      ",
        format(n_nulos_geo, big.mark = "."),
        " (", round(100 * n_nulos_geo / n_d2, 2), "%)")
message("  Coordenadas fuera del bbox CV:          ",
        format(n_fuera_bbox, big.mark = "."),
        " (", round(100 * n_fuera_bbox / n_d2, 2), "%)")
message("  Georreferenciados válidos:              ",
        format(nrow(sip_geo), big.mark = "."),
        " (", round(100 * nrow(sip_geo) / n_d2, 2), "%)")

# El subset sip (completo, sin filtro geo) se conserva para la normalización
# percentílica: los percentiles se calculan sobre TODOS los residentes CV
# (no solo los georreferenciados), de modo que el rango refleja la distribución
# poblacional completa antes de la restricción espacial.
# sip_geo contiene solo los que entran en el cálculo del IVEC_base.


# =============================================================================
# SECCIÓN 3 — CODIFICACIÓN DE INDICADORES Vi
# =============================================================================
# Para cada indicador se asigna una puntuación de vulnerabilidad ordinal en [0,1]
# que respeta la hipótesis causal documentada en el capítulo quinto
# (§sec-dim-vi). Los valores se transforman luego a rango percentílico.

message("\n── Sección 3: codificación de indicadores Vi ───────────────────")

# ── 3.1 vi_cronicidad ─────────────────────────────────────────────────────
# Hipótesis: mayor carga crónica → mayor fragilidad sistémica ante perturbaciones.
# Mapeo ordinal directo: 0 → 0, 1 → 1/3, 2 → 2/3, 3 → 1
cronicidad_map <- c("0" = 0, "1" = 1/3, "2" = 2/3, "3" = 1)

sip <- sip |>
  mutate(
    vi_cronicidad = cronicidad_map[`cronicidad(cod)`],
    vi_cronicidad = replace_na(vi_cronicidad, 0)   # sin dato → nivel 0 (conservador)
  )

message("  vi_cronicidad: distribución")
print(table(sip$`cronicidad(cod)`, useNA = "ifany"))


# ── 3.2 vi_renta ─────────────────────────────────────────────────────────
# Hipótesis: posición más cercana al umbral de pobreza → mayor inseguridad
# económica → mayor vulnerabilidad ante pérdida de ingresos o gastos de emergencia.
# Los códigos de D9_raf_renta representan tramos de copago farmacéutico
# (a menor copago → menor renta → mayor vulnerabilidad).
# Fuente de la lógica: cap. 5 §sec-dim-vi; clasificación copago RDL 16/2012.
renta_map <- c(
  "10" = 1.00,   # PN-0%:  exento por nivel de renta mínimo → máx. vulnerabilidad
  "20" = 0.90,   # PN-0% alternativo (variante codificación)
  "21" = 0.80,   # PN-10%: pensionistas de baja renta
  "22" = 0.75,   # PN-10% (variante)
  "63" = 0.65,   # NA-40%: no asegurado tramo bajo → vulnerabilidad adm. + económica
  "65" = 0.60,   # NA-100%: no asegurado sin exención
  "30" = 0.40,   # AC-40%: activo con renta moderada-baja
  "40" = 0.30,   # AC-50%: activo con renta media
  "50" = 0.20,   # AC-60%: activo con renta media-alta
  "53" = 0.18,   # PN-60%: pensionista renta alta
  "60" = 0.10,   # SF-100%: pago íntegro → renta elevada → mín. vulnerabilidad
  "64" = 0.50,   # NA-50%: no asegurado tramo medio (valor intermedio)
  "DA" = NA_real_,
  "99" = NA_real_
)

sip <- sip |>
  mutate(
    vi_renta = renta_map[`D9_raf_renta(cod)`],
    vi_renta = replace_na(vi_renta, median(vi_renta, na.rm = TRUE))
  )

# Imputar NA con mediana de los no-NA (después de la primera pasada)
mediana_renta <- median(sip$vi_renta, na.rm = TRUE)
sip <- sip |>
  mutate(vi_renta = if_else(is.na(vi_renta), mediana_renta, vi_renta))


# ── 3.3 vi_labor ─────────────────────────────────────────────────────────
# Hipótesis: la situación laboral determina simultáneamente la seguridad
# económica corriente, el acceso a prestaciones y la disponibilidad temporal
# ante una emergencia (cap. 5 §sec-dim-vi, líneas 299-305).
labor_map <- c(
  "P" = 1.00,   # B + Discapacitado: no puede trabajar + discapacidad → máx.
  "R" = 0.90,   # D + Discapacitado: desempleado + discapacidad
  "B" = 0.85,   # No puede trabajar (limitación de salud grave)
  "T" = 0.75,   # O + Discapacitado: otra situación + discapacidad
  "D" = 0.60,   # No trabaja pero puede trabajar (desempleo)
  "Q" = 0.50,   # C + Discapacitado: trabaja pero con discapacidad
  "E" = 0.40,   # Otra situación activa
  "O" = 0.35,   # Cualquier otra situación (ambigua)
  "C" = 0.10,   # Trabaja → menor vulnerabilidad laboral
  "S" = 0.35,   # Minoritario, tratar como O
  "9" = NA_real_,
  "0" = NA_real_,
  "2" = NA_real_
)

sip <- sip |>
  mutate(vi_labor = labor_map[`D4_lab(cod)`])

mediana_labor <- median(sip$vi_labor, na.rm = TRUE)
sip <- sip |>
  mutate(vi_labor = if_else(is.na(vi_labor), mediana_labor, vi_labor))


# ── 3.4 vi_migr ───────────────────────────────────────────────────────────
# Hipótesis: la recencia de la trayectoria migratoria indica fragilidad de
# las redes de apoyo local e infamiliaridad con los sistemas institucionales
# de protección (cap. 5 §sec-dim-vi, líneas 306-312).
migr_map <- c(
  "21" = 1.00,   # Inmigrante reciente (<= 2 años) del Extranjero → máx. fragilidad redes
  "22" = 0.80,   # Inmigrante de estancia media del Extranjero
  "31" = 0.70,   # Inmigrante reciente de otra CCAA
  "32" = 0.50,   # Inmigrante de estancia media de otra CCAA
  "23" = 0.40,   # Inmigrante antiguo del Extranjero (redes más consolidadas)
  "33" = 0.30,   # Inmigrante antiguo de otra CCAA
  "40" = 0.30,   # Otra situación migratoria
  "10" = 0.10,   # No migrante → redes de apoyo establecidas → menor vulnerabilidad
  "H1" = NA_real_,
  "51" = NA_real_,
  "96" = NA_real_,
  "00" = NA_real_
)

sip <- sip |>
  mutate(vi_migr = migr_map[`D3_migr(cod)`])

mediana_migr <- median(sip$vi_migr, na.rm = TRUE)
sip <- sip |>
  mutate(vi_migr = if_else(is.na(vi_migr), mediana_migr, vi_migr))


# ── 3.5 vi_aseg ───────────────────────────────────────────────────────────
# Hipótesis: la situación administrativa ante el sistema sanitario es un
# indicador de la dimensión institucional de la fragilidad individual:
# irregularidad administrativa → menor acceso efectivo a la red de protección
# en situación de emergencia (cap. 5 §sec-dim-vi, líneas 311-314).
aseg_map <- c(
  "C5" = 1.00,   # Extranjeros irregulares → acceso sanitario restringido
  "B2" = 0.90,   # Sin Recursos y Asistencia Sanitaria Universal
  "C1" = 0.70,   # Acreditación caducada → irregularidad adm. activa
  "C2" = 0.65,   # No Acreditados
  "B4" = 0.50,   # Otras Acreditaciones Conselleria (heterogéneo)
  "A1" = 0.30,   # Convenio Internacional (no residente habitual)
  "A2" = 0.20,   # Tarjeta Sanitaria Europea (visitante)
  "C3" = 0.15,   # Mutualismos Privados (MUFACE/MUGEJU: general estabilidad)
  "A3" = 0.10,   # Asegurados INSS: situación regular → menor vulnerabilidad
  "73" = NA_real_,
  "75" = NA_real_,
  "64" = NA_real_
)

sip <- sip |>
  mutate(vi_aseg = aseg_map[`D5_aseg_subgr(cod)`])

mediana_aseg <- median(sip$vi_aseg, na.rm = TRUE)
sip <- sip |>
  mutate(vi_aseg = if_else(is.na(vi_aseg), mediana_aseg, vi_aseg))


# ── 3.6 vi_padronal — anclaje padronal ────────────────────────────────────
# Hipótesis (cap. 3 §sec-sip-indicadores-cap3 y cap. 5 §sec-dim-vi):
# la ausencia de inscripción padronal estable en el municipio de residencia
# efectiva produce un déficit acumulativo de acceso a los sistemas locales
# de protección (prestaciones municipales, planes territoriales de
# emergencia, censo electoral local, trámites que exigen el Padrón) con
# independencia de la trayectoria migratoria y del aseguramiento sanitario.
# Codificación binaria: anclaje estable (valor 1) → 0; déficit padronal
# (valores 2 = > 1 mes y 3 = < 1 mes agregados) → 1.
# La unión de los dos valores de déficit (2+3) corresponde a la decisión
# operativa D7 documentada en el Apéndice D.
padronal_map <- c(
  "1" = 0.00,   # Empadronado en CV → anclaje estable
  "2" = 1.00,   # No empadronado > 1 mes → déficit prolongado
  "3" = 1.00    # No empadronado < 1 mes → déficit reciente (mismo tratamiento)
)

sip <- sip |>
  mutate(vi_padronal = padronal_map[`empadronamiento(cod)`])

mediana_padronal <- median(sip$vi_padronal, na.rm = TRUE)
sip <- sip |>
  mutate(vi_padronal = if_else(is.na(vi_padronal), mediana_padronal, vi_padronal))


# ── 3.7 vi_d1cobertura — fragilidad econ-institucional de la cobertura ────
# Hipótesis (cap. 3 §sec-sip-indicadores-cap3 y cap. 5 §sec-dim-vi):
# la fuente económica de la financiación de la cobertura sanitaria captura
# una dimensión de fragilidad económico-institucional distinta de la
# posición relativa al umbral de pobreza (D9, dirigida a la aportación
# farmacéutica) y del subgrupo administrativo de aseguramiento
# (D5_aseg_subgr, dirigido al estatus administrativo). La dependencia de
# la cobertura subsidiada regional o la ausencia de aseguramiento son
# indicadores específicos de una posición precaria que combina
# insuficiencia de ingresos por cotización formal con dependencia
# institucional para el acceso a derechos sanitarios.
# Codificación ordinal: cotización ordinaria o mutualismo institucional
# (valores 10, 30, 40, 51) → 0; cobertura subsidiada regional GVA (20)
# → 0.5; ausencia de aseguramiento (60) → 1.
# Los valores 52 (mutualismo privado sin convenio) y 99 (desconocido)
# se tratan como datos perdidos por baja frecuencia y por ambigüedad
# de su perfil de fragilidad (decisión operativa D8).
d1cobertura_map <- c(
  "10" = 0.00,   # Asegurados INSS → estabilidad
  "30" = 0.00,   # Mutualismo Administrativo Público → estabilidad
  "40" = 0.00,   # Convenio Internacional → estabilidad
  "51" = 0.00,   # Mutualismo Administrativo Privado con convenio → estabilidad
  "20" = 0.50,   # Cobertura SNS Conselleria (subsidiada) → fragilidad media
  "60" = 1.00,   # No Asegurados → máxima fragilidad
  "52" = NA_real_,  # Mutualismo Privado sin convenio (perfil ambiguo)
  "99" = NA_real_   # Desconocido
)

sip <- sip |>
  mutate(vi_d1cobertura = d1cobertura_map[`D1_fin_cov(cod)`])

mediana_d1cob <- median(sip$vi_d1cobertura, na.rm = TRUE)
sip <- sip |>
  mutate(vi_d1cobertura = if_else(is.na(vi_d1cobertura), mediana_d1cob, vi_d1cobertura))


# ── 3.8 vi_edad ───────────────────────────────────────────────────────────
# Hipótesis: la edad es un modulador de la fragilidad individual ante
# perturbaciones: a mayor edad, menor capacidad de recuperación funcional
# tras el impacto y mayor dependencia de rutinas de cuidado. Se incorpora
# como variable continua normalizada por percentil, no como categoría.
# La interacción edad×cronicidad se implementa como séptimo indicador
# en versiones futuras del instrumento (cap. 5 §sec-dim-vi, líneas 316-319).

sip <- sip |>
  mutate(
    f_nacim_date = as.Date(as.character(f_nacim), format = "%Y%m%d"),
    vi_edad_raw  = as.numeric(difftime(FECHA_EXTRACCION, f_nacim_date,
                                       units = "days")) / 365.25,
    # Valores implausibles (nacidos antes de 1900 o después de 2025): NA
    vi_edad_raw  = if_else(vi_edad_raw < 0 | vi_edad_raw > 125,
                           NA_real_, vi_edad_raw)
  )

n_sin_edad <- sum(is.na(sip$vi_edad_raw))
message("  vi_edad: individuos sin fecha de nacimiento válida: ",
        format(n_sin_edad, big.mark = "."))


# =============================================================================
# SECCIÓN 4 — CODIFICACIÓN DE INDICADORES CH
# =============================================================================

message("\n── Sección 4: codificación de indicadores CH ───────────────────")

# ── 4.1 ch_tipo_res ───────────────────────────────────────────────────────
# Hipótesis: los individuos fuera de una unidad familiar reconocida carecen
# de la red de convivencia que distribuye la carga de cuidados ante
# una perturbación. Máxima vulnerabilidad CH por aislamiento estructural
# (cap. 5 §sec-dim-ch, líneas 332-336).
sip <- sip |>
  mutate(
    ch_tipo_res = if_else(`D8_tipo_res(cod)` == "01", 0.0, 1.0)
    # 0 = Unidad Familiar (menor vulnerabilidad por tipo residencial)
    # 1 = Residencia no familiar (máxima vulnerabilidad por aislamiento)
    # Nota: los tipos institucionales (residencias 3ª edad, centros etc.)
    #   ya fueron excluidos en D1 del pipeline SIP. Los valores != "01"
    #   que permanecen son principalmente "00" (sin clasificar).
  )


# ── 4.2 ch_comp_res ───────────────────────────────────────────────────────
# Hipótesis: la capacidad de redistribución interna del hogar determina la
# velocidad y el alcance de la respuesta ante una perturbación. Los hogares
# con mayor ratio adultos/dependientes tienen mayor capacidad de movilización
# sin recurrir a recursos externos (cap. 5 §sec-dim-ch, líneas 337-344).
#
# Ordenación por vulnerabilidad (mayor score = mayor fragilidad):
#   "1"  Un menor solo            → 1.00  (sin adulto cuidador)
#   "2"  >1 menor solo            → 1.00
#   "0"  No es Unidad Familiar    → 0.95  (ausencia de red de convivencia)
#   "4"  Un adulto + N menores    → 0.85  (monoparental)
#   "3"  Un adulto solo           → 0.75  (aislamiento adulto)
#   "6"  2 adultos + N menores    → 0.45
#   "8"  >2 adultos + N menores   → 0.35
#   "5"  2 adultos sin menores    → 0.25
#   "7"  >2 adultos sin menores   → 0.10  (máxima capacidad de redistribución)
comp_res_map <- c(
  "1" = 1.00, "2" = 1.00, "0" = 0.95,
  "4" = 0.85, "3" = 0.75,
  "6" = 0.45, "8" = 0.35,
  "5" = 0.25, "7" = 0.10,
  "9" = 0.50, "O" = NA_real_
)

sip <- sip |>
  mutate(ch_comp_res = comp_res_map[`D8_comp_res(cod)`])

mediana_comp <- median(sip$ch_comp_res, na.rm = TRUE)
sip <- sip |>
  mutate(ch_comp_res = if_else(is.na(ch_comp_res), mediana_comp, ch_comp_res))


# ── 4.3 ch_tam_res ────────────────────────────────────────────────────────
# Hipótesis: el tamaño del hogar es un indicador de la capacidad bruta de
# redistribución interna, modulada por ch_comp_res. Los hogares pequeños
# tienen menor redundancia ante la pérdida de capacidad de un miembro;
# los hogares grandes con alta proporción de dependientes intensifican
# la presión sobre los cuidadores disponibles (cap. 5 §sec-dim-ch, líneas 345-352).
tam_res_map <- c(
  "0" = 0.90,   # No es Unidad Familiar → sin red de tamaño
  "1" = 0.70,   # < 3 personas (pequeña) → menor redundancia
  "2" = 0.30,   # 3-4 personas (mediana) → capacidad estándar
  "3" = 0.40,   # > 4 personas (grande) → capacidad bruta alta pero
                #   potencial presión de dependientes (interacción con comp_res)
  "C" = NA_real_
)

sip <- sip |>
  mutate(ch_tam_res = tam_res_map[`D8_tam_res(cod)`])

mediana_tam <- median(sip$ch_tam_res, na.rm = TRUE)
sip <- sip |>
  mutate(ch_tam_res = if_else(is.na(ch_tam_res), mediana_tam, ch_tam_res))


# ── 4.4 ch_ratio_dep — ratio dependientes/total por UCO_CODI ─────────────
# Hipótesis: el ratio entre miembros dependientes (menores + edad avanzada
# con alta cronicidad) y el total de miembros del hogar mide la presión
# de cuidados sobre los adultos activos. Un hogar con todos sus miembros
# dependientes y ningún adulto activo tiene ratio = 1 (máxima vulnerabilidad
# en la dimensión CH). Reconstrucción a partir de UCO_CODI (cap. 5 §sec-dim-ch).
#
# Definición operativa de "dependiente":
#   — Menor de 18 años (nacido después de 2007-06-01)
#   — O edad >= 75 años con cronicidad nivel 2-3
#   — O D4_lab en {P, B, R} (no puede trabajar / discapacitado grave)
# Definición operativa de "adulto activo":
#   — Edad 18-74 Y D4_lab ∉ {P, B, R} Y cronicidad 0-1
# NOTA: individuos con f_nacim nulo no pueden clasificarse; se les asigna
# el ratio medio del hogar (UCO_CODI), o la mediana global si el hogar
# entero carece de edades válidas.

message("  Calculando ratio dependientes por UCO_CODI...")

sip_ratio <- sip |>
  select(UCO_CODI, f_nacim_date = f_nacim_date,
         cronicidad_cod = `cronicidad(cod)`,
         labor_cod = `D4_lab(cod)`) |>
  mutate(
    # f_nacim_date ya es un objeto Date (creado en sección 3.6);
    # NO re-envolver en as.Date(as.character(...)) porque as.character sobre
    # un Date produce "YYYY-MM-DD", que el formato "%Y%m%d" no puede parsear.
    edad_calc = as.numeric(
      difftime(FECHA_EXTRACCION, f_nacim_date, units = "days")) / 365.25,
    es_dependiente = case_when(
      is.na(edad_calc)                                    ~ NA,
      edad_calc < 18                                      ~ TRUE,
      edad_calc >= 75 & cronicidad_cod %in% c("2","3")    ~ TRUE,
      labor_cod %in% c("P","B","R")                       ~ TRUE,
      TRUE                                                 ~ FALSE
    )
  ) |>
  group_by(UCO_CODI) |>
  summarise(
    n_miembros     = n(),
    n_dependientes = sum(es_dependiente == TRUE, na.rm = TRUE),
    ratio_dep      = n_dependientes / n_miembros,
    .groups = "drop"
  )

# Unir al dataset principal
sip <- sip |>
  left_join(sip_ratio |> select(UCO_CODI, ch_ratio_dep = ratio_dep),
            by = "UCO_CODI")

mediana_ratio <- median(sip$ch_ratio_dep, na.rm = TRUE)
sip <- sip |>
  mutate(ch_ratio_dep = if_else(is.na(ch_ratio_dep), mediana_ratio, ch_ratio_dep))

message("  ratio_dep: mediana = ", round(mediana_ratio, 3),
        " ; rango = [", round(min(sip$ch_ratio_dep, na.rm=TRUE),3),
        ", ", round(max(sip$ch_ratio_dep, na.rm=TRUE),3), "]")

rm(sip_ratio)


# =============================================================================
# SECCIÓN 5 — NORMALIZACIÓN PERCENTÍLICA INDIVIDUAL
# =============================================================================
# Los percentiles se calculan sobre el universo completo de residentes CV
# (sip, antes del filtro de georreferenciación) para que el rango percentílico
# refleje la distribución poblacional total, no solo los georreferenciados.
# Función de rango percentílico (midpoint): P(x) = (Σ[xi<x] + 0.5·Σ[xi=x]) / N

message("\n── Sección 5: normalización percentílica individual ────────────")

prank <- function(x) {
  # Midpoint percentile rank in [0, 1]
  n  <- sum(!is.na(x))
  rx <- rank(x, ties.method = "average", na.last = "keep")
  (rx - 0.5) / n
}

indicadores_vi <- c("vi_cronicidad", "vi_renta", "vi_labor",
                    "vi_migr", "vi_aseg", "vi_padronal", "vi_d1cobertura",
                    "vi_edad_raw")
indicadores_ch_basicos <- c("ch_tipo_res", "ch_comp_res",
                             "ch_tam_res", "ch_ratio_dep")

message("  Calculando rangos percentílicos (N = ",
        format(nrow(sip), big.mark = "."), " residentes CV)...")

for (ind in c(indicadores_vi, indicadores_ch_basicos)) {
  nuevo_nombre <- paste0(ind, "_p")
  sip[[nuevo_nombre]] <- prank(sip[[ind]])
  message("    ", nuevo_nombre, ": media = ",
          round(mean(sip[[nuevo_nombre]], na.rm = TRUE), 3))
}

# Renombrar vi_edad_raw_p → vi_edad_p para claridad
sip <- sip |>
  rename(vi_edad_p = vi_edad_raw_p)

indicadores_vi_p <- c("vi_cronicidad_p", "vi_renta_p", "vi_labor_p",
                       "vi_migr_p", "vi_aseg_p", "vi_padronal_p",
                       "vi_d1cobertura_p", "vi_edad_p")


# =============================================================================
# SECCIÓN 5.5 — INDICADORES CH DERIVADOS INTRAHOGAR (Vi medio y heterogeneidad)
# =============================================================================
# Los dos nuevos indicadores derivados de CH se construyen sobre la puntuación
# Vi individual ya normalizada a percentil, agregada por UCO_CODI según
# §sec-dim-ch del cap. 5. El orden es crítico: la normalización percentílica
# de los indicadores Vi básicos debe haberse completado ANTES de la agregación
# intrahogar (decisión operativa D9 del Apéndice D).

message("\n── Sección 5.5: indicadores CH derivados intrahogar ────────────")

# Vi individual score: media de los 8 indicadores Vi percentílicos
sip <- sip |>
  mutate(
    vi_individual_score = (vi_cronicidad_p + vi_renta_p + vi_labor_p +
                           vi_migr_p + vi_aseg_p + vi_padronal_p +
                           vi_d1cobertura_p + vi_edad_p) / 8
  )

# Agregación a nivel de hogar (UCO_CODI)
# - vi_medio_hogar_raw: media intrahogar de Vi individual.
#   HIGH = composición sustantiva de fragilidad alta (sigue convención CH:
#   high = high vulnerabilidad / low capacidad).
# - vi_het_hogar_raw: desviación estándar intrahogar.
#   HIGH = mayor heterogeneidad = mayor capacidad de redistribución =
#   MENOR vulnerabilidad. Para mantener la convención (high = fragilidad
#   en el módulo CH), invertimos el signo en el ranking percentílico
#   posterior. Hogares unipersonales reciben sd = 0 por construcción.
message("  Calculando agregaciones por UCO_CODI...")
sip_hogar_vi <- sip |>
  group_by(UCO_CODI) |>
  summarise(
    n_miembros_hogar    = n(),
    vi_medio_hogar_raw  = mean(vi_individual_score, na.rm = TRUE),
    vi_het_hogar_raw    = if_else(
      n() > 1,
      sd(vi_individual_score, na.rm = TRUE),
      0
    ),
    .groups = "drop"
  )

sip <- sip |>
  left_join(
    sip_hogar_vi |> select(UCO_CODI, vi_medio_hogar_raw, vi_het_hogar_raw),
    by = "UCO_CODI"
  )

# Imputar NA de heterogeneidad como 0 (hogares con n=1 y NA en sd)
sip <- sip |>
  mutate(vi_het_hogar_raw = replace_na(vi_het_hogar_raw, 0))

# Percentil rank de los indicadores derivados
# ch_vi_medio_p: directo (alto Vi medio → más fragilidad)
sip$ch_vi_medio_p <- prank(sip$vi_medio_hogar_raw)
# ch_vi_het_p: invertido (alto sd = alta heterogeneidad = alta capacidad)
# Al rankear el negativo, el percentil alto corresponde a sd baja, que
# en la convención del módulo CH es "mayor fragilidad por homogeneidad
# de la acumulación intrahogar".
sip$ch_vi_het_p   <- prank(-sip$vi_het_hogar_raw)

message("    ch_vi_medio_p: media = ",
        round(mean(sip$ch_vi_medio_p, na.rm = TRUE), 3))
message("    ch_vi_het_p:   media = ",
        round(mean(sip$ch_vi_het_p, na.rm = TRUE), 3))

# Lista completa de indicadores CH percentílicos (6 indicadores)
indicadores_ch_p <- c("ch_tipo_res_p", "ch_comp_res_p",
                       "ch_tam_res_p", "ch_ratio_dep_p",
                       "ch_vi_medio_p", "ch_vi_het_p")

rm(sip_hogar_vi)


# =============================================================================
# SECCIÓN 6 — ASIGNACIÓN A CUADRÍCULA LAEA 1 km²
# =============================================================================
# Reprojectar ST_X / ST_Y (ETRS89 UTM30N, EPSG:25830) a ETRS89-LAEA (EPSG:3035)
# y construir GRD_ID canónico: CRS3035RES1000mN{N}E{E}

message("\n── Sección 6: asignación a cuadrícula LAEA ─────────────────────")

# Filtrar a georreferenciados válidos (ya calculado en sección 2)
cols_para_geo <- c("UCO_CODI", "N_SIP", "ST_X", "ST_Y",
                   "D7_vulner_apsig(cod)",
                   indicadores_vi_p, indicadores_ch_p)

sip_geo <- sip |>
  filter(
    !is.na(ST_X), !is.na(ST_Y),
    ST_X > 620000, ST_X < 900000,   # 620 000 incluye franja occidental CV
    ST_Y > 4175000, ST_Y < 4550000
  ) |>
  select(all_of(cols_para_geo))

message("  Individuos georreferenciados para asignación: ",
        format(nrow(sip_geo), big.mark = "."))

# Convertir a sf en UTM30N
message("  Convirtiendo a sf y reprojectando a LAEA (EPSG:3035)...")
pts_utm  <- st_as_sf(sip_geo, coords = c("ST_X", "ST_Y"),
                     crs = 25830, remove = FALSE)
pts_laea <- st_transform(pts_utm, 3035)
coords_laea <- st_coordinates(pts_laea)

sip_geo <- sip_geo |>
  mutate(
    laea_N = floor(coords_laea[, 2] / 1000) * 1000L,
    laea_E = floor(coords_laea[, 1] / 1000) * 1000L,
    GRD_ID = paste0("CRS3035RES1000mN", laea_N, "E", laea_E)
  )

n_celdas_brutas <- n_distinct(sip_geo$GRD_ID)
message("  Celdas únicas antes del umbral: ", format(n_celdas_brutas, big.mark = "."))

rm(pts_utm, pts_laea, coords_laea)


# =============================================================================
# SECCIÓN 7 — AGREGACIÓN A NIVEL DE CELDA Y UMBRAL MÍNIMO
# =============================================================================

message("\n── Sección 7: agregación a celda y umbral mínimo ──────────────")

# Media de indicadores percentílicos por celda
sip_celda <- sip_geo |>
  group_by(GRD_ID, laea_N, laea_E) |>
  summarise(
    n_individuos = n(),
    # Dimensión Vi: media de los 8 indicadores percentílicos
    vi_cronicidad   = mean(vi_cronicidad_p,   na.rm = TRUE),
    vi_renta        = mean(vi_renta_p,        na.rm = TRUE),
    vi_labor        = mean(vi_labor_p,        na.rm = TRUE),
    vi_migr         = mean(vi_migr_p,         na.rm = TRUE),
    vi_aseg         = mean(vi_aseg_p,         na.rm = TRUE),
    vi_padronal     = mean(vi_padronal_p,     na.rm = TRUE),
    vi_d1cobertura  = mean(vi_d1cobertura_p,  na.rm = TRUE),
    vi_edad         = mean(vi_edad_p,         na.rm = TRUE),
    # Dimensión CH: media de los 6 indicadores percentílicos
    ch_tipo_res     = mean(ch_tipo_res_p,     na.rm = TRUE),
    ch_comp_res     = mean(ch_comp_res_p,     na.rm = TRUE),
    ch_tam_res      = mean(ch_tam_res_p,      na.rm = TRUE),
    ch_ratio_dep    = mean(ch_ratio_dep_p,    na.rm = TRUE),
    ch_vi_medio     = mean(ch_vi_medio_p,     na.rm = TRUE),
    ch_vi_het       = mean(ch_vi_het_p,       na.rm = TRUE),
    # Proporción D7 alto para validación convergente (H1)
    # Estratos D7 de alta vulnerabilidad: 3 (sin recursos), 5 (irr. extranjero),
    # 7 (desempleado en riesgo objetivo) — como en §sec-relacion-d7
    d7_alto_prop  = mean(`D7_vulner_apsig(cod)` %in% c("3","5","7"),
                         na.rm = TRUE),
    .groups = "drop"
  )

# Dimensiones Vi y CH como medias de los indicadores componentes
sip_celda <- sip_celda |>
  mutate(
    Vi_celda = (vi_cronicidad + vi_renta + vi_labor +
                vi_migr + vi_aseg + vi_padronal +
                vi_d1cobertura + vi_edad) / 8,
    # CORRECCIÓN DE SIGNO (mayo 2026): los seis indicadores ch_* están
    # codificados como FRAGILIDAD de composición (alto = mayor vulnerabilidad).
    # CH debe ser una dimensión de CAPACIDAD para entrar correctamente en la
    # fórmula como (1 - CH*); por eso aquí se invierte la media de fragilidad
    # (1 - media). Así CH_celda y CH_star quedan orientados como capacidad
    # (alto = mayor capacidad compensatoria del hogar), y (1 - CH*) recupera la
    # ausencia de capacidad que suma a la vulnerabilidad. La coherencia interna
    # (alfa/omega sobre mat_ch) es invariante a esta inversión.
    CH_celda = 1 - (ch_tipo_res + ch_comp_res + ch_tam_res +
                    ch_ratio_dep + ch_vi_medio + ch_vi_het) / 6
  )

# Aplicar umbral mínimo (decisión D2: >= 15 individuos por celda)
n_celdas_antes <- nrow(sip_celda)
sip_celda <- sip_celda |>
  filter(n_individuos >= UMBRAL_CELDA)
n_excluidas_umbral <- n_celdas_antes - nrow(sip_celda)

message("  Celdas con < ", UMBRAL_CELDA, " individuos excluidas: ",
        format(n_excluidas_umbral, big.mark = "."),
        " (", round(100 * n_excluidas_umbral / n_celdas_antes, 1), "%)")
message("  Celdas activas tras el umbral: ",
        format(nrow(sip_celda), big.mark = "."))


# =============================================================================
# SECCIÓN 8 — NORMALIZACIÓN MIN-MAX DIMENSIONAL A NIVEL DE CELDA
# =============================================================================
# Vi* y CH* se normalizan al rango [0,1] sobre el conjunto de celdas activas.

message("\n── Sección 8: normalización min-max dimensional ────────────────")

minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2]) return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

sip_celda <- sip_celda |>
  mutate(
    Vi_star = minmax(Vi_celda),
    CH_star = minmax(CH_celda)
  )

message("  Vi*: media = ", round(mean(sip_celda$Vi_star), 3),
        " ; dt = ", round(sd(sip_celda$Vi_star), 3))
message("  CH*: media = ", round(mean(sip_celda$CH_star), 3),
        " ; dt = ", round(sd(sip_celda$CH_star), 3))


# =============================================================================
# SECCIÓN 9 — FÓRMULA IVEC_base
# =============================================================================
# IVEC_base_i = ½·Vi_i* + ½·(1 − CH_i*)
# CH entra invertida: valores altos de CH* indican mayor capacidad de
# redistribución interna → reducen la vulnerabilidad neta de la celda.

message("\n── Sección 9: cálculo IVEC_base ────────────────────────────────")

sip_celda <- sip_celda |>
  mutate(
    ivec_base = 0.5 * Vi_star + 0.5 * (1 - CH_star)
  )

message("  IVEC_base: media = ", round(mean(sip_celda$ivec_base), 3),
        " ; mediana = ",  round(median(sip_celda$ivec_base), 3),
        " ; dt = ", round(sd(sip_celda$ivec_base), 3))
message("  Rango: [", round(min(sip_celda$ivec_base), 3),
        ", ", round(max(sip_celda$ivec_base), 3), "]")


# =============================================================================
# SECCIÓN 10 — ESTADÍSTICOS DE VALIDACIÓN (PLANOS 1 Y 2)
# =============================================================================

message("\n── Sección 10: estadísticos de validación ──────────────────────")

# ── Plano 1: coherencia interna (alfa de Cronbach, omega de McDonald) ─────
message("  Plano 1 — coherencia interna del IVEC_base...")

# Se calcula sobre las puntuaciones de celda (no individuales) para reflejar
# la estructura del instrumento tal como opera en la unidad de análisis.
mat_vi <- sip_celda |>
  select(vi_cronicidad, vi_renta, vi_labor, vi_migr, vi_aseg,
         vi_padronal, vi_d1cobertura, vi_edad) |>
  as.matrix()

mat_ch <- sip_celda |>
  select(ch_tipo_res, ch_comp_res, ch_tam_res, ch_ratio_dep,
         ch_vi_medio, ch_vi_het) |>
  as.matrix()

alpha_vi <- tryCatch(
  psych::alpha(mat_vi, check.keys = FALSE)$total$raw_alpha,
  error = function(e) NA_real_
)
alpha_ch <- tryCatch(
  psych::alpha(mat_ch, check.keys = FALSE)$total$raw_alpha,
  error = function(e) NA_real_
)
omega_vi <- tryCatch(
  psych::omega(mat_vi, nfactors = 1, plot = FALSE)$omega.tot,
  error = function(e) NA_real_
)
omega_ch <- tryCatch(
  psych::omega(mat_ch, nfactors = 1, plot = FALSE)$omega.tot,
  error = function(e) NA_real_
)

tab_coherencia <- tibble::tibble(
  Dimensión       = c("Vi", "CH"),
  `α Cronbach`    = round(c(alpha_vi, alpha_ch), 3),
  `ω McDonald`    = round(c(omega_vi, omega_ch), 3),
  `N indicadores` = c(8L, 6L),
  Resultado       = case_when(
    c(alpha_vi, alpha_ch) >= 0.60 &
    c(omega_vi, omega_ch) >= 0.65  ~ "Supera umbral (α≥0.60; ω≥0.65)",
    c(alpha_vi, alpha_ch) >= 0.60  ~ "α supera umbral; ω por debajo",
    TRUE                           ~ "Revisar estructura dimensional"
  )
)

message("  Vi: α = ", round(alpha_vi, 3), " ; ω = ", round(omega_vi, 3))
message("  CH: α = ", round(alpha_ch, 3), " ; ω = ", round(omega_ch, 3))

# ── Plano 2: asociación espacial IVEC_base ~ D7_alto (I de Moran) ─────────
# NOTA: el I de Moran requiere la matriz de pesos espaciales (queen contiguity
# sobre las celdas activas). Se calculará en 02_validacion_espacial.R una vez
# que el grid con geometría esté disponible. Aquí se guarda la tabla de
# estadísticos descriptivos preliminares.
message("  Plano 2 — correlación preliminar IVEC_base ~ D7_alto_prop...")
cor_ivec_d7 <- cor(sip_celda$ivec_base, sip_celda$d7_alto_prop,
                   use = "complete.obs", method = "pearson")
message("  Correlación de Pearson (preliminar, sin control espacial): ",
        round(cor_ivec_d7, 3))
message("  NOTA: el I de Moran (contraste formal H1) se calcula en",
        " 02_validacion_espacial.R")


# =============================================================================
# SECCIÓN 11 — TABLA DE DEPURACIÓN SECUENCIAL
# =============================================================================

message("\n── Sección 11: tabla de depuración ─────────────────────────────")

n_umbral_excluidos <- sip_geo |>
  group_by(GRD_ID) |> summarise(n = n()) |>
  filter(n < UMBRAL_CELDA) |> summarise(s = sum(n)) |> pull(s)

n_celdas_activas <- sip_celda |> summarise(s = sum(n_individuos)) |> pull(s)

tab_depuracion <- tibble::tibble(
  Etapa = c(
    "Registro total SIP (fecha extracción junio 2025)",
    "D2: fuera del ámbito CV (no residentes)",
    "Sin georreferenciación (ST_X/ST_Y nulos)",
    "Coordenadas fuera del bbox CV (ETRS89 UTM30N)",
    "Celdas excluidas por umbral mínimo (< 15 individuos)",
    "Individuos en celdas activas del IVEC_base"
  ),
  `N excluido` = c(
    NA_integer_,
    n_total - n_d2,
    n_nulos_geo,
    n_fuera_bbox,
    n_umbral_excluidos,
    NA_integer_
  ),
  `N retenido` = c(
    n_total,
    n_d2,
    n_d2 - n_nulos_geo,
    n_d2 - n_nulos_geo - n_fuera_bbox,
    n_celdas_activas,
    n_celdas_activas
  )
) |>
  mutate(
    `% sobre 5.365.233` = round(100 * `N retenido` / n_total, 2)
  )

print(tab_depuracion)


# =============================================================================
# SECCIÓN 12 — GUARDAR RESULTADOS
# =============================================================================

message("\n── Sección 12: guardando resultados ────────────────────────────")

# ivec_base.parquet — tabla de celdas activas con todos los valores
write_parquet(sip_celda, file.path(dir_out, "ivec_base.parquet"))
message("  Guardado: ivec_base.parquet  (",
        nrow(sip_celda), " celdas × ",
        ncol(sip_celda), " columnas)")

# tabla_depuracion.rds — para cap. 6 §sec-res-cobertura y Apéndice D §D.2
saveRDS(tab_depuracion,
        file.path(dir_out, "tabla_depuracion.rds"))
message("  Guardado: tabla_depuracion.rds")

# validacion_d7.rds — para cap. 6 §sec-res-validacion (planos 1 y 2)
validacion_d7 <- list(
  coherencia_interna  = tab_coherencia,
  cor_pearson_ivec_d7 = cor_ivec_d7,
  tabla_contraste_h1  = tibble::tibble(
    Estadístico = c(
      "Correlación de Pearson preliminar (IVEC_base ~ D7_alto_prop)",
      "I de Moran (IVEC_base ~ D7) — pendiente 02_validacion_espacial.R",
      "Valor esperado bajo H0",
      "Z-score",
      "p-valor (bilateral)",
      "Decisión sobre H1"
    ),
    Valor = c(
      round(cor_ivec_d7, 4),
      rep("— pendiente —", 5)
    )
  )
)
saveRDS(validacion_d7, file.path(dir_out, "validacion_d7.rds"))
message("  Guardado: validacion_d7.rds")

message("\n══════════════════════════════════════════════════════════════")
message("  IVEC_base — pipeline completado")
message("  Celdas activas: ", format(nrow(sip_celda), big.mark = "."))
message("  Siguiente paso: ejecutar 02_validacion_espacial.R")
message("                  para el contraste formal de H1 (I de Moran)")
message("══════════════════════════════════════════════════════════════\n")
