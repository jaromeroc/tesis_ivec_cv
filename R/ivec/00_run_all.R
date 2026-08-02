# =============================================================================
# 00_run_all.R — Script maestro del pipeline del IVEC (capitulo 6)
# =============================================================================
# Ejecuta EN ORDEN todos los scripts de R/ivec/ que generan los datos, las
# validaciones y la cartografia que alimentan el capitulo sexto
# (06-resultados.Rmd).
#
# USO:
#   Abrir el proyecto en RStudio (o situar el working directory en la raiz del
#   proyecto) y ejecutar:
#       source("R/ivec/00_run_all.R")
#
#   Cada script se ejecuta en un entorno (new.env) independiente dentro de la
#   misma sesion de R: comparte los paquetes ya cargados pero evita que las
#   variables de un script contaminen al siguiente. La comunicacion entre
#   scripts es por ficheros en disco (parquet / rds / gpkg), no por objetos
#   en memoria, de modo que esta aislacion es segura.
#
# EXCLUIDOS A PROPOSITO:
#   - 10_fig_zoom_vegabaja.py  -> figura Python; imagenes ya definitivas.
#   - El re-knit del capitulo (bookdown) es un paso posterior e independiente.
#
# ORDEN Y DEPENDENCIAS (el orden numerico respeta todas las dependencias):
#   01 base                      -> ivec_base.parquet (base de todo lo demas)
#   02 validacion espacial base  -> Moran/LISA + superposicion PATRICOVA
#   03 sensibilidad base (R6)
#   04 contextual                -> ivec_contextual.parquet (hereda universo base)
#   05 validacion contextual     -> lee base + contextual
#   06 mapas/tablas contextual   -> lee contextual + su validacion
#   07 estructural               -> ivec_estructural.parquet (k-means K=5)
#   08 validacion estructural    -> RMEs FISABIO / VEUS (discriminante)
#   09 mapas/tablas estructural
#   11 mapas base                -> regenera fig_mapa_ch_star a 4200x4800
#   12 cruce celda->municipio    -> alimenta las tablas 6.6 y 6.7 (necesita 07)
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) {
  stop("El paquete 'here' es necesario. Instalalo con install.packages('here').")
}

root     <- here::here()
ivec_dir <- file.path(root, "R", "ivec")

# Asegura que el working directory es la raiz del proyecto durante la ejecucion
# (los scripts usan here::here(), pero esto lo hace robusto ante cualquier wd).
old_wd <- getwd()
setwd(root)
on.exit(setwd(old_wd), add = TRUE)

scripts <- c(
  "01_ivec_base.R",
  "02_validacion_espacial.R",
  "03_analisis_sensibilidad.R",
  "04_ivec_contextual.R",
  "05_validacion_externa_contextual.R",
  "06_mapas_tablas_contextual.R",
  "07_ivec_estructural.R",
  "08_validacion_externa_estructural.R",
  "09_mapas_tablas_estructural.R",
  "11_mapas_base.R",
  "12_cruce_grd_municipio.R"
)

cat("=============================================================\n")
cat(sprintf(" Pipeline IVEC -- cap. 6 | %d scripts | %s\n",
            length(scripts), format(Sys.time(), "%Y-%m-%d %H:%M")))
cat(sprintf(" Raiz del proyecto: %s\n", root))
cat("=============================================================\n\n")

t0 <- Sys.time()

for (i in seq_along(scripts)) {
  s    <- scripts[i]
  path <- file.path(ivec_dir, s)

  if (!file.exists(path)) {
    stop(sprintf("No se encuentra el script: %s", path))
  }

  cat(sprintf("[%2d/%d] >>> %s\n", i, length(scripts), s))
  utils::flush.console()
  ti <- Sys.time()

  ok <- tryCatch({
    source(path, local = new.env(), chdir = FALSE, echo = FALSE)
    TRUE
  }, error = function(e) {
    message(sprintf("\n  X ERROR en %s:\n    %s", s, conditionMessage(e)))
    FALSE
  })

  if (!ok) {
    stop(sprintf("Pipeline DETENIDO en '%s' (script %d de %d). ",
                 s, i, length(scripts)),
         "Corrige el error y vuelve a lanzar el maestro.")
  }

  dt <- as.numeric(difftime(Sys.time(), ti, units = "secs"))
  cat(sprintf("        OK  %s  (%.1f s)\n\n", s, dt))
  utils::flush.console()
}

dt_total <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
cat("=============================================================\n")
cat(sprintf(" Pipeline completado en %.1f min.\n", dt_total))
cat(" Siguiente paso: re-knit de 06-resultados.Rmd (bookdown).\n")
cat("=============================================================\n")
