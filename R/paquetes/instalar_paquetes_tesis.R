# =============================================================================
# SCRIPT DE INSTALACIÓN DE PAQUETES - TESIS DOCTORAL
# Vulnerabilidad Estructural y Contextual - Comunitat Valenciana
# =============================================================================
# Ejecutar este script en la consola de RStudio:
#   source("instalar_paquetes_tesis.R")
# =============================================================================

# --- Función auxiliar para instalar solo si no están ya instalados ----------
instalar_si_falta <- function(paquetes) {
  nuevos <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
  if (length(nuevos) > 0) {
    message("Instalando: ", paste(nuevos, collapse = ", "))
    install.packages(nuevos, dependencies = TRUE, repos = "https://cloud.r-project.org")
  } else {
    message("Ya instalados: ", paste(paquetes, collapse = ", "))
  }
}


# =============================================================================
# 1. GESTIÓN Y MANIPULACIÓN DE DATOS
# =============================================================================
message("\n>>> [1/5] Instalando paquetes de gestión de datos...")

pkgs_datos <- c(
  "tidyverse",   # Ecosistema completo: dplyr, ggplot2, tidyr, purrr, readr, stringr
  "readxl",      # Leer archivos Excel (.xlsx, .xls)
  "openxlsx",    # Escribir archivos Excel
  "haven",       # Importar datos SPSS, Stata, SAS
  "janitor",     # Limpieza de nombres y datos sucios
  "skimr",       # Resúmenes estadísticos rápidos
  "Hmisc",       # Utilidades estadísticas varias
  "lubridate",   # Manejo de fechas
  "arrow"        # Lectura/escritura de archivos Arrow/Parquet (pipeline SIP)
)

instalar_si_falta(pkgs_datos)


# =============================================================================
# 2. ANÁLISIS ESTADÍSTICO Y CONSTRUCCIÓN DE ÍNDICES
# =============================================================================
message("\n>>> [2/5] Instalando paquetes de análisis estadístico...")

pkgs_estadistica <- c(
  # Análisis multivariante
  "psych",         # Análisis factorial, alpha de Cronbach, PCA
  "FactoMineR",    # ACP, ACM, AFC, cluster jerárquico
  "factoextra",    # Visualización de análisis multivariante
  "corrplot",      # Visualización de matrices de correlación
  "GPArotation",   # Rotaciones para análisis factorial

  # Índices compuestos y ponderación
  "weights",       # Ponderación de datos
  "compindexR",    # Índices compuestos

  # Clustering y clasificación
  "cluster",       # Algoritmos de cluster (PAM, CLARA, etc.)
  "NbClust",       # Número óptimo de clusters
  "mclust",        # Modelos de mezcla gaussiana

  # Modelos de regresión y diagnóstico
  "car",           # Diagnóstico de regresión, VIF, test de hipótesis
  "lmtest",        # Tests de hipótesis para regresión
  "MASS",          # Regresión robusta, distribuciones

  # Modelos de ecuaciones estructurales
  "lavaan",        # SEM (Structural Equation Modeling)
  "semPlot",       # Visualización de modelos SEM

  # Tests de normalidad y no paramétricos
  "nortest",       # Tests de normalidad (Anderson-Darling, etc.)
  "coin",          # Tests no paramétricos exactos

  # Series temporales
  "zoo",           # Medias móviles y series irregulares (rollapply — análisis bibliométrico)

  # Análisis de redes
  "igraph",        # Construcción y análisis de grafos (redes KW y CR del Apéndice B)

  # Medidas de asociación
  "vcd",           # V de Cramér y tablas de contingencia

  # Fiabilidad y validez de escalas
  "ltm",           # Teoría de respuesta al ítem
  "mokken"         # Escalas de Mokken (análisis de ítems no paramétrico)
)

instalar_si_falta(pkgs_estadistica)


# =============================================================================
# 3. ANÁLISIS ESPACIAL Y DEPENDENCIA TERRITORIAL
# =============================================================================
message("\n>>> [3/5] Instalando paquetes de análisis espacial...")

pkgs_espacial <- c(
  # Dependencia espacial (imprescindible para análisis territorial)
  "spdep",         # Matrices de contigüidad, Moran, LISA, correlogramas
  "spatialreg",    # Modelos de regresión espacial (SAR, SEM, SDM)

  # Datos vectoriales y raster
  "sf",            # Simple Features: manejo de shapefiles, GeoJSON, etc.
  "terra",         # Datos raster (sustituto moderno de 'raster')
  "stars",         # Datos espacio-temporales raster

  # Geocodificación y datos externos
  "osmdata",       # Descarga datos de OpenStreetMap
  "tidygeocoder",  # Geocodificación de direcciones

  # Estadística zonal y análisis de áreas
  "exactextractr", # Extracción exacta de estadísticas raster por polígono
  "areal"          # Interpolación areal entre unidades espaciales
)

instalar_si_falta(pkgs_espacial)


# =============================================================================
# 4. CARTOGRAFÍA Y VISUALIZACIÓN (ESTILO QGIS)
# =============================================================================
message("\n>>> [4/5] Instalando paquetes de cartografía y visualización...")

pkgs_mapas <- c(
  # Mapas temáticos (el más similar a QGIS en R)
  "tmap",          # Mapas temáticos: coropletas, puntos, raster — estilo QGIS
  "tmaptools",     # Herramientas complementarias para tmap

  # Mapas interactivos
  "leaflet",       # Mapas interactivos web (como Google Maps)
  "leaflet.extras",# Extensiones de leaflet (heatmaps, clusters, etc.)
  "mapview",       # Visualización interactiva rápida de objetos sf

  # Integración cartográfica con ggplot2
  "ggspatial",     # Escala gráfica, rosa de los vientos, capas espaciales en ggplot2
  "ggmap",         # Mapas base de Google/Stamen en ggplot2

  # Paletas de color para cartografía
  "RColorBrewer",  # Paletas ColorBrewer (secuenciales, divergentes, cualitativas)
  "viridis",       # Paletas perceptualmente uniformes (accesibles para daltónicos)
  "scico",         # Paletas científicas de Fabio Crameri

  # Datos cartográficos de referencia
  "rnaturalearth",    # Capas naturales (países, ríos, costas)
  "rnaturalearthdata",# Datos adicionales de Natural Earth
  "mapSpain",         # Capas cartográficas de España (municipios, provincias, CCAA)

  # Diseño editorial de mapas
  "cowplot",       # Composición de figuras múltiples
  "patchwork",     # Combinar gráficos ggplot2
  "ggthemes",      # Temas adicionales para ggplot2
  "ggalluvial"     # Diagramas aluviales/Sankey (flujos entre categorías)
)

instalar_si_falta(pkgs_mapas)


# =============================================================================
# 5. REPRODUCIBILIDAD Y ESCRITURA ACADÉMICA (BOOKDOWN / RMARKDOWN)
# =============================================================================
message("\n>>> [5/5] Instalando paquetes de escritura académica y reproducibilidad...")

pkgs_escritura <- c(
  "bookdown",      # Escritura de tesis en formato libro (PDF, HTML, Word)
  "rmarkdown",     # Documentos dinámicos
  "knitr",         # Motor de compilación de chunks de código
  "kableExtra",    # Tablas académicas con formato avanzado
  "flextable",     # Tablas para documentos Word y PowerPoint
  "gt",            # Tablas de publicación con diseño moderno
  "tinytex",       # Gestor ligero de LaTeX (para compilar PDF con XeLaTeX)
  "here",          # Rutas de archivo relativas y reproducibles
  "renv"           # Gestión de entornos reproducibles de R
)

instalar_si_falta(pkgs_escritura)


# =============================================================================
# VERIFICACIÓN FINAL — Comprobar que todos los paquetes cargan sin errores
# =============================================================================
message("\n>>> Verificando que todos los paquetes se cargan correctamente...\n")

todos_los_paquetes <- c(
  pkgs_datos, pkgs_estadistica, pkgs_espacial, pkgs_mapas, pkgs_escritura
)

# Paquetes que pueden tener conflictos de nombres — cargar con precaución
excluir_de_carga_masiva <- c("MASS", "car", "Hmisc")  # Pueden enmascarar funciones base

resultados <- sapply(todos_los_paquetes, function(pkg) {
  if (pkg %in% excluir_de_carga_masiva) {
    tryCatch({
      requireNamespace(pkg, quietly = TRUE)
      return("OK (namespace)")
    }, error = function(e) return(paste("ERROR:", e$message)))
  } else {
    tryCatch({
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      return("OK")
    }, error = function(e) return(paste("ERROR:", e$message)))
  }
})

# Mostrar resumen
df_resultado <- data.frame(
  Paquete = names(resultados),
  Estado   = unname(resultados),
  stringsAsFactors = FALSE
)

cat("\n========================================================\n")
cat("  RESUMEN DE INSTALACIÓN\n")
cat("========================================================\n")
cat(sprintf("  Paquetes correctos : %d\n", sum(grepl("^OK", df_resultado$Estado))))
cat(sprintf("  Paquetes con error : %d\n", sum(grepl("^ERROR", df_resultado$Estado))))
cat("========================================================\n\n")

# Mostrar solo los que fallaron
errores <- df_resultado[grepl("^ERROR", df_resultado$Estado), ]
if (nrow(errores) > 0) {
  cat("Paquetes que requieren atención:\n")
  print(errores)
  cat("\nSugerencia: instala las dependencias del sistema que falten y\n")
  cat("vuelve a ejecutar install.packages() para los paquetes indicados.\n")
} else {
  cat("¡Todo correcto! Todos los paquetes están disponibles.\n")
  cat("Tu entorno está listo para análisis estadístico y cartografía.\n\n")
  cat("Paquetes clave disponibles:\n")
  cat("  • Datos      : tidyverse, readxl, janitor, skimr, arrow\n")
  cat("  • Estadística: psych, FactoMineR, lavaan, zoo, igraph, vcd\n")
  cat("  • Cartografía: sf, tmap, leaflet, mapSpain, ggalluvial\n")
  cat("  • Escritura  : bookdown, knitr, kableExtra, flextable, tinytex\n")
}

# Instalar TinyTeX si no hay LaTeX (necesario para compilar PDF con XeLaTeX)
if (!tinytex::is_tinytex() && Sys.which("xelatex") == "") {
  message("\nNo se detectó XeLaTeX. Instalando TinyTeX para compilar PDF...")
  tinytex::install_tinytex()
  message("TinyTeX instalado. Reinicia RStudio antes de compilar el PDF de la tesis.")
} else {
  message("\nLaTeX detectado correctamente. Listo para compilar con XeLaTeX.")
}

message("\n=== Instalación completada ===\n")

