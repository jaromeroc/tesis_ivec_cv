# Tesis doctoral — IVEC: Instrumento de Valoración de la Vulnerabilidad Estructural y Contextual de la Comunitat Valenciana

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21759660.svg)](https://doi.org/10.5281/zenodo.21759660)


Repositorio reproducible de la tesis doctoral de Juan Antonio Romero Crespo (Universitat de València). El proyecto construye el **IVEC** (*Instrumento de Valoración de la Vulnerabilidad Estructural y Contextual*), un instrumento de medición de la vulnerabilidad social en la Comunitat Valenciana que puede **cerrar la Brecha Protección Civil / Protección Social desde dentro de la arquitectura existente**, sin exigir la fusión institucional de los dos sistemas.

---

## Estructura de la tesis

### Frontmatter (no numerado)

| Archivo | Contenido |
|---|---|
| `index.Rmd` | Portada y metadatos del libro |
| `00b-certificado.Rmd` | Certificado de dirección de la tesis |
| `00c-financiacion.Rmd` | Declaración de financiación |
| `00d-resumenes.Rmd` | Resúmenes trilingües (valenciano, español, inglés) |
| `00e-justificacion.Rmd` | Motivación, pertinencia, déficits del campo, alcance y limitaciones |
| `00f-ods.Rmd` | Vinculación del IVEC a los ODS 1, 3, 10, 11, 13 y 16 |
| `00g-agradecimientos.Rmd` | Agradecimientos |
| `00h-indice.Rmd` | Índice general |
| `00i-glosario.Rmd` | Glosario de términos canónicos |
| `00j-abreviaturas.Rmd` | Glosario de siglas |
| `00k-resume-frances.Rmd` | Résumé en français (mención internacional de doctorado) |

### Cuerpo principal

| Capítulo | Archivo | Contenido |
|---|---|---|
| Cap. 1 | `01-intro.Rmd` | El problema, posicionamiento teórico, pregunta de investigación, hipótesis, objetivos y estructura |
| Cap. 2 | `02-estado-cuestion.Rmd` | Análisis bibliométrico (corpus WoS 1980–2024, 22.098 docs); cinco tradiciones; siete requisitos de diseño R1–R7 |
| Cap. 3 | `03-marco-teorico.Rmd` | PAR, MOVE, producción social de la vulnerabilidad; arquitectura ideal de indicadores de los tres módulos |
| Cap. 4 | `04-marco-normativo.Rmd` | Marco de Sendai, Agenda 2030, legislación valenciana; Brecha Protección Civil / Protección Social como consecuencia normativa |
| Cap. 5 | `05-metodologia.Rmd` | Diseño de investigación; SIP/APSIG; operacionalización del IVEC; marco ético; validación |
| Cap. 6 | `06-resultados.Rmd` | Resultados del IVEC |
| Cap. 7 | `07-discusion.Rmd` | Discusión |
| Cap. 8 | `08-conclusiones.Rmd` | Conclusiones |

### Apéndices

| Apéndice | Archivo | Contenido |
|---|---|---|
| — | `A0-portada-apendices.Rmd` | Portada del bloque de apéndices |
| A | `A1-corpus.Rmd` | Pipeline técnico de construcción del corpus WoS (Fases 1–8) |
| B | `A2-biblio.Rmd` | Análisis bibliométrico completo (series temporales, coocurrencias KW, cocitación CR, integración KW↔CR) |
| C | `A3-sip.Rmd` | Pipeline reproducible del tratamiento de la base APSIG/SIP (Módulos 1–9) |
| D | `A4-decisiones-metodologicas.Rmd` | Decisiones metodológicas del IVEC: normalización, depuración, alfa de Cronbach y omega de McDonald por dimensión, matrices de correlación |
| E | `A5-decisiones-operativas.Rmd` | Decisiones operativas: asignación al grid de 1 km², escala territorial de IVEC_estructural y cobertura de indicadores de IVEC_contextual |
| F | `A6-fuentes.Rmd` | Ficha consolidada de metadatos y trazabilidad fuente–fichero de los indicadores del IVEC, con tres tablas operativas (base, contextual, estructural) |

---

## El sistema IVEC

El IVEC se organiza en **tres módulos diferenciados por el mecanismo causal que cada uno opera** (individual-hogar, contextual-comunitario, estructural-institucional). La dimensión escalar es propiedad emergente: los dos módulos micro (base y contextual) operan a 1 km² nativamente; el módulo macro (estructural) opera a municipio/comarca nativamente con proyección al grid.

### IVEC_base
Módulo individual/hogar a resolución de cuadrícula de **1 km² LAEA**, construido sobre los microdatos del SIP (5.287.051 residentes en la CV a junio 2025). Arquitectura V1: 14 indicadores (8 Vi + 6 CH) sobre 3.124 celdas activas con umbral ≥ 15 residentes por celda. Arquitectura ideal: 24 indicadores (14 Vi + 10 CH); los 10 pendientes requieren interoperabilidad SAAD, IVASS, SICD, SIUSS, Catastro y datos de redes.

- **Vi** — vulnerabilidad individual
- **CH** — composición del hogar

### IVEC_contextual
Módulo a resolución de cuadrícula de **1 km² LAEA**, en simetría con el IVEC_base. Arquitectura ideal: 14 indicadores teóricos (VC físico-residencial 4 + VC demográfico-poblacional 5 + CS 5). En V1 hay **13 indicadores activos** (4 físico-residenciales + 5 demográfico-poblacionales + 4 CS); `cs_asociativo` queda diferido a V2 por el bloqueo de la geocodificación catastral. La fórmula compensadora completa `IVEC_contextual = ½·VC* + ½·(1−CS*)` opera desde V1.

- **VC** — vulnerabilidad contextual (sub-dimensiones físico-residencial y demográfico-poblacional)
- **CS** — capacidades sociales del entorno

### IVEC_estructural
Módulo a escala **municipal** sobre los 542 municipios de la Comunitat Valenciana, con proyección cartográfica al grid. Arquitectura V1: **12 indicadores activos** (8 VE + 4 CAD). VE* se deriva de una clusterización **k-means con K = 5** sobre los 8 indicadores VE estandarizados; los cinco clústeres se identifican con perfiles PAR y se ordenan por la mediana del índice compuesto de severidad demográfica.

- **VE** — vulnerabilidad estructural (modelo productivo TGSS + envejecimiento PEGV + estructura migratoria)
- **CAD** — capacidades de adaptación y transformación estructural del municipio (BDEL: deuda viva, gasto en servicios sociales, inversiones y subvenciones per cápita)

---

## Diseño de investigación

La investigación formula **una proposición de diseño** y contrasta **dos hipótesis empíricas**:

- **Proposición de diseño:** el IVEC satisface los siete requisitos de diseño R1–R7.
- **H1 (validez convergente y discriminante por módulo):** el IVEC_base presenta validez convergente con la estratificación D7 del SIP (criterio individual homólogo), positiva, significativa y de magnitud moderada. El IVEC_contextual, de naturaleza territorial, presenta validez convergente con criterios territoriales independientes —la renta neta media del Atlas de Distribución de Renta del INE y, principalmente, el índice de vulnerabilidad urbana del VEUS-CV— y validez **discriminante** respecto a D7 (la ausencia de convergencia con un criterio individual-clínico es la consecuencia esperada de que el módulo mida el entorno y no al sujeto). La validez discriminante del módulo estructural (no convergencia con las RMEs FISABIO ni con el VEUS) se vincula a H2/R2.
- **H2 (diferenciación territorial):** el IVEC produce una diferenciación territorial de la vulnerabilidad que los indicadores municipales convencionales no capturan.

Los siete requisitos R1–R7 se formulan en el Cap. 2, se fundamentan teóricamente en el Cap. 3, se validan normativamente en el Cap. 4 y se operacionalizan en el Cap. 5. R1 (posición ontológica declarada sobre qué es la vulnerabilidad) y R2 (arquitectura tridimensional y diferenciación analítica por mecanismo causal) son los requisitos fundacionales.

---

## Resultados clave (V1, mayo 2026)

La primera versión operativa del IVEC se ha implementado sobre el conjunto de fuentes accesibles tras la verificación del diccionario APSIG completo del SIP. **Todas las cifras de esta sección han sido verificadas exactamente contra el output de consola de la re-ejecución completa del pipeline** (la fuente de verdad del proyecto), tras la corrección del signo de CH y la unificación de la definición de D7-alta de mayo de 2026.

### IVEC_base
Cómputo sobre **3.124 celdas activas** (umbral ≥ 15 residentes) y 14 indicadores (8 Vi + 6 CH).

- ρ de Spearman (IVEC_base × D7-alto): **0,2794** (p < 0,001, N = 3.124); ρ(Vi* × D7) = 0,3318; ρ(CH* × D7) = −0,1546
- I de Moran global: **0,5193** (p = 0,001, 999 permutaciones, N = 3.113 celdas con geometría)
- Coherencia interna Vi: α = **0,751**; ω = **0,765**. CH: α = **0,673**; ω = **0,709**
- Clústeres LISA: **HH 306 (9,83 %)**, **LL 183 (5,88 %)**, HL 12, LH 29, NS 2.583. Los HH se concentran en el litoral alicantino (Marina Alta, Baix Segura/Vega Baja, Marina Baixa, Baix Vinalopó)
- Superposición con peligrosidad alta PATRICOVA: **56 celdas HH × peligrosidad alta** (subconjunto de máximo riesgo compuesto)
- Acumulación, no compensación: r(Vi*, 1−CH*) = **+0,779**
- Sensibilidad (R6): **72,3 %** de las celdas conserva el mismo quintil entre cinco especificaciones (98,8 % dentro de ±1 quintil); 38 celdas (1,22 %) de incertidumbre alta; 569 celdas (18,21 %) de acumulación dimensional

### IVEC_contextual
Cómputo sobre las mismas **3.124 celdas** con **13 indicadores activos** de los 14 de la tabla operativa (4 físico-residenciales + 5 demográfico-poblacionales + 4 CS); `cs_asociativo` diferido a V2. Fórmula compensadora completa `½·VC* + ½·(1−CS*)`.

- VC* media 0,585 (dt 0,188); CS* media 0,396 (dt 0,236); IVEC_contextual media 0,594 / mediana 0,596
- Validez **discriminante** respecto a D7-alto: ρ = **0,0831** (negligible; criterio individual-clínico). Convergencia territorial: ρ con el índice `iv` del VEUS-CV = **0,3418** (criterio convergente principal); ρ externo con renta neta media (Atlas INE 2023) = **−0,2364**. Coherencia inter-módulos (× IVEC_base) ρ = **0,644**
- Coherencia interna VC demográfico-poblacional (reflectiva): α = 0,639; ω = 0,673 (las sub-dimensiones formativas físico-residencial y CS se reportan descriptivamente)

### IVEC_estructural
Módulo macro **implementado en V1** sobre los **542 municipios** CV; VE* derivado de k-means **K = 5**.

- Cinco perfiles PAR (mediana de severidad demográfica descendente): agrario-interior despoblado extremo (VE\*=0,9; n=23), monoproducción estacional envejecida (0,7; n=90), especializado-productivo intermedio (0,5; n=50), atractor de inmigración exterior (0,3; n=52) y diversificado de baja severidad estructural (0,1; n=327)
- Escalar IVEC_estructural: media **0,409**, mediana **0,388**, mín 0,068, máx 0,836; CAD* media 0,472
- Validación de plano 2: **validez discriminante** (no convergente), coherente con R2/H2 — ρ(IVEC_estructural × RMEs FISABIO/MEDEA) = **−0,06** (n.s.); ρ(VE* × RMEs) = **−0,33** (gradiente inverso); VEUS-CV agregado a municipio ρ ≈ 0 (ortogonal)

### Hallazgo sustantivo
La lectura conjunta de los tres módulos arroja el principal hallazgo de la investigación: la alta vulnerabilidad social se concentra en el **litoral alicantino envejecido y de fuerte residencia extranjera** —no en las periferias industriales—, y la zona cero de la DANA del 29 de octubre de 2024 (l'Horta Sud, 224 fallecidos confirmados) presenta vulnerabilidad social **moderada** (ningún clúster HH). La magnitud del desastre se explica por **exposición física extrema** y **fallo de las capacidades de reacción** (excluidas del IVEC por diseño), no por vulnerabilidad social estructural previa. El riesgo compuesto real (vulnerabilidad alta × peligrosidad alta) se localiza en la llanura del Segura (Vega Baja) y los valles de la Marina.

---

## Estructura del repositorio

```
.
├── index.Rmd                    # Portada y metadatos
├── 00*.Rmd                      # Frontmatter (certificado, financiación, resúmenes, justificación, ODS, agradecimientos, índice, glosario, abreviaturas, résumé)
├── 01-intro.Rmd … 08-conclusiones.Rmd   # Cuerpo principal
├── 09-bibliografia.Rmd
├── A0-portada-apendices.Rmd … A6-fuentes.Rmd   # Apéndices
├── R/
│   ├── apendices/               # Pipelines reproducibles de los apéndices técnicos (corpus, decisiones metodológicas y operativas)
│   ├── bibliometria/            # Scripts del análisis bibliométrico
│   ├── descarga/                # Scripts de descarga de fuentes externas
│   ├── ivec/                    # Pipeline de cómputo del IVEC (base, contextual, estructural, validación, sensibilidad y mapas)
│   ├── paquetes/                # Instalación de dependencias (instalar_paquetes_tesis.R)
│   ├── renderizado/             # Compilación bookdown y helper de tablas Word
│   │   ├── Renderizar.R         # Compila a PDF y a Word
│   │   ├── Renderizar_Word.R    # Compila solo a Word (.docx)
│   │   └── tesis_tabla.R        # Helper de tablas académicas para salida Word (flextable)
│   └── sip/                     # Scripts de tratamiento de la base SIP/APSIG
├── data/
│   ├── SIP/                     # Microdatos del SIP/APSIG (acceso restringido)
│   ├── base/                    # Datos del módulo IVEC_base y resultados intermedios
│   ├── contextual/              # Datos del módulo IVEC_contextual (Catastro, Censo 2021, Observatori Hàbitat, RECESSO, OSM, etc.)
│   ├── estructural/             # Datos del módulo IVEC_estructural (TGSS, PEGV, BDEL, Atlas INE, RMEs FISABIO)
│   ├── mallas/                  # Cartografía de referencia (común a todos los módulos)
│   │   ├── malla_laea/          # Cuadrícula ETRS89-LAEA 1×1 km (EPSG:3035)
│   │   ├── malla_administrativa/  # Delimitaciones provinciales, comarcales, municipales y secciones censales
│   │   └── malla_patricova/     # Zonas de peligrosidad de inundación (PATRICOVA)
│   ├── WoS/                     # Exportaciones del corpus Web of Science
│   ├── Indicadores/             # Fuentes administrativas de indicadores
│   └── Literatura/              # Referencias cartográficas auxiliares (GRID Eurostat, shapefiles legados)
├── outputs/
│   ├── bibliometria/            # Artefactos del análisis bibliométrico
│   ├── corpus/                  # Artefactos del pipeline del corpus
│   ├── figures/                 # Figuras generadas por capítulo (cap-1, cap-2, …)
│   ├── integracion/             # Resultados de integración espacial
│   └── vosviewer/               # Mapas exportados desde VOSviewer
├── plantillas/
│   └── plantilla.docx           # Plantilla Word para compilación en .docx
├── preamble.tex                 # Preámbulo XeLaTeX personalizado
├── references.bib               # Base bibliográfica (BibTeX)
├── _bookdown.yml                # Configuración Bookdown (orden de archivos)
├── _output.yml                  # Configuración de formatos de salida (PDF/Word)
└── _common.R                    # Script de inicialización ejecutado antes de cada capítulo
```

### Lógica del repositorio

La organización del repositorio responde a tres principios de diseño que orientan toda la tesis: separación entre adquisición y procesamiento de datos, simetría entre entradas y salidas, y trazabilidad reproducible de cada decisión metodológica.

**Archivos del manuscrito en la raíz.** Los `.Rmd` del cuerpo de la tesis (`index.Rmd`, capítulos `01–08`, apéndices `A0–A6`, frontmatter `00b–00k`) viven directamente en la raíz porque son las unidades que `bookdown` ensambla en cada compilación. Mantenerlos al nivel superior simplifica la configuración del libro (el `_bookdown.yml` enumera los archivos sin rutas relativas) y los hace inmediatamente accesibles para los flujos de revisión cruzada entre capítulos, que son frecuentes durante la redacción.

**`R/` — código organizado por flujo, no por fuente.** Las siete subcarpetas de `R/` no se separan por origen de datos sino por momento del flujo de trabajo. `descarga/` documenta de manera reproducible cómo se obtuvieron las fuentes externas; `sip/` aplica los Módulos 1–9 de tratamiento de la base APSIG; `ivec/` ejecuta el cómputo de los módulos del instrumento (base, contextual, estructural, validación espacial, sensibilidad y mapas) sobre los datos ya tratados; `bibliometria/` corre en paralelo el análisis del corpus WoS; `apendices/` reúne los pipelines reproducibles que documentan los apéndices técnicos (corpus, decisiones metodológicas y operativas); `renderizado/` agrupa los scripts de compilación bookdown y el helper de tablas Word; `paquetes/` aísla la instalación de dependencias. Esta organización pretende que cualquier auditor externo pueda recorrer el repositorio de izquierda a derecha (adquisición → tratamiento → cómputo → renderizado) sin tener que reconstruir mentalmente el orden.

**`data/` y `outputs/` — asimetría entre entradas y salidas.** `data/` contiene exclusivamente fuentes (microdatos SIP, indicadores administrativos, corpus WoS, cartografía); `outputs/` contiene exclusivamente artefactos generados (figuras del libro, exportaciones VOSviewer, resultados de integración espacial). Esta separación permite que los scripts de cómputo sean idempotentes en sus entradas y que cualquier salida pueda regenerarse desde cero sin riesgo de contaminación cruzada. Ambos directorios están excluidos del control de versiones por su volumen (~73 GB en `data/`); el repositorio público distribuye solo el código y los derivados ligeros que el pipeline permite reconstruir.

**`data/` por módulo y por escala.** Dentro de `data/`, la distinción entre `base/`, `contextual/` y `mallas/` no es por fuente externa sino por función dentro del IVEC: `base/` y `contextual/` agrupan los datos que alimentan cada módulo de cómputo respectivo, mientras que `mallas/` reúne la cartografía de referencia compartida entre módulos (cuadrícula LAEA, delimitaciones administrativas, zonas PATRICOVA). Los microdatos crudos del SIP residen aparte en `data/SIP/` por su régimen de acceso restringido y porque alimentan ambos módulos micro simultáneamente.

**Apéndices A–F como pipelines auditables.** Los seis apéndices no son contenido suplementario sino el registro técnico-documental del proceso. A1 y A2 documentan paso a paso la construcción y el análisis del corpus bibliométrico; A3 reproduce el tratamiento completo de la base APSIG; A4 y A5 registran las decisiones metodológicas y operativas del IVEC; A6 consolida los metadatos de fuentes e indicadores. Cada apéndice es legible de forma autónoma por un revisor que solo quiera auditar una pieza concreta del diseño sin pasar por el cuerpo principal.

---

## Compilación

El manuscrito se compila con **Bookdown** + **XeLaTeX**. El punto de entrada es `index.Rmd`. La secuencia de archivos está declarada en `_bookdown.yml`.

Desde RStudio, la forma recomendada es ejecutar `R/renderizado/Renderizar.R`:

```r
source("R/renderizado/Renderizar.R")
```

O directamente:

```r
bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book")
```

### Salida Word (para revisión)

```r
bookdown::render_book("index.Rmd", output_format = "bookdown::word_document2")
```

La salida Word usa la plantilla `plantillas/plantilla.docx` y el helper `R/renderizado/tesis_tabla.R` (basado en `flextable`) para el formato de tablas.

### Requisitos

- R ≥ 4.3
- Paquetes principales: `bookdown`, `knitr`, `kableExtra`, `flextable`, `tidyverse`, `here`, `sf`, `spdep`, `psych`
- Distribución LaTeX con XeLaTeX (recomendado: TinyTeX o TeX Live)
- Fuentes: Palatino Linotype (cuerpo), Source Code Pro (código)

Para instalar todos los paquetes necesarios:

```r
source("R/paquetes/instalar_paquetes_tesis.R")
```

---

## Datos y acceso

- **Corpus WoS:** 22.098 documentos (1980–2024). Los datos crudos no se distribuyen por licencia; el repositorio incluye los derivados normalizados generados por el pipeline del Apéndice A.
- **SIP (Sistema de Información Poblacional):** acceso restringido. Los microdatos fueron solicitados institucionalmente el 19 de mayo de 2025 a la Conselleria de Sanitat (Servicio de Aseguramiento Sanitario y SIP). La extracción facilitada contiene **5.365.233 individuos** registrados (junio 2025), de los cuales 5.287.051 son residentes en la CV y 78.182 son no residentes atendidos por el sistema sanitario valenciano. El repositorio no distribuye microdatos individuales; únicamente los resultados agregados a cuadrícula de 1 km² con umbral mínimo de 15 residentes por celda.
- **Base jurídica del tratamiento:** RGPD art. 89 (investigación científica de interés público) + LOPD-GDD (LO 3/2018).

---

## Financiación

Esta tesis doctoral es resultado de una investigación financiada por el programa **FPU** (*Formación de Profesorado Universitario*):

- **Ayuda predoctoral FPU** — ref. **FPU18/00637** — Ministerio de Ciencia, Innovación y Universidades.
- **Ayuda complementaria de movilidad** (Estancias Breves y Traslados Temporales, convocatoria 2021) — ref. **EST22/00406** — Ministerio de Educación y Formación Profesional. Financió una estancia de investigación de tres meses en Lyon (Francia), conducente a la mención internacional del doctorado.

---

## Licencia

Este repositorio combina dos tipos de contenido, con licencias diferenciadas:

| Contenido | Licencia |
|---|---|
| **Texto de la tesis, figuras y tablas** (los `.Rmd` del manuscrito, el PDF resultante y las figuras de `outputs/figures/`) | [**CC BY-NC-ND 4.0**](https://creativecommons.org/licenses/by-nc-nd/4.0/deed.es) — Reconocimiento · No Comercial · Sin Obra Derivada |
| **Código** (scripts de `R/`, chunks de cómputo y ficheros de configuración de la compilación) | [**MIT**](https://opensource.org/license/mit) |

**Texto, figuras y tablas — CC BY-NC-ND 4.0.** Se permite descargar, compartir y citar la obra siempre que se reconozca la autoría. **No** se autoriza el uso comercial ni la distribución de versiones modificadas del texto o del instrumento IVEC. Esta licencia maximiza la difusión académica a la vez que protege la integridad del instrumento, y es coherente con el depósito en RODERIC / TESEO.

**Código — MIT.** Los scripts de R que implementan el pipeline del IVEC se publican bajo licencia MIT para permitir su verificación, reejecución y reutilización sin fricción; la atribución se mantiene mediante la cita de la tesis.

> **Nota sobre las fuentes de datos.** Las licencias anteriores cubren el texto y el código de este repositorio, pero **no se extienden a las fuentes de datos de terceros** (microdatos SIP, corpus Web of Science, cartografía oficial e indicadores administrativos), que se rigen por sus propias condiciones de uso (véase la sección «Datos y acceso» y el Apéndice F).

### Cómo citar

> Romero Crespo, J. A. (2026). *La vulnerabilidad social como fenómeno estructural y contextual. Diseño del instrumento IVEC para la Comunitat Valenciana* [Tesis doctoral, Universitat de València]. RODERIC. https://hdl.handle.net/10550/128856

Depósito original: **25 de mayo de 2026**. Defensa: **31 de julio de 2026**. Esta versión corregida constituye el depósito definitivo posterior a la defensa.

Registro en TESEO (base de datos nacional de tesis): https://aplicaciones.ciencia.gob.es/teseo/#/tesis/330085/detalle

El repositorio y el código están archivados en Zenodo: https://doi.org/10.5281/zenodo.21759660 (DOI de concepto, apunta a la última versión).


---

## Correcciones incorporadas en la versión de depósito

Esta versión definitiva integra las correcciones surgidas de la evaluación del tribunal (en particular las observaciones del Dr. Rafael Temes) durante la defensa del **31 de julio de 2026**, y de una revisión sistemática posterior. **Ninguna corrección altera los resultados ni introduce análisis fabricados:** las incorporaciones metodológicas se apoyan en cálculos efectivamente ejecutados sobre los datos del proyecto.

**Nuevo análisis — Problema de la Unidad de Área Modificable (MAUP).** Se incorpora un análisis de sensibilidad a la resolución de la cuadrícula del IVEC_base a **tres escalas (0,5 km, 1 km y 2 km)**, atravesando introducción (declaración de limitación), metodología, discusión y un apéndice específico (§E.5, con tabla y figura). El resultado confirma la **robustez del ordenamiento** entre escalas (ρ ≈ 0,95–0,96; ~98 % de las celdas se mantiene dentro de ±1 quintil) y que la resolución de 1 km es la elección **conservadora** (la validez convergente con D7 alcanza su máximo a 1 km y la divergencia intramunicipal decrece monotónicamente al agregar). La exploración de otras zonaciones queda declarada como refinamiento futuro.

**Cierre del Análisis de Componentes Principales (ACP).** Se reporta el ACP prometido sobre la ponderación (§D.4): PC1 explica el **49,4 %** de la varianza con cargas todas positivas y ρ(IVEC, PC1) = **0,981**. Se argumenta que el ACP es **diagnóstico y no constructivo** —se mantienen los pesos equiponderados— porque el requisito R2 exige diferenciar analíticamente la vulnerabilidad individual (Vi) de las capacidades del hogar (CH), y CH tiene naturaleza formativa (la puntuación se asigna por tipo de hogar).

**Reencuadre del análisis de divergencia territorial.** Se precisa que la divergencia intramunicipal compara la puntuación de celda con la **media del propio IVEC a escala municipal** (no con indicadores municipales externos), se reconcilian los capítulos afectados y se incorporan al texto las cifras del contraste (183 / 78 celdas), diferenciándolas de los 183 clústeres LL.

**Erratas puntuales.** Corrección del sentido del sesgo por georreferenciación (coherente entre los capítulos 5, 8 y el résumé en francés); `media`→`mediana` donde correspondía; referencia cruzada colgante; y formato numérico español (coma decimal) en el código de las tablas.

**Depuración de repeticiones.** Barrido y consolidación de solapamientos en discusión, conclusiones y los apéndices metodológicos (A4/A5), remitiendo a la sección canónica en cada caso.

**Unificación de estilo y bibliografía.** `cuadro`→`tabla`; `dana` en minúscula (incluido el résumé francés); `periodo` sin tilde; `shocks` (equivalente a «impactos», no «choques»); «V de Cramer» sin tilde en español y «Cramér» con tilde en francés; corrección de citas que renderizaban «y otros / & otros» (errores en el campo *author* del `.bib`) y protección de mayúsculas en títulos (`{España}`, `{Valencia}`, etc.).

**Estructura y numeración.** Homogeneización de la numeración de los apéndices en el índice (los apéndices D y F no arrancan en `.0`).

**Verificación de cifras.** Contraste de recuentos frente a la fuente real del proyecto (p. ej. «278 de 306» celdas HH y su desglose comarcal), confirmando su exactitud.

