# `R/descarga/` — Adquisición de fuentes del IVEC

> **Nota de reproducibilidad.** La adquisición de las fuentes del instrumento en V1 es
> **mixta**: una parte se descarga de forma **automática** desde APIs y endpoints abiertos
> que funcionan de manera programática (OpenStreetMap, portales de datos abiertos, repositorios
> públicos), y otra parte se obtuvo **manualmente** —descargándola a mano del portal del
> organismo— porque la fuente es de acceso restringido, no ofrece descarga programática
> estable, o requiere navegación interactiva. Los scripts de esta carpeta documentan, para
> cada fuente, su procedencia, su método de obtención y el procesamiento aplicado.
>
> Para reproducir la adquisición: los scripts marcados como **automáticos** descargan la
> fuente al ejecutarse; para las fuentes **manuales** hay que obtener primero el fichero del
> portal indicado y colocarlo en la subcarpeta de `data/` correspondiente antes de ejecutar el
> script que lo procesa. La carpeta `data/` está excluida del control de versiones por su
> volumen y, en el caso del SIP, por su régimen de acceso restringido. El cuadro definitivo de
> metadatos fuente–fichero está en el **Apéndice F** de la tesis (`A6-fuentes.Rmd`).
>
> Aviso: los endpoints de descarga automática (URLs de OSM, portales de datos abiertos, INE,
> GitHub) pueden cambiar o dejar de estar disponibles con el tiempo; en ese caso la fuente
> deberá obtenerse manualmente desde el portal del organismo.

## Fuentes por método de adquisición

### Descarga automática (el script la realiza al ejecutarse)

| Fuente | Organismo / endpoint | Carpeta destino | Script |
|---|---|---|---|
| Paradas de transporte público | OpenStreetMap — Overpass API | `data/contextual/` | `17_descarga_contextual_cs.R` |
| Centros cívicos / culturales | GeoJSON Diputació de Castelló + Ajuntament de València + OSM | `data/contextual/` | `17_descarga_contextual_cs.R` |
| Densidad asociativa y ONGs | Dades Obertes GVA (CSV) | `data/contextual/` | `17_descarga_contextual_cs.R` |
| Bibliotecas | Directorio de Bibliotecas — Ministerio de Cultura (TXT) | `data/contextual/` | `17_descarga_contextual_cs.R` |
| Atlas demográfico (30877) y de renta (31106) | INE — endpoint `jaxiT3` | `data/estructural/atlas_demografico/`, `atlas_renta_municipal/` | `18_descarga_estructural.R` |
| Liquidaciones presupuestarias (BDEL) | gobierto-budgets-data (GitHub, MHFP) | `data/estructural/bdel/gobierto/` | `19_descarga_bdel_gobierto.R`, `19b_parsear_bdel_gobierto.R` |

### Obtención manual (descargar del portal y colocar en `data/` antes de ejecutar)

| Fuente | Organismo / portal | Carpeta destino | Script de procesamiento |
|---|---|---|---|
| Microdatos poblacionales (SIP/APSIG) | Conselleria de Sanitat (GVA) — **acceso restringido, no redistribuible** | `data/SIP/` | `R/sip/` |
| Catastro Inmobiliario (CAT + SHP; motor del geocoder) | Dirección General del Catastro | `data/contextual/catastro/` | `17_descarga_contextual_cs.R`, `R/ivec/04_ivec_contextual.R` |
| Régimen de tenencia (Censo 2021) | INE — `C2021_Indicadores.csv` | `data/contextual/` | `01_censo2021_seccion.R` |
| Nivel educativo del entorno | INE — Censo Anual de Población 2025 | `data/contextual/` | `R/ivec/04_ivec_contextual.R` |
| Listado de oficinas de farmacia | Conselleria de Sanitat — `ListadoOficinasFarmacia.xlsx` | `data/contextual/` | `17_descarga_contextual_cs.R` |
| Centros docentes (escuelas 0-3 y centros 3+) | Registre de Centres Docents (GVA) — CSV | `data/contextual/` | `17_descarga_contextual_cs.R` |
| Centros culturales de Alicante | Diputación de Alicante — GeoJSON | `data/contextual/` | `17_descarga_contextual_cs.R` |
| Cuadrícula de referencia LAEA 1×1 km | Eurostat / GEOSTAT — shapefile `CV_grid_1km.shp` | `data/Literatura/GRID/CV_shapefiles/` | `16_malla_ign_ivecbase.R` |
| Índices demográficos y migraciones municipales | Portal Estadístic de la Generalitat Valenciana (PEGV) | `data/estructural/` | `R/ivec/07_ivec_estructural.R` |
| Afiliación a la Seguridad Social (modelo productivo) | Tesorería General de la Seguridad Social (TGSS) — ficheros `TGS/MUNCNAE…xlsx` | `data/estructural/TGS/` | `R/ivec/07_ivec_estructural.R` |
| Deuda viva municipal | Ministerio de Hacienda (BDEL) — XLS | `data/estructural/bdel/` | `R/ivec/07_ivec_estructural.R` |
| Razones de Mortalidad Estandarizada (RMEs) | FISABIO — proyecto MEDEA (`datos_RME.csv`) | `data/estructural/` | `R/ivec/08_validacion_externa_estructural.R` |
| Espacios Urbanos Sensibles (VEUS-CV) | R. Temes / Generalitat Valenciana | `data/estructural/veus_cv/` | validación externa |
| Peligrosidad de inundación (PATRICOVA) | Institut Cartogràfic Valencià (ICV) | `data/mallas/malla_patricova/` | superposición cartográfica |
| Cartografía de límites administrativos | INE / IGN-CNIG / GVA | `data/mallas/malla_administrativa/` | — (manual) |

> Nota sobre la cartografía de límites: la cartografía administrativa (municipios, secciones y
> comarcas) utilizada por el instrumento se obtuvo manualmente y reside en
> `data/mallas/malla_administrativa/`. No se descarga por script.

## Scripts de esta carpeta

- `00_descarga_maestro.R` — guion maestro que encadena los scripts por módulo.
- `01_censo2021_seccion.R` — procesa el CSV de indicadores del Censo 2021 (régimen de tenencia).
- `16_malla_ign_ivecbase.R` — construye la cuadrícula LAEA de 1 km² del IVEC_base a partir del shapefile de referencia.
- `17_descarga_contextual_cs.R` — fuentes del módulo IVEC_contextual (OSM, GVA, MCU, GeoJSON automáticos; Catastro, farmacias y docentes manuales) y geocoder catastral.
- `18_descarga_estructural.R` — descarga automática del Atlas demográfico y de renta del INE.
- `19_descarga_bdel_gobierto.R` — descarga automática de las liquidaciones BDEL del repositorio gobierto-budgets.
- `19b_parsear_bdel_gobierto.R` — parseo de los volcados SQL de la BDEL a tablas analíticas.

El corpus bibliométrico (Web of Science) y su tratamiento se documentan aparte, en los
pipelines de `R/apendices/` y en los Apéndices A y B de la tesis.
