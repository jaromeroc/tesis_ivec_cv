```{=latex}
\cleardoublepage
```

# 6. Resultados y validación empírica del modelo IVEC_base

## 6.1 Introducción

Este capítulo presenta la aplicación del modelo **IVEC_base** a la Comunidad Valenciana. Se describen las fuentes de datos empleadas, la estructura de los módulos micro, meso y estructural, y los resultados espaciales derivados. Finalmente, se contrasta la hipótesis H2: *las zonas con mayor vulnerabilidad estructural (IVEC) registran mayores impactos ante amenazas observadas*, evaluando la validez empírica del índice.

---

## 6.2 Fuentes de información y unidades de análisis

* **Unidad territorial:** celdas regulares de 1 km² derivadas del APSIG/SIP.
* **Base principal:** APSIG/SIP (abril 2024).

  * Nivel individual y del hogar: condiciones de salud, empleo, dependencia, tipo de vivienda, composición familiar.
  * No incluye renta ni educación; se emplean variables proxy (tipo de cotización, régimen de tenencia, dependencia funcional).
* **Fuentes secundarias:**

  * Seguridad Social y AEAT: tasas de afiliación y contribución.
  * Padrón continuo (INE): estructura demográfica y envejecimiento.
  * Catastro y Generalitat Valenciana: dotaciones sociales y equipamientos municipales.
* **Escala analítica:** integración de microdatos en celdas de 1 km² con anonimización espacial y procedimientos reproducibles en R.

---

## 6.3 Definición de módulos e indicadores del IVEC_base

El modelo se estructura en tres niveles analíticos, diseñados para capturar vulnerabilidad sin incluir variables de exposición:

[
\begin{aligned}
M_{micro,i} &= Vi_i (1 - CH_i) \
M_{context,i} &= VC_{g(i)} (1 - CS_{g(i)}) \
M_{estruct,i} &= VE_t(i) (1 - CAD_t(i)) \
IVEC_i &= [M_{micro,i} \times M_{context,i} \times M_{estruct,i}]^{1/3}
\end{aligned}
]

### Indicadores orientativos

| Nivel       | Dimensión                | Indicador proxy                                | Fuente                 | Naturaleza                |
| :---------- | :----------------------- | :--------------------------------------------- | :--------------------- | :------------------------ |
| Micro       | Salud y dependencia      | % hogares con personas dependientes            | APSIG/SIP              | Condición individual      |
| Micro       | Empleo y cobertura       | % sin protección contributiva                  | APSIG/SIP              | Fragilidad socioeconómica |
| Meso        | Capital social           | Nº asociaciones o recursos comunitarios / hab. | GV / Registros locales | Capacidad social          |
| Meso        | Servicios de proximidad  | Accesibilidad a centros sociales / sanitarios  | Catastro / SIG         | Capacidad territorial     |
| Estructural | Recursos administrativos | Gasto municipal en servicios / hab.            | GV / IGAE              | Capacidad institucional   |
| Estructural | Gobernanza territorial   | % municipios con planes activos de emergencia  | GV / Protección Civil  | Capacidad estructural     |

> *Nota:* se excluyen variables de exposición (densidad poblacional, viviendas, superficie urbana), dado que estas pertenecen al término (E) de la ecuación de riesgo (R = H \times E \times IVEC).

---

## 6.4 Resultados descriptivos

### 6.4.1 Distribución general del IVEC_base (t₀ = abril 2024)

Incluir:

* Estadísticos descriptivos (media, desviación, percentiles).
* Mapa general de vulnerabilidad (escala 1 km²).

### 6.4.2 Patrones espaciales

Aplicar análisis de autocorrelación espacial para identificar concentración de vulnerabilidad:

* **Moran’s I** → tendencia global.
* **LISA** → clusters significativos (Alta–Alta, Baja–Baja).

Interpretación tipo:

> Los clusters Alta–Alta se concentran en entornos urbanos con baja cobertura social y limitada capacidad administrativa, mientras que las áreas rurales tienden a mostrar vulnerabilidad estructural baja, asociada a redes comunitarias más cohesionadas.

---

## 6.5 Validación empírica de la hipótesis H2

**Hipótesis:**

> Las zonas con valores altos del IVEC presentan mayores impactos en escenarios de riesgo recientes.

### Procedimiento

1. Seleccionar un evento documentado (p.e. DANA 2019, incendios 2022, olas de calor 2023 o COVID-19).
2. Derivar una variable de impacto (pérdidas económicas, población afectada o mortalidad).
3. Asociar espacialmente con las celdas IVEC mediante `sf::st_join()` u overlay raster.
4. Calcular correlación no paramétrica (Spearman o Kendall τ).
5. Representar resultados:

```r
# Ejemplo básico
cor.test(IVEC, danos, method = "spearman")
```

### Resultados esperados

* Correlación positiva y significativa (r > 0.5; p < 0.05).
* Gráfico comparativo: daños medios por decil IVEC.

Interpretación:

> Los resultados confirman que las celdas con mayor vulnerabilidad estructural (IVEC alto) presentan impactos medios más elevados, validando parcialmente la hipótesis H2.

---

## 6.6 Síntesis interpretativa

* El IVEC_base muestra coherencia espacial con las desigualdades estructurales identificadas en la literatura sobre vulnerabilidad social.
* La independencia de la exposición evita redundancias y mejora la comparabilidad interterritorial.
* El modelo permite detectar zonas críticas donde la fragilidad social y la debilidad institucional coexisten.
* Se confirma la utilidad del IVEC como componente reproducible del módulo de vulnerabilidad en el cálculo del riesgo total.

---

## 6.7 Conclusión del capítulo

> El análisis empírico demuestra que la vulnerabilidad estructural y contextual, medida mediante el IVEC_base, es una propiedad mensurable y espacialmente coherente del sistema social valenciano. La validación frente a impactos reales confirma su pertinencia analítica dentro de la ecuación de riesgo (R = H \times E \times IVEC).

---

## Referencias

```bibtex
@book{Birkmann2013,
  title = {Measuring Vulnerability to Natural Hazards: Towards Disaster Resilient Societies},
  author = {Birkmann, Joern},
  year = {2013},
  publisher = {United Nations University Press},
  address = {Tokyo}
}

@incollection{Cardona2012,
  title = {Determinants of Risk: Exposure and Vulnerability},
  author = {Cardona, Omar D. and van Aalst, Maarten and Birkmann, Joern and Fordham, Maureen},
  booktitle = {Managing the Risks of Extreme Events and Disasters to Advance Climate Change Adaptation},
  editor = {Field, C. B. and Barros, V. and Stocker, T. F. and Qin, D.},
  year = {2012},
  publisher = {Cambridge University Press},
  pages = {65--108}
}

@book{Wisner2004,
  title = {At Risk: Natural Hazards, People's Vulnerability and Disasters},
  author = {Wisner, Ben and Blaikie, Piers and Cannon, Terry and Davis, Ian},
  edition = {2nd},
  year = {2004},
  publisher = {Routledge},
  address = {London}
}

@misc{IPCC2022,
  title = {Climate Change 2022: Impacts, Adaptation and Vulnerability},
  author = {{IPCC}},
  year = {2022},
  publisher = {Cambridge University Press}
}

@misc{UNDRR2015,
  title = {Sendai Framework for Disaster Risk Reduction 2015--2030},
  author = {{UNDRR}},
  year = {2015},
  publisher = {United Nations Office for Disaster Risk Reduction},
  address = {Geneva}
}
```

---RMD_OUTPUT_END---

---
