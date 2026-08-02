#!/usr/bin/env python3
# =============================================================================
# 10_fig_zoom_vegabaja.py — FIGURA DEL ZOOM COMPARATIVO (cap. 6, riesgo compuesto)
# =============================================================================
# Genera la figura "Dos territorios, dos diagnósticos": IVEC_base por quintiles
# (paleta semáforo canónica RdYlGn) superpuesto con la inundación, en dos
# ventanas de 12x12 km a la misma escala:
#   • l'Horta Sud  -> huella REAL de la dana 29-O-2024 (capa actualizada),
#                     porque PATRICOVA no marcaba adecuadamente la zona del Poyo.
#   • Baix Segura / Vega Baja -> peligrosidad PATRICOVA alta (niveles 1-3).
#
# Paleta semáforo canónica (RdYlGn 5 clases ColorBrewer), dirección 1 (IVEC_base
# es vulnerabilidad: mayor valor = rojo). Paleta semáforo canónica del proyecto.
# Salida: outputs/figures/cap-6/fig_zoom_vegabaja.{pdf,png}
# =============================================================================
import geopandas as gpd
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patheffects as pe
from matplotlib.patches import Patch
from matplotlib.colors import ListedColormap, BoundaryNorm

BASE = "data/base/ivec_resultados/lisa_clusters.gpkg"
PAT  = "data/mallas/malla_patricova/orde_patricova_peligrosidad_inun0.shp"
DANA = "data/mallas/00_DANA2024_shp/DANA2024.ZonasInundadas.HuellaInundacion.shp"
MUN  = "data/mallas/malla_administrativa/Delimitaciones_municipios.gpkg"
OUT  = "outputs/figures/cap-6/fig_zoom_vegabaja"

lisa = gpd.read_file(BASE)
pat  = gpd.read_file(PAT)[["n_pelig", "geometry"]]
mun  = gpd.read_file(MUN)[["nom_mun_cas", "geometry"]]
dana = gpd.read_file(DANA)
pat_alta = pat[pat["n_pelig"].isin([1, 2, 3])]

# Quintiles globales sobre las 3.124/3.113 celdas (mismo ntile que el resto del cap. 6)
lisa["quintil"] = pd.qcut(lisa["ivec_base"], 5, labels=[1, 2, 3, 4, 5]).astype(int)

# Paleta semáforo canónica (RdYlGn) — dirección 1: Q5 = rojo (vulnerabilidad)
PAL = {1: "#1a9641", 2: "#a6d96a", 3: "#ffffbf", 4: "#fdae61", 5: "#d7191c"}
cmap = ListedColormap([PAL[i] for i in [1, 2, 3, 4, 5]])
norm = BoundaryNorm([.5, 1.5, 2.5, 3.5, 4.5, 5.5], cmap.N)

# Centroides de celda -> municipio (para etiquetar sobre celdas activas)
cen = lisa[["GRD_ID", "quintil", "geometry"]].copy()
cen["geometry"] = cen.geometry.centroid
cell_mun = gpd.sjoin(cen, mun, how="left", predicate="within")

HALF = 6000  # ventana 12 x 12 km

def win(names):
    c = mun[mun["nom_mun_cas"].isin(names)].union_all().centroid
    return (c.x - HALF, c.y - HALF, c.x + HALF, c.y + HALF)

def label_munis(ax, labels, bbox):
    """Etiqueta cada municipio sobre el centroide de SUS celdas activas."""
    minx, miny, maxx, maxy = bbox
    for m in labels:
        sub = cell_mun[cell_mun["nom_mun_cas"] == m]
        if len(sub) == 0:
            continue
        x, y = sub.geometry.x.mean(), sub.geometry.y.mean()
        if minx < x < maxx and miny < y < maxy:
            ax.annotate(m, (x, y), fontsize=7.5, ha="center", color="black",
                        path_effects=[pe.withStroke(linewidth=2, foreground="white")])

def diag(ax, txt):
    ax.text(0.5, 0.965, txt, transform=ax.transAxes, ha="center", va="top",
            fontsize=8.5, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.3", fc="white", ec="0.6", alpha=0.92))

fig, axes = plt.subplots(1, 2, figsize=(13, 6.9))

# --- Panel izquierdo: l'Horta Sud + huella real DANA ---
ax = axes[0]
bb = win(["Paiporta", "Catarroja", "Massanassa", "Alfafar"])
minx, miny, maxx, maxy = bb
lisa.cx[minx:maxx, miny:maxy].plot(ax=ax, column="quintil", cmap=cmap, norm=norm,
                                   edgecolor="white", linewidth=0.2, zorder=1)
mun.cx[minx:maxx, miny:maxy].boundary.plot(ax=ax, color="0.4", linewidth=0.7, zorder=2)
dana.cx[minx:maxx, miny:maxy].plot(ax=ax, facecolor="none", edgecolor="#08519c",
                                   linewidth=0.7, hatch="\\\\", zorder=3)
ax.set_xlim(minx, maxx); ax.set_ylim(miny, maxy); ax.set_xticks([]); ax.set_yticks([])
ax.set_title("l'Horta Sud  (zona cero DANA)", fontsize=12, fontweight="bold")
diag(ax, "Vulnerabilidad moderada  ·  exposición alta")
label_munis(ax, ["Paiporta", "Catarroja", "Alfafar", "Benetússer", "Massanassa",
                 "Sedaví", "Picanya", "Albal"], bb)
ax.legend(handles=[Patch(facecolor="none", edgecolor="#08519c", hatch="\\\\",
                         label="Huella real de la inundación (29-O-2024)")],
          loc="lower left", fontsize=7, framealpha=0.95)

# --- Panel derecho: Vega Baja + PATRICOVA ---
ax = axes[1]
bb = win(["San Fulgencio", "Rojales", "Benijófar", "Formentera del Segura"])
minx, miny, maxx, maxy = bb
lisa.cx[minx:maxx, miny:maxy].plot(ax=ax, column="quintil", cmap=cmap, norm=norm,
                                   edgecolor="white", linewidth=0.2, zorder=1)
mun.cx[minx:maxx, miny:maxy].boundary.plot(ax=ax, color="0.4", linewidth=0.7, zorder=2)
pat_alta.cx[minx:maxx, miny:maxy].plot(ax=ax, facecolor="none", edgecolor="#5e2d79",
                                       linewidth=0.55, hatch="//", zorder=3)
ax.set_xlim(minx, maxx); ax.set_ylim(miny, maxy); ax.set_xticks([]); ax.set_yticks([])
ax.set_title("Baix Segura / Vega Baja", fontsize=12, fontweight="bold")
diag(ax, "Vulnerabilidad muy alta  ·  exposición alta")
label_munis(ax, ["San Fulgencio", "Rojales", "Benijófar", "Formentera del Segura",
                 "Daya Nueva", "Almoradí", "Guardamar del Segura"], bb)
ql = [Patch(facecolor=PAL[i], edgecolor="0.5", label=l) for i, l in
      zip([1, 2, 3, 4, 5],
          ["Q1 — muy baja", "Q2 — baja", "Q3 — moderada", "Q4 — alta", "Q5 — muy alta"])]
ql.append(Patch(facecolor="none", edgecolor="#5e2d79", hatch="//",
                label="Peligrosidad alta PATRICOVA (1–3)"))
ax.legend(handles=ql, loc="lower right", fontsize=6.6,
          title="IVEC_base (quintil)", framealpha=0.95)

fig.suptitle("IVEC_base por quintiles × inundación: dos territorios, dos diagnósticos",
             fontsize=12.5, fontweight="bold", y=0.995)
# Nota al pie: umbral de 15 residentes (tres líneas, fuente 9)
fig.text(0.5, 0.015,
         "Solo se representan las celdas con 15 o más residentes georreferenciados\n"
         "(umbral de estabilidad estadística y de protección de la identidad);\n"
         "las celdas en blanco corresponden a suelo no residencial o de densidad insuficiente.",
         ha="center", va="bottom", fontsize=9, style="italic", color="0.25")

plt.subplots_adjust(bottom=0.14)
for ext in ("pdf", "png"):
    fig.savefig(f"{OUT}.{ext}", dpi=150, bbox_inches="tight")
print("OK ->", OUT + ".pdf / .png")
