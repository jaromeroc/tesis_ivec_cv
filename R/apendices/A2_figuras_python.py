#!/usr/bin/env python3
"""
Figuras B.2 y B.3 del Apéndice B (A2-biblio.Rmd).
Ejecutar desde la raíz del proyecto (misma CWD que al compilar el Rmd).

Salida (nombres canónicos, pisan los ficheros generados por A2_pipeline_bibliometria.R):
  outputs/figures/app-b/bubble_kw_cr_1980_2024.{png,pdf}
  outputs/figures/app-b/alluvial_kw_cr_1980_2024.{png,pdf}

Diseño para compilación en sidewaysfigure (preamble.tex: \\usepackage{rotating}):
  • Bubble  : 9" × 5.5" → out.width='0.90\\textheight' en el chunk
  • Alluvial: 9" × 6.0" → out.width='0.85\\textheight' en el chunk
  Con textheight ≈ 24.7 cm (A4, márgenes 2.5 cm), la escala es casi 1:1 y las
  fuentes de 8–9 pt se renderizan legibles en el PDF final.

Dependencias: pandas, numpy, matplotlib, Pillow
  pip install pandas numpy matplotlib Pillow
"""

import os
import textwrap
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import matplotlib.patches as mpatches
from matplotlib.path import Path as MPath
from matplotlib.patches import PathPatch

# ── rutas ──────────────────────────────────────────────────────────────────
DATA = os.path.join("outputs", "integracion", "global_1980_2024")
OUT  = os.path.join("outputs", "figures", "app-b")
os.makedirs(OUT, exist_ok=True)


def wrap(txt, w):
    lines = textwrap.wrap(str(txt), w)
    return "\n".join(lines) if lines else str(txt)


# ── datos compartidos ──────────────────────────────────────────────────────
W  = pd.read_csv(os.path.join(DATA, "W_long.csv"))
P  = pd.read_csv(os.path.join(DATA, "p_cr_given_kw_long.csv"))[
        ["kw_cluster", "cr_cluster", "p"]]
df = W.merge(P, on=["kw_cluster", "cr_cluster"])

kw_lbl = (df.sort_values("kw_cluster")
            .drop_duplicates("kw_cluster")
            .set_index("kw_cluster")["kw_label"]
            .to_dict())
cr_lbl = (df.sort_values("cr_cluster")
            .drop_duplicates("cr_cluster")
            .set_index("cr_cluster")["cr_label"]
            .to_dict())
n_kw = len(kw_lbl)   # 12
n_cr = len(cr_lbl)   # 10

# Etiquetas abreviadas para las figuras (no modifican los datos originales)
kw_lbl_short = {
    10: "Cambio climático: impactos urbanos extremos",
    11: "Percepción del riesgo, conocimiento y gobernanza",
    12: "Justicia y pobreza energética",
}
cr_lbl_short = {
    10: "Justicia ambiental",
}


# ══════════════════════════════════════════════════════════════════════════════
# FIGURA B.2 — Bubble chart KW × CR
# Diseño: 9" × 5.5" para sidewaysfigure con out.width='0.90\textheight'
# ══════════════════════════════════════════════════════════════════════════════
df2 = df.copy()
df2["x"] = df2["cr_cluster"] - 1
df2["y"] = df2["kw_cluster"] - 1
w_max = df2["w_ij"].max()
S_MAX = 420
df2["s"] = ((df2["w_ij"] / w_max) * S_MAX).clip(lower=10)

cmap2 = plt.get_cmap("magma_r")
norm2 = mcolors.Normalize(vmin=0, vmax=df2["p"].max())

fig2 = plt.figure(figsize=(9, 9), dpi=150)

# Márgenes físicos calculados para 9"×9":
#   izq  2.0" → 0.222   bot 3.0" → 0.333   der 1.1" (cbar+gap) → ancho axes 0.638
#   arr  0.12" → 0.013  → alto axes  1 – 0.333 – 0.013 = 0.654
ax2 = fig2.add_axes([0.222, 0.333, 0.638, 0.654])

for i in range(n_kw):
    ax2.axhspan(i - .5, i + .5,
                color="#f3f3f3" if i % 2 == 0 else "#ffffff",
                zorder=0, lw=0)
for x in range(n_cr):
    ax2.axvline(x, color="#dfdfdf", lw=0.5, zorder=1)
for y in range(n_kw):
    ax2.axhline(y, color="#dfdfdf", lw=0.5, zorder=1)

sc2 = ax2.scatter(df2["x"], df2["y"], s=df2["s"], c=df2["p"],
                  cmap=cmap2, norm=norm2, alpha=0.88,
                  edgecolors="white", linewidths=0.5, zorder=4)

ax2.set_xlim(-0.6, n_cr - 0.4)
ax2.set_ylim(-0.6, n_kw - 0.4)
ax2.set_xticks(range(n_cr))
ax2.set_yticks(range(n_kw))
ax2.set_yticklabels([wrap(kw_lbl[i + 1], 30) for i in range(n_kw)],
                    fontsize=7, linespacing=1.20, va="center", ha="right")
# Etiquetas X escalonadas en dos filas (pares arriba, impares abajo) — sin rotación
ax2.set_xticklabels(['' for _ in range(n_cr)])
ax2.tick_params(axis="y", length=0, pad=7)
ax2.tick_params(axis="x", length=4, pad=3)
_trans = ax2.get_xaxis_transform()  # x=datos, y=fracción de ejes (0=base, negativo=bajo)
for i in range(n_cr):
    row = i % 2          # 0 → fila alta, 1 → fila baja
    y_off = -0.07 if row == 0 else -0.30
    ax2.text(i, y_off, wrap(cr_lbl_short.get(i + 1, cr_lbl[i + 1]), 13),
             transform=_trans, ha='center', va='top',
             fontsize=7, linespacing=1.15)
    # línea guía de la fila baja
    if row == 1:
        ax2.plot([i, i], [0, y_off + 0.02], transform=_trans,
                 color="#bbbbbb", lw=0.5, clip_on=False)
ax2.set_xlabel("Polos intelectuales (CR)", fontsize=9.5, labelpad=90)
ax2.set_ylabel("Áreas temáticas (KW)",      fontsize=9.5, labelpad=12)
for sp in ax2.spines.values():
    sp.set_edgecolor("#aaaaaa")
    sp.set_linewidth(0.6)

# barra de color — derecha (axes_left + axes_width + gap = 0.222+0.638+0.025)
cbar_ax2 = fig2.add_axes([0.885, 0.390, 0.020, 0.490])
cb2 = fig2.colorbar(sc2, cax=cbar_ax2)
cb2.set_label("Alineamiento (p)", fontsize=8.5, labelpad=7)
cb2.ax.tick_params(labelsize=7.5)
cb2.outline.set_edgecolor("#aaaaaa")
cb2.outline.set_linewidth(0.6)

# leyenda de tamaño — inferior derecha, bajo la barra de color
sv   = [250, 500, 750, 1000]
sleg = fig2.add_axes([0.874, 0.050, 0.115, 0.180])
sleg.set_xlim(0, 1)
sleg.set_ylim(-0.5, len(sv) + 0.5)
sleg.axis("off")
sleg.text(0.50, len(sv) + 0.20, "Volumen\n(w_ij)",
          ha="center", va="bottom", fontsize=8.5, fontweight="semibold")
for i, val in enumerate(reversed(sv)):
    s_ = (val / w_max) * S_MAX
    sleg.scatter([0.28], [i], s=s_, color="#888888", alpha=0.82,
                 edgecolors="white", linewidths=0.45, zorder=3)
    sleg.text(0.60, i, str(val), ha="left", va="center", fontsize=8)

fig2.savefig(os.path.join(OUT, "bubble_kw_cr_1980_2024.png"),
             dpi=300, bbox_inches="tight")
fig2.savefig(os.path.join(OUT, "bubble_kw_cr_1980_2024.pdf"),
             dpi=300, bbox_inches="tight")
plt.close(fig2)
print("Figura B.2 generada.")


# ══════════════════════════════════════════════════════════════════════════════
# FIGURA B.3 — Sankey KW → CR
# Diseño: 9" × 6.0" para sidewaysfigure con out.width='0.85\textheight'
# ══════════════════════════════════════════════════════════════════════════════
Q25 = W["w_ij"].quantile(0.25)
df3 = W[W["w_ij"] > Q25].copy()

tot_kw = (W.groupby(["kw_cluster", "kw_label"])["w_ij"].sum()
           .reset_index().sort_values("kw_cluster").reset_index(drop=True))
tot_cr = (W.groupby(["cr_cluster", "cr_label"])["w_ij"].sum()
           .reset_index().sort_values("cr_cluster").reset_index(drop=True))
grand  = tot_kw["w_ij"].sum()

GAP    = 0.006
PAD    = 0.018
USABLE = 1.0 - 2 * PAD
N_KW   = len(tot_kw)
N_CR   = len(tot_cr)
SCALE  = (USABLE - (N_KW - 1) * GAP) / grand

# nodos KW
kw_node = {}
y = PAD
for _, r in tot_kw.iterrows():
    h = r["w_ij"] * SCALE
    k = int(r["kw_cluster"])
    lbl = kw_lbl_short.get(k, r["kw_label"])
    kw_node[k] = dict(y0=y, y1=y + h, h=h, label=lbl)
    y += h + GAP

# nodos CR (centrados verticalmente)
cr_total_span = grand * SCALE + (N_CR - 1) * GAP
cr_start      = (1.0 - cr_total_span) / 2.0
cr_node = {}
y = cr_start
for _, r in tot_cr.iterrows():
    h = r["w_ij"] * SCALE
    c = int(r["cr_cluster"])
    lbl = cr_lbl_short.get(c, r["cr_label"])
    cr_node[c] = dict(y0=y, y1=y + h, h=h, label=lbl)
    y += h + GAP

turbo  = plt.get_cmap("turbo")
kw_col = {k: turbo(i / (N_KW - 1)) for i, k in enumerate(sorted(kw_node))}

fig3 = plt.figure(figsize=(9, 9), dpi=150)
# Mismo layout proporcional: 28 % izq (etiquetas KW), 44 % flujo, 28 % der (etiquetas CR)
ax3  = fig3.add_axes([0.28, 0.02, 0.44, 0.96])
ax3.set_xlim(0, 1)
ax3.set_ylim(0, 1)
ax3.axis("off")

NW   = 0.055
XL0, XL1 = 0.0, NW
XR0, XR1 = 1.0 - NW, 1.0

# flujos (bezier cúbico)
kw_fill = {k: v["y0"] for k, v in kw_node.items()}
cr_fill = {k: v["y0"] for k, v in cr_node.items()}

for _, r in df3.sort_values(["kw_cluster", "cr_cluster"]).iterrows():
    kw, cr, w = int(r["kw_cluster"]), int(r["cr_cluster"]), r["w_ij"]
    hl  = w * SCALE
    y0l = kw_fill[kw]; y1l = y0l + hl; kw_fill[kw] = y1l
    y0r = cr_fill[cr]; y1r = y0r + hl; cr_fill[cr] = y1r
    cx  = 0.38
    verts = [
        (XL1, y1l),
        (XL1 + cx * (XR0 - XL1),       y1l),
        (XL1 + (1 - cx) * (XR0 - XL1), y1r),
        (XR0, y1r), (XR0, y0r),
        (XL1 + (1 - cx) * (XR0 - XL1), y0r),
        (XL1 + cx * (XR0 - XL1),       y0l),
        (XL1, y0l), (XL1, y1l),
    ]
    codes = [MPath.MOVETO,
             MPath.CURVE4, MPath.CURVE4, MPath.CURVE4,
             MPath.LINETO,
             MPath.CURVE4, MPath.CURVE4, MPath.CURVE4,
             MPath.CLOSEPOLY]
    ax3.add_patch(PathPatch(MPath(verts, codes),
                             fc=kw_col[kw], ec="none", alpha=0.58, zorder=2))

# barras de nodo
for kw, info in kw_node.items():
    ax3.add_patch(mpatches.FancyBboxPatch(
        (XL0, info["y0"]), XL1 - XL0, info["h"],
        boxstyle="square,pad=0", fc=kw_col[kw], ec="white", lw=0.8, zorder=3))
for cr, info in cr_node.items():
    ax3.add_patch(mpatches.FancyBboxPatch(
        (XR0, info["y0"]), XR1 - XR0, info["h"],
        boxstyle="square,pad=0", fc="#6b6b6b", ec="white", lw=0.8, zorder=3))


def nudge_labels(node_dict, min_sep, n_iter=400):
    keys = sorted(node_dict)
    ys   = np.array([(node_dict[k]["y0"] + node_dict[k]["y1"]) / 2 for k in keys])
    for _ in range(n_iter):
        moved = False
        for i in range(1, len(ys)):
            if ys[i] - ys[i - 1] < min_sep:
                mid = (ys[i] + ys[i - 1]) / 2
                ys[i - 1] = mid - min_sep / 2
                ys[i]     = mid + min_sep / 2
                moved = True
        if not moved:
            break
    return dict(zip(keys, ys))


# min_sep ajustado para figura de 6" de alto (vs. 14" original)
kw_lbl_y = nudge_labels(kw_node, min_sep=0.048)
cr_lbl_y = nudge_labels(cr_node, min_sep=0.058)

# etiquetas KW
for kw in sorted(kw_node):
    info     = kw_node[kw]
    node_mid = (info["y0"] + info["y1"]) / 2
    lbl_y    = kw_lbl_y[kw]
    ax3.plot([XL0 - 0.008, XL0 - 0.008], [node_mid, lbl_y],
             color="#cccccc", lw=0.6, zorder=1, clip_on=False)
    ax3.plot(XL0 - 0.008, node_mid, "o",
             color=kw_col[kw], ms=3.5, zorder=5, clip_on=False)
    ax3.text(XL0 - 0.022, lbl_y, wrap(info["label"], 22),
             ha="right", va="center", fontsize=7, linespacing=1.20,
             clip_on=False, color="#1a1a1a")

# etiquetas CR
for cr in sorted(cr_node):
    info     = cr_node[cr]
    node_mid = (info["y0"] + info["y1"]) / 2
    lbl_y    = cr_lbl_y[cr]
    ax3.plot([XR1 + 0.008, XR1 + 0.008], [node_mid, lbl_y],
             color="#cccccc", lw=0.6, zorder=1, clip_on=False)
    ax3.plot(XR1 + 0.008, node_mid, "o",
             color="#6b6b6b", ms=3.5, zorder=5, clip_on=False)
    ax3.text(XR1 + 0.022, lbl_y, wrap(info["label"], 18),
             ha="left", va="center", fontsize=7, linespacing=1.20,
             clip_on=False, color="#1a1a1a")

# encabezados de columna
ax3.text((XL0 + XL1) / 2, PAD - 0.025, "Area tematica (KW)",
         ha="center", va="top", fontsize=10, fontweight="semibold", clip_on=False)
ax3.text((XR0 + XR1) / 2, PAD - 0.025, "Polo intelectual (CR)",
         ha="center", va="top", fontsize=10, fontweight="semibold", clip_on=False)
fig3.text(0.04, 0.50, "Peso del acoplamiento (w_ij)",
          ha="center", va="center", rotation=90, fontsize=10)

fig3.savefig(os.path.join(OUT, "alluvial_kw_cr_1980_2024.pdf"),
             dpi=300, bbox_inches="tight")
fig3.savefig(os.path.join(OUT, "alluvial_kw_cr_1980_2024.png"),
             dpi=300, bbox_inches="tight")
plt.close(fig3)
print("Figura B.3 generada.")
