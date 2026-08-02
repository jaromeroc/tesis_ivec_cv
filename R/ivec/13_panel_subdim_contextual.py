# =============================================================================
# 13_panel_subdim_contextual.py — Panel apaisado de mapas de subdimension CS
# =============================================================================
# Compone en una unica figura (estilo "sidewaysfigure", paralelo al diagrama
# alluvial del Apendice B) los CUATRO mapas de la dimension de capacidades
# sociales del entorno (CS) activos en la primera version operativa del modulo
# IVEC_contextual, generados por R/ivec/06_mapas_tablas_contextual.R:
#
#   Fila 1: cs_sociosanitario | cs_transporte      (accesibilidad temporal)
#   Fila 2: cs_proximidad     | cs_nodos_civicos    (densidad de puntos)
#
# Los mapas de subdimension de VC (fisico-residencial y demografico-poblacional)
# se presentan por separado en su propia seccion del capitulo (decision de mayo
# de 2026), de modo que este panel queda reservado a la dimension CS.
#
# Cada panel de origen ya lleva su titulo, leyenda semaforo y nota de fuente,
# de modo que el montaje no anade etiquetas: solo dispone los cuatro mapas en
# una rejilla 2 x 2 sobre fondo blanco con medianiles uniformes.
#
# Salida: outputs/figures/cap-6/fig_panel_subdim_contextual.{png,pdf}
# Insercion en cap. 6: chunk `fig-panel-subdim-contextual` (06-resultados.Rmd).
#
# USO:  python R/ivec/13_panel_subdim_contextual.py
# Dependencias: pillow, img2pdf.
# EXCLUIDO de 00_run_all.R (figura Python; imagenes ya definitivas), en
# simetria con 10_fig_zoom_vegabaja.py.
# =============================================================================
import os
from PIL import Image
import img2pdf

DIR = os.path.join("outputs", "figures", "cap-6")
ROW1 = ["fig_mapa_cs_sociosanitario.png", "fig_mapa_cs_transporte.png"]
ROW2 = ["fig_mapa_cs_proximidad.png", "fig_mapa_cs_nodos_civicos.png"]

TW = 1700   # ancho objetivo de cada panel (px)
GUT = 40    # medianil uniforme (px)


def load(fname):
    im = Image.open(os.path.join(DIR, fname)).convert("RGB")
    w, h = im.size
    return im.resize((TW, round(h * TW / w)), Image.LANCZOS)


def main():
    rows = [[load(f) for f in ROW1], [load(f) for f in ROW2]]
    ph = rows[0][0].size[1]
    cols = 2
    canvas_w = cols * TW + (cols + 1) * GUT
    canvas_h = 2 * ph + 3 * GUT
    canvas = Image.new("RGB", (canvas_w, canvas_h), "white")
    for r, row in enumerate(rows):
        y = GUT + r * (ph + GUT)
        for c, im in enumerate(row):
            canvas.paste(im, (GUT + c * (TW + GUT), y))
    out_png = os.path.join(DIR, "fig_panel_subdim_contextual.png")
    canvas.save(out_png, dpi=(300, 300))
    with open(os.path.join(DIR, "fig_panel_subdim_contextual.pdf"), "wb") as f:
        f.write(img2pdf.convert(out_png))
    print("Panel CS 2x2 generado:", canvas.size)


if __name__ == "__main__":
    main()
