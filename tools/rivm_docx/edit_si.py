import sys, os
import docx
from docx.oxml.ns import qn
sys.path.insert(0, os.path.dirname(__file__))
import trk
import vals
from vals import CAPS, FIG, fix_citations, AUDIT, SITABLE, PUBCHECK

src, dst = sys.argv[1], sys.argv[2]
doc = docx.Document(src)
body = doc.element.body
PARAS = [el for el in body if el.tag == qn("w:p")]


def find_p(start_text, nth=0):
    hits = [p for p in PARAS if trk.ptext(p).startswith(start_text)]
    if len(hits) <= nth:
        raise ValueError("paragraph not found: " + start_text)
    return hits[nth]


SUP = {"sup": True}; SUB = {"sub": True}; IT = {"italic": True}; BO = {"bold": True}
CITE_SI = {
    "Quik et al. 2026": "ref. 11",
    "Mintenig et al. 2020, 2025": "refs. 12, 14",
    "Mughini-Gras et al. 2021": "ref. 13",
    "the Segur et al. (2026) LOD window": "the LOD window of ref. 4",
    "Segur et al. 2026": "ref. 4",
    "Mintenig et al. (2020) and Mintenig et al. (2025)": "refs. 12 and 14",
}
STUDY_REF = {"Mintenig et al. 2020": "Mintenig et al. 2020 (12)", "Mintenig et al. 2025": "Mintenig et al. 2025 (14)",
             "Mughini-Gras et al. 2021": "Mughini-Gras et al. 2021 (13)"}

# ── Counts for the limitations paragraph, derived from the audit table ────────
def is_true(x): return str(x).upper() == "TRUE"
aud = AUDIT
n_incl = sum(is_true(a["included"]) for a in aud)
n_eff = sum(a["water_type"] == "Effluent WWTP" for a in aud)
n_300 = sum("LLm = 300" in (a["exclusion_reason"] or "") for a in aud)
n_300_noneff = sum("LLm = 300" in (a["exclusion_reason"] or "") and a["water_type"] != "Effluent WWTP" for a in aud)
mg = [a for a in aud if a["study"] == "Mughini-Gras et al. 2021"]
n_mg_tot, n_mg = sum(is_true(a["included"]) for a in mg), len(mg)
n_m25 = sum(is_true(a["included"]) for a in aud if a["study"] == "Mintenig et al. 2025")
ratios = [float(r["ratio"]) for r in PUBCHECK]
pct = max(max(r, 1 / r) for r in ratios) * 100 - 100
pct_txt = "%d" % max(1, round(pct + 0.4999))
WORDS = {2: "two", 3: "three", 4: "four", 5: "five", 7: "seven", 8: "eight", 9: "nine"}
w = lambda n: WORDS.get(n, str(n))

# ── Section title (TOC entry and heading) ─────────────────────────────────────
for nth in (0, 1):
    p = find_p("Comparison with published PSDs", 0)
    trk.tracked_replace(p, "Comparison with published PSDs", [("Benchmarking against Dutch river datasets", IT)])
    P = [el for el in body if el.tag == qn("w:p")]

# ── Rationale paragraph ───────────────────────────────────────────────────────
p71 = find_p("We benchmarked fitted PSD slopes from Monterey Bay rivers")
trk.tracked_replace(p71, "We benchmarked fitted PSD slopes from Monterey Bay rivers against selected published microplastic PSD estimates",
                    ["We benchmarked fitted PSD slopes and rescaled concentrations from Monterey Bay rivers against selected published microplastic data"])
trk.tracked_replace(p71, "", [
    " For concentrations, the comparison was further restricted to water-based (per-volume) river datasets with public sample-level totals and published PSD fits, "
    "namely the Dutch surface-water data compiled for RIVM report 2026-0069", ("11", SUP), "."])

p72 = find_p("Published slopes were taken as reported")
trk.tracked_replace(p72, "", [
    " The RIVM exponents", ("11", SUP), " were likewise taken as reported. As noted for Fig. SX, refs. 3, 9 and 10, and RIVM (poweRlaw maximum likelihood above an estimated xmin), "
    "used fitting algorithms different from ours, so slope differences partly reflect method."])

# ── New benchmark paragraphs after the MPSizeBase paragraph ───────────────────
p73 = find_p("For the expanded comparison shown in")
r0 = p73.find(qn("w:r"))
P4 = [
    "Dutch river concentrations were taken from Mintenig et al.", ("12", SUP), " (Meuse and Dommel; FTIR, 20–5000 µm), Mughini-Gras et al.", ("13", SUP),
    " (Rhine at Lobith and Lek canal at Vianen; LDIR, 10–500 µm) and Mintenig et al.", ("14", SUP),
    " (Dommel; FTIR, 20–5000 µm), as compiled in the RIVM repository", ("11", SUP), " (EUPL-1.2). Only water-based, per-volume concentrations were used. "
    "These are the study-reported totals (particles m⁻³), which include the authors’ blank and subsampling corrections; they were not recomputed from particles. "
    "RIVM’s power-law exponents (α; longest particle dimension) are poweRlaw maximum-likelihood fits above an estimated ", ("x", IT), ("min", SUB),
    " to particles ≤ 5000 µm, aggregated per water body and technique. We converted them to differential slopes (", ("a", IT),
    " = −α) and applied the FTIR or LDIR fit for each water body to every sample from it. Each concentration was rescaled with ",
    ("CF", IT), " = (UL", ("d", SUB), ("1−α", SUP), " − LL", ("d", SUB), ("1−α", SUP), ")/(UL", ("m", SUB), ("1−α", SUP),
    " − LL", ("m", SUB), ("1−α", SUP), "), where LL", ("m", SUB), "–UL", ("m", SUB), " are the reported sampling limits and LL", ("d", SUB), "–UL", ("d", SUB),
    " the target window. This is identical to the correction factor above with ", ("a", IT), " = −α. "
    "Targets were W2 (50–500 µm; primary, because it lies inside every measured range) and W1 (1–5000 µm). "
    "Primary uncertainty followed RIVM: α was drawn from a triangular distribution between its bootstrap confidence limits with mode at the estimate, and each sampling limit from a triangular distribution of ±10% (clipped to 1–5000 µm), with 5,000 draws per sample. "
    "As a harmonised sensitivity, α was drawn from the truncated normal distribution used for our own data (SD equal to the confidence-interval width divided by 3.92, plus the structural SD of 0.25) with fixed limits. "
    "Our concentrations (per-sample sums over plastic polymer classes) were rescaled from the 50–500 µm sieve window, so our W2 values are the measurements themselves. "
    "Our implementation of the RIVM method reproduced RIVM’s own published per-sample W1 medians to within PCT% for every water body.".replace("PCT", pct_txt),
]
P5 = [
    ("Data limitations. ", IT),
    "Particle-level data for the LDIR studies of Bäuerlein et al. (Meuse, Overijsselse Vecht, Lek and a WWTP effluent canal) are not public, so these water bodies appear only in the slope comparison (main-text Fig. 8). "
    "Mughini-Gras et al. reported totals for %s of %s samples. We excluded all WWTP effluent samples (n = %d), the remaining Mintenig et al. samples analysed only above 300 µm (n = %d; %d in total, %d of them effluent), and the Mughini-Gras samples without totals, leaving %d Dutch samples (Table S-A). "
    % (w(n_mg_tot), w(n_mg), n_eff, n_300_noneff, n_300, n_300 - n_300_noneff, n_incl) +
    "Dommel concentrations from Mintenig et al. (2025; n = %d) were obtained from the RIVM repository and are pooled with the 2020 Dommel samples in main-text Fig. 7. " % n_m25 +
    "The LDIR lower size limit is 10 µm in the sample metadata but 20 µm in the RIVM report text; we used 10 µm and tested 20 µm (Fig. S-B). "
    "Because our polymer library has no rubber class, Dutch totals excluding rubber were also evaluated (Fig. S-B). "
    "Because slopes were fitted with different methods and windows, differences between W1 estimates conflate PSD shape with fitting method; W2 is therefore the primary comparison.",
]
pa = trk.new_paragraph(p73, P4, rpr_from=r0)
pb = trk.new_paragraph(p73, P5, rpr_from=r0)
p73.addnext(pa); pa.addnext(pb)

# ── Table S-A (after the last existing table) ─────────────────────────────────
tbls = [el for el in body if el.tag == qn("w:tbl")]
last_tbl = tbls[-1]
p_cap_tmpl = find_p("Table SX. Lab blank levels")
cols = list(SITABLE[0].keys())
rows = [cols] + [[STUDY_REF.get(r[c], r[c]) if c == "Study" else r[c] for c in cols] for r in SITABLE]
cap = trk.new_paragraph(p_cap_tmpl, [("Table S-A. Dutch river and Monterey Bay sample sets included in the concentration benchmark.", BO),
    " Numbers in parentheses are SI reference numbers. ", ("a", IT),
    ", differential slope (Dutch: RIVM bootstrap 95% CI, as published; this study: pooled fit ±1.96 SE). Fit window: Dutch, estimated ", ("x", IT), ("min", SUB),
    " (fit extends to 5000 µm); this study, LOD window. Reported, range of study-reported totals (measured). Rescaled columns: median of per-sample medians "
    "[lowest per-sample 5th – highest per-sample 95th percentile], particles m⁻³; Dutch values reconstructed with the RIVM method, this study’s with the pooled single slope (W2 measured, W1 reconstructed)."],
    rpr_from=p_cap_tmpl.find(qn("w:r")))
spacer0 = trk.new_paragraph(p_cap_tmpl, None)
tbl = trk.table_from_rows(doc, rows, font_sz=14)
last_tbl.addnext(spacer0); spacer0.addnext(cap); cap.addnext(tbl)

# ── Figures S-A, S-B, S-C after the last supplementary figure ─────────────────
p_lastcap = find_p("Figure SX. Particle-length PSD slopes")
p_img_tmpl = p_lastcap.getprevious()
while p_img_tmpl is not None and not list(p_img_tmpl.iter("{http://schemas.openxmlformats.org/drawingml/2006/main}blip")):
    p_img_tmpl = p_img_tmpl.getprevious()
anchor = p_lastcap
for lab, title, key, img in [
    ("S-A", "Empirical distributions of rescaled concentrations, Dutch rivers vs this study.", "figSA", "rivm_figSA_ecdf.png"),
    ("S-B", "Sensitivity of the Dutch-river benchmark.", "figSB", "rivm_figSB_sensitivity.png"),
    ("S-C", "Correction factors versus PSD slope.", "figSC", "rivm_figSC_cf_vs_slope.png"),
]:
    pic = trk.picture_paragraph(doc, p_img_tmpl, os.path.join(FIG, img), 6.5)
    capp = trk.new_paragraph(p_lastcap, [("Figure %s. " % lab, BO), title + " " + fix_citations(CAPS[key], CITE_SI)],
                             rpr_from=p_lastcap.find(qn("w:r")))
    anchor.addnext(pic); pic.addnext(capp); anchor = capp

# ── References 11-14 (outside the Zotero bibliography field) ──────────────────
p_ref10 = find_p("(10)")
p_fld_end = p_ref10.getnext()
refs = [
    ["(11)\t", "Quik, J. T. K.; van Breemen, P.; Hids, A. R.; van Loon, S.; Mintenig, S. M.; Koelmans, A. A. ",
     ("Screening Level Ecological Risk Assessment of Microplastics in Dutch Surface Waters", IT),
     "; RIVM Letter Report 2026-0069; National Institute for Public Health and the Environment (RIVM): Bilthoven, 2026. https://doi.org/10.21945/RIVM-2026-0069. Data and code: https://github.com/rivm-syso/MicroplasticsERA_water."],
    ["(12)\t", "Mintenig, S. M.; Kooi, M.; Erich, M. W.; Primpke, S.; Redondo-Hasselerharm, P. E.; Dekker, S. C.; Koelmans, A. A.; van Wezel, A. P. A Systems Approach to Understand Microplastic Occurrence and Variability in Dutch Riverine Surface Waters. ",
     ("Water Research", IT), " ", ("2020", BO), ", ", ("176", IT), ", 115723. https://doi.org/10.1016/j.watres.2020.115723."],
    ["(13)\t", "Mughini-Gras, L.; van der Plaats, R. Q. J.; van der Wielen, P. W. J. J.; Bauerlein, P. S.; de Roda Husman, A. M. Riverine Microplastic and Microbial Community Compositions: A Field Study in the Netherlands. ",
     ("Water Research", IT), " ", ("2021", BO), ", ", ("192", IT), ", 116852. https://doi.org/10.1016/j.watres.2021.116852."],
    ["(14)\t", "Mintenig, S. M.; Hunnestad, A.; Koelmans, A. A. ", ("Microplastics in het oppervlaktewater van de Dommel", IT),
     "; Wageningen University & Research, 2025."],
]
anchor = p_fld_end
first = None
for spec in refs:
    rp = trk.new_paragraph(p_ref10, spec, rpr_from=p_ref10.find(qn("w:r")))
    anchor.addnext(rp); anchor = rp
    first = first if first is not None else rp
ins0 = first.find(qn("w:ins"))
trk.comment_markers(ins0, ins0, 811)

# Comment on the benchmark methods paragraph (data provenance of our concentrations).
ia = pa.find(qn("w:ins"))
trk.comment_markers(ia, ia, 812)

# ── PSD numbers and regenerated pipeline figures (psd_rescaling_params.json) ──
P = vals.PARAMS
assert P is not None, "run the pipeline first (psd_rescaling_params.json)"
pooled = P["pooled"]; rs = P["rescaling"]
p56 = find_p("Because the same fitted PSD is used in the numerator and denominator")
win_lo, win_hi = P["meta"]["headline_length_window_um"]
trk.tracked_replace(p56, "For the pooled river-water C-PSD, the fitted cumulative slope was", [f"For pooled river-water particle lengths, the slope fitted by maximum likelihood on the fixed {win_lo:.0f}–{win_hi:.0f} µm window, in the cumulative convention, was"])
trk.tracked_replace(p56, "-1.920 ± 0.035 SE", [f"{pooled['a_cum']:.3f} ± {pooled['a_cum_SE']:.3f} SE"])
trk.tracked_replace(p56, "a = -2.92,", [f"a = {pooled['a_diff']:.2f},"])
trk.tracked_replace(p56, "", [
    f" In the final analysis, each sample’s concentration within the {win_lo:.0f}–{win_hi:.0f} µm fitting window was rebuilt from its quality-controlled particle records (each scaled by the sample’s aliquot extrapolation factor; zero if none, retained as a non-detect), "
    "so that concentration and correction factor share the same size interval. Each river’s samples were then rescaled with that river’s own fitted slope (primary), with slope uncertainty propagated by Monte Carlo "
    "(fit standard error plus a structural standard deviation of " + str(P["meta"]["slope_structural_sd"]) + "); the pooled slope above is retained for comparison. "
    "River-specific slopes and rescaled concentrations are given in Table S-A."])
# Held sentence (G1/G2): annotate rather than change.
p66 = find_p("The piecewise model produced a substantially smaller correction factor")
cf_single, cf_two = rs["CF_pooled"][1], rs["CF_two"]
trk.tracked_replace(p66, "22,465 under the single-slope model to 2,403 under the piecewise model, a 9.3-fold reduction (piecewise/single-slope ratio = 0.107)",
                    [f"{vals.sig(cf_single, 3)} under the single-slope model to {vals.sig(cf_two, 3)} under the piecewise model, "
                     f"a {cf_single / cf_two:.1f}-fold reduction (piecewise/single-slope ratio = {cf_two / cf_single:.3f}; pooled fitting window, median of the single-slope Monte Carlo)"])
r66 = [r for r in p66.iter(qn("w:r")) if r.find(qn("w:t")) is not None and "fold reduction" in (r.find(qn("w:t")).text or "")]
if r66:
    trk.comment_markers(r66[0], r66[0], 813)
SI_CF_NOTE = ("Updated to the resolved concentration basis: concentrations restricted to the fixed fitting window and rescaled from that same window (pooled-slope values shown here; "
              "the main text uses river-specific slopes).")

from PIL import Image
def swap_image(caption_start, fig_path, cid):
    pcap = find_p(caption_start)
    pimg = pcap.getprevious()
    while pimg is not None and not list(pimg.iter("{http://schemas.openxmlformats.org/drawingml/2006/main}blip")):
        pimg = pimg.getprevious()
    blip = list(pimg.iter("{http://schemas.openxmlformats.org/drawingml/2006/main}blip"))[0]
    part = doc.part.related_parts[blip.get(qn("r:embed"))]
    part._blob = open(fig_path, "rb").read()
    w, h = Image.open(fig_path).size
    for ext in pimg.iter("{http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing}extent"):
        ext.set("cy", str(int(int(ext.get("cx")) * h / w)))
    for ext in pimg.iter("{http://schemas.openxmlformats.org/drawingml/2006/main}ext"):
        if ext.get("cx"):
            ext.set("cy", str(int(int(ext.get("cx")) * h / w)))
    for src in pimg.iter("{http://schemas.openxmlformats.org/drawingml/2006/main}srcRect"):
        src.getparent().remove(src)
    runs = [r for r in pcap.iter(qn("w:r")) if r.find(qn("w:t")) is not None]
    trk.comment_markers(runs[0], runs[0], cid)

SI_FIGS = [
    ("Figure SX. C-PSD power law fits for particle lengths for different", "cpsd_single_vs_piecewise_plot_river.png"),
    ("Figure SX. Buoyant-polymer (PP + PE) fraction", "m14_buoyant_plot_river.png"),
    ("Figure SX. C-PSD power-law fits by matrix", "cpsd_river_ocean_facet_plot_river.png"),
    ("Figure SX. C-PSD power law fits by particle attribute", "cpsd_slopes_all_metrics_plot_river.png"),
    ("Figure SX. Particle-length PSD slopes (a), this study vs.", "alpha_comparison_plot_river.png"),
]
for k, (cap, fn) in enumerate(SI_FIGS):
    swap_image(cap, os.path.join(FIG, fn), 814 + k)

doc.save(dst)
print("counts:", dict(incl=n_incl, eff=n_eff, l300=n_300, l300_noneff=n_300_noneff, mg=(n_mg_tot, n_mg), m25=n_m25, pct=pct_txt))
