import sys, os, re
import docx
from docx.oxml.ns import qn
sys.path.insert(0, os.path.dirname(__file__))
import trk
from vals import f, headline, group, sample_medians, CAPS, FIG, fix_citations, PUBCHECK

src, dst = sys.argv[1], sys.argv[2]
doc = docx.Document(src)
body = doc.element.body
P = [el for el in body if el.tag == qn("w:p")]


def find_p(start_text, nth=0):
    hits = [p for p in P if trk.ptext(p).startswith(start_text)]
    if len(hits) <= nth:
        raise ValueError("paragraph not found: " + start_text)
    return hits[nth]


SUP = {"sup": True}
IT = {"italic": True}
CITE_MAIN = {
    "Quik et al. 2026": "ref. 51",
    "Mintenig et al. 2020, 2025": "refs. 52, 54",
    "Mughini-Gras et al. 2021": "ref. 53",
    "the Segur et al. (2026) LOD window": "the LOD window of ref. 25",
    "Segur et al. 2026": "ref. 25",
    "Mintenig et al. (2020) and Mintenig et al. (2025)": "refs. 52 and 54",
}

# ── Interpretive wording is derived from the data (never asserted by hand) ──
import vals
P = vals.PARAMS
assert P is not None, "run R/probabilistic_risk_characterization.qmd first (psd_rescaling_params.json)"
ours = vals.VALS["ours_allrivers_river_W2"]
ours_q25, ours_q75 = float(ours["q25"]), float(ours["q75"])
def overlaps(g):
    gg = group(g, "published", "W2")
    return float(gg["q25"]) <= ours_q75 and float(gg["q75"]) >= ours_q25
ftir_overlap = [g for g in ("Meuse (FTIR)", "Dommel (FTIR)") if overlaps(g)]
ldir_above = [g for g in ("Rhine (LDIR)", "Lek canal (LDIR)") if min(sample_medians(g, "published", "W2")) > ours_q75]
nm = {"Meuse (FTIR)": "Meuse", "Dommel (FTIR)": "Dommel", "Rhine (LDIR)": "Rhine", "Lek canal (LDIR)": "Lek canal"}
def join_names(gs):
    n = [nm[g] for g in gs]
    return n[0] if len(n) == 1 else " and ".join(n)
w2_sentences = []
if ftir_overlap:
    w2_sentences.append("The interquartile range of our samples overlapped that of the FTIR-measured %s" % join_names(ftir_overlap))
if ldir_above:
    w2_sentences.append("every LDIR sample from the %s exceeded our upper quartile" % join_names(ldir_above))
w2_phrase = (", whereas ".join(w2_sentences) + "; ") if w2_sentences else ""
s_ours = vals.num("slope_ours_rivers"); s_ftir = vals.num("slope_dutch_FTIR_waterbodies"); s_ldir = vals.num("slope_dutch_LDIR_waterbodies")
frag_a = P["fragment"]["a_diff"]
frag_txt = f"{frag_a:.2f}".replace("-", "\u2212")
wang_lo, wang_hi = -3.89, -3.19   # Wang et al. (2026) site means (literature values, ref. 23)
frag_rel = "lies within that range" if wang_lo <= frag_a <= wang_hi else ("is steeper than that range" if frag_a < wang_lo else "is shallower than that range")
w1 = {g: float(group(g, "published", "W1")["median"]) for g in ("Meuse (FTIR)", "Dommel (FTIR)", "Rhine (LDIR)", "Lek canal (LDIR)")}
w1_min_g = min(w1, key=w1.get); w1_max_g = max(w1, key=w1.get)
key_of = {"Meuse (FTIR)": "Meuse", "Dommel (FTIR)": "Dommel", "Rhine (LDIR)": "Rhine", "Lek canal (LDIR)": "Lekcanal"}
name_of = nm

# ── 5a. Methods: "Benchmark against Dutch river data" ─────────────────────────
methods_text = [
    "To place our results in context, we compared them with water-based (per-volume) concentrations from Dutch rivers compiled for RIVM report 2026-0069", ("51", SUP),
    ": the Meuse and Dommel, measured by FTIR over 20–5000 µm", ("52,54", SUP),
    ", and the Rhine and Lek canal, measured by LDIR over 10–500 µm", ("53", SUP),
    ". We used RIVM’s published power-law exponents without refitting, converting each positive exponent α to our differential slope (", ("a", IT), " = −α); these were fitted by maximum likelihood above an estimated lower size limit, a different method from our C-PSD fits. "
    "Study-reported total concentrations were rescaled with the correction factor described above, both to our 50–500 µm sieve window (W2), which lies inside every measured range, and to 1–5000 µm (W1). "
    "Uncertainty followed RIVM (triangular α and ±10% size limits), with our own Monte Carlo method as a sensitivity. "
    "Rubber, which our polymer library lacks, was retained as reported and excluded in a sensitivity analysis.",
]
plain = "".join(s if isinstance(s, str) else s[0] for s in methods_text)
n_words = len(re.findall(r"\S+", plain))
assert n_words <= 150, n_words
p_dp_head = find_p("Data Processing and Analysis")
p_cf = find_p("To estimate microplastic concentrations across the full 1-5000")
p_empty = p_dp_head.getprevious()
head = trk.new_paragraph(p_dp_head, [("Benchmark against Dutch river data", IT)], rpr_from=p_dp_head.find(qn("w:r")))
bodyp = trk.new_paragraph(p_cf, methods_text, rpr_from=p_cf.find(qn("w:r")))
spacer = trk.new_paragraph(p_empty, None)
p_dp_head.addprevious(head); p_dp_head.addprevious(bodyp); p_dp_head.addprevious(spacer)

# ── 5b. Slope comparison sentence -> median (IQR), SPM vs water distinguished ──
p72 = find_p("Particle-size distributions also differed strongly among rivers")
old = ("Our differential fragment PSD slope (a = -3.74) was within the range reported "
       "from particle-resolved µFTIR measurements of Dutch river suspended solids")
new = [
    "Across the four rivers, our differential slopes had a median of ", f("slope_ours_rivers"),
    ", compared with medians of ", f("slope_dutch_FTIR_waterbodies"), " for FTIR and ",
    f("slope_dutch_LDIR_waterbodies"), " for LDIR fits to Dutch river water", ("51", SUP),
    " (Fig. 8). Because the Dutch slopes were fitted by maximum likelihood above an estimated lower size limit rather than by C-PSD, this comparison is indicative only. "
    "These water-column data differ from particle-resolved µFTIR measurements of Dutch river suspended solids (per mass of sediment), whose site-mean slopes ranged from −3.89 to −3.19; our fragment slope (",
    ("a", IT), " = " + frag_txt + ") " + frag_rel,
]
trk.tracked_replace(p72, old, new)

# ── 5b. Concentration paragraph (W2 first, then W1) ───────────────────────────
conc = [
    "Rescaled to our 50–500 µm sieve window, which requires only interior rescaling for every dataset, Dutch river water measured over 20–5000 µm (FTIR) or 10–500 µm (LDIR) had medians of ",
    f("dutch_Meuse_published_W2"), " (Meuse), ", f("dutch_Dommel_published_W2"), " (Dommel), ",
    f("dutch_Rhine_published_W2"), " (Rhine) and ", f("dutch_Lekcanal_published_W2"),
    " (Lek canal) particles m⁻³, versus ", f("ours_allrivers_river_W2"), " particles m⁻³ measured here (Fig. 7a). "
    w2_phrase + "with two LDIR samples per water body and different analytical techniques, these differences should not be generalized. "
    "Extrapolated to 1–5000 µm, Dutch medians ranged from ", headline("dutch_%s_published_W1" % key_of[w1_min_g]), " (", name_of[w1_min_g], ") to ",
    headline("dutch_%s_published_W1" % key_of[w1_max_g]), " (", name_of[w1_max_g], ") particles m⁻³, versus ",
    headline("ours_allrivers_river_W1"), " with each river’s own single slope (", headline("ours_allrivers_pooled_W1"), " with one pooled slope) and ", headline("ours_allrivers_piecewise_W1"),
    " under the two-segment model (Fig. 7b). These W1 differences are dominated by the fitted slopes, which were estimated with different methods and fitting windows, "
    "and by differences between large perennial and small episodic rivers; they should therefore be read as model-based contrasts rather than differences in true abundance "
    "(Supporting Information, Table S-A and Figs. S-A to S-C).",
]
pconc = trk.new_paragraph(p72, conc, rpr_from=p72.find(qn("w:r")))
p72.addnext(pconc)

# ── Figures 7 and 8 after Fig. 6 caption ──────────────────────────────────────
p_fig6cap = find_p("Fig. 6. C-PSD particle length fits")
p_fig6img = p_fig6cap.getprevious()
anchor = p_fig6cap
for num_, title, capkey, img, (w, h) in [
    (7, "Rescaled concentrations: Dutch rivers vs Monterey Bay rivers.", "fig7", "rivm_fig7_concentrations.png", (7, 7.5)),
    (8, "Particle-size-distribution slopes: Dutch rivers vs this study.", "fig8", "rivm_fig8_slopes.png", (7, 6.5)),
]:
    pic = trk.picture_paragraph(doc, p_fig6img, os.path.join(FIG, img), 6.5)
    cap = trk.new_paragraph(p_fig6cap, [("Fig. %d. %s" % (num_, title), {"bold": True}),
                                        " " + fix_citations(CAPS[capkey], CITE_MAIN)])
    anchor.addnext(pic); pic.addnext(cap); anchor = cap

# ── 5e. Code availability ─────────────────────────────────────────────────────
p_code = find_p("All data and code used in this study are available at")
trk.tracked_replace(p_code, "", [
    " The Dutch-river benchmark analysis (R/rivm_utils.R and R/rivm_benchmark.qmd) and its derived data tables "
    "(data_output/rivm_benchmark_*_v1.csv; data_input/external/rivm/derived/) are included. Raw Dutch data and code are available from the RIVM repository "
    "(https://github.com/rivm-syso/MicroplasticsERA_water; EUPL-1.2) and are not redistributed here; some underlying particle data (Bäuerlein et al.) are not publicly available.",
])

# ── References 51-54 (outside the Zotero bibliography field) ──────────────────
p_ref50 = find_p("(50)")
p_fld_end = p_ref50.getnext()          # paragraph holding the bibliography field end
refs = [
    ["(51)\t", "Quik, J. T. K.; van Breemen, P.; Hids, A. R.; van Loon, S.; Mintenig, S. M.; Koelmans, A. A. ",
     ("Screening Level Ecological Risk Assessment of Microplastics in Dutch Surface Waters", IT),
     "; RIVM Letter Report 2026-0069; National Institute for Public Health and the Environment (RIVM): Bilthoven, 2026. https://doi.org/10.21945/RIVM-2026-0069. Data and code: https://github.com/rivm-syso/MicroplasticsERA_water."],
    ["(52)\t", "Mintenig, S. M.; Kooi, M.; Erich, M. W.; Primpke, S.; Redondo-Hasselerharm, P. E.; Dekker, S. C.; Koelmans, A. A.; van Wezel, A. P. A Systems Approach to Understand Microplastic Occurrence and Variability in Dutch Riverine Surface Waters. ",
     ("Water Research", IT), " ", ("2020", {"bold": True}), ", ", ("176", IT), ", 115723. https://doi.org/10.1016/j.watres.2020.115723."],
    ["(53)\t", "Mughini-Gras, L.; van der Plaats, R. Q. J.; van der Wielen, P. W. J. J.; Bauerlein, P. S.; de Roda Husman, A. M. Riverine Microplastic and Microbial Community Compositions: A Field Study in the Netherlands. ",
     ("Water Research", IT), " ", ("2021", {"bold": True}), ", ", ("192", IT), ", 116852. https://doi.org/10.1016/j.watres.2021.116852."],
    ["(54)\t", "Mintenig, S. M.; Hunnestad, A.; Koelmans, A. A. ", ("Microplastics in het oppervlaktewater van de Dommel", IT),
     "; Wageningen University & Research, 2025."],
]
anchor = p_fld_end
first_ref = None
for spec in refs:
    rp = trk.new_paragraph(p_ref50, spec, rpr_from=p_ref50.find(qn("w:r")))
    anchor.addnext(rp); anchor = rp
    first_ref = first_ref if first_ref is not None else rp
fr_ins = first_ref.find(qn("w:ins"))
trk.comment_markers(fr_ins, fr_ins, 802)

# Reply anchor for existing comment 25 ("Report median and IQR"): reuse its range.
rng_end = [e for e in p72.iter(qn("w:commentRangeEnd")) if e.get(qn("w:id")) == "25"][0]
rng_start = [e for e in p72.iter(qn("w:commentRangeStart")) if e.get(qn("w:id")) == "25"][0]
s = trk.OxmlElement("w:commentRangeStart"); s.set(qn("w:id"), "803"); rng_start.addnext(s)
e = trk.OxmlElement("w:commentRangeEnd"); e.set(qn("w:id"), "803"); rng_end.addprevious(e)
ref_run = [r for r in p72.iter(qn("w:r")) if r.find(qn("w:commentReference")) is not None
           and r.find(qn("w:commentReference")).get(qn("w:id")) == "25"][0]
rr = trk.OxmlElement("w:r"); cr = trk.OxmlElement("w:commentReference"); cr.set(qn("w:id"), "803"); rr.append(cr)
ref_run.addnext(rr)

# ── PSD / extrapolation numbers from the production pipeline (psd_rescaling_params.json) ──
rs = P["rescaling"]; pooled = P["pooled"]; br = P["by_river"]; het = P["heterogeneity"]; two = P["two_segment"]
d3 = lambda x: f"{x:.3f}"
sigL = lambda x: vals.sig(x, 2)
cf3 = lambda x: vals.sig(x, 3)
W1r, W1p, W1t, W2, Wlod = rs["C_W1_river"], rs["C_W1_pooled"], rs["C_W1_two"], rs["C_W2_reported"], rs["C_lod_window_river"]
cfm = lambda q: q[1]   # [p05, p50, p95] -> median
assert het["p_value"] < 0.001, "Cochran P no longer < 0.001; update wording"

# Abstract
p_abs = find_p("Intermittent rivers may deliver microplastics")
trk.tracked_replace(p_abs, "approximately 600 particles L⁻¹ (slope-derived 95% CI: 460-790 particles L⁻¹)",
                    ["a median of approximately " + sigL(W1r["median_L"]) + " particles L⁻¹ (river-specific power-law slopes; "
                     + sigL(W1t["median_L"]) + " particles L⁻¹ under a two-segment size distribution)"])
r01 = [r for r in p_abs.iter(qn("w:r")) if (r.find(qn("w:t")) is not None and "0.33 microplastics" in (r.find(qn("w:t")).text or ""))]
if r01:
    trk.comment_markers(r01[0], r01[0], 801)

# Methods: river-specific rescaling, MC uncertainty, pooled LOD window
p_cf = find_p("To estimate microplastic concentrations across the full 1-5000")
trk.tracked_replace(p_cf, "and empirically measured (50-500 µm) size ranges", ["and the C-PSD fitting (LOD) window of each river"])
trk.tracked_replace(p_cf, "multiplied by the empirical 50-500 µm concentration.",
                    ["multiplied by the concentration within that window. That concentration was obtained by multiplying each sample’s reported 50-500 µm concentration by the fraction of its QC-passing particles lying inside the window, "
                     "so that the concentration and the correction factor refer to the same size interval; samples with no particle inside the window were retained as non-detects (zero)."])
trk.tracked_replace(p_cf, "Uncertainty in the extrapolated concentration was estimated by propagating the 95% confidence interval of the fitted differential PSD slope through the same integration.",
                    ["Because slopes differed significantly among rivers (Results), each river’s samples were rescaled with that river’s own fitted slope; the pooled slope and a two-segment model are reported for comparison. "
                     "Slope uncertainty was propagated by Monte Carlo sampling of each fitted slope (fit standard error plus a structural standard deviation of " + str(P["meta"]["slope_structural_sd"]) + ")."])
lo, hi = pooled["window_LOD_um"]
trk.tracked_replace(p_cf, "The 165-385 µm limit-of-detection (LOD) window was used to estimate the power-law slope and does not represent the empirical concentration interval; the latter was defined operationally by the 50- and 500-µm sampling sieves.",
                    [f"The 50- and 500-µm sampling sieves define the reported concentration, whereas the C-PSD window ({lo:.0f}-{hi:.0f} µm for the pooled fit; river-specific windows in the Supporting Information) defines the size interval over which both the concentration and the power-law slope are empirically supported."])

# Results: pooled / shape slopes
p71 = find_p("As in numerous other studies, our microplastic abundance")
fr, fi = P["fragment"], P["fiber"]
for old, new in [("-1.920 ± 0.035 SE, R² = 0.988", f"{d3(pooled['a_cum'])} ± {d3(pooled['a_cum_SE'])} SE, R² = {d3(pooled['R2'])}"),
                 ("-2.739 ± 0.038, R² = 0.993", f"{d3(fr['a_cum'])} ± {d3(fr['a_cum_SE'])}, R² = {d3(fr['R2'])}"),
                 ("-1.671 ± 0.046, R² = 0.970", f"{d3(fi['a_cum'])} ± {d3(fi['a_cum_SE'])}, R² = {d3(fi['R2'])}"),
                 ("(a = -3.74)", f"(a = {fr['a_diff']:.2f})"), ("(a = -2.67)", f"(a = {fi['a_diff']:.2f})")]:
    if old.replace(" ", "") != new.replace(" ", ""):
        trk.tracked_replace(p71, old, [new])

# Results: river slopes and heterogeneity
rv = sorted(br.items(), key=lambda kv: -kv[1]["a_cum"])     # shallowest first
fmt_r = lambda kv: f"{d3(kv[1]['a_cum'])} ± {d3(kv[1]['a_cum_SE'])} in the {kv[0]} River"
trk.tracked_replace(p72, "-1.493 ± 0.068 in the Salinas River and -1.503 ± 0.074 in the Carmel River to -1.975 ± 0.050 in the San Lorenzo River and -2.711 ± 0.063 in the Pajaro River",
                    [f"{fmt_r(rv[0])} and {fmt_r(rv[1])} to {fmt_r(rv[2])} and {fmt_r(rv[3])}"])
trk.tracked_replace(p72, " = 227.4, ", [f" = {het['Q']:.1f}, "])
trk.tracked_replace(p72, " = 98.7%)", [f" = {het['i2']:.1f}%)"])

# Results: extrapolation paragraph
p73 = find_p("The steep river-water PSD indicates")
cf_r = {r: cfm(v) for r, v in rs["CF_by_river"].items()}
trk.tracked_replace(p73,
    "Using the pooled differential PSD slope (a = -2.920 ± 0.035 SE), integration of the fitted power law yielded a correction factor of approximately 1,850 from the 50-500 µm fraction to the full 1-5000 µm range. "
    "Applying this correction to the empirically measured mean concentration of 0.33 particles L⁻¹ yielded a model-derived estimate of approximately 600 microplastics L⁻¹ (slope-derived 95% CI: 460–790 microplastics L⁻¹) across 1-5000 µm.",
    ["Because slopes differed among rivers, each river’s C-PSD-window concentrations (median " + f"{Wlod['median_L']:.2f}" + " particles L⁻¹; " + str(rs["n_nondetect_river_window"]) + " non-detects) were rescaled with that river’s own fitted differential slope, giving median correction factors from the C-PSD window to the full 1-5000 µm range of "
     + ", ".join(f"{cf3(cf_r[r])} ({r})" for r in sorted(cf_r))
     + f"; the pooled slope (a = {pooled['a_diff']:.3f} ± {pooled['a_diff_SE']:.3f} SE) over the pooled window gives {cf3(cfm(rs['CF_pooled']))}. "
     f"Applied to the per-sample concentrations, this yielded a model-derived median of approximately {sigL(W1r['median_L'])} microplastics L⁻¹ "
     f"(IQR {sigL(W1r['q25_L'])}–{sigL(W1r['q75_L'])}; n = {W1r['n']}) across 1-5000 µm, compared with {sigL(W1p['median_L'])} microplastics L⁻¹ using the pooled slope."])
trk.tracked_replace(p73, "The fitted slope was estimated over the empirically supported 165-385 µm C-PSD window",
                    [f"The fitted slopes and concentrations were both restricted to each river’s empirically supported C-PSD window ({lo:.0f}-{hi:.0f} µm for the pooled fit)"])
trk.tracked_replace(p73, "(ΔAIC = -44.0)",
                    [f"(ΔAIC = {two['dAIC']:.1f}; breakpoint {two['breakpoint_um']:.0f} µm; differential slopes {two['a_fine_diff']:.2f} below and {two['a_coarse_diff']:.2f} above), "
                     f"giving a correction factor of {cf3(rs['CF_two'])} and a median of {sigL(W1t['median_L'])} microplastics L⁻¹ ({rs['ratio_W1_river_over_two']:.1f}-fold lower than with river-specific single slopes)"])

# Conclusions
p77 = find_p("Our results also demonstrate how environmental microplastic abundance")
trk.tracked_replace(p77, "approximately 600 particles L⁻¹ across the full 1-5000 µm microplastic size range, compared with 0.33 microplastics L⁻¹ measured",
                    [f"a median of approximately {sigL(W1r['median_L'])} particles L⁻¹ ({sigL(W1t['median_L'])} under a two-segment model) across the full 1-5000 µm microplastic size range, "
                     f"compared with a median of {W2['median_L']:.2f} microplastics L⁻¹ measured"])

# Regenerated pipeline figure(s): swap image bytes, keep width, correct aspect ratio.
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
swap_image("Fig. 6. C-PSD particle length fits", os.path.join(FIG, "cpsd_river_length_facet_plot.png"), 804)

doc.save(dst)
print("methods words:", n_words)
