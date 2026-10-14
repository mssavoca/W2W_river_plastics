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

# ── Checks behind every interpretive word (fail loudly rather than overstate) ──
import vals
ours = vals.VALS["ours_allrivers_pooled_W2"]
ours_q25, ours_q75 = float(ours["q25"]), float(ours["q75"])
for g in ("Meuse (FTIR)", "Dommel (FTIR)"):
    gg = group(g, "published", "W2")
    assert float(gg["q25"]) <= ours_q75 and float(gg["q75"]) >= ours_q25, g + " IQR does not overlap ours"
for g in ("Rhine (LDIR)", "Lek canal (LDIR)"):
    assert min(sample_medians(g, "published", "W2")) > ours_q75, g + " not all above our upper quartile"
    assert len(sample_medians(g, "published", "W2")) == 2
s_ours = vals.num("slope_ours_rivers"); s_ftir = vals.num("slope_dutch_FTIR_waterbodies"); s_ldir = vals.num("slope_dutch_LDIR_waterbodies")
assert s_ldir < s_ours < s_ftir, "ours median slope not between Dutch FTIR and LDIR medians"
w1 = {g: float(group(g, "published", "W1")["median"]) for g in ("Meuse (FTIR)", "Dommel (FTIR)", "Rhine (LDIR)", "Lek canal (LDIR)")}
w1_min_g = min(w1, key=w1.get); w1_max_g = max(w1, key=w1.get)
key_of = {"Meuse (FTIR)": "Meuse", "Dommel (FTIR)": "Dommel", "Rhine (LDIR)": "Rhine", "Lek canal (LDIR)": "Lekcanal"}
name_of = {"Meuse (FTIR)": "Meuse", "Dommel (FTIR)": "Dommel", "Rhine (LDIR)": "Rhine", "Lek canal (LDIR)": "Lek canal"}

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
    ("a", IT), " = −3.74) lies within that range",
]
trk.tracked_replace(p72, old, new)

# ── 5b. Concentration paragraph (W2 first, then W1) ───────────────────────────
conc = [
    "Rescaled to our 50–500 µm sieve window, which requires only interior rescaling for every dataset, Dutch river water measured over 20–5000 µm (FTIR) or 10–500 µm (LDIR) had medians of ",
    f("dutch_Meuse_published_W2"), " (Meuse), ", f("dutch_Dommel_published_W2"), " (Dommel), ",
    f("dutch_Rhine_published_W2"), " (Rhine) and ", f("dutch_Lekcanal_published_W2"),
    " (Lek canal) particles m⁻³, versus ", f("ours_allrivers_pooled_W2"), " particles m⁻³ measured here (Fig. 7a). "
    "The interquartile range of our samples overlapped those of the FTIR-measured Meuse and Dommel, whereas every LDIR sample from the Rhine and Lek canal exceeded our upper quartile; "
    "with two LDIR samples per water body and different analytical techniques, these differences should not be generalized. "
    "Extrapolated to 1–5000 µm, Dutch medians ranged from ", headline("dutch_%s_published_W1" % key_of[w1_min_g]), " (", name_of[w1_min_g], ") to ",
    headline("dutch_%s_published_W1" % key_of[w1_max_g]), " (", name_of[w1_max_g], ") particles m⁻³, versus ",
    headline("ours_allrivers_pooled_W1"), " under our single-slope model and ", headline("ours_allrivers_piecewise_W1"),
    " under the two-segment model (Fig. 7b). These W1 differences are dominated by the fitted slopes, which were estimated with different methods and fitting windows, "
    "and by differences between large perennial and small episodic rivers; they should therefore be read as model-based contrasts rather than differences in true abundance "
    "(Supporting Information, Table S-A and Figs. S-A to S-C).",
]
pconc = trk.new_paragraph(p72, conc, rpr_from=p72.find(qn("w:r")))
p72.addnext(pconc)
ins_el = pconc.find(qn("w:ins"))
trk.comment_markers(ins_el, ins_el, 801)

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

doc.save(dst)
print("methods words:", n_words)
