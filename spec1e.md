Spec-Driven Task Prompt — SPEC 1e (sediment temporal-fragmentation framing + PSD-shift sensitivity) & SPEC Q1 (R↔Python C-PSD bit-identity)
0. Context
Two scoped additions to the microplastics probabilistic ERA (probabilistic_risk_characterization.html; edit the .Rmd/.qmd source, not the rendered HTML):
•	SPEC 1e — incorporate the Thuy-Dung et al. (2026) temporal-fragmentation insight into the sediment (depositional) compartment: reframe sediment as time-dependent, add a PSD-shift sensitivity, adopt time-varying bioaccessibility (future-gated), and add a most-sensitive-species screen.
•	SPEC Q1 — reconcile the native R C-PSD fit with the Segur reference PSD_fit.py so the two become bit-identical (currently equivalent but not identical).
Do not disturb any prior fix (SPEC 1/1b/1c/1d/2b/3b/3c) or the river/ocean pipelines. Sediment remains excluded from the headline risk characterization (SPEC 1d-i: sediment_concentration_is_quantitative <- FALSE); nothing here re-introduces it into the conclusion.
1. Operating rules
1.	No changes to analytical budgets, seeds, or existing fixes; additive + prose only.
2.	Every prose number is an inline R reference to a live object; no fabricated numbers or CIs; every literature value carries a citation.
3.	Keep hazard and risk strictly distinct; keep size-range/ERM alignment explicit.
4.	Re-render after edits; update docs/CHANGELOG_hoffman_revisions.md (one entry per spec).
2. Canonical sources
•	Thuy-Dung, Groenenberg & Koelmans (2026), Microplastics and Nanoplastics (doi:10.1186/s43591-026-00210-8) — prospective, temporally-explicit risk for fragmenting MPs; fragmentation kinetics (size-independent k_frag, doubling time TD); HC5 and RCR are time-dependent because shrinking particles become bioaccessible to more sensitive species; HC5 fluctuates (e.g., a median HC5 drop from 2.1×10⁴ to 1.95×10³ particles/kg when a sensitive species becomes accessible); identical HC5 can map to very different RCR as exposure rises; individual-species RCR>1 can precede a derivable community SSD by ~a decade.
•	Redondo-Hasselerharm et al. (2023), J. Hazard. Mater. — freshwater-sediment benthic HC5: food dilution 4.9×10⁹ particles/kg dw (95% CI 6.6×10⁷–1.9×10¹¹); translocation 1.1×10¹⁰ particles/kg dw.
•	Segur et al. (2026) — the C-PSD method and the PSD_fit.py reference implementation (repo assets/SI/SI 3 code and data/PSD_fit.py).

SPEC 1e — Sediment temporal-fragmentation framing and PSD-shift sensitivity
Rationale. Sediment is an accumulating, long-residence depositional compartment, and it is the sink for the large/dense particles that differential settling removes from the water column (Hoffman). Those deposited particles continue to fragment in situ, so — per Thuy-Dung et al. (2026) — both exposure (bioaccessible particle number) and hazard (HC5, via which benthic species are size-accessible) are dynamic. A single-time-point sediment RQ is therefore a snapshot that can under-represent future risk in an accumulating bed. This is a framework upgrade, not a live risk calc, because the sediment exposure concentration is currently non-quantitative (SPEC 1d).
1e-i — Narrative caveat / reframing (applies now)
Add a subsection to the sediment section (and one sentence to the executive summary) stating:
•	Sediment is treated as a time-dependent, accumulating compartment; deposited MPs continue to fragment, shifting the PSD toward smaller, more numerous, more bioaccessible particles over residence time.
•	Consequently, any static sediment metric is a lower-bound snapshot; both HC5 and bioaccessible exposure evolve as fragmentation proceeds.
•	Cite Thuy-Dung et al. (2026) and cross-reference the Hoffman differential-settling sink argument already in the document.
1e-ii — PSD-shift sensitivity (illustrative now; production-gated)
Implement sediment_psd_shift_sensitivity() that re-runs the sediment hazard alignment across a family of progressively finer sediment PSDs representing continued in-situ fragmentation — e.g., shift the sediment C-PSD differential slope and/or lower the LOD-low toward finer sizes over a defined grid — and report how the derived sediment HC5 (Food Dilution and Tissue Translocation) responds as more particles fall below benthic bioaccessibility limits.
•	Gate on sediment_concentration_is_quantitative. While FALSE, run it as an illustrative/methods demonstration only (label every output non-quantitative, exclude from conclusions), analogous to the existing single-slope↔piecewise CF bound (SPEC 2b). When TRUE, it becomes a production sensitivity feeding the sediment RQ range.
•	Do not transplant Thuy-Dung's k_frag or shell-geometry model — it is calibrated to polymer-coated-fertilizer prills with 7-year field data, not heterogeneous mixed-polymer sediment MPs of unknown age. Parameterize the size-shift as an explicit, cited assumption (a scenario grid), not a fitted kinetic rate.
1e-iii — Time-varying bioaccessibility (future enhancement, documented now)
Document how the existing pSSD++ bioaccessibility alignment (food dilution → gut volume; translocation truncated at 500 µm) would be made size-shift-dependent: as the sediment PSD moves finer, smaller-mouthed benthic species become size-accessible and enter the SSD, which can lower HC5. Note this is the same alignment machinery already in use (ToMEx/pSSD++), so it is an extension rather than a new framework. Implement if straightforward; otherwise leave a clearly-scoped TODO with the method description and citation.
1e-iv — Most-sensitive-species screen (applies now)
Add a hazard-side screen that identifies, for the sediment benthic dataset (and, as a companion, the freshwater/marine water datasets), the single most sensitive species / lowest aligned EC, and reports it alongside the community HC5. Rationale: per Thuy-Dung, individual-species risk can precede a derivable community SSD by ~a decade, and our sediment SSD is small/unstable (~89 records). For water compartments where exposure is quantitative, also report the individual-species RCR (EED / lowest-species EC) as an early-warning companion to the community-HC5 RQ.
Acceptance (1e): sediment section reframed with the temporal caveat + citation; a labeled illustrative PSD-shift sensitivity table/plot (gated on the quantitative flag); a documented time-varying-bioaccessibility method; a most-sensitive-species screen for sediment (and a companion individual-species RCR for river/ocean); sediment remains excluded from the headline; executive summary carries the one-line temporal caveat.

SPEC Q1 — Make the R and Segur-Python C-PSD fits bit-identical (QA)
Rationale. The in-document head-to-head shows the two implement the same algorithm and produce identical LOD windows with slopes agreeing to |Δa_cpsd| ≤ 0.044, but they do not regress over the same bin set: the native R fit uses a uniform bin grid (e.g., 5 µm) while the reference PSD_fit.py fits over the populated/observed size classes, so R's n_bins exceeds Python's in every PSD (most extreme at ocean-fiber, R = 102 vs Python = 5, where the slope divergence is also largest). The residual is immaterial to risk (≪ the propagated slope SD of 0.25), but bit-identity is worth having for QA/reproducibility.
Fix (one change, two acceptance checks): reconcile the binning so both fits use the identical bin set — either have fit_cpsd_segur_r() regress over the same populated observed-size bins the CSV exports to PSD_fit.py (preferred), or export the R uniform-grid bins to the Python run so both see the same grid; do not apply two different binnings and compare. Then:
1.	Assert max(abs(delta_a_cpsd)) < 1e-6 across all nine matrix×shape PSDs, and n_bins_R == n_bins_Python for each.
2.	If any residual remains, document its exact cause (e.g., empty-bin handling, tie-breaking on equal R²) rather than leaving it unexplained.
Acceptance (Q1): the comparison table shows Δa_cpsd ≈ 0 and matching bin counts for all nine PSDs, or a documented, understood residual; the sparse ocean-fiber fit is explicitly flagged as low-n regardless.

3. Deliverables & guardrails
•	[ ] Branch sediment-temporal-and-cpsd-bitparity; one commit for SPEC 1e, one for SPEC Q1.
•	[ ] Sediment temporal reframing + illustrative PSD-shift sensitivity + most-sensitive-species screen; documented time-varying bioaccessibility.
•	[ ] R↔Python C-PSD bit-identity (matching bins and slopes) or a documented residual.
•	[ ] Clean re-render; changelog entries; executive-summary temporal caveat.
•	Do not re-introduce sediment into the headline risk conclusion; do not transplant Thuy-Dung k_frag/shell geometry; do not change river/ocean pipelines or any prior fix; do not compare two different binnings in Q1 (reconcile to one).
