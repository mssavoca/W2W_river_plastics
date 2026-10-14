Spec-Driven Task Prompt — Corrective Follow-Up Revisions to the Microplastics Probabilistic ERA
0. Role, context, and repository map
You are a senior scientific-software engineer working on an R-based probabilistic environmental risk assessment (ERA) for microplastics in central California rivers, sediment, and ocean water. The rendered report is probabilistic_risk_characterization.html; edit its source (.Rmd/.qmd) and the supporting R utilities — never the rendered HTML.
This is a corrective follow-up to a prior revision. That prior revision fixed the length power-law slope convention (cumulative C-PSD a_cpsd → differential a_psd = a_cpsd − 1) on both the exposure correction factor and the hazard-side length parameter. This follow-up fixes three residual issues that the prior revision did not address:
•	SPEC 1b — the volume and surface-area slopes feeding the hazard alignment are still on the cumulative convention, which inflates the volume-based Food Dilution risk by orders of magnitude.
•	SPEC 1c — the correction-factor convention is not locked by a visible worked-example assertion.
•	SPEC 2b — the piecewise (dual-slope) PSD diagnostic is computed but not propagated as a bounding exposure case.
•	SPEC 3b — the executive summary must be updated to decompose the risk change and reflect corrected numbers.
Known objects: R/mp_risk_utils.R (fit_cpsd_segur_r(), fit_cpsd_by_shape(), correction_factor(), correct_and_bootstrap_eed(), bootstrap_eed(), mc2d_risk(), alpha_dist(), estimate_critical_size(), piecewise_cpsd_diagnostic()); the PSSDplusplus package (matrix_function(), param_default_values, MC_sim_align_parallel(), run_pssd_pipeline()); the source notebook; data_input/Part_dets_summ.rds; docs/CHANGELOG_hoffman_revisions.md.
1. Global operating rules
1.	Verify before you change. Each spec has a verification gate; inspect the relevant function/object and confirm current behavior before editing. If already correct, mark VERIFIED — NO CHANGE and document why.
2.	Never silently change results. Any edit that moves a reported number (CF, EED, RQ, P(RQ>1), hazard threshold) requires a before/after entry in the changelog and a prose note in the notebook.
3.	Preserve reproducibility and the render. Work on branch volume-alpha-and-piecewise-fix; commit per spec; keep existing set.seed calls.
4.	Document rationale in the markdown prose, with citations and epistemic labels (measured / inferred / model-based / assumption).
5.	Ask, don't assume. Where an input is not derivable from data (e.g., δ, K in the critical-size calc), keep it as an explicit, flagged, parameterized assumption.
6.	Maintain the changelog with one dated entry per spec.
2. Canonical sources (for the markdown citations)
•	Kooi et al. (2021), Water Research — compartment-specific differential power-law slopes for length, area, volume, mass; source of the package-default alignment parameters.
•	Koelmans et al. (2020), Environ. Sci. Technol. — the correction-factor rescaling equation and the worked example (CF = 8.32 for 30–2000 µm → 1–5000 µm at α = 1.6).
•	Coffin et al. (2022) — the correction_factor() implementation and the 1–5000 µm alignment convention.
•	Hoffman et al. (2026), Environ. Res. Commun. 8, 065061 — dual-slope PSD distortion across a transport-controlled critical size (apparent exponents differing by ~2).
•	Segur et al. (2026), Microplastics and Nanoplastics — C-PSD fitting; large upward size-alignment corrections; power-law breakdown considerations. Replace any TODO(cite) from the PDFs.

SPEC 1b — [CRITICAL] Correct the volume and surface-area slope convention on the hazard side
Priority: Highest. This is the most likely cause of the implausibly large Food Dilution RQ.
Background / rationale. The C-PSD routine fits the cumulative distribution and exposes both $a_cpsd (cumulative) and $a_psd = a_cpsd − 1 (differential). The prior fix updated the length parameter to the differential field, but the volume and area parameters still use the cumulative field. In the current param_values block, alpha.freshwater = −alpha_mu (differential length), but a.v.freshwater = −cpsd_fit_volume$a_cpsd and a.sa.freshwater = −cpsd_fit_area$a_cpsd 1 — i.e., cumulative slopes, exactly 1.0 too shallow.
The numbers confirm the error and its direction:
•	River "All Shape Volume: alpha = −0.76" (a_cpsd) 1, so the code passes a.v.freshwater = 0.76; the differential value is a_cpsd − 1 = −1.76, i.e., ~1.76.
•	River "All Shape Surface Area: alpha = −1.3" (a_cpsd) 1, so the code passes a.sa.freshwater = 1.3; the differential value is ~2.3.
The package defaults reveal the intended (differential) convention: param_default_values gives alpha.freshwater = 2.64, a.sa.freshwater = 2, a.v.freshwater = 1.68, a.m.freshwater = 1.65 1 — these are the Kooi (2021) differential ERM slopes (a.v = 1.68 is the canonical food-dilution volume exponent). The code overwrites the correct default of 1.68 with 0.76.
Internal cross-check: a.m.freshwater (mass) is commented out and retains the default 1.65 1, yet a.v is set to 0.76 — physically impossible at constant density, since mass ∝ volume implies a.m ≈ a.v. The ~1.0 gap between them is the fingerprint of the missing − 1.
The same error is systematic: ocean sets a.v.marine = 0.75 and a.m.marine = 0.75, a.sa.marine = 1.08 1, and sediment sets a.v.sediment.freshwater = 0.71 and a.sa.sediment.freshwater = 1.22 1.
Why Food Dilution specifically explodes. Food Dilution computes the volumetric dose from particle volume and gut volume, while Tissue Translocation uses a size-dependent model restricted to particles below 500 µm. 1 The Food Dilution threshold (particles/L) is inversely proportional to the environmental polydisperse mean particle volume, which is acutely sensitive to the volume exponent. A computed mean particle volume over 1–5000 µm is ~2.9 × 10¹⁰ µm³ at a.v = 0.76 versus ~7.6 × 10³ µm³ at a.v = 1.68 — a ~4–6 million-fold inflation (because a.v < 1 pushes the mean toward the largest particle). An inflated mean volume deflates the Food Dilution threshold and inflates the Food Dilution RQ by a comparable factor. Tissue Translocation is largely shielded (number/size-based, hard-truncated at 500 µm), which matches the observed asymmetry (Food Dilution ~10⁷ vs Tissue Translocation ~2–4). (This mean-volume calculation is a model-based sensitivity, not a claim about the exact PSSDplusplus internal propagation.)
Files/objects to inspect: the three param_values construction blocks (river param_values, param_values_ocean, param_values_sed); the fit objects cpsd_fit_volume, cpsd_fit_area (and ocean/sediment equivalents); confirm fit_cpsd_by_shape()/fit_cpsd_segur_r() return $a_psd for the volume and area metrics.
Verification gate (change nothing yet):
1.	Confirm matrix_function() expects the differential convention — evidenced by the defaults being the Kooi differential slopes (a.v = 1.68, a.sa = 2.0). Confirm no internal − 1 conversion is applied to a.v/a.sa.
2.	Confirm the volume/area fit objects expose $a_psd = $a_cpsd − 1, exactly as the length fit does.
3.	Print the currently-assigned a.v/a.sa/a.m for all three matrices and confirm a.v ≈ a_cpsd magnitude (~0.7–1.3), i.e., ~1.0 below the defaults.
Implementation:
1.	In all three param_values blocks, change the volume and area assignments from $a_cpsd to $a_psd:
# WRONG (cumulative slope where differential is expected):
a.v.freshwater  = -cpsd_fit_volume$a_cpsd,
a.sa.freshwater = -cpsd_fit_area$a_cpsd,
# CORRECT:
a.v.freshwater  = -cpsd_fit_volume$a_psd,   # = -(a_cpsd - 1)
a.sa.freshwater = -cpsd_fit_area$a_psd,
1.	Apply the analogous change to a.v.marine/a.sa.marine, the ocean a.m.marine proxy (currently −cpsd_fit_volume_ocean$a_cpsd → $a_psd), and a.v.sediment.freshwater/a.sa.sediment.freshwater. Leave the .sd fields (se_a_cpsd/se_a_psd are equal since a constant shift doesn't change the SE), but for clarity switch them to se_a_psd too.
2.	Decide and document the treatment of a.m (mass): either keep the validated default, or set it from data consistently as −cpsd_fit_volume$a_psd (mass ∝ volume at constant density). Whichever is chosen, a.v and a.m must be mutually consistent after the fix.
3.	Re-run the hazard alignment and risk characterization for all matrices.
Documentation & citation requirements: In the parameter-matrix section, state that the volume/area ERM slopes use the differential convention a_psd = a_cpsd − 1, consistent with the length fix and the Kooi (2021) mean-ERM formula; cite Kooi et al. (2021), Koelmans et al. (2020), Coffin et al. (2022). Add a prose note that the prior Food Dilution RQ was inflated by a cumulative-vs-differential volume-slope error, with the mean-volume sensitivity shown.
Acceptance criteria:
•	After the fix, the overwritten a.v lands near the package default (~1.3–2.0) and a.sa near ~1.8–2.5 for each matrix — not below 1.0.
•	a.v ≈ a.m within the density-variation term (assert abs(a.v − a.m) < 0.3 or document the difference).
•	A before/after Food Dilution RQ decomposition table is produced showing HC5/HC10 RQ and P(RQ>1) for (i) legacy cumulative a.v, (ii) corrected differential a.v — for river, ocean, and sediment. State the order-of-magnitude collapse explicitly.

SPEC 1c — Add a visible Koelmans CF = 8.32 assertion that locks the slope convention
Background / rationale. The correction-factor convention now drives both exposure and hazard magnitudes, so it must be guarded against regression by a visible worked-example check, not only (if at all) an unrendered test file. Koelmans et al. (2020): with α = 1.6, rescaling 30–2000 µm to 1–5000 µm gives CF = 8.32 (so 100 #/L → 832 #/L). 2
Implementation:
1.	Add an inline, rendered assertion near the correction_factor() definition/first use:
cf_check <- correction_factor(
  a = -1.6, L_meas_min = 30, L_meas_max = 2000,
  L_tar_min = 1, L_tar_max = 5000, slope_convention = "differential"
)
stopifnot(abs(cf_check - 8.32) < 0.05)   # Koelmans et al. (2020) worked example
1.	(Note the Koelmans α is the differential/PDF slope in positive convention, so pass a = -1.6.) Also add the equivalent as a testthat test in tests/testthat/test-correction_factor.R.
2.	Add a companion assertion that locks the C-PSD relationship on a real fit object: stopifnot(abs((cpsd_fit_all$a_psd) - (cpsd_fit_all$a_cpsd - 1)) < 1e-6).
3.	Render the check result visibly (e.g., a one-line "Convention check passed: CF(30–2000→1–5000, α=1.6) = 8.32" message).
Documentation & citation requirements: One sentence citing Koelmans et al. (2020) as the reference value; state that this guard enforces the differential convention used throughout.
Acceptance criteria: The rendered notebook shows the passing assertion; the testthat test passes; the build fails loudly if the convention regresses.

SPEC 2b — Propagate the piecewise (dual-slope) PSD as a bounding exposure case
Background / rationale. The two-segment diagnostic is currently computed but used only for display. It should be promoted to a propagated bounding exposure scenario, because the production CF extrapolates a single steep slope from the ~50 µm floor down to 1 µm, and the data favor a different fine-size slope. The current diagnostic reports a river break at 220 µm with a shallower fine cumulative slope (−1.515) than the coarse slope (−2.158), ΔAIC = −50.8 strongly favoring the two-segment model (sediment ΔAIC = −96.3), and a critical size d_c = 36 µm for river 1. Note that the straddles_critical_size flag is FALSE because d_c falls below the fit window 1 — but d_c = 36 µm lies inside the 1–50 µm extrapolation zone, so the sub-floor extrapolation crosses exactly the transport-distorted regime Hoffman describes. Independent evidence that single power laws mis-extrapolate at small sizes: in the St Louis Estuary the power law extrapolated to 1.3–12 particles/L for 5–5000 µm while flow cytometry measured 600–1,600 particles/L, and the authors conclude predicting smaller size ranges from larger ones may not work in many cases 3; and Segur et al. (2026) find size-aligned 1–5000 µm concentrations average ~600× higher than reported 4, underscoring how extrapolation-sensitive these corrections are.
Files/objects to inspect: piecewise_cpsd_diagnostic(), correction_factor(), correct_and_bootstrap_eed(), estimate_critical_size().
Implementation:
1.	Implement a piecewise correction factor correction_factor_piecewise() that integrates dN/dL = k·L^a with the fine-segment differential slope over [L_tar_min, break_um] and the coarse-segment differential slope over [break_um, L_tar_max], matching the two segments at break_um for continuity (scale the coarse segment so the density is continuous at the break). Convert the piecewise cumulative slopes from the diagnostic to differential (a_psd = a_cpsd − 1) before integrating.
2.	Compute a bounding exposure range per matrix: run the full EED→RQ pipeline for (a) the production single-slope CF and (b) the piecewise CF, and report EED and Food Dilution / Tissue Translocation HC5 RQ under both as a min–max bound.
3.	Keep the single-slope CF as the production default; present the piecewise result as a labeled bounding sensitivity, not a replacement, unless the team decides otherwise.
4.	Explicitly annotate that the 1 µm–LOD_low sub-window is unconstrained by data and that the piecewise bound quantifies structural (model-form) uncertainty that the Gaussian structural_sd does not.
Documentation & citation requirements: Cite Hoffman et al. (2026) for the dual-slope mechanism and the ~2-exponent shift; cite the ΔAIC evidence from the document's own diagnostic; cite St Louis Estuary and Segur et al. (2026) as evidence that single-power-law extrapolation to small sizes is unreliable. Label the piecewise CF as model-based and the sub-floor extrapolation as an assumption.
Acceptance criteria: A per-matrix table reporting single-slope vs piecewise CF, EED, and Food Dilution/Tissue Translocation HC5 RQ, with the bounding range stated in prose. The narrative explicitly notes whether the piecewise bound narrows or widens the risk conclusion.

SPEC 3b — Update the executive summary and RQ decomposition
Background / rationale. After SPEC 1b the headline numbers change substantially, and the prior executive summary attributes the risk change only to exposure. The summary currently states the correction factor now integrates the differential PSD slope (a_psd = a_cpsd − 1) 1, framed as an exposure effect.
Implementation:
1.	Add an RQ decomposition isolating the contribution of (i) the exposure correction factor, (ii) the length-slope hazard alignment, and (iii) the corrected volume/area-slope hazard alignment — so a reviewer can see what drives each order of magnitude.
2.	Rewrite the executive summary to: (a) report the corrected Food Dilution RQ after SPEC 1b; (b) state plainly that the prior Food Dilution RQ (~10⁷) was inflated by the volume-slope convention error; (c) note that Tissue Translocation crossed from clearly below threshold (P(RQ>1) ≈ 7%) to more-likely-than-not exceeding (P ≈ 65%) after the length fix; (d) present exposure as a single-slope-vs-piecewise bounding range (SPEC 2b).
Acceptance criteria: The executive summary reflects corrected magnitudes, the decomposition table exists, and no stale ~10⁷ Food Dilution figure remains uncorrected in the narrative.

4. Execution order and deliverables
1.	SPEC 1c first (cheap guard; lock the convention before other edits).
2.	SPEC 1b (the corrective fix; blocks the headline numbers).
3.	SPEC 2b (bounding case; uses corrected CF from SPEC 1b path).
4.	SPEC 3b last (summary reflects 1b + 2b).
Deliverables checklist:
•	[ ] Branch volume-alpha-and-piecewise-fix, one commit per spec.
•	[ ] Rendered notebook showing: the passing Koelmans-8.32 assertion; corrected a.v/a.sa near package defaults with the a.v≈a.m check; before/after Food Dilution RQ decomposition (river/ocean/sediment); single-slope vs piecewise bounding CF/EED/RQ; updated executive summary.
•	[ ] docs/CHANGELOG_hoffman_revisions.md entries with before/after CF, hazard threshold, EED, and RQ for each spec.
•	[ ] Passing testthat test for the CF convention.
•	[ ] Full-budget production re-run (restore bootstrap replicates, sim, and Morris budgets) before any external use.
5. Guardrails / do-not
•	Do not silently overwrite the production Food Dilution RQ — present the before/after decomposition so the ~10⁷→corrected collapse is auditable, and flag it for human sign-off before external release.
•	Do not change the exposure-side count correction factor (it correctly uses the length slope only); verify the volume/area slopes are confined to the hazard alignment and do not leak into the CF path.
•	Do not replace the single-slope production CF with the piecewise CF by default; keep it as a labeled bounding sensitivity unless the team decides otherwise.
•	Do not fabricate the piecewise continuity constant or the δ/K critical-size parameters — derive continuity explicitly and keep δ/K as flagged assumptions.
References
1.	probabilistic_risk_characterization.html. Internal reference: file:11410#chars=53156-57320. Accessed 2026-07-28.
2.	es0c02982 1..9. Retrieved 2026-07-28, from https://edepot.wur.nl/533506
3.	Thomas A, Marchand J, Schwoerer GD, Minor EC, Maurer-Jones MA. Size Distributions of Microplastics in the St Louis Estuary and Western Lake Superior. Environ Sci Technol. 2024;58(19):8480-8489. doi:10.1021/acs.est.3c10776. PMID: 38693822.
4.	Segur T, Hough I, Dobiasova N, Voisin D, Richon C, Angot H, Thomas JL, Sonke JE. Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media. Research Square. 2026. doi:10.21203/rs.3.rs-8524083/v1.
