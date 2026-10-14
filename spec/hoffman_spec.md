Spec-Driven Task Prompt — Hoffman et al. (2026)-Informed Revisions to the Microplastics Probabilistic ERA
0. Role, context, and repository map
You are a senior scientific-software engineer working on an R-based probabilistic environmental risk assessment (ERA) for microplastics (MP) in central California rivers, sediment, and ocean water. The rendered report is probabilistic_risk_characterization.html; edit its source (.Rmd/.qmd) and the supporting R utilities — never the rendered HTML directly.
Known repository objects (confirm paths before editing):
•	R/mp_risk_utils.R — custom functions: fit_cpsd_segur_r(), correction_factor(), bootstrap_eed(), mc2d_risk(), alpha_dist(), bootstrap_aspect_ratio(), draw_rq_mc1d(), summarize_rq().
•	R/cpsd_plotting.R — plot_cpsd_multi(), parameter_histograms_function().
•	PSSDplusplus package — matrix_function(), MC_sim_align_parallel(), make_all_pSSDs(), param_default_values.
•	Source notebook rendering to probabilistic_risk_characterization.html.
•	data_input/Part_dets_summ.rds, data_output/Part_dets_cleaned.csv, figures/.
Key variables: monitoring, monitoring_sed, monitoring_ocean; raw_particles_river/_sed/_ocean; alpha, alpha_sed, alpha_ocean; cf_rescale, combined_cf, cf_rescale_sed, cf_rescale_ocean; cpsd_fit_all, cpsd_fit_area, cpsd_fit_volume; param_values, param_matrix, MC_sim_df, pSSDs, haz, eed_boot, mc2d_results; the sample_depth_general factor (subsurface, surface); Lmin_measured_um = 50, Lmax_measured_um = 500.
1. Global operating rules (apply to every spec)
1.	Verify before you change. Each spec has a Verification gate. Do not modify numeric logic until you have inspected the relevant function body and confirmed the current behavior. If verification shows the current code is already correct, do not change it — instead document the verification outcome and mark the spec VERIFIED — NO CHANGE.
2.	Never silently change results. Any edit that moves a reported number (CF, EED, RQ, P(RQ>1)) must be accompanied by (a) a before/after value in the changelog, and (b) a markdown note explaining why.
3.	Preserve reproducibility. Keep the render working end-to-end (set.seed where stochastic; do not remove existing seeds). Work on a branch hoffman-2026-revisions; commit per spec.
4.	Document rationale in the markdown prose, not only in code comments. Every implemented change gets a short prose paragraph in the notebook, at the relevant section, stating the rationale, the epistemic status (measured / inferred / model-based / assumption), and the citation(s).
5.	Cite canonically. Use the reference list in §2. Reproduce author/year exactly; do not invent DOIs or page numbers. Where a citation's full bibliographic detail is not in the repo, insert a TODO(cite) placeholder rather than fabricating it.
6.	Maintain a changelog at docs/CHANGELOG_hoffman_revisions.md with one dated entry per spec: what was verified, what changed, before/after numbers, and open questions.
7.	Ask, don't assume. Where a spec requires an input you cannot derive from the data (e.g., effective sampling depth δ, eddy diffusivity K), insert a clearly-flagged, parameterized default and surface it as an open question in the changelog — do not hard-code a hidden assumption.
2. Canonical sources (use these for the markdown citations)
•	Hoffman et al. (2026), Environ. Res. Commun. 8, 065061 — the driver for these revisions. Confirm the full author list and title from the PDF and replace TODO(cite) as needed.
•	Segur et al. (2026), Microplastics and Nanoplastics (doi:10.1186/s43591-026-00205-5) — C-PSD / power-law size-alignment method; reports fragment slopes steeper than fibers.
•	Koelmans et al. (2020), Environ. Sci. Technol. — original correction-factor (CF) rescaling equation to the 1–5000 µm default range.
•	Kooi et al. (2021), Water Research — compartment-specific power-law length slopes.
•	Kooi & Koelmans (2019), Environ. Sci. Technol. — continuous size/shape/density distributions; default α = 1.6.
•	Coffin et al. (2022) — San Francisco Bay MP risk characterization; source of the correction_factor() implementation and the 1–5000 µm alignment convention used here.
•	RIVM (2025), report 2025-0095 — Koelmans-approach implementation using a deliberately wide slope prior.

SPEC 1 — [CRITICAL, VERIFY-FIRST] Confirm the power-law slope convention passed to correction_factor()
Priority: Highest. This can change exposure and the headline risk conclusion by up to ~2 orders of magnitude.
Background / rationale. The C-PSD routine fits the cumulative distribution N(≥L) ∝ L^(α_CPSD) and defines the differential (BN-PSD) slope as a = α_CPSD − 1. In this project α_CPSD(all) ≈ −1.93 and the corresponding differential slope is a ≈ −2.93. The rescaling correction factor, however, is defined on the differential PSD (dN/dL = k·L^a), integrated as k/(a+1)·(L₂^(a+1) − L₁^(a+1)). The published Koelmans et al. (2020) / Kooi et al. (2021) CF equation uses the probability-density (differential) slope — where abundance y ∝ x^(−α) and the CF scales the integral of that density across size ranges — and Kooi et al. (2021) report those length PDF slopes as 2.1 (marine surface water) to 3.3 (freshwater sediment). The project's differential slope (≈2.93 in magnitude) sits inside that published range; the cumulative slope (≈1.93) does not.
The reported river median CF of 207 is reproduced exactly by feeding a = −1.925 (the cumulative α_CPSD) into the integral. If the correct input is the differential slope a = −2.925, the CF for the same 165–385 µm → 1–5000 µm rescaling is ~23,000 — a factor of ~112× higher — which would raise the exposure distribution and RQ by roughly two orders of magnitude.
Files/functions to inspect: correction_factor() in R/mp_risk_utils.R; the notebook cells that build alpha, alpha_sed, alpha_ocean (via alpha_dist()) and call correction_factor(a = alpha, ...); the param_values assignment (alpha.freshwater = -alpha_mu, comment "use cPSD − 1 since dN/dL = k·L^a").
Verification gate (do this first, change nothing yet):
1.	Read the body of correction_factor(). Determine unambiguously whether its a argument is expected to be the differential slope (exponent used directly as L^(a+1)) or whether it internally converts (e.g., subtracts 1). Document the exact exponent arithmetic.
2.	Trace the value actually passed: confirm whether alpha (mean ≈ −1.925, the cumulative α_CPSD) or a differential slope (≈ −2.925) reaches correction_factor(). The param_values comment says "use cPSD − 1" but the assignment alpha.freshwater = -alpha_mu only negates — verify whether an analogous −1 conversion is (or is not) applied on the CF path.
3.	Reproduce the current CF numerically and confirm it matches the reported median (207 for river; ~63.6 for ocean). Record which slope value reproduces it.
4.	Cross-check against Koelmans/Kooi: confirm whether their published CF exponent corresponds to the differential PDF slope (it does) and therefore whether the project should pass a = α_CPSD − 1.
Implementation (only if verification shows a mismatch):
•	Correct the slope passed to correction_factor() on all three matrices (river, sediment, ocean) so the exponent used in the integral is the differential slope consistent with Koelmans/Kooi. Prefer fixing at the call site (pass α_CPSD − 1) OR inside correction_factor() — but not both — and add an explicit argument (e.g., slope_convention = c("differential","cumulative")) so the intent is self-documenting.
•	Add a unit test (tests/testthat/test-correction_factor.R) that reproduces the Koelmans et al. (2020) worked example (30–2000 µm → 1–5000 µm with α = 1.6 gives CF ≈ 8.32) to lock the convention.
Documentation & citation requirements:
•	In the "Step 2 — Rescale" markdown section, state the convention explicitly (differential vs cumulative), show the exponent arithmetic, and cite Koelmans et al. (2020), Kooi et al. (2021), and Coffin et al. (2022).
•	If a change is made, add a before/after CF table (river/sediment/ocean) and propagate the corrected CF through EED, RQ, and MC2D; report the new headline numbers.
Acceptance criteria: The convention is stated in one sentence in prose and enforced by a passing unit test reproducing the Koelmans CF = 8.32 example. The changelog records which slope reproduces the legacy CF and whether a correction was applied, with before/after exposure and RQ.

SPEC 2 — Widen the α (slope) uncertainty to include transport-induced structural uncertainty
Background / rationale. Currently the slope is propagated as a truncated normal on [−6, −1.1] with SD ≈ 0.031 — that SD is only the OLS fitting error of a single power law, not the uncertainty in whether one power law is the correct model. Hoffman et al. (2026) show that size-dependent vertical transport produces an observed PSD with two power-law regimes whose apparent exponents differ by two across a transport-controlled critical size; below that size the observed exponent is the reduced α − 2, above it the true α. Because the CF is extremely sensitive to the exponent (a shift of ~1 unit moves the river CF ~100×), a structural uncertainty of order ±1–2 exponent units dwarfs the current fitting SD and is the dominant unpropagated uncertainty. Independent implementations already use wide slope priors for this reason: RIVM (2025) adopts a slope of 2.5 ± 0.25, reflecting that environmental slopes range ~2–3.
Files/functions to inspect: alpha_dist(); the truncated-normal construction for alpha, alpha_sed, alpha_ocean; the MC2D outer loop in mc2d_risk().
Implementation:
1.	Add a structural-uncertainty component to the slope distribution, separate from and larger than the fit SE. Implement as a parameterized inflation (e.g., a structural_sd argument and/or a mixture that, with a user-set probability, shifts the exponent toward α − 2 to represent a distorted-regime fit). Default structural_sd should be documented as a policy choice, not silently fixed — expose it and justify the value in prose.
2.	Ensure this widened slope uncertainty is sampled in the outer (uncertainty) loop of mc2d_risk(), alongside the CF and hazard draws — not the inner (variability) loop.
3.	Re-run and report how P(RQ>1) and RQ_p50/p95 for Food Dilution and Tissue Translocation respond to the widened band.
Documentation & citation requirements: In the slope-uncertainty markdown section, distinguish fitting error from structural/transport uncertainty, cite Hoffman et al. (2026) for the ±2-exponent dual-slope result and RIVM (2025) for the wide-prior precedent, and label the chosen structural_sd as an assumption.
Acceptance criteria: MC2D outputs include a sensitivity panel of RQ / P(RQ>1) vs slope-uncertainty width; prose states the dominant contribution of slope uncertainty relative to other terms.

SPEC 3 — Stratify rescaling by sampling depth (surface vs subsurface)
Background / rationale. The workflow already carries a sample_depth_general factor (subsurface, surface) but pools depths and applies a single CF distribution. Hoffman et al. (2026) show the smallest MPs behave as near-uniformly-mixed tracers while larger settling particles accumulate at depth and larger buoyant particles accumulate near the surface — so samples collected near a boundary are enriched with larger particles and the apparent slope is depth-dependent. Pooling surface and subsurface grabs therefore biases the fitted slope in an uncontrolled, polymer-density-dependent direction.
Files/functions to inspect: the monitoring construction and its sample_depth_general recode; the C-PSD fit (fit_cpsd_segur_r) and correction_factor() calls.
Implementation:
1.	Fit the C-PSD and derive CF distributions separately by depth stratum where sample sizes permit; otherwise implement a sensitivity mode that runs the full pipeline for surface-only, subsurface-only, and pooled, and reports the three EED/RQ results side by side.
2.	Add a diagnostic plot of C-PSD slope by depth stratum and shape.
Documentation & citation requirements: Prose explaining depth-dependent enrichment, citing Hoffman et al. (2026); state explicitly whether stratified or pooled results are used for the headline assessment and why.
Acceptance criteria: A depth-stratified sensitivity table for EED and RQ is produced; if data are too sparse to stratify, this is stated and the pooled result is retained with a documented caveat.

SPEC 4 — Add a piecewise / dual-slope PSD diagnostic and estimate the critical size
Background / rationale. The current pipeline extrapolates a single slope across 1–5000 µm (≈3.7 decades) from a fit window of ≈0.4 decade (e.g., 165–385 µm). Hoffman et al. (2026) argue this is exactly where a single power law can fail and recommend testing whether piecewise or size-dependent power-law descriptions better represent environmental PSDs. The transport-controlled critical size is where the sampling depth equals the mixing length (δ = Lₘ, with Lₘ = K/ws), i.e., where ws(d)·δ/K ≈ 1. Real PSDs are known to deviate from a single power law at the fine end.
Files/functions to inspect: fit_cpsd_segur_r(); the C-PSD fit windows/LODs.
Implementation:
1.	Implement estimate_critical_size(delta, K, density, ...) that returns the size d_c solving ws(d)·δ/K = 1, using a standard MP settling/rise-velocity model. Per Hoffman, the choice of settling equation is not the dominant error — 3-D geometry is — so any reasonable equation is acceptable; parameterize δ and K as explicit inputs with documented defaults.
2.	Add a diagnostic that overlays the fitted single power law with a two-segment fit (slopes free to differ) and reports whether the fit window straddles d_c. If it does, flag that the fitted slope is likely a distorted α − 2 apparent slope and that the downward extrapolation to 1 µm is biased.
3.	Do not replace the production CF with a piecewise CF by default — deliver it as a labeled sensitivity case unless SPEC 1/2 review concludes otherwise.
Documentation & citation requirements: Give the f(d,δ) = 1 − exp(−ws(d)·δ/K) capture-fraction relationship and the δ = Lₘ critical-size definition in prose, cite Hoffman et al. (2026), and clearly label δ and K as user-supplied assumptions with their sources.
Acceptance criteria: The report shows, per compartment, d_c for the assumed δ/K and whether the fit window straddles it; a two-segment-fit sensitivity is available.

SPEC 5 — Replace the fixed 1.1 g/cm³ density with a polymer-resolved density distribution
Background / rationale. Volume/mass are computed with an ellipsoid model (fragments) and a cylinder model (fibers), with a single fixed density of 1.1 g/cm³. Hoffman et al. (2026) find that 3-D geometry and density inputs — not the settling equation — dominate transport error, and the sign of the settling/rise velocity (settle vs rise) is set by whether density is above or below the water density. A single density cannot represent both settling and buoyant polymers, which behave oppositely in the depth-stratification logic of SPEC 3.
Files/functions to inspect: the V_um3 / mass_ug computations for river/sediment/ocean; the µFTIR polymer-ID field in Part_dets_summ.rds.
Implementation:
1.	Join a polymer→density lookup (parameterized, with a cited default table) and replace the fixed 1.1 with a per-particle or per-polymer density distribution.
2.	Derive a buoyant vs settling flag per polymer (relative to freshwater/seawater density as appropriate) for use by SPEC 3/4.
3.	Keep the fixed-density result as a labeled sensitivity comparison.
Documentation & citation requirements: Prose noting Hoffman's geometry/density-dominance finding (cite Hoffman et al. 2026), the density source table, and that the buoyant/settling split feeds the depth-stratified transport logic.
Acceptance criteria: Density is polymer-resolved; a settling/buoyant flag exists; the fixed-1.1 case is retained as sensitivity.

SPEC 6 — Add a water–sediment size-partition coherence check
Background / rationale. The sediment compartment (pooled slope α ≈ −1.99) is the destination of exactly the large, dense particles that differential settling removes from the water column. Under Hoffman's mechanism the water-column and sediment PSDs are two ends of one size-sorting process, not independent compartments; a water slope artificially steepened by loss of large particles should correspond to sediment enrichment in those same sizes.
Implementation: Add a joint diagnostic comparing river-water and sediment C-PSD slopes and size spectra, and a short prose interpretation of whether the water slope may be transport-steepened (cross-referencing SPEC 4's d_c).
Documentation & citation requirements: Cite Hoffman et al. (2026); frame as a mass-balance/coherence check, explicitly labeled as interpretive (inference), not a new correction.
Acceptance criteria: A water-vs-sediment slope/spectrum comparison figure plus interpretive prose exists.

SPEC 7 — Revise the "no further correction warranted" narrative conclusion
Background / rationale. The report currently concludes that no additional data correction is warranted and that the only outstanding methodological uncertainty is the µFTIR objective, which it states affects absolute concentrations but not the relative risk ranking. Hoffman et al. (2026) introduce a second, larger outstanding uncertainty — transport-induced PSD distortion and the validity of single-power-law extrapolation — which acts differentially on fragments vs fibers and surface vs subsurface, and therefore can affect both absolute concentrations and potentially the relative ranking.
Implementation: Rewrite that conclusion to add transport-induced PSD distortion as a named, unresolved uncertainty; cross-reference SPECs 1–6; retain the µFTIR-objective caveat.
Documentation & citation requirements: Cite Hoffman et al. (2026); keep the tone calibrated (do not overstate — the ranking is likely robust even if magnitudes move).
Acceptance criteria: The concluding uncertainty discussion lists transport/PSD-structural uncertainty and points to the new sensitivity analyses.

SPEC 8 — Add a geometry-priority guardrail note
Background / rationale. Hoffman et al. (2026) show that measurements of 3-D particle shape matter more than the choice of settling-velocity model, and that even with precise geometry the best settling models retain >60% average error. This should steer any future transport-correction work toward better geometry/density inputs rather than more elaborate settling equations.
Implementation: Add a short "Modeling-priorities" note in the methods discussion codifying: (a) invest in 3-D geometry/density (SPEC 5) over settling-equation refinement; (b) treat any settling-based correction as bias-level, not high-precision.
Documentation & citation requirements: Cite Hoffman et al. (2026) for both the geometry-dominance and the >60%-residual-error findings.
Acceptance criteria: The note exists and is cross-referenced from SPECs 4 and 5.

3. Execution order and dependencies
1.	SPEC 1 first (blocking — everything downstream depends on the correct CF).
2.	SPEC 5 (density/geometry) before SPEC 3/4, since depth stratification and critical-size logic need the settling/buoyant split.
3.	SPEC 2 and SPEC 4 can proceed in parallel after SPEC 1.
4.	SPEC 3 after SPEC 5.
5.	SPEC 6, SPEC 7, SPEC 8 (documentation-heavy) last, after the analytical specs settle.
4. Final deliverables checklist
•	[ ] hoffman-2026-revisions branch with one commit per spec.
•	[ ] docs/CHANGELOG_hoffman_revisions.md with verification outcomes and before/after numbers for every spec.
•	[ ] Passing testthat unit test locking the CF slope convention (Koelmans CF ≈ 8.32 worked example).
•	[ ] Re-rendered report with: corrected/verified CF; widened slope-uncertainty propagation; depth-stratified sensitivity; critical-size diagnostic and two-segment sensitivity; polymer-resolved density with settling/buoyant flag; water–sediment coherence figure; revised conclusion; geometry-priority note.
•	[ ] Every implemented change accompanied by cited rationale prose in the notebook (sources per §2), with epistemic status labeled (measured / inferred / model / assumption).
•	[ ] A one-paragraph executive summary at the top of the report stating whether the headline risk conclusion (currently Food Dilution RQ ≫ 1, P(RQ>1) ≈ 99.9%; Tissue Translocation RQ ≪ 1) is unchanged, and by how much magnitudes moved, after all revisions.
5. Guardrails / do-not
•	Do not apply the Zhao et al. mesh-selectivity correction on top of the power-law CF for the membrane-filtered µFTIR data — that is deliberately excluded to avoid double-counting; leave that decision unchanged unless SPEC 1 review reveals a related inconsistency.
•	Do not convert the workflow to mass-based exposure; it is count-based (particles/L, particles/kg) by design.
•	Do not fabricate settling-model parameters (δ, K) — surface them as explicit, cited assumptions and run them as sensitivities.
•	Do not overwrite prior report versions; increment and keep the legacy render for comparison.
