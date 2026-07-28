# Hoffman et al. (2026) Revision Changelog

Date: 2026-07-28

## SPEC 1 - slope convention passed to `correction_factor()`

Verified `R/mp_risk_utils.R::correction_factor()` integrates the exponent it receives directly as `L^(a + 1)` and does not internally subtract 1 unless `slope_convention = "cumulative"` is now requested. The prior notebook path passed the cumulative C-PSD slope (`a_cpsd`) as if it were the differential PSD slope, reproducing the legacy CF values.

Changed the notebook CF path to sample and pass the differential slope `a_psd = a_cpsd - 1` with `slope_convention = "differential"`. Added a `testthat` guard reproducing the Koelmans-style worked example: 30-2000 um to 1-5000 um with alpha = 1.6 gives CF = 8.319.

Verified before/after CF medians from a lightweight local R run:

| Matrix | `a_cpsd` | `a_psd` | bias-corrected measured range (um) | Legacy CF p50 | Corrected fit-only CF p50 | Corrected structural-SD CF p50 | Corrected structural-SD CF p05-p95 |
| --- | ---: | ---: | --- | ---: | ---: | ---: | ---: |
| River water | -1.926 | -2.926 | 165-385 | 208.0 | 23077.8 | 23257.0 | 3110.2-186335.7 |
| Sediment | -1.990 | -2.990 | 135-500 | 177.2 | 18759.1 | 18760.8 | 2604.3-133648.7 |
| Ocean water | -1.767 | -2.767 | 95-245 | 63.4 | 3844.1 | 3783.7 | 666.9-23693.3 |

Open questions: Full report render is still needed to capture before/after EED, RQ, and P(RQ>1) after PSSD++ hazard propagation.

## SPEC 2 - widened slope uncertainty

Added `slope_structural_sd` as an explicit model assumption in the QMD setup and extended `alpha_dist()` so structural/model-form uncertainty is added in quadrature to the fit SE. The river MC2D sensitivity table samples widened CF distributions in the outer uncertainty loop.

Open questions: `slope_structural_sd = 0.25` is a documented default aligned with the wide-prior precedent, not a measured site-specific value.

## SPEC 3 - depth-stratified sensitivity

Verified river surface and subsurface strata are both large enough for C-PSD and monitoring bootstrap sensitivity. Added depth-stratified C-PSD slope plotting plus surface/subsurface EED and HC5 RQ sensitivity tables.

Open questions: Pooled results remain the headline assessment until a mechanistic depth/transport correction is parameterized.

## SPEC 4 - critical size and dual-slope diagnostic

Added `estimate_critical_size()` using explicit delta and K assumptions and `piecewise_cpsd_diagnostic()` for two-segment C-PSD screening. Added an all-matrix table reporting critical size, fit-window straddling, and piecewise AIC diagnostics.

Open questions: `delta_m = 0.10` and `K_m2_s = 1e-5` are placeholders that need field justification before mechanistic use.

## SPEC 5 - polymer-resolved density

Added a polymer-density lookup, per-particle `density_g_cm3`, density group, freshwater/marine buoyancy flags, and fixed-density sensitivity mass columns. The lookup includes TODO(cite) source placeholders rather than fabricated bibliographic detail.

Open questions: Select and cite the canonical polymer-density source table.

## SPEC 6 - water-sediment coherence check

Added a river-water vs sediment size-spectrum ECDF and slope summary to interpret whether sediment enrichment is coherent with transport sorting.

Open questions: This is interpretive inference only; no mass-balance correction was applied.

## SPEC 7 - revised conclusion

Replaced the previous "no additional data correction warranted" statement with a calibrated uncertainty discussion naming transport-induced PSD distortion while retaining the existing mesh-selectivity and Wang et al. double-counting exclusions.

## SPEC 8 - geometry-priority guardrail

Added a modeling-priorities note directing future transport work toward measured 3-D geometry and density before settling-equation refinement, and labeling settling diagnostics as bias-level sensitivities.

---

# Corrective follow-up revisions (2026-07-28)

Branch work described below corrects three residual issues left by the SPEC 1-8 revision above: the volume/area hazard-slope convention (SPEC 1b), an unguarded correction-factor convention (SPEC 1c), and an unpropagated piecewise PSD diagnostic (SPEC 2b), then updates the executive summary to reflect both (SPEC 3b). Numbers below are from a development-scale render (`n_boot = 10`, `n_mc = 20000`); a full-budget production re-run is still required before external use (see deliverables checklist in the corrective spec).

## SPEC 1c - Koelmans CF = 8.32 convention guard

Added a rendered, in-notebook assertion (`R/probabilistic_risk_characterization.qmd`, new "5.1.0 Convention guard" subsection) reproducing the Koelmans et al. (2020) worked example: 30-2000 um -> 1-5000 um at alpha = 1.6 gives CF = 8.32. Companion assertion locks `cpsd_fit_all$a_psd == cpsd_fit_all$a_cpsd - 1` on a real fit object. Both `stopifnot()` guards passed in the dev-scale render (rendered value: CF = 8.319). The pre-existing `tests/testthat/test-correction_factor.R` (2 tests) continues to pass and now has a rendered-notebook counterpart, not just an unrendered test file.

## SPEC 1b - volume/area hazard-slope convention (cumulative -> differential)

**Root cause confirmed before editing:** in all three `param_values*` blocks, `a.v.*`/`a.sa.*`/`a.m.*` were assigned `-cpsd_fit_volume$a_cpsd` / `-cpsd_fit_area$a_cpsd` (cumulative C-PSD slope) instead of `$a_psd = a_cpsd - 1` (differential BN-PSD slope), exactly 1.0 too shallow versus the package's Kooi (2021)-derived defaults (a.v.freshwater default 1.68 vs. the pre-fix assigned ~0.7-1.3).

**Fix:** changed all three param_values blocks (`R/probabilistic_risk_characterization.qmd`, river ~L1188, sediment ~L2296, ocean ~L3040) to use `$a_psd`/`$se_a_psd`. Set `a.m.*` explicitly from `-cpsd_fit_volume$a_psd` for all three matrices (mass proportional to volume at constant density) rather than leaving river's `a.m.freshwater` at the unrelated package default (1.65), which had been inconsistent with the corrected `a.v.freshwater`. Added `stopifnot(abs(a.v - a.m) < 0.3)` after each block.

**Before/after Food Dilution RQ decomposition (river, dev-scale `n_boot=10`; new Section 13.1 in the qmd, `legacy_cumulative_av_params()` in `R/mp_risk_utils.R`):** reverting only `a.v`/`a.sa`/`a.m` to the legacy cumulative convention (holding exposure and the length-slope alignment at their corrected values) and re-running the pSSD++ hazard pipeline shows the same directional error described in the spec (Food Dilution RQ inflated under the legacy cumulative slope) at this matrix; see the rendered Section 13.1/14.1/15.3 tables for the exact dev-scale ratio, which is expected to be much smaller than the spec's production-scale ~4-6 million-fold mean-particle-volume estimate because of the small `n_boot`/`sim` development budget. Sediment and ocean water are included in the Section 13.1 all-matrix table using the same `legacy_cumulative_av_params()` helper.

Open questions: exact production-scale collapse factor requires the full-budget re-run; `a.m.*` treatment (set from data vs. retaining defaults) was decided in favor of "set from data" for internal consistency — flagged for review.

## SPEC 2b - piecewise (dual-slope) PSD propagated as a bounding exposure case

Added `correction_factor_piecewise()` to `R/mp_risk_utils.R`: integrates `dN/dL = k*L^a` with the fine-segment differential slope over `[L_tar_min, break_um]` and the coarse-segment differential slope over `[break_um, L_tar_max]`, matching the two segments' density at `break_um` for continuity; accepts `slope_convention = "cumulative"` to convert the piecewise diagnostic's cumulative slopes internally. Verified in isolation: reduces exactly to `correction_factor()` when `a_low == a_high`, handles windows fully above/below/straddling the break, and vectorizes correctly.

Propagated as a labeled bounding sensitivity in a new Section 13.3: per matrix, computes the piecewise CF from the existing `piecewise_cpsd_diagnostic()` break/slopes, applies it to the raw per-sample measured concentrations (exposure-only effect; hazard is unchanged), bootstraps the EED, and reports single-slope (production) vs. piecewise (bounding) CF, EED, and Food Dilution/Tissue Translocation HC5 RQ side by side. The single-slope CF remains the production default per the spec's guardrail; the piecewise result is reported strictly as a sensitivity bound quantifying the unconstrained 1 um-to-LOD extrapolation zone.

Open questions: whether the piecewise bound narrows or widens the risk conclusion is matrix-dependent and is read directly off the rendered Section 13.3 tables rather than assumed in this changelog entry.

## SPEC 3b - executive summary and RQ decomposition

Added Section 14.1 (river-water RQ decomposition isolating (i) the exposure correction factor, (ii) the hazard length-slope alignment, and (iii) the hazard volume/area-slope alignment as one-at-a-time reversions relative to the fully corrected baseline) and Section 15.3 (fully quantified revised executive summary using only inline-computed values, added after all matrices' corrected and legacy pipelines are in scope). Section 1's introductory executive-summary paragraph was rewritten to describe both slope-convention fixes qualitatively and explicitly defers all numbers to Section 15.3, so no stale figure (e.g., an uncorrected ~10^7 Food Dilution RQ) remains asserted as current in the narrative.

Open questions: Section 14.1's decomposition is computed for river water only (representative matrix, one additional pSSD rerun) to bound compute at development scale; the SPEC 1b before/after table (Section 13.1) already covers all three matrices for the volume/area-slope piece specifically.

---

# Corrective follow-up revisions, round 2 (2026-07-28): SPEC 3c and SPEC 1d

Two residual issues addressed: an incorrect Tissue Translocation attribution in the SPEC 3b narrative (SPEC 3c), and an implausible sediment pathway traced to exposure-concentration provenance (SPEC 1d). Numbers below are from a development-scale render (`n_boot = 10`, `n_mc = 20000`); a full-budget production re-run is still required before external use.

## SPEC 3c - Tissue Translocation attribution correction

**Root cause confirmed before editing:** the SPEC 3b narrative stated Tissue Translocation P(RQ>1) is "68% under the legacy volume/area slope, i.e. essentially unchanged from the corrected value" of 30.9%. 68% -> 30.9% is more than a 2x reduction, not "essentially unchanged" (verified against the rendered river-water decomposition table before making any change: legacy a.v+a.sa+a.m RQ_p50 = 2.438, P(RQ>1) = 68.0%; corrected RQ_p50 = 0.210, P(RQ>1) = 30.9%). Mechanistically, Tissue Translocation/oxidative-stress alignment is surface-area-weighted, so it is expected to be sensitive to the `a.sa` correction even though it is (correctly) insensitive to the volume slope.

**Fix:** added a fourth one-at-a-time reversion row isolating `a.sa` alone (river, one additional pSSD rerun: `param_values_legacy_asa_river` reverts only `a.sa.freshwater`/`.sd` to the legacy cumulative slope, holding `a.v`/`a.m`/`alpha`/exposure at their corrected values). The Section 14.1 decomposition table now reports both ERMs (previously Food-Dilution-only) with all four reversion rows. Rewrote the Tissue Translocation narrative in Section 14.1 and Section 15.3(c) to state that both the length-slope and surface-area-slope fixes are material drivers, retracting the "essentially unchanged" claim; also corrected the same stale claim where it recurred in the Section 13.1 prose.

Open questions: the a.sa-only isolation is river-only (compute budget); full-budget re-run should confirm the magnitude holds for ocean/sediment (sediment numbers are illustrative regardless per SPEC 1d below).

## SPEC 1d - sediment-pathway audit

**1d-i (concentration provenance) -- root cause confirmed:** traced `monitoring_sed$C_measured_pkg` to `extrap_conc_PPL`, the same column used for river (`C_measured_pL`) and ocean water. Verified via a rendered data audit (`data_input/Part_dets_summ.rds` has no sample dry-weight, wet-weight, or bulk-density column for any `sample_type`, including "beach sand"; `extrap_count / extrap_conc_PPL` reconstructs to a single fixed volume-like divisor regardless of sediment sample, i.e., a per-liter processing/pumped-volume normalization, not a per-kg dry-sediment normalization). A separate large-microplastics dataset (`Opt_micro_all_cut.rds`, used only in the unrelated, non-sourced `R/Util.R`) independently records its own "beach sand" samples on a `Volume_Pumped_L` basis, corroborating that this sampling program's beach-sand processing is volume-based, not dry-mass-based. **No genuine particles/kg-dry-weight sediment concentration exists in the data available to this analysis.** Per the spec's fallback path, the sediment concentration/pathway is flagged non-quantitative/illustrative via a new `sediment_concentration_is_quantitative <- FALSE` flag (Section 11.3.0) rather than corrected in place, and is excluded from the headline risk characterization (Sections 14, 15.3) while the underlying EED/RQ machinery is retained for illustrative/methods purposes (e.g., confirming the SPEC 1b slope-convention mechanism holds across matrices in Section 13.1).

**1d-ii (hazard-unit alignment) -- verified, no discrete code fix identified:** traced `dose_unit = "kg"` through `run_pssd_pipeline()` (`R/mp_risk_utils.R`) into `PSSDplusplus::MC_sim_align_parallel()`/`align_data()`/`convert_units_fxn()` (installed package source, inspected via `deparse()`). Found that `particles_kg_food_dilution`/`particles_kg_ox_stress` are populated directly from each sediment study's own reported dose (`dose.particles.kg.sediment.master`, coalesced from ToMEx's `measured.dose.particles.kg.sediment`/`nominal.dose.particles.kg.sediment`), not derived from a water-based value via any hardcoded conversion factor; the raw ToMEx sediment doses span a physically plausible range (tens to ~1e10 particles/kg dw), consistent with Redondo-Hasselerharm et al. (2023). No missing bulk-density/dry-weight conversion bug was found in this code path. The implausibly low derived sediment HC5 is therefore attributed to a combination of the tiny development-scale Monte Carlo budget and size-alignment sensitivity to the sediment ERM exponents (same mechanism as SPEC 1b) rather than a specific incorrect line, and is flagged as an open question for the full-budget production re-run. This finding is documented as a rendered trace in Section 11.4.0; it does not change the SPEC 1d-i exclusion decision.

**1d-iii (benthic-HC5 sanity guard):** added a rendered guard (Section 11.4.0) comparing the derived sediment Food Dilution/Tissue Translocation HC5 against a plausible band (1e1, 1e12 particles/kg dw; anchored on Redondo-Hasselerharm et al. 2023's ~4.9e9 and the wider IJC Great Lakes SAB 2024 spread down to 62.6). Because `sediment_concentration_is_quantitative` is FALSE, the guard is rendered as a loud, informative, non-fatal `message()` rather than a render-halting `stopifnot()` (which would prevent river/ocean sections from rendering at all given this pathway cannot be fixed with available data); it becomes a hard `stopifnot()` automatically the moment that flag is set TRUE. The guard confirms the derived sediment Food Dilution HC5 fails the plausible band by roughly eleven orders of magnitude, as the spec anticipated.

**1d-iv (sediment-alpha rule-out) -- VERIFIED, NO CHANGE:** confirmed (already correct from the SPEC 1b fix) that `alpha.sediment.freshwater` (~2.99), `a.v.sediment.freshwater` (~1.71), `a.sa.sediment.freshwater` (~2.22), and `a.m.sediment.freshwater` (~1.71, = a.v) are each on the differential convention. Added rendered `stopifnot()` assertions (Section 11.2) that each equals `-(a_cpsd - 1)` of its underlying fit, plus the existing `a.v` ~= `a.m` consistency check. Documented in prose that the sediment RQ magnitude is therefore driven by 1d-i/1d-ii, not the slope convention.

**Downstream headline-conclusion changes:** Section 14's combined RQ plot now labels the sediment series "(illustrative only, SPEC 1d)"; Section 15.3(a) no longer reports the sediment Food Dilution RQ as a corrected finding alongside river/ocean (moved to a caveat sentence); added new Section 15.3(e) stating the sediment exclusion explicitly and updated the closing "Summary" paragraph accordingly.

Open questions: whether a genuine dry-weight sediment concentration can be sourced from field/lab records outside this repository is a data-collection question for the study team, not a code question; 1d-ii's attribution (MC-budget noise vs. size-alignment sensitivity) should be revisited once a real sediment concentration exists and a full-budget run is available to separate those two effects.

---

# Corrective follow-up revisions, round 3 (2026-07-28): SPEC Q1 and SPEC 1e

Two scoped additions on branch `sediment-temporal-and-cpsd-bitparity`: reconciling the native-R vs. Segur-Python C-PSD fits to bit-identity (SPEC Q1, a QA fix), and incorporating the Thuy-Dung et al. (2026) temporal-fragmentation framing into the sediment section (SPEC 1e, a framework/sensitivity addition). Neither disturbs SPEC 1/1b/1c/1d/2b/3b/3c or the river/ocean pipelines; sediment remains excluded from the headline risk characterization (`sediment_concentration_is_quantitative` stays FALSE). Numbers below are from the same development-scale render (`n_boot = 10`, `n_mc = 20000`) as prior entries.

## SPEC Q1 - R/Python C-PSD bit-identity

**Root cause confirmed before editing:** `fit_cpsd_segur_r()`'s two-step LOD-window *search* (`.segur_detect_lod()`) already restricted itself to populated (`n > 0`) bins, matching `PSD_fit.py` exactly (its input CSV never contains zero-count bins: `segur_bins_from_fit()` filters `n > 0` before export, and the script's `__main__` re-filters `Bin_concentration != 0`) — this is why the two implementations always agreed on the LOD window. But the *final* regression (`R/mp_risk_utils.R:203-211`, pre-fix) re-derived its data (`df_full`/`df_fit`) from the full uniform `bin_um` grid, including empty (`n == 0`) bins, instead of reusing the already-populated-only subset the window search used. This silently regressed the reported slope over many redundant flat-`N_ge`-step points at fixed bin-width intervals, inflating `n_bins` and biasing `a_cpsd` — worst case ocean-fiber, R `n_bins` = 102 vs. Python `n_bins_used` = 5, `|delta_a_cpsd|` ~= 0.044 (matching the spec's cited numbers exactly).

**Fix:** added `n > 0` to `df_full`'s filter (`R/mp_risk_utils.R`, `fit_cpsd_segur_r()`), so the final regression uses the identical populated-bin set already used for LOD-window selection and already exported to Python. One-line change; no other function signature or behavior touched.

**Verified in isolation before touching the qmd:** reproduced the ocean-fiber case end-to-end (real particle data from `data_input/Part_dets_comb.rds`, real `PSD_fit.py` run) post-fix: R `a_cpsd = -0.980550233595`, `n_bins = 5`; Python `slope_C_PSD = -0.980550233595`, `n_bins_used = 5`; `|delta_a_cpsd| = 2.26e-13` (well under the 1e-6 acceptance bound), on the single worst-offending PSD identified by the spec.

**qmd changes (Section 12.1.2):** added prose explaining the fix and its rationale; added a rendered `stopifnot()` acceptance check after the R/Python comparison table asserting `max(abs(delta_a_cpsd)) < 1e-6` and `n_bins_R == n_bins_Python` across all nine matrix x shape PSDs, printing a diagnostic table of any mismatch before failing rather than failing silently; added a standing note flagging the sparse ocean-fiber fit (5 populated bins) as low-n regardless of the bit-identity result.

Open questions: none identified in the isolated worst-case reproduction; the full nine-PSD assertion in the rendered document is the authoritative check going forward, and any future residual it reports would need its own root-cause note per the spec's acceptance criteria (empty-bin handling / tie-breaking), rather than being left unexplained.

## SPEC 1e - sediment temporal-fragmentation framing and PSD-shift sensitivity

Added a new Section 11.5 ("Sediment as a time-dependent, accumulating compartment"), placed after the existing Section 11.4 risk characterization and before Section 12 (Ocean Water), plus a Section 15.4 companion once every matrix's hazard objects exist in scope. All new R functions live in `R/mp_risk_utils.R`.

**1e-i (narrative reframing):** added the required bullets (sediment as time-dependent/accumulating; any static metric as a lower-bound snapshot; cross-reference to the Hoffman et al. (2026) differential-settling sink argument already used in Section 13's critical-size diagnostic) citing [Thuy-Dung, Groenenberg & Koelmans (2026)](https://doi.org/10.1186/s43591-026-00210-8). Added one sentence to the Section 1 introductory executive summary and a new point (f) to the Section 15.3 revised executive summary, both cross-referencing Section 11.5 without re-stating any number as current outside that section.

**1e-ii (PSD-shift sensitivity, illustrative; production-gated):** added `sediment_psd_shift_sensitivity()` (`R/mp_risk_utils.R`) -- re-runs the *full* sediment hazard alignment (`matrix_function()` -> `run_pssd_pipeline()` -> `build_haz_df()`) across an explicit, cited scenario grid of added shifts to the differential length/area/volume/mass slopes (`slope_shift_grid = c(0, 0.75, 1.5)`, positive-alpha convention), using the *same* `n_boot`/`sim`/`cv_uf`/`rmore_method` as the production sediment call (no analytical-budget change). A full re-run (rather than only shifting the exposure-side CF/EED) was necessary because steepening the PSD changes the simulated particle-size population that pSSD++'s ingestible/translocatable size-gating operates on, i.e. it can change *which* species clear the bioaccessibility threshold, not just rescale an existing number. Deliberately does **not** transplant Thuy-Dung et al.'s k_frag/shell-geometry model (calibrated to polymer-coated-fertilizer prills with 7-year field data, not this dataset's heterogeneous mixed-polymer sediment MPs of unknown age); the shift grid is an explicit scenario assumption instead. Gated on `sediment_concentration_is_quantitative` (FALSE): every returned row carries `is_quantitative = FALSE` and the rendered table/plot/prose label every number illustrative and excluded from the risk conclusion, mirroring the SPEC 1d-iii guard pattern.

**Rendered result and an honest surprise:** at this development-scale budget the derived HC5 *rose* rather than fell across the grid (Food Dilution: 0.098 -> 6.653 -> 6.738 particles/kg dw from shift 0 -> 0.75 -> 1.5; Tissue Translocation: 12.738 -> 12.903 -> 13.376), the opposite of the direction Section 11.5.1's mechanism would naively suggest. Rather than force the narrative to fit the a priori expectation, the qmd's interpretation paragraph (Section 11.5.2) reports the observed direction plainly and attributes it to SSD instability: the sediment benthic SSD has only 7-9 species (Section 11.5.4), so shifting which particles clear the size gate can change *which* one or two species anchor the low-percentile HC5 extrapolation, and with this few species that anchor can swap non-monotonically. This instability is itself consistent with -- and part of the motivation for -- Thuy-Dung et al.'s point that individual-species risk can be a more stable early-warning signal than a community HC5 extrapolated from a small SSD, which is exactly why 1e-iv is reported alongside it.

**1e-iii (time-varying bioaccessibility):** documented as a clearly-scoped TODO rather than implemented, because a defensible residence-time/age-of-deposition axis does not exist in the source sediment dataset -- fabricating one would misrepresent the method's evidentiary basis. The TODO explicitly notes the extension reuses the existing pSSD++ alignment machinery (food-dilution gut-volume alignment; translocation truncated at 500 um) already exercised by 1e-ii's scenario grid, and only needs a time axis (Delta-alpha(t)) to convert that scenario grid into a trajectory HC5(t)/RCR(t).

**1e-iv (most-sensitive-species screen):** added `most_sensitive_species()` (groups an `erm_registry` tier by `Species`, returns the species with the lowest median aligned dose per ERM alongside `n_species`) and `individual_species_rcr()` (RCR = EED / lowest-species EC, as a distribution over `eed_boot` draws). Applied to the sediment benthic dataset (n = 89 records, 9 species; Section 11.5.4) alongside the existing community HC5 from Section 11.4, and as a companion table for river/ocean (Section 15.4, added *after* Section 15.3 because it is the first point in document execution order where `haz`/`haz_ocean`/`eed_boot`/`eed_boot_ocean` all exist -- placing it earlier would forward-reference not-yet-computed ocean objects).

**Rendered results:** sediment's most sensitive species is *Chironomus tepperi* for both ERMs (Food Dilution median EC = 1.20, Tissue Translocation median EC = 185 particles/kg dw; 7 of 9 species retained after the ERM-specific alignment filters). River: *Moina macrocopa* (Food Dilution, 3340 particles/L) and *Ceriodaphnia dubia* (Tissue Translocation, 144570 particles/L). Ocean: *Pseudechinus huttoni* for both ERMs (32.4 / 3431 particles/L). The Section 15.4 individual-RCR companion table shows the individual-species RCR below the community-HC5 RQ in both river rows at this development-scale budget (Food Dilution: RCR_p50 = 0.186 vs. community RQ_p50 = 10.905; Tissue Translocation: RCR_p50 = 0.004 vs. 0.209) -- i.e., at present the single most-sensitive species identified is not yet the risk-driving signal relative to the community HC5 extrapolation, which is itself informative context for reading the SPEC 1e-ii sediment result above.

Open questions: 1e-ii's grid is a coarse 3-point illustrative scenario, not a calibrated forecast -- exact magnitude/direction of the HC5 response should be read from the rendered Section 11.5.2 table/plot, not assumed here; 1e-iii's TODO cannot be closed without a residence-time estimate for this sediment dataset, which is a data-collection question for the study team.
