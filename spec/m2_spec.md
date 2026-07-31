Model Spec M2 — Decomposition of the M1.2 Floor Effect
Separating study-filtration (composition) from exposure-CF-extrapolation and hazard-integration components of the L_tar_min sensitivity
Status: New spec. Diagnostic experiment. No production headline numbers change. All outputs are labeled decomposition diagnostics, not risk estimates. Depends on: v11932 harness (M1.2 floor-sensitivity machinery already implemented). Scope: Surface water only — Rivers (pooled) and Ocean. Ignore sediment entirely. Both ERMs (Food Dilution + Tissue Translocation).

1. Motivation & hypothesis
1.1 What M1.2 showed (do not re-derive; reuse the rendered v11932 results)
M1.2 varied a single control knob, L_tar_min, across {1, 2.5, 5, 10} µm and found the Food Dilution RQ collapses ~100–130× as the floor moves off 1 µm (river 1D RQ: 1 µm = 10.8 → 2.5 µm = 0.082 → 5 µm = 0.033 → 10 µm = 0.005; ocean: 1 µm = 3.99 → 2.5 µm = 0.87 → 5 µm = 0.057), while the HC5 jumps ~19× (river 64.7 → 1215 over 1 → 2.5 µm). This is not a gentle monotone trend — it is a discontinuity concentrated at the 1 → 2.5 µm step.

1.2 The confound
In M1.2, moving L_tar_min moves three things at once:

Exposure CF extrapolation — the correction factor integral's lower bound (x1D) shifts, changing the extrapolated particles/L (EED q50 drops as floor rises).
Hazard integration range — the pSSD++ volume/surface-area alignment lower bound (x1M/x1D) shifts, changing how each tox record's effect concentration is re-aligned to the 1–5000 µm (or floor–5000 µm) ERM window.
Study filtration / taxonomic composition — filter_tox_by_size_floor(tox, L_tar_min) drops every tox record whose characteristic size falls entirely below the floor. At high floors this collapses the SSD (n_species river 19 → 14, ocean 23 → 10; ocean FD SSD fails at 20 µm; marine fish records collapse 170 → 1 at a 20 µm floor).
Because these move together, M1.2 alone cannot attribute the RQ collapse to a mechanism. The candidate explanations are:

(A) genuine small-particle food-dilution potency (small particles legitimately drive food-dilution effects);
(B) volume-alignment / mechanism-misassignment artifact — small-particle tox studies whose effects are actually translocation/oxidative-stress-mediated get force-aligned to the volume-based food-dilution ERM, yielding an artificially low HC5 at a 1 µm floor;
(C) legitimate exposure de-extrapolation (raising the floor removes the vast, unmeasured sub-LOD particle count);
(D) SSD instability at high floors (small n_species);
(E) taxonomic re-composition (loss of sensitive marine fish records).
1.3 Prior hypothesis (to be tested, NOT assumed)
Mechanistically, (B) is favored over (A): for food dilution, larger particles are the potent ones (gut-filling / volume in gut), whereas smaller particles drive translocation, not food dilution. Small particles are more likely to translocate, and translocation-relevant records are those <83 µm; food dilution is aligned to volume and translocation to surface area over a common 1–5000 µm window. If effects observed at low plastic volume in small-particle studies are translocation/oxidative-stress-mediated but get aligned to the volume-based food-dilution ERM, the food-dilution HC5 is artificially depressed at low floors. This spec is designed to test whether the collapse is composition-driven (consistent with B/E) or CF-driven (consistent with A/C). Do not hard-code the conclusion.

2. Experimental design — hold composition fixed, vary only the extrapolation
Core idea: Freeze the study-filtration confound by filtering the tox database once at a common floor, then reuse that fixed tox set at every exposure floor. This removes component (3) [composition] so any residual RQ trend must come from (1) [exposure CF] and/or (2) [hazard integration].

2.1 Fixed tox set (single source of truth)
## M2: freeze taxonomic composition ONCE at the 10 µm reference floor.
## This is the SAME filtered set used at every treatment floor below.
TOX_FIXED_FLOOR <- 10  # µm; matches M1.2's high-floor reference
tox_fixed <- filter_tox_by_size_floor(tox, TOX_FIXED_FLOOR)
## filter_tox_by_size_floor() uses size.length.um.used.for.conversions (monodisperse)
## or size.length.max.um.used.for.conversions (polydisperse); excludes a record if
## its characteristic size falls entirely below the floor; missing size => excluded.
## tox_fixed is COMPUTED ONCE and never re-filtered inside the floor loop.
Rationale for choosing 10 µm as the freeze point: it is the highest M1.2 floor at which both river and ocean FD SSDs still converge (ocean fails at 20 µm), giving a composition-stable, well-conditioned SSD common to all arms. Caveat (must appear in output): filtering tox at ≥10 µm is mechanistically conservative for Tissue Translocation — it removes exactly the small (<83 µm, and especially <10 µm) particles that are most translocation-relevant. TT arm results are therefore a lower bound on TT hazard and must be read as a diagnostic contrast, not a TT risk estimate.

2.2 Treatment grid
Exposure floor L_tar_min ∈ {1, 2.5, 5, 7.5, 10} µm (adds 7.5 µm vs M1.2 to resolve the 5→10 shoulder).
Compartment ∈ {River (pooled), Ocean}.
ERM ∈ {Food Dilution, Tissue Translocation}.
Arm ∈ {A, B} (defined below).
2.3 The two arms
Arm A — pure exposure-CF extrapolation (primary diagnostic). Hazard is held completely fixed: use tox_fixed (≥10 µm) AND pin the pSSD++ integration bounds at the reference floor (x1M_set = x1D_set = 10) for every treatment. The HC5 is therefore a constant across the whole grid, identical to M1.2's 10 µm HC5 (river FD ≈ 1215; carry the rendered value, do not retype). Only the exposure correction-factor floor moves with L_tar_min. → Any RQ trend in Arm A is pure exposure-CF / de-extrapolation (component 1 in isolation).

Arm B — exposure CF + hazard-integration range (secondary diagnostic). Tox composition still frozen (tox_fixed, ≥10 µm), but now the pSSD++ integration bounds track the treatment floor (x1M_set = x1D_set = L_tar_min) and the exposure CF floor tracks L_tar_min. Composition is fixed but the alignment window breathes. → Arm B adds the hazard-integration-range effect on top of Arm A.

2.4 Three-way decomposition (per compartment × ERM, in RQ log-space)
For each treatment floor f:

ΔlogRQ_total(f)        = logRQ_M1.2(f)   − logRQ_M1.2(10 µm)     # from v11932, reused
ΔlogRQ_exposureCF(f)   = logRQ_ArmA(f)   − logRQ_ArmA(10 µm)     # pure CF
ΔlogRQ_hazardInteg(f)  = logRQ_ArmB(f)   − logRQ_ArmA(f)         # integration-range
ΔlogRQ_composition(f)  = logRQ_M1.2(f)   − logRQ_ArmB(f)         # study-filtration residual
Identity that must hold (arithmetic, not approximate): ΔlogRQ_total(f) ≈ ΔlogRQ_exposureCF(f) + ΔlogRQ_hazardInteg(f) + ΔlogRQ_composition(f) (exact in log-space because RQ = EED / HC5 and the three factors are multiplicative; any residual is Monte Carlo noise and must be reported).

Report each component as a fraction of the total collapse, e.g. "of the total 130× FD RQ collapse over 1→10 µm, exposure-CF accounts for X×, hazard-integration Y×, composition Z×."

2.5 Acceptance / self-consistency checks (must render and pass)
Coincidence at the freeze point: at L_tar_min = 10 µm, Arm A = Arm B = M1.2 (all three RQ values equal within MC tolerance). If they diverge, the harness is mis-wired — halt and report.
Arm A HC5 is flat: the Arm A HC5 must be identical (bit-level, deterministic seed) at every floor. Assert and print.
Decomposition closes: the residual of the additive identity in §2.4 must be < MC noise (report max |residual| in log10 units).
Sign sanity: exposure-CF component should be negative (RQ falls as floor rises, EED drops); composition component sign is the empirical question of interest — do not constrain it.
3. Implementation
3.1 Harness (reuse M1.2's m12_run_one_floor; add an arm argument)
## Single source of truth for the per-floor pipeline. Extends the M1.2 harness.
## Reuses, unchanged: correct_and_bootstrap_eed(), run_pssd_pipeline(),
## matrix_function(), mc2d_risk(). Seeds set locally inside each call for reproducibility.
m2_run_one <- function(compartment, erm, arm, L_tar_min,
                       tox_fixed, TOX_FIXED_FLOOR = 10, seed = 20260601) {
  set.seed(seed)

  ## --- EXPOSURE: CF floor always tracks the treatment floor (all arms) ---
  eed <- correct_and_bootstrap_eed(compartment = compartment,
                                   L_tar_min = L_tar_min,   # <-- moves in every arm
                                   L_tar_max = 5000)

  ## --- HAZARD: composition ALWAYS frozen (tox_fixed, ≥10 µm) ---
  ## Arm A: integration bounds PINNED at the freeze floor (HC5 constant).
  ## Arm B: integration bounds TRACK the treatment floor.
  x_haz <- if (arm == "A") TOX_FIXED_FLOOR else L_tar_min

  pssd <- run_pssd_pipeline(tox = tox_fixed, erm = erm,
                            dose_unit = "L", sim = 30, cv_uf = 0.5,
                            rmore_method = "lognormal",
                            x1D_set = x_haz)
  mat  <- matrix_function(x1M_set = x_haz, x2D_set = 5000,
                          upper.tissue.truncation.limit = 500)

  hc5  <- pssd$hc5          # constant across floors in Arm A by construction
  rq   <- mc2d_risk(eed = eed, hc5_dist = pssd$hc5_dist)  # returns 1D + MC2D summaries

  list(compartment = compartment, erm = erm, arm = arm,
       L_tar_min = L_tar_min, hc5 = hc5,
       eed_q50 = eed$q50, rq_1d = rq$rq_1d, rq_mc2d_med = rq$rq_med,
       n_species = pssd$n_species)
}
3.2 Grid runner
m2_grid <- expand.grid(
  compartment = c("river", "ocean"),
  erm         = c("food_dilution", "tissue_translocation"),
  arm         = c("A", "B"),
  L_tar_min   = c(1, 2.5, 5, 7.5, 10),
  stringsAsFactors = FALSE
)
tox_fixed <- filter_tox_by_size_floor(tox, 10)   # computed ONCE, §2.1
m2_results <- purrr::pmap_dfr(m2_grid, function(compartment, erm, arm, L_tar_min)
  as.data.frame(m2_run_one(compartment, erm, arm, L_tar_min, tox_fixed = tox_fixed)))
3.3 M1.2 linkage
Do not re-run M1.2. Read the already-rendered M1.2 per-floor RQ/HC5/EED table from v11932 into a small inline data frame m12_ref (values transcribed with a comment citing the M1.2 code chunk), and join on (compartment, erm, L_tar_min) to compute ΔlogRQ_composition and ΔlogRQ_total. Add 7.5 µm to M1.2's grid only if trivially available; otherwise interpolate for display and flag it as interpolated.

4. Outputs (all clearly labeled "DECOMPOSITION DIAGNOSTIC — NOT A RISK ESTIMATE")
Table M2-1 — Arm A/B/M1.2 RQ by floor (river & ocean, both ERMs): columns = floor, Arm A RQ (1D + MC2D med), Arm B RQ, M1.2 RQ, HC5 per arm, EED q50, n_species. Bold the acceptance-check row at 10 µm where all three coincide.
Table M2-2 — Three-way decomposition: for each (compartment, ERM, floor), the three ΔlogRQ components and the closure residual; plus a summary line giving the fold-contribution of each component to the total 1→10 µm collapse.
Figure M2-1 — stacked decomposition bars: x = floor, y = ΔlogRQ vs the 10 µm reference, three stacked contributions (exposure-CF, hazard-integration, composition), one panel per compartment×ERM. Annotate the M1.2 total as an overlaid point/line. Export download-only.
Figure M2-2 — Arm A RQ vs floor with HC5 flat-line overlay: visually demonstrates that when composition AND hazard integration are frozen, the residual exposure-only trend is small (the pre-registered expectation for explanation B).
4.1 Pre-registered expected outcome (frame as hypothesis, report what actually renders)
If the collapse is composition/mechanism-misassignment-driven (B/E), then Arm A — fixed ≥10 µm tox, so HC5 ≈ 1215 (river FD) held constant — should show only a modest RQ decline as the exposure floor rises (pure CF de-extrapolation), and even at a 1 µm exposure floor the river FD RQ should be < 1 (rough pre-registered estimate ≈ 0.46, EED-q50-dependent). That would demonstrate the M1.2 exceedance at a 1 µm floor was composition-driven (sensitive small-particle records entering the SSD and depressing HC5), not CF-driven. Conversely, if Arm A RQ stays ≫ 1 across floors, exposure-CF de-extrapolation is the dominant driver and explanation (C) gains weight. Report the rendered numbers regardless of which way they fall; do not tune to the expectation.

5. Guardrails (hard)
No live production numbers change. M2 adds a new diagnostic section; the production headline (v11794/v11932 1 µm-floor results and the MC2D primary) is untouched. Do not let M2 outputs leak into any headline table, abstract, or risk conclusion.
Diagnostics, not risk estimates. Arm A deliberately size-mismatches exposure and hazard (exposure floor ≠ hazard floor). This is intentional for attribution and makes Arm A an invalid risk quotient. Label every Arm A/B number accordingly.
TT ≥10 µm caveat (from §2.1) printed adjacent to every Tissue Translocation result.
Ignore sediment entirely.
Single source of truth: tox_fixed computed once (§2.1); m2_run_one is the only per-floor pipeline; M1.2 values transcribed once into m12_ref with a citing comment.
Seeds local inside each m2_run_one call; deterministic Arm A HC5 asserted (§2.5 check 2).
Methods provenance: C-PSD / correction-factor conventions per Segur et al. 2026 and the SPEC 1b differential-slope fix (a_psd = a_cpsd − 1) are inherited unchanged from production; do not re-implement. Power-law fitting per Clauset, Shalizi & Newman 2009 conventions as already used.
If the ocean FD SSD fails to converge at any floor with tox_fixed (it should not, since composition is frozen at the 10 µm-converging set), report the failure rather than silently dropping the cell.
6. Keep / relegate / delete
Keep (new): §3 harness m2_run_one + grid; Tables M2-1/M2-2; Figures M2-1/M2-2; the m12_ref transcription block.
Relegate: full per-arm MC2D distributional summaries → appendix/supplement; body carries only the decomposition tables/figures and a 2–3 sentence interpretation.
Delete: nothing. No existing production chunk is modified.
7. Interpretation stub for the render (fill with actual rendered values)
The M1.2 Food Dilution RQ collapse of ~[TOTAL]× over a 1→10 µm exposure floor decomposes into exposure-CF de-extrapolation ([X]×), hazard-integration-range shift ([Y]×), and study-filtration/taxonomic composition ([Z]×). [If B/E confirmed:] With taxonomic composition frozen at ≥10 µm, raising only the exposure floor changes the RQ by just [X]×, and the river Food Dilution RQ remains <1 even at a 1 µm exposure floor — indicating the 1 µm-floor exceedance in the production run is driven by sensitive small-particle records entering the food-dilution SSD, not by correction-factor extrapolation. This is mechanistically consistent with the expectation that food dilution is a large-particle (volume/gut-filling) mechanism while small particles preferentially drive translocation, so small-particle records aligned to the volume-based food-dilution ERM may mis-assign translocation/oxidative-stress effects. [Report the actual direction if it differs.]

Changelog
v1 (M2): New spec. Decomposition experiment separating the M1.2 L_tar_min floor effect into exposure-CF, hazard-integration, and study-filtration/composition components via a fixed-composition (≥10 µm) tox set and two arms (A = pinned hazard, pure CF; B = tracking hazard integration). Adds 7.5 µm treatment. Diagnostic-only; production unchanged; sediment excluded; TT small-particle-removal caveat carried throughout.