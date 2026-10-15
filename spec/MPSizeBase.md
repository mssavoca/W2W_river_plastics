IDE Agent Spec: Spectra-QC Audit, Anchor/Shape Sensitivities, MPsizeBase Comparison
Project: Savoca et al. W2W river-plastics manuscript and probabilistic_risk_characterization.qmd pipeline. Files: probabilistic_risk_characterization.qmd, R/mp_risk_utils.R, data_input/Part_dets_comb.rds, data_input/Part_dets_summ_final.rds, docs/Manuscript.qmd, MPsizeBase-v19-5-2026.xlsx. Order of work: Part A, then Gate G0 (user sign-off), then Parts B and C, then Part D.

0. Standing rules
Do not change any production number until Gate G0 is signed off by the user.
Put new analyses in child files (R/anchor_shape_sensitivity.qmd, R/mpsizebase_comparison.qmd), following the rivm_benchmark.qmd pattern.
Production stays as is until G0: LOD-anchored sample concentrations, river-specific slopes, single-slope headline.
Decision criteria are pre-registered below. Do not choose a filter or window because it gives better-looking numbers.
Every manuscript or SI number is written from data_output/psd_rescaling_params.json. Nothing is hardcoded.
If any production number changes, regenerate the C1 snapshot baseline deliberately and record the reason in docs/CHANGELOG_hoffman_revisions.md.
1. Verified context from the review (read before starting)
Anchor in the current pipeline.

The river chunk passes monitoring through lod_scope_monitoring_concentrations(group_col = "river", cpsd_fit_by_group = cpsd_fits_by_loc).
The sample concentration is the reported concentration multiplied by the in-LOD particle fraction. The CF window is that river's own LOD window.
This is sample-specific observed counts anchored in the LOD window with a river-specific slope.
Segur et al. describe the same idea: further use of generic PSD slopes to extrapolate MP mass concentrations is best done by first calculating the intercept for each datapoint, then applying both the generic slope a and the datapoint-specific b. Their statement is about mass, so the extension to number concentrations is an inference.
Where the stale CF numbers come from (inference from the qmd and manuscript).

The SI's 22,465 is the LOD-window CF (165–385 µm → 1–5000 µm) at a ≈ −2.92.
The main text's ≈1,850 is the nominal 50–500 µm CF at the same slope.
Both come from the same slope with different anchors.
Check that the main-text 1,850 and "≈600 L⁻¹" are regenerated from the final pipeline. The nominal 50–500 µm diagnostic chunk (lmeas-sensitivity) is the likely source.
Particle accounting.

The qmd comment gives 1,207 river particle-level rows, whereas the monitoring table sums to 3,777 counted particles.
lod_particle_fraction is computed on the particle-level rows and multiplied into a concentration built from the summary table's counts.
It is not yet known what the other ~2,570 counted particles are. The user has to check this, and the audit below must resolve it.
Segur's shape handling.

Segur et al. fit fragments and fibers separately.
Their text does not say the two shapes are summed into a total. The excerpts show separate fitting and extrapolation for fragments and fibers, but do not say that the two shapes are summed into a total concentration per sample or study.
PART A: bad_spectra and particle-accounting investigation
Why
The raw-particle filter in the qmd uses bad_spectra as a keep-condition. The name suggests TRUE means bad, but it may be a quality-pass flag. Its polarity has not been verified.
Small particles often have poorer spectra. If the filter preferentially drops them, the retained particles are biased toward larger sizes. That would move the fitted slopes and the in-LOD fraction (inference, untested).
The same filter feeds the ocean and sand particle sets. Ocean has about 117 particles, so any change matters more there.
A1. Establish what bad_spectra means
Find its definition in the upstream scripts that produce Part_dets_comb.rds (including Util_MSS.R) and in any data dictionary.
Report class, values and NA count.
Cross-tabulate against acc_analy_conf, material_class, material_simple, sample_type, sample_or_blank, and any match-quality fields.
Decide the polarity from evidence. If TRUE rows carry polymer IDs and high match scores, TRUE means pass. If TRUE rows have missing IDs or low scores, TRUE means bad.
Record the finding in a code comment and in the Methods.
A2. Particle-accounting waterfall
Build a waterfall for river, ocean and sand separately. Stages:

All rows.
Samples only (blanks removed).
By sample_type.
Plastic only (material_class not mineral or organic matter).
acc_analy_conf == "confident".
The bad_spectra condition.
Rows with a valid length.
Rows inside 50–500 µm.
Rows inside the LOD window.
Reconcile per sample against Part_dets_summ_final's count and extrap_count:

Find in the upstream code which criteria produced count and extrap_count.
Test these hypotheses and record which holds:
H1: the summary counts all plastic IDs regardless of confidence.
H2: the bad_spectra polarity is inverted.
H3: only some particles have size or morphology measurements.
H4: count is the number analysed and extrap_count is the aliquot extrapolation.
H5: the sub("_[0-9]+$", "", ...) ID merge collapses distinct samples. Only SRR20240205SS_2 should be merged.
Output data_output/particle_qc_waterfall.csv and data_output/particle_qc_reconciliation_by_sample.csv. Render both as tables.
A3. Test size bias of the filter
At each stage, compare retained and dropped particles on:

length median and quartiles, with a KS test;
the share inside 50–500 µm;
the share inside each river's LOD window;
the fragment/fiber ratio;
polymer mix.
Report effect sizes, not only p-values. Run the same analysis for ocean and sand.

A4. Filter-variant sensitivity
Re-run the production pipeline under each variant:

F0: current filter.
F1: bad_spectra polarity flipped (run only if A1 is ambiguous).
F2: no bad_spectra filter.
F3: no acc_analy_conf filter.
F4: both removed (all plastic-classified rows).
For each variant, report:

pooled, per-river, fragment and fiber a_psd ± SE, LOD window, n particles and R²;
the observed lod_particle_fraction per river;
CF medians;
W1 EED q50 and q95;
the headline RQ.
Add a hash of the filter specification to raw_data_cache_key. The current key tracks only the file mtime, so cached fits would otherwise survive a filter edit.

A5. Gate G0: stop and report before editing production
Present the polarity finding, the waterfall, the A3 bias results and the A4 table to the user. Pre-registered decision rules:

If the filter in use has the wrong polarity, fix it. The user approves the resulting headline changes.
If the particle table and the summary count use different inclusion criteria, put concentration and fraction on one basis. Options to propose to the user:
(a) Rebuild the sample concentration from the particle rows that meet the criteria, using the aliquot factor extrap_count/count.
(b) Keep the reported concentration and document the representativeness assumption, with the A3 evidence.
Flag prominently any change greater than 0.1 in a slope, or greater than 10% in the W1 median.
A6. Code changes after approval
Replace the bare bad_spectra filter with a named predicate (for example spectra_qc_pass) shared by river, ocean and sand.
Add stopifnot polarity checks that fail loudly. For example, retained rows must have a non-missing polymer ID.
Add a reconciliation assertion against the summary count, or a documented tolerance.
A7. Methods and SI updates
Add a "Spectral QC and particle accounting" subsection to docs/Manuscript.qmd stating:

the exact QC criteria and what bad_spectra encodes;
waterfall counts (inline R values from the exported JSON);
how the 3,777 counted particles relate to the particles with size data;
the representativeness test and its outcome;
which concentration basis feeds the in-window fraction.
Add the waterfall and reconciliation tables to the SI. Add the new keys to psd_rescaling_params.json.

PART B: Supplement sensitivities (after G0)
S-A. Dual-anchor
Run correct_and_bootstrap_eed() twice with identical alpha_by_river draws.

A (LOD-anchored, production): the current monitoring table.
B (sieve-anchored): Lmin_measured_um = 50, Lmax_measured_um = 500, and C_measured_pL = C_reported_pL.
Output per river × depth: W1 median and IQR for A and B, and the ratio A/B. Add a diagnostic: predicted fraction = (L₂^(a+1) − L₁^(a+1)) / (500^(a+1) − 50^(a+1)), using the river slope and LOD edges, versus the observed lod_particle_fraction. Report n particles in the LOD window per sample, and flag samples with fewer than 10.

S-B. Variance-reduced anchor
Use the river-level pooled fraction (Σ in-window particles ÷ Σ in-range particles) with the sample-specific reported concentration. Report the effect on EED spread and on the number of zero-concentration samples.

S-C. Shape-stratified rescaling (sensitivity only)
Add raw_particles_denominator to lod_scope_monitoring_concentrations() (or write a shape variant) so that each shape's fraction is n_shape,LOD ÷ n_all,in-range. The shapes must sum to the all-shape total.
Use shape-specific slopes and LODs, preferably pooled across rivers (cpsd_fits_river_length$fragment and $fiber).
Use per-river × shape fits only where valid is TRUE and the ≥3 bins / R² ≥ 0.90 rule passes. Report the failures.
Draw slopes independently per shape with slope_structural_sd. Also run the fully correlated case.
Sum shapes per sample before bootstrap_eed().
Output EED q50 and q95 against production, and the share of the 1–5000 µm total that lies below 10 µm.
RQ is optional. If shown, label it as a documented ALIGNMENT RULE exception with the pooled hazard. Do not re-derive hazards.
Illustration of why this is a sensitivity bracket and not a competing estimate (arithmetic with the manuscript slopes and a common 165–385 µm window; actual shape LODs will differ): a pooled CF of about 22,500, a fiber CF of about 6,700 and a fragment CF of about 1,310,000.
Segur's own caution applies to the sub-10 µm region: they assume the power law holds over the full range, but note that log-normal behavior may need revisiting as higher quality 1–20 µm data become available.
S-D. Reporting
One Supplement table stating, for every CF and concentration, the anchor, slope source and window. Retire the unlabeled 22,465 / 1,850 / 2,403 triple. Compare pooled single-slope against two-segment on the same anchor.

PART C: Other fixes
Appendix M ratio. Change ratio_W1_river_over_two to C_W1_pooled / C_W1_two. It currently divides a river-specific-slope W1 by a pooled two-segment W1.
Slope SE realism.
Add a particle-level bootstrap per river: resample particles, refit the full LOD algorithm, 1,000 replicates, gated on the production budget.
Report bootstrap SEs next to the OLS SEs.
Recompute Cochran's Q and I² with the bootstrap SEs. OLS SEs on cumulative counts are probably too small (inference).
Stale text. Replace the hard-coded "~100–330 µm" fragment window in Section 4.2.1.2 with inline LOD values.
Section 10.2. Label the lmeas-sensitivity output as a nominal diagnostic, not the production CF.
Manuscript values. Map every manuscript CF and concentration to a key in psd_rescaling_params.json, and list any that disagree with the render.
PART D: MPsizeBase comparison (after Parts A–C are closed)
D0. Source and integrity
Use the Zenodo dataset MPsizeBase: a database for particle size distributed environmental microplastic data. The record reads version v5, published June 24, 2026, DOI 10.5281/zenodo.20832480, CC BY 4.0.
Its file list gives MPsizeBase v19-5-2026.xlsx, 3.2 MB, MD5 bd31cb5863cf1022f3717d219e2e6865.
The uploaded copy (MPsizeBase-v19-5-2026.xlsx, 3,156,745 bytes) has the same MD5. The IDE agent must assert this checksum before use.
The record page does not show version history. Earlier Zenodo record numbers (17380284 / 17380283) cannot be linked to this record from the page content. Cite DOI 10.5281/zenodo.20832480 and the checksum in the Methods.
D1. Reproduce the prepared windows tables
Regenerate mpsizebase_ocean_psd_windows_v1.csv and mpsizebase_ocean_sample_windows_v1.csv with mpsizebase_ocean_windows_v1.py from the checked workbook. Acceptance checks (my own sandbox results, to be reproduced and not assumed):

99 ocean PSDs, with 7 Raman, 2 microscopy and 90 FTIR.
57 PSDs from 11 studies with an observed 165–385 µm window.
26 PSDs whose fitted LOD fully covers 165–385 µm. In this subset, fragments have a median a_psd of about −2.94 (n=18) and fibers about −1.34 (n=8).
40 samples with an observed sample-level 165–385 µm sum, with a median of about 61.9 MP/m³ (25 coastal, 15 offshore; 20 pump, 10 CTD, 8 IS pump, 2 tow).
Assert that cumsum_powerlaw_alpha == cumsum_slope − 1, and that the extrapolated column equals 10^b(1 − 5000^a′) to within 1e-6.
The workbook's unit label is "MP/m3" for all rows, including deposition rows reported per m² per day. Atmosphere rows are excluded here; assert units for the ocean rows used.
The sheet has no river, freshwater, sediment or beach rows. MPsizeBase can only be an ocean-water and method comparator. Rivers keep the RIVM and Kooi benchmarks.
D2. Corrections to existing qmd Section 12.1.1 (verify first)
literature_ocean_df assigns Shape = "all" to every literature PSD, so fragment and fiber PSDs are pooled in one box. Split them. Segur fit fragments and fibers separately, so compare shape to shape.
lod_low_um and lod_high_um in literature_psd_alpha are the min and max of the reported bins, not the fitted LOD window. Use the filtered-bin bounds, or re-derive as in mpsizebase_ocean_windows_v1.py.
The qmd reads MPsizeBase_v15_05_2026.xlsx. Switch to v19-5-2026 and record the DOI and checksum.
Segur averaged per study (slopes by mean, intercept by median, extrapolated concentrations by median) so that every study has the same weight. Report study-level medians alongside PSD-level ones.
D3. Pre-registered inclusion filters (report counts at each stage)
Ocean only.
Detection FTIR or Raman. Microscopy-only is a flagged sensitivity.
PSD_MP_count ≥ 50.
Slope comparison only where the PSD's LOD window overlaps your fit window.
Concentration comparison uses the 165–385 µm window.
Stratify by surface vs subsurface, coastal vs offshore, and pump/CTD vs tow. Your ocean samples have no recorded depth, so compare against both depth classes and say so.
Nearest sampling analogue: pump and CTD. Nearest location analogue: coastal.
D4. Tier 1: slopes
Compare pooled, fragment and fiber ocean slopes (river slopes as context only) with MPsizeBase on the a_psd convention, split by shape.
Use medians, IQR and bootstrap CIs. Mann–Whitney or KS tests are descriptive only.
Always state each side's fit window. The cleanest subset is the 26 PSDs whose LOD covers 165–385 µm.
D5. Tier 2: concentrations in the fully detected window (primary like-for-like)
Compute your ocean concentrations in 165–385 µm directly from the particle-level rows: reported concentration × in-window fraction with exact edges. Convert L⁻¹ × 1000 → m⁻³.
Compare with the MPsizeBase observed sample-level sums (direct_165_385).
Show ECDF and boxplots on a log axis, with per-group n.
Separate shapes only where both sides are separated.
D6. Tier 3: rescaled to 1–5000 µm (secondary, flagged)
For each MPsizeBase PSD, compute:

(a) the published fitted-line value (MPsizeBase_1_5000_mean);
(b) LOD-anchored: the observed LOD-window concentration × CF with the PSD's own slope;
(c) a reported-range anchor as a contrast.
For your ocean data use production (LOD-anchored) and the S-C shape arm.

The per-sample sum of fragment and fiber extrapolations is our aggregation, not a quantity the paper reports.
For every value, report the share of the 1–5000 µm count lying below 10 µm. For a pooled slope of −2.92 my arithmetic gives about 99%, and about 99.8% for a fragment slope of −3.74. Agreement at 1–5000 µm is therefore not independent confirmation; it is dominated by extrapolation.
D7. Tier 4 (optional, qualitative)
Compare your visually extracted 500–5000 µm fraction (n=237 suspected particles) with the 11 MPsizeBase PSDs that cover 500–5000 µm (mostly tows). Treat as qualitative only.

D8. Outputs
Figure 1: slopes by shape, with window labels.
Figure 2: the 165–385 µm ECDF/boxplot.
Figure 3: forest of 1–5000 µm values by anchor.
A Supplement inclusion table listing every PSD and sample with its exclusion reason.
JSON keys for every quoted number.
Text placeholders only.
D9. Interpretation guardrails (write into Methods and Discussion)
No California or river rows exist in the sheet (my own check; the sheet's ocean data are mostly coastal Korea, Mediterranean, Atlantic and Arctic). This is context, not validation.
The 165–385 µm window is the main comparison. Differences in sampling, mesh and detection limits remain large.
Record that the PSD counts differ between versions of the paper and the database. The paper describes 81 binned MP PSDs for fragments and fibers from 44 studies, whereas the v19-5-2026 sheet has more PSDs.
Acceptance checks (all parts)
bad_spectra polarity is documented with evidence.
G0 was reported to the user before any production edit.
The MPsizeBase workbook MD5 assertion passes, and the D1 acceptance numbers are reproduced.
The anchor is stated for every CF and concentration in the manuscript and SI.
The Table 1 guard still passes, and C1 snapshot differences are explained in the changelog.
Cache keys include the filter hash.
Return to the user
Send one summary with:

the bad_spectra polarity finding;
the waterfall and size-bias results;
the filter-variant table;
the S-A/B/C headline contrasts;
the MPsizeBase tier results;
a list of manuscript numbers that changed.
References
9 references across 4 sources


All sources
Filter references...
1
further use of generic PSD slopes to extrapolate MP mass concentrations is best done by first calculating the intercept for each datapoint, then applying both the generic slope a and the datapoint-specific b


Using the power law size distribution to extrapolate and ...

link.springer.com

2
fragments and fibers separately


Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media

archimer.ifremer.fr

3
The excerpts show separate fitting and extrapolation for fragments and fibers, but do not say that the two shapes are summed into a total concentration per sample or study


Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media | Microplastics and Nanoplastics | Springer Nature Link

link.springer.com

4
they assume the power law holds over the full range, but note that log-normal behavior may need revisiting as higher quality 1–20 µm data become available


Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media | Microplastics and Nanoplastics | Springer Nature Link

link.springer.com

5
version v5, published June 24, 2026, DOI 10.5281/zenodo.20832480, CC BY 4.0


MPsizeBase: a database for particle size distributed environmental microplastic data | Zenodo

zenodo.org

6
MPsizeBase v19-5-2026.xlsx, 3.2 MB, MD5 bd31cb5863cf1022f3717d219e2e6865


MPsizeBase: a database for particle size distributed environmental microplastic data | Zenodo

zenodo.org

7
fragments and fibers separately


Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media

archimer.ifremer.fr

8
per study (slopes by mean, intercept by median, extrapolated concentrations by median) so that every study has the same weight


Using the power law size distribution to extrapolate and ...

link.springer.com

9
81 binned MP PSDs for fragments and fibers from 44 studies


Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media | Microplastics and Nanoplastics | Springer Nature Link

link.springer.com