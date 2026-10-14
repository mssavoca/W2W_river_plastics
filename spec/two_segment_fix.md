IDE AGENT INSTRUCTION SHEET
Update the PSD-rescaling pipeline and the manuscript (Savoca et al., W2W river plastics, Monterey Bay)
Audience: coding/writing agent operating in the project repo (R/Quarto pipeline plus the manuscript .docx/.qmd).Owner/decision-maker: Scott Coffin (OEHHA). Do not make scientific judgement calls beyond what is written here. Raise anything ambiguous as a TODO-AUTHOR item (Section 7) instead of guessing.

0. Decision record (what has been decided and why)
Decision D1 - Primary W1 (1-5000 µm) estimate = pooled SINGLE-slope C-PSD model.Decision D2 - The TWO-SEGMENT (piecewise) model is reported in the main-text Results, in full (breakpoint, both slopes, ΔAIC, CF, concentration), as a structural-uncertainty alternative. It is not a footnote.Decision D3 - The abstract, conclusions and synopsis present the W1 number as a range spanning both model forms, not as a single number with a slope-only CI.Decision D4 - W2 (50-500 µm, the sieve window) remains the primary quantity for all cross-study comparisons.
Rationale to carry into the manuscript (each point is sourced; see the literature table in Section 5):
1.	Comparability. The single-power-law correction factor is the established size-alignment method. It scales the integral of the power-law for the measured size range against the integral for the default range. 1 The newer C-PSD extension extrapolates observed MP number and mass concentrations to the full MP size range (1 to 5000 μm), or any other sub-size range. 2 The Dutch/RIVM benchmark uses the same single-power-law CF form. The SI states: this is identical to the correction factor above with a = −α. 3
2.	No validation of either model below 50 µm. The draft states the primary slope was estimated over the empirically supported 165-385 µm C-PSD window and extrapolated beyond that range, particularly toward smaller particle sizes. 4 The piecewise model was fitted to the same C-PSD window selected for the primary single-slope model. 3 The two-segment model therefore extrapolates its fine-segment slope from within a window only about 2.3x wide in size down to 1 µm.
3.	The literature shows PSDs can depart from a single power law, and the direction of departure at small sizes is not settled.
◦	Fibre PSDs "show curvature and underestimation of small MP" 2, and underestimated small bins "tend to increase the slope (i.e. less steep slope)." 2
◦	A Lake Superior study found power-law modelling significantly underpredicts microplastic counts for smaller-sized particles (5-45 μm). 5
◦	A threshold-fragmentation model describes two rather different size distributions for microplastics (small MP <150 μm, large MP between 1 and 5 mm). 6
◦	A fracture model yields a log-normal-like shape toward the small-size end and a power-law shape toward the large-size end. 7
◦	Together these mean the two-segment model is not automatically a "conservative" or "more correct" alternative. It is a different structural assumption.
4.	The slope-only CI understates uncertainty. The draft already concedes that the CI does not incorporate structural uncertainty associated with extrapolating a single power law beyond the empirically supported fitting window. 3 Current values differ about 6-fold between models: 7.4 × 10⁵ under our single-slope model and 1.2 × 10⁵ under the two-segment model 4 (particles m⁻³).
Novelty language: In my literature search (OpenAlex returned zero hits for several queries; Europe PMC returned one off-target record), no prior application of a piecewise-slope CF within the size-alignment framework was found. This is "not found", not "does not exist". Do NOT write "novel" or "first" in the manuscript. If a statement is wanted, use "to our knowledge" and flag it with TODO-AUTHOR-4 (a human literature check is needed first).

1. Ground rules for the agent
•	R1. No hand-typed numbers. Every number in the manuscript or SI that comes from the analysis must be generated from pipeline output (Section 2.2 export file) via inline code or a template. Values quoted in this sheet are current draft values for orientation and unit tests only; they will change after task P1.
•	R2. No invented references. Only cite works listed in Section 5, and only after resolving each in the reference manager (Zotero) and confirming title/authors/year/DOI against the publisher page. The author lists for several sources were NOT visible in the material this sheet was built from, so do not fill them from memory. Refs 51-54 in the draft were added as plain text and must be re-inserted via Zotero (see the draft's own comment: Refs 51-54 were added as plain text outside the Zotero bibliography field, and their in-text citations are plain superscripts. Insert them via Zotero; they are first cited in Methods, so ACS order-of-appearance numbering will shift. 4).
•	R3. Do not change the primary model selection logic (LOD-window procedure, R² ≥ 0.90, ≥3 bins, longest qualifying window). The SI documents it: all contiguous windows of at least three bins are fitted by OLS on log10 cumulative abundance vs log10 length, windows with R² < 0.90 are excluded, and the longest qualifying window is selected, with ties resolved using R². 3
•	R4. One slope convention in the manuscript body: the differential slope a (dN/dL = kL^a; a = a′ − 1). Cumulative a′ appears only in the SI where the conversion is documented. This is the convention used by the MPsizeBase workflow, which says to convert the C-PSD fitted slope a′ and intercept b′ to regular PSD slope a and intercept b as follows: a = a′ − 1 and b = −b′a′. 8 Keep the sign convention explicit: a is negative; RIVM α is positive and a = −α.
•	R5. Never silently overwrite. Commit pipeline changes separately from manuscript changes; each commit message references the task ID (P1, P2, M1, ...).
•	R6. If a required input is missing (e.g., storage metadata), insert a visible [TODO-AUTHOR-n] tag. Do not fabricate.

2. PIPELINE TASKS
Repo anchors known from author comments (confirm paths by searching the repo): data_input/Part_dets_summ.rds (per-sample table; column extrap_conc_PPL summed over plastic classes) and R/rivm_benchmark.qmd (regenerates all benchmark numbers).
P1 - BLOCKING: reconcile the sample set and QC filter
Why: An author comment records that the pipeline's per-sample table has 60 samples ... median 0.40 L-1, mean 1.01 L-1 4, which do not match Table 1 or the 0.33 L-1 / 3,777-particle / 57-sample figures in this draft. 4 The draft text reports an average concentration in the 50-500 µm fraction of 0.33 microplastics L⁻¹ 4 and 3,777 microplastics in the 50-500 µm fraction ... in a total of 11,400 L of river water. 4 Every extrapolated number scales linearly with this concentration, so it matters more than the PSD model choice.Do:
1.	Produce a single canonical sample manifest (CSV) with columns: sample_id, river, date, season, depth, volume_L, n_particles_50_500, conc_50_500_L, QC_pass (bool), QC_reason.
2.	Document exactly which filter yields 57 vs 60 samples and which gives mean 0.33 vs 1.01 L⁻¹ (suspect candidates: blank/contamination subtraction, QC exclusions, extrapolation of subsampled FTIR area, inclusion of non-plastic classes). Report findings; do not choose silently.
3.	Add assertions that fail the build if the manifest and the Table 1 inputs disagree.
4.	Output both mean and median (with IQR) of conc_50_500_L on the canonical set.Stop condition: If the discrepancy cannot be resolved from the repo, stop and raise TODO-AUTHOR-1. Do not proceed to regenerate manuscript numbers on an unreconciled set.
P2 - Parameter export (single source of truth)
Create outputs/psd_rescaling_params.json (plus a flat CSV) written by the pipeline and read by the manuscript. Required keys:
•	window_LOD_um (currently 165-385), n_bins_window, R2_window
•	Single slope: a_cum, a_cum_SE, a_diff (= a_cum − 1), a_diff_SE, CI95_a_diff, CF_single, CF_single_CI95, C_W2_mean, C_W2_median, C_W2_IQR, C_W1_single_mean, C_W1_single_median, C_W1_single_IQR, C_W1_single_slopeCI95
•	Two-segment: breakpoint_um, a_fine_cum, a_fine_cum_SE, a_coarse_cum, a_coarse_cum_SE, a_fine_diff, a_coarse_diff, dAIC, CF_two, C_W1_two_mean, C_W1_two_median, C_W1_two_IQR
•	Cross-model: ratio_W1_single_over_twoRounding: store full precision; round only at render time. Render CFs to 3 significant figures in the manuscript and SI. This avoids the existing inconsistency in the draft: the SI reports factors of 1,800 for the pooled slope and 300 for the pooled two-segment model 3 while the main text says a correction factor of approximately 1,850. 4
P3 - Unit tests for the CF functions (add to test suite)
Use testthat (or the repo's framework). Tests must pass before any manuscript regeneration.
1.	Literature benchmark. With the cumulative-exponent convention CF = (UL_D^(1+a) − LL_D^(1+a)) / (UL_M^(1+a) − LL_M^(1+a)), a = −1.6, measured 30-2000 µm, target 1-5000 µm: CF = 8.32 (±0.01). Source: with these boundary values and α = 1.6, the CF would be 8.32 1, so a measured 100 #/L gives 832 #/L. 1 Note the source uses a positive α with x^−α; our a is −α.
2.	Internal benchmark. a = −2.92, measured 50-500, target 1-5000: CF ≈ 1,850 (1,850.4).
3.	Piecewise reduction. If the fine and coarse differential slopes are equal, CF_two must equal CF_single to numerical tolerance (1e-9 relative).
4.	Continuity. The two-segment differential PSD must be continuous at the breakpoint. The SI states the coarse-segment scaling coefficient was adjusted to maintain continuity of the differential PSD at the fitted breakpoint. 3
5.	Interval spanning the breakpoint. Intervals spanning the breakpoint are divided into fine and coarse portions, integrated separately, and summed. 3 Test an interval fully fine, fully coarse and spanning.
6.	Singularity. a = −1 (the log case, a+1 = 0) must be handled explicitly, or the function must error clearly.
7.	Convention. a_diff = a_cum − 1 (e.g., −1.920 → −2.920), and RIVM α = 2.32 maps to a = −2.32.
P4 - Make the model-comparison statistic defensible (robustness of ΔAIC)
Why (general-knowledge statistical reasoning, not a literature finding): OLS on cumulative bins violates the independence assumption behind AIC, because cumulative counts are monotone and serially dependent. Also, the breakpoint is selected by minimising AIC over candidates, which adds an unpenalised parameter. The reported ΔAIC = −44.0 is therefore likely optimistic. This is an inference about the method, so do not cite a source for it unless one is found (TODO-AUTHOR-5).Do (in this order of priority; stop at the first that is feasible and report which):
1.	Particle-level maximum-likelihood alternative. Fit single vs broken power law by MLE on particle-level lengths within the same window (truncated to the window bounds), compare via AIC/BIC and a parametric-bootstrap likelihood-ratio test. This avoids binning and cumulative-dependence problems. Note that for the RIVM benchmark, RIVM's power-law exponents are poweRlaw maximum-likelihood fits above an estimated x_min 3, so an MLE approach is also consistent with that comparator.
2.	Bootstrap of the C-PSD two-segment fit. Resample samples (not bins, not particles) with replacement, e.g. B = 2,000; for each replicate redo breakpoint search, fine/coarse slopes, ΔAIC and CF_two. Report the breakpoint distribution (median, 2.5-97.5%), the slope CIs, and the CF_two CI.
3.	ΔAIC with a breakpoint-search penalty. At minimum, report ΔAIC under a permutation/parametric null for the breakpoint search.Outputs: add dAIC_boot_CI, breakpoint_boot_CI, CF_two_boot_CI, LRT_p_boot to the JSON export. If the bootstrap CI of ΔAIC includes 0 or the breakpoint is unstable, say so; it changes the manuscript wording (M3).
P5 - Out-of-window consistency check (500-5000 µm)
Why: The draft counted an additional 237 putative microplastics visually identified in the 500-5000 µm size fraction 4 alongside 3,777 µFTIR-identified particles in 50-500 µm. This is an independent check on the coarse side that neither model was fitted to.Reference arithmetic (my back-of-envelope; the agent must regenerate it properly): observed ratio N(500-5000)/N(50-500) ≈ 237/3,777 = 0.063. A single slope a = −2.92 predicts ≈ 0.012. A single slope giving CF ≈ 300 (a ≈ −2.45, my back-calculation) predicts ≈ 0.035.Do:
1.	Compute the predicted ratio under (i) the pooled single slope, (ii) the two-segment model (using its actual coarse segment), (iii) the river-specific slopes. Provide bootstrap CIs (resampling samples).
2.	Compare with the observed ratio per river and pooled, with a binomial CI on the observed ratio.
3.	Run sensitivity sets: all 237 visual IDs vs only those confirmed spectroscopically (if any), and volume-matched samples only.
4.	Report as a "consistency check" (not a validation), with these stated caveats: the 237 particles are putative visual IDs without spectroscopic confirmation; this check informs the coarse side and says little about the 1-50 µm fine side, which is where most of the extrapolated particles reside.Stop condition: If the 500-5000 µm counts are not volume-matched to the 50-500 µm counts, raise TODO-AUTHOR-2.
P6 - Structural-scenario table for W1 (replaces slope-only CI as the headline uncertainty)
Compute W1 (1-5000 µm) under these sceS1 (primary)	River-specific single slopes	Heterogeneity (draft: river-specific C-PSD slopes ranged from -1.493 ± 0.068 in the Salinas River and -1.503 ± 0.074 in the Carmel River to -1.975 ± 0.050 in the San Lorenzo River and -2.711 ± 0.063 in the Pajaro River 4)2S2  pooled slopes
o	Slope source	Purpose
S1 (primary)	Pooled siin the San Lorenzo River and -2.711 ± 0.063 in the Pajaro River 4)

S4	Morphology-specific (fragment vs fibre)	The draft reports fragments exhibited a substantially steeper C-PSD slope (a′ = -2.739 ± 0.038, R² = 0.993) than fibers 4

S5	Existing Monte Carlo with structural slope SD 0.25	Already in the draft (95% interval after adding the structural slope SD (0.25) used in this study's Monte Carlo 4)

S6 (reference only)	External slopes: Dutch FTIR median and LDIR median	The draft gives medians of −2.32 (range −2.35 to −2.29; n = 2) for FTIR and −3.31 (IQR −3.56 to −2.98; n = 5) for LDIR fits to Dutch river water 4 as context, not as a prediction

Each row: CF, W1 mean, W1 median (IQR), and ratio to S1. If S3/S4 aggregation requires weighting by river/morphology share, document the weights.
P7 - Figures
1.	Main-text figure: pooled C-PSD with both fits (single and two-segment) overlaid; shade the LOD fitting window (165-385 µm) and the measured sieve window (50-500 µm); mark the breakpoint (with bootstrap CI as a bar); show the extrapolation region down to 1 µm; annotate CF and W1 for each model. Plot with log axes.
2.	Main-text or SI figure: the scenario table (P6) as a forest/dot plot (log x-axis).
3.	SI figure: the P5 observed-vs-predicted coarse-side ratio.
4.	Keep the existing Fig. 8 (slope comparison vs Dutch rivers) and the existing SI figure comparing the single-slope vs two-segment fits across matrices (C-PSD power law fits for particle lengths for different matrices (colored points/lines) for single-slope (left panel) vs two-segment fits (right panel) 3). Update from the new export.
P8 - Reproducibility metadata
•	Record package names and versions used for fitting and for the heterogeneity test (an author comment asks for exactly this: Hazel can you share what package version you used? 4). Write them to outputs/session_info.txt and report in Methods/SI. If the version is not recoverable from the lockfile, raise TODO-AUTHOR-6.
•	Pin the random seed(s) for the Monte Carlo and bootstrap and log them.

3. MANUSCRIPT TASKS
General: edit the .docx/.qmd; numbers in {{ }} come from outputs/psd_rescaling_params.json. Keep Zotero fields intact. Do not remove existing author comments. Add new comments in tracked changes only.
M1 - Methods: "Particle size distribution (PSD) and power-law fitting"
The current subsection is largely stripped of equations (the extracted draft text shows the heading with citation fragments and only the CF prose). Restore and define, in this order:
1.	The differential power law: dN/dL = kL^a, where L is particle length, a the differential slope, k the scaling coefficient (SI form: dN/dL = kL^a, where L is particle length, a is the differential PSD slope, and k is the scaling coefficient 3). This resolves an author comment: Define power law equation here 4.
2.	The cumulative form and conversion: N(≥L) = b′L^a′ with a = a′ − 1 (SI). One sentence only; details in SI (per R4).
3.	The CF equation, as an explicit displayed equation (resolves Should define the CF equation 4): CF = N(L_target,min, L_target,max) / N(L_measured,min, L_measured,max) = (UL_D^(a+1) − LL_D^(a+1)) / (UL_M^(a+1) − LL_M^(a+1)); the coefficient k cancels, as stated in the SI.
4.	The window-selection rule (LOD window), then the explicit statement that the fitting window (165-385 µm) is not the empirical concentration interval, which is defined by the 50 and 500 µm sieves (already in the draft).
5.	The two-segment model: a short paragraph (3-4 sentences) plus a pointer to SI. Content: fitted to the same window as the primary model; breakpoint chosen by minimum AIC among bin-edge candidates with at least three bins per side; independent slopes via interaction term; continuity of the differential PSD enforced at the breakpoint; ΔAIC = AIC_piecewise − AIC_single (negative favours piecewise). All of this is in the SI (candidate breakpoints at bin edges with at least three bins on each side; OLS with segment-by-length interaction; breakpoint with minimum AIC chosen; ΔAIC = AIC_piecewise − AIC_single 3). Add whichever of the P4 procedures was implemented.
6.	The sample-handling statement and citation of SWB-MP1-Rev1: the author comment asks to cite https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/documents/microplastics/swb-mp1-rev1.pdf 4. Add it as a Zotero entry (it is a state method document; confirm the title/date on the page before entry).
M2 - Results: "Particle-size distributions reveal substantial unmeasured microplastics"
Rewrite so that both models are reported with equal numerical completeness. Template (fill from JSON; do not alter the structure without author approval):
Pooled river-water C-PSDs were well described by a power law (differential slope a = {{a_diff}} ± {{a_diff_SE}} SE; R² = {{R2_window}}) within the {{window_LOD_um}} µm fitting window. Applying the single-slope model yields a size-range correction factor of {{CF_single}} (50-500 → 1-5000 µm) and a model-based median concentration of {{C_W1_single_median}} particles L⁻¹ (IQR {{C_W1_single_IQR}}). A two-segment model with a breakpoint at {{breakpoint_um}} µm (bootstrap 95% interval {{breakpoint_boot_CI}}; differential slopes {{a_fine_diff}} below and {{a_coarse_diff}} above) was favoured by ΔAIC = {{dAIC}} ({{dAIC_boot_CI}}), giving CF = {{CF_two}} and {{C_W1_two_median}} particles L⁻¹ ({{ratio_W1_single_over_two}}-fold lower than the single-slope estimate). Because both models are constrained only by data within the {{window_LOD_um}} µm window and neither is validated below 50 µm, we treat the single-slope value as the primary, comparison-aligned estimate and the difference between models as a structural uncertainty (Table X).
Rules for M2:
•	Use median (IQR) for concentrations, per the author comments (Report median and IQR 4, and "Median?" on the abstract's "model-based concentration"). If the headline remains a mean, label it "mean" explicitly (TODO-AUTHOR-3).
•	Retain the existing caveat that the CI reflects uncertainty in the estimated slope but not uncertainty in the assumption that a single power law extends to 1 µm. 4
•	Add Table X = the P6 scenario table.
•	Add the P5 consistency check as 2-3 sentences, hedged as described in P5.
•	Dutch comparison paragraph: keep W2 as primary (existing text: rescaled to our 50-500 µm sieve window, which requires only interior rescaling for every dataset 4). The W1 contrast keeps its existing caveat; the Dutch data have no two-segment fit, so only our data are shown under both models. 4 Update the numbers (the draft currently gives Dutch medians from 39,000 (Meuse) to 9.4 × 10⁷ (Lek canal) particles m⁻³ 4) from the regenerated R/rivm_benchmark.qmd.
M3 - Wording rules driven by the P4 outcome
•	If the bootstrap CI of ΔAIC excludes 0 and the breakpoint is stable: keep "strongly favoured" but add "within the fitting window", and keep D1 reasoning (no sub-50 µm validation).
•	If the CI includes 0 or the breakpoint is unstable: replace "strongly favoured" with "weakly supported" and state that the data cannot distinguish single from piecewise structure.
•	In either case, never write that the two-segment model "confirms" the single-slope result or is "more accurate". It is a different structural assumption, with no independent validation at small sizes.
M4 - Abstract, Synopsis, Conclusions
Replace the single-number statement. The current abstract reads: power-law size-distribution modeling yielded a model-based concentration of approximately 600 particles L⁻¹ (slope-derived 95% CI: 460-790 particles L⁻¹) across the full 1-5000 µm microplastic size range. 4 New template:
Size-distribution modelling suggests that the full 1-5000 µm microplastic concentration is roughly {{C_W1_two_median}}-{{C_W1_single_median}} particles L⁻¹ (median) depending on the assumed power-law structure (two-segment vs single-slope), i.e. {{ratio_W1_single_over_two}}-fold higher than the directly measured 50-500 µm fraction ({{C_W2_median}} particles L⁻¹ median).
Keep the existing hedge that these are model-based size-aligned estimates rather than directly measured concentrations 4. In Conclusions, the existing sentence "Further applying the fitted PSD ... approximately 600 particles L⁻¹" must be rewritten with the same range. Note the extracted Conclusions text is corrupted by citation fragments (...approximately 600 particles L⁻¹ ⁴⁹⁵⁰not be extrapolated from our microplastic PSD without additional evidence... 4); inspect the source .docx, do not paste over the other content in that paragraph.
M5 - Discussion: new paragraph "Choice of PSD model and structural uncertainty"
Write 2 paragraphs (~250-300 words total), covering, in order and with citations from Section 5 only:
1.	Why the single power law is the primary model: it is the basis of the established size-alignment framework and keeps our W1 values on the same footing as other studies (Koelmans 2020; Segur 2026; the Dutch comparator).
2.	Evidence that real PSDs can deviate from a single power law, in either direction at small sizes: curvature and low-bias for small particles in many reported PSDs (Segur 2026); underprediction of 5-45 µm counts by a power law in an estuary-to-lake transect (Lake Superior study); two fragmentation regimes (threshold model); log-normal-like small-size end (fracture model).
3.	Fitted minimum size and slope are not independent: a larger-scale analysis reported that in general, the exponent increases with increasing values of the minimum size 9 and that with a lower size detection limit, the minimum size for which the power law regime is valid is also lower. 9 That same study argued exponents are likely to be valid from 1 to 5000 µm 9 for its datasets. Present this as a counterpoint, not as support for either model.
4.	Our two-segment model: describe it as a sensitivity/structural alternative within the established framework; state that, to the best of our knowledge, [TODO-AUTHOR-4 - only if the human literature check supports it] piecewise-slope rescaling has not previously been applied in size alignment.
5.	Consequence: the headline range, not a single value, should be used when this study is cited, and the extrapolated W1 values should not be used for exposure or risk assessment without sub-50 µm measurement.
M6 - Limitations paragraph (add or extend)
Include each of the following as a separate sentence:
•	Both models are fitted within a {{window_LOD_um}} µm window and extrapolated more than two orders of magnitude to 1 µm; the selected window is also the longest near-linear stretch, so choice of window influences the result (cf. the cited dependence of the power law on size range: this suggests that the power law is sensitive to the size ranges used to predict trends and indicates that predicting smaller size ranges from larger ones may not work in many cases. 5).
•	Statistical limits of ΔAIC on cumulative bins and the breakpoint search (state what P4 did).
•	Sample storage: samples were stored in airtight glass containers for up to a year until processing 4 (after preservation in isopropyl alcohol). Add the storage conditions (temperature, light exposure, duration per sample) once supplied (TODO-AUTHOR-7), and state that fragmentation during storage could steepen the slope (an author comment raises this: it's possible they could have fragmented during storage - increasing the power law slope and influencing the extrapolation 4). Frame this as a possibility, not a finding, unless a test exists.
•	Field contamination controls: an author comment asks whether there was a clothing policy for collection; add the answer (TODO-AUTHOR-8). Do not invent.
•	Nanoplastics and sub-1 µm material are outside scope; the Segur framework notes that nanoplastics or DOC produced during fragmentation are not included in the model 2.
M7 - Consistency sweep (global find-and-fix, then report a diff list)
•	1,800 vs 1,850 CF: use the JSON value everywhere.
•	0.33 L⁻¹ (mean) vs 0.40 L⁻¹ (median) vs 400 particles m⁻³: after P1, make every instance consistent in statistic and units; L⁻¹ vs m⁻³ conversions must be exact (1 L⁻¹ = 1,000 m⁻³).
•	Sample counts (57 vs 60) and particle counts in Abstract, Methods, Table 1, and SI.
•	Slope convention: all body-text slopes in a (differential); the draft's mixed reporting of a′ and a must be reduced per R4. Per the author's comment, We should simply report one metric (the differential-slope) in the manuscript - too confusing to report two. 4 Flag any place where a′ is needed for a methods statement and move it to the SI.
•	"Slope-derived 95% CI" must never appear without the clause that it excludes structural uncertainty.
•	Replace any "novel", "first" or "new method" wording about the piecewise approach (see Section 0).
M8 - SI edits
1.	Add the numeric two-segment results the SI currently does not show in text (the SI describes the method but the breakpoint, segment slopes and ΔAIC values were not found in the extracted text): add Table S-B with the P2 two-segment keys, bootstrap CIs, and CF.
2.	Add the P6 scenario table and the P5 consistency-check figure/table.
3.	Update the benchmark table (the SI's Dutch/this-study W2 and W1 table) with regenerated values, including the two-segment W1 column for this study.
4.	Add the unit-test benchmark (CF = 8.32 for α = 1.6, 30-2000 → 1-5000 µm) as a one-line validation note in the SI CF section.
5.	Add a short "Model-form sensitivity" subsection to match the pipeline additions (P4, P5), keeping the existing section "Sensitivity of extrapolation to single- versus piecewise power-law structure" and its wording that the comparison isolates the effect of allowing the PSD slope to change without introducing differences in fitting-window selection. 3

4. Sequence and gating
1.	P1 (blocking) → stop and report if unresolved.
2.	P3 tests → P2 export → P4, P5, P6, P7, P8.
3.	Report a short results memo to the owner (all numbers, plus any unexpected result: unstable breakpoint, ΔAIC CI including 0, observed coarse ratio far from both models).
4.	Only after the owner approves the memo: M1-M8.
5.	Final: regenerate the full document from code and run the Acceptance checklist.

5. Literature grounding table (verify before citing; resolve each in Zotero)
The status column reflects what was actually retrieved when this sheet was prepared. "Snippet/abstract" means only search-result text or an abstract was seen, so verify the specific passage on the source page before quoting. Author lists are intentionally omitted where they were not visible; fill them from the publisher page, not from memory.

Key	Source (as identified)	Supports	Retrieved status
KEY_Koelmans2020	"Solving the Nonalignment of Methods and Approaches Used in Microplastic Research ..." (Koelmans et al., 2020); https://pmc.ncbi.nlm.nih.gov/articles/PMC7547870/
Single-power-law CF; the default exponent α = 1.6 and the worked example CF = 8.32 (30-2000 → 1-5000 µm); the recommendation to calibrate with the dataset's own PSD: it is recommended to always correct a data set with a calibration of eq 1 that is based on that same data set. 1
Snippet (search result)
KEY_Segur2026	"Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media", Microplastics and Nanoplastics; https://link.springer.com/article/10.1186/s43591-026-00205-5 (identified in the search results as Segur et al., 2026; the page text read did not display the author list)	C-PSD removes binning bias; C-PSD extension of size alignment; single power law per dataset; small-particle low bias; curvature in fibres; extrapolation uncertainty. We highlight how standard linear regression of the power law slope is strongly biased by data binning, and show that fitting a cumulative PSD (C-PSD) removes the binning bias. 2
url_read of the page (fields extracted); verify the quoted passages
KEY_Kooi2021	"Characterizing the multidimensionality of microplastics across environmental compartments" (Kooi et al., 2021); https://www.sciencedirect.com/science/article/pii/S0043135421006278
MLE (Clauset-style) power-law fits above x_min; exponent depends on x_min; compartment-specific slopes; argument that exponents likely hold from 1-5000 µm	Snippet (open-access PDF text and abstract)
KEY_LakeSuperior	"Size Distributions of Microplastics in the St Louis Estuary and Western Lake Superior", Environ. Sci. Technol. 2024; https://pubs.acs.org/doi/10.1021/acs.est.3c10776
Power-law extrapolation underpredicted 5-45 µm counts versus flow cytometry; sensitivity of the power law to size range: Power law size distribution modeling reveals deviations that help in the understanding of potential sources and removal mechanisms, although it significantly underpredicts microplastic counts for smaller-sized particles (5-45 μm) 5
Snippet (abstract and results excerpts)
KEY_Threshold	"A threshold model of plastic waste fragmentation: New insights into the distribution of microplastics in the ocean and its evolution over time" (arXiv 2307.04162); https://arxiv.org/html/2307.04162v2/
Two distinct size regimes (large power-law, small-particle regime below ~150 µm); preprint, treat as a conceptual model	Snippet (preprint text)
KEY_MarineModel	"A model for the size distribution of marine microplastics", PLOS ONE (2021); https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0259781 (also PMC8631679)
Log-normal-like small-size end with power-law large-size end; conceptual support for slope change	Snippet
KEY_MPsizeBase	"MPsizeBase: a database for particle size distributed and ..." (EarthArXiv); https://eartharxiv.org/repository/object/10605/download/23199/
Conversion a = a′ − 1; fit of C-PSD on log-log; small-MP bias correction	Snippet (preprint text)
KEY_RIVM / Dutch comparators	RIVM report 2026-0069 (as cited in the SI: Dutch surface-water data compiled for RIVM report 2026-0069 3) and the Dutch FTIR/LDIR studies listed in the SI table	Benchmark method (poweRlaw MLE above x_min, triangular α and ±10% limits)	From the user's SI only; verify against the RIVM report itself
KEY_Clauset2009	Cited in a published paper's text as the basis of the bootstrap/MLE power-law fitting (the fit was bootstrapped (n = 100) to obtain means and standard deviations for a (Clauset et al. 2009) 10)
MLE fitting reference for the P4 option 1	Name/year seen only inside another paper; locate and verify the actual reference before use

Do not cite for the piecewise approach any source not in this table. If P4 requires statistical-methods references (AIC, breakpoint tests, bootstrap), mark them [CITATION NEEDED - owner to supply].

6. Acceptance checklist (agent must tick every item and attach evidence)
•	[ ] P1 resolved or escalated; one canonical sample manifest; assertions in CI.
•	[ ] Unit tests pass, including CF = 8.32 (α = 1.6; 30-2000 → 1-5000 µm) and CF ≈ 1,850 (a = −2.92; 50-500 → 1-5000 µm).
•	[ ] psd_rescaling_params.json exists; manuscript and SI numbers all render from it (grep shows no hard-coded copies of CF, slopes, ΔAIC, or W1 values).
•	[ ] No remaining "1,800" or other conflicting CF; Abstract, Results, Conclusions and SI agree.
•	[ ] The headline is a range spanning both models; no standalone "600 particles L⁻¹ (95% CI 460-790)" remains.
•	[ ] Median (IQR) is used wherever concentrations are summarised, or "mean" is explicitly labelled.
•	[ ] One slope convention (a) in the body text; a′ only in the SI.
•	[ ] Methods define the power law, the CF equation, the window rule, and the two-segment procedure; SWB-MP1-Rev1 cited; package versions given.
•	[ ] Discussion paragraph and Limitations paragraph added; every cited work comes from Section 5 and is resolved in Zotero with a verified DOI/URL; refs 51-54 re-inserted via Zotero; numbering regenerated.
•	[ ] No "novel"/"first" claim about the piecewise approach.
•	[ ] P4 and P5 results are reported honestly, including any unfavourable outcome, with the M3 wording rules applied.
•	[ ] All open TODO-AUTHOR items are listed in the final memo.

7. TODO-AUTHOR items (agent must not resolve these; list them in the memo)
1.	Sample-set discrepancy: which sample set and QC filter are canonical (57 vs 60 samples; 0.33 vs 0.40/1.01 L⁻¹)?
2.	Volume-matching of the 500-5000 µm visual-ID counts relative to the 50-500 µm µFTIR counts, and whether any of the 237 were spectroscopically confirmed.
3.	Headline statistic: mean or median for the abstract number (the owner's comment favours median).
4.	Novelty statement: a human literature check on piecewise-slope size alignment before any "to our knowledge" claim.
5.	Statistical-methods references for AIC/breakpoint-search/bootstrap justification (none retrieved).
6.	Package versions used for fitting and the heterogeneity test.
7.	Storage conditions (temperature, light, duration per sample) for IPA-preserved samples.
8.	Field clothing/contamination policy for collection.
9.	Whether the headline range should be shown in the Synopsis/TOC graphic text as well as the abstract.
References
1.	Koelmans AA, Redondo-Hasselerharm PE, Mohamed Nor NH, Kooi M. Solving the Nonalignment of Methods and Approaches Used in Microplastic Research to Consistently Characterize Risk. Environ Sci Technol. 2020;54(19):12307-12315. doi:10.1021/acs.est.0c02982. PMID: 32885967.
2.	Segur T, Hough I, Dobiasova N, Voisin D, Richon C, Angot H, Thomas JL, Sonke JE. Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations in environmental media. Micropl.&amp;Nanopl. 2026;6(1). doi:10.1186/s43591-026-00205-5.
3.	Savoca-et-al._W2W-river-plastics_SI_v1_sc_rivm.docx. Internal reference: file:28961#chars=11331-11393. Accessed 2026-10-14.
4.	Savoca-et-al._W2W-river-plastics_complete-main-text_v1_sc_rivm.docx. Internal reference: file:28962#chars=20459-20787. Accessed 2026-10-14.
5.	Thomas A, Marchand J, Schwoerer GD, Minor EC, Maurer-Jones MA. Size Distributions of Microplastics in the St Louis Estuary and Western Lake Superior. Environ Sci Technol. 2024;58(19):8480-8489. doi:10.1021/acs.est.3c10776. PMID: 38693822.
6.	A threshold model of plastic waste fragmentation: New insights into the distribution of microplastics in the ocean and its evolution over time. Retrieved 2026-10-14, from https://arxiv.org/html/2307.04162v2/
7.	Aoki K, Furue R. A model for the size distribution of marine microplastics: A statistical mechanics approach. PLoS One. 2021;16(11):e0259781. doi:10.1371/journal.pone.0259781. PMID: 34847182.
8.	MPsizeBase: a database for particle size distributed and. Retrieved 2026-10-14, from https://eartharxiv.org/repository/object/10605/download/23199/
9.	[PDF] Characterizing the multidimensionality of microplastics across. Retrieved 2026-10-14, from https://vbn.aau.dk/ws/files/465270446/Characterizing_the_multidimensionality_of_microplastics_across_environmental_compartments.pdf
10.	Semmouri I, Vercauteren M, Van Acker E, Pequeur E, Asselman J, Janssen C. Presence of microplastics in drinking water from different freshwater sources in Flanders (Belgium), an urbanized region in Europe. FoodContamination. 2022;9(1). doi:10.1186/s40550-022-00091-8.
