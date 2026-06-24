# W2W River Plastics — Probabilistic Microplastics Risk Characterization

End-to-end, fully probabilistic environmental risk assessment (ERA) workflow for microplastics in freshwater surface water and beach sediment. Developed as part of the California Watershed to Whales (W2W) project at [OEHHA](https://oehha.ca.gov/).

---

## Overview

This repository implements a five-step probabilistic workflow:

1. **C-PSD fitting** — fit cumulative particle size distributions (C-PSD) to individual particle measurements using the Segur et al. (2026) two-step LOD detection algorithm (Python, called via `reticulate`).
2. **Concentration rescaling** — use the fitted power-law slope (α) to rescale monitoring concentrations from a sampled size range to a target range, propagating slope uncertainty via Monte Carlo ([Coffin et al. 2022](https://doi.org/10.1016/j.scitotenv.2022.155859)).
3. **Environmental Exposure Distribution (EED)** — nonparametric bootstrap of corrected site-level concentrations.
4. **Hazard threshold distribution** — probabilistic species sensitivity distributions (pSSD++) via the [PSSDplusplus R package](https://github.com/ScottCoffin/ToMEx2.0_EcoToxRisk/tree/main/package) using the [ToMEx 2.0 toxicity database](https://github.com/ScottCoffin/ToMEx2.0_EcoToxRisk).
5. **Risk characterization** — 1D and 2D Monte Carlo pairing of exposure and hazard, expressed as Risk Quotient (RQ = Exposure/Hazard) and exceedance probability P(RQ > 1).

All reusable computational functions are documented with roxygen2-style headers for eventual integration into PSSDplusplus.

---

## Repository Structure

```
W2W_river_plastics/
├── R/
│   ├── probabilistic_risk_characterization.Rmd   # Main analysis document (renders to HTML)
│   ├── probabilistic_risk_characterization_example.Rmd  # Minimal template for new datasets
│   ├── mp_risk_utils.R         # Reusable functions: C-PSD fitting, correction factor, EED, RQ
│   ├── cpsd_plotting.R         # Plotting functions: plot_cpsd_multi(), MP_PALETTE
│   ├── segur_psd_adapter.py    # Python wrapper for Segur 2026 LOD detection algorithm
│   ├── Util.R                  # Legacy project utilities (river flow, large MP data)
│   ├── FirstDraftFigs.R        # Exploratory figures (drafts)
│   ├── FirstDraftTables.R      # Exploratory tables (drafts)
│   └── SuppInfoDraft.R         # Supplementary information drafts
├── data_input/
│   ├── Part_dets_comb.rds      # Individual particle measurements (µFTIR, river water + sediment)
│   ├── Part_dets_summ.rds      # Site-level summary concentrations (particles/L)
│   └── Opt_micro_all_cut.rds   # Large MP (optical) monitoring data
├── data_output/
│   ├── Part_dets_cleaned.csv   # Cleaned particle data exported by Rmd
│   └── Part_dets_comb_summary.csv
├── figures/                    # Saved plot outputs (PNG, 300 dpi)
├── assets/                     # Reference PDFs and supporting literature
└── README.md
```

---

## Prerequisites

### R packages

```r
install.packages(c(
  "tidyverse", "truncnorm", "fitdistrplus", "ggpubr",
  "sensitivity", "rmarkdown", "reticulate", "knitr"
))

# PSSDplusplus (GitHub)
devtools::install_github(
  "ScottCoffin/ToMEx2.0_EcoToxRisk",
  subdir = "package",
  upgrade = "never",
  build_vignettes = FALSE
)
```

### Python

Python 3.8+ with `numpy` and `scipy` is required for the Segur 2026 C-PSD algorithm:

```bash
pip install numpy scipy
```

The R package `reticulate` will use whichever Python is on your PATH (`Sys.which("python")`).

---

## Running the Analysis

Open R in the **project root** directory (or use RStudio with the project file), then:

```r
rmarkdown::render("R/probabilistic_risk_characterization.Rmd", output_dir = "R")
```

The Rmd sets `knitr::opts_knit$set(root.dir = normalizePath(".."))` so all data paths resolve from the project root regardless of where R is launched.

Monte Carlo parameters are controlled by two variables at the top of the setup chunk:

```r
n_boot <- 100    # bootstrap replicates (increase to ≥ 1000 for publication)
n_mc   <- 20000  # alpha draws for correction factor propagation
```

---

## Adapting to a New Dataset

The minimal template is `R/probabilistic_risk_characterization_example.Rmd`. To apply this workflow to a new dataset:

### 1. Prepare input data

Two RDS files are required:

**`Part_dets_comb.rds`** — individual particle table, one row per particle:

| Column | Type | Description |
|---|---|---|
| `max_length_um` | numeric | Longest particle dimension (µm) |
| `min_length_um` | numeric | Shortest measured dimension (µm) |
| `aspect_ratio` | numeric | L/W; ≥ 3 → fiber, < 3 → fragment |
| `area_um2` | numeric | Projected area (µm²) |
| `material_class` | character | "plastic", "mineral", "organic matter", etc. |
| `sample_type` | character | "river water", "beach sand", etc. |
| `sample_location` | character | Site/location name |
| `bad_spectra` | logical | TRUE = keep (good spectrum) |
| `Client_ID_MSSupdate` | character | Sample ID containing 8-digit date (YYYYMMDD) |

**`Part_dets_summ.rds`** — site-level concentration table, one row per sample:

| Column | Type | Description |
|---|---|---|
| `extrap_conc_PPL` | numeric | Measured MP concentration (particles/L) |
| `sample_type` | character | "river water", "beach sand", etc. |
| `material_simple` | character | "plastic", "non-plastic" |
| `sample_or_blank` | character | "sample" or "blank" |
| `Client_ID_MSSupdate` | character | Sample ID (same format as above) |

### 2. Adjust size range parameters

In the `rescaling-cf` chunk, update the size range to match your dataset:

```r
L_meas_min <- 50   # Lower detection limit of your instrument (µm)
L_meas_max <- 500  # Upper size range of measured particles (µm)
L_tar_min  <- 1    # Target lower bound for risk assessment (µm)
L_tar_max  <- 5000 # Target upper bound for risk assessment (µm)
```

### 3. Update the monitoring concentration column

In section 5.1, change `C_measured_ppL = extrap_conc_PPL` to match your concentration column name.

### 4. Customize PSSDplusplus parameters

In section 7, update `param_values` to use your fitted α and SE:

```r
param_values <- PSSDplusplus::param_default_values |>
  dplyr::mutate(
    freshwater.alpha.mean = cpsd_fit_all$a_cpsd,
    freshwater.alpha.sd   = cpsd_fit_all$se_a_cpsd
  )
```

---

## Key Functions (in `R/mp_risk_utils.R`)

All functions carry roxygen2-style documentation for future integration into PSSDplusplus.

| Function | Purpose |
|---|---|
| `fit_cpsd_segur_r(x_um, bin_um, fit_range_um)` | Bin particles, call Python LOD algorithm, return power-law fit |
| `volume_particle(shape, length_um, width_um, height_um)` | Vectorized particle volume (fragment = ellipsoid, fiber = cylinder) |
| `bootstrap_aspect_ratio(aspect_ratio_vec, n_boot)` | Bootstrap median 1/AR for width estimation |
| `alpha_dist(mu, sd, n, lower, upper)` | Truncated normal draws of power-law slope |
| `correction_factor(a, L_meas_min, L_meas_max, L_tar_min, L_tar_max)` | Power-law rescaling correction factor |
| `bootstrap_eed(x, n_boot, probs)` | Nonparametric EED percentiles with bootstrap uncertainty |
| `ecdf_bands(x, grid, n_boot, probs)` | Bootstrap ECDF confidence bands |
| `draw_rq_mc1d(haz_df, exposure_draws, n_risk)` | 1D Monte Carlo risk quotient |
| `summarize_rq(risk_draws)` | P(RQ > 1), RQ_p50, RQ_p95, RQ_p99 per ERM × HCx |
| `mc2d_risk(monitoring_df, combined_cf, haz_df, ...)` | 2D Monte Carlo (outer = uncertainty, inner = variability) |
| `param_bounds(param_values, k)` | Extract ±k·SD bounds from PSSDplusplus param_values |

---

## Key Outputs

The rendered HTML report includes:

- Log-log C-PSD plots for all shape classes (fragment, fiber, all) for both length, surface area, and volume
- Location-stratified power-law slope comparison table
- Correction factor distribution (median, 90% CI)
- EED bootstrap distributions (q50, q95)
- 1D and 2D Monte Carlo RQ distributions by ERM × HCx
- Morris sensitivity analysis ranking parameter influence on RQ
- Key findings summary table (Section 12)
- Dynamic inline R text throughout — all reported numbers update automatically when data changes

Saved figures (PNG, 300 dpi) are written to `figures/`.

---

## References

- Segur A, et al. (2026). Using the power law size distribution to extrapolate and compare microplastic number and mass concentrations across studies. *Microplastics and Nanoplastics*. [doi:10.1186/s43591-026-00205-5](https://doi.org/10.1186/s43591-026-00205-5)
- Coffin S, et al. (2022). Risk-based management framework for microplastics in aquatic ecosystems. *Science of the Total Environment*. [doi:10.1016/j.scitotenv.2022.155859](https://doi.org/10.1016/j.scitotenv.2022.155859)
- Kooi M, Koelmans AA (2019). Simplifying microplastic via continuous probability distributions for size, shape, and density. *Environmental Science & Technology Letters*. [doi:10.1021/acs.estlett.9b00379](https://doi.org/10.1021/acs.estlett.9b00379)
- Zhao B, et al. (2026). Physically-based mesh selectivity correction model for standardized microplastic abundance estimates in aquatic environment. *Water Research* 299: 125833. [doi:10.1016/j.watres.2026.125833](https://doi.org/10.1016/j.watres.2026.125833)
- Wang S, et al. (2026). Implications of method- and instrument-based size detection limits in µFTIR-based microplastic analysis. *Talanta* 296: 128417. [doi:10.1016/j.talanta.2025.128417](https://doi.org/10.1016/j.talanta.2025.128417)
- Chen H, et al. (2026). An accurate size-probability distribution method for converting microplastic counts to mass. *Environmental Science & Technology* 60: 1263–1274. [doi:10.1021/acs.est.5c12243](https://doi.org/10.1021/acs.est.5c12243)

---

## License

Code: MIT. Data: California OEHHA — contact scott.l.coffin@gmail.com for access inquiries.
