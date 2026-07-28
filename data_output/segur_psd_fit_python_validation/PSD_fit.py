"""
psd_powerlaw_fit.py
===================
Automatic power-law fitting of microplastic Particle Size Distributions (PSDs).

INPUT
-----
An Excel file (MPsizeBase_Concentrations.xlsx) with a 'Literature PSD data'
sheet whose first two rows are metadata and whose third row contains column
names.  The script uses four of those columns:
  PSD_ID            – identifier for each PSD (string)
  Low_size          – lower bound of the size bin (µm)
  High_size         – upper bound of the size bin (µm)
  Bin_concentration – MP number concentration in the bin (MP# m⁻³)
  shape_factor      – volume coefficient for the MP shape (used for mass extrapolation)
  dimension         – size dimension exponent for the MP shape: 1 for Fibers, 2 for Films, 3 for Fragments (used for mass extrapolation)
All other metadata columns are loaded but left untouched.

WHAT THE SCRIPT DOES (for each PSD)
-------------------------------------
1. Computes the BN-PSD (Bin-Normalised PSD):
     BN-PSD = Bin_concentration / bin_width   [MP# m⁻³ µm⁻¹]

2. Computes the C-PSD (Cumulative PSD):
     C-PSD[i] = sum of Bin_concentration for all bins with Low_size >= Low_size[i]

3. Detects the automatic LOD window (two-step method):

   Step 1 – Global residual filter
     A log-log OLS regression is fitted on all bins.
     Residuals are normalised by their standard deviation.
     Only bins with normalised residual > RESIDUAL_THRESHOLD are kept as
     candidates (i.e. bins that sit above the global trend line if 
     RESIDUAL_THRESHOLD = 0).

   Step 2 – Contiguous window optimisation
     All contiguous sub-windows of the candidate bins (length ≥ MIN_BINS,
     R² ≥ MIN_R2) are tested.  The window that maximises the number of bins
     (ties broken by R²) is selected as the final LOD window. If no windows 
     fall under these criterias, the code select all the candidate bins.

4. Fits a power law  C-PSD = 10^b * Low_size^a  on the selected window
   via OLS in log-log space.  Extracts slope (a) and intercept (b).

5. Converts C-PSD parameters to BN-PSD parameters:
     slope_BN_PSD     = slope_C_PSD - 1
     intercept_BN_PSD = -slope_C_PSD × 10^intercept_C_PSD
     
6. Extrapolate the reported concentration to the size range MP_NUM_LOW to 
   MP_NUM_HIGH (set to 1-5000µm by default)
   
7. Saves a two-panel figure per PSD (BN-PSD left, C-PSD right) to a
   subfolder and writes a summary Excel file.

OUTPUTS
-------
  <OUTPUT_DIR>/               – one PNG per PSD
  <OUTPUT_EXCEL>              – one row per PSD with LOD, slope, intercept

USAGE
-----
  python psd_powerlaw_fit.py
  (edit Section 1 to point to your file and tune parameters)
"""

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats
import warnings
warnings.filterwarnings("ignore")


# ──────────────────────────────────────────────────────────────────────────────
# 1.  CONFIGURATION  ← edit here
# ──────────────────────────────────────────────────────────────────────────────

INPUT_FILE   = "MPsizeBase_v15_05_2026.xlsx"  # input Excel file
OUTPUT_EXCEL = "PSD_fit_results.xlsx"   # summary results
OUTPUT_DIR   = "PSD_plots"            # folder for per-PSD plots

# --- LOD detection parameters ------------------------------------------------
MIN_BINS   = 3      # minimum number of bins required in the regression window
MIN_R2     = 0.90   # windows with R² below this threshold are discarded
                    # (set to 0 to accept any window)

# Normalised-residual threshold for Step 1.
# Bins whose normalised residual ≤ this value are excluded as candidates.
# 0 = exclude only strictly negative residuals (default, conservative).
# Increase (e.g. 0.5) to be more aggressive in removing borderline bins.
RESIDUAL_THRESHOLD = 0.0

# Hard upper size limit: bins with Low_size ≥ this value are always excluded.
MAX_LOW_SIZE_UM = 5000.0   # µm

# Coefficient applied to L_max (Low_size of the BN-PSD peak bin).
# Bins with Low_size < LAMBDA_LMAX × L_max are always excluded.
# Set to 0 to disable this lower-size cut.
LAMBDA_LMAX = 1.0

# If True:  apply both size limits BEFORE the global regression in Step 1
#           → residuals are computed on a physically clean subset.
# If False: apply size limits as a post-filter on the candidate mask.
FILTER_BEFORE_REGRESSION = False

# --- Integration range for MP extrapolation (number and mass) ----------------
MP_NUM_LOW  =    1.0   # µm  (lower bound)
MP_NUM_HIGH = 5000.0   # µm  (upper bound)


# ──────────────────────────────────────────────────────────────────────────────
# 2.  CORE COMPUTATIONS
# ──────────────────────────────────────────────────────────────────────────────

def compute_bn_psd(df):
    """
    Bin-Normalised PSD: concentration divided by bin width (µm).
    Adds column 'BN_conc' [MP# m⁻³ µm⁻¹].
    """
    df = df.copy()
    df["bin_width"] = df["High_size"] - df["Low_size"]
    df["BN_conc"]   = df["Bin_concentration"] / df["bin_width"]
    return df


def compute_cpsd(df):
    """
    Cumulative PSD: C-PSD[i] = total concentration of all particles
    with size ≥ Low_size[i].  Sorts by Low_size and adds column 'cum_conc'.
    """
    df = df.sort_values("Low_size").reset_index(drop=True)
    df["cum_conc"] = df["Bin_concentration"][::-1].cumsum()[::-1].values
    return df


def fit_log_log(df_cpsd, lod_low, lod_high):
    """
    OLS regression of log10(cum_conc) on log10(Low_size) for bins whose
    Low_size ∈ [lod_low, lod_high).  (lod_high is the High_size of the last
    selected bin, so strict '<' correctly includes only bins within the window.)

    Returns a dict {slope, intercept, r2, n, se_slope} or None if fewer
    than MIN_BINS valid points are available.
    """
    mask  = (df_cpsd["Low_size"] >= lod_low) & (df_cpsd["Low_size"] < lod_high)
    valid = df_cpsd[mask & (df_cpsd["cum_conc"] > 0) & (df_cpsd["Low_size"] > 0)]
    if len(valid) < MIN_BINS:
        return None
    log_x = np.log10(valid["Low_size"].values)
    log_y = np.log10(valid["cum_conc"].values)
    slope, intercept, r, _, se = stats.linregress(log_x, log_y)
    return {"slope": slope, "intercept": intercept,
            "r2": r**2, "n": len(valid), "se_slope": se}


def cpsd_to_bnpsd(slope_c, intercept_c):
    """
    Convert C-PSD power-law parameters to BN-PSD parameters.
      slope_BN     = slope_C  - 1
      intercept_BN = -slope_C × 10^intercept_C
    """
    return slope_c - 1, -slope_c * (10 ** intercept_c)


def compute_mp_number(slope_bn, intercept_bn):
    """
    Integrate the BN-PSD power law over [MP_NUM_LOW, MP_NUM_HIGH] (µm):
      MP# = intercept_BN / (slope_BN + 1) × (HIGH^(slope_BN+1) − LOW^(slope_BN+1))
    Returns NaN when slope_BN = −1 (non-integrable) or inputs are NaN.
    """
    if not np.isfinite(slope_bn) or not np.isfinite(intercept_bn):
        return np.nan
    if slope_bn == -1:
        return np.nan
    return (intercept_bn / (slope_bn + 1)
            * (MP_NUM_HIGH ** (slope_bn + 1) - MP_NUM_LOW ** (slope_bn + 1)))


def compute_mp_mass(slope_bn, intercept_bn, shape_factor, dimention):
    """
    Integrate the BN-PSD power law weighted by particle volume over
    [MP_NUM_LOW, MP_NUM_HIGH] (µm) to obtain a mass concentration (µg m⁻³):

      MP_mass = shape_factor * intercept_BN
                / (1 + dimention + slope_BN)
                * (HIGH^(1 + dimention + slope_BN) − LOW^(1 + dimention + slope_BN))

    The shape_factor (µg µm⁻ᵈⁱᵐᵉⁿᵗⁱᵒⁿ) encodes both the volume-to-size
    conversion and the MP density for a given shape.
    Returns NaN when the exponent (1 + dimention + slope_BN) = 0, when any
    input is non-finite, or when shape_factor / dimention are unavailable.
    """
    if not np.isfinite(slope_bn) or not np.isfinite(intercept_bn):
        return np.nan
    if not np.isfinite(shape_factor) or not np.isfinite(dimention):
        return np.nan
    exponent = 1 + dimention + slope_bn
    if exponent == 0:
        return np.nan
    return (shape_factor * intercept_bn / exponent
            * (MP_NUM_HIGH ** exponent - MP_NUM_LOW ** exponent))


# ──────────────────────────────────────────────────────────────────────────────
# 3.  AUTOMATIC LOD DETECTION
# ──────────────────────────────────────────────────────────────────────────────

def _contiguous_groups(positions):
    """
    Split a sorted array of integer positions into maximal contiguous groups.
    Yields (start_idx, end_idx) pairs into `positions`.
    Example: [0,1,2,5,6] → [(0,2), (3,4)]
    """
    if len(positions) == 0:
        return
    start = 0
    for i in range(1, len(positions)):
        if positions[i] != positions[i - 1] + 1:
            yield start, i - 1
            start = i
    yield start, len(positions) - 1


def detect_lod(df_psd):
    """
    Two-step automatic LOD detection for a single PSD.

    Step 1 — global residual filter
      Fit a log-log regression on all (or pre-filtered) bins.
      Normalise residuals by their std so RESIDUAL_THRESHOLD is scale-invariant.
      Keep only bins with normalised residual > RESIDUAL_THRESHOLD as candidates.

    Step 2 — contiguous window optimisation
      Test every contiguous sub-window of the candidate bins (length ≥ MIN_BINS,
      R² ≥ MIN_R2).  Select the longest window; ties broken by R².
      Fall back to the full size-limited range if no window passes R².

    Returns
    -------
    lod_low   : float  – Low_size  of the first selected bin (µm)
    lod_high  : float  – High_size of the last  selected bin (µm)
    fit       : dict   – regression result (slope, intercept, r2, n, se_slope)
                         or None if no valid window was found
    """
    # --- Sort bins and build BN-PSD / C-PSD ----------------------------------
    df = df_psd.sort_values("Low_size").reset_index(drop=True)
    df = compute_bn_psd(df)
    df_cpsd = compute_cpsd(df)

    # L_max: Low_size of the bin with the highest BN-PSD value
    L_max        = df.loc[df["BN_conc"].idxmax(), "Low_size"]
    lod_lo_limit = LAMBDA_LMAX * L_max   # hard lower size cut

    # ── Step 1: global regression and normalised residuals ──────────────────
    base_ok = (df_cpsd["Low_size"] > 0) & (df_cpsd["cum_conc"] > 0)
    if FILTER_BEFORE_REGRESSION:
        reg_mask = base_ok & (df_cpsd["Low_size"] >= lod_lo_limit) \
                           & (df_cpsd["Low_size"] <  MAX_LOW_SIZE_UM)
    else:
        reg_mask = base_ok

    valid_reg = df_cpsd[reg_mask]

    if len(valid_reg) < 2:
        # Too few bins for a global regression → use all size-limited bins
        cand_mask = (df["Low_size"] >= lod_lo_limit) & \
                    (df["Low_size"] <  MAX_LOW_SIZE_UM)
    else:
        log_x  = np.log10(valid_reg["Low_size"].values)
        log_y  = np.log10(valid_reg["cum_conc"].values)
        sl, ic, *_ = stats.linregress(log_x, log_y)
        res    = log_y - (sl * log_x + ic)              # raw residuals
        res_std = res.std()
        res_norm = res / res_std if res_std > 0 else res  # normalised residuals

        # Map normalised residuals back to df via Low_size
        pos_in_cpsd = np.where(reg_mask.values)[0]
        res_arr     = np.full(len(df_cpsd), np.nan)
        res_arr[pos_in_cpsd] = res_norm
        df_cpsd["_res"] = res_arr
        size_to_res = df_cpsd.set_index("Low_size")["_res"].to_dict()
        df["_res"]  = df["Low_size"].map(size_to_res)

        cand_mask = (df["_res"]    > RESIDUAL_THRESHOLD) & \
                    (df["Low_size"] >= lod_lo_limit)       & \
                    (df["Low_size"] <  MAX_LOW_SIZE_UM)

    # ── Step 2: contiguous window optimisation ───────────────────────────────
    cand_pos = np.where(cand_mask.values)[0]

    # Fallback positions (size-limited only, ignoring residuals)
    fallback_pos = np.where(
        (df["Low_size"].values >= lod_lo_limit) &
        (df["Low_size"].values <  MAX_LOW_SIZE_UM)
    )[0]

    if len(cand_pos) < MIN_BINS:
        cand_pos = fallback_pos  # not enough candidates → ignore residual filter

    if len(cand_pos) == 0:
        return np.nan, np.nan, None

    windows = []
    for g_start, g_end in _contiguous_groups(cand_pos):
        group = cand_pos[g_start:g_end + 1]
        for i in range(len(group)):
            for j in range(i + MIN_BINS - 1, len(group)):
                win   = group[i:j + 1]
                lo    = df.loc[win[0],  "Low_size"]
                hi    = df.loc[win[-1], "High_size"]
                fit   = fit_log_log(df_cpsd, lo, hi)
                if fit is None or fit["r2"] < MIN_R2:
                    continue
                windows.append({"lod_low": lo, "lod_high": hi,
                                 "n": fit["n"], "r2": fit["r2"],
                                 "fit": fit})

    if not windows:
        # Relax R² constraint and use the full fallback range
        if len(fallback_pos) >= MIN_BINS:
            lo  = df.loc[fallback_pos[0],  "Low_size"]
            hi  = df.loc[fallback_pos[-1], "High_size"]
            fit = fit_log_log(df_cpsd, lo, hi)
            if fit is not None:
                windows.append({"lod_low": lo, "lod_high": hi,
                                 "n": fit["n"], "r2": fit["r2"],
                                 "fit": fit, "fallback": True})

    if not windows:
        return np.nan, np.nan, None

    # Select the longest window; break ties by R²
    best = max(windows, key=lambda w: (w["n"], w["r2"]))
    return best["lod_low"], best["lod_high"], best["fit"]


# ──────────────────────────────────────────────────────────────────────────────
# 4.  PER-PSD DIAGNOSTIC PLOT
# ──────────────────────────────────────────────────────────────────────────────

def plot_psd(df_psd, psd_id, lod_low, lod_high, fit):
    """
    Two-panel figure for one PSD.

    Left panel  – BN-PSD (log-log): shows raw distribution and LOD window.
    Right panel – C-PSD  (log-log): shows cumulative distribution with the
                  fitted power-law regression line and its parameters.
    """
    df      = compute_bn_psd(df_psd.copy()).sort_values("Low_size").reset_index(drop=True)
    df_cpsd = compute_cpsd(df)

    C_SEL  = "#2196F3"   # blue  – selected bins / regression line
    C_ALL  = "#9E9E9E"   # grey  – all bins
    ALPHA  = 0.10        # shading transparency

    fig, axes = plt.subplots(1, 2, figsize=(11, 4))
    fig.suptitle(psd_id, fontsize=13, fontweight="bold", y=1.02)

    x_min = df["Low_size"].min() * 0.8
    x_max = df["Low_size"].max() * 1.3

    # ── Left: BN-PSD ─────────────────────────────────────────────────────────
    ax = axes[0]
    ax.loglog(df["Low_size"], df["BN_conc"], "o-",
              color=C_ALL, lw=1.2, ms=5, alpha=0.8, label="All bins")

    if np.isfinite(float(lod_low)) and np.isfinite(float(lod_high)):
        # Shade the selected LOD window
        ax.axvspan(lod_low, lod_high, color=C_SEL, alpha=ALPHA, label="LOD window")
        ax.axvline(lod_low,  color=C_SEL, lw=1.5, ls="--",
                   label=f"LOD_low  = {lod_low:.0f} µm")
        ax.axvline(lod_high, color=C_SEL, lw=1.5, ls=":",
                   label=f"LOD_high = {lod_high:.0f} µm")

        # Highlight selected bins in blue
        sel_mask = (df["Low_size"] >= lod_low) & (df["Low_size"] < lod_high)
        ax.loglog(df.loc[sel_mask, "Low_size"], df.loc[sel_mask, "BN_conc"],
                  "s", color=C_SEL, ms=6, alpha=0.9, zorder=3, label="Selected bins")

    ax.set_xlim(x_min, x_max)
    ax.set_xlabel("Lower bin size (µm)", fontsize=10)
    ax.set_ylabel("BN-PSD  (MP# m⁻³ µm⁻¹)", fontsize=10)
    ax.set_title("Bin-Normalised PSD", fontsize=11)
    ax.legend(fontsize=8, loc="lower left")
    ax.grid(True, which="both", alpha=0.25)

    # ── Right: C-PSD + regression line ───────────────────────────────────────
    ax = axes[1]
    ax.loglog(df_cpsd["Low_size"], df_cpsd["cum_conc"], "o",
              color=C_ALL, ms=5, alpha=0.7, label="All bins (C-PSD)")

    if np.isfinite(float(lod_low)) and np.isfinite(float(lod_high)):
        # Highlight selected bins
        sel_cpsd = (df_cpsd["Low_size"] >= lod_low) & \
                   (df_cpsd["Low_size"] <  lod_high)
        ax.loglog(df_cpsd.loc[sel_cpsd, "Low_size"],
                  df_cpsd.loc[sel_cpsd, "cum_conc"],
                  "s", color=C_SEL, ms=6, alpha=0.9, zorder=3,
                  label="Selected bins (regression)")

    if fit is not None:
        # Draw the fitted power-law line slightly beyond the window
        x_fit = np.logspace(np.log10(max(lod_low * 0.9, 1e-3)),
                            np.log10(lod_high * 1.1), 200)
        y_fit = 10**fit["intercept"] * x_fit**fit["slope"]
        label = (f"Power-law fit\n"
                 f"  slope = {fit['slope']:.3f}\n"
                 f"  intercept = {fit['intercept']:.3f}\n"
                 f"  R² = {fit['r2']:.4f}  (n = {fit['n']} bins)")
        ax.loglog(x_fit, y_fit, "-", color=C_SEL, lw=2.5, zorder=4, label=label)

    y_pos = df_cpsd["cum_conc"]
    ax.set_ylim(y_pos[y_pos > 0].min() * 0.5, y_pos.max() * 2)
    ax.set_xlim(x_min, x_max)
    ax.set_xlabel("Lower bin size (µm)", fontsize=10)
    ax.set_ylabel("Cumulative concentration  (MP# m⁻³)", fontsize=10)
    ax.set_title("Cumulative PSD", fontsize=11)
    ax.legend(fontsize=8, loc="lower left")
    ax.grid(True, which="both", alpha=0.25)

    plt.tight_layout()
    return fig


# ──────────────────────────────────────────────────────────────────────────────
# 5.  MAIN
# ──────────────────────────────────────────────────────────────────────────────

if __name__ == "__main__":

    # --- Load data -----------------------------------------------------------
    print(f"Loading '{INPUT_FILE}' …")
    df_all = pd.read_excel(
        INPUT_FILE,
        sheet_name="Literature PSD data",  # skip the 'Read Me' sheet
        header=2,   # row 3 (0-indexed: 2) contains the column names;
                    # pandas automatically discards rows 1-2 above the header
    )
    # Remove columns whose name is NaN (empty header cells)
    df_all = df_all.loc[:, df_all.columns.notna()]
    # Ensure all required columns are present
    for col in ("PSD_ID", "Low_size", "High_size", "Bin_concentration",
                "shape_factor", "dimention"):
        if col not in df_all.columns:
            raise ValueError(
                f"Required column '{col}' not found in '{INPUT_FILE}'. "
                f"Available columns: {list(df_all.columns)}"
            )
    # Remove bins with 0 concentration
    df_all = df_all[df_all['Bin_concentration'] != 0]
    # Collect all PSD ID
    psd_ids = df_all["PSD_ID"].dropna().unique()
    print(f"  Found {len(psd_ids)} PSD(s).")

    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # --- Process each PSD ----------------------------------------------------
    records = []

    for psd_id in psd_ids:
        print(psd_id)
        df_psd = df_all[df_all["PSD_ID"] == psd_id].copy()

        # Step 1+2: detect LOD window and fit power law
        lod_low, lod_high, fit = detect_lod(df_psd)

        # shape_factor and dimention are constant within a PSD (one value per PSD)
        shape_factor = (df_psd["shape_factor"].dropna().iloc[0]
                        if df_psd["shape_factor"].notna().any() else np.nan)
        dimention    = (df_psd["dimention"].dropna().iloc[0]
                        if df_psd["dimention"].notna().any() else np.nan)

        if fit is not None:
            # Convert C-PSD → BN-PSD parameters
            slope_bn, intercept_bn = cpsd_to_bnpsd(fit["slope"], fit["intercept"])
            # Extrapolate the number concentration from 1 to 5000 µm
            mp_num  = compute_mp_number(slope_bn, intercept_bn)
            # Extrapolate the mass concentration from 1 to 5000 µm
            mp_mass = compute_mp_mass(slope_bn, intercept_bn, shape_factor, dimention)
            fit_mode = "ok"
        else:
            slope_bn = intercept_bn = mp_num = mp_mass = np.nan
            fit_mode = "no_fit"

        records.append({
            "PSD_ID":           psd_id,
            "LOD_low_um":       lod_low,
            "LOD_high_um":      lod_high,
            # C-PSD parameters
            "slope_C_PSD":          fit["slope"]     if fit else np.nan,
            "intercept_C_PSD":      fit["intercept"] if fit else np.nan,
            "R2":               fit["r2"]        if fit else np.nan,
            "n_bins_used":      fit["n"]         if fit else np.nan,
            # BN-PSD parameters
            "slope_BN_PSD":         slope_bn,
            "intercept_BN_PSD":     intercept_bn,
            # MP# estimated by integrating the BN-PSD power law
            "extrapolation_low_size_um":    MP_NUM_LOW,
            "extrapolation_high_size_um":   MP_NUM_HIGH,
            f"MP#_{MP_NUM_LOW:.0f}_{MP_NUM_HIGH:.0f}um_MP#/m3": mp_num,
            f"MP_Mass_{MP_NUM_LOW:.0f}_{MP_NUM_HIGH:.0f}um_ug/m3": mp_mass,
            "fit_mode":         fit_mode,
        })

        # Generate and save the two-panel figure
        fig = plot_psd(df_psd, psd_id, lod_low, lod_high, fit)
        safe_name = psd_id.replace("/", "_").replace(" ", "_")
        fig.savefig(os.path.join(OUTPUT_DIR, f"{safe_name}.png"),
                    dpi=100, bbox_inches="tight")
        plt.close(fig)

    # --- Save summary Excel --------------------------------------------------
    results = pd.DataFrame(records)
    results.to_excel(OUTPUT_EXCEL, index=False)

    # --- Print summary -------------------------------------------------------
    print(f"\nResults saved to '{OUTPUT_EXCEL}'")
    print(f"Figures saved to '{OUTPUT_DIR}/'")
    print(f"\nSummary ({len(results)} PSDs):")
    print(results[["PSD_ID", "LOD_low_um", "LOD_high_um", "R2", "slope_BN_PSD", 
                   "fit_mode"]].to_string(index=False))
    print("\nDone.")
