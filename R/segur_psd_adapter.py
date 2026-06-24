"""
segur_psd_adapter.py
====================
Exposes the Segur et al. 2026 C-PSD power-law fitting algorithm as a
Python module callable from R via reticulate.

The core two-step LOD-detection algorithm is extracted verbatim from the
paper's SI code (PSD_fit.py) and wrapped in a single entry-point
function fit_psd_segur() that accepts plain arrays and returns a dict.

Key algorithm differences vs. the current R fit_cpsd():
  - Lower LOD: Python uses LAMBDA_LMAX × Low_size of BN-PSD peak bin
    (default 1.0 ×).  R uses 2 × geometric mean of the peak bin.
  - Window selection: Python exhaustively tests all contiguous sub-
    windows of positive-residual bins, selects the LONGEST window
    with R² >= MIN_R2.  R applies a sequential lower/upper LOD trim.
  - Upper LOD: Python has no explicit tail-slump pass; it is handled
    implicitly by requiring positive residuals + R² threshold.

Reference: Segur et al. 2026 https://doi.org/10.1186/s43591-026-00205-5
"""

import numpy as np
import pandas as pd
from scipy import stats
import warnings
warnings.filterwarnings("ignore")


# ─────────────────────────────────────────────────────────────────────────────
# Public entry point
# ─────────────────────────────────────────────────────────────────────────────

def fit_psd_segur(
    low_size,
    high_size,
    bin_count,
    min_bins=3,
    min_r2=0.90,
    residual_threshold=0.0,
    max_low_size_um=5000.0,
    lambda_lmax=1.0,
    mp_num_low=1.0,
    mp_num_high=5000.0,
    filter_before_regression=False,
    shape_factor=float("nan"),
    dimension=float("nan"),
):
    """
    Fit a C-PSD power law to one binned PSD using the Segur 2026 algorithm.

    Parameters
    ----------
    low_size, high_size, bin_count : array-like
        Lower bin bounds (µm), upper bin bounds (µm), and counts (or
        concentrations) per bin.  Bins with count == 0 are ignored.
    min_bins : int
        Minimum window length required for a valid fit (default 3).
    min_r2 : float
        Minimum R² for a window to be accepted (default 0.90).
    residual_threshold : float
        Normalised-residual cutoff for Step 1 candidate selection.
        0 = keep only positive-residual bins (default, conservative).
    max_low_size_um : float
        Hard upper size cut applied to Low_size (default 5000 µm).
    lambda_lmax : float
        Coefficient applied to Low_size of BN-PSD peak bin to set the
        hard lower size cut (default 1.0, i.e. bins below the peak
        are always excluded).
    mp_num_low, mp_num_high : float
        Integration limits for MP# and mass extrapolation (µm).
    filter_before_regression : bool
        If True, apply size limits BEFORE the global Step-1 regression.
    shape_factor, dimension : float
        Volume coefficient and shape dimension used for mass calculation.
        Pass NaN to skip mass computation.

    Returns
    -------
    dict with keys:
        lod_low_um, lod_high_um       – detected LOD window (µm)
        slope_C_PSD, intercept_C_PSD  – C-PSD regression parameters
        se_slope_C_PSD                – standard error of C-PSD slope
        r2, n_bins_used               – fit quality
        slope_BN_PSD, intercept_BN_PSD – BN-PSD parameters
        mp_num, mp_mass               – extrapolated concentrations
        fit_mode                      – "ok" | "no_fit" | "too_few_bins"
    """
    params = dict(
        MIN_BINS=min_bins,
        MIN_R2=min_r2,
        RESIDUAL_THRESHOLD=residual_threshold,
        MAX_LOW_SIZE_UM=max_low_size_um,
        LAMBDA_LMAX=lambda_lmax,
        FILTER_BEFORE_REGRESSION=filter_before_regression,
    )

    df_psd = pd.DataFrame({
        "Low_size":          np.asarray(low_size,  dtype=float),
        "High_size":         np.asarray(high_size, dtype=float),
        "Bin_concentration": np.asarray(bin_count, dtype=float),
    })
    df_psd = df_psd[df_psd["Bin_concentration"] > 0].copy()

    if len(df_psd) < min_bins:
        return _empty_result("too_few_bins")

    lod_low, lod_high, fit = _detect_lod(df_psd, params)

    if fit is not None:
        slope_bn, intercept_bn = _cpsd_to_bnpsd(fit["slope"], fit["intercept"])
        mp_num  = _compute_mp_number(slope_bn, intercept_bn, mp_num_low, mp_num_high)
        mp_mass = _compute_mp_mass(slope_bn, intercept_bn,
                                   float(shape_factor), float(dimension),
                                   mp_num_low, mp_num_high)
        fit_mode = "ok"
    else:
        slope_bn = intercept_bn = mp_num = mp_mass = float("nan")
        fit_mode = "no_fit"

    return {
        "lod_low_um":        float(lod_low)  if np.isfinite(float(lod_low))  else float("nan"),
        "lod_high_um":       float(lod_high) if np.isfinite(float(lod_high)) else float("nan"),
        "slope_C_PSD":       fit["slope"]     if fit else float("nan"),
        "intercept_C_PSD":   fit["intercept"] if fit else float("nan"),
        "se_slope_C_PSD":    fit["se_slope"]  if fit else float("nan"),
        "r2":                fit["r2"]        if fit else float("nan"),
        "n_bins_used":       int(fit["n"])    if fit else 0,
        "slope_BN_PSD":      float(slope_bn),
        "intercept_BN_PSD":  float(intercept_bn),
        "mp_num":            float(mp_num),
        "mp_mass":           float(mp_mass),
        "fit_mode":          fit_mode,
    }


# ─────────────────────────────────────────────────────────────────────────────
# Core algorithm — extracted verbatim from PSD_fit.py (Segur et al. 2026 SI 3)
# ─────────────────────────────────────────────────────────────────────────────

def _empty_result(mode):
    nan = float("nan")
    return dict(lod_low_um=nan, lod_high_um=nan,
                slope_C_PSD=nan, intercept_C_PSD=nan, se_slope_C_PSD=nan,
                r2=nan, n_bins_used=0,
                slope_BN_PSD=nan, intercept_BN_PSD=nan,
                mp_num=nan, mp_mass=nan, fit_mode=mode)


def _compute_bn_psd(df):
    df = df.copy()
    df["bin_width"] = df["High_size"] - df["Low_size"]
    df["BN_conc"]   = df["Bin_concentration"] / df["bin_width"]
    return df


def _compute_cpsd(df):
    df = df.sort_values("Low_size").reset_index(drop=True)
    df["cum_conc"] = df["Bin_concentration"][::-1].cumsum()[::-1].values
    return df


def _fit_log_log(df_cpsd, lod_low, lod_high, min_bins):
    mask  = (df_cpsd["Low_size"] >= lod_low) & (df_cpsd["Low_size"] < lod_high)
    valid = df_cpsd[mask & (df_cpsd["cum_conc"] > 0) & (df_cpsd["Low_size"] > 0)]
    if len(valid) < min_bins:
        return None
    log_x = np.log10(valid["Low_size"].values)
    log_y = np.log10(valid["cum_conc"].values)
    slope, intercept, r, _, se = stats.linregress(log_x, log_y)
    return {"slope": slope, "intercept": intercept,
            "r2": r ** 2, "n": len(valid), "se_slope": se}


def _contiguous_groups(positions):
    if len(positions) == 0:
        return
    start = 0
    for i in range(1, len(positions)):
        if positions[i] != positions[i - 1] + 1:
            yield start, i - 1
            start = i
    yield start, len(positions) - 1


def _detect_lod(df_psd, params):
    """Two-step automatic LOD detection (Segur et al. 2026, Sect. 'Systematic LOD determination')."""
    MIN_BINS             = params["MIN_BINS"]
    MIN_R2               = params["MIN_R2"]
    RESIDUAL_THRESHOLD   = params["RESIDUAL_THRESHOLD"]
    MAX_LOW_SIZE_UM      = params["MAX_LOW_SIZE_UM"]
    LAMBDA_LMAX          = params["LAMBDA_LMAX"]
    FILTER_BEFORE_REGRESSION = params["FILTER_BEFORE_REGRESSION"]

    df      = df_psd.sort_values("Low_size").reset_index(drop=True)
    df      = _compute_bn_psd(df)
    df_cpsd = _compute_cpsd(df)

    # Hard lower size cut: bins below the BN-PSD peak are always excluded
    L_max        = df.loc[df["BN_conc"].idxmax(), "Low_size"]
    lod_lo_limit = LAMBDA_LMAX * L_max

    # ── Step 1: global regression → normalised residuals → candidate mask ──
    base_ok = (df_cpsd["Low_size"] > 0) & (df_cpsd["cum_conc"] > 0)
    if FILTER_BEFORE_REGRESSION:
        reg_mask = base_ok & (df_cpsd["Low_size"] >= lod_lo_limit) \
                           & (df_cpsd["Low_size"] <  MAX_LOW_SIZE_UM)
    else:
        reg_mask = base_ok

    valid_reg = df_cpsd[reg_mask]

    if len(valid_reg) < 2:
        cand_mask = (df["Low_size"] >= lod_lo_limit) & \
                    (df["Low_size"] <  MAX_LOW_SIZE_UM)
    else:
        log_x = np.log10(valid_reg["Low_size"].values)
        log_y = np.log10(valid_reg["cum_conc"].values)
        sl, ic, *_ = stats.linregress(log_x, log_y)
        res      = log_y - (sl * log_x + ic)
        res_std  = res.std()
        res_norm = res / res_std if res_std > 0 else res

        pos_in_cpsd      = np.where(reg_mask.values)[0]
        res_arr          = np.full(len(df_cpsd), np.nan)
        res_arr[pos_in_cpsd] = res_norm
        df_cpsd["_res"]  = res_arr
        size_to_res      = df_cpsd.set_index("Low_size")["_res"].to_dict()
        df["_res"]       = df["Low_size"].map(size_to_res)

        cand_mask = (df["_res"]     > RESIDUAL_THRESHOLD) & \
                    (df["Low_size"] >= lod_lo_limit)        & \
                    (df["Low_size"] <  MAX_LOW_SIZE_UM)

    # ── Step 2: contiguous window optimisation ───────────────────────────────
    cand_pos = np.where(cand_mask.values)[0]

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
                win = group[i:j + 1]
                lo  = df.loc[win[0],  "Low_size"]
                hi  = df.loc[win[-1], "High_size"]
                fit = _fit_log_log(df_cpsd, lo, hi, MIN_BINS)
                if fit is None or fit["r2"] < MIN_R2:
                    continue
                windows.append({"lod_low": lo, "lod_high": hi,
                                 "n": fit["n"], "r2": fit["r2"], "fit": fit})

    if not windows:
        # Relax R² constraint and use full fallback range
        if len(fallback_pos) >= MIN_BINS:
            lo  = df.loc[fallback_pos[0],  "Low_size"]
            hi  = df.loc[fallback_pos[-1], "High_size"]
            fit = _fit_log_log(df_cpsd, lo, hi, MIN_BINS)
            if fit is not None:
                windows.append({"lod_low": lo, "lod_high": hi,
                                 "n": fit["n"], "r2": fit["r2"],
                                 "fit": fit, "fallback": True})

    if not windows:
        return np.nan, np.nan, None

    # Select the longest window; break ties by R²
    best = max(windows, key=lambda w: (w["n"], w["r2"]))
    return best["lod_low"], best["lod_high"], best["fit"]


def _cpsd_to_bnpsd(slope_c, intercept_c):
    return slope_c - 1, -slope_c * (10 ** intercept_c)


def _compute_mp_number(slope_bn, intercept_bn, mp_num_low, mp_num_high):
    if not np.isfinite(slope_bn) or not np.isfinite(intercept_bn):
        return float("nan")
    if slope_bn == -1:
        return float("nan")
    return (intercept_bn / (slope_bn + 1)
            * (mp_num_high ** (slope_bn + 1) - mp_num_low ** (slope_bn + 1)))


def _compute_mp_mass(slope_bn, intercept_bn, shape_factor, dimension,
                     mp_num_low, mp_num_high):
    if not np.isfinite(slope_bn) or not np.isfinite(intercept_bn):
        return float("nan")
    if not np.isfinite(shape_factor) or not np.isfinite(dimension):
        return float("nan")
    exponent = 1 + dimension + slope_bn
    if exponent == 0:
        return float("nan")
    return (shape_factor * intercept_bn / exponent
            * (mp_num_high ** exponent - mp_num_low ** exponent))
