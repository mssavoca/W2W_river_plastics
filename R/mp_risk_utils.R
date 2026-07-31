# mp_risk_utils.R
# Reusable functions for probabilistic microplastics risk characterization.
#
# Designed for eventual integration into the PSSDplusplus R package:
#   https://github.com/ScottCoffin/ToMEx2.0_EcoToxRisk/tree/main/package
#
# Key dependencies: tibble, dplyr, truncnorm, stats
#
# References:
#   Segur et al. (2026) <doi:10.1186/s43591-026-00205-5>
#   Coffin et al. (2022) <doi:10.1016/j.scitotenv.2022.155859>
#   Coffin et al. (2026) PSSDplusplus — probabilistic SSD for microplastics


# ── Segur et al. 2026 C-PSD LOD algorithm – pure R implementation ─────────────
# Ported from PSD_fit.py (SI 3); no Python/reticulate dependency.

.segur_compute_cpsd <- function(df) {
  df <- df[order(df$L_low), ]
  df$cum_conc <- rev(cumsum(rev(df$n)))
  df
}

.segur_fit_log_log <- function(df_cpsd, lod_low, lod_high, min_bins) {
  mask  <- df_cpsd$L_low >= lod_low & df_cpsd$L_low < lod_high
  valid <- df_cpsd[mask & df_cpsd$cum_conc > 0 & df_cpsd$L_low > 0, ]
  if (nrow(valid) < min_bins) return(NULL)
  log_x <- log10(valid$L_low)
  log_y <- log10(valid$cum_conc)
  if (stats::var(log_x) == 0 || stats::var(log_y) == 0) return(NULL)
  fit  <- lm(log_y ~ log_x)
  summ <- summary(fit)
  r2   <- summ$r.squared
  if (!is.finite(r2)) return(NULL)
  list(
    slope     = unname(coef(fit)["log_x"]),
    intercept = unname(coef(fit)["(Intercept)"]),
    r2        = r2,
    n         = nrow(valid),
    se_slope  = unname(summ$coefficients["log_x", "Std. Error"])
  )
}

.segur_contiguous_groups <- function(positions) {
  if (length(positions) == 0L) return(list())
  group_id <- cumsum(c(TRUE, diff(positions) != 1L))
  lapply(split(seq_along(positions), group_id), function(idx) positions[idx])
}

# Two-step automatic LOD detection (Segur et al. 2026, Sect. 'Systematic LOD determination').
# df must have columns: L_low, L_high, n  (sorted by L_low or will be sorted internally).
.segur_detect_lod <- function(df, params) {
  MIN_BINS              <- params$MIN_BINS
  MIN_R2                <- params$MIN_R2
  RESIDUAL_THRESHOLD    <- params$RESIDUAL_THRESHOLD
  MAX_LOW_SIZE_UM       <- params$MAX_LOW_SIZE_UM
  LAMBDA_LMAX           <- params$LAMBDA_LMAX
  FILTER_BEFORE_REGRESSION <- params$FILTER_BEFORE_REGRESSION

  df <- df[df$n > 0, ]            # match Python: remove zero-count bins first
  df <- df[order(df$L_low), ]
  df$BN_conc <- df$n / (df$L_high - df$L_low)
  df_cpsd    <- .segur_compute_cpsd(df)

  L_max        <- df$L_low[which.max(df$BN_conc)]
  lod_lo_limit <- LAMBDA_LMAX * L_max

  # ── Step 1: global regression → normalised residuals → candidate mask ──────
  base_ok  <- df_cpsd$L_low > 0 & df_cpsd$cum_conc > 0
  reg_mask <- if (FILTER_BEFORE_REGRESSION) {
    base_ok & df_cpsd$L_low >= lod_lo_limit & df_cpsd$L_low < MAX_LOW_SIZE_UM
  } else {
    base_ok
  }

  valid_reg <- df_cpsd[reg_mask, ]

  if (nrow(valid_reg) < 2L) {
    cand_mask <- df$L_low >= lod_lo_limit & df$L_low < MAX_LOW_SIZE_UM
  } else {
    log_x    <- log10(valid_reg$L_low)
    log_y    <- log10(valid_reg$cum_conc)
    coef1    <- coef(lm(log_y ~ log_x))
    res      <- log_y - (coef1[2] * log_x + coef1[1])
    res_std  <- sd(res)
    res_norm <- if (res_std > 0) res / res_std else res

    res_arr <- rep(NA_real_, nrow(df))
    res_arr[which(reg_mask)] <- res_norm
    df$res_norm <- res_arr

    cand_mask <- !is.na(df$res_norm) &
                 df$res_norm > RESIDUAL_THRESHOLD &
                 df$L_low   >= lod_lo_limit &
                 df$L_low   <  MAX_LOW_SIZE_UM
  }

  # ── Step 2: contiguous window optimisation ───────────────────────────────────
  cand_pos     <- which(cand_mask)
  fallback_pos <- which(df$L_low >= lod_lo_limit & df$L_low < MAX_LOW_SIZE_UM)

  if (length(cand_pos) < MIN_BINS) cand_pos <- fallback_pos
  if (length(cand_pos) == 0L) return(list(lod_low = NA_real_, lod_high = NA_real_, fit = NULL))

  windows <- list()
  for (group in .segur_contiguous_groups(cand_pos)) {
    n_g <- length(group)
    for (i in seq_len(n_g)) {
      j_start <- i + MIN_BINS - 1L
      if (j_start > n_g) break
      for (j in seq(j_start, n_g)) {
        win <- group[i:j]
        lo  <- df$L_low[win[1]]
        hi  <- df$L_high[win[length(win)]]
        f   <- .segur_fit_log_log(df_cpsd, lo, hi, MIN_BINS)
        if (is.null(f) || f$r2 < MIN_R2) next
        windows <- c(windows, list(list(lod_low = lo, lod_high = hi,
                                        n = f$n, r2 = f$r2, fit = f)))
      }
    }
  }

  if (length(windows) == 0L) {
    if (length(fallback_pos) >= MIN_BINS) {
      lo <- df$L_low[fallback_pos[1]]
      hi <- df$L_high[fallback_pos[length(fallback_pos)]]
      f  <- .segur_fit_log_log(df_cpsd, lo, hi, MIN_BINS)
      if (!is.null(f)) windows <- list(list(lod_low = lo, lod_high = hi,
                                             n = f$n, r2 = f$r2, fit = f))
    }
  }

  if (length(windows) == 0L) return(list(lod_low = NA_real_, lod_high = NA_real_, fit = NULL))

  ns   <- sapply(windows, `[[`, "n")
  r2s  <- sapply(windows, `[[`, "r2")
  best <- windows[[order(ns, r2s, decreasing = TRUE)[1]]]
  list(lod_low = best$lod_low, lod_high = best$lod_high, fit = best$fit)
}


# ── C-PSD fitting ─────────────────────────────────────────────────────────────

#' Fit C-PSD power law to raw particle size measurements
#'
#' Bins particle measurements, applies the Segur et al. 2026 two-step LOD
#' detection algorithm (pure R), and returns an lm()-compatible list suitable
#' for plotting and downstream parameter extraction.
#'
#' @param x_um Numeric vector of particle measurements in µm (positive, finite).
#' @param bin_um Bin width in µm (default 10).
#' @param fit_range_um Length-2 vector c(lower, upper) in µm to pre-restrict
#'   the window before the LOD algorithm. Use NA for auto-detection.
#'
#' @return Named list:
#'   data              – data frame of the fitted C-PSD window
#'   bins              – full binned PSD tibble
#'   fit               – lm object (log10 space, for predict() / CI bands)
#'   fit_range_um      – c(lower, upper) LOD bounds in µm
#'   lower_lod_um, lower_lod_used_um, upper_lod_um – LOD scalars (µm)
#'   a_cpsd, b_cpsd    – C-PSD slope and log10-intercept
#'   se_a_cpsd, se_b_cpsd – standard errors
#'   b_cpsd_lin        – linear intercept (10^b_cpsd)
#'   a_psd, se_a_psd, b_psd – differential BN-PSD parameters (a = a_cpsd - 1)
#'   r2, n_bins, fit_mode – fit quality diagnostics
fit_cpsd_segur_r <- function(x_um, bin_um = 10, fit_range_um = c(NA_real_, NA_real_)) {
  x_um <- x_um[is.finite(x_um) & x_um > 0]
  if (length(x_um) < 3) stop("fit_cpsd_segur_r: fewer than 3 valid measurements.")

  min_edge <- floor(min(x_um) / bin_um) * bin_um
  max_edge <- ceiling(max(x_um) / bin_um) * bin_um
  breaks   <- seq(min_edge, max_edge, by = bin_um)
  h        <- hist(x_um, breaks = breaks, plot = FALSE)

  bins <- tibble::tibble(
    L_low     = h$breaks[-length(h$breaks)],
    L_high    = h$breaks[-1],
    L_mid     = h$mids,
    L_geom    = sqrt(h$breaks[-length(h$breaks)] * h$breaks[-1]),
    n         = h$counts,
    bin_width = h$breaks[-1] - h$breaks[-length(h$breaks)],
    bn_psd    = h$counts / (h$breaks[-1] - h$breaks[-length(h$breaks)])
  )

  bins_fit <- bins
  if (!is.na(fit_range_um[1])) bins_fit <- dplyr::filter(bins_fit, L_low >= fit_range_um[1])
  hi_lim <- if (!is.na(fit_range_um[2])) fit_range_um[2] else max(bins$L_high)

  lod <- .segur_detect_lod(
    df = as.data.frame(bins_fit[, c("L_low", "L_high", "n")]),
    params = list(
      MIN_BINS              = 3L,
      MIN_R2                = 0.90,
      RESIDUAL_THRESHOLD    = 0.0,
      MAX_LOW_SIZE_UM       = hi_lim,
      LAMBDA_LMAX           = 1.0,
      FILTER_BEFORE_REGRESSION = FALSE
    )
  )

  fit_mode <- if (is.null(lod$fit)) "no_fit" else "ok"

  # SPEC Q1: N_ge (cumulative count at/above each bin's L_low) is computed over
  # the full uniform bin_um grid so its *values* are unaffected by empty bins,
  # but the regression itself must be restricted to populated (n > 0) bins only
  # -- exactly mirroring PSD_fit.py, whose input CSV never contains zero-count
  # bins to begin with (segur_bins_from_fit() filters n > 0 before export, and
  # Python's __main__ re-filters `Bin_concentration != 0`). Without the `n > 0`
  # filter here, R silently regresses over every empty 5-um grid cell inside
  # the LOD window (flat N_ge step-function points), inflating n_bins and
  # biasing a_cpsd relative to Python's populated-bins-only fit.
  df_full <- bins |>
    dplyr::arrange(L_low) |>
    dplyr::mutate(N_ge = rev(cumsum(rev(n)))) |>
    dplyr::filter(L_low > 0, N_ge > 0, n > 0)

  df_fit <- df_full |>
    dplyr::filter(L_low >= lod$lod_low, L_low < lod$lod_high)

  fit_lm   <- lm(log10(N_ge) ~ log10(L_low), data = df_fit)
  fit_summ <- summary(fit_lm)
  a_cpsd   <- unname(coef(fit_lm)["log10(L_low)"])
  b_cpsd   <- unname(coef(fit_lm)["(Intercept)"])

  list(
    data              = df_fit,
    bins              = bins,
    fit               = fit_lm,
    fit_range_um      = c(lod$lod_low, lod$lod_high),
    lower_lod_um      = lod$lod_low,
    lower_lod_used_um = lod$lod_low,
    upper_lod_um      = lod$lod_high,
    a_cpsd            = a_cpsd,
    b_cpsd            = b_cpsd,
    se_a_cpsd         = unname(fit_summ$coefficients["log10(L_low)", "Std. Error"]),
    se_b_cpsd         = unname(fit_summ$coefficients["(Intercept)",  "Std. Error"]),
    b_cpsd_lin        = 10^b_cpsd,
    a_psd             = a_cpsd - 1,
    se_a_psd          = unname(fit_summ$coefficients["log10(L_low)", "Std. Error"]),
    b_psd             = -(a_cpsd) * (10^b_cpsd),
    r2                = fit_summ$r.squared,
    n_bins            = nrow(df_fit),
    fit_mode          = fit_mode
  )
}


# ── Particle morphology helpers ───────────────────────────────────────────────

#' Compute particle volume from shape and dimensions (vectorized)
#'
#' Fragment: ellipsoid V = (π/6) · L · W · H
#' Fiber:    cylinder  V = π · (W/2)² · L
#'
#' @param shape Character vector: "fragment" or "fiber".
#' @param length_um,width_um,height_um Numeric vectors in µm.
#' @return Numeric vector of volumes in µm³ (NA for unknown shapes).
volume_particle <- function(shape, length_um, width_um, height_um) {
  dplyr::case_when(
    shape == "fragment" ~ (pi / 6) * length_um * width_um * height_um,
    shape == "fiber"    ~ pi * (width_um / 2)^2 * length_um,
    TRUE                ~ NA_real_
  )
}

#' Derive volume-fit LOD bounds from length LOD bounds and shape geometry
#'
#' Converts each shape's length-based C-PSD LOD window into a volume-based LOD
#' window using the *measured* width/length ratio for that shape (`r_med`,
#' typically `median(W/L)` from the same ratio table used to compute particle
#' volume), rather than assuming an isotropic (L=W=H) particle. An isotropic
#' assumption is a rough approximation for fragments and badly wrong for
#' fibers (aspect ratio >= 3 means width is a small fraction of length), which
#' can produce a volume window containing zero real fiber particles for
#' small/elongated samples. The "all" (pooled shapes) window is the union of
#' the fragment and fiber windows, so the fit range used for the pooled volume
#' fit stays consistent with what's actually reliably measured for each shape.
#'
#' @param cpsd_fit_frag,cpsd_fit_fiber fit_cpsd_segur_r() outputs for length,
#'   by shape (uses $lower_lod_um / $upper_lod_um).
#' @param r_med_frag,r_med_fiber Median W/L ratio for each shape (e.g. the
#'   `r_med` column of the ratio_tbl computed alongside particle volume).
#' @return Named list of length-2 numeric vectors c(lower, upper), in µm³:
#'   fragment, fiber, all (union of fragment/fiber ranges).
volume_lod_bounds <- function(cpsd_fit_frag, cpsd_fit_fiber, r_med_frag, r_med_fiber) {
  frag_bounds <- c(
    volume_particle("fragment",
      cpsd_fit_frag$lower_lod_um, r_med_frag * cpsd_fit_frag$lower_lod_um, r_med_frag^2 * cpsd_fit_frag$lower_lod_um),
    volume_particle("fragment",
      cpsd_fit_frag$upper_lod_um, r_med_frag * cpsd_fit_frag$upper_lod_um, r_med_frag^2 * cpsd_fit_frag$upper_lod_um)
  )
  fiber_bounds <- c(
    volume_particle("fiber",
      cpsd_fit_fiber$lower_lod_um, r_med_fiber * cpsd_fit_fiber$lower_lod_um, r_med_fiber * cpsd_fit_fiber$lower_lod_um),
    volume_particle("fiber",
      cpsd_fit_fiber$upper_lod_um, r_med_fiber * cpsd_fit_fiber$upper_lod_um, r_med_fiber * cpsd_fit_fiber$upper_lod_um)
  )
  list(
    fragment = frag_bounds,
    fiber    = fiber_bounds,
    all      = c(min(frag_bounds[1], fiber_bounds[1]), max(frag_bounds[2], fiber_bounds[2]))
  )
}

#' Bootstrap aspect ratio and derive length:width summary
#'
#' Returns inverse aspect ratios (W/L), their bootstrap mean and SD,
#' and a summary tibble. Used to parameterise H_W_ratio in PSSDplusplus.
#'
#' @param aspect_ratio_vec Numeric vector of raw aspect ratios (L/W), positive finite.
#' @param n_boot Number of bootstrap replicates.
#' @return List with:
#'   vals      – W/L values (1/aspect_ratio), positive finite
#'   boot_mean – bootstrap mean of mean(vals)
#'   boot_sd   – bootstrap SD of mean(vals)
#'   summary   – tibble with quantiles and bootstrap stats
bootstrap_aspect_ratio <- function(aspect_ratio_vec, n_boot = 100) {
  vals <- aspect_ratio_vec[is.finite(aspect_ratio_vec) & aspect_ratio_vec > 0]
  vals <- 1 / vals
  boot <- replicate(n_boot, mean(sample(vals, size = length(vals), replace = TRUE)))
  list(
    vals      = vals,
    boot_mean = mean(boot),
    boot_sd   = sd(boot),
    summary   = tibble::tibble(
      p05       = quantile(vals, 0.05),
      p50       = quantile(vals, 0.50),
      p95       = quantile(vals, 0.95),
      mean      = mean(vals),
      sd        = sd(vals),
      boot_mean = mean(boot),
      boot_sd   = sd(boot)
    )
  )
}


# ── Alpha (power-law slope) distribution ─────────────────────────────────────

#' Sample truncated-normal distribution for power-law alpha
#'
#' Represents uncertainty in the C-PSD slope as a truncated normal distribution,
#' following the probabilistic approach in Coffin et al. (2022).
#'
#' @param mu Mean (fitted C-PSD slope or BN-PSD slope, typically negative).
#' @param sd Standard deviation (regression SE or user-supplied).
#' @param n Number of Monte Carlo draws.
#' @param lower,upper Truncation bounds (default −6, −1.1). Ensure integrals converge.
#' @return Numeric vector of length n.
alpha_dist <- function(mu, sd, n = 10000, lower = -6, upper = -1.1, structural_sd = 0) {
  total_sd <- sqrt(sd^2 + structural_sd^2)
  truncnorm::rtruncnorm(n, a = lower, b = upper, mean = mu, sd = total_sd)
}

#' Convert cumulative C-PSD slope to differential PSD slope
#'
#' Segur-style C-PSD fits model N(>=L) proportional to L^a_cpsd. The
#' corresponding differential PSD exponent used by correction_factor() is
#' a_psd = a_cpsd - 1.
cpsd_to_differential_slope <- function(a_cpsd) {
  a_cpsd - 1
}


# ── Exposure rescaling ────────────────────────────────────────────────────────

#' Power-law rescaling correction factor
#'
#' Converts measured concentrations (reported over one size range) to a target
#' size range using the integrated differential PSD (Coffin et al. 2022,
#' doi:10.1016/j.scitotenv.2022.155859).
#'
#' CF = [∫_{L_tar_min}^{L_tar_max} k L^a dL] / [∫_{L_meas_min}^{L_meas_max} k L^a dL]
#'    = (L_tar_max^{a+1} - L_tar_min^{a+1}) / (L_meas_max^{a+1} - L_meas_min^{a+1})
#'
#' @param a BN-PSD slope (numeric scalar or vector; must be < −1 for convergence).
#' @param L_meas_min,L_meas_max Measured size range in µm.
#' @param L_tar_min,L_tar_max Target size range in µm.
#' @return Correction factor (same length as a).
correction_factor <- function(a, L_meas_min, L_meas_max, L_tar_min, L_tar_max,
                              slope_convention = c("differential", "cumulative")) {
  slope_convention <- match.arg(slope_convention)
  if (identical(slope_convention, "cumulative")) {
    a <- cpsd_to_differential_slope(a)
  }
  stopifnot(all(a < -1, na.rm = TRUE))
  num <- (L_tar_max^(a + 1) - L_tar_min^(a + 1)) / (a + 1)
  den <- (L_meas_max^(a + 1) - L_meas_min^(a + 1)) / (a + 1)
  num / den
}

#' Polymer density lookup used for particle-level density sensitivity
#'
#' Values are screening defaults in g/cm3 for broad FTIR library classes. They
#' are intentionally editable inputs rather than hidden constants.
polymer_density_lookup <- function() {
  tibble::tribble(
    ~pattern, ~polymer_group, ~density_g_cm3, ~source_note,
    "poly\\(ethylene\\)|polyethylene", "polyethylene", 0.94, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "poly\\(propylene\\)|polypropylene", "polypropylene", 0.90, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polystyrene|styrene", "styrenic polymers", 1.05, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "poly\\(tetrafluoroethylene\\)|ptfe", "PTFE", 2.20, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "poly\\(esters|terephthalate|polyester", "polyester/PET", 1.38, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polycarbonates", "polycarbonate", 1.20, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polyurethanes", "polyurethane", 1.20, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polyamides|poly\\(acrylamide", "polyamide/acrylamide", 1.14, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polyacryl|polymethacryl", "acrylic polymers", 1.18, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polyvinylalcohol|polyvinyl", "vinyl polymers", 1.25, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polyhaloolefins|vinylhalides", "halogenated vinyl polymers", 1.35, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "polysiloxanes", "silicone polymers", 1.05, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505",
    "cellulose", "cellulose derivatives", 1.50, "Hidalgo-Ruz et al. (2012) Table 2, doi:10.1021/es2031505"
  )
}

#' Assign polymer-resolved density and buoyancy flags
assign_polymer_density <- function(df, polymer_col = "material_class",
                                   lookup = polymer_density_lookup(),
                                   default_density_g_cm3 = 1.10,
                                   freshwater_density_g_cm3 = 1.00,
                                   seawater_density_g_cm3 = 1.025) {
  stopifnot(is.data.frame(df), polymer_col %in% names(df))
  polymer <- tolower(as.character(df[[polymer_col]]))
  density <- rep(default_density_g_cm3, length(polymer))
  group <- rep("default_fixed_1.10", length(polymer))
  note <- rep("Fallback fixed density retained for unmatched polymer class", length(polymer))

  for (i in seq_len(nrow(lookup))) {
    hit <- grepl(lookup$pattern[i], polymer, perl = TRUE) & group == "default_fixed_1.10"
    density[hit] <- lookup$density_g_cm3[i]
    group[hit] <- lookup$polymer_group[i]
    note[hit] <- lookup$source_note[i]
  }

  dplyr::mutate(
    df,
    polymer_density_group = group,
    density_g_cm3 = density,
    density_source_note = note,
    buoyancy_freshwater = dplyr::case_when(
      density_g_cm3 < freshwater_density_g_cm3 ~ "buoyant",
      density_g_cm3 > freshwater_density_g_cm3 ~ "settling",
      TRUE ~ "neutral"
    ),
    buoyancy_marine = dplyr::case_when(
      density_g_cm3 < seawater_density_g_cm3 ~ "buoyant",
      density_g_cm3 > seawater_density_g_cm3 ~ "settling",
      TRUE ~ "neutral"
    )
  )
}

#' Estimate Hoffman-style critical size for transport distortion
estimate_critical_size <- function(delta_m = 0.10, K_m2_s = 1e-5,
                                   density_g_cm3 = 1.10,
                                   water_density_g_cm3 = 1.00,
                                   viscosity_pa_s = 0.001,
                                   diameter_bounds_um = c(1, 5000)) {
  rho_p <- density_g_cm3 * 1000
  rho_w <- water_density_g_cm3 * 1000
  f <- function(d_um) {
    d_m <- d_um * 1e-6
    ws <- abs((rho_p - rho_w) * 9.80665 * d_m^2 / (18 * viscosity_pa_s))
    ws * delta_m / K_m2_s - 1
  }
  vals <- f(diameter_bounds_um)
  if (!all(is.finite(vals)) || vals[1] * vals[2] > 0) return(NA_real_)
  uniroot(f, interval = diameter_bounds_um)$root
}

#' Fit a two-segment diagnostic to a fitted C-PSD window
piecewise_cpsd_diagnostic <- function(cpsd_fit, min_bins_per_segment = 3) {
  df <- cpsd_fit$data
  if (is.null(df) || nrow(df) < (2 * min_bins_per_segment + 1)) {
    return(tibble::tibble(
      break_um = NA_real_, slope_low = NA_real_, slope_high = NA_real_,
      aic_single = stats::AIC(cpsd_fit$fit), aic_piecewise = NA_real_,
      delta_aic = NA_real_
    ))
  }
  df <- dplyr::mutate(df, log_L = log10(L_low), log_N = log10(N_ge))
  candidates <- df$L_low[(min_bins_per_segment + 1):(nrow(df) - min_bins_per_segment)]
  fits <- lapply(candidates, function(brk) {
    dat <- dplyr::mutate(df, segment = ifelse(L_low < brk, "low", "high"))
    fit <- stats::lm(log_N ~ log_L * segment, data = dat)
    co <- stats::coef(fit)
    high_term <- ifelse("log_L:segmentlow" %in% names(co), co["log_L:segmentlow"], 0)
    tibble::tibble(
      break_um = brk,
      slope_low = unname(co["log_L"] + high_term),
      slope_high = unname(co["log_L"]),
      aic_piecewise = stats::AIC(fit)
    )
  })
  best <- dplyr::bind_rows(fits) |> dplyr::slice_min(aic_piecewise, n = 1, with_ties = FALSE)
  dplyr::mutate(
    best,
    aic_single = stats::AIC(cpsd_fit$fit),
    delta_aic = aic_piecewise - aic_single
  )
}

#' Piecewise (dual-slope) power-law rescaling correction factor
#'
#' Bounding-sensitivity counterpart to correction_factor() (SPEC 2b). Rather
#' than extrapolating a single power-law slope across the full target range,
#' integrates dN/dL = k * L^a using the fine-size segment's slope below
#' `break_um` and the coarse-size segment's slope at/above `break_um`, with
#' the coarse segment's density scaled so dN/dL is continuous at `break_um`
#' (i.e., the two segments meet exactly at the break, not just piecewise in
#' isolation). Both the target-range and measured-range integrals are
#' evaluated with this same piecewise density, so segments that straddle
#' `break_um` are split automatically and segments that fall entirely on one
#' side reduce to a single-slope integral.
#'
#' The 1 um-to-LOD_low sub-window below the fitted C-PSD window is
#' unconstrained by data in both the single-slope and piecewise case; the
#' piecewise result is a labeled bounding sensitivity that quantifies
#' structural (model-form) uncertainty in how that sub-window is
#' extrapolated (Hoffman et al. 2026), not a replacement for the single-slope
#' production correction factor.
#'
#' @param a_low,a_high Fine- and coarse-segment slopes (numeric scalar or
#'   vector; must be < -1 after conversion to the differential convention).
#' @param break_um Break point between the fine and coarse segments (um).
#' @param L_meas_min,L_meas_max Measured size range in um.
#' @param L_tar_min,L_tar_max Target size range in um.
#' @param slope_convention "differential" (default; a_low/a_high are already
#'   BN-PSD slopes) or "cumulative" (a_low/a_high are C-PSD slopes and will be
#'   converted via a_psd = a_cpsd - 1 before integrating).
#' @return Correction factor (same length as a_low/a_high).
correction_factor_piecewise <- function(a_low, a_high, break_um,
                                        L_meas_min, L_meas_max,
                                        L_tar_min, L_tar_max,
                                        slope_convention = c("differential", "cumulative")) {
  slope_convention <- match.arg(slope_convention)
  if (identical(slope_convention, "cumulative")) {
    a_low  <- cpsd_to_differential_slope(a_low)
    a_high <- cpsd_to_differential_slope(a_high)
  }
  stopifnot(all(a_low < -1, na.rm = TRUE), all(a_high < -1, na.rm = TRUE), all(break_um > 0, na.rm = TRUE))

  k_low  <- 1
  k_high <- k_low * break_um^(a_low - a_high)   # density-continuity at break_um

  seg_integral <- function(lo, hi, a, k) k * (hi^(a + 1) - lo^(a + 1)) / (a + 1)

  integrate_window <- function(lo, hi) {
    lo_low  <- pmin(lo, break_um); hi_low  <- pmin(hi, break_um)
    lo_high <- pmax(lo, break_um); hi_high <- pmax(hi, break_um)
    seg_integral(lo_low, hi_low, a_low, k_low) + seg_integral(lo_high, hi_high, a_high, k_high)
  }

  num <- integrate_window(L_tar_min, L_tar_max)
  den <- integrate_window(L_meas_min, L_meas_max)
  num / den
}


# ── Environmental Exposure Distribution ───────────────────────────────────────

#' Nonparametric bootstrap of EED percentiles
#'
#' Resamples corrected concentrations with replacement and tracks the
#' distribution of requested quantiles across replicates.
#'
#' @param x Numeric vector of concentrations (positive, finite).
#' @param n_boot Number of bootstrap replicates.
#' @param probs Quantile levels to track (default c(0.5, 0.95)).
#' @return Data frame with one column per probability (named q50, q95, etc.).
bootstrap_eed <- function(x, n_boot = 5000, probs = c(0.5, 0.95)) {
  x <- x[is.finite(x) & x > 0]
  stopifnot(length(x) >= 5)
  boot_stats <- replicate(n_boot, {
    xb <- sample(x, size = length(x), replace = TRUE)
    as.numeric(quantile(xb, probs = probs, names = FALSE))
  })
  boot_df <- as.data.frame(t(boot_stats))
  names(boot_df) <- paste0("q", probs * 100)
  boot_df
}

#' Bootstrap ECDF confidence bands
#'
#' Returns median and quantile bounds of the empirical CDF evaluated at a
#' common x-grid. Used to visualise uncertainty in exposure and hazard CDFs.
#'
#' @param x Numeric vector (concentrations or PNEC values).
#' @param grid Numeric vector of x-axis evaluation points.
#' @param n_boot Number of bootstrap replicates.
#' @param probs Three-element vector: lower, median, upper CDF quantiles.
#' @return Tibble with columns x, cdf_lo, cdf_med, cdf_hi.
ecdf_bands <- function(x, grid, n_boot = 100, probs = c(0.025, 0.5, 0.975)) {
  x <- x[is.finite(x)]
  stopifnot(length(x) >= 5)
  boot <- replicate(n_boot, {
    xb <- sample(x, size = length(x), replace = TRUE)
    stats::ecdf(xb)(grid)
  })
  tibble::tibble(
    x       = grid,
    cdf_lo  = apply(boot, 1, quantile, probs = probs[1], names = FALSE),
    cdf_med = apply(boot, 1, quantile, probs = probs[2], names = FALSE),
    cdf_hi  = apply(boot, 1, quantile, probs = probs[3], names = FALSE)
  )
}


# ── Risk characterization ─────────────────────────────────────────────────────

#' 1D Monte Carlo risk quotient distribution
#'
#' Pairs random draws of exposure and hazard (PNEC) to generate a distribution
#' of risk quotients (RQ = Exposure / Hazard) for each ERM × HCx combination.
#'
#' @param haz_df Data frame with columns ERM, HCx, PNEC.
#' @param exposure_draws Numeric vector of exposure concentration draws (particles/L).
#' @param n_risk Number of paired draws per ERM × HCx group.
#' @return Grouped tibble with columns ERM, HCx, E, H, RQ.
draw_rq_mc1d <- function(haz_df, exposure_draws, n_risk = 1000) {
  exposure_draws <- exposure_draws[is.finite(exposure_draws) & exposure_draws > 0]
  stopifnot(length(exposure_draws) >= 5)
  haz_df |>
    dplyr::group_by(ERM, HCx) |>
    dplyr::group_modify(~ {
      tibble::tibble(
        E  = sample(exposure_draws, n_risk, replace = TRUE),
        H  = sample(.x$PNEC, n_risk, replace = TRUE)
      ) |> dplyr::mutate(RQ = E / H)
    }) |>
    dplyr::ungroup()
}

#' Summarize risk quotient distribution
#'
#' @param risk_draws Output of draw_rq_mc1d().
#' @return Summary tibble with P_exceed, RQ_p50, RQ_p95, RQ_p99 per ERM × HCx.
summarize_rq <- function(risk_draws) {
  risk_draws |>
    dplyr::group_by(ERM, HCx) |>
    dplyr::summarise(
      P_exceed = mean(RQ > 1, na.rm = TRUE),
      RQ_p50   = median(RQ, na.rm = TRUE),
      RQ_p95   = quantile(RQ, 0.95, na.rm = TRUE),
      RQ_p99   = quantile(RQ, 0.99, na.rm = TRUE),
      .groups  = "drop"
    )
}

#' Two-dimensional Monte Carlo risk characterization
#'
#' Separates uncertainty (outer loop: correction factors + one hazard draw)
#' from variability (inner loop: measured concentrations across sites/times).
#'
#' Outer loop: n_uncertainty iterations, each sampling one CF draw and one PNEC.
#' Inner loop: n_variability exposure draws per outer iteration.
#'
#' @param monitoring_df Data frame with column C_measured_pL (particles/L, positive).
#' @param combined_cf Numeric vector of combined correction factor draws.
#' @param haz_df Data frame with columns ERM, HCx, PNEC.
#' @param n_uncertainty Number of outer (uncertainty) iterations.
#' @param n_variability Number of inner (variability) draws per outer iteration.
#' @param seed Integer seed for reproducibility (NULL to skip).
#' @return Tibble with columns ERM, HCx, iter_u, cf, H, P_exceed, RQ_p50, RQ_p95, RQ_p99.
mc2d_risk <- function(monitoring_df,
                      combined_cf,
                      haz_df,
                      n_uncertainty = 300,
                      n_variability = 1000,
                      seed = 1) {
  if (!is.null(seed)) set.seed(seed)
  measured <- monitoring_df$C_measured_pL
  measured <- measured[is.finite(measured) & measured > 0]
  stopifnot(length(measured) >= 5)

  cf_unc     <- sample(combined_cf, size = n_uncertainty, replace = TRUE)
  haz_groups <- haz_df |> dplyr::group_by(ERM, HCx) |> dplyr::group_split()

  dplyr::bind_rows(lapply(seq_len(n_uncertainty), function(i) {
    cf_i  <- cf_unc[i]
    E_var <- sample(measured, size = n_variability, replace = TRUE) * cf_i
    dplyr::bind_rows(lapply(haz_groups, function(hg) {
      H_i <- sample(hg$PNEC, size = 1)
      rq  <- E_var / H_i
      tibble::tibble(
        ERM      = unique(hg$ERM),
        HCx      = unique(hg$HCx),
        iter_u   = i,
        cf       = cf_i,
        H        = H_i,
        P_exceed = mean(rq > 1),
        RQ_p50   = stats::median(rq),
        RQ_p95   = stats::quantile(rq, 0.95),
        RQ_p99   = stats::quantile(rq, 0.99)
      )
    }))
  }))
}


# ── Sensitivity analysis helpers ─────────────────────────────────────────────

#' Extract Morris parameter bounds from a PSSDplusplus param_values data frame
#'
#' Scans for mean/SD column pairs and computes min/max as mean ± k·SD.
#' Pairs with SD = 0 or non-finite values are excluded.
#'
#' @param param_values One-row data frame (e.g., PSSDplusplus::param_default_values).
#' @param k SD multiplier for bounds (default 2, giving ±2 SD range).
#' @return Tibble with columns param, mean, sd, min, max.
param_bounds <- function(param_values, k = 2) {
  pv      <- param_values[1, , drop = FALSE]
  nm      <- names(pv)
  sd_nms  <- nm[grepl("\\.sd$", nm)]
  base_nm <- sub("\\.sd$", "", sd_nms)
  base_nm <- base_nm[base_nm %in% nm]
  tibble::tibble(
    param = base_nm,
    mean  = as.numeric(pv[1, base_nm]),
    sd    = as.numeric(pv[1, paste0(base_nm, ".sd")])
  ) |>
    dplyr::filter(is.finite(mean), is.finite(sd), sd > 0) |>
    dplyr::mutate(min = mean - k * sd, max = mean + k * sd)
}


# ── Shared per-matrix pipeline helpers ────────────────────────────────────────
# These consolidate steps that are repeated near-identically across the three
# environmental matrices (river water, sediment, ocean water) analyzed in
# probabilistic_risk_characterization.qmd.

#' Fit C-PSD power law separately by shape category
#'
#' Thin wrapper around fit_cpsd_segur_r() that fits "fragment", "fiber", and
#' pooled "all" models from one data frame in a single call, replacing the
#' repeated fragment/fiber/all triplication used for every size metric
#' (length/area/volume) in every matrix (river/sediment/ocean).
#'
#' @param df Data frame containing a shape column and the value column to fit.
#' @param value_col Name of the numeric column to fit (e.g. "length_um",
#'   "area_um2", "V_um3").
#' @param shape_col Name of the shape column (default "shape").
#' @param config Named list with elements "fragment", "fiber", "all", each a
#'   list(bin_um = , fit_range_um = c(NA_real_, NA_real_)) passed through to
#'   fit_cpsd_segur_r(). Names present in `config` determine which
#'   shapes/groups are fitted; "all" fits the full (unfiltered) `value_col`.
#' @return Named list (matching names(config)) of fit_cpsd_segur_r() outputs,
#'   each with a $shape element appended.
fit_cpsd_by_shape <- function(df, value_col, shape_col = "shape", config) {
  stopifnot(is.data.frame(df), value_col %in% names(df), is.list(config))
  out <- lapply(names(config), function(shape_name) {
    cfg  <- config[[shape_name]]
    vals <- if (identical(shape_name, "all")) {
      df[[value_col]]
    } else {
      df[[value_col]][df[[shape_col]] == shape_name]
    }
    c(
      fit_cpsd_segur_r(vals, bin_um = cfg$bin_um, fit_range_um = cfg$fit_range_um),
      list(shape = shape_name)
    )
  })
  stats::setNames(out, names(config))
}

#' Bias-correct monitoring data, apply power-law CF, and bootstrap the EED
#'
#' Shared post-load pipeline step: bias-corrects the measured LOD window
#' against the C-PSD fit, computes the power-law correction_factor(), draws
#' Monte Carlo corrected concentrations, collapses to per-sample medians, and
#' bootstraps the Environmental Exposure Distribution. Matrix-specific
#' monitoring *loading* (query filters, one-off date/QA fixes, unit columns)
#' stays outside this function since it differs meaningfully by matrix.
#'
#' @param monitoring Data frame with columns sample_id, Lmin_measured_um,
#'   Lmax_measured_um, and `conc_col`.
#' @param conc_col Name of the measured-concentration column (e.g. "C_measured_pL").
#' @param cpsd_fit A fit_cpsd_segur_r() output supplying lower_lod_um/upper_lod_um.
#' @param alpha_draws Numeric vector of BN-PSD slope draws (alpha_dist() output).
#' @param L_tar_min,L_tar_max Target size range in µm (default 1, 5000).
#' @param n_draws Monte Carlo draws per monitoring sample (default 3000).
#' @param n_boot Bootstrap replicates for bootstrap_eed().
#' @param probs Quantile levels for bootstrap_eed() (default c(0.5, 0.95)).
#' @return List: monitoring (with bias-corrected LOD columns), combined_cf,
#'   C_corrected_draws (with a generic $C_corrected column), C_sample_median,
#'   eed_boot, L_meas_min_use, L_meas_max_use.
correct_and_bootstrap_eed <- function(monitoring, conc_col, cpsd_fit, alpha_draws,
                                       L_tar_min = 1, L_tar_max = 5000,
                                       n_draws = 3000, n_boot, probs = c(0.5, 0.95),
                                       slope_convention = c("differential", "cumulative")) {
  stopifnot(is.data.frame(monitoring), conc_col %in% names(monitoring))
  slope_convention <- match.arg(slope_convention)

  monitoring <- monitoring |>
    dplyr::mutate(
      Lmin_biascorr_um = pmax(Lmin_measured_um, cpsd_fit$lower_lod_um, na.rm = TRUE),
      Lmax_biascorr_um = pmin(Lmax_measured_um, cpsd_fit$upper_lod_um,  na.rm = TRUE)
    )

  L_meas_min_use <- stats::median(monitoring$Lmin_biascorr_um, na.rm = TRUE)
  L_meas_max_use <- stats::median(monitoring$Lmax_biascorr_um, na.rm = TRUE)

  combined_cf <- correction_factor(
    a          = alpha_draws,
    L_meas_min = L_meas_min_use,
    L_meas_max = L_meas_max_use,
    L_tar_min  = L_tar_min,
    L_tar_max  = L_tar_max,
    slope_convention = slope_convention
  )

  C_corrected_draws <- monitoring |>
    dplyr::mutate(idx = dplyr::row_number()) |>
    tidyr::crossing(draw = seq_len(n_draws)) |>
    dplyr::mutate(
      cf          = sample(combined_cf, size = dplyr::n(), replace = TRUE),
      C_corrected = .data[[conc_col]] * cf
    )

  C_sample_median <- C_corrected_draws |>
    dplyr::group_by(sample_id) |>
    dplyr::summarise(C_corr_med = stats::median(C_corrected), .groups = "drop")

  eed_boot <- bootstrap_eed(C_sample_median$C_corr_med, n_boot = n_boot, probs = probs)

  list(
    monitoring        = monitoring,
    combined_cf       = combined_cf,
    C_corrected_draws = C_corrected_draws,
    C_sample_median   = C_sample_median,
    eed_boot          = eed_boot,
    L_meas_min_use    = L_meas_min_use,
    L_meas_max_use    = L_meas_max_use
  )
}

#' Run the MC_sim_align_parallel() + make_all_pSSDs() pipeline for one matrix
#'
#' Wraps toxicity-data/particle-trait Monte Carlo alignment, ERM-specific
#' results filtering, erm_registry construction, and pSSD++ fitting — the
#' ~90-line block repeated once per matrix (river/sediment/ocean) that differs
#' only in the dose unit (particles/L vs particles/kg for sediment), the
#' `environments` value passed to make_all_pSSDs(), and cache/output dirs.
#'
#' @param tox_data Filtered toxicity data frame for this matrix.
#' @param param_matrix Parameter matrix (matrix_function() output) for this matrix.
#' @param environments Character vector passed to make_all_pSSDs()'s `environments`
#'   argument (e.g. "Freshwater", "Freshwater Sediment", "Marine").
#' @param cache_suffix String appended to the pssd_cache_/pssd_figures_ tempdir
#'   subdirectory names, to keep matrices' caches separate.
#' @param dose_unit "L" (particles/L; river, ocean) or "kg" (particles/kg; sediment).
#' @param n_sim Size of param_matrix / MC_sim_align_parallel's n_sim.
#' @param num_cores Worker count (default parallel::detectCores() - 2). Ignored
#'   internally as of the C1 reproducibility fix (see `seed`) -- execution is
#'   forced sequential (num_cores = 1 / make_all_pSSDs(parallel = FALSE)) so the
#'   run is bit-reproducible under `seed`. PSSDplusplus::MC_sim_align_parallel()'s
#'   `%dopar%` path spins up its own PSOCK cluster with no clusterSetRNGStream(),
#'   so unseeded parallel workers produce non-reproducible hazard draws (HC5,
#'   HC10, and everything downstream) across separate renders; the sequential
#'   `lapply()` path inside MC_sim_align_parallel() (and make_all_pSSDs()) draws
#'   from R's ordinary global RNG stream instead, which `seed` controls exactly.
#' @param sim,cv_uf,rmore_method Passed through to make_all_pSSDs().
#' @param base_tempdir Base directory for cache/output subfolders (default tempdir()).
#' @param seed If not NULL, set.seed(seed) immediately before the Monte Carlo
#'   alignment + pSSD fit, for reproducibility independent of call order.
#' @param x1D_set Lower size-integration bound (um) passed to
#'   MC_sim_align_parallel(). Default 1 preserves prior behavior for every
#'   existing call site; M1.2 passes L_tar_min here to match the exposure-side
#'   extrapolation floor tested in that sensitivity (must be moved together
#'   with the exposure CF's L_tar_min and the tox-record size filter, or
#'   exposure and hazard are no longer on the same size basis).
#' @return List: MC_sim_df, erm_registry, pSSDs.
run_pssd_pipeline <- function(tox_data, param_matrix, environments, cache_suffix,
                               dose_unit = c("L", "kg"),
                               n_sim, num_cores = parallel::detectCores() - 2,
                               sim = 30, cv_uf = 0.5, rmore_method = "lognormal",
                               base_tempdir = tempdir(), seed = NULL, x1D_set = 1) {
  dose_unit <- match.arg(dose_unit)
  food_col   <- paste0("particles_", dose_unit, "_food_dilution")
  tissue_col <- paste0("particles_", dose_unit, "_ox_stress")
  dose_col   <- paste0("dose_new_particles_", dose_unit)

  if (!is.null(seed)) set.seed(seed)

  # PSSDplusplus::MC_sim_align_parallel() writes per-step status via cat()
  # (not message(), so message=FALSE chunk options don't suppress it) --
  # capture.output(..., type="output") swallows that; suppressMessages()
  # covers its separate message()-based lines. The expression is evaluated
  # in this function's own frame (capture.output()'s default behavior), so
  # the assignment below is unaffected.
  invisible(utils::capture.output(
    suppressMessages({
      MC_sim_df <- PSSDplusplus::MC_sim_align_parallel(
        tox_data     = tox_data,
        param_matrix = param_matrix,
        n_sim        = n_sim,
        x1D_set      = x1D_set,
        x2D_set      = 5000,
        num_cores    = 1L
      )
    }),
    type = "output"
  ))

  results_df_food <- dplyr::filter(
    MC_sim_df,
    ingestible != "not ingestible",
    .data[[food_col]] > 0,
    Group != "Algae"
  ) |>
    dplyr::mutate(!!dose_col := .data[[food_col]]) |>
    tidyr::drop_na(dplyr::all_of(food_col))
  results_df_food_t3_t4 <- dplyr::filter(
    results_df_food, risk.13 != 1, bio_f %in% c("Organism", "Population")
  )

  results_df_tissue <- dplyr::filter(
    MC_sim_df,
    translocatable != "not translocatable",
    .data[[tissue_col]] > 0
  ) |>
    dplyr::mutate(!!dose_col := .data[[tissue_col]]) |>
    tidyr::drop_na(dplyr::all_of(tissue_col))
  results_df_tissue_t3_t4 <- dplyr::filter(
    results_df_tissue, risk.13 != 1, bio_f %in% c("Organism", "Population")
  )

  erm_registry <- list(
    "Food Dilution"        = list(base = results_df_food,   t3_t4 = results_df_food_t3_t4),
    "Tissue Translocation" = list(base = results_df_tissue, t3_t4 = results_df_tissue_t3_t4)
  )

  # Same rationale as MC_sim_align_parallel() above: make_all_pSSDs() prints
  # one cat() line per matrix x ERM x HCx combination (Skipping/Completed/
  # ERROR status, a per-combination progress stream) regardless of
  # `progress` (that argument only controls an optional progressr bar on
  # top of this, not this base status stream).
  invisible(utils::capture.output(
    suppressMessages({
      pSSDs <- PSSDplusplus::make_all_pSSDs(
        MC_sim_df        = MC_sim_df,
        environments     = environments,
        erm_registry     = erm_registry,
        sim              = sim,
        cv_uf            = cv_uf,
        rmore_method     = rmore_method,
        parallel         = FALSE,
        workers          = 1L,
        base_cache_dir   = file.path(base_tempdir, paste0("pssd_cache_",   cache_suffix)),
        base_output_path = file.path(base_tempdir, paste0("pssd_figures_", cache_suffix)),
        overwrite_cache  = TRUE
      )
    }),
    type = "output"
  ))

  list(MC_sim_df = MC_sim_df, erm_registry = erm_registry, pSSDs = pSSDs)
}

#' Build a "legacy" (cumulative-slope) copy of a param_values row (SPEC 1b)
#'
#' Returns a copy of `param_values` with the `a.v.<suffix>`, `a.sa.<suffix>`,
#' and `a.m.<suffix>` fields overwritten with the (incorrect) cumulative C-PSD
#' slope `$a_cpsd`, instead of the differential `$a_psd` used by the corrected
#' pipeline. All other parameters (length slope, R.ave, etc.) are left as-is,
#' isolating the volume/area-slope convention as the only difference between
#' the legacy and corrected hazard alignment — used to build the SPEC 1b
#' before/after Food Dilution RQ decomposition table.
#'
#' @param param_values A param_default_values-shaped one-row data frame/tibble
#'   already carrying the corrected (differential) overrides.
#' @param suffix PSSDplusplus environment suffix, e.g. "freshwater", "marine",
#'   "sediment.freshwater".
#' @param cpsd_fit_volume,cpsd_fit_area fit_cpsd_segur_r() outputs for volume/area.
#' @return Copy of param_values with legacy (cumulative-slope) a.v/a.sa/a.m.
legacy_cumulative_av_params <- function(param_values, suffix, cpsd_fit_volume, cpsd_fit_area) {
  pv <- param_values
  pv[[paste0("a.v.", suffix)]]        <- -cpsd_fit_volume$a_cpsd
  pv[[paste0("a.v.", suffix, ".sd")]] <- cpsd_fit_volume$se_a_cpsd
  pv[[paste0("a.sa.", suffix)]]        <- -cpsd_fit_area$a_cpsd
  pv[[paste0("a.sa.", suffix, ".sd")]] <- cpsd_fit_area$se_a_cpsd
  pv[[paste0("a.m.", suffix)]]        <- -cpsd_fit_volume$a_cpsd
  pv[[paste0("a.m.", suffix, ".sd")]] <- cpsd_fit_volume$se_a_cpsd
  pv
}

#' Build combined HC5/HC10 hazard data frame for one matrix/environment
#'
#' Extracts and row-binds the HC5 and HC10 summaries for the Food Dilution and
#' Tissue Translocation ERMs from a make_all_pSSDs() output, tagging each with
#' HCx and ERM columns. Replaces the repeated 4-block
#' haz_HC5_food/haz_HC10_food/haz_HC5_tissue/haz_HC10_tissue construction
#' duplicated once per matrix.
#'
#' @param pSSDs Output of make_all_pSSDs() (or run_pssd_pipeline()$pSSDs).
#' @param environment_key Tier key used to index pSSDs, e.g. "Freshwater",
#'   "Freshwater Sediment", "Marine" — matched against
#'   `Tier3_<environment_key>_Food Dilution` / `...Tissue Translocation`.
#' @return Data frame with HC5 + HC10 rows for both ERMs, columns include HCx, ERM.
build_haz_df <- function(pSSDs, environment_key) {
  food_key   <- paste0("Tier3_", environment_key, "_Food Dilution")
  tissue_key <- paste0("Tier3_", environment_key, "_Tissue Translocation")

  haz_HC5_food    <- pSSDs[[food_key]]$summary_05$df   |> dplyr::mutate(HCx = 5,  ERM = "Food Dilution")
  haz_HC10_food   <- pSSDs[[food_key]]$summary_10$df   |> dplyr::mutate(HCx = 10, ERM = "Food Dilution")
  haz_HC5_tissue  <- pSSDs[[tissue_key]]$summary_05$df |> dplyr::mutate(HCx = 5,  ERM = "Tissue Translocation")
  haz_HC10_tissue <- pSSDs[[tissue_key]]$summary_10$df |> dplyr::mutate(HCx = 10, ERM = "Tissue Translocation")

  dplyr::bind_rows(haz_HC5_food, haz_HC10_food, haz_HC5_tissue, haz_HC10_tissue)
}


# ── SPEC 1e: sediment temporal-fragmentation screens ──────────────────────────

#' Most-sensitive-species screen (SPEC 1e-iv)
#'
#' For each ERM in an erm_registry (as returned by run_pssd_pipeline()), groups
#' the aligned MC records by Species and identifies the single species with the
#' lowest median aligned effect dose -- the most sensitive species -- alongside
#' the number of species considered. Per Thuy-Dung, Groenenberg & Koelmans
#' (2026), individual-species risk can precede a derivable community SSD signal
#' by roughly a decade, so this screen is reported *alongside*, not instead of,
#' the community HC5 (which remains build_haz_df()'s job).
#'
#' @param erm_registry List as returned by run_pssd_pipeline()$erm_registry:
#'   list(`Food Dilution` = list(base=..., t3_t4=...), `Tissue Translocation` = list(...)).
#' @param dose_col Column holding the aligned dose, e.g. "dose_new_particles_kg"
#'   (sediment) or "dose_new_particles_L" (river/ocean) -- matches the
#'   `dose_col` naming already constructed by run_pssd_pipeline().
#' @param records_subset Which erm_registry tier to summarize: "base" (all
#'   aligned records passing the ingestible/translocatable + dose>0 filters) or
#'   "t3_t4" (further restricted to risk.13 tier-3/4, Organism/Population). Default "base".
#' @return Tibble, one row per ERM: ERM, Species (most sensitive), n_records
#'   (records for that species), median_EC (its median aligned dose), n_species
#'   (total distinct species considered for that ERM).
most_sensitive_species <- function(erm_registry, dose_col, records_subset = "base") {
  empty_row <- function(erm_name) {
    tibble::tibble(ERM = erm_name, Species = NA_character_, n_records = 0L,
                   median_EC = NA_real_, n_species = 0L)
  }
  rows <- lapply(names(erm_registry), function(erm_name) {
    df <- erm_registry[[erm_name]][[records_subset]]
    if (is.null(df) || nrow(df) == 0 || !dose_col %in% names(df) || !"Species" %in% names(df)) {
      return(empty_row(erm_name))
    }
    by_species <- df |>
      dplyr::filter(is.finite(.data[[dose_col]]), .data[[dose_col]] > 0) |>
      dplyr::group_by(Species) |>
      dplyr::summarise(median_EC = stats::median(.data[[dose_col]]), n_records = dplyr::n(), .groups = "drop")
    if (nrow(by_species) == 0) return(empty_row(erm_name))
    most_sensitive <- by_species |> dplyr::slice_min(median_EC, n = 1, with_ties = FALSE)
    tibble::tibble(
      ERM       = erm_name,
      Species   = most_sensitive$Species,
      n_records = most_sensitive$n_records,
      median_EC = most_sensitive$median_EC,
      n_species = nrow(by_species)
    )
  })
  dplyr::bind_rows(rows)
}

#' Individual-species risk characterization ratio (SPEC 1e-iv, early-warning companion)
#'
#' RCR = EED / lowest-species EC, as a companion to the community-HC5 RQ. Only
#' meaningful where the exposure concentration (EED) is quantitative (river,
#' ocean) -- deliberately not applied to sediment, whose exposure concentration
#' is non-quantitative (SPEC 1d).
#'
#' @param eed_draws Numeric vector of bootstrap EED draws (e.g. eed_boot$q50).
#' @param lowest_species_EC Single numeric value from most_sensitive_species()$median_EC.
#' @return List: RCR_p50, RCR_p95, P_exceed (fraction of draws with RCR > 1).
individual_species_rcr <- function(eed_draws, lowest_species_EC) {
  if (length(lowest_species_EC) != 1 || !is.finite(lowest_species_EC) || lowest_species_EC <= 0) {
    return(list(RCR_p50 = NA_real_, RCR_p95 = NA_real_, P_exceed = NA_real_))
  }
  rcr_draws <- eed_draws / lowest_species_EC
  list(
    RCR_p50  = stats::median(rcr_draws, na.rm = TRUE),
    RCR_p95  = unname(stats::quantile(rcr_draws, 0.95, na.rm = TRUE)),
    P_exceed = mean(rcr_draws > 1, na.rm = TRUE)
  )
}

#' Sediment PSD-shift sensitivity (SPEC 1e-ii)
#'
#' Illustrative, methods-demonstration sensitivity (gated on
#' `quantitative_flag`): re-runs the full sediment hazard alignment
#' (matrix_function() -> run_pssd_pipeline() -> build_haz_df()) across a grid
#' of progressively finer sediment PSDs, representing continued in-situ
#' fragmentation of deposited microplastics over residence time (Thuy-Dung,
#' Groenenberg & Koelmans 2026). Each grid point is an explicit, cited
#' assumption -- an added shift to the differential length/area/volume/mass
#' slopes -- NOT a fitted fragmentation kinetic rate: Thuy-Dung's k_frag/
#' shell-geometry model is calibrated to polymer-coated-fertilizer prills with
#' 7-year field data and is not transplanted to heterogeneous mixed-polymer
#' sediment MPs of unknown age.
#'
#' A genuine (not merely cosmetic) hazard-side response requires re-running
#' MC_sim_align_parallel()/make_all_pSSDs(), because steepening the PSD shifts
#' the simulated particle-size population that PSSDplusplus's ingestible/
#' translocatable size-gating operates on -- i.e. it can change *which*
#' species clear the bioaccessibility threshold and enter the SSD, not just
#' rescale an existing exposure number. This is why the sensitivity re-runs
#' the full alignment rather than only shifting the CF/EED exposure side.
#'
#' @param param_values_base Baseline param_values_sed (already alpha/a.v/a.sa/
#'   a.m.sediment.freshwater-populated, as built in Section 11.2).
#' @param slope_shift_grid Numeric vector of *added* shifts (positive = finer/
#'   steeper, on the same positive-alpha convention as
#'   param_values_sed$alpha.sediment.freshwater) applied identically to alpha,
#'   a.v, a.sa, and a.m (simplifying assumption: fragmentation shifts all four
#'   size-metric slopes proportionally; noted as such wherever reported).
#' @param tox_data_sed,n_boot,cv_uf,rmore_method,sim,num_cores Passed through
#'   to matrix_function()/run_pssd_pipeline() using the *same* values as the
#'   production sediment call (Section 11.2/11.3) -- no analytical-budget change.
#' @param quantitative_flag The sediment_concentration_is_quantitative flag;
#'   every returned row carries this as `is_quantitative` so callers can label
#'   the table/plot as illustrative (FALSE) vs. production (TRUE) without
#'   re-deriving that decision downstream.
#' @return Tibble: one row per grid point x ERM x HCx, with columns
#'   slope_shift, alpha_sediment_freshwater (post-shift), ERM, HCx, HC5/HC10
#'   value (PNEC, particles/kg dw), and is_quantitative.
sediment_psd_shift_sensitivity <- function(param_values_base, slope_shift_grid,
                                            tox_data_sed, n_boot, cv_uf = 0.5,
                                            rmore_method = "lognormal", sim = 30,
                                            num_cores = parallel::detectCores() - 2,
                                            quantitative_flag = FALSE,
                                            seed_base = 6000) {
  rows <- lapply(seq_along(slope_shift_grid), function(i) {
    delta <- slope_shift_grid[i]
    set.seed(seed_base + i)

    pv <- param_values_base
    pv$alpha.sediment.freshwater <- pv$alpha.sediment.freshwater + delta
    pv$a.v.sediment.freshwater   <- pv$a.v.sediment.freshwater   + delta
    pv$a.sa.sediment.freshwater  <- pv$a.sa.sediment.freshwater  + delta
    pv$a.m.sediment.freshwater   <- pv$a.m.sediment.freshwater   + delta

    param_matrix_shift <- PSSDplusplus::matrix_function(
      n = n_boot,
      params = pv,
      upper.tissue.truncation.limit = 500,
      x1M_set = 1,
      x2D_set = 5000,
      include_marine_surface_water = FALSE,
      include_freshwater_surface_water = FALSE,
      include_marine_sediment = FALSE,
      include_freshwater_sediment = TRUE
    )

    pssd_shift <- run_pssd_pipeline(
      tox_data     = tox_data_sed,
      param_matrix = param_matrix_shift,
      environments = c("Freshwater Sediment"),
      cache_suffix = paste0("sediment_psd_shift_", i),
      dose_unit    = "kg",
      n_sim        = n_boot,
      num_cores    = num_cores,
      sim          = sim,
      cv_uf        = cv_uf,
      rmore_method = rmore_method
    )

    haz_shift <- build_haz_df(pssd_shift$pSSDs, "Freshwater Sediment")

    haz_shift |>
      dplyr::group_by(ERM, HCx) |>
      dplyr::summarise(HC_value_particles_kg = stats::median(PNEC), .groups = "drop") |>
      dplyr::mutate(
        slope_shift                 = delta,
        alpha_sediment_freshwater   = pv$alpha.sediment.freshwater,
        is_quantitative             = quantitative_flag
      )
  })

  dplyr::bind_rows(rows) |>
    dplyr::relocate(slope_shift, alpha_sediment_freshwater, ERM, HCx, HC_value_particles_kg, is_quantitative)
}

#' Filter ToMEx toxicity records to match an exposure-side size floor (M1.2)
#'
#' PSSDplusplus::align_data() (called inside MC_sim_align_parallel()) sizes-
#' aligns each dose using `size.length.um.used.for.conversions` for
#' monodisperse records, or the min/max pair for polydisperse records --
#' confirmed by inspecting align_data()'s source. This filter uses the same
#' field so a record is excluded from a given L_tar_min floor exactly when
#' its own tested size basis would fall below that floor, keeping exposure
#' (the CF's L_tar_min) and hazard (this filter + x1M_set/x1D_set) on a
#' consistent size basis at every floor -- the entire point of M1.2.
#'
#' Filtering rule (documented assumption, per M1.2's spec): monodisperse
#' records are excluded if their single characteristic size is below the
#' floor; polydisperse records are excluded only if their ENTIRE tested range
#' (up to size.length.max.um.used.for.conversions) falls below the floor --
#' i.e. kept if the tested range overlaps or exceeds the floor. Records with
#' a missing/NA characteristic size are excluded (flagged, not imputed).
#'
#' @param tox_data Toxicity data frame (must have polydispersity,
#'   size.length.um.used.for.conversions, size.length.max.um.used.for.conversions).
#' @param L_tar_min Lower size floor (um) to filter to.
#' @return Filtered tox_data (same columns, fewer rows).
filter_tox_by_size_floor <- function(tox_data, L_tar_min) {
  stopifnot(all(c("polydispersity", "size.length.um.used.for.conversions",
                   "size.length.max.um.used.for.conversions") %in% names(tox_data)))
  tox_data |>
    dplyr::mutate(
      .char_size_um = dplyr::case_when(
        polydispersity == "polydisperse" & !is.na(size.length.max.um.used.for.conversions) ~
          size.length.max.um.used.for.conversions,
        polydispersity == "monodisperse" & !is.na(size.length.um.used.for.conversions) ~
          size.length.um.used.for.conversions,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::filter(!is.na(.char_size_um), .char_size_um >= L_tar_min) |>
    dplyr::select(-.char_size_um)
}

#' Locally patched PSSDplusplus::matrix_function() (M1.2 workaround)
#'
#' PSSDplusplus::matrix_function() draws `nrow(mat) * 1.4` candidate values
#' for `upper.tissue.trans.size.um` (via sim_X50 = -sim_beta_0/sim_beta_1),
#' filters to those within (x1M_set, min(x2D_set, upper.tissue.truncation.limit)),
#' and takes the first nrow(mat) survivors with dplyr::slice() -- with no
#' check that enough survived. Production always calls it with x1M_set = 1,
#' where the 1.4x oversample comfortably covers the quota. M1.2 varies
#' x1M_set up to 100 um, which narrows the acceptance window enough that
#' fewer than nrow(mat) values survive; slice() silently returns a short
#' vector, and the subsequent `mat[, upper.tissue.trans.size.um := ...]`
#' errors with a length mismatch ("Supplied N items to be assigned to M
#' items"). Confirmed by direct inspection of matrix_function()'s body
#' (deparse(body(PSSDplusplus::matrix_function))) and by reproducing the
#' failure in isolation at x1M_set = 20-100 with both package defaults and
#' this analysis's real river parameters.
#'
#' Root cause: the 1.4x multiplier is a fixed ratio of nrow(mat), which
#' itself scales with n_sobol * n_params -- so increasing n_sobol does not
#' change the accept/reject *ratio* at all (numerator and denominator scale
#' together). No caller-supplied argument can fix this from the outside.
#'
#' Local-only workaround per user direction (do not modify the installed
#' PSSDplusplus package or its GitHub repo): this function is a full COPY of
#' matrix_function()'s body (via deparse/parse of the installed function,
#' captured once at first call) with only the fixed-oversample block
#' replaced by an iterative draw-until-quota-met loop using the exact same
#' rnorm() parameters and filter conditions -- same statistical model, just
#' guaranteed enough valid draws. The original PSSDplusplus::matrix_function
#' is never altered; this is an independent function object.
#'
#' @param ... Passed through with the same names/semantics as
#'   PSSDplusplus::matrix_function() (n_sobol, params, upper.tissue.truncation.limit,
#'   x1M_set, x2D_set, include_marine_surface_water, include_freshwater_surface_water,
#'   include_marine_sediment, include_freshwater_sediment).
#' @return Same structure as PSSDplusplus::matrix_function()'s output.
.matrix_function_patched_cache <- NULL

matrix_function_safe <- function(...) {
  if (is.null(.matrix_function_patched_cache)) {
    orig_text <- deparse(body(PSSDplusplus::matrix_function))

    broken_start <- grep("sim_beta_0 <- stats::rnorm\\(nrow\\(mat\\) \\* 1\\.4", orig_text)
    broken_end   <- grep("upper\\.tissue\\.trans\\.size\\.um_samples <- as\\.numeric", orig_text)
    stopifnot(
      "matrix_function_safe(): could not locate the expected broken block in PSSDplusplus::matrix_function() -- the package source may have changed; re-inspect deparse(body(PSSDplusplus::matrix_function)) before trusting this patch" =
        length(broken_start) == 1 && length(broken_end) == 1 && broken_end > broken_start
    )

    patched_block <- c(
      "    .mf_target_n <- nrow(mat)",
      "    .mf_samples <- numeric(0)",
      "    .mf_mult <- 1.4",
      "    .mf_attempts <- 0",
      "    while (length(.mf_samples) < .mf_target_n && .mf_attempts < 25) {",
      "      .mf_attempts <- .mf_attempts + 1",
      "      .mf_draw_n <- ceiling(max(.mf_target_n, (.mf_target_n - length(.mf_samples))) * .mf_mult)",
      "      .mf_b0 <- stats::rnorm(.mf_draw_n, mean = params$beta_0, sd = params$se_beta_0)",
      "      .mf_b1 <- stats::rnorm(.mf_draw_n, mean = params$beta_1, sd = params$se_beta_1)",
      "      .mf_x50 <- -.mf_b0 / .mf_b1",
      "      .mf_valid <- .mf_x50[.mf_x50 > x1M_set & .mf_x50 < x2D_set & .mf_x50 < upper.tissue.truncation.limit]",
      "      .mf_samples <- c(.mf_samples, .mf_valid)",
      "      .mf_mult <- .mf_mult * 3",
      "    }",
      "    if (length(.mf_samples) < .mf_target_n) {",
      "      stop(\"matrix_function_safe(): could not draw enough valid upper.tissue.trans.size.um samples after 25 attempts (got \", length(.mf_samples), \" of \", .mf_target_n, \" needed) -- x1M_set may be too close to upper.tissue.truncation.limit for this parameter set.\")",
      "    }",
      "    upper.tissue.trans.size.um_samples <- .mf_samples[seq_len(.mf_target_n)]"
    )

    new_text <- c(orig_text[seq_len(broken_start - 1)], patched_block, orig_text[(broken_end + 1):length(orig_text)])

    new_fn <- eval(parse(text = c("function(n_sobol = 10, params = PSSDplusplus::param_default_values,",
                                   "upper.tissue.truncation.limit = 500, x1M_set = 1, x2D_set = 5000,",
                                   "include_marine_surface_water = TRUE, include_freshwater_surface_water = TRUE,",
                                   "include_marine_sediment = TRUE, include_freshwater_sediment = TRUE)",
                                   new_text)))
    environment(new_fn) <- asNamespace("PSSDplusplus")
    .matrix_function_patched_cache <<- new_fn
    message("matrix_function_safe(): local patched copy of PSSDplusplus::matrix_function() built ",
            "(installed package untouched); see R/mp_risk_utils.R for the diff.")
  }
  .matrix_function_patched_cache(...)
}
