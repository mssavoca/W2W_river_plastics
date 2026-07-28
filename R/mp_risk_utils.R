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

  df_full <- bins |>
    dplyr::arrange(L_low) |>
    dplyr::mutate(N_ge = rev(cumsum(rev(n)))) |>
    dplyr::filter(L_low > 0, N_ge > 0)

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
    "poly\\(ethylene\\)|polyethylene", "polyethylene", 0.94, "TODO(cite): polymer density handbook/default table",
    "poly\\(propylene\\)|polypropylene", "polypropylene", 0.90, "TODO(cite): polymer density handbook/default table",
    "polystyrene|styrene", "styrenic polymers", 1.05, "TODO(cite): polymer density handbook/default table",
    "poly\\(tetrafluoroethylene\\)|ptfe", "PTFE", 2.20, "TODO(cite): polymer density handbook/default table",
    "poly\\(esters|terephthalate|polyester", "polyester/PET", 1.38, "TODO(cite): polymer density handbook/default table",
    "polycarbonates", "polycarbonate", 1.20, "TODO(cite): polymer density handbook/default table",
    "polyurethanes", "polyurethane", 1.20, "TODO(cite): polymer density handbook/default table",
    "polyamides|poly\\(acrylamide", "polyamide/acrylamide", 1.14, "TODO(cite): polymer density handbook/default table",
    "polyacryl|polymethacryl", "acrylic polymers", 1.18, "TODO(cite): polymer density handbook/default table",
    "polyvinylalcohol|polyvinyl", "vinyl polymers", 1.25, "TODO(cite): polymer density handbook/default table",
    "polyhaloolefins|vinylhalides", "halogenated vinyl polymers", 1.35, "TODO(cite): polymer density handbook/default table",
    "polysiloxanes", "silicone polymers", 1.05, "TODO(cite): polymer density handbook/default table",
    "cellulose", "cellulose derivatives", 1.50, "TODO(cite): polymer density handbook/default table"
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
#' @param num_cores Worker count (default parallel::detectCores() - 2).
#' @param sim,cv_uf,rmore_method Passed through to make_all_pSSDs().
#' @param base_tempdir Base directory for cache/output subfolders (default tempdir()).
#' @return List: MC_sim_df, erm_registry, pSSDs.
run_pssd_pipeline <- function(tox_data, param_matrix, environments, cache_suffix,
                               dose_unit = c("L", "kg"),
                               n_sim, num_cores = parallel::detectCores() - 2,
                               sim = 30, cv_uf = 0.5, rmore_method = "lognormal",
                               base_tempdir = tempdir()) {
  dose_unit <- match.arg(dose_unit)
  food_col   <- paste0("particles_", dose_unit, "_food_dilution")
  tissue_col <- paste0("particles_", dose_unit, "_ox_stress")
  dose_col   <- paste0("dose_new_particles_", dose_unit)

  MC_sim_df <- PSSDplusplus::MC_sim_align_parallel(
    tox_data     = tox_data,
    param_matrix = param_matrix,
    n_sim        = n_sim,
    x1D_set      = 1,
    x2D_set      = 5000,
    num_cores    = num_cores
  )

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

  pSSDs <- PSSDplusplus::make_all_pSSDs(
    MC_sim_df        = MC_sim_df,
    environments     = environments,
    erm_registry     = erm_registry,
    sim              = sim,
    cv_uf            = cv_uf,
    rmore_method     = rmore_method,
    parallel         = TRUE,
    workers          = num_cores,
    base_cache_dir   = file.path(base_tempdir, paste0("pssd_cache_",   cache_suffix)),
    base_output_path = file.path(base_tempdir, paste0("pssd_figures_", cache_suffix)),
    overwrite_cache  = TRUE
  )

  list(MC_sim_df = MC_sim_df, erm_registry = erm_registry, pSSDs = pSSDs)
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
