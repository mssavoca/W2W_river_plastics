#
#
#
#
#
#
#
#
#
#
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE, warning = FALSE
)
set.seed(1)
n_boot <- 100
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
pkgs <- c("tidyverse", "truncnorm", "fitdistrplus", "ggpubr", "sensitivity")
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

# pSSD++ package (GitHub)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# install from github unless library(PSSDplusplus) is not already installed
if (!requireNamespace("PSSDplusplus", quietly = TRUE)) {
  devtools::install_github("ScottCoffin/ToMEx2.0_EcoToxRisk", upgrade = "never", subdir = "package", build_vignettes = FALSE)
}
library(PSSDplusplus)
#
#
#
#
#
#
#
#
#
# import river particle data (includes plastics and non-plastics)
raw_particles <- readRDS("data_input/Part_dets_comb.rds") |> 
  dplyr::filter(sample_type == "river water",
                !material_class %in% c("mineral", "organic matter"),
                bad_spectra) %>% 
  # extract the first 8-digit block from the ID
  mutate(date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"), date = ymd(date_raw), # convert YYYYMMDD → Date
         shape = case_when(aspect_ratio >= 3 ~ "fiber", # common definition of fiber - check!
                           aspect_ratio <= 3 ~ "fragment"),
         length_um = max_length_um,
         width_um = min_length_um
  )

raw_particles |> 
  group_by(shape) |> 
  summarize(count = n(),
            mean_length = mean(length_um, na.rm = TRUE),
            min_length = min(length_um, na.rm = TRUE),
            max_length = max(length_um, na.rm = TRUE),
            sd_length = sd(length_um, na.rm = TRUE),
            mean_width = mean(width_um, na.rm = TRUE),
            min_width = min(width_um, na.rm = TRUE),
            max_width = max(width_um, na.rm = TRUE)
            )
#
#
#
#
#
# ---- Thickness (height) factor from measured raw data ----
# Kooi et al. (2022) H = r * W, where r is median(W/L). 
ratio_tbl <- raw_particles |>
  mutate(
    WL = 1/aspect_ratio,
    # area-derived effective width (rectangle proxy); protects against noisy min_length
    width_eff_um = case_when(
      shape == "fragment" & !is.na(area_um2) & !is.na(length_um) & length_um > 0 &
        !is.na(circularity) & circularity < 0.7 ~ area_um2 / length_um,
      TRUE ~ width_um
    )) |> 
  group_by(shape) |>
  summarise(
    n = n(),
    r_med  = median(WL, na.rm = TRUE),            # central estimate
    r_low  = quantile(WL, 0.25, na.rm = TRUE),    # conservative low
    r_high = quantile(WL, 0.75, na.rm = TRUE),    # conservative high
    .groups = "drop"
  )

# quick sanity check: ratio table + summary of volumes
ratio_tbl

# ---- Volumes ----
# Ellipsoid (Kooi-compatible): V = (pi/6) * L * W * H  with H = r * W
# => V = (pi/6) * r * L * W^2
# Cylinder assumption for fibers: V = pi * (W/2)^2 * L

raw_particles <- raw_particles |>
  left_join(ratio_tbl |> dplyr::select(shape, r_med), by = "shape") |>  
  mutate(# thickness in um (assume L/W = H/W)
    height_um = r_med * width_um) |> 

    mutate(# ellipsoid volume in um^3 (primary; Kooi-consistent)
      V_um3  = case_when(shape == "fragment" ~ (pi/6) * length_um * width_um * height_um,
    # fiber cylinder sensitivity in um^3 (only meaningful for fibers)
                        shape == "fiber"    ~ pi * (width_um/2)^2 * length_um,
                        TRUE ~ NA_real_)
                        )

raw_particles |>
  group_by(shape) |>
  summarise(
    n = n(),
    V_um3_median = median(V_um3, na.rm = TRUE),
    V_um3_mean = mean(V_um3, na.rm = TRUE),
    V_um3_sd = sd(V_um3, na.rm = TRUE),
    .groups = "drop"
  )
#
#
#
#
#
#
#
monitoring <- readRDS("data_input/Part_dets_summ.rds") |> 
  filter(sample_type == "river water",
         material_simple == "plastic",
         sample_or_blank == "sample"
  ) %>% 
  mutate(# extract the first 8-digit block from the ID
    date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"),  
    # convert YYYYMMDD → Date
    date = ymd(date_raw),    
    # correct the specific sample date
    date = case_when(
      Client_ID_MSSupdate == "SRR20250303SS" ~ ymd("20250303"),
      TRUE ~ date
    ),
    sample_dets = case_when(Client_ID_MSSupdate %in% c("CRR20231207SS", "CRR20231207SD") ~ "lagoon",
                            Client_ID_MSSupdate %in% c("SRR20231207SS", "SRR20231207SD") ~ "lagoon"),
    # recode depth to subsurface
    sample_depth_general = recode(sample_depth_general,
                                  "depth" = "subsurface"),
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("subsurface", "surface")
    ) 
  ) %>%
  rename(river = sample_location,
         C_measured_pL = extrap_conc_PPL) |> 
          # should adjust based on power law LLOD determined below
  mutate(Lmin_measured_um = 50, 
         Lmax_measured_um = 500,
         sample_id = Client_ID_MSSupdate)

head(monitoring)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
bin_psd <- function(length_um, bin_um = 10) {
  stopifnot(all(is.finite(length_um)))
  length_um <- length_um[length_um > 0]

  # Start bins at the minimum observed size (rounded down) to avoid L=0 in the fit
  min_edge <- floor(min(length_um, na.rm = TRUE) / bin_um) * bin_um
  max_edge <- ceiling(max(length_um, na.rm = TRUE) / bin_um) * bin_um
  breaks <- seq(min_edge, max_edge, by = bin_um)

  h <- hist(length_um, breaks = breaks, plot = FALSE)
  tibble(
    L_low = h$breaks[-length(h$breaks)],
    L_high = h$breaks[-1],
    L_mid = h$mids,
    L_geom = sqrt(h$breaks[-length(h$breaks)] * h$breaks[-1]),
    n = h$counts,
    bin_width = h$breaks[-1] - h$breaks[-length(h$breaks)],
    bn_psd = h$counts / (h$breaks[-1] - h$breaks[-length(h$breaks)])
  )
}

# Bias-corrected lower size bound (MP#max rule)
# If the BN-PSD peak is not in the smallest bin, treat that as evidence of low bias
# and set L_lower_LOD = 2 * geometric mean of the peak bin.
infer_lower_lod <- function(bins) {
  bins2 <- bins |>
    filter(is.finite(L_low), L_low > 0, n > 0, is.finite(bn_psd))

  if (nrow(bins2) == 0) {
    return(list(L_lower_lod_um = NA_real_, bias_detected = FALSE, mpmax_bin = NULL))
  }

  bins2 <- bins2 |> arrange(L_low)
  max_idx <- which.max(bins2$bn_psd)
  bias_detected <- max_idx > 1
  mpmax_bin <- bins2[max_idx, , drop = FALSE]

  L_lower_lod_um <- if (bias_detected) {
    2 * mpmax_bin$L_geom # uses 2x the geomean per the publication. Can try different values
  } else {
    min(bins2$L_low)
  }

  list(
    L_lower_lod_um = as.numeric(L_lower_lod_um),
    bias_detected = bias_detected,
    mpmax_bin = mpmax_bin
  )
}

# Refine lower LOD by detecting under-counting at small sizes
refine_lower_lod_by_resid <- function(df_cpsd,
                                      start_lod,
                                      upper_lod = NA_real_,
                                      min_points = 6,
                                      head_k = 4,
                                      run_k = 3,
                                      z = 1.5) {
  df_cpsd <- df_cpsd |>
    dplyr::arrange(L_low)

  if (!is.finite(upper_lod)) {
    upper_lod <- max(df_cpsd$L_high, na.rm = TRUE)
  }

  candidates <- sort(unique(df_cpsd$L_low[df_cpsd$L_low >= start_lod]))
  if (length(candidates) == 0) {
    return(list(L_lower_lod_um = start_lod, flagged = FALSE))
  }

  for (L_lo in candidates) {
    sub <- df_cpsd |>
      dplyr::filter(L_low >= L_lo, L_high <= upper_lod)
    if (nrow(sub) < min_points) next

    fit <- lm(log10(N_ge) ~ log10(L_low), data = sub)
    r <- resid(fit)
    sigma <- sd(r, na.rm = TRUE)

    sub2 <- sub |>
      dplyr::mutate(resid = r) |>
      dplyr::arrange(L_low)
    head_df <- utils::head(sub2, head_k)

    strong_neg <- head_df$resid < (-z * sigma)
    left_slump <- sum(strong_neg, na.rm = TRUE) >= run_k

    if (!left_slump) {
      return(list(L_lower_lod_um = L_lo, flagged = TRUE))
    }
  }

  list(L_lower_lod_um = max(candidates, na.rm = TRUE), flagged = TRUE)
}

# Upper LOD inference via tail-slump detection in C-PSD residuals
infer_upper_lod <- function(df_cpsd,
                            min_points = 6,
                            tail_k = 4,
                            run_k = 3,
                            z = 1.5,
                            N_min = 10,
                            min_nonzero_tail = 5,
                            require_slope_stability = FALSE,
                            slope_tol = 0.08) {

  df_cpsd <- df_cpsd |>
    dplyr::arrange(L_low)

  if (nrow(df_cpsd) < min_points) {
    return(list(
      L_upper_lod_um = max(df_cpsd$L_high, na.rm = TRUE),
      flagged = FALSE,
      diagnostics = NULL
    ))
  }

  # candidates only from supported bins
  candidates <- sort(unique(df_cpsd$L_high[df_cpsd$n > 0]))
  candidates <- candidates[candidates > min(df_cpsd$L_low, na.rm = TRUE)]
  if (length(candidates) == 0) {
    return(list(
      L_upper_lod_um = max(df_cpsd$L_high, na.rm = TRUE),
      flagged = FALSE,
      diagnostics = NULL
    ))
  }

  results <- vector("list", length(candidates))
  names(results) <- as.character(candidates)
  prev_slope <- NA_real_

  supported_max <- max(df_cpsd$L_high[df_cpsd$N_ge >= N_min], na.rm = TRUE)
  hard_cap <- max(df_cpsd$L_high[df_cpsd$n > 0], na.rm = TRUE)
  supported_max <- min(supported_max, hard_cap, na.rm = TRUE)
  if (!is.finite(supported_max)) supported_max <- hard_cap

  for (i in seq_along(candidates)) {
    L_up <- candidates[length(candidates) - i + 1]  # descending
    if (is.finite(supported_max) && L_up > supported_max) next
    sub <- df_cpsd |> dplyr::filter(L_high <= L_up)

    if (nrow(sub) < min_points) next

    fit <- lm(log10(N_ge) ~ log10(L_low), data = sub)
    r <- resid(fit)
    sigma <- sd(r, na.rm = TRUE)

    sub2 <- sub |>
      dplyr::mutate(resid = r) |>
      dplyr::arrange(L_low)
    tail_df <- utils::tail(sub2, tail_k)
    
    slump_flag <- FALSE
    if (sum(tail_df$n > 0, na.rm = TRUE) < min_nonzero_tail) slump_flag <- TRUE
    strong_neg <- tail_df$resid < (-z * sigma)
    slump_flag <- slump_flag || (sum(strong_neg, na.rm = TRUE) >= run_k)

    slope <- unname(coef(fit)[2])
    stable_flag <- TRUE
    if (require_slope_stability && is.finite(prev_slope)) {
      stable_flag <- abs(slope - prev_slope) <= slope_tol
    }

    results[[as.character(L_up)]] <- list(
      L_up = L_up,
      n = nrow(sub),
      slope = slope,
      intercept = unname(coef(fit)[1]),
      r2 = summary(fit)$r.squared,
      sigma = sigma,
      slump_flag = slump_flag,
      stable_flag = stable_flag
    )

    if (!slump_flag && stable_flag) {
      diag_df <- dplyr::bind_rows(lapply(results, function(x) {
        if (is.null(x)) NULL else as.data.frame(x)
      }))
      return(list(
        L_upper_lod_um = L_up,
        flagged = TRUE,
        diagnostics = diag_df
      ))
    }

    prev_slope <- slope
  }

  fallback <- sort(unique(df_cpsd$L_high), decreasing = TRUE)
  L_fallback <- if (length(fallback) >= 2) fallback[2] else fallback[1]
  diag_df <- dplyr::bind_rows(lapply(results, function(x) {
    if (is.null(x)) NULL else as.data.frame(x)
  }))

  list(
    L_upper_lod_um = L_fallback,
    flagged = TRUE,
    diagnostics = diag_df
  )
}

fit_cpsd <- function(length_um,
                     bin_um = 10,
                     fit_range_um = c(NA, NA),
                     lower_lod_method = c("mpmax", "none"),
                     lower_resid_method = c("none", "left_resid"),
                     upper_lod_method = c("tail_resid", "none"),
                     min_points = 5,
                     upper_min_points = 8,
                     tail_k = 10,
                     run_k = 3,
                     z = 1, # lower is more strict; 1.5 = moderately strict
                     N_min = 10,
                     min_nonzero_tail = 5,
                     N_min_lowbias = 3,
                     min_nonzero_tail_lowbias = 1,
                     lower_tail_k = 4,
                     lower_run_k = 3,
                     lower_z = 1.5,
                     require_slope_stability = TRUE,
                     slope_tol = 0.08) {

  lower_lod_method <- match.arg(lower_lod_method)
  lower_resid_method <- match.arg(lower_resid_method)
  upper_lod_method <- match.arg(upper_lod_method)
  bins <- bin_psd(length_um, bin_um = bin_um)

  lower_lod <- list(L_lower_lod_um = NA_real_, bias_detected = FALSE, mpmax_bin = NULL)
  if (lower_lod_method == "mpmax") {
    lower_lod <- infer_lower_lod(bins)
  }

  # Default fit range: start at bias-corrected lower LOD (if defined), end at max bin high
  fit_range_um <- fit_range_um
  if (is.na(fit_range_um[1])) {
    fit_range_um[1] <- if (is.finite(lower_lod$L_lower_lod_um)) {
      lower_lod$L_lower_lod_um
    } else {
      min(bins$L_low[bins$L_low > 0], na.rm = TRUE)
    }
  }
  if (is.na(fit_range_um[2])) {
    fit_range_um[2] <- max(bins$L_high, na.rm = TRUE)
  }

  df_full <- bins |>
    arrange(L_low) |>
    mutate(N_ge = rev(cumsum(rev(n)))) |>
    filter(L_low > 0,
           N_ge > 0)

  lower_lod_refined <- list(L_lower_lod_um = fit_range_um[1], flagged = FALSE)
  if (lower_resid_method == "left_resid") {
    lower_lod_refined <- refine_lower_lod_by_resid(
      df_cpsd = df_full,
      start_lod = fit_range_um[1],
      upper_lod = fit_range_um[2],
      min_points = min_points,
      head_k = lower_tail_k,
      run_k = lower_run_k,
      z = lower_z
    )
    fit_range_um[1] <- max(fit_range_um[1], lower_lod_refined$L_lower_lod_um, na.rm = TRUE)
  }
  lower_lod_used_um <- fit_range_um[1]

  df0 <- df_full |>
    filter(L_low >= fit_range_um[1])

  upper_lod <- list(L_upper_lod_um = fit_range_um[2], flagged = FALSE, diagnostics = NULL)
  if (upper_lod_method == "tail_resid") {
    N_min_use <- if (lower_lod_refined$flagged) N_min_lowbias else N_min
    min_nonzero_tail_use <- if (lower_lod_refined$flagged) min_nonzero_tail_lowbias else min_nonzero_tail
    upper_lod <- infer_upper_lod(
      df_cpsd = df0,
      min_points = upper_min_points,
      tail_k = tail_k,
      run_k = run_k,
      z = z,
      N_min = N_min_use,
      min_nonzero_tail = min_nonzero_tail_use,
      require_slope_stability = require_slope_stability,
      slope_tol = slope_tol
    )
    fit_range_um[2] <- min(fit_range_um[2], upper_lod$L_upper_lod_um, na.rm = TRUE)
  }

  df <- df0 |>
    filter(L_high <= fit_range_um[2])

  if (nrow(df) < min_points) {
    stop("Not enough bins in the fit range to fit the C-PSD model.")
  }

  # Linear fit in log-log space for the cumulative distribution
  fit <- lm(log10(N_ge) ~ log10(L_low), data = df)
  coefs <- coef(fit)
  fit_summ <- summary(fit)
  se_a_cpsd <- unname(fit_summ$coefficients["log10(L_low)", "Std. Error"])
  se_b_cpsd <- unname(fit_summ$coefficients["(Intercept)", "Std. Error"])

  a_cpsd <- unname(coefs[2])
  b_cpsd <- unname(coefs[1])      # intercept of log10(N_ge)
  b_cpsd_lin <- 10^b_cpsd         # b' in N_ge = b' * L^(a')
  a_psd <- a_cpsd - 1             # a' = a + 1
  b_psd <- -b_cpsd_lin * a_cpsd   # b = -b' * (a + 1) = -b' * a'

  list(
    data = df,
    bins = bins,
    fit = fit,
    fit_range_um = fit_range_um,
    lower_lod_um = lower_lod$L_lower_lod_um,
    lower_lod_bias_detected = lower_lod$bias_detected,
    lower_lod_refined_um = lower_lod_refined$L_lower_lod_um,
    lower_lod_refined_flag = lower_lod_refined$flagged,
    lower_lod_used_um = lower_lod_used_um,
    mpmax_bin = lower_lod$mpmax_bin,
    upper_lod_um = upper_lod$L_upper_lod_um,
    upper_lod_diagnostics = upper_lod$diagnostics,
    a_cpsd = a_cpsd,          # slope in log10 N_ge vs log10 L_low
    b_cpsd = b_cpsd,          # log10 intercept for N_ge
    se_a_cpsd = se_a_cpsd,    # Std. Error of a_cpsd
    se_b_cpsd = se_b_cpsd,    # Std. Error of b_cpsd (log10 scale)
    b_cpsd_lin = b_cpsd_lin,  # linear intercept b'
    a_psd = a_psd,            # differential exponent
    se_a_psd = se_a_cpsd,     # same as se_a_cpsd (shift by -1 doesn't change SE)
    b_psd = b_psd             # differential intercept
  )
}
#
#
#
#
#
#
cpsd_fit_frag <- fit_cpsd(raw_particles |> filter(shape == "fragment") |> pull(length_um),
                          bin_um = 5,
                          fit_range_um = c(NA, 5000),
                          lower_lod_method = "mpmax",
                          lower_resid_method = "left_resid",
                          N_min_lowbias = 3,
                          min_nonzero_tail_lowbias = 1
                           ) |> 
                          c(list(shape = "fragment"))

cpsd_fit_fiber <- fit_cpsd(raw_particles |> filter(shape == "fiber") |> pull(length_um),
                           bin_um = 5,
                           fit_range_um = c(NA, 5000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fiber"))

cpsd_fit_all <- fit_cpsd(raw_particles |> pull(length_um),
                           bin_um = 5,
                           fit_range_um = c(NA, 5000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "all"))

                          
                           
# report alpha +- sd, and lower LODs for each shape
cat("Fragment: alpha =", signif(cpsd_fit_frag$a_cpsd,2), "+-", signif(cpsd_fit_frag$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_frag$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_frag$upper_lod_um,2), "\n")

cat("Fiber: alpha =", signif(cpsd_fit_fiber$a_cpsd,2), "+-", signif(cpsd_fit_fiber$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_fiber$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_fiber$upper_lod_um,2), "\n")

cat("All: alpha =", signif(cpsd_fit_all$a_cpsd,2), "+-", signif(cpsd_fit_all$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_all$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_all$upper_lod_um,2), "\n")
#
#
#
#
custom_palette <- c(
  "all" = "#999999",       # Grey
  "fiber" = "#E69F00",      # Orange
  "fragment" = "#56B4E9",   # Light Blue
  "film" = "#009E73",       # Green
  "nurdle" = "#F0E442",     # Yellow
  "foam" = "#0072B2",       # Dark Blue
  "other" = "#D55E00"       # Red (for any unexpected categories)
)

plot_cpsd_multi <- function(fits, title = "", palette = custom_palette, attribute = "Length", x_text = 3, y_text = 1) {
  stopifnot(is.list(fits), length(fits) > 0)

  # Combine all points (full C-PSD, not just fit range)
  df_all <- dplyr::bind_rows(lapply(names(fits), function(shp) {
    fo <- fits[[shp]]
    lower_lod_plot <- if (!is.null(fo$lower_lod_used_um) && is.finite(fo$lower_lod_used_um)) {
      fo$lower_lod_used_um
    } else {
      fo$lower_lod_um
    }
      df <- fo$bins |>
        arrange(L_low) |>
        mutate(N_ge = rev(cumsum(rev(n)))) |>
        filter(L_low > 0, N_ge > 0) |>
        mutate(
          shape = shp,
          in_lod = is.finite(lower_lod_plot) &
            is.finite(fo$upper_lod_um) &
            L_low >= lower_lod_plot &
            L_high <= fo$upper_lod_um
        )
      df
    }))

  # Combine per-shape fit params
  params <- dplyr::bind_rows(lapply(names(fits), function(shp) {
    fo <- fits[[shp]]
    data.frame(
      shape = shp,
      a_cpsd = fo$a_cpsd,
      b_cpsd = fo$b_cpsd,
      se_a_cpsd = fo$se_a_cpsd,
      se_b_cpsd = fo$se_b_cpsd,
      lower_lod_um = fo$lower_lod_um,
      lower_lod_used_um = if (!is.null(fo$lower_lod_used_um) && is.finite(fo$lower_lod_used_um)) {
        fo$lower_lod_used_um
      } else {
        fo$lower_lod_um
      },
      upper_lod_um = fo$upper_lod_um,
      stringsAsFactors = FALSE
    )
  }))

  # Prediction bands for each shape (95% CI in log10 space)
  pred_all <- dplyr::bind_rows(lapply(names(fits), function(shp) {
    fo <- fits[[shp]]
    df <- fo$data
    grid <- tibble(L_low = seq(min(df$L_low), max(df$L_low), length.out = 200))
    pred <- predict(fo$fit, newdata = grid, se.fit = TRUE)
    tibble(
      shape = shp,
      L_low = grid$L_low,
      logN_fit = pred$fit,
      logN_lo = pred$fit - 1.96 * pred$se.fit,
      logN_hi = pred$fit + 1.96 * pred$se.fit
    )
  }))

   params <- params |>
    dplyr::mutate(
      fit_label = sprintf(
        "y = %.2f x %s %.2f",
        a_cpsd,
        ifelse(b_cpsd < 0, "-", "+"),
        abs(b_cpsd)
      )
    )

  ggplot(df_all, aes(x = log10(L_low), y = log10(N_ge), color = shape)) +
    geom_point(aes(alpha = in_lod)) +

    # Per-facet 95% CI ribbon (log10 space)
    geom_ribbon(
      data = pred_all,
      aes(x = log10(L_low), ymin = logN_lo, ymax = logN_hi, fill = shape),
      inherit.aes = FALSE,
      alpha = 0.2,
      color = NA
    ) +

    # Per-facet fitted line (log10 space)
    geom_line(
      data = pred_all,
      aes(x = log10(L_low), y = logN_fit),
      inherit.aes = FALSE
    ) +

    # Per-facet LOD line (use params df)
    # geom_vline(
    #   data = params,
    #   aes(xintercept = log10(lower_lod_used_um)),
    #   linetype = "dashed",
    #   color = "red",
    #   inherit.aes = FALSE
    # ) +

    # Per-facet upper LOD line (use params df)
    # geom_vline(
    #   data = params,
    #   aes(xintercept = log10(upper_lod_um)),
    #   linetype = "dashed",
    #   color = "red",
    #   inherit.aes = FALSE
    # ) +
      geom_text(
      data = params,
      aes(label = fit_label),
      x = x_text,
      y = y_text,
      #hjust = 1.8,
      #vjust = 2.5,
      color = "black",
      inherit.aes = FALSE,
      size = 5
    ) + 
    facet_wrap(~ shape, scales = "free_x") +
    scale_color_manual(values = palette, drop = FALSE) +
    scale_fill_manual(values = palette, drop = FALSE) +
    scale_alpha_manual(values = c(`TRUE` = 0.7, `FALSE` = 0.15)) +
    labs(
      title = title,
      x = bquote(log[10] * (.(attribute)~","~mu*m)),
      y = expression(log[10] * (Cumulative~count~","~N(geq~L)))
    ) +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )
}

# combine fits by shape
fits_by_shape <- list(
  fragment = cpsd_fit_frag,
  fiber    = cpsd_fit_fiber,
  all      = cpsd_fit_all
  # film  = cpsd_fit_film,
  # foam  = cpsd_fit_foam,
  # etc.
)
fits_by_shape <- fits_by_shape[names(custom_palette)[names(custom_palette) %in% names(fits_by_shape)]]
alpha_cpsh_plot <- plot_cpsd_multi(fits_by_shape, title = "C-PSD fits by shape", attribute = "Length", x_text = 2, y_text = 1)

ggsave(alpha_cpsh_plot,
       filename = "figures/alpha_cpsh_plot.png",
       width = 8, height = 6, dpi = 300)

alpha_cpsh_plot
#
#
#
# re-calculate particles/L count for samples based on analytically-determined LODs


#
#
#
#
#
cpsd_fit_area_fragment <- fit_cpsd(raw_particles |> filter(shape == "fragment") |> pull(area_um2),
                           bin_um = 500,
                           fit_range_um = c(2000, 50000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fragment"))

cpsd_fit_area_fiber <- fit_cpsd(raw_particles |> filter(shape == "fiber") |> pull(area_um2),
                           bin_um = 5,
                           fit_range_um = c(2000, 500000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fiber"))

cpsd_fit_area <- fit_cpsd(raw_particles |> pull(area_um2),
                           bin_um = 5,
                           fit_range_um = c(2000, 500000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "all"))
                          
                           
# report alpha +- sd, and lower LODs for each shape
cat("Fragment Surface Area: alpha =", signif(cpsd_fit_area_fragment$a_cpsd,2), "+-", signif(cpsd_fit_area_fragment$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_area_fragment$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_area_fragment$upper_lod_um,2), "\n")

cat("Fiber Surface Area: alpha =", signif(cpsd_fit_area_fiber$a_cpsd,2), "+-", signif(cpsd_fit_area_fiber$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_area_fiber$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_area_fiber$upper_lod_um,2), "\n")

cat("All Shape Surface Area: alpha =", signif(cpsd_fit_area$a_cpsd,2), "+-", signif(cpsd_fit_area$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_area$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_area$upper_lod_um,2), "\n")
#
#
#
#
#
#
#
surface_area_fits_by_shape <- list(
  fragment = cpsd_fit_area_fragment,
  fiber    = cpsd_fit_area_fiber,
  all      = cpsd_fit_area
  # film  = cpsd_fit_film,
  # foam  = cpsd_fit_foam,
  # etc.
)

surfacearea_cpsd_plot <- plot_cpsd_multi(surface_area_fits_by_shape, title = "C-PSD fits by shape", attribute = "Area", x_text = 3.8, y_text = 1)

ggsave(surfacearea_cpsd_plot,
       filename = "figures/surfacearea_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

surfacearea_cpsd_plot
#
#
#
#
#
cpsd_fit_volume_fragment <- fit_cpsd(raw_particles |> filter(shape == "fragment") |> pull(V_um3),
                           bin_um = 1e4,
                           fit_range_um = c(NA, NA),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fragment"))

cpsd_fit_volume_fiber <- fit_cpsd(raw_particles |> filter(shape == "fiber") |> pull(V_um3),
                           bin_um = 1e4,
                           fit_range_um = c(NA, NA),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fiber"))

cpsd_fit_volume <- fit_cpsd(raw_particles |> pull(V_um3),
                           bin_um = 1e4,
                           fit_range_um = c(NA, NA),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "all"))


# report alpha +- sd, and lower LODs for each shape
cat("Fragment Volume: alpha =", signif(cpsd_fit_volume_fragment$a_cpsd,2), "+-", signif(cpsd_fit_volume_fragment$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_volume_fragment$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_volume_fragment$upper_lod_um,2), "\n")

cat("Fiber Volume: alpha =", signif(cpsd_fit_volume_fiber$a_cpsd,2), "+-", signif(cpsd_fit_volume_fiber$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_volume_fiber$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_volume_fiber$upper_lod_um,2), "\n")

cat("All Shape Volume: alpha =", signif(cpsd_fit_volume$a_cpsd,2), "+-", signif(cpsd_fit_volume$se_a_cpsd,2), ", lower LOD =", signif(cpsd_fit_volume$lower_lod_used_um,2), ", upper LOD =", signif(cpsd_fit_volume$upper_lod_um,2), "\n")
#
#
#
#
volume_fits_by_shape <- list(
  fragment = cpsd_fit_volume_fragment,
  fiber    = cpsd_fit_volume_fiber,
  all      = cpsd_fit_volume
  # film  = cpsd_fit_film,
  # foam  = cpsd_fit_foam,
  # etc.
)

volume_cpsd_plot <- plot_cpsd_multi(volume_fits_by_shape, title = "C-PSD fits by shape", attribute = "Volume", x_text = 3.8, y_text = 1)

ggsave(volume_cpsd_plot,
       filename = "figures/volume_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

volume_cpsd_plot
```
#
#
#
#
#
alpha_dist <- function(mu, sd, n = 10000, lower = -6, upper = -1.1) {
  truncnorm::rtruncnorm(n, a = lower, b = upper, mean = mu, sd = sd)
}

alpha<- alpha_dist(
  mu = cpsd_fit_all$a_cpsd,
  sd = cpsd_fit_all$se_a_cpsd,
  n = 20000
)

alpha_mu <- mean(alpha)
alpha_se <- sd(alpha)

summary(alpha)
#
#
#
#
#
#
raw_particles |> 
  ggplot(aes(x = 1/aspect_ratio, fill = shape)) +
  geom_histogram() +
  labs(x = "Length to Width Ratio", y = "Count") +
  theme_minimal(base_size = 15)
#
#
#
# summary statistics for aspect ratio
R.ave_vals <- raw_particles$aspect_ratio
R.ave_vals <- R.ave_vals[is.finite(R.ave_vals) & R.ave_vals > 0]
R.ave_vals <- 1 / R.ave_vals

R.ave_boot <- replicate(n_boot, {
  xb <- sample(R.ave_vals, size = length(R.ave_vals), replace = TRUE)
  mean(xb)
})

R.ave_summary <- tibble(
  p05 = quantile(R.ave_vals, 0.05),
  p50 = quantile(R.ave_vals, 0.50),
  p95 = quantile(R.ave_vals, 0.95),
  mean = mean(R.ave_vals),
  sd = sd(R.ave_vals),
  boot_mean = mean(R.ave_boot),
  boot_sd = sd(R.ave_boot)
)

R.ave_mean <- R.ave_summary$boot_mean
R.ave_sd <- R.ave_summary$boot_sd

print(R.ave_summary)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
correction_factor <- function(a, L_meas_min, L_meas_max, L_tar_min, L_tar_max) {
  stopifnot(a < -1)  # ensure convergence for extrapolations to small L
  num <- (L_tar_max^(a+1) - L_tar_min^(a+1)) / (a+1)
  den <- (L_meas_max^(a+1) - L_meas_min^(a+1)) / (a+1)
  num / den
}

# Bias-corrected lower LOD bound (Segur et al. draft, MP#max rule)
# Replace the reported Lmin with a bias-corrected L_lower_LOD when the BN-PSD shows
# low-bias at small sizes.
monitoring <- monitoring |>
  mutate(
    Lmin_biascorr_um = pmax(Lmin_measured_um, cpsd_fit_all$lower_lod_um, na.rm = TRUE),
    Lmax_biascorr_um = pmin(Lmax_measured_um, cpsd_fit_all$upper_lod_um, na.rm = TRUE)
  )

L_meas_min_use <- median(monitoring$Lmin_biascorr_um, na.rm = TRUE)
L_meas_max_use <- median(monitoring$Lmax_biascorr_um, na.rm = TRUE)

# Example: convert net-tow-like 300–5000 µm to 1–5000 µm using alpha_frag draws
cf_rescale <- correction_factor(
  a = alpha,
  L_meas_min = L_meas_min_use, 
  L_meas_max = L_meas_max_use,
  L_tar_min  = 1,   
  L_tar_max  = 5000
)

quantile(cf_rescale, c(0.05, 0.5, 0.95))
#
#
#
#
#
#
#
#
#
n_mc <- 20000

# Simulated multiplicative factors (replace with your fitted distributions)
fiber_cf   <- rlnorm(n_mc, meanlog = log(2.0), sdlog = 0.5)      # >1
plastic_pf <- pmin(rbeta(n_mc, shape1 = 20, shape2 = 3), 1.0)   # 0–1

combined_cf <- cf_rescale #* fiber_cf * plastic_pf
quantile(combined_cf, c(0.05, 0.5, 0.95))
#
#
#
#
#
#
#
#
#
#
#
# For simplicity, assume the same CF distribution applies to each sample.
# If CF depends on site/method/shape, build CF distributions per strata and apply accordingly.

C_corrected_draws <- monitoring |>
  mutate(idx = row_number()) |>
  tidyr::crossing(draw = 1:3000) |>
  mutate(
    cf = sample(combined_cf, size = n(), replace = TRUE),
    C_corrected_pL = C_measured_pL * cf
  )

C_corrected_draws |>
  summarise(p05 = quantile(C_corrected_pL, 0.05),
            p50 = quantile(C_corrected_pL, 0.50),
            p95 = quantile(C_corrected_pL, 0.95))
#
#
#
#
#
#
#
#
#
#
#
bootstrap_eed <- function(x, n_boot = 5000, probs = c(0.5, 0.95)) {
  x <- x[is.finite(x)]
  stopifnot(length(x) >= 5)
  boot_stats <- replicate(n_boot, {
    xb <- sample(x, size = length(x), replace = TRUE)
    as.numeric(quantile(xb, probs = probs, names = FALSE))
  })
  boot_df <- as.data.frame(t(boot_stats))
  names(boot_df) <- paste0("q", probs*100)
  boot_df
}

# Use one corrected draw per sample (to isolate EED uncertainty from correction-factor uncertainty),
# then optionally repeat over CF draws as a second uncertainty layer.
# Here we "collapse" each sample to its median corrected value, then bootstrap across samples.
C_sample_median <- C_corrected_draws |>
  group_by(sample_id) |>
  summarise(C_corr_med = median(C_corrected_pL), .groups = "drop")

eed_boot <- bootstrap_eed(C_sample_median$C_corr_med, n_boot = n_boot, probs = c(0.5, 0.95))
summary(eed_boot)
#
#
#
# Visualize the bootstrap uncertainty in EED percentiles
eed_boot |>
  tidyr::pivot_longer(cols = everything(), names_to = "stat", values_to = "value") |>
  ggplot(aes(x = value)) +
  geom_histogram(bins = 40) +
  facet_wrap(~stat, scales = "free") +
  labs(x = "particles/L", y = "count", title = "Empirical bootstrap distributions of EED percentiles")
#
#
#
#
#
#
#
#
#
# filter species by environment
tox_data <- tomex2 |> 
   dplyr::filter(
    env_f == "Freshwater",
    Group != "Bacterium", # Drop studies with Bacterium group
    Group != "Plant", # Drop studies with Plant group
    effect.metric != "HONEC", # Drop studies with HONEC effect metric
    tier_zero_tech_f == "Red Criteria Passed", #minimum QC for technical criteria
    tier_zero_risk_f == "Red Criteria Passed", #minimum QC for risk criteria
    risk.13 != 0 #Drop studies that received a score of 0 for endpoints criteria (this also drops studies that have not yet been)
  )

# summarize by group, species, and polymer type to understand tox data included in model
tox_data |> 
  group_by(Group, Species, poly_f, shape_f) |> 
  summarise(n = n(), .groups = "drop")
#
#
#
# replace the default parameter matrix distribution with values derived using the C-PSD method here
param_values <- PSSDplusplus::param_default_values |> 
  # update parameters from C-PSD method
  mutate(alpha.freshwater = -alpha_mu, # use cPSD - 1 since dN/dL = k * L^a
         alpha.freshwater.sd = alpha_se,
         # additional values need updating:
         ### Length to width ratio
         R.ave.water.freshwater = R.ave_mean,
         R.ave.water.freshwater.sd = R.ave_sd*100,

         ### polymer density
         #p.ave.freshwater = ,
         #p.ave.freshwater.sd = ,

         ### volume
         a.v.freshwater = -cpsd_fit_volume$a_cpsd, 
         a.v.freshwater.sd = cpsd_fit_volume$se_a_cpsd,

         ### mass
         #a.m.freshwater = ,
         #a.m.freshwater.sd = ,

         ### surface area
         a.sa.freshwater = -cpsd_fit_area$a_cpsd,# derived value for all shapes
         a.sa.freshwater.sd = cpsd_fit_area$se_a_cpsd
         )

# generate parameter matrix using bespoke environmental parameter values
param_matrix <- matrix_function(
  n = n_boot, #size of matrix (10,000 is recommended)
  params = param_values, #parameter distribution values 
  #params = PSSDplusplus::param_default_values, #beta-test with defaults
  upper.tissue.truncation.limit = 500, # maximum particle size (microns) to truncate tissue tranlocation size limit distribution to based on biological plausibility
  x1M_set = 1, #minimum particle size to generate distributions to in microns
  x2D_set = 5000 #maximum particle size to generate distributions to in microns
) |> 
  # replace R.ave.water.freshwater with bootstrapped values
   mutate(R.ave.water.freshwater = sample(R.ave_vals, size = n(), replace = TRUE),
          H_W_ratio.freshwater = sample(R.ave_vals, size = n(), replace = TRUE)) #H_W ratio is assumed same as length-width ratio

# visualize parameter distributions
param_plots <- parameter_histograms_function(param_matrix) # generate parameter distribution plots
#visualize plots
param_plots$alpha_combined_plot # distribution of alpha parameters 
#
#
#
#
#
MC_sim_df <- MC_sim_align_parallel(
  tox_data = tox_data, # toxicity data (ToMEx 2.0 is default)
  param_matrix = param_matrix, # parameter matrix generated in prior cell
  n_sim = n_boot, # size of parameter matrix (ensure this is the same as the size of the matrix generated)
  x1D_set = 1, # minimum particle size to generate distributions to (in microns)
  x2D_set = 5000, # maximum particle size to generate distributions to (in microns)
  num_cores = parallel::detectCores() - 2 # number of cores to use for parallel processing (auto-detect - 2)
)
#
#
#
#
results_df_food <- dplyr::filter(
  MC_sim_df,
  ingestible != "not ingestible",
  particles_L_food_dilution > 0,
  Group != "Algae"
) |>
  dplyr::mutate(dose_new_particles_L = particles_L_food_dilution) |>
  tidyr::drop_na(particles_L_food_dilution)
results_df_food_t3_t4 <- dplyr::filter(
  results_df_food,
  risk.13 != 1,
  bio_f %in% c("Organism", "Population")
)

results_df_tissue <- dplyr::filter(
  MC_sim_df,
  translocatable != "not translocatable",
  particles_L_ox_stress > 0
) |>
  dplyr::mutate(dose_new_particles_L = particles_L_ox_stress) |>
  tidyr::drop_na(particles_L_ox_stress)
results_df_tissue_t3_t4 <- dplyr::filter(
  results_df_tissue,
  risk.13 != 1,
  bio_f %in% c("Organism", "Population")
)

erm_registry <- list(
  "Food Dilution" = list(base = results_df_food, t3_t4 = results_df_food_t3_t4),
  "Tissue Translocation" = list(base = results_df_tissue, t3_t4 = results_df_tissue_t3_t4)
)
#
#
#
#
#
# make all pSSDs
pSSDs <- make_all_pSSDs(
  MC_sim_df = MC_sim_df, # ensure aligned data is included
  environments = c("Freshwater"), #specify which environments to include
  erm_registry = erm_registry,
  sim = 30, # specify the number of simulations to run (300 used in publication)
  cv_uf = 0.5, # specify the coefficient of variation for uncertainty factors (acute -> chronic and LOEC/EC50 -> NOEC)
  rmore_method = "lognormal", # method to handle pSSD distribution building (options = 'step' i.e., original trapezoidal method in Wigger et al., 2020, or 'lognormal' - shortcut developed in Coffin et al. (2025))
  parallel = TRUE, #whether or not to use parallel processing
  workers = parallel::detectCores() - 2, # number of worker processes to use for parallel processing
  base_cache_dir = file.path(tempdir(), "pssd_cache"), # directory to store cached pSSD objects (important when running high number of simulations to resume if needed)
  base_output_path = file.path(tempdir(), "pssd_figures"), # directory to store output figures
  overwrite_cache = TRUE # whether or not to overwrite cached pSSD objects
)
#
#
#
#
#
PNEC_summary <- summarize_PNECs(pSSDs)
head(PNEC_summary)
#
#
#
pSSDs$`Tier3_Freshwater_Food Dilution`$PNEC_plot_05
#
#
#
#
#
pSSDs$`Tier3_Freshwater_Food Dilution`$pSSD_plot
#
#
#
pSSDs$`Tier3_Freshwater_Food Dilution`$arranged_plot
#
#
#
#
# extract raw HC5 distribution
haz_HC5_food <- pSSDs$`Tier3_Freshwater_Food Dilution`$summary_05$df |> 
  mutate(HCx = 5,
         ERM = "Food Dilution")

haz_HC10_food <- pSSDs$`Tier3_Freshwater_Food Dilution`$summary_10$df |> 
  mutate(HCx = 10,
         ERM = "Food Dilution")
         
# extract raw HC5 distribution
haz_HC5_tissue <- pSSDs$`Tier3_Freshwater_Tissue Translocation`$summary_05$df |> 
  mutate(HCx = 5,
         ERM = "Tissue Translocation")

haz_HC10_tissue <- pSSDs$`Tier3_Freshwater_Tissue Translocation`$summary_10$df |> 
  mutate(HCx = 10,
         ERM = "Tissue Translocation")

# make a combined df of HC5 and HC10 values
haz <- bind_rows(haz_HC5_food, haz_HC10_food, haz_HC5_tissue, haz_HC10_tissue)
#
#
#
ggplot(haz, aes(x = PNEC, fill = as.factor(ERM))) +
  geom_histogram(bins = 15) +
  facet_wrap(~HCx + ERM, ncol = 2, scales = "free_y",
             labeller = labeller(HCx = function(x) paste0("HC", x))) +
  scale_x_log10() +
  labs(x = "HC5 (particles/L)", y = "count", title = "Hazard threshold distribution") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")
#
#
#
#
#
#
#
#
#
#
#
draw_rq_mc1d <- function(haz_df, exposure_draws, n_risk = 1000) {
  exposure_draws <- exposure_draws[is.finite(exposure_draws)]
  stopifnot(length(exposure_draws) >= 5)

  haz_df |>
    dplyr::group_by(ERM, HCx) |>
    dplyr::group_modify(~{
      tibble(
        E = sample(exposure_draws, n_risk, replace = TRUE),
        H = sample(.x$PNEC, n_risk, replace = TRUE)
      ) |>
        mutate(RQ = E / H)
    }) |>
    dplyr::ungroup()
}

summarize_rq <- function(risk_draws) {
  risk_draws |>
    dplyr::group_by(ERM, HCx) |>
    summarise(
      P_exceed = mean(RQ > 1),
      RQ_p50 = median(RQ),
      RQ_p95 = quantile(RQ, 0.95),
      RQ_p99 = quantile(RQ, 0.99),
      .groups = "drop"
    )
}

n_risk <- 1000
risk_draws <- draw_rq_mc1d(haz, eed_boot$q50, n_risk = n_risk)
risk_summary <- summarize_rq(risk_draws)
risk_summary
#
#
#
ggplot(risk_draws, aes(x = RQ, fill = as.factor(ERM))) +
  geom_histogram(bins = 60) +
  scale_x_log10() +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  facet_grid(HCx ~ ERM, scales = "free_y",
             labeller = labeller(HCx = function(x) paste0("HC", x))) +
  labs(x = "Risk Quotient (Exposure / Hazard) [log10 scale]", y = "count",
       title = "Monte Carlo risk quotient distributions by ERM and HCx") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")
#
#
#
#
#
#
#
# Helper: bootstrap ECDF bands
ecdf_bands <- function(x, grid, n_boot = 100, probs = c(0.025, 0.5, 0.975)) {
  x <- x[is.finite(x)]
  stopifnot(length(x) >= 5)
  boot <- replicate(n_boot, {
    xb <- sample(x, size = length(x), replace = TRUE)
    stats::ecdf(xb)(grid)
  })
  tibble(
    x = grid,
    cdf_lo = apply(boot, 1, quantile, probs = probs[1], names = FALSE),
    cdf_med = apply(boot, 1, quantile, probs = probs[2], names = FALSE),
    cdf_hi = apply(boot, 1, quantile, probs = probs[3], names = FALSE)
  )
}

# EED distribution: empirical CDF over sample-level corrected medians
eed_vals <- C_sample_median$C_corr_med

# Common x-grid across EED and HCx values
all_x <- c(eed_vals, haz$PNEC)
grid_x <- 10^seq(log10(min(all_x, na.rm = TRUE)),
                 log10(max(all_x, na.rm = TRUE)),
                 length.out = 200)

# EED bands (green)
eed_band <- ecdf_bands(eed_vals, grid_x, n_boot = n_boot) |>
  mutate(source = "EED")

# HCx bands by ERM and HCx
haz_band <- haz |>
  group_by(ERM, HCx) |>
  group_modify(~{
    ecdf_bands(.x$PNEC, grid_x, n_boot = n_boot) |>
      mutate(source = .y$ERM)
  }) |>
  ungroup()

erm_levels <- sort(unique(haz$ERM))
erm_colors <- setNames(c("#1F78B4", "#FF7F00")[seq_along(erm_levels)], erm_levels)
source_colors <- c("EED" = "forestgreen", erm_colors)

ggplot() +
  # EED ribbon + line (green)
  geom_ribbon(
    data = eed_band,
    aes(x = x, ymin = cdf_lo, ymax = cdf_hi, fill = source),
    alpha = 0.15
  ) +
  geom_line(
    data = eed_band,
    aes(x = x, y = cdf_med, color = source),
    linewidth = 1
  ) +
  # HCx ribbons + lines (color by ERM, linetype by HCx)
  geom_ribbon(
    data = haz_band,
    aes(x = x, ymin = cdf_lo, ymax = cdf_hi, fill = source),
    alpha = 0.15
  ) +
  geom_line(
    data = haz_band,
    aes(x = x, y = cdf_med, color = source, linetype = as.factor(HCx)),
    linewidth = 1
  ) +
  scale_x_log10() +
  scale_color_manual(values = source_colors, breaks = names(source_colors)) +
  scale_fill_manual(values = source_colors, breaks = names(source_colors)) +
  labs(
    x = "particles/L (log10 scale)",
    y = "CDF",
    color = "Source",
    fill = "Source",
    linetype = "HCx",
    title = "Overlap of EED and HCx distributions (CDFs with uncertainty)"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.title = element_blank()) +
  guides(fill = "none")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
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

  cf_unc <- sample(combined_cf, size = n_uncertainty, replace = TRUE)
  haz_groups <- haz_df |>
    dplyr::group_by(ERM, HCx) |>
    dplyr::group_split()

  res <- lapply(seq_len(n_uncertainty), function(i) {
    cf_i <- cf_unc[i]
    E_var <- sample(measured, size = n_variability, replace = TRUE) * cf_i

    dplyr::bind_rows(lapply(haz_groups, function(hg) {
      H_i <- sample(hg$PNEC, size = 1, replace = TRUE)
      rq <- E_var / H_i
      tibble(
        ERM = unique(hg$ERM),
        HCx = unique(hg$HCx),
        iter_u = i,
        cf = cf_i,
        H = H_i,
        P_exceed = mean(rq > 1),
        RQ_p50 = median(rq),
        RQ_p95 = quantile(rq, 0.95),
        RQ_p99 = quantile(rq, 0.99)
      )
    }))
  })

  dplyr::bind_rows(res)
}

mc2d_results <- mc2d_risk(
  monitoring_df = monitoring,
  combined_cf = combined_cf,
  haz_df = haz,
  n_uncertainty = 300,
  n_variability = 1000,
  seed = 1
)

mc2d_summary <- mc2d_results |>
  dplyr::group_by(ERM, HCx) |>
  summarise(
    P_exceed_med = median(P_exceed),
    P_exceed_p95 = quantile(P_exceed, 0.95),
    P_exceed_p05 = quantile(P_exceed, 0.05),
    RQ_p50_med = median(RQ_p50),
    RQ_p95_med = median(RQ_p95),
    .groups = "drop"
  )

mc2d_summary
#
#
#
# Visualize uncertainty in exceedance probability (outer loop)
mc2d_results |>
  ggplot(aes(x = P_exceed, fill = ERM)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_grid(HCx ~ ERM, scales = "free_y",
             labeller = labeller(HCx = function(x) paste0("HC", x))) +
  labs(x = "P(RQ > 1) across uncertainty", y = "count",
       title = "MC2D uncertainty distribution for exceedance probability") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")
#
#
#
# Diagnostic: compare uncertainty spread of P_exceed across ERM/HCx
mc2d_results |>
  ggplot(aes(x = as.factor(HCx), y = P_exceed, fill = ERM)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
  scale_x_discrete(labels = function(x) paste0("HC", x)) +
  labs(x = "HCx", y = "P(RQ > 1)",
       title = "MC2D uncertainty spread in exceedance probability") +
  theme_minimal(base_size = 15)
#
#
#
# Diagnostic: CF influence on exceedance probability
mc2d_results |>
  ggplot(aes(x = cf, y = P_exceed, color = ERM)) +
  geom_point(alpha = 0.4, size = 1.2) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~HCx, scales = "free_y",
             labeller = labeller(HCx = function(x) paste0("HC", x))) +
  labs(x = "Correction factor draw", y = "P(RQ > 1)",
       title = "Relationship between correction factor and exceedance") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")
#
#
#
# Diagnostic: distribution of high-end risk (RQ_p95)
mc2d_results |>
  tidyr::pivot_longer(cols = c(RQ_p50, RQ_p95, RQ_p99),
                      names_to = "stat", values_to = "value") |>
  ggplot(aes(x = value, fill = ERM)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  facet_grid(stat ~ HCx, scales = "free",
             labeller = labeller(HCx = function(x) paste0("HC", x))) +
  scale_x_log10() +
  labs(x = "Risk Quotient", y = "density",
       title = "MC2D uncertainty distribution for RQ summaries (log10 scale)") +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Build parameter bounds from param_values (mean +/- k * sd)
param_bounds <- function(param_values, k = 2) {
  pv <- param_values[1, , drop = FALSE]
  nm <- names(pv)
  sd_names <- nm[grepl("\\.sd$", nm)]
  base_names <- sub("\\.sd$", "", sd_names)
  base_names <- base_names[base_names %in% nm]

  tibble(
    param = base_names,
    mean = as.numeric(pv[1, base_names]),
    sd = as.numeric(pv[1, paste0(base_names, ".sd")])
  ) |>
    filter(is.finite(mean), is.finite(sd), sd > 0) |>
    mutate(min = mean - k * sd,
           max = mean + k * sd)
}

# Choose a small subset for beta testing (adjust as needed)
morris_params <- param_bounds(param_values, k = 2)
morris_subset <- c("alpha.freshwater", "R.ave.water.freshwater", "a.sa.freshwater")
if (all(morris_subset %in% morris_params$param)) {
  morris_params <- dplyr::filter(morris_params, param %in% morris_subset)
} else {
  morris_params <- dplyr::slice(morris_params, 1:min(4, nrow(morris_params)))
}

morris_params
#
#
#
# Helper: build hazard distributions from a custom param_values dataframe (small budgets for beta test)
build_haz_from_params <- function(pv = param_values,
                                  n_matrix = 2,
                                  n_sim = 2,
                                  sim = 2,
                                  num_cores = 1) {
  param_matrix_i <- matrix_function(
    n = n_matrix,
    params = pv,
    upper.tissue.truncation.limit = 500,
    x1M_set = 1,
    x2D_set = 5000
  )

  MC_sim_df_i <- MC_sim_align_parallel(
    tox_data = tox_data,
    param_matrix = param_matrix_i,
    n_sim = n_matrix,
    x1D_set = 1,
    x2D_set = 5000
  )

  results_df_food_i <- dplyr::filter(
    MC_sim_df_i,
    ingestible != "not ingestible",
    particles_L_food_dilution > 0,
    Group != "Algae"
  ) |>
    dplyr::mutate(dose_new_particles_L = particles_L_food_dilution) |>
    tidyr::drop_na(particles_L_food_dilution)
  results_df_food_t3_t4_i <- dplyr::filter(
    results_df_food_i,
    risk.13 != 1,
    bio_f %in% c("Organism", "Population")
  )

  results_df_tissue_i <- dplyr::filter(
    MC_sim_df_i,
    translocatable != "not translocatable",
    particles_L_ox_stress > 0
  ) |>
    dplyr::mutate(dose_new_particles_L = particles_L_ox_stress) |>
    tidyr::drop_na(particles_L_ox_stress)
  results_df_tissue_t3_t4_i <- dplyr::filter(
    results_df_tissue_i,
    risk.13 != 1,
    bio_f %in% c("Organism", "Population")
  )

  erm_registry_i <- list(
    "Food Dilution" = list(base = results_df_food_i, t3_t4 = results_df_food_t3_t4_i),
    "Tissue Translocation" = list(base = results_df_tissue_i, t3_t4 = results_df_tissue_t3_t4_i)
  )

  pSSDs_i <- make_all_pSSDs(
    MC_sim_df = MC_sim_df_i,
    environments = c("Freshwater"),
    erm_registry = erm_registry_i,
    sim = sim,
    cv_uf = 0.5,
    rmore_method = "lognormal",    
    parallel = T,
    workers = parallel::detectCores() - 1,
    base_cache_dir = file.path(tempdir(), "morris_pssd_cache"),
    base_output_path = file.path(tempdir(), "morris_pssd_figures"),
    overwrite_cache = TRUE
  )

  haz_HC5_food_i <- pSSDs_i$`Tier3_Freshwater_Food Dilution`$summary_05$df |>
    mutate(HCx = 5, ERM = "Food Dilution")
  haz_HC10_food_i <- pSSDs_i$`Tier3_Freshwater_Food Dilution`$summary_10$df |>
    mutate(HCx = 10, ERM = "Food Dilution")
  haz_HC5_tissue_i <- pSSDs_i$`Tier3_Freshwater_Tissue Translocation`$summary_05$df |>
    mutate(HCx = 5, ERM = "Tissue Translocation")
  haz_HC10_tissue_i <- pSSDs_i$`Tier3_Freshwater_Tissue Translocation`$summary_10$df |>
    mutate(HCx = 10, ERM = "Tissue Translocation")

  bind_rows(haz_HC5_food_i, haz_HC10_food_i, haz_HC5_tissue_i, haz_HC10_tissue_i)
}
#
#
#
# Morris model: returns a scalar RQ_p50 (median across ERM/HCx) for each input set
morris_model <- function(X) {
  apply(X, 1, function(x_row) {
    pv <- param_values
    for (j in seq_along(x_row)) {
      pv[[colnames(X)[j]]] <- x_row[j]
    }

    haz_i <- build_haz_from_params(pv, n_matrix = 10, n_sim = 10, sim = 10, num_cores = 5)
    mc2d_i <- mc2d_risk(
      monitoring_df = monitoring,
      combined_cf = combined_cf,
      haz_df = haz_i,
      n_uncertainty = 50,
      n_variability = 200,
      seed = 1
    )

    median(mc2d_i$RQ_p50, na.rm = TRUE)
  })
}

# Small Morris budget for beta test
morris_levels <- 5
morris_r <- 5

morris_design <- sensitivity::morris(
  model = NULL,
  factors = morris_params$param,
  r = morris_r,
  design = list(type = "oat", levels = morris_levels, grid.jump = 1),
  binf = morris_params$min,
  bsup = morris_params$max
)

morris_design$y <- morris_model(morris_design$X)
#
#
#
# Morris results + plots
morris_res <- sensitivity::tell(morris_design)
print(morris_res)

plot(morris_res) # mu* vs sigma
#
#
#
#
#
#
#
# import sediment particle data (beach sand as sediment proxy)
raw_particles_sed <- readRDS("data_input/Part_dets_comb.rds") |> 
  dplyr::filter(
    sample_type == "beach sand",
    !material_class %in% c("mineral", "organic matter"),
    bad_spectra
  ) |> 
  mutate(
    date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"),
    date = ymd(date_raw),
    shape = case_when(
      aspect_ratio >= 3 ~ "fiber",
      aspect_ratio <= 3 ~ "fragment"
    ),
    length_um = max_length_um,
    width_um = min_length_um
  )

# ---- Thickness (height) factor from sediment data ----
ratio_tbl_sed <- raw_particles_sed |>
  mutate(
    WL = 1 / aspect_ratio,
    width_eff_um = case_when(
      shape == "fragment" & !is.na(area_um2) & !is.na(length_um) & length_um > 0 &
        !is.na(circularity) & circularity < 0.7 ~ area_um2 / length_um,
      TRUE ~ width_um
    )
  ) |> 
  group_by(shape) |>
  summarise(
    n = n(),
    r_med  = median(WL, na.rm = TRUE),
    r_low  = quantile(WL, 0.25, na.rm = TRUE),
    r_high = quantile(WL, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Volumes ----
raw_particles_sed <- raw_particles_sed |>
  left_join(ratio_tbl_sed |> dplyr::select(shape, r_med), by = "shape") |>  
  mutate(
    height_um = r_med * width_um,
    V_um3 = case_when(
      shape == "fragment" ~ (pi / 6) * length_um * width_um * height_um,
      shape == "fiber"    ~ pi * (width_um / 2)^2 * length_um,
      TRUE ~ NA_real_
    )
  )
#
#
#
#
cpsd_fit_sed_frag <- fit_cpsd(raw_particles_sed |> filter(shape == "fragment") |> pull(length_um),
                          bin_um = 5,
                          fit_range_um = c(NA, 5000),
                          lower_lod_method = "mpmax",
                          lower_resid_method = "left_resid",
                          N_min_lowbias = 3,
                          min_nonzero_tail_lowbias = 1
                           ) |> 
                          c(list(shape = "fragment"))

cpsd_fit_sed_fiber <- fit_cpsd(raw_particles_sed |> filter(shape == "fiber") |> pull(length_um),
                          bin_um = 5,
                          fit_range_um = c(NA, 5000),
                          lower_lod_method = "mpmax",
                          lower_resid_method = "left_resid",
                          N_min_lowbias = 3,
                          min_nonzero_tail_lowbias = 1
                           ) |> 
                          c(list(shape = "fiber"))

cpsd_fit_sed_all <- fit_cpsd(raw_particles_sed |> pull(length_um),
                          bin_um = 5,
                          fit_range_um = c(NA, 5000),
                          lower_lod_method = "mpmax",
                          lower_resid_method = "left_resid",
                          N_min_lowbias = 3,
                          min_nonzero_tail_lowbias = 1
                           ) |> 
                          c(list(shape = "all"))

alpha_sed <- alpha_dist(
  mu = cpsd_fit_sed_all$a_cpsd,
  sd = cpsd_fit_sed_all$se_a_cpsd,
  n = 20000
)

alpha_sed_mu <- mean(alpha_sed)
alpha_sed_se <- sd(alpha_sed)
#
#
#
#
sed_length_fits_by_shape <- list(
  fragment = cpsd_fit_sed_frag,
  fiber    = cpsd_fit_sed_fiber,
  all      = cpsd_fit_sed_all
)
sed_length_fits_by_shape <- sed_length_fits_by_shape[names(custom_palette)[names(custom_palette) %in% names(sed_length_fits_by_shape)]]

sed_length_cpsd_plot <- plot_cpsd_multi(sed_length_fits_by_shape, title = "Sediment C-PSD fits by shape", attribute = "Length", x_text = 2, y_text = 1)

ggsave(sed_length_cpsd_plot,
       filename = "figures/sediment_length_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

sed_length_cpsd_plot
#
#
#
#
cpsd_fit_area_sed_fragment <- fit_cpsd(raw_particles_sed |> filter(shape == "fragment") |> pull(area_um2),
                           bin_um = 500,
                           fit_range_um = c(2000, 50000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fragment"))

cpsd_fit_area_sed_fiber <- fit_cpsd(raw_particles_sed |> filter(shape == "fiber") |> pull(area_um2),
                           bin_um = 5,
                           fit_range_um = c(2000, 500000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fiber"))

cpsd_fit_area_sed <- fit_cpsd(raw_particles_sed |> pull(area_um2),
                           bin_um = 5,
                           fit_range_um = c(2000, 500000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "all"))
#
#
#
#
sed_area_fits_by_shape <- list(
  fragment = cpsd_fit_area_sed_fragment,
  fiber    = cpsd_fit_area_sed_fiber,
  all      = cpsd_fit_area_sed
)

sed_area_cpsd_plot <- plot_cpsd_multi(sed_area_fits_by_shape, title = "Sediment C-PSD fits by shape", attribute = "Area", x_text = 3.8, y_text = 1)

ggsave(sed_area_cpsd_plot,
       filename = "figures/sediment_surfacearea_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

sed_area_cpsd_plot
#
#
#
#
cpsd_fit_volume_sed_fragment <- fit_cpsd(raw_particles_sed |> filter(shape == "fragment") |> pull(V_um3),
                           bin_um = 5,
                           fit_range_um = c(2000, 500000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fragment"))

cpsd_fit_volume_sed_fiber <- fit_cpsd(raw_particles_sed |> filter(shape == "fiber") |> pull(V_um3),
                           bin_um = 5,
                           fit_range_um = c(2000, 500000),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "fiber"))

cpsd_fit_volume_sed <- fit_cpsd(raw_particles_sed |> pull(V_um3),
                           bin_um = 5,
                           fit_range_um = c(NA, NA),
                           lower_lod_method = "mpmax",
                           lower_resid_method = "left_resid",
                           N_min_lowbias = 3,
                           min_nonzero_tail_lowbias = 1
                            ) |> 
                           c(list(shape = "all"))
#
#
#
#
sed_volume_fits_by_shape <- list(
  fragment = cpsd_fit_volume_sed_fragment,
  fiber    = cpsd_fit_volume_sed_fiber,
  all      = cpsd_fit_volume_sed
)

sed_volume_cpsd_plot <- plot_cpsd_multi(sed_volume_fits_by_shape, title = "Sediment C-PSD fits by shape", attribute = "Volume", x_text = 3.8, y_text = 1)

ggsave(sed_volume_cpsd_plot,
       filename = "figures/sediment_volume_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

sed_volume_cpsd_plot
#
#
#
#
#
monitoring_sed <- readRDS("data_input/Part_dets_summ.rds") |> 
  filter(
    sample_type == "beach sand",
    material_simple == "plastic",
    sample_or_blank == "sample"
  ) |>
  mutate(
    date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"),
    date = ymd(date_raw),
    sample_id = Client_ID_MSSupdate,
    # placeholder: update if a sediment-specific concentration column is available
    C_measured_pkg = extrap_conc_PPL,
    Lmin_measured_um = 50,
    Lmax_measured_um = 500
  )

monitoring_sed <- monitoring_sed |>
  mutate(
    Lmin_biascorr_um = pmax(Lmin_measured_um, cpsd_fit_sed_all$lower_lod_um, na.rm = TRUE),
    Lmax_biascorr_um = pmin(Lmax_measured_um, cpsd_fit_sed_all$upper_lod_um, na.rm = TRUE)
  )

L_meas_min_use_sed <- median(monitoring_sed$Lmin_biascorr_um, na.rm = TRUE)
L_meas_max_use_sed <- median(monitoring_sed$Lmax_biascorr_um, na.rm = TRUE)

cf_rescale_sed <- correction_factor(
  a = alpha_sed,
  L_meas_min = L_meas_min_use_sed, 
  L_meas_max = L_meas_max_use_sed,
  L_tar_min  = 1,   
  L_tar_max  = 5000
)

combined_cf_sed <- cf_rescale_sed

C_corrected_draws_sed <- monitoring_sed |>
  mutate(idx = row_number()) |>
  tidyr::crossing(draw = 1:3000) |>
  mutate(
    cf = sample(combined_cf_sed, size = n(), replace = TRUE),
    C_corrected_pkg = C_measured_pkg * cf
  )

C_sample_median_sed <- C_corrected_draws_sed |>
  group_by(sample_id) |>
  summarise(C_corr_med = median(C_corrected_pkg), .groups = "drop")

eed_boot_sed <- bootstrap_eed(C_sample_median_sed$C_corr_med, n_boot = n_boot, probs = c(0.5, 0.95))
summary(eed_boot_sed)
#
#
#
#
#
tox_data_sed <- tomex2 |> 
   dplyr::filter(
    env_f %in% c("Marine", "Freshwater"),
    exposure_route == "sediment",
    Group != "Bacterium",
    effect.metric != "HONEC",
    tier_zero_tech_f == "Red Criteria Passed",
    tier_zero_risk_f == "Red Criteria Passed",
    risk.13 != 0
  ) |> 
    # since limited data available, and working with estuarine system - combine marine and freshwater species by overwriting environment to be freshwater for all
    mutate(env_f = "Freshwater")
#
#
#
#
#
#
R.ave_sed_vals <- raw_particles_sed$aspect_ratio
R.ave_sed_vals <- R.ave_sed_vals[is.finite(R.ave_sed_vals) & R.ave_sed_vals > 0]
R.ave_sed_vals <- 1 / R.ave_sed_vals

R.ave_sed_boot <- replicate(n_boot, {
  xb <- sample(R.ave_sed_vals, size = length(R.ave_sed_vals), replace = TRUE)
  mean(xb)
})

R.ave_sed_summary <- tibble(
  p05 = quantile(R.ave_sed_vals, 0.05),
  p50 = quantile(R.ave_sed_vals, 0.50),
  p95 = quantile(R.ave_sed_vals, 0.95),
  mean = mean(R.ave_sed_vals),
  sd = sd(R.ave_sed_vals),
  boot_mean = mean(R.ave_sed_boot),
  boot_sd = sd(R.ave_sed_boot)
)

R.ave_sed_mean <- R.ave_sed_summary$boot_mean
R.ave_sed_sd <- R.ave_sed_summary$boot_sd

sed_env_lower <- tolower(sed_env)
param_values_sed <- PSSDplusplus::param_default_values

param_values_sed[[paste0("alpha.sediment.", sed_env_lower)]] <- -alpha_sed_mu
param_values_sed[[paste0("alpha.sediment.", sed_env_lower, ".sd")]] <- alpha_sed_se
param_values_sed[[paste0("R.ave.sediment.", sed_env_lower)]] <- R.ave_sed_mean
param_values_sed[[paste0("R.ave.sediment.", sed_env_lower, ".sd")]] <- R.ave_sed_sd * 100
param_values_sed[[paste0("a.v.sediment.", sed_env_lower)]] <- -cpsd_fit_volume_sed$a_cpsd
param_values_sed[[paste0("a.v.sediment.", sed_env_lower, ".sd")]] <- cpsd_fit_volume_sed$se_a_cpsd
param_values_sed[[paste0("a.sa.sediment.", sed_env_lower)]] <- -cpsd_fit_area_sed$a_cpsd
param_values_sed[[paste0("a.sa.sediment.", sed_env_lower, ".sd")]] <- cpsd_fit_area_sed$se_a_cpsd
#
#
#
#
param_matrix_sed <- matrix_function(
  n = n_boot,
  params = param_values_sed,
  upper.tissue.truncation.limit = 500,
  x1M_set = 1,
  x2D_set = 5000
)

sed_R_col <- rlang::sym(paste0("R.ave.sediment.", sed_env_lower))
sed_HW_col <- rlang::sym(paste0("H_W_ratio.sediment.", sed_env_lower))

param_matrix_sed <- param_matrix_sed |>
  mutate(
    !!sed_R_col := sample(R.ave_sed_vals, size = n(), replace = TRUE),
    !!sed_HW_col := sample(R.ave_sed_vals, size = n(), replace = TRUE)
  )

MC_sim_df_sed <- MC_sim_align_parallel(
  tox_data = tox_data_sed,
  param_matrix = param_matrix_sed,
  n_sim = n_boot,
  x1D_set = 1,
  x2D_set = 5000,
  num_cores = parallel::detectCores() - 2
)

results_df_food_sed <- dplyr::filter(
  MC_sim_df_sed,
  ingestible != "not ingestible",
  particles_kg_food_dilution > 0,
  Group != "Algae"
) |>
  dplyr::mutate(dose_new_particles_kg = particles_kg_food_dilution) |>
  tidyr::drop_na(particles_kg_food_dilution)
results_df_food_sed_t3_t4 <- dplyr::filter(
  results_df_food_sed,
  risk.13 != 1,
  bio_f %in% c("Organism", "Population")
)

results_df_tissue_sed <- dplyr::filter(
  MC_sim_df_sed,
  translocatable != "not translocatable",
  particles_kg_ox_stress > 0
) |>
  dplyr::mutate(dose_new_particles_kg = particles_kg_ox_stress) |>
  tidyr::drop_na(particles_kg_ox_stress)
results_df_tissue_sed_t3_t4 <- dplyr::filter(
  results_df_tissue_sed,
  risk.13 != 1,
  bio_f %in% c("Organism", "Population")
)

erm_registry_sed <- list(
  "Food Dilution" = list(base = results_df_food_sed, t3_t4 = results_df_food_sed_t3_t4),
  "Tissue Translocation" = list(base = results_df_tissue_sed, t3_t4 = results_df_tissue_sed_t3_t4)
)
#
#
#
#
pSSDs_sed <- make_all_pSSDs(
  MC_sim_df = MC_sim_df_sed,
  environments = c("Freshwater Sediment"),
  erm_registry = erm_registry_sed,
  sim = 30,
  cv_uf = 0.5,
  rmore_method = "lognormal",
  parallel = TRUE,
  workers = parallel::detectCores() - 2,
  base_cache_dir = file.path(tempdir(), "pssd_cache_sediment"),
  base_output_path = file.path(tempdir(), "pssd_figures_sediment"),
  overwrite_cache = TRUE
)
#
#
#
#
#
pSSDs_sed$`Tier3_Freshwater Sediment_Food Dilution`$pSSD_plot
#
#
#
#
#
pssd_key_food_sed <- paste0("Tier3_", sed_env, "_Food Dilution")
pssd_key_tissue_sed <- paste0("Tier3_", sed_env, "_Tissue Translocation")

haz_sed_HC5_food <- pSSDs_sed[[pssd_key_food_sed]]$summary_05$df |> 
  mutate(HCx = 5, ERM = "Food Dilution")
haz_sed_HC10_food <- pSSDs_sed[[pssd_key_food_sed]]$summary_10$df |> 
  mutate(HCx = 10, ERM = "Food Dilution")

haz_sed_HC5_tissue <- pSSDs_sed[[pssd_key_tissue_sed]]$summary_05$df |> 
  mutate(HCx = 5, ERM = "Tissue Translocation")
haz_sed_HC10_tissue <- pSSDs_sed[[pssd_key_tissue_sed]]$summary_10$df |> 
  mutate(HCx = 10, ERM = "Tissue Translocation")

haz_sed <- bind_rows(haz_sed_HC5_food, haz_sed_HC10_food, haz_sed_HC5_tissue, haz_sed_HC10_tissue)

n_risk_sed <- 1000
risk_draws_sed <- draw_rq_mc1d(haz_sed, eed_boot_sed$q50, n_risk = n_risk_sed)
risk_summary_sed <- summarize_rq(risk_draws_sed)
risk_summary_sed
#
#
#
#
