# risk_plotting.R
# Matrix-aware plotting helpers for probabilistic risk characterization figures.
# Every plotting function here takes a `matrix_label` argument (e.g. "River water",
# "Sediment", "Ocean water") and bakes it into the title/subtitle so a figure can
# never be produced without stating which environmental matrix it represents.
# Requires: ggplot2, dplyr, tibble

# ── Risk quotient histogram ────────────────────────────────────────────────────

#' Plot a 1D Monte Carlo risk quotient (RQ) histogram
#'
#' @param risk_draws Output of draw_rq_mc1d() (columns ERM, HCx, RQ).
#' @param matrix_label Character label for the matrix (e.g. "River water").
#' @return A ggplot object.
plot_rq_hist <- function(risk_draws, matrix_label) {
  ggplot2::ggplot(risk_draws, ggplot2::aes(x = RQ, fill = as.factor(ERM))) +
    ggplot2::geom_histogram(bins = 60) +
    ggplot2::scale_x_log10() +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::facet_grid(HCx ~ ERM, scales = "free_y",
                        labeller = ggplot2::labeller(HCx = function(x) paste0("HC", x))) +
    ggplot2::labs(
      x        = "Risk Quotient (Exposure / Hazard) [log10 scale]",
      y        = "count",
      title    = "Monte Carlo risk quotient distributions by ERM and HCx",
      subtitle = matrix_label
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(legend.position = "none")
}

#' Combined risk quotient comparison across matrices
#'
#' Row-binds a named list of draw_rq_mc1d() outputs (one per matrix), tagging
#' each with a Matrix column, and facets by Matrix (rows) x ERM (columns) so
#' every panel is self-labeled by matrix.
#'
#' @param risk_draws_list Named list of draw_rq_mc1d() outputs, e.g.
#'   list("River water" = risk_draws, "Sediment" = risk_draws_sed,
#'        "Ocean water" = risk_draws_ocean). Names become the Matrix facet labels.
#' @return A ggplot object.
plot_rq_hist_combined <- function(risk_draws_list) {
  stopifnot(is.list(risk_draws_list), length(risk_draws_list) > 0)

  combined <- dplyr::bind_rows(
    lapply(names(risk_draws_list), function(mat) {
      dplyr::mutate(risk_draws_list[[mat]], Matrix = mat)
    })
  )
  combined$Matrix <- factor(combined$Matrix, levels = names(risk_draws_list))

  ggplot2::ggplot(combined, ggplot2::aes(x = RQ, fill = as.factor(HCx))) +
    ggplot2::geom_histogram(bins = 60, position = "identity", alpha = 0.6) +
    ggplot2::scale_x_log10() +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::facet_grid(Matrix ~ ERM, scales = "free_y") +
    ggplot2::scale_fill_manual(values = c(`5` = "#56B4E9", `10` = "#E69F00"), name = "HCx") +
    ggplot2::labs(
      x        = "Risk Quotient (Exposure / Hazard) [log10 scale]",
      y        = "count",
      title    = "1D Monte Carlo risk quotient comparison across matrices",
      subtitle = "Rows = matrix, columns = exposure route model (ERM). Dashed line = RQ = 1."
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      legend.position = "bottom",
      strip.text      = ggplot2::element_text(face = "bold"),
      panel.border    = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.4)
    )
}


# ── Hazard threshold histogram ─────────────────────────────────────────────────

#' Plot the HC5/HC10 hazard threshold distribution
#'
#' @param haz_df Data frame with columns PNEC, HCx, ERM (build_haz_df() output).
#' @param matrix_label Character label for the matrix (e.g. "Sediment").
#' @return A ggplot object.
plot_hazard_threshold <- function(haz_df, matrix_label) {
  ggplot2::ggplot(haz_df, ggplot2::aes(x = PNEC, fill = as.factor(ERM))) +
    ggplot2::geom_histogram(bins = 15) +
    ggplot2::facet_wrap(~ HCx + ERM, ncol = 2, scales = "free_y",
                        labeller = ggplot2::labeller(HCx = function(x) paste0("HC", x))) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x        = "HC5 (particles/L)",
      y        = "count",
      title    = "Hazard threshold distribution",
      subtitle = matrix_label
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(legend.position = "none")
}


# ── EED bootstrap histogram ────────────────────────────────────────────────────

#' Plot bootstrap uncertainty in EED percentiles
#'
#' @param eed_boot Output of bootstrap_eed() (columns q50, q95, ...).
#' @param n_boot Number of bootstrap replicates (for the subtitle text).
#' @param matrix_label Character label for the matrix (e.g. "Ocean water").
#' @return A ggplot object.
plot_eed_bootstrap <- function(eed_boot, n_boot, matrix_label) {
  eed_boot_long <- eed_boot |>
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "stat", values_to = "value") |>
    dplyr::mutate(
      stat_label = dplyr::case_when(
        stat == "q50" ~ "EED 50th percentile",
        stat == "q95" ~ "EED 95th percentile",
        TRUE          ~ stat
      )
    )

  ggplot2::ggplot(eed_boot_long, ggplot2::aes(x = value, fill = stat_label, color = stat_label)) +
    ggplot2::geom_histogram(bins = 40, alpha = 0.75, linewidth = 0.2) +
    ggplot2::facet_wrap(~stat_label, scales = "free", ncol = 2) +
    ggplot2::scale_fill_manual(values  = c("EED 50th percentile" = "#56B4E9",
                                           "EED 95th percentile" = "#E69F00")) +
    ggplot2::scale_color_manual(values = c("EED 50th percentile" = "#2A7FAD",
                                           "EED 95th percentile" = "#BF7A00")) +
    ggplot2::labs(
      x        = "Corrected concentration (particles/L)",
      y        = "Bootstrap replicates",
      title    = "Bootstrap uncertainty in Environmental Exposure Distribution (EED) percentiles",
      subtitle = paste0(matrix_label, " — n = ", n_boot, " bootstrap replicates of site-level median corrected concentrations"),
      caption  = "Correction factor (CF) propagated via Monte Carlo; each replicate resamples sites with replacement."
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      legend.position  = "none",
      strip.text       = ggplot2::element_text(face = "bold"),
      panel.border     = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.4),
      plot.caption     = ggplot2::element_text(size = 9, color = "gray50", hjust = 0)
    )
}


# ── ECDF exposure/hazard overlap ───────────────────────────────────────────────

#' Plot the EED CDF alongside HCx CDFs with bootstrap uncertainty ribbons
#'
#' Wraps the ecdf_bands() computation (from mp_risk_utils.R) for both the
#' exposure distribution and each ERM x HCx hazard group, on a common x-grid,
#' then overlays them as a combined ECDF plot.
#'
#' @param eed_vals Numeric vector of exposure concentrations (e.g. sample medians).
#' @param haz_df Data frame with columns PNEC, ERM, HCx.
#' @param matrix_label Character label for the matrix (e.g. "River water").
#' @param n_boot Bootstrap replicates for ecdf_bands().
#' @param grid_n Number of points in the common x-grid (default 200).
#' @return A ggplot object.
plot_ecdf_overlap <- function(eed_vals, haz_df, matrix_label, n_boot, grid_n = 200) {
  all_x  <- c(eed_vals, haz_df$PNEC)
  grid_x <- 10^seq(log10(min(all_x, na.rm = TRUE)),
                   log10(max(all_x, na.rm = TRUE)),
                   length.out = grid_n)

  eed_band <- ecdf_bands(eed_vals, grid_x, n_boot = n_boot) |>
    dplyr::mutate(source = "EED")

  haz_band <- haz_df |>
    dplyr::group_by(ERM, HCx) |>
    dplyr::group_modify(~{
      ecdf_bands(.x$PNEC, grid_x, n_boot = n_boot) |>
        dplyr::mutate(source = .y$ERM)
    }) |>
    dplyr::ungroup()

  erm_levels  <- sort(unique(haz_df$ERM))
  erm_colors  <- stats::setNames(c("#1F78B4", "#FF7F00")[seq_along(erm_levels)], erm_levels)
  source_colors <- c("EED" = "forestgreen", erm_colors)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = eed_band,
      ggplot2::aes(x = x, ymin = cdf_lo, ymax = cdf_hi, fill = source),
      alpha = 0.15
    ) +
    ggplot2::geom_line(
      data = eed_band,
      ggplot2::aes(x = x, y = cdf_med, color = source, linetype = "EED"),
      linewidth = 1.1
    ) +
    ggplot2::geom_ribbon(
      data = haz_band,
      ggplot2::aes(x = x, ymin = cdf_lo, ymax = cdf_hi, fill = source),
      alpha = 0.12
    ) +
    ggplot2::geom_line(
      data = haz_band,
      ggplot2::aes(x = x, y = cdf_med, color = source, linetype = paste0("HC", HCx)),
      linewidth = 1
    ) +
    ggplot2::scale_x_log10(
      labels = scales::label_log(),
      name   = "Concentration (particles/L, log₁₀ scale)"
    ) +
    ggplot2::scale_color_manual(values = source_colors, breaks = names(source_colors), name = "Distribution") +
    ggplot2::scale_fill_manual(values  = source_colors, breaks = names(source_colors), guide = "none") +
    ggplot2::scale_linetype_manual(
      values = c("EED" = "solid", "HC5" = "dashed", "HC10" = "dotted"),
      name   = "Threshold / percentile"
    ) +
    ggplot2::labs(
      y        = "Cumulative probability",
      title    = "Overlap of Exposure and Hazard distributions",
      subtitle = paste0(matrix_label, " — EED = Environmental Exposure Distribution; HC5/HC10 = Hazard Concentration (5th/10th percentile of SSD)"),
      caption  = paste0("Shaded bands = bootstrap 95% CI (n = ", n_boot, " replicates). Log₁₀ x-axis.")
    ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(
      legend.position  = "right",
      legend.box       = "vertical",
      panel.border     = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.4),
      plot.caption     = ggplot2::element_text(size = 9, color = "gray50", hjust = 0)
    )
}


# ── MC2D diagnostic plots (river-only) ─────────────────────────────────────────

#' Plot one of the four MC2D uncertainty diagnostic figures
#'
#' @param mc2d_df Output of mc2d_risk().
#' @param matrix_label Character label for the matrix (e.g. "River water").
#' @param type One of "pexceed_hist", "pexceed_boxplot", "cf_scatter", "rq_density".
#' @return A ggplot object.
plot_mc2d_diagnostic <- function(mc2d_df, matrix_label,
                                  type = c("pexceed_hist", "pexceed_boxplot", "cf_scatter", "rq_density")) {
  type <- match.arg(type)

  if (type == "pexceed_hist") {
    return(
      ggplot2::ggplot(mc2d_df, ggplot2::aes(x = P_exceed, fill = ERM)) +
        ggplot2::geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
        ggplot2::facet_grid(HCx ~ ERM, scales = "free_y",
                            labeller = ggplot2::labeller(HCx = function(x) paste0("HC", x))) +
        ggplot2::labs(x = "P(RQ > 1) across uncertainty", y = "count",
                      title = "MC2D uncertainty distribution for exceedance probability",
                      subtitle = matrix_label) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::theme(legend.position = "none")
    )
  }

  if (type == "pexceed_boxplot") {
    return(
      ggplot2::ggplot(mc2d_df, ggplot2::aes(x = as.factor(HCx), y = P_exceed, fill = ERM)) +
        ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.4) +
        ggplot2::scale_x_discrete(labels = function(x) paste0("HC", x)) +
        ggplot2::labs(x = "HCx", y = "P(RQ > 1)",
                      title = "MC2D uncertainty spread in exceedance probability",
                      subtitle = matrix_label) +
        ggplot2::theme_minimal(base_size = 20)
    )
  }

  if (type == "cf_scatter") {
    return(
      ggplot2::ggplot(mc2d_df, ggplot2::aes(x = cf, y = P_exceed, color = ERM)) +
        ggplot2::geom_point(alpha = 0.4, size = 1.2) +
        ggplot2::geom_smooth(method = "loess", se = FALSE) +
        ggplot2::facet_wrap(~HCx, scales = "free_y",
                            labeller = ggplot2::labeller(HCx = function(x) paste0("HC", x))) +
        ggplot2::labs(x = "Correction factor draw", y = "P(RQ > 1)",
                      title = "Relationship between correction factor and exceedance",
                      subtitle = matrix_label) +
        ggplot2::theme_minimal(base_size = 20) +
        ggplot2::theme(legend.position = "none")
    )
  }

  # type == "rq_density"
  mc2d_long <- tidyr::pivot_longer(mc2d_df, cols = c(RQ_p50, RQ_p95, RQ_p99),
                                    names_to = "stat", values_to = "value")
  ggplot2::ggplot(mc2d_long, ggplot2::aes(x = value, fill = ERM)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::facet_grid(stat ~ HCx, scales = "free",
                        labeller = ggplot2::labeller(HCx = function(x) paste0("HC", x))) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(x = "Risk Quotient", y = "density",
                  title = "MC2D uncertainty distribution for RQ summaries (log10 scale)",
                  subtitle = matrix_label) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::theme(legend.position = "top")
}


# ── Matrix-label helper for package-generated plots ────────────────────────────

#' Append a matrix-name subtitle to an existing plot object
#'
#' For plots produced by external functions (e.g. PSSDplusplus's pSSD_plot,
#' PNEC_plot, arranged_plot, alpha_combined_plot) that don't accept a title
#' argument, this appends a subtitle stating the matrix without altering the
#' rest of the plot.
#'
#' @param plot_obj A ggplot (or ggplot-compatible, e.g. ggpubr::ggarrange) object.
#' @param matrix_label Character label for the matrix (e.g. "Sediment").
#' @return The plot object with an added/overridden subtitle.
label_matrix <- function(plot_obj, matrix_label) {
  plot_obj + ggplot2::labs(subtitle = matrix_label)
}
