# cpsd_plotting.R
# Visualization functions for C-PSD (cumulative particle size distribution) fits.
# Requires: ggplot2, dplyr, tibble

# ── Colour palette ────────────────────────────────────────────────────────────

#' Standard colour palette for microplastic shape categories
#'
#' Named character vector mapping shape labels to hex colours. Includes "all"
#' for pooled datasets. Matches the existing project palette in Util.R and
#' extends it with an "all" entry.
MP_PALETTE <- c(
  all      = "#999999",
  fiber    = "#E69F00",
  fragment = "#56B4E9",
  film     = "#009E73",
  nurdle   = "#F0E442",
  foam     = "#0072B2",
  other    = "#D55E00"
)


# ── C-PSD multi-shape plot ────────────────────────────────────────────────────

#' Plot C-PSD fits for multiple shape categories
#'
#' Produces a faceted log-log plot of cumulative counts N(>=L) vs lower bin
#' bound L_low, overlaying the fitted power-law with a 95% confidence band
#' restricted to the valid LOD window. Vertical dotted lines mark the LOD
#' boundaries. Points outside the fitted LOD window are shown at reduced
#' opacity. The fit equation label is anchored to the top-right corner of each
#' facet so it remains visible regardless of the data range.
#'
#' @param fits Named list of fit_cpsd_segur_r() outputs; list names are used as
#'   shape/group labels and matched to the colour palette.
#' @param title Character plot title.
#' @param palette Named colour vector (default MP_PALETTE).
#' @param attribute Label for the x-axis (e.g., "Length", "Area", "Volume").
#'
#' @return A ggplot object.
#'
#' @examples
#' \dontrun{
#' fits <- list(fragment = cpsd_fit_frag, fiber = cpsd_fit_fiber, all = cpsd_fit_all)
#' plot_cpsd_multi(fits, title = "River water C-PSD", attribute = "Length")
#' }
plot_cpsd_multi <- function(fits,
                             title     = "",
                             palette   = MP_PALETTE,
                             attribute = "Length") {
  stopifnot(is.list(fits), length(fits) > 0)

  # Full C-PSD data (all bins) with an in_lod flag for opacity
  df_all <- dplyr::bind_rows(lapply(names(fits), function(grp) {
    fo <- fits[[grp]]
    lower_lod_plot <- if (!is.null(fo$lower_lod_used_um) && is.finite(fo$lower_lod_used_um))
      fo$lower_lod_used_um else fo$lower_lod_um
    fo$bins |>
      dplyr::arrange(L_low) |>
      dplyr::mutate(
        N_ge   = rev(cumsum(rev(n))),
        shape  = grp,
        in_lod = is.finite(lower_lod_plot) & is.finite(fo$upper_lod_um) &
                 L_low >= lower_lod_plot & L_high <= fo$upper_lod_um
      ) |>
      dplyr::filter(L_low > 0, N_ge > 0)
  }))

  # Per-group fit parameters for equation labels
  params <- dplyr::bind_rows(lapply(names(fits), function(grp) {
    fo         <- fits[[grp]]
    r_squared  <- summary(fo$fit)$r.squared
    lod_lo     <- if (!is.null(fo$lower_lod_used_um) && is.finite(fo$lower_lod_used_um))
                    fo$lower_lod_used_um else fo$lower_lod_um
    data.frame(
      shape            = grp,
      a_cpsd           = fo$a_cpsd,
      se_a_cpsd        = fo$se_a_cpsd,
      r_squared        = r_squared,
      lower_lod_um     = lod_lo,
      upper_lod_um     = fo$upper_lod_um,
      stringsAsFactors = FALSE
    )
  })) |>
    dplyr::mutate(
      fit_label = sprintf(
        "atop(italic(a)[cpsd]==%.2f~(SE==%.3f), italic(R)^2==%.3f~~'LOD:'~%d*'-'*%d~mu*m)",
        a_cpsd, se_a_cpsd, r_squared,
        round(lower_lod_um), round(upper_lod_um)
      )
    )

  # 95% CI prediction band restricted to the valid LOD window only
  pred_lod <- dplyr::bind_rows(lapply(names(fits), function(grp) {
    fo     <- fits[[grp]]
    lod_lo <- if (!is.null(fo$lower_lod_used_um) && is.finite(fo$lower_lod_used_um))
                 fo$lower_lod_used_um else fo$lower_lod_um
    grid <- tibble::tibble(
      L_low = seq(lod_lo, fo$upper_lod_um, length.out = 200)
    )
    pred <- predict(fo$fit, newdata = grid, se.fit = TRUE)
    tibble::tibble(
      shape    = grp,
      L_low    = grid$L_low,
      logN_fit = pred$fit,
      logN_lo  = pred$fit - 1.96 * pred$se.fit,
      logN_hi  = pred$fit + 1.96 * pred$se.fit
    )
  }))

  # LOD boundary positions for vertical dotted lines (one row per boundary per shape)
  lod_bounds <- dplyr::bind_rows(lapply(names(fits), function(grp) {
    fo     <- fits[[grp]]
    lod_lo <- if (!is.null(fo$lower_lod_used_um) && is.finite(fo$lower_lod_used_um))
                 fo$lower_lod_used_um else fo$lower_lod_um
    data.frame(
      shape = c(grp, grp),
      xval  = c(log10(lod_lo), log10(fo$upper_lod_um)),
      stringsAsFactors = FALSE
    )
  }))

  ggplot2::ggplot(df_all, ggplot2::aes(x = log10(L_low), y = log10(N_ge), color = shape)) +
    # LOD boundary lines drawn first (behind data points)
    ggplot2::geom_vline(
      data        = lod_bounds,
      ggplot2::aes(xintercept = xval),
      linetype    = "dotted",
      color       = "gray50",
      linewidth   = 0.65,
      inherit.aes = FALSE
    ) +
    # Data points (reduced opacity outside LOD window)
    ggplot2::geom_point(ggplot2::aes(alpha = in_lod), size = 2) +
    # 95% CI ribbon restricted to LOD window
    ggplot2::geom_ribbon(
      data        = pred_lod,
      ggplot2::aes(x = log10(L_low), ymin = logN_lo, ymax = logN_hi, fill = shape),
      inherit.aes = FALSE,
      alpha       = 0.2,
      color       = NA
    ) +
    # Regression line restricted to LOD window
    ggplot2::geom_line(
      data        = pred_lod,
      ggplot2::aes(x = log10(L_low), y = logN_fit, color = shape),
      inherit.aes = FALSE,
      linewidth   = 1
    ) +
    # Equation label anchored to top-right corner of each facet panel.
    # x = Inf / y = Inf positions at the panel edge; hjust/vjust pull it inward.
    ggplot2::geom_text(
      data        = params,
      ggplot2::aes(label = fit_label),
      x           = Inf,
      y           = Inf,
      hjust       = 1.05,
      vjust       = 1.4,
      color       = "gray20",
      inherit.aes = FALSE,
      size        = 3.2,
      parse       = TRUE
    ) +
    ggplot2::facet_wrap(~ shape, scales = "free") +
    ggplot2::scale_color_manual(values = palette, drop = FALSE) +
    ggplot2::scale_fill_manual(values  = palette, drop = FALSE) +
    ggplot2::scale_alpha_manual(values = c(`TRUE` = 0.8, `FALSE` = 0.15), guide = "none") +
    ggplot2::labs(
      title  = title,
      x      = bquote(log[10] * "(" * .(attribute) * "," ~ mu * "m)"),
      y      = expression(log[10] * (N("">=L))),
      color  = NULL,
      fill   = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position  = "none",
      strip.text       = ggplot2::element_text(face = "bold", size = 13),
      panel.border     = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.4),
      plot.margin      = ggplot2::margin(8, 14, 8, 8)
    )
}
