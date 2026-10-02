# si_pdf_tables.R
# Supplementary-information tables for every particle-size PDF fit in
# probabilistic_risk_characterization.qmd, formatted after Kooi et al. (2021,
# Water Research 198:117011) SI Tables S3 (particle properties) and S4 (fitted
# minimum values and power-law exponents). Read-only: every function here
# summarizes raw particles or fit objects already produced in the .qmd; the
# only fitting done is the deterministic depth-stratified length refit in
# si_depth_stratified_fits(), which reproduces the Section 10.0 fits with
# their full fit objects retained.

#' Kooi-style particle property summary statistics
#'
#' @param df Raw particle table for one matrix (needs shape, length_um,
#'   width_um, aspect_ratio, area_um2, V_um3, density_g_cm3).
#' @param matrix_label Matrix name carried into the output.
#' @return Long tibble: Matrix, Property, Unit, Shape, n, median, q25, q75,
#'   mean, sd. Shape "all" pools every particle in `df`.
si_particle_properties <- function(df, matrix_label) {
  props <- df |>
    dplyr::transmute(
      shape,
      `Length`               = length_um,
      `Width`                = width_um,
      `Width-to-length ratio` = 1 / aspect_ratio,
      `Projected area`       = area_um2,
      `Volume`               = V_um3,
      # V (um^3) x rho (g/cm^3) = pg; x 1e-6 -> ug (Kooi et al. 2021 units)
      `Mass`                 = V_um3 * density_g_cm3 * 1e-6,
      `Density`              = density_g_cm3
    )
  units <- c(`Length` = "µm", `Width` = "µm", `Width-to-length ratio` = "–",
             `Projected area` = "µm²", `Volume` = "µm³", `Mass` = "µg",
             `Density` = "g/cm³")

  long <- dplyr::bind_rows(props, dplyr::mutate(props, shape = "all")) |>
    tidyr::pivot_longer(-shape, names_to = "Property", values_to = "value") |>
    dplyr::filter(is.finite(value))

  long |>
    dplyr::group_by(Property, Shape = shape) |>
    dplyr::summarise(
      n      = dplyr::n(),
      median = stats::median(value),
      q25    = stats::quantile(value, 0.25, names = FALSE),
      q75    = stats::quantile(value, 0.75, names = FALSE),
      mean   = mean(value),
      sd     = stats::sd(value),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      Matrix   = matrix_label,
      Unit     = unname(units[Property]),
      Property = factor(Property, levels = names(units)),
      Shape    = factor(Shape, levels = c("fragment", "fiber", "all"))
    ) |>
    dplyr::arrange(Property, Shape) |>
    dplyr::mutate(Property = as.character(Property), Shape = as.character(Shape)) |>
    dplyr::select(Matrix, Property, Unit, Shape, n, median, q25, q75, mean, sd)
}

#' One row of C-PSD fit parameters from a fit_cpsd_segur_r() output
#'
#' @param fo Fit object (fit_cpsd_segur_r() / fit_cpsd_by_shape() element,
#'   or a safe_cpsd() result carrying valid = FALSE when no fit was possible).
#' @param n_particles Number of particles supplied to the fit.
#' @param bin_width Bin width used (same units as the metric).
#' @return One-row tibble of numeric fit parameters (NA when no fit).
si_cpsd_fit_row <- function(fo, n_particles, bin_width) {
  if (is.null(fo) || isFALSE(fo$valid) || is.null(fo$a_cpsd)) {
    return(tibble::tibble(
      n_particles = n_particles, n_in_window = NA_integer_, bin_width = bin_width,
      x_min = NA_real_, x_max = NA_real_, a_cpsd = NA_real_, se_a_cpsd = NA_real_,
      a_psd = NA_real_, b_cpsd = NA_real_, se_b_cpsd = NA_real_, r2 = NA_real_,
      n_bins = NA_integer_, fit_status = "no fit"
    ))
  }
  in_win <- fo$bins$L_low >= fo$lower_lod_used_um & fo$bins$L_low < fo$upper_lod_um
  tibble::tibble(
    n_particles = n_particles,
    n_in_window = as.integer(sum(fo$bins$n[in_win])),
    bin_width   = bin_width,
    x_min       = fo$lower_lod_used_um,
    x_max       = fo$upper_lod_um,
    a_cpsd      = fo$a_cpsd,
    se_a_cpsd   = fo$se_a_cpsd,
    a_psd       = fo$a_psd,
    b_cpsd      = fo$b_cpsd,
    se_b_cpsd   = fo$se_b_cpsd,
    r2          = fo$r2,
    n_bins      = as.integer(fo$n_bins),
    fit_status  = "ok"
  )
}

#' Tabulate a named list of shape fits (fragment/fiber/all) for one metric
#'
#' @param fits Named list of fit objects keyed by shape.
#' @param df Raw particle table the fits were drawn from (to count n).
#' @param value_col Column fitted (length_um, area_um2, V_um3).
#' @param bin_width Bin width used.
#' @param matrix_label,metric_label,subset_label Labels for the output.
si_cpsd_fit_rows <- function(fits, df, value_col, bin_width,
                             matrix_label, metric_label, subset_label) {
  dplyr::bind_rows(lapply(names(fits), function(sh) {
    vals <- if (sh == "all") df[[value_col]] else df[[value_col]][df$shape == sh]
    n <- sum(is.finite(vals) & vals > 0)
    dplyr::bind_cols(
      tibble::tibble(Matrix = matrix_label, Metric = metric_label,
                     Subset = subset_label, Shape = sh),
      si_cpsd_fit_row(fits[[sh]], n, bin_width)
    )
  }))
}

#' Depth-stratified (river surface/subsurface) length C-PSD fits
#'
#' Reproduces the Section 10.0 hoffman-depth-stratified-sensitivity fits
#' (same data subsets, same bin_um = 5 um, auto-detected LOD window) but
#' keeps the full fit objects so intercepts, n_bins, and window counts can
#' be reported. Deterministic, so results equal depth_shape_fits exactly.
si_depth_stratified_fits <- function(df, depth_levels) {
  stats::setNames(lapply(depth_levels, function(d) {
    sub <- dplyr::filter(df, sample_depth_general == d)
    list(
      fragment = fit_cpsd_segur_r(sub$length_um[sub$shape == "fragment"], bin_um = 5),
      fiber    = fit_cpsd_segur_r(sub$length_um[sub$shape == "fiber"],    bin_um = 5),
      all      = fit_cpsd_segur_r(sub$length_um,                          bin_um = 5)
    )
  }), depth_levels)
}

#' Format numeric fit-parameter table into a Kooi-style "estimate ± SE" table
#'
#' @param tbl Output of si_cpsd_fit_rows() (possibly row-bound across groups).
#' @param include_matrix Keep the Matrix column (for multi-matrix tables).
si_format_cpsd_table <- function(tbl, include_matrix = FALSE) {
  fmt_x <- function(x) ifelse(is.na(x), "–",
    ifelse(abs(x) >= 1e4, formatC(x, format = "e", digits = 2),
           formatC(x, format = "fg", digits = 3, big.mark = ",")))
  pm <- function(est, se, d = 2) ifelse(is.na(est), "–",
    paste0(formatC(est, format = "f", digits = d), " ± ", formatC(se, format = "f", digits = d)))
  out <- tbl |>
    dplyr::transmute(
      Matrix, Metric, Subset, Shape,
      `n` = n_particles,
      `n in window` = ifelse(is.na(n_in_window), "–", as.character(n_in_window)),
      `Bin width` = fmt_x(bin_width),
      `x_min (lower LOD)` = fmt_x(x_min),
      `x_max (upper LOD)` = fmt_x(x_max),
      `α_CPSD ± SE` = pm(a_cpsd, se_a_cpsd),
      `a_PSD ± SE` = pm(a_psd, se_a_cpsd),
      `log10 b ± SE` = pm(b_cpsd, se_b_cpsd),
      `R²` = ifelse(is.na(r2), "–", formatC(r2, format = "f", digits = 3)),
      `n bins` = ifelse(is.na(n_bins), "–", as.character(n_bins))
    )
  if (!include_matrix) out <- dplyr::select(out, -Matrix)
  out
}

#' Format the particle-property table (Kooi Table S3 layout)
si_format_property_table <- function(tbl, include_matrix = FALSE) {
  fmt <- function(x, prop) {
    ifelse(prop %in% c("Width-to-length ratio", "Density"),
           formatC(x, format = "f", digits = 2),
           ifelse(abs(x) >= 1e4 | abs(x) < 1e-2,
                  formatC(x, format = "e", digits = 2),
                  formatC(x, format = "f", digits = 2)))
  }
  out <- tbl |>
    dplyr::mutate(
      Property = paste0(Property, " (", Unit, ")"),
      dplyr::across(c(median, q25, q75, mean, sd), ~ fmt(.x, sub(" \\(.*", "", Property)))
    ) |>
    dplyr::select(Matrix, Property, Shape, n,
                  Median = median, `Lower quartile (Q25)` = q25,
                  `Upper quartile (Q75)` = q75, Mean = mean, SD = sd)
  if (!include_matrix) out <- dplyr::select(out, -Matrix)
  out
}

#' Write SI Tables S5-S8 to a Word document with variable definitions
#'
#' @param path Output .docx path.
#' @param props_river,props_ocean_sed si_particle_properties() outputs.
#' @param fits_river,fits_ocean_sed si_cpsd_fit_rows() outputs (row-bound).
#' @param alt_river,alt_ocean_sed Alternative-model tables (already formatted).
#' @param n_river,n_ocean,n_sed Particle counts per matrix, for captions.
si_write_docx <- function(path, props_river, props_ocean_sed, fits_river, fits_ocean_sed,
                          alt_river, alt_ocean_sed, n_river, n_ocean, n_sed) {
  fp_txt  <- officer::fp_text(font.family = "Calibri", font.size = 11)
  fp_bold <- update(fp_txt, bold = TRUE)
  fmt_n <- function(x) format(x, big.mark = ",")

  txt  <- function(x) officer::ftext(x, fp_txt)
  bold <- function(x) officer::ftext(x, fp_bold)
  sub  <- function(x) officer::ftext(x, update(fp_txt, vertical.align = "subscript"))
  sup  <- function(x) officer::ftext(x, update(fp_txt, vertical.align = "superscript"))
  para <- function(doc, ...) {
    officer::body_add_fpar(doc, officer::fpar(..., fp_p = officer::fp_par(padding.bottom = 6)))
  }
  # Definition entry: bold term (may include sub/superscript runs), dash, explanation
  defn <- function(doc, term, ...) {
    term <- if (is.character(term)) list(bold(term)) else term
    do.call(para, c(list(doc), term, list(txt(" — ")), list(...)))
  }
  bsub <- function(x) officer::ftext(x, update(fp_bold, vertical.align = "subscript"))
  heading <- function(doc, x, size) {
    officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(x, update(fp_bold, font.size = size)),
      fp_p = officer::fp_par(padding.top = 10, padding.bottom = 6, keep_with_next = TRUE)))
  }

  # Shared flextable styling; merges repeated Matrix/Property/Metric/Subset cells
  style_ft <- function(df, merge_cols, page_width = 9.5) {
    ft <- flextable::flextable(df) |>
      flextable::theme_booktabs() |>
      flextable::font(fontname = "Calibri", part = "all") |>
      flextable::fontsize(size = 8, part = "all") |>
      flextable::bold(part = "header") |>
      flextable::bg(bg = "#D9E1F2", part = "header") |>
      flextable::align(align = "center", part = "header") |>
      flextable::valign(valign = "top", part = "body") |>
      flextable::padding(padding = 1.5, part = "all")
    label_cols <- intersect(c("Matrix", "Property", "Metric", "Subset", "Shape", "Model",
                              "Parameters", "Fit statistics"), names(df))
    value_cols <- setdiff(names(df), label_cols)
    ft <- flextable::align(ft, j = label_cols, align = "left", part = "all")
    if (length(value_cols)) ft <- flextable::align(ft, j = value_cols, align = "right", part = "all")
    merge_cols <- intersect(merge_cols, names(df))
    if (length(merge_cols)) ft <- flextable::merge_v(ft, j = merge_cols)
    ft <- flextable::fix_border_issues(ft)
    flextable::autofit(ft) |> flextable::fit_to_width(max_width = page_width)
  }
  # Header labels with real subscripts for the slope/intercept/window columns
  cpsd_header <- function(ft) {
    hdr <- function(ft, j, ...) flextable::compose(ft, part = "header", j = j,
                                                   value = flextable::as_paragraph(...))
    ft |>
      hdr("α_CPSD ± SE", "α", flextable::as_sub("CPSD"), " ± SE") |>
      hdr("a_PSD ± SE", "a", flextable::as_sub("PSD"), " ± SE") |>
      hdr("log10 b ± SE", "log", flextable::as_sub("10"), " b ± SE") |>
      hdr("x_min (lower LOD)", "x", flextable::as_sub("min"), " (lower LOD)") |>
      hdr("x_max (upper LOD)", "x", flextable::as_sub("max"), " (upper LOD)")
  }
  caption <- function(doc, label, ...) para(doc, bold(label), txt(" "), ...)
  footnote <- function(doc, text) {
    officer::body_add_fpar(doc, officer::fpar(
      officer::ftext(text, update(fp_txt, font.size = 9, italic = TRUE)),
      fp_p = officer::fp_par(padding.top = 4, padding.bottom = 12)))
  }

  portrait  <- officer::prop_section(
    page_size = officer::page_size(orient = "portrait"),
    page_margins = officer::page_mar(top = 1, bottom = 1, left = 1, right = 1),
    type = "nextPage")
  landscape <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape"),
    page_margins = officer::page_mar(top = 0.75, bottom = 0.75, left = 0.75, right = 0.75),
    type = "nextPage")

  doc <- officer::read_docx()
  doc <- heading(doc, "Supplementary Tables S5–S8: Particle properties and particle-size distribution fits", 16)
  doc <- para(doc, txt(paste0(
    "These tables report the measured particle properties, and the fitted parameters and fit statistics, for every particle-size ",
    "probability distribution (PDF) fitted in this work. They are formatted after Kooi et al. (2021, Water Research 198:117011, SI Tables S3–S4). ",
    "River water (freshwater; n = ", fmt_n(n_river), " particles) is reported in Tables S5 and S7. Ocean water (n = ", fmt_n(n_ocean),
    ") and beach sand (n = ", fmt_n(n_sed), ") are reported together in Tables S6 and S8. Every particle was identified by µFTIR as ",
    "plastic with a confident spectral match; non-plastic (mineral, organic) particles were excluded before any summary or fit.")))

  # ---- Variable definitions ----
  doc <- heading(doc, "Variable definitions", 13)
  doc <- heading(doc, "Particle-property tables (Tables S5 and S6)", 11.5)
  doc <- defn(doc, "Matrix", txt("Sampled environmental compartment: river water, ocean water (coastal water adjacent to each river mouth), or beach sand."))
  doc <- defn(doc, "Shape", txt("Particle morphology class. Particles with an aspect ratio (length ÷ width) ≥ 3 are classed as fibers; all others as fragments. “all” pools every particle in the matrix regardless of shape."))
  doc <- defn(doc, "n", txt("Number of particles with a finite value for that property."))
  doc <- defn(doc, "Median, Lower quartile (Q25), Upper quartile (Q75)", txt("The 50th, 25th and 75th percentiles of the property across particles. Kooi et al. (2021) label the quartiles “lower” and “upper quantile”."))
  doc <- defn(doc, "Mean, SD", txt("Arithmetic mean and sample standard deviation across particles. Particle-size distributions are strongly right-skewed, so the mean and SD are dominated by the few largest particles. The median and quartiles describe a typical particle better."))
  doc <- defn(doc, "Length (µm)", txt("The particle’s maximum dimension in the 2-D µFTIR image, as reported by the particle-analysis software."))
  doc <- defn(doc, "Width (µm)", txt("The particle’s minimum dimension in the 2-D µFTIR image."))
  doc <- defn(doc, "Width-to-length ratio (–)", txt("Width ÷ length (the reciprocal of the aspect ratio). Its median per shape and matrix is also used as the thickness factor r in the volume calculation below (Kooi et al. 2021)."))
  doc <- defn(doc, "Projected area (µm²)", txt("2-D area of the particle’s image footprint. Small areas are quantized in multiples of the 25 µm × 25 µm µFTIR pixel (625 µm²), which is why several quartiles are identical across shapes."))
  doc <- defn(doc, "Volume (µm³)", txt("Estimated 3-D volume. Particle thickness (height) H is not measured, so it is estimated as H = r × W, where r is the median width-to-length ratio for that shape and matrix. Fragments are treated as ellipsoids, V = (π/6) × L × W × H; fibers as cylinders, V = π × (W/2)² × L."))
  doc <- defn(doc, "Mass (µg)", txt("Volume × polymer density × 10⁻⁶ (1 µm³ at 1 g/cm³ = 1 pg = 10⁻⁶ µg)."))
  doc <- defn(doc, "Density (g/cm³)", txt("Polymer density assigned from each particle’s FTIR polymer class using the Hidalgo-Ruz et al. (2012, Table 2) lookup. Particles whose polymer class matched no density group were assigned 1.10 g/cm³."))

  doc <- heading(doc, "C-PSD fit tables (Tables S7a and S8a)", 11.5)
  doc <- para(doc, txt("Each row is one cumulative particle-size distribution (C-PSD) fit using the Segur et al. (2026) two-step method. Particles are binned at a fixed bin width. For each bin, N(≥x) is the number of particles in the data set at or above that bin’s lower edge x. A straight line is then fitted by ordinary least squares (OLS) on log–log axes over the fit window:"))
  doc <- officer::body_add_fpar(doc, officer::fpar(
    txt("log"), sub("10"), txt(" N(≥x) = log"), sub("10"), txt(" b + α"), sub("CPSD"), txt(" × log"), sub("10"),
    txt(" x,     for x"), sub("min"), txt(" ≤ x < x"), sub("max"),
    fp_p = officer::fp_par(text.align = "center", padding.top = 4, padding.bottom = 8)))
  doc <- defn(doc, "Metric", txt("The size measure the distribution is fitted to: length (µm), projected area (µm²) or volume (µm³). Bin width, x"), sub("min"), txt(" and x"), sub("max"), txt(" are in the units of this metric."))
  doc <- defn(doc, "Subset", txt("Which particles were fitted. “Pooled (all sites)” fits all particles in the matrix. “Site: <river>” fits only particles from that river (river water), or from coastal water next to that river’s mouth (ocean water). “Depth: surface/subsurface” fits river particles from one sampling-depth stratum."))
  doc <- defn(doc, "Shape", txt("As defined for the particle-property tables."))
  doc <- defn(doc, "n", txt("Total number of particles supplied to the fit, across all sizes."))
  doc <- defn(doc, "n in window", txt("Number of those particles that fall in bins inside the fit window [x"), sub("min"), txt(", x"), sub("max"), txt(")."))
  doc <- defn(doc, "Bin width", txt("Width of the histogram bins: 5 µm for length, 500 µm² for area and 2 × 10⁴ µm³ for volume, held constant across all matrices and shapes."))
  doc <- defn(doc, list(bold("x"), bsub("min"), bold(" (lower LOD)")),
    txt("Lower bound of the fit window: the smallest size at which the distribution behaves as a power law. Below this size, particles are progressively undercounted as they approach the instrument’s detection limit, so they are excluded from the fit. It is the counterpart of the fitted minimum value (L"), sub("min"), txt(", A"), sub("min"), txt(", V"), sub("min"),
    txt(") in Kooi et al. (2021). For length, the Segur et al. (2026) algorithm selects it automatically. For area and volume, the algorithm searches within fixed bounds: 2,000 µm² is the lowest allowed area, and the volume bounds are converted from the length fit window using the measured width-to-length ratio."))
  doc <- defn(doc, list(bold("x"), bsub("max"), bold(" (upper LOD)")),
    txt("Upper bound of the fit window. Above this size, large particles are too rare to be sampled reliably, and the C-PSD bends downward (“right-tail slump”)."))
  doc <- defn(doc, list(bold("α"), bsub("CPSD"), bold(" ± SE")),
    txt("Fitted slope of the cumulative distribution, with its OLS standard error. It is negative because fewer particles exceed each larger size."))
  doc <- defn(doc, list(bold("a"), bsub("PSD"), bold(" ± SE")),
    txt("Slope of the corresponding differential (bin-normalized) size distribution, dN/dx ∝ x"), sup("a"), txt(", where a"), sub("PSD"), txt(" = α"), sub("CPSD"),
    txt(" − 1. Its SE equals that of α"), sub("CPSD"), txt(". The positive power-law exponent α reported by Kooi et al. (2021) equals |a"), sub("PSD"),
    txt("|. a"), sub("PSD"), txt(" is the slope used to rescale measured concentrations to other size ranges."))
  doc <- defn(doc, list(bold("log"), bsub("10"), bold(" b ± SE")),
    txt("Fitted intercept of the line above, with its OLS standard error. N(≥x) is a raw particle count, so b scales with the number of particles analyzed. It is a fitting constant, not an environmental concentration."))
  doc <- defn(doc, "R²", txt("Coefficient of determination of the log–log regression. The algorithm only accepts fit windows with R² ≥ 0.90."))
  doc <- defn(doc, "n bins", txt("Number of populated (non-empty) bins inside the fit window used in the regression. The method requires at least 3. Fits with few bins (≲ 5) or few particles in the window are more uncertain than their SE alone suggests."))
  doc <- defn(doc, "– (dash)", txt("No fit possible. A per-site fit was attempted only when the subset contained at least 10 particles, and it fails if the algorithm finds no valid fit window."))

  doc <- heading(doc, "Alternative-model tables (Tables S7b and S8b)", 11.5)
  doc <- para(doc, txt("These models were fitted to pooled (all-shape) particle length to test how robust the production single-slope C-PSD model is. They are sensitivity analyses and do not feed the headline results."))
  doc <- defn(doc, "MLE power law", txt("A continuous power law, p(x) = ((α − 1)/x"), sub("min"), txt(") × (x/x"), sub("min"), txt(")"), sup("−α"), txt(" for x ≥ x"), sub("min"),
    txt(", fitted by maximum likelihood with the poweRlaw R package (conpl), following Clauset et al. (2009). x"), sub("min"),
    txt(" is the value that minimizes the Kolmogorov–Smirnov (KS) distance between the data and the fitted model. α is the positive differential exponent, directly comparable to |a"), sub("PSD"), txt("|."))
  doc <- defn(doc, "Lognormal", txt("A lognormal distribution truncated at the same x"), sub("min"), txt(" as the MLE power law (poweRlaw::conlnorm). meanlog and sdlog are the mean and standard deviation of ln(length in µm). This model tests whether the small-particle end of the distribution rolls off (curves away from a straight power law)."))
  doc <- defn(doc, "Two-segment C-PSD", txt("The production C-PSD fit window split at a break point, with separate OLS slopes below (α"), sub("CPSD,low"), txt(") and above (α"), sub("CPSD,high"),
    txt(") the break. The break is chosen to minimize AIC. Both slopes are on the cumulative scale; subtract 1 to get the differential slope."))
  doc <- defn(doc, "x_min or break (µm)", txt("x"), sub("min"), txt(" for the MLE power law and the lognormal; the break point for the two-segment model."))
  doc <- defn(doc, "n (tail or total)", txt("Number of particles ≥ x"), sub("min"), txt(" used in the MLE and lognormal fits. Not applicable (–) to the two-segment model, which is fitted to the binned C-PSD."))
  doc <- defn(doc, "KS D", txt("Kolmogorov–Smirnov distance between the observed and fitted cumulative distributions above x"), sub("min"), txt("; smaller values mean a closer fit."))
  doc <- defn(doc, "Bootstrap GOF p", txt("Goodness-of-fit p-value from 100 semi-parametric bootstrap simulations (Clauset et al. 2009). p < 0.1 rejects the power law; larger values mean the power law is a plausible description of the data."))
  doc <- defn(doc, "Vuong R, p", txt("Normalized log-likelihood ratio comparing the power law with the lognormal at the same x"), sub("min"),
    txt(", with its two-sided p-value. R > 0 favors the power law and R < 0 favors the lognormal. A non-significant p means the data cannot distinguish the two models."))
  doc <- defn(doc, "ΔAIC vs. single slope", txt("AIC of the two-segment fit minus AIC of the single-slope fit, over the same bins. Negative values favor the two-segment model; values below about −2 are conventionally treated as meaningful."))

  doc <- officer::body_end_block_section(doc, officer::block_section(portrait))

  # ---- Tables (landscape) ----
  merge_props <- c("Matrix", "Property")
  merge_fits  <- c("Matrix", "Metric", "Subset")
  fit_note <- "Bin width, x_min and x_max are in the units of the Metric column. The Kooi et al. (2021) exponent α = |a_PSD|."

  doc <- caption(doc, "Table S5.", txt(paste0("Particle properties, river water (µFTIR, n = ", fmt_n(n_river), " particles), by shape: median, lower (Q25) and upper (Q75) quartiles, mean and standard deviation (SD).")))
  doc <- flextable::body_add_flextable(doc, style_ft(si_format_property_table(props_river), merge_props))
  doc <- footnote(doc, "Mass = volume × polymer-resolved density. Volume assumes thickness = median width-to-length ratio × width. Fibers: aspect ratio ≥ 3.")
  doc <- officer::body_add_break(doc)

  doc <- caption(doc, "Table S6.", txt(paste0("Particle properties, ocean water (n = ", fmt_n(n_ocean), ") and beach sand (n = ", fmt_n(n_sed), "), by shape. Columns as in Table S5.")))
  doc <- flextable::body_add_flextable(doc, style_ft(si_format_property_table(props_ocean_sed, include_matrix = TRUE), merge_props))
  doc <- officer::body_add_break(doc)

  doc <- caption(doc, "Table S7a.", txt("C-PSD fits, river water: fit window [x"), sub("min"), txt(", x"), sub("max"), txt("), cumulative slope α"), sub("CPSD"),
    txt(" and log"), sub("10"), txt(" intercept b (± SE), differential slope a"), sub("PSD"), txt(" = α"), sub("CPSD"),
    txt(" − 1, R² and number of populated bins fitted, for every river length, area and volume distribution (pooled, per site and per depth stratum)."))
  doc <- flextable::body_add_flextable(doc, cpsd_header(style_ft(si_format_cpsd_table(fits_river), merge_fits)))
  doc <- footnote(doc, fit_note)
  doc <- officer::body_add_break(doc)

  doc <- caption(doc, "Table S7b.", txt("Alternative size-distribution models fitted to pooled river-water particle length."))
  doc <- flextable::body_add_flextable(doc, style_ft(dplyr::select(alt_river, -Matrix), character(0)))
  doc <- footnote(doc, "MLE α is the positive differential exponent (comparable to |a_PSD|). Two-segment slopes are on the cumulative (C-PSD) scale.")
  doc <- officer::body_add_break(doc)

  doc <- caption(doc, "Table S8a.", txt("C-PSD fits, ocean water and beach sand. Columns as in Table S7a. Per-site fits were run for ocean-water length only; – marks a site with too few particles for a valid fit window."))
  doc <- flextable::body_add_flextable(doc, cpsd_header(style_ft(si_format_cpsd_table(fits_ocean_sed, include_matrix = TRUE), merge_fits)))
  doc <- footnote(doc, fit_note)
  doc <- officer::body_add_break(doc)

  doc <- caption(doc, "Table S8b.", txt("Alternative size-distribution models fitted to pooled particle length, ocean water and beach sand. The MLE and lognormal models were fitted to ocean water only."))
  doc <- flextable::body_add_flextable(doc, style_ft(alt_ocean_sed, "Matrix"))
  doc <- footnote(doc, "Columns as in Table S7b.")

  doc <- officer::body_set_default_section(doc, landscape)
  print(doc, target = path)
  invisible(path)
}
