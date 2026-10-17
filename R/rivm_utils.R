# rivm_utils.R
# Benchmark of this study's river-water PSD slopes and rescaled concentrations
# against Dutch river datasets compiled for RIVM letter report 2026-0069
# (Quik et al. 2026; data/code: github.com/rivm-syso/MicroplasticsERA_water,
# EUPL-1.2).
#
# Conventions (locked by tests/testthat/test-rivm_utils.R):
#   * RIVM `alpha` is the POSITIVE power-law PDF exponent (poweRlaw MLE above
#     an estimated xmin). This study's differential slope is a_psd = -alpha.
#   * rivm_cf() is RIVM's F.Correctionfactor() and is identical to
#     correction_factor(a = -alpha, ..., slope_convention = "differential").
#   * Concentrations are carried in particles m^-3 throughout (ours are
#     converted from particles L^-1 x 1000).
#
# Requires: dplyr, tidyr, tibble, readxl, ggplot2, patchwork, withr, digest.
# correction_factor() and alpha_dist() come from R/mp_risk_utils.R.

# ── Constants ─────────────────────────────────────────────────────────────────

RIVM_REPO_URL <- "https://github.com/rivm-syso/MicroplasticsERA_water"

# Relative paths of every external input read from the RIVM clone. Hashed into
# the benchmark chunk's cache.extra so any upstream data change invalidates it.
RIVM_INPUT_FILES <- c(
  alphas_ftir    = "Output/Data/powerlawFitFTIR_All2026-06-30.csv",
  alphas_ldir    = "Output/Data/powerlawFitLDIR_All2026-04-09.csv",
  mintenig2020   = "rawdata/Mintenig_2020/readme.xlsx",
  mughinigras    = "rawdata/Mughini-Gras_2021/readme.xlsx",
  mintenig2025   = "rawdata/Mintenig_2025/readme.xlsx",
  dommel2025_conc = "rawdata/Mintenig_2025/Concentrations.xlsx",
  rescaled_rds   = "Output/Data/Rescaled_concentrations_2026-05-13.Rds"
)

# Okabe-Ito palette; dataset colours used in every benchmark figure.
OKABE_ITO <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000")
# This study's benchmark uses each river's own headline slope (fixed-window MLE since 2026-10-15).
RIVM_OURS_PRIMARY <- "river"
RIVM_DATASET_COLS <- c("Dutch rivers" = "#0072B2", "This study" = "#D55E00")
RIVM_TECH_SHAPES  <- c("FTIR" = 21, "LDIR" = 24, "µFTIR" = 22)

# Target windows. W2 is primary (interior to every measured range); W1 is the
# Koelmans et al. (2020) default and a long extrapolation for all datasets.
rivm_windows <- function() {
  tibble::tibble(window = c("W2", "W1"), LLd = c(50, 1), ULd = c(500, 5000),
                 window_label = c("W2: 50–500 µm", "W1: 1–5000 µm"))
}

# Prototype regression targets (Section 7 of the task spec): median across
# samples of per-sample median, particles m^-3, Mintenig 2020 / Mughini-Gras
# 2021 only, as-reported totals, RIVM ("published") uncertainty method.
RIVM_PROTOTYPE_TARGETS <- tibble::tribble(
  ~group,            ~window, ~target,  ~n_target,
  "Meuse (FTIR)",     "W2",    251,      11,
  "Dommel (FTIR)",    "W2",    181,      18,
  "Rhine (LDIR)",     "W2",    9079,     2,
  "Lek canal (LDIR)", "W2",    3160,     2,
  "Meuse (FTIR)",     "W1",    38700,    11,
  "Dommel (FTIR)",    "W1",    37700,    18,
  "Rhine (LDIR)",     "W1",    2.19e7,   2,
  "Lek canal (LDIR)", "W1",    9.37e7,   2
)

# ── Paths, provenance, hashing ───────────────────────────────────────────────

#' Locate the RIVM clone and the derived-summary directory
#'
#' The raw clone is gitignored (not redistributed); its location can be
#' overridden with the RIVM_REPO_DIR environment variable.
rivm_paths <- function(proj_root = ".") {
  ext <- file.path(proj_root, "data_input", "external", "rivm")
  repo <- Sys.getenv("RIVM_REPO_DIR", file.path(ext, "MicroplasticsERA_water"))
  list(ext_dir = ext, repo_dir = repo, derived_dir = file.path(ext, "derived"))
}

rivm_repo_available <- function(repo_dir) {
  all(file.exists(file.path(repo_dir, RIVM_INPUT_FILES))) &&
    # Git LFS pointer stubs are ~130 bytes; the real .Rds is several MB.
    file.size(file.path(repo_dir, RIVM_INPUT_FILES[["rescaled_rds"]])) > 1e5
}

#' MD5 hashes of the external inputs (raw clone if present, else derived CSVs)
rivm_input_hashes <- function(paths) {
  files <- if (rivm_repo_available(paths$repo_dir)) {
    file.path(paths$repo_dir, RIVM_INPUT_FILES)
  } else {
    list.files(paths$derived_dir, pattern = "[.]csv$", full.names = TRUE)
  }
  stats::setNames(vapply(files, digest::digest, character(1), file = TRUE),
                  basename(files))
}

.rivm_local_seed <- function(seed, code) withr::with_seed(seed, code)

# ── Correction factor and samplers ───────────────────────────────────────────

#' RIVM correction factor (Koelmans et al. 2020 analytical form)
#'
#' Verbatim re-implementation of F.Correctionfactor() in the RIVM repository
#' (R/CorrectionFactor.R). `alpha` is the positive PDF exponent.
rivm_cf <- function(ULm, LLm, ULd, LLd, alpha) {
  (ULd^(1 - alpha) - LLd^(1 - alpha)) / (ULm^(1 - alpha) - LLm^(1 - alpha))
}

#' Triangular random variates (inverse CDF; equivalent to triangle::rtriangle)
rtriangular <- function(n, a, b, c) {
  stopifnot(a <= c, c <= b)
  if (b == a) return(rep(a, n))
  u <- stats::runif(n)
  fc <- (c - a) / (b - a)
  ifelse(u < fc,
         a + sqrt(u * (b - a) * (c - a)),
         b - sqrt((1 - u) * (b - a) * (b - c)))
}

# ── Readers ──────────────────────────────────────────────────────────────────

.rivm_expect_cols <- function(df, cols, what) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    stop(sprintf("%s: unexpected layout, missing column(s): %s", what,
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(df)
}

#' Read RIVM power-law fits (param == "longest" only)
#'
#' @return tibble: technique, river, alpha, alpha_lower, alpha_upper, xmin,
#'   xmin_lower, xmin_upper, a_psd (= -alpha), source_file.
read_rivm_alphas <- function(repo_dir) {
  one <- function(key, technique) {
    f <- file.path(repo_dir, RIVM_INPUT_FILES[[key]])
    d <- utils::read.csv(f, check.names = FALSE)
    d <- d[nzchar(names(d))]   # drop the unnamed row-index column written by write.csv
    .rivm_expect_cols(d, c("id", "param", "alpha", "alpha_lower", "alpha_upper",
                           "xmin", "xmin_lower", "xmin_upper"), basename(f))
    d |>
      dplyr::filter(.data$param == "longest") |>
      dplyr::transmute(technique = technique, river = .data$id,
                       alpha = .data$alpha, alpha_lower = .data$alpha_lower,
                       alpha_upper = .data$alpha_upper, xmin = .data$xmin,
                       xmin_lower = .data$xmin_lower, xmin_upper = .data$xmin_upper,
                       a_psd = -.data$alpha, source_file = basename(f))
  }
  dplyr::bind_rows(one("alphas_ftir", "FTIR"), one("alphas_ldir", "LDIR")) |>
    # Harmonise one label (RIVM spells it "Overijsselse vecht").
    dplyr::mutate(river = dplyr::recode(.data$river, "Overijsselse vecht" = "Overijsselse Vecht"))
}

#' Read and harmonise Dutch sample-level total concentrations
#'
#' Mintenig 2020: Meta joined to Corrected_concentrations_total by Filename;
#' rubber-excluded totals from Corrected_concentrations (matched by
#' Location == Location_name). Mughini-Gras 2021: Meta joined to
#' Concentrations$Total by Sample_no; rubber-excluded = Total - Isoprene.
#' Mintenig 2025 (Dommel): Meta joined to Concentrations.xlsx summed by
#' Location_name; rubber-excluded drops polymer_group "32-EPDM rubber".
#'
#' @return list(samples = included samples, audit = every Meta row with its
#'   inclusion status and reason, rubber_match = Mintenig 2020 match table).
read_rivm_samples <- function(repo_dir) {
  xl <- function(key, sheet = NULL) {
    readxl::read_excel(file.path(repo_dir, RIVM_INPUT_FILES[[key]]), sheet = sheet)
  }
  meta_cols <- c("Filename", "Water_type", "Lower_sampling_size_limit_um",
                 "Upper_sampling_size_limit_um", "Analysis_method",
                 "Location_name", "River", "Sample_no")

  # Mintenig et al. 2020 ------------------------------------------------------
  m20_meta <- .rivm_expect_cols(xl("mintenig2020", "Meta"), meta_cols, "Mintenig 2020 Meta")
  m20_tot  <- .rivm_expect_cols(xl("mintenig2020", "Corrected_concentrations_total"),
                                c("Filename", "Concentration", "unit"), "Mintenig 2020 totals")
  m20_cc   <- .rivm_expect_cols(xl("mintenig2020", "Corrected_concentrations"),
                                c("Location", "Rubbers_#/m3", "Total_#/m3"),
                                "Mintenig 2020 Corrected_concentrations")
  stopifnot(all(m20_tot$unit == "#/m3"))
  rubber_match <- m20_meta |>
    dplyr::select("Filename", "Location_name") |>
    dplyr::left_join(m20_cc |> dplyr::select(Location = "Location",
                                             rubber_m3 = "Rubbers_#/m3",
                                             cc_total_m3 = "Total_#/m3"),
                     by = c("Location_name" = "Location")) |>
    dplyr::mutate(matched = !is.na(.data$cc_total_m3))
  m20 <- m20_meta |>
    dplyr::left_join(m20_tot |> dplyr::select("Filename", reported_conc_m3 = "Concentration"),
                     by = "Filename") |>
    dplyr::left_join(rubber_match |> dplyr::select("Filename", "rubber_m3", "cc_total_m3"),
                     by = "Filename") |>
    dplyr::mutate(study = "Mintenig et al. 2020", technique = "FTIR",
                  conc_excl_rubber_m3 = .data$reported_conc_m3 - .data$rubber_m3)

  # Mughini-Gras et al. 2021 --------------------------------------------------
  mg_meta <- .rivm_expect_cols(xl("mughinigras", "Meta"),
                               c(setdiff(meta_cols, character()), "Sample_volume_L"),
                               "Mughini-Gras 2021 Meta")
  mg_conc <- .rivm_expect_cols(xl("mughinigras", "Concentrations"),
                               c("unit", "Sample_no", "Isoprene", "Total"),
                               "Mughini-Gras 2021 Concentrations")
  stopifnot(all(mg_conc$unit == "#/m3"))
  mg <- mg_meta |>
    dplyr::left_join(mg_conc |> dplyr::select("Sample_no", reported_conc_m3 = "Total",
                                              rubber_m3 = "Isoprene"),
                     by = "Sample_no") |>
    dplyr::mutate(study = "Mughini-Gras et al. 2021", technique = "LDIR",
                  conc_excl_rubber_m3 = .data$reported_conc_m3 - .data$rubber_m3)

  # Mintenig, Hunnestad & Koelmans 2025 (Dommel) ------------------------------
  m25_meta <- .rivm_expect_cols(xl("mintenig2025", "Meta"), meta_cols, "Mintenig 2025 Meta")
  m25_conc <- .rivm_expect_cols(xl("dommel2025_conc"),
                                c("Location_name", "polymer_group", "concentration", "unit"),
                                "Mintenig 2025 Concentrations")
  stopifnot(all(m25_conc$unit == "#/m3"))
  m25_tot <- m25_conc |>
    dplyr::group_by(.data$Location_name) |>
    dplyr::summarise(reported_conc_m3 = sum(.data$concentration),
                     rubber_m3 = sum(.data$concentration[grepl("rubber", .data$polymer_group, ignore.case = TRUE)]),
                     .groups = "drop")
  m25 <- m25_meta |>
    dplyr::left_join(m25_tot, by = "Location_name") |>
    dplyr::mutate(study = "Mintenig et al. 2025", technique = "FTIR",
                  conc_excl_rubber_m3 = .data$reported_conc_m3 - .data$rubber_m3)

  keep <- c("study", "technique", "River", "Filename", "Location_name", "Water_type",
            "Lower_sampling_size_limit_um", "Upper_sampling_size_limit_um",
            "reported_conc_m3", "conc_excl_rubber_m3")
  all_rows <- dplyr::bind_rows(m20[keep], mg[keep], m25[keep]) |>
    dplyr::rename(river = "River", sample_id = "Filename", location = "Location_name",
                  water_type = "Water_type", LLm = "Lower_sampling_size_limit_um",
                  ULm = "Upper_sampling_size_limit_um")

  audit <- all_rows |>
    dplyr::mutate(
      r_eff = ifelse(.data$water_type == "Effluent WWTP", "WWTP effluent (not surface water)", NA),
      r_llm = ifelse(.data$LLm >= 300, "LLm = 300 um (no total in Corrected_concentrations_total)", NA),
      r_tot = ifelse(is.na(.data$reported_conc_m3) & .data$LLm < 300, "No total concentration reported", NA),
      exclusion_reason = apply(cbind(.data$r_eff, .data$r_llm, .data$r_tot), 1, function(r) {
        r <- r[!is.na(r)]; if (length(r)) paste(r, collapse = "; ") else NA_character_
      }),
      included = is.na(.data$exclusion_reason)
    ) |>
    dplyr::select(-"r_eff", -"r_llm", -"r_tot")

  list(samples = dplyr::filter(audit, .data$included) |> dplyr::select(-"exclusion_reason", -"included"),
       audit = audit, rubber_match = rubber_match)
}

#' Per-sample summary of RIVM's own rescaled concentrations ("as published")
#'
#' Reads Rescaled_concentrations_*.Rds (RIVM 03_NumberConcentrationRescaling,
#' OptionType "concentrationOption3" = triangular alpha and +/-10% limits,
#' 1 -> 5000 um, as used in the report). Returns NULL if the file is absent or
#' is a Git LFS pointer stub.
read_rivm_published <- function(repo_dir) {
  f <- file.path(repo_dir, RIVM_INPUT_FILES[["rescaled_rds"]])
  if (!file.exists(f) || file.size(f) < 1e5) return(NULL)
  rs <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(rs)) return(NULL)
  .rivm_expect_cols(rs, c("Data_source", "River", "Filename", "param", "OptionType",
                          "concentration", "CF_option3"), basename(f))
  rs |>
    dplyr::filter(.data$OptionType == "concentrationOption3", .data$param == "longest") |>
    dplyr::group_by(.data$Data_source, .data$River, .data$Filename) |>
    dplyr::summarise(n_draws = dplyr::n(),
                     cf_p05 = stats::quantile(.data$CF_option3, 0.05),
                     cf_p50 = stats::median(.data$CF_option3),
                     cf_p95 = stats::quantile(.data$CF_option3, 0.95),
                     conc_p05 = stats::quantile(.data$concentration, 0.05),
                     conc_p50 = stats::median(.data$concentration),
                     conc_p95 = stats::quantile(.data$concentration, 0.95),
                     .groups = "drop") |>
    dplyr::rename(river = "River", sample_id = "Filename") |>
    dplyr::mutate(river = dplyr::recode(.data$river, "Overijsselse vecht" = "Overijsselse Vecht"),
                  study = dplyr::recode(.data$Data_source,
                                        "Mintenig et al. (2020)" = "Mintenig et al. 2020",
                                        "Mughini-Gras et al. (2021)" = "Mughini-Gras et al. 2021",
                                        "Mintenig, Hunnestad & Koelmans (2025)" = "Mintenig et al. 2025"),
                  source_file = basename(f))
}

# ── Derived summaries (the only RIVM-derived files committed to this repo) ──

#' Build derived CSVs from the RIVM clone
rivm_build_derived <- function(paths) {
  stopifnot(rivm_repo_available(paths$repo_dir))
  dir.create(paths$derived_dir, recursive = TRUE, showWarnings = FALSE)
  alphas <- read_rivm_alphas(paths$repo_dir)
  s <- read_rivm_samples(paths$repo_dir)
  pub <- read_rivm_published(paths$repo_dir)
  utils::write.csv(alphas, file.path(paths$derived_dir, "rivm_alphas_longest.csv"), row.names = FALSE)
  utils::write.csv(s$audit, file.path(paths$derived_dir, "rivm_sample_audit.csv"), row.names = FALSE)
  utils::write.csv(s$rubber_match, file.path(paths$derived_dir, "rivm_mintenig2020_rubber_match.csv"), row.names = FALSE)
  if (!is.null(pub)) {
    utils::write.csv(pub, file.path(paths$derived_dir, "rivm_published_rescaled_W1_by_sample.csv"), row.names = FALSE)
  }
  invisible(list(alphas = alphas, samples = s$samples, audit = s$audit,
                 rubber_match = s$rubber_match, published = pub))
}

#' Load RIVM inputs: from the clone when available (rebuilding derived CSVs),
#' otherwise from the committed derived CSVs.
rivm_load_inputs <- function(paths) {
  if (rivm_repo_available(paths$repo_dir)) {
    out <- rivm_build_derived(paths)
    out$source <- "RIVM clone (rebuilt derived CSVs)"
    return(out)
  }
  rd <- function(f) {
    p <- file.path(paths$derived_dir, f)
    if (file.exists(p)) tibble::as_tibble(utils::read.csv(p, check.names = FALSE)) else NULL
  }
  audit <- rd("rivm_sample_audit.csv")
  if (is.null(audit)) stop("Neither the RIVM clone nor derived CSVs were found; see PROVENANCE.md", call. = FALSE)
  list(alphas = rd("rivm_alphas_longest.csv"),
       samples = dplyr::filter(audit, .data$included) |> dplyr::select(-"exclusion_reason", -"included"),
       audit = audit, rubber_match = rd("rivm_mintenig2020_rubber_match.csv"),
       published = rd("rivm_published_rescaled_W1_by_sample.csv"),
       source = "committed derived CSVs")
}

# ── Rescaling ────────────────────────────────────────────────────────────────

#' Attach alphas to Dutch samples (FTIR samples -> FTIR fit, LDIR -> LDIR fit,
#' matched by water body)
rivm_join_alphas <- function(samples, alphas) {
  out <- samples |>
    dplyr::left_join(alphas |> dplyr::select("technique", "river", "alpha", "alpha_lower",
                                             "alpha_upper", "xmin"),
                     by = c("technique", "river"))
  if (anyNA(out$alpha)) stop("Dutch samples without a matching RIVM alpha: ",
                             paste(unique(out$river[is.na(out$alpha)]), collapse = ", "))
  out
}

.cf_quantiles <- function(cf) {
  q <- stats::quantile(cf, c(0.05, 0.5, 0.95), names = FALSE)
  tibble::tibble(cf_p05 = q[1], cf_p50 = q[2], cf_p95 = q[3])
}

#' Rescale Dutch samples to the target windows
#'
#' @param samples Output of rivm_join_alphas().
#' @param method "published" = RIVM method (alpha ~ triangular(lower, mode,
#'   upper); LLm and ULm each triangular +/-10% clipped to [1, 5000]);
#'   "harmonised" = this study's method (alpha_dist() truncated normal with
#'   sd = (upper - lower) / (2 * 1.96) plus structural_sd; limits fixed).
#' @param conc_col Column holding the concentration to rescale.
#' @param LLm_override Optional named numeric (technique -> LLm), e.g.
#'   c(LDIR = 20) for the RIVM-text LDIR limit sensitivity.
#' @return One row per sample x window with CF and concentration quantiles.
rescale_rivm <- function(samples, method = c("published", "harmonised"),
                         conc_col = "reported_conc_m3", n = 5000, seed = 2026,
                         structural_sd = 0, LLm_override = NULL,
                         windows = rivm_windows()) {
  method <- match.arg(method)
  if (!is.null(LLm_override)) {
    hit <- samples$technique %in% names(LLm_override)
    samples$LLm[hit] <- LLm_override[samples$technique[hit]]
  }
  rows <- lapply(seq_len(nrow(samples)), function(i) {
    s <- samples[i, ]
    draws <- .rivm_local_seed(seed + i, {
      if (method == "published") {
        lo <- min(s$alpha_lower, s$alpha); hi <- max(s$alpha_upper, s$alpha)  # RIVM check_alpha swap
        list(alpha = rtriangular(n, lo, hi, s$alpha),
             LLm = rtriangular(n, max(0.9 * s$LLm, 1), 1.1 * s$LLm, s$LLm),
             ULm = rtriangular(n, 0.9 * s$ULm, min(1.1 * s$ULm, 5000), s$ULm))
      } else {
        sd_fit <- (s$alpha_upper - s$alpha_lower) / (2 * 1.96)
        list(alpha = -alpha_dist(mu = -s$alpha, sd = sd_fit, n = n, structural_sd = structural_sd),
             LLm = rep(s$LLm, n), ULm = rep(s$ULm, n))
      }
    })
    dplyr::bind_rows(lapply(seq_len(nrow(windows)), function(w) {
      cf <- rivm_cf(draws$ULm, draws$LLm, windows$ULd[w], windows$LLd[w], draws$alpha)
      cbind(s, window = windows$window[w], .cf_quantiles(cf))
    }))
  })
  dplyr::bind_rows(rows) |>
    dplyr::mutate(conc_p05 = .data[[conc_col]] * .data$cf_p05,
                  conc_p50 = .data[[conc_col]] * .data$cf_p50,
                  conc_p95 = .data[[conc_col]] * .data$cf_p95,
                  method_variant = method)
}

#' Collapse this study's monitoring table to per-sample totals (particles m^-3)
#'
#' The pipeline's `monitoring` table has one row per sample x polymer class;
#' sample totals (sum over plastic classes) are the quantity comparable to the
#' study-reported Dutch totals.
ours_sample_totals <- function(monitoring, conc_col = "C_measured_pL") {
  monitoring |>
    dplyr::group_by(.data$sample_id, .data$river) |>
    dplyr::summarise(reported_conc_m3 = sum(.data[[conc_col]]) * 1000,
                     n_polymer_rows = dplyr::n(), .groups = "drop")
}

#' Rescale this study's samples with river-specific headline slopes
#'
#' @param totals ours_sample_totals() output.
#' @param river_fits Named list (river -> fit_cpsd_segur_r() output for "all").
#' @param L_meas Measured window (default: the 50/500 um sampling sieves, the
#'   analogue of RIVM's sampling limits LLm/ULm).
rescale_ours <- function(totals, river_fits, L_meas = c(50, 500), n_mc = 20000,
                         structural_sd = 0.25, seed = 2027, windows = rivm_windows()) {
  base <- totals |>
    dplyr::mutate(dataset = "This study", study = "This study", technique = "µFTIR",
                  water_type = "Surface water", LLm = L_meas[1], ULm = L_meas[2],
                  rubber_treatment = "as reported (no rubber class)")

  slope_row <- function(a_hat, se) {
    tibble::tibble(a_psd = a_hat, alpha = -a_hat,
                   alpha_lo = -(a_hat + 1.96 * se), alpha_hi = -(a_hat - 1.96 * se))
  }
  out <- list()
  for (w in seq_len(nrow(windows))) {
    LLd <- windows$LLd[w]; ULd <- windows$ULd[w]
    for (r in unique(base$river)) {
      f <- river_fits[[r]]
      a_r <- .rivm_local_seed(seed + match(r, names(river_fits)),
                              alpha_dist(mu = f$a_psd, sd = f$se_a_psd, n = n_mc,
                                         structural_sd = structural_sd))
      cf_r <- .cf_quantiles(correction_factor(a_r, L_meas[1], L_meas[2], LLd, ULd))
      out[[length(out) + 1]] <- cbind(dplyr::filter(base, .data$river == r),
                                      window = windows$window[w], method_variant = "river",
                                      slope_row(f$a_psd, f$se_a_psd), cf_r)
    }
  }
  dplyr::bind_rows(out) |>
    dplyr::mutate(conc_p05 = .data$reported_conc_m3 * .data$cf_p05,
                  conc_p50 = .data$reported_conc_m3 * .data$cf_p50,
                  conc_p95 = .data$reported_conc_m3 * .data$cf_p95,
                  # W2 equals our measured sieve window: CF = 1, value is the measurement.
                  value_type = ifelse(abs(.data$cf_p50 - 1) < 1e-9, "measured", "reconstructed"))
}

# ── Assembly ─────────────────────────────────────────────────────────────────

RIVM_SAMPLE_COLS <- c("dataset", "study", "technique", "river", "group", "sample_id",
                      "water_type", "LLm", "ULm", "reported_conc_m3", "alpha", "alpha_lo",
                      "alpha_hi", "a_psd", "method_variant", "scenario", "window",
                      "cf_p05", "cf_p50", "cf_p95", "conc_p05", "conc_p50", "conc_p95",
                      "rubber_treatment", "value_type")

rivm_group_label <- function(river, technique) paste0(river, " (", technique, ")")

#' Run every Dutch rescaling variant and our variants; return the tidy table
#'
#' Primary: Dutch "published" method, as-reported totals, metadata LLm.
#' Sensitivities (scenario column): rubber excluded; LDIR LLm = 20 um;
#' harmonised uncertainty. RIVM's own per-sample W1 values are appended as
#' value_type "published".
run_rivm_benchmark <- function(rivm, ours_totals, river_fits, n_rivm = 5000,
                               n_mc = 20000, structural_sd = 0.25,
                               L_meas_ours = c(50, 500)) {
  dutch <- rivm_join_alphas(rivm$samples, rivm$alphas)
  tag <- function(d, scenario, rubber, conc_col = "reported_conc_m3") {
    d |> dplyr::mutate(dataset = "Dutch rivers", scenario = scenario,
                       rubber_treatment = rubber, value_type = "reconstructed",
                       reported_conc_m3 = .data[[conc_col]],
                       alpha_lo = .data$alpha_lower, alpha_hi = .data$alpha_upper,
                       a_psd = -.data$alpha)
  }
  d_pub  <- tag(rescale_rivm(dutch, "published", n = n_rivm), "primary", "as reported")
  d_harm <- tag(rescale_rivm(dutch, "harmonised", n = n_mc, structural_sd = structural_sd),
                "primary", "as reported")
  d_rub  <- tag(rescale_rivm(dutch, "published", conc_col = "conc_excl_rubber_m3", n = n_rivm),
                "rubber excluded", "excl. rubber", conc_col = "conc_excl_rubber_m3")
  d_ld20 <- tag(rescale_rivm(dplyr::filter(dutch, .data$technique == "LDIR"), "published",
                             n = n_rivm, LLm_override = c(LDIR = 20)),
                "LDIR LLm = 20 um", "as reported")

  pub <- rivm$published
  d_rivm <- NULL
  if (!is.null(pub)) {
    d_rivm <- dutch |>
      dplyr::select(-dplyr::any_of(c("cf_p05", "cf_p50", "cf_p95"))) |>
      dplyr::inner_join(pub |> dplyr::select("study", "sample_id", "cf_p05", "cf_p50", "cf_p95",
                                             "conc_p05", "conc_p50", "conc_p95"),
                        by = c("study", "sample_id")) |>
      dplyr::mutate(window = "W1", method_variant = "published_rivm") |>
      tag("primary", "as reported") |>
      dplyr::mutate(value_type = "published")
  }

  ours <- rescale_ours(ours_totals, river_fits,
                       L_meas = L_meas_ours, n_mc = n_mc, structural_sd = structural_sd) |>
    dplyr::mutate(scenario = "primary")

  dplyr::bind_rows(d_pub, d_harm, d_rub, d_ld20, d_rivm, ours) |>
    dplyr::mutate(group = ifelse(.data$dataset == "This study", .data$river,
                                 rivm_group_label(.data$river, .data$technique))) |>
    dplyr::select(dplyr::all_of(RIVM_SAMPLE_COLS), dplyr::any_of("location"))
}

#' Group (water body) summaries derived from per-sample medians
rivm_group_summary <- function(samples_tbl) {
  samples_tbl |>
    dplyr::group_by(.data$dataset, .data$group, .data$technique, .data$method_variant,
                    .data$scenario, .data$rubber_treatment, .data$window, .data$value_type) |>
    dplyr::summarise(n = dplyr::n(),
                     median = stats::median(.data$conc_p50),
                     q25 = stats::quantile(.data$conc_p50, 0.25),
                     q75 = stats::quantile(.data$conc_p50, 0.75),
                     min = min(.data$conc_p50), max = max(.data$conc_p50),
                     cf_p50_median = stats::median(.data$cf_p50),
                     .groups = "drop")
}

#' Slope comparison table (differential slope a; RIVM a = -alpha)
#'
#' RIVM CIs are their bootstrap CIs as published; ours are +/- 1.96 SE. The
#' "struct" interval adds slope_structural_sd in quadrature (the total SD used
#' by alpha_dist() in the harmonised Monte Carlo).
rivm_slope_table <- function(alphas, river_fits, structural_sd = 0.25) {
  dutch <- alphas |>
    dplyr::transmute(
      dataset = "Dutch rivers",
      facet = paste0("Dutch ", .data$technique),
      label = .data$river, technique = .data$technique,
      a = -.data$alpha, ci_lo = -.data$alpha_upper, ci_hi = -.data$alpha_lower,
      sd_fit = (.data$alpha_upper - .data$alpha_lower) / (2 * 1.96),
      fit_window = sprintf("xmin %.0f µm", .data$xmin),
      fit_method = "poweRlaw MLE above xmin (particles ≤ 5000 µm)",
      value_type = "published")
  ours_row <- function(facet, label, f) {
    tibble::tibble(dataset = "This study", facet = facet, label = label, technique = "µFTIR",
                   a = f$a_psd, ci_lo = f$a_psd - 1.96 * f$se_a_psd, ci_hi = f$a_psd + 1.96 * f$se_a_psd,
                   sd_fit = f$se_a_psd,
                   fit_window = sprintf("window %.0f–%.0f µm", f$lower_lod_um, f$upper_lod_um),
                   fit_method = if (identical(f$estimator, "tpl_mle")) "Doubly truncated power-law MLE, fixed window" else "C-PSD OLS in Segur LOD window",
                   value_type = "measured")
  }
  ours <- dplyr::bind_rows(
    lapply(names(river_fits), function(r) ours_row("This study: by river", r, river_fits[[r]])))
  dplyr::bind_rows(dutch, ours) |>
    dplyr::mutate(sd_total = sqrt(.data$sd_fit^2 + structural_sd^2),
                  struct_lo = .data$a - 1.96 * .data$sd_total,
                  struct_hi = .data$a + 1.96 * .data$sd_total,
                  facet = factor(.data$facet, levels = c("Dutch FTIR", "Dutch LDIR",
                                                         "This study: by river")))
}

#' Regression check against the prototype targets (+/-10% on medians)
rivm_regression_check <- function(samples_tbl, tol = 0.10) {
  got <- samples_tbl |>
    dplyr::filter(.data$dataset == "Dutch rivers", .data$method_variant == "published",
                  .data$scenario == "primary",
                  .data$study %in% c("Mintenig et al. 2020", "Mughini-Gras et al. 2021")) |>
    dplyr::group_by(.data$group, .data$window) |>
    dplyr::summarise(n = dplyr::n(), median = stats::median(.data$conc_p50), .groups = "drop")
  RIVM_PROTOTYPE_TARGETS |>
    dplyr::left_join(got, by = c("group", "window")) |>
    dplyr::mutate(rel_diff = .data$median / .data$target - 1,
                  pass = abs(.data$rel_diff) <= tol & .data$n == .data$n_target)
}

#' Compare RIVM's own W1 medians with our reconstruction of their method
rivm_published_vs_reconstructed <- function(samples_tbl) {
  s <- samples_tbl |>
    dplyr::filter(.data$dataset == "Dutch rivers", .data$window == "W1", .data$scenario == "primary",
                  .data$method_variant %in% c("published", "published_rivm"))
  s |>
    dplyr::select("study", "group", "sample_id", "method_variant", "conc_p50") |>
    tidyr::pivot_wider(names_from = "method_variant", values_from = "conc_p50") |>
    dplyr::filter(!is.na(.data$published_rivm)) |>
    dplyr::group_by(.data$group) |>
    dplyr::summarise(n = dplyr::n(), reconstructed = stats::median(.data$published),
                     rivm_published = stats::median(.data$published_rivm), .groups = "drop") |>
    dplyr::mutate(ratio = .data$rivm_published / .data$reconstructed,
                  within_2x = .data$ratio <= 2 & .data$ratio >= 0.5)
}

# ── Figures ──────────────────────────────────────────────────────────────────

rivm_theme <- function(base_size = 10) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   strip.background = ggplot2::element_rect(fill = "grey92", colour = "grey60"),
                   strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
                   legend.position = "bottom",
                   plot.title.position = "plot")
}

.rivm_log_x <- function(name) {
  # Whole decades only (no 10^1.5-style labels).
  ggplot2::scale_x_log10(name = name, labels = scales::label_log(), breaks = 10^(-2:12))
}

# Ordered y-levels for Figure 7 rows.
.rivm_row_order <- function(groups_dutch, rivers_ours) {
  dutch_order <- c("Meuse (FTIR)", "Dommel (FTIR)", "Rhine (LDIR)", "Lek canal (LDIR)")
  c(intersect(dutch_order, groups_dutch), setdiff(groups_dutch, dutch_order), sort(rivers_ours))
}

#' Figure 7: rescaled concentrations, Dutch rivers vs this study
#'
#' Panel (a) W2, panel (b) W1. Open symbols = per-sample medians (jittered on a
#' discrete y); thin bars = per-sample 5-95%; thick pointrange = group median
#' and IQR of the per-sample medians; "+" = RIVM's own published per-sample
#' median (W1 only).
plot_rivm_fig7 <- function(samples_tbl, base_size = 10) {
  prim <- samples_tbl |>
    dplyr::filter(.data$scenario == "primary",
                  (.data$dataset == "Dutch rivers" & .data$method_variant %in% c("published", "published_rivm")) |
                    (.data$dataset == "This study" & .data$method_variant == RIVM_OURS_PRIMARY))
  n_tbl <- prim |> dplyr::distinct(.data$group, .data$sample_id) |> dplyr::count(.data$group)
  lvl <- .rivm_row_order(unique(prim$group[prim$dataset == "Dutch rivers"]),
                         unique(prim$group[prim$dataset == "This study"]))
  # Each row uses the river-specific slope that produced its rescaling.
  fs <- function(x) sub("^-", "−", sprintf("%.2f", x))
  prim <- prim |>
    dplyr::left_join(n_tbl, by = "group") |>
    dplyr::mutate(
      row = paste0(.data$group, " (a = ", fs(.data$a_psd), ", n = ", .data$n, ")"),
      group = factor(.data$group, levels = lvl))

  one_panel <- function(win, title) {
    d <- dplyr::filter(prim, .data$window == win)
    row_lvls <- d |> dplyr::distinct(.data$group, .data$method_variant, .data$row) |>
      dplyr::arrange(.data$group, .data$method_variant != RIVM_OURS_PRIMARY) |> dplyr::pull(.data$row) |> unique()
    d <- dplyr::mutate(d, row = factor(.data$row, levels = rev(row_lvls)))
    samp <- dplyr::filter(d, .data$value_type != "published")
    pubr <- dplyr::filter(d, .data$value_type == "published")
    grp <- samp |>
      dplyr::group_by(.data$dataset, .data$row, .data$technique) |>
      dplyr::summarise(med = stats::median(.data$conc_p50), q25 = stats::quantile(.data$conc_p50, .25),
                       q75 = stats::quantile(.data$conc_p50, .75), .groups = "drop")
    jit <- ggplot2::position_jitter(width = 0, height = 0.22, seed = 11)
    p <- ggplot2::ggplot(samp, ggplot2::aes(y = .data$row, colour = .data$dataset)) +
      # x is mapped (unused by the line) so position_jitter() can operate on this layer.
      ggplot2::geom_linerange(ggplot2::aes(x = .data$conc_p50, xmin = .data$conc_p05, xmax = .data$conc_p95),
                              position = jit, linewidth = 0.25, alpha = 0.45) +
      ggplot2::geom_point(ggplot2::aes(x = .data$conc_p50, shape = .data$technique),
                          position = jit, size = 1.4, stroke = 0.4, fill = NA, alpha = 0.8) +
      ggplot2::geom_pointrange(data = grp,
                               ggplot2::aes(x = .data$med, xmin = .data$q25, xmax = .data$q75,
                                            shape = .data$technique, fill = .data$dataset),
                               colour = "black", linewidth = 1.1, size = 0.45, stroke = 0.5)
    if (nrow(pubr)) {
      p <- p + ggplot2::geom_point(data = pubr, ggplot2::aes(x = .data$conc_p50, alpha = "RIVM published"),
                                   shape = 3, colour = "black", size = 1.2, stroke = 0.4,
                                   position = ggplot2::position_jitter(width = 0, height = 0.22, seed = 12)) +
        ggplot2::scale_alpha_manual(NULL, values = c("RIVM published" = 1))
    }
    p +
      ggplot2::facet_grid(dataset ~ ., scales = "free_y", space = "free_y") +
      .rivm_log_x(expression("Concentration (particles m"^-3*")")) +
      ggplot2::scale_colour_manual("Dataset", values = RIVM_DATASET_COLS) +
      ggplot2::scale_fill_manual("Dataset", values = RIVM_DATASET_COLS) +
      ggplot2::scale_shape_manual("Technique", values = RIVM_TECH_SHAPES) +
      ggplot2::labs(y = NULL, title = title) +
      rivm_theme(base_size) +
      ggplot2::guides(alpha = ggplot2::guide_legend(override.aes = list(shape = 3, size = 2)),
                      fill = "none")
  }
  wl <- rivm_windows()
  pa <- one_panel("W2", paste0("(a) ", wl$window_label[1], " (primary)"))
  pb <- one_panel("W1", paste0("(b) ", wl$window_label[2], " (extrapolated)"))
  patchwork::wrap_plots(pa, pb, ncol = 1, heights = c(1, 1.35)) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical",
                   legend.spacing.y = ggplot2::unit(0, "pt"),
                   legend.margin = ggplot2::margin(0, 2, 0, 2),
                   legend.spacing.x = ggplot2::unit(2, "pt"),
                   legend.title = ggplot2::element_text(size = base_size),
                   legend.text = ggplot2::element_text(size = base_size - 1),
                   legend.box.spacing = ggplot2::unit(2, "pt")) &
    ggplot2::guides(shape = ggplot2::guide_legend(override.aes = list(linewidth = 0, fill = "white")),
                    colour = ggplot2::guide_legend(override.aes = list(shape = 16, linewidth = 0)))
}

#' Figure 8: differential PSD slopes, Dutch rivers vs this study
plot_rivm_fig8 <- function(slopes_tbl, mpsize_ref = rivm_mpsizebase_ref(), base_size = 10) {
  d <- slopes_tbl |>
    dplyr::mutate(row = paste0(.data$label, "  [", .data$fit_window, "]"))
  # Table order top-to-bottom within each facet.
  d$row <- factor(d$row, levels = rev(unique(d$row)))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$a, y = .data$row, colour = .data$dataset)) +
    ggplot2::geom_rect(data = mpsize_ref, inherit.aes = FALSE,
                       ggplot2::aes(xmin = .data$mean - .data$sd, xmax = .data$mean + .data$sd,
                                    ymin = -Inf, ymax = Inf, fill = .data$ref),
                       alpha = 0.12) +
    ggplot2::geom_vline(data = mpsize_ref, ggplot2::aes(xintercept = .data$mean, linetype = .data$ref),
                        colour = "grey35", linewidth = 0.4) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$struct_lo, xmax = .data$struct_hi), linewidth = 0.3) +
    ggplot2::geom_linerange(ggplot2::aes(xmin = .data$ci_lo, xmax = .data$ci_hi), linewidth = 1.1) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$technique), size = 2, fill = "white", stroke = 0.6) +
    ggplot2::facet_grid(facet ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_colour_manual("Dataset", values = RIVM_DATASET_COLS) +
    ggplot2::scale_shape_manual("Technique", values = RIVM_TECH_SHAPES) +
    ggplot2::scale_fill_manual("MPSizeBase mean ± SD", values = c("grey40", "grey70")) +
    ggplot2::scale_linetype_manual("MPSizeBase mean ± SD", values = c("dashed", "dotted")) +
    ggplot2::labs(x = expression("Differential PSD slope, " * italic(a)[psd] * "  (" * italic(a) == -alpha * ")"),
                  y = NULL) +
    rivm_theme(base_size) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), legend.box = "vertical",
                   legend.spacing.y = ggplot2::unit(0, "pt"), legend.margin = ggplot2::margin(0, 0, 0, 0),
                   legend.box.spacing = ggplot2::unit(4, "pt"))
}

#' MPSizeBase morphology means used as the Figure 8 reference band
#' (Segur et al. 2026; already cited in the SI).
rivm_mpsizebase_ref <- function() {
  tibble::tibble(ref = c("fragment (−2.66 ± 0.68)", "fiber (−1.86 ± 0.36)"),
                 mean = c(-2.66, -1.86), sd = c(0.68, 0.36))
}

#' Figure S-A: ECDF of per-sample rescaled concentrations by window
plot_rivm_ecdf <- function(samples_tbl, base_size = 10) {
  d <- samples_tbl |>
    dplyr::filter(.data$scenario == "primary",
                  (.data$dataset == "Dutch rivers" & .data$method_variant %in% c("published", "harmonised")) |
                    (.data$dataset == "This study" & .data$method_variant == RIVM_OURS_PRIMARY)) |>
    dplyr::mutate(uncertainty = dplyr::case_when(
      .data$method_variant == "published" ~ "RIVM method (published)",
      .data$method_variant == "harmonised" ~ "This study's method (harmonised)",
      TRUE ~ "This study (river-specific slopes)"),
      window = factor(.data$window, levels = c("W2", "W1"), labels = rivm_windows()$window_label))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$conc_p50, colour = .data$group, linetype = .data$uncertainty)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = 0.5) +
    ggplot2::facet_wrap(~ window, ncol = 2, scales = "free_x") +
    .rivm_log_x(expression("Per-sample median concentration (particles m"^-3*")")) +
    ggplot2::scale_colour_manual("Water body", values = rep(OKABE_ITO, 2)) +
    ggplot2::scale_linetype_manual("Uncertainty method",
                                   values = c("RIVM method (published)" = "solid",
                                              "This study's method (harmonised)" = "dashed",
                                              "This study (river-specific slopes)" = "solid")) +
    ggplot2::labs(y = "Cumulative proportion of samples") +
    rivm_theme(base_size) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey90"), legend.box = "vertical")
}

#' Figure S-B: Dutch-data sensitivity panels (group medians and IQR)
plot_rivm_sensitivity <- function(groups_tbl, base_size = 10) {
  wl <- rivm_windows()
  pr <- function(d, title, colour_var, xlab = expression("Concentration (particles m"^-3*")")) {
    d <- dplyr::mutate(d, window = factor(.data$window, levels = wl$window, labels = wl$window_label))
    ggplot2::ggplot(d, ggplot2::aes(x = .data$median, xmin = .data$q25, xmax = .data$q75,
                                    y = .data$group, colour = .data[[colour_var]])) +
      ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.6), size = 0.3, linewidth = 0.6) +
      ggplot2::facet_wrap(~ window, scales = "free_x") +
      .rivm_log_x(xlab) +
      ggplot2::scale_colour_manual(NULL, values = OKABE_ITO[c(5, 6, 3, 1)]) +
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1)) +
      ggplot2::labs(y = NULL, title = title) +
      rivm_theme(base_size) +
      ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = base_size + 1),
                     panel.spacing.x = ggplot2::unit(12, "pt"))
  }
  g <- groups_tbl
  p1 <- pr(dplyr::filter(g, .data$dataset == "Dutch rivers", .data$method_variant == "published",
                         .data$scenario %in% c("primary", "rubber excluded")),
           "(1) Rubber as reported vs excluded", "rubber_treatment")
  p2 <- pr(dplyr::filter(g, .data$technique == "LDIR", .data$method_variant == "published",
                         .data$scenario %in% c("primary", "LDIR LLm = 20 um")) |>
             dplyr::mutate(LLm = ifelse(.data$scenario == "primary", "LLm = 10 µm (metadata)", "LLm = 20 µm (report text)")),
           "(2) LDIR lower limit 10 vs 20 µm", "LLm")
  p3 <- pr(dplyr::filter(g, .data$dataset == "Dutch rivers", .data$scenario == "primary",
                         .data$method_variant %in% c("published", "harmonised")) |>
             dplyr::mutate(method = ifelse(.data$method_variant == "published", "RIVM method (published)", "harmonised (this study's method)")),
           "(3) Uncertainty method", "method")
  patchwork::wrap_plots(p1, p2, p3, ncol = 2)
}

#' Figure S-C: W1 correction factor vs slope, one point per water body and series
#'
#' Within a water body every sample shares one slope and (nearly) one size
#' window, so per-sample factors are collapsed to the median of the per-sample
#' medians, with the widest per-sample 5-95% interval.
plot_rivm_cf_alpha <- function(samples_tbl, base_size = 10) {
  d <- samples_tbl |>
    dplyr::filter(.data$window == "W1", .data$scenario == "primary",
                  .data$method_variant %in% c("published", "river")) |>
    dplyr::mutate(label = dplyr::case_when(
      .data$method_variant == "river" ~ paste0(.data$group, " (river slope)"),
      TRUE ~ .data$group)) |>
    dplyr::group_by(.data$dataset, .data$technique, .data$label, .data$a_psd) |>
    dplyr::summarise(cf_p50 = stats::median(.data$cf_p50), cf_p05 = min(.data$cf_p05),
                     cf_p95 = max(.data$cf_p95), n = dplyr::n(), .groups = "drop")
  rng <- range(d$cf_p50[d$dataset == "Dutch rivers"])
  ggplot2::ggplot(d, ggplot2::aes(x = .data$a_psd, y = .data$cf_p50, colour = .data$dataset)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = rng[1], ymax = rng[2],
                      fill = RIVM_DATASET_COLS[["Dutch rivers"]], alpha = 0.08) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = .data$cf_p05, ymax = .data$cf_p95), linewidth = 0.4, alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(shape = .data$technique), size = 2.2, fill = "white", stroke = 0.7) +
    ggrepel::geom_text_repel(ggplot2::aes(label = .data$label), size = base_size / 3.4,
                             show.legend = FALSE, min.segment.length = 0, box.padding = 0.4, seed = 1) +
    ggplot2::scale_y_log10(labels = scales::label_comma(), breaks = 10^(0:7)) +
    ggplot2::scale_colour_manual("Dataset", values = RIVM_DATASET_COLS) +
    ggplot2::scale_shape_manual("Technique", values = RIVM_TECH_SHAPES) +
    ggplot2::labs(x = expression("Differential PSD slope, " * italic(a)[psd]),
                  y = paste0("Correction factor to 1–5000 µm", "
", "(median, 5–95%)")) +
    rivm_theme(base_size) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(colour = "grey90"))
}

#' Save a figure as 300-dpi PNG + PDF, with its data as CSV alongside
save_rivm_figure <- function(plot, name, data, width = 7, height = 6, dir = "figures") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(file.path(dir, paste0(name, ".png")), plot, width = width, height = height,
                  dpi = 300, device = ragg::agg_png)
  ggplot2::ggsave(file.path(dir, paste0(name, ".pdf")), plot, width = width, height = height,
                  device = grDevices::cairo_pdf)
  utils::write.csv(data, file.path(dir, paste0(name, "_data.csv")), row.names = FALSE)
  invisible(file.path(dir, paste0(name, c(".png", ".pdf", "_data.csv"))))
}

# ── Manuscript values (read by the docx-editing step; never hand-typed) ─────

.fmt_n <- function(x) {
  # Two significant figures; >= 1e5 as "m.m × 10^k" with Unicode superscripts.
  sup <- c("0" = "⁰", "1" = "¹", "2" = "²", "3" = "³", "4" = "⁴",
           "5" = "⁵", "6" = "⁶", "7" = "⁷", "8" = "⁸", "9" = "⁹")
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    if (v < 1e5) return(formatC(signif(v, 2), format = "d", big.mark = ","))
    k <- floor(log10(signif(v, 2)))
    paste0(sprintf("%.1f", signif(v, 2) / 10^k), " × 10",
           paste(sup[strsplit(as.character(k), "")[[1]]], collapse = ""))
  }, character(1))
}

.fmt_slope <- function(x) sub("^-", "−", sprintf("%.2f", x))

#' Named values for manuscript placeholders (particles m^-3 unless stated)
rivm_manuscript_values <- function(samples_tbl, groups_tbl, slopes_tbl) {
  # IQR for n >= 4; otherwise the range (an IQR of 2-3 values is not meaningful).
  fmt_conc <- function(med, q25, q75, n, lo = q25, hi = q75) {
    ifelse(n >= 4,
           sprintf("%s (IQR %s–%s; n = %d)", .fmt_n(med), .fmt_n(q25), .fmt_n(q75), n),
           sprintf("%s (range %s–%s; n = %d)", .fmt_n(med), .fmt_n(lo), .fmt_n(hi), n))
  }
  key <- function(g) gsub("[^A-Za-z]", "", sub(" \\(.*", "", g))
  conc <- groups_tbl |>
    dplyr::filter(.data$scenario == "primary",
                  (.data$dataset == "Dutch rivers" & .data$method_variant %in% c("published", "harmonised")) |
                    (.data$dataset == "This study" & .data$method_variant == RIVM_OURS_PRIMARY)) |>
    dplyr::transmute(key = paste0(ifelse(.data$dataset == "This study", "ours_", "dutch_"),
                                  key(.data$group), "_", .data$method_variant, "_", .data$window),
                     value = .data$median, q25 = .data$q25, q75 = .data$q75, n = .data$n,
                     formatted = fmt_conc(.data$median, .data$q25, .data$q75, .data$n, .data$min, .data$max))
  # This study, all four rivers pooled across samples.
  ours_all <- samples_tbl |>
    dplyr::filter(.data$dataset == "This study", .data$scenario == "primary",
                  .data$method_variant == RIVM_OURS_PRIMARY) |>
    dplyr::group_by(.data$method_variant, .data$window) |>
    dplyr::summarise(value = stats::median(.data$conc_p50), q25 = stats::quantile(.data$conc_p50, .25),
                     q75 = stats::quantile(.data$conc_p50, .75), n = dplyr::n(),
                     cf = stats::median(.data$cf_p50), .groups = "drop") |>
    dplyr::mutate(key = paste0("ours_allrivers_", .data$method_variant, "_", .data$window),
                  formatted = fmt_conc(.data$value, .data$q25, .data$q75, .data$n)) |>
    dplyr::select("key", "value", "q25", "q75", "n", "formatted", "cf")
  cf_rows <- ours_all |>
    dplyr::transmute(key = sub("^ours_allrivers_", "cf_ours_", .data$key),
                     value = .data$cf, formatted = .fmt_n(.data$cf))
  dutch_cf <- samples_tbl |>
    dplyr::filter(.data$dataset == "Dutch rivers", .data$method_variant == "published",
                  .data$scenario == "primary", .data$window == "W1")
  cf_rows <- dplyr::bind_rows(cf_rows, tibble::tibble(
    key = c("cf_dutch_W1_min", "cf_dutch_W1_max"),
    value = range(dutch_cf$cf_p50), formatted = .fmt_n(range(dutch_cf$cf_p50))))
  slope_stats <- function(lbl, a) {
    q <- stats::quantile(a, c(.25, .5, .75), names = FALSE)
    # An IQR of fewer than four values is not meaningful; report the range instead.
    spread <- if (length(a) >= 4) sprintf("IQR %s to %s", .fmt_slope(q[1]), .fmt_slope(q[3])) else
      sprintf("range %s to %s", .fmt_slope(min(a)), .fmt_slope(max(a)))
    tibble::tibble(key = lbl, value = q[2], q25 = q[1], q75 = q[3], n = length(a),
                   formatted = sprintf("%s (%s; n = %d)", .fmt_slope(q[2]), spread, length(a)))
  }
  sl <- slopes_tbl
  slopes <- dplyr::bind_rows(
    slope_stats("slope_ours_rivers", sl$a[sl$facet == "This study: by river"]),
    slope_stats("slope_dutch_FTIR_waterbodies", sl$a[sl$facet == "Dutch FTIR" & sl$label != "All"]),
    slope_stats("slope_dutch_LDIR_waterbodies", sl$a[sl$facet == "Dutch LDIR" & sl$label != "All"]),
    slope_stats("slope_dutch_all_waterbodies", sl$a[sl$dataset == "Dutch rivers" & sl$label != "All"]))
  dplyr::bind_rows(conc, dplyr::select(ours_all, -"cf"), cf_rows, slopes)
}

# ── Standalone bootstrap (outside a full render of the main qmd) ─────────────

#' Recreate the pipeline objects the benchmark needs, without a full render
#'
#' Loads the cached C-PSD fits from the main document's knitr cache and
#' rebuilds `monitoring` from the monitoring-load chunk. Inside a full render
#' these objects already exist and this function is not called.
rivm_bootstrap_pipeline_objects <- function(proj_root, n_mc = 20000, slope_structural_sd = 0.25,
                                            envir = parent.frame()) {
  cd <- file.path(proj_root, "R", "probabilistic_risk_characterization_cache", "html")
  for (p in "cpsd-per-river-fits") {
    f <- list.files(cd, paste0("^", p, "_.*[.]rdb$"), full.names = TRUE)
    if (!length(f)) stop("No knitr cache for chunk '", p, "'; render the main qmd first.", call. = FALSE)
    f <- f[which.max(file.mtime(f))]
    lazyLoad(sub("[.]rdb$", "", f), envir = envir)
  }
  assign("monitoring",
         readRDS(file.path(proj_root, "data_input", "Part_dets_summ_final.rds")) |>
           dplyr::mutate(Client_ID_MSSupdate = dplyr::recode(.data$Client_ID_MSSupdate,
                                                             "SRR20240205SS_2" = "SRR20240205SS")) |>
           dplyr::filter(.data$sample_type == "river water", .data$material_simple == "plastic",
                         .data$sample_or_blank == "sample") |>
           dplyr::group_by(sample_id = .data$Client_ID_MSSupdate, river = .data$sample_location) |>
           dplyr::summarise(C_reported_pL = sum(.data$extrap_conc_PPL), .groups = "drop") |>
           dplyr::mutate(C_measured_pL = .data$C_reported_pL),
         envir = envir)
  assign("n_mc", n_mc, envir = envir)
  assign("slope_structural_sd", slope_structural_sd, envir = envir)
  invisible(TRUE)
}

# ── Figure captions (single source for the qmd and the manuscript) ───────────

#' Common caveat text required in every benchmark legend
rivm_caption_common <- function() {
  paste(
    "Slope convention: differential PSD slope a (dN/dL ∝ L^a); RIVM's positive PDF exponent α is converted as a = −α.",
    "Fitting methods differ: Dutch slopes are poweRlaw maximum-likelihood fits above an estimated xmin to particles ≤ 5000 µm, aggregated per water body and technique (Quik et al. 2026); this study's slopes are maximum-likelihood fits of a power law truncated at both ends of a fixed 100–500 µm window (the closest like-for-like estimator; the former C-PSD slopes were 0.45–0.9 steeper for three rivers).",
    "Systems differ: large perennial Dutch rivers and a canal (FTIR, Mintenig et al. 2020, 2025; LDIR, Mughini-Gras et al. 2021) versus discrete pumped samples from small, episodic Central Coast rivers (µFTIR).",
    "Dutch concentrations are study-reported totals (authors' blank and subsampling corrections), not recomputed from particles."
  )
}

#' Figure captions keyed by figure id; numeric content comes from `vals`
#' (rivm_manuscript_values() output).
rivm_figure_captions <- function(vals) {
  v <- function(k) vals$formatted[vals$key == k]
  list(
    fig7 = paste(
      "Rescaled microplastic concentrations in Dutch rivers and the four Monterey Bay rivers, (a) rescaled to 50–500 µm (W2, primary: interior to every measured range) and (b) extrapolated to 1–5000 µm (W1).",
      "Open symbols, per-sample median; thin bars, per-sample 5–95% Monte Carlo interval; thick point ranges, median and interquartile range of the per-sample medians for each water body; symbol shape, analytical technique; colour, dataset. Row labels give the differential slope a used to rescale that row and the number of samples; this study's W2 rows are measurements, so the river-specific fitted slope is shown there for reference.",
      "Value types: Dutch W2 and W1, reconstructed here with the RIVM uncertainty method (triangular α and ±10% size limits); “+”, RIVM's own published per-sample medians (W1 only); this study W2, measured (50–500 µm sieve fraction, CF = 1); this study W1, reconstructed from each river's own single slope.",
      "Dutch FTIR slopes were fitted above xmin ≈ 170–270 µm but are applied down to 20 µm; W1 differences are dominated by slope and should be read as secondary.",
      "Dommel combines Mintenig et al. (2020) and Mintenig et al. (2025).",
      rivm_caption_common()),
    fig8 = paste(
      "Differential particle-length PSD slopes (a) for Dutch water bodies (FTIR and LDIR; RIVM 2026-0069) and this study (by river).",
      "Points, estimate; thick bars, 95% CI (Dutch: RIVM bootstrap CI as published; this study: ±1.96 SE); thin bars, 95% interval after adding the structural slope SD (0.25) used in this study's Monte Carlo.",
      "Bracketed text gives each fit window (Dutch: estimated xmin, fit extends to 5000 µm; this study: LOD window).",
      "Shaded bands, MPSizeBase morphology means ± SD (Segur et al. 2026). Value types: Dutch, published; this study, measured.",
      rivm_caption_common()),
    figSA = paste(
      "Empirical cumulative distributions of per-sample median rescaled concentrations by water body for W2 (50–500 µm) and W1 (1–5000 µm).",
      "Dutch: solid, RIVM uncertainty method; dashed, this study's harmonised method (truncated-normal slope with structural SD, fixed size limits); both reconstructed. This study: river-specific single slopes (W2 measured; W1 reconstructed).",
      rivm_caption_common()),
    figSB = paste(
      "Sensitivity of Dutch water-body medians (points) and interquartile ranges (bars) of per-sample medians: (1) totals as reported versus excluding rubber (Mintenig: “Rubbers”; Mughini-Gras: isoprene; Mintenig 2025: EPDM rubber), since this study's polymer library has no rubber class;",
      "(2) LDIR lower size limit 10 µm (sample metadata) versus 20 µm (RIVM report text); (3) RIVM versus harmonised uncertainty method. All Dutch values reconstructed.",
      rivm_caption_common()),
    figSC = paste(
      sprintf("Correction factors to 1–5000 µm (median and 5–95%%) versus differential slope. Dutch per-sample factors span %s–%s (RIVM report: 46–440; shaded band); this study's factors (50–500 µm sieve window) use each river's own slope and are labelled by river.",
              v("cf_dutch_W1_min"), v("cf_dutch_W1_max")),
      "All values reconstructed.",
      rivm_caption_common())
  )
}

#' SI table of included sample sets (one row per study x water body)
#'
#' Rescaled columns: median of per-sample medians [lowest per-sample 5th
#' percentile - highest per-sample 95th percentile], primary variant (Dutch:
#' RIVM method; this study: river-specific single slopes).
rivm_si_table <- function(samples_tbl, slopes_tbl) {
  prim <- samples_tbl |>
    dplyr::filter(.data$scenario == "primary",
                  (.data$dataset == "Dutch rivers" & .data$method_variant == "published") |
                    (.data$dataset == "This study" & .data$method_variant == RIVM_OURS_PRIMARY))
  resc <- prim |>
    dplyr::group_by(.data$dataset, .data$study, .data$river, .data$technique, .data$window) |>
    dplyr::summarise(txt = sprintf("%s [%s–%s]", .fmt_n(stats::median(.data$conc_p50)),
                                   .fmt_n(min(.data$conc_p05)), .fmt_n(max(.data$conc_p95))),
                     .groups = "drop") |>
    tidyr::pivot_wider(names_from = "window", values_from = "txt")
  base <- prim |>
    dplyr::filter(.data$window == "W2") |>
    dplyr::group_by(.data$dataset, .data$study, .data$river, .data$technique) |>
    dplyr::summarise(n = dplyr::n(), LLm = min(.data$LLm), ULm = max(.data$ULm),
                     reported = sprintf("%s–%s", .fmt_n(min(.data$reported_conc_m3)),
                                        .fmt_n(max(.data$reported_conc_m3))),
                     .groups = "drop")
  sl <- slopes_tbl |>
    dplyr::filter(.data$facet %in% c("Dutch FTIR", "Dutch LDIR")) |>
    dplyr::transmute(technique = .data$technique, river = .data$label,
                     slope = sprintf("%s [%s, %s]", .fmt_slope(.data$a), .fmt_slope(.data$ci_lo), .fmt_slope(.data$ci_hi)),
                     fit_window = .data$fit_window)
  ours_sl <- slopes_tbl |>
    dplyr::filter(.data$facet == "This study: by river") |>
    dplyr::transmute(technique = .data$technique, river = .data$label,
                     slope = sprintf("%s [%s, %s]", .fmt_slope(.data$a), .fmt_slope(.data$ci_lo), .fmt_slope(.data$ci_hi)),
                     fit_window = .data$fit_window)
  out <- base |>
    dplyr::left_join(dplyr::bind_rows(sl, ours_sl), by = c("technique", "river")) |>
    dplyr::left_join(resc, by = c("dataset", "study", "river", "technique"))
  out |>
    dplyr::transmute(Study = .data$study, `Water body` = .data$river, n = .data$n,
                     Technique = .data$technique,
                     `Measured range (µm)` = sprintf("%g–%g", .data$LLm, .data$ULm),
                     `a [95% CI]` = .data$slope, `Fit window` = .data$fit_window,
                     `Reported (particles m⁻³)` = .data$reported,
                     `W2 50–500 µm` = .data$W2, `W1 1–5000 µm` = .data$W1)
}
