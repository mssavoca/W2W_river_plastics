# Part A (spec/MPSizeBase.md): bad_spectra polarity, particle-accounting
# waterfall, filter size-bias, and filter-variant sensitivity.
#
# Read-only with respect to production: this script re-derives everything from
# data_input/ and writes only data_output/particle_qc_*.csv. It does not edit
# the production qmd or any production object. Run from the project root:
#   Rscript R/particle_qc_audit.R            # exposure-side outputs (fast)
#   W2W_QC_RQ=1 Rscript R/particle_qc_audit.R  # + aligned-hazard RQ per variant (slow)

suppressMessages({
  library(dplyr); library(tidyr); library(stringr); library(lubridate)
})
source("R/mp_risk_utils.R")

out_dir <- "data_output"
run_rq  <- identical(Sys.getenv("W2W_QC_RQ"), "1")
n_boot  <- as.integer(Sys.getenv("W2W_QC_NBOOT", "1000"))
n_mc    <- 20000
slope_structural_sd <- 0.25
sample_types <- c(river = "river water", ocean = "ocean water", sand = "beach sand")

# ── Inputs (same ID corrections as the production qmd setup chunk) ───────────
monitoring_raw <- readRDS("data_input/Part_dets_summ_final.rds") |>
  mutate(Client_ID_MSSupdate = recode(Client_ID_MSSupdate,
                                      "CR020250611S" = "CR020250611SS",
                                      "SRR20240205SS_2" = "SRR20240205SS"))
parts_all <- readRDS("data_input/Part_dets_comb.rds") |>
  mutate(Client_ID_raw = Client_ID_MSSupdate,
         Client_ID_MSSupdate = recode(sub("_[0-9]+$", "", Client_ID_MSSupdate),
                                      "SRR20240205SS_2" = "SRR20240205SS"),
         length_um = max_length_um, width_um = min_length_um,
         shape = case_when(aspect_ratio >= 3 ~ "fiber", aspect_ratio <= 3 ~ "fragment"))

canonical_ids <- function(st) {
  monitoring_raw |>
    filter(sample_type == st, material_simple == "plastic", sample_or_blank == "sample") |>
    pull(Client_ID_MSSupdate) |> unique()
}

# ── A1: what bad_spectra encodes ──────────────────────────────────────────────
a1_threshold <- parts_all |>
  filter(!is.na(bad_spectra)) |>
  group_by(bad_spectra) |>
  summarise(n = n(), max_cor_val_min = min(max_cor_val), max_cor_val_max = max(max_cor_val),
            max_cor_val_median = median(max_cor_val), mean_snr_median = median(mean_snr),
            n_confident = sum(acc_analy_conf == "confident"), n_possible = sum(acc_analy_conf == "possible"),
            n_undetermined = sum(acc_analy_conf == "undetermined"), n_missing_polymer = sum(is.na(material_class)),
            length_median_um = median(max_length_um), .groups = "drop")
# Deterministic rule: bad_spectra == TRUE  <=>  max_cor_val <= 0.66
a1_rule_violations <- with(filter(parts_all, !is.na(bad_spectra)),
                           sum(bad_spectra != (max_cor_val <= 0.66)))
stopifnot(a1_rule_violations == 0)
readr::write_csv(a1_threshold, file.path(out_dir, "particle_qc_bad_spectra_polarity.csv"))

# ── Filter variants (F1 = flipped polarity, i.e. keep bad_spectra == FALSE) ──
qc_variants <- list(
  F0_production      = function(d) d$acc_analy_conf %in% "confident" & d$bad_spectra %in% TRUE,
  F1_polarity_flip   = function(d) d$acc_analy_conf %in% "confident" & d$bad_spectra %in% FALSE,
  F2_no_spectra      = function(d) d$acc_analy_conf %in% "confident",
  F3_no_conf         = function(d) d$bad_spectra %in% TRUE,
  F3b_no_conf_flip   = function(d) d$bad_spectra %in% FALSE,
  F4_all_plastic     = function(d) rep(TRUE, nrow(d))
)

plastic_rows <- function(st) {
  parts_all |>
    filter(sample_type == st, sample_or_blank == "sample",
           !material_class %in% c("mineral", "organic matter"), !is.na(material_class),
           Client_ID_MSSupdate %in% canonical_ids(st))
}

# ── Production-equivalent fits (used for LOD windows in A2/A3) ───────────────
fit_len <- function(x) fit_cpsd_segur_r(x, bin_um = 5)
safe_fit <- function(x, ...) {
  if (length(x) < 10) return(list(valid = FALSE))
  tryCatch(c(fit_cpsd_segur_r(x, ...), list(valid = TRUE)), error = function(e) list(valid = FALSE))
}

prod_river <- plastic_rows(sample_types[["river"]]) |> (\(d) d[qc_variants$F0_production(d), ])()
prod_lod_river <- lapply(split(prod_river$length_um, prod_river$sample_location), fit_len)
prod_lod_other <- lapply(sample_types[c("ocean", "sand")], function(st) {
  d <- plastic_rows(st); fit_len(d$length_um[qc_variants$F0_production(d)])
})
lod_lo_hi <- function(d, matrix) {
  if (matrix == "river") {
    cbind(vapply(d$sample_location, function(l) prod_lod_river[[l]]$lower_lod_um, numeric(1)),
          vapply(d$sample_location, function(l) prod_lod_river[[l]]$upper_lod_um, numeric(1)))
  } else cbind(rep(prod_lod_other[[matrix]]$lower_lod_um, nrow(d)), rep(prod_lod_other[[matrix]]$upper_lod_um, nrow(d)))
}

# ── A2: waterfall ─────────────────────────────────────────────────────────────
waterfall_one <- function(matrix, polarity = c("production", "flipped")) {
  polarity <- match.arg(polarity)
  st <- sample_types[[matrix]]
  keep_spec <- if (polarity == "production") TRUE else FALSE
  d0 <- parts_all
  steps <- list(
    "1 all rows (all matrices)"          = function(d) rep(TRUE, nrow(d)),
    "2 samples only (blanks removed)"    = function(d) d$sample_or_blank %in% "sample",
    "3 sample_type"                      = function(d) d$sample_type %in% st,
    "3b canonical sample set"            = function(d) d$Client_ID_MSSupdate %in% canonical_ids(st),
    "4 plastic (not mineral/organic)"    = function(d) !d$material_class %in% c("mineral", "organic matter") & !is.na(d$material_class),
    "5 acc_analy_conf == confident"      = function(d) d$acc_analy_conf %in% "confident",
    "6 bad_spectra condition"            = function(d) d$bad_spectra %in% keep_spec,
    "7 valid length"                     = function(d) is.finite(d$length_um) & d$length_um > 0,
    "8 inside 50-500 um"                 = function(d) d$length_um >= 50 & d$length_um <= 500
  )
  rows <- list(); d <- d0
  for (nm in names(steps)) {
    d <- d[steps[[nm]](d), ]
    rows[[nm]] <- tibble(matrix = matrix, polarity = polarity, stage = nm, n = nrow(d))
  }
  lh <- lod_lo_hi(d, matrix)
  d_lod <- d[d$length_um >= lh[, 1] & d$length_um < lh[, 2], ]
  bind_rows(c(unname(rows), list(tibble(matrix = matrix, polarity = polarity, stage = "9 inside LOD window (production fit)", n = nrow(d_lod)))))
}
waterfall <- bind_rows(lapply(names(sample_types), function(m) bind_rows(waterfall_one(m, "production"), waterfall_one(m, "flipped")))) |>
  group_by(matrix, polarity) |> mutate(n_dropped = lag(n) - n, pct_retained_of_prev = round(100 * n / lag(n), 1)) |> ungroup()
readr::write_csv(waterfall, file.path(out_dir, "particle_qc_audit_waterfall.csv"))

# ── A2: per-sample reconciliation against Part_dets_summ_final ───────────────
recon_one <- function(matrix) {
  st <- sample_types[[matrix]]
  summ <- monitoring_raw |>
    filter(sample_type == st, material_simple == "plastic", sample_or_blank == "sample") |>
    group_by(sample_id = Client_ID_MSSupdate) |>
    summarise(sample_location = first(sample_location), summ_count = sum(count), summ_extrap_count = sum(extrap_count),
              summ_conc = sum(extrap_conc_PPL), aliquot_factor = summ_extrap_count / summ_count,
              summ_n_polymer_classes = n(), .groups = "drop")
  d <- plastic_rows(st)
  raw_ids <- d |> group_by(sample_id = Client_ID_MSSupdate) |>
    summarise(raw_ids_merged = paste(sort(unique(Client_ID_raw)), collapse = ";"),
              n_instrument_runs = n_distinct(sample_id), .groups = "drop")
  cnts <- bind_rows(lapply(names(qc_variants), function(v) {
    tibble(sample_id = d$Client_ID_MSSupdate, keep = qc_variants[[v]](d), variant = v)
  })) |> filter(keep) |> count(sample_id, variant) |>
    pivot_wider(names_from = variant, values_from = n, names_prefix = "rows_", values_fill = 0)
  lh <- lod_lo_hi(d, matrix)
  f0 <- qc_variants$F0_production(d)
  win <- tibble(sample_id = d$Client_ID_MSSupdate,
                rows_F0_50_500 = f0 & d$length_um >= 50 & d$length_um <= 500,
                rows_F0_lod = f0 & d$length_um >= lh[, 1] & d$length_um < lh[, 2],
                rows_F1_50_500 = qc_variants$F1_polarity_flip(d) & d$length_um >= 50 & d$length_um <= 500,
                rows_F1_lod = qc_variants$F1_polarity_flip(d) & d$length_um >= lh[, 1] & d$length_um < lh[, 2]) |>
    group_by(sample_id) |> summarise(across(everything(), sum), .groups = "drop")
  summ |> left_join(raw_ids, by = "sample_id") |> left_join(cnts, by = "sample_id") |> left_join(win, by = "sample_id") |>
    mutate(across(starts_with("rows_"), ~ coalesce(.x, 0L)), matrix = matrix, .before = 1) |>
    mutate(ratio_F0_to_summ = rows_F0_production / summ_count,
           ratio_F1_to_summ = rows_F1_polarity_flip / summ_count,
           ratio_F4_to_summ = rows_F4_all_plastic / summ_count)
}
recon <- bind_rows(lapply(names(sample_types), recon_one))
readr::write_csv(recon, file.path(out_dir, "particle_qc_audit_reconciliation_by_sample.csv"))

# Class-level comparison: is the summary a subset of the particle table?
class_recon <- bind_rows(lapply(names(sample_types), function(matrix) {
  st <- sample_types[[matrix]]
  s <- monitoring_raw |> filter(sample_type == st, material_simple == "plastic", sample_or_blank == "sample") |>
    group_by(sample_id = Client_ID_MSSupdate, material_class) |> summarise(summ = sum(count), .groups = "drop")
  p <- plastic_rows(st) |> group_by(sample_id = Client_ID_MSSupdate, material_class) |>
    summarise(all = n(), F1 = sum(qc_variants$F1_polarity_flip(pick(everything()))), .groups = "drop")
  full_join(s, p, by = c("sample_id", "material_class")) |> mutate(across(c(summ, all, F1), ~ coalesce(.x, 0))) |>
    summarise(matrix = matrix, n_sample_class_cells = n(),
              cells_summary_zero_but_particles = sum(summ == 0 & all > 0),
              cells_summary_exceeds_all_rows = sum(summ > all),
              particles_in_classes_absent_from_summary = sum(all[summ == 0]),
              summary_count_in_cells_exceeding_rows = sum((summ - all)[summ > all]))
}))
readr::write_csv(class_recon, file.path(out_dir, "particle_qc_class_reconciliation.csv"))

# ── A3: size bias of each filter stage (retained vs dropped) ─────────────────
tvd <- function(a, b) {
  lv <- union(a, b); pa <- table(factor(a, lv)) / length(a); pb <- table(factor(b, lv)) / length(b)
  0.5 * sum(abs(pa - pb))
}
bias_compare <- function(d, keep, matrix, stage) {
  r <- d[keep, ]; x <- d[!keep, ]
  if (nrow(r) < 2 || nrow(x) < 2) return(NULL)
  lh_r <- lod_lo_hi(r, matrix); lh_x <- lod_lo_hi(x, matrix)
  ks <- suppressWarnings(stats::ks.test(r$length_um, x$length_um))
  tibble(matrix = matrix, stage = stage, n_retained = nrow(r), n_dropped = nrow(x),
         len_med_ret = median(r$length_um), len_med_drop = median(x$length_um),
         len_q25_ret = quantile(r$length_um, .25, names = FALSE), len_q75_ret = quantile(r$length_um, .75, names = FALSE),
         len_q25_drop = quantile(x$length_um, .25, names = FALSE), len_q75_drop = quantile(x$length_um, .75, names = FALSE),
         log10_median_ratio = log10(median(r$length_um) / median(x$length_um)),
         ks_D = unname(ks$statistic), ks_p = ks$p.value,
         share_50_500_ret = mean(r$length_um >= 50 & r$length_um <= 500), share_50_500_drop = mean(x$length_um >= 50 & x$length_um <= 500),
         share_lod_ret = mean(r$length_um >= lh_r[, 1] & r$length_um < lh_r[, 2]),
         share_lod_drop = mean(x$length_um >= lh_x[, 1] & x$length_um < lh_x[, 2]),
         fiber_share_ret = mean(r$shape == "fiber", na.rm = TRUE), fiber_share_drop = mean(x$shape == "fiber", na.rm = TRUE),
         polymer_tvd = tvd(r$material_class, x$material_class),
         top_polymer_ret = names(sort(table(r$material_class), decreasing = TRUE))[1],
         top_polymer_drop = names(sort(table(x$material_class), decreasing = TRUE))[1])
}
size_bias <- bind_rows(lapply(names(sample_types), function(matrix) {
  d <- plastic_rows(sample_types[[matrix]]) |> filter(is.finite(length_um), length_um > 0)
  conf <- d$acc_analy_conf %in% "confident"
  dc <- d[conf, ]
  bind_rows(
    bias_compare(d, conf, matrix, "acc_analy_conf == confident (of all plastic)"),
    bias_compare(dc, dc$bad_spectra %in% TRUE, matrix, "bad_spectra == TRUE kept (production, of confident)"),
    bias_compare(dc, dc$bad_spectra %in% FALSE, matrix, "bad_spectra == FALSE kept (flipped, of confident)"),
    bias_compare(d, qc_variants$F0_production(d), matrix, "F0 production vs all plastic"),
    bias_compare(d, qc_variants$F1_polarity_flip(d), matrix, "F1 flipped vs all plastic")
  )
}))
readr::write_csv(size_bias, file.path(out_dir, "particle_qc_size_bias.csv"))

# ── A4: filter-variant sensitivity on the production exposure pipeline ───────
prep_particles <- function(st, variant) {
  d <- plastic_rows(st); d <- d[qc_variants[[variant]](d), ]
  d <- assign_polymer_density(d, lookup = polymer_density_lookup())
  rt <- d |> group_by(shape) |> summarise(r_med = median(1 / aspect_ratio, na.rm = TRUE), .groups = "drop")
  d |> left_join(rt, by = "shape") |>
    mutate(height_um = r_med * width_um,
           V_um3 = case_when(shape == "fragment" ~ (pi / 6) * length_um * width_um * height_um,
                             shape == "fiber" ~ pi * (width_um / 2)^2 * length_um))
}
build_monitoring <- function(st) {
  monitoring_raw |>
    filter(sample_type == st, material_simple == "plastic", sample_or_blank == "sample") |>
    transmute(sample_id = Client_ID_MSSupdate, river = sample_location, sample_depth_general,
              count, extrap_count, C_reported_pL = extrap_conc_PPL,
              Lmin_reported_um = 50, Lmax_reported_um = 500, Lmin_measured_um = 50, Lmax_measured_um = 500) |>
    group_by(sample_id) |>
    summarise(across(c(count, extrap_count, C_reported_pL), sum),
              across(c(river, sample_depth_general, Lmin_reported_um, Lmax_reported_um, Lmin_measured_um, Lmax_measured_um), first),
              .groups = "drop")
}
fit_row <- function(f, label, variant, matrix, n) {
  if (!isTRUE(f$valid %||% TRUE) || is.null(f$a_psd)) return(tibble(variant, matrix, fit = label, n_particles = n))
  tibble(variant, matrix, fit = label, n_particles = n, a_psd = f$a_psd, se_a_psd = f$se_a_psd,
         lod_low_um = f$lower_lod_um, lod_high_um = f$upper_lod_um, r2 = f$r2, n_bins = f$n_bins)
}

variant_results <- function(variant) {
  # River: per-river slopes (production slope_application = "river")
  rp <- prep_particles(sample_types[["river"]], variant)
  locs <- sort(unique(rp$sample_location))
  fits_loc <- lapply(setNames(locs, locs), function(l) fit_len(rp$length_um[rp$sample_location == l]))
  fit_all <- fit_len(rp$length_um)
  fits <- bind_rows(
    fit_row(fit_all, "pooled", variant, "river", nrow(rp)),
    fit_row(safe_fit(rp$length_um[rp$shape == "fragment"], bin_um = 5), "fragment", variant, "river", sum(rp$shape == "fragment")),
    fit_row(safe_fit(rp$length_um[rp$shape == "fiber"], bin_um = 5), "fiber", variant, "river", sum(rp$shape == "fiber")),
    bind_rows(lapply(locs, function(l) fit_row(fits_loc[[l]], l, variant, "river", sum(rp$sample_location == l)))))

  mon <- build_monitoring(sample_types[["river"]]) |>
    lod_scope_monitoring_concentrations(raw_particles = rp, reported_conc_col = "C_reported_pL",
                                        output_conc_col = "C_measured_pL", cpsd_fit = fit_all,
                                        group_col = "river", cpsd_fit_by_group = fits_loc)
  alpha_pooled <- withr::with_seed(8001, alpha_dist(fit_all$a_psd, fit_all$se_a_psd, n = n_mc, structural_sd = slope_structural_sd))
  alpha_by_river <- alpha_draws_by_group(fits_loc, n = n_mc, structural_sd = slope_structural_sd, seed = 8101)
  corr <- withr::with_seed(9101, correct_and_bootstrap_eed(
    monitoring = mon, conc_col = "C_measured_pL", cpsd_fit = fit_all, alpha_draws = alpha_pooled,
    L_tar_min = 1, L_tar_max = 5000, n_draws = 3000, n_boot = n_boot, probs = c(0.5, 0.95),
    slope_convention = "differential", group_col = "river", cpsd_fit_by_group = fits_loc,
    alpha_draws_by_group = alpha_by_river))
  csm <- corr$C_sample_median |> left_join(distinct(mon, sample_id, river), by = "sample_id")

  by_river <- mon |> group_by(river) |>
    summarise(n_samples = n(), n_zero_lod_conc = sum(C_measured_pL == 0),
              lod_fraction_median = median(lod_particle_fraction),
              lod_fraction_pooled = sum(n_particles_lod_window) / sum(n_particles_reported_window),
              n_lod_particles_total = sum(n_particles_lod_window), .groups = "drop") |>
    mutate(CF_p50 = vapply(river, function(r) median(corr$combined_cf_by_group[[r]]), numeric(1)),
           W1_median = vapply(river, function(r) median(csm$C_corr_med[csm$river == r]), numeric(1)),
           variant = variant, matrix = "river", .before = 1)
  summary_row <- tibble(variant = variant, matrix = "river",
                        n_particles = nrow(rp), W1_median_all = median(csm$C_corr_med),
                        EED_q50 = median(corr$eed_boot$q50), EED_q95 = median(corr$eed_boot$q95),
                        n_zero_lod_conc = sum(mon$C_measured_pL == 0))

  # Ocean / sand: pooled slope (production)
  other <- lapply(c("ocean", "sand"), function(m) {
    op <- prep_particles(sample_types[[m]], variant)
    f <- safe_fit(op$length_um, bin_um = 5)
    if (!isTRUE(f$valid)) return(list(fit = fit_row(f, "pooled", variant, m, nrow(op)), sum = tibble(variant = variant, matrix = m, n_particles = nrow(op))))
    mo <- build_monitoring(sample_types[[m]]) |>
      lod_scope_monitoring_concentrations(raw_particles = op, reported_conc_col = "C_reported_pL",
                                          output_conc_col = "C_measured_pL", cpsd_fit = f)
    seed_a <- if (m == "ocean") 8003 else 8002; seed_e <- if (m == "ocean") 9103 else 9102
    al <- withr::with_seed(seed_a, alpha_dist(f$a_psd, f$se_a_psd, n = n_mc, structural_sd = slope_structural_sd))
    co <- withr::with_seed(seed_e, correct_and_bootstrap_eed(mo, "C_measured_pL", f, al, n_draws = 3000, n_boot = n_boot,
                                                              probs = c(0.5, 0.95), slope_convention = "differential"))
    list(fit = fit_row(f, "pooled", variant, m, nrow(op)),
         sum = tibble(variant = variant, matrix = m, n_particles = nrow(op), CF_p50 = median(co$combined_cf),
                      lod_fraction_median = median(mo$lod_particle_fraction), W1_median_all = median(co$C_sample_median$C_corr_med),
                      EED_q50 = median(co$eed_boot$q50), EED_q95 = median(co$eed_boot$q95),
                      n_zero_lod_conc = sum(mo$C_measured_pL == 0)))
  })

  list(fits = bind_rows(fits, bind_rows(lapply(other, `[[`, "fit"))),
       by_river = by_river,
       summary = bind_rows(summary_row, bind_rows(lapply(other, `[[`, "sum"))),
       rp = rp, fits_loc = fits_loc, alpha_by_river = alpha_by_river,
       eed_by_river = lapply(setNames(locs, locs), function(r) withr::with_seed(9121 + match(r, locs),
         bootstrap_eed(csm$C_corr_med[csm$river == r], n_boot = n_boot, probs = c(0.5, 0.95)))))
}

a4_variants <- c("F0_production", "F1_polarity_flip", "F2_no_spectra", "F3_no_conf", "F4_all_plastic")
a4 <- lapply(setNames(a4_variants, a4_variants), variant_results)
if (!run_rq) readr::write_csv(bind_rows(lapply(a4, `[[`, "fits")),     file.path(out_dir, "particle_qc_variant_fits.csv"))
if (!run_rq) readr::write_csv(bind_rows(lapply(a4, `[[`, "by_river")), file.path(out_dir, "particle_qc_variant_by_river.csv"))
if (!run_rq) readr::write_csv(bind_rows(lapply(a4, `[[`, "summary")),  file.path(out_dir, "particle_qc_variant_summary.csv"))

# F0 must reproduce the production export (guards that this script mirrors production).
prod_json <- "data_output/psd_rescaling_params.json"
if (file.exists(prod_json)) {
  pj <- jsonlite::read_json(prod_json)
  f0_w1 <- a4$F0_production$summary$W1_median_all[a4$F0_production$summary$matrix == "river"]
  cat(sprintf("F0 check: W1 river median script = %.4f | production JSON = %.4f\n", f0_w1, pj$rescaling$C_W1_river$median_L))
  for (r in names(pj$by_river)) cat(sprintf("  %s a_psd script = %.4f | JSON = %.4f\n", r,
                                            a4$F0_production$fits_loc[[r]]$a_psd, pj$by_river[[r]]$a_diff))
}

# ── Optional: headline RQ per variant with re-derived aligned hazards ────────
if (run_rq) {
  suppressMessages(library(PSSDplusplus))
  tox_data <- tomex2 |>
    filter(env_f == "Freshwater", Group != "Bacterium", Group != "Plant", effect.metric != "HONEC",
           tier_zero_tech_f == "Red Criteria Passed", tier_zero_risk_f == "Red Criteria Passed", risk.13 != 0)
  rq <- bind_rows(lapply(a4_variants, function(v) {
    res <- a4[[v]]; rp <- res$rp; locs <- names(res$fits_loc)
    rt <- rp |> group_by(shape) |> summarise(r = median(1 / aspect_ratio, na.rm = TRUE))
    haz <- setNames(lapply(seq_along(locs), function(i) {
      l <- locs[i]; sub <- rp[rp$sample_location == l, ]
      vlb <- volume_lod_bounds(fit_len(rp$length_um[rp$shape == "fragment"]), fit_len(rp$length_um[rp$shape == "fiber"]),
                               rt$r[rt$shape == "fragment"], rt$r[rt$shape == "fiber"])
      h <- aligned_freshwater_hazard(
        tox_data = tox_data, alpha_draws = res$alpha_by_river[[l]], aspect_ratio_vec = sub$aspect_ratio,
        fit_volume = fit_cpsd_segur_r(sub$V_um3, bin_um = 2e4, fit_range_um = vlb$all),
        fit_area = fit_cpsd_segur_r(sub$area_um2, bin_um = 500, fit_range_um = c(2000, 500000)),
        n = n_boot, seed = 5100 + 10 * i, psd_key = paste0("river:", l))
      h$haz
    }), locs)
    draws <- draw_rq_aligned(haz, setNames(lapply(locs, function(l) with_psd_key(res$eed_by_river[[l]]$q50, paste0("river:", l))), locs),
                             n_risk = 1000, seed = 9300)
    summarize_rq(draws) |> mutate(variant = v, .before = 1)
  }))
  readr::write_csv(rq, file.path(out_dir, "particle_qc_variant_rq.csv"))
}
cat("Part A audit complete.\n")
