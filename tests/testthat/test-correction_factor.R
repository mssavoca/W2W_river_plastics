testthat::test_that("correction_factor uses differential PSD slope convention", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))

  cf <- correction_factor(
    a = -1.6,
    L_meas_min = 30,
    L_meas_max = 2000,
    L_tar_min = 1,
    L_tar_max = 5000,
    slope_convention = "differential"
  )

  testthat::expect_equal(as.numeric(cf), 8.319, tolerance = 0.01)
})

testthat::test_that("cumulative slopes are converted exactly once when requested", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))

  cf_cumulative <- correction_factor(
    a = -0.6,
    L_meas_min = 30,
    L_meas_max = 2000,
    L_tar_min = 1,
    L_tar_max = 5000,
    slope_convention = "cumulative"
  )
  cf_differential <- correction_factor(
    a = -1.6,
    L_meas_min = 30,
    L_meas_max = 2000,
    L_tar_min = 1,
    L_tar_max = 5000,
    slope_convention = "differential"
  )

  testthat::expect_equal(cf_cumulative, cf_differential)
})

testthat::test_that("LOD-scoped concentration and grouped rescaling use the same window", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))
  fit <- function(a, lo, hi) list(a_psd = a, se_a_psd = 0.01, lower_lod_um = lo, upper_lod_um = hi)
  fits <- list(A = fit(-2.5, 100, 300), B = fit(-3.5, 100, 300))
  draws <- list(A = rep(-2.5, 50), B = rep(-3.5, 50))
  mon <- data.frame(sample_id = c("a1", "a2", "a3", "b1", "b2", "b3"), river = rep(c("A", "B"), each = 3),
                    C_reported_pL = 10, count = 4, Lmin_reported_um = 50, Lmax_reported_um = 500)
  raw <- data.frame(
    Client_ID_MSSupdate = rep(mon$sample_id, each = 4),
    length_um = rep(c(75, 150, 250, 350), times = 6),
    count = 1
  )
  mon <- lod_scope_monitoring_concentrations(
    monitoring = mon, raw_particles = raw,
    reported_conc_col = "C_reported_pL", output_conc_col = "C_measured_pL",
    cpsd_fit = fits$A, group_col = "river", cpsd_fit_by_group = fits
  )
  testthat::expect_equal(mon$C_measured_pL, rep(5, 6))
  testthat::expect_equal(mon$Lmin_measured_um, rep(100, 6))
  testthat::expect_equal(mon$Lmax_measured_um, rep(300, 6))
  set.seed(1)
  out <- correct_and_bootstrap_eed(mon, "C_measured_pL", cpsd_fit = fits$A, alpha_draws = draws$A,
                                   n_draws = 10, n_boot = 10, group_col = "river",
                                   cpsd_fit_by_group = fits, alpha_draws_by_group = draws)
  med <- stats::setNames(out$C_sample_median$C_corr_med, out$C_sample_median$sample_id)
  testthat::expect_equal(med[["a1"]], 5 * correction_factor(-2.5, 100, 300, 1, 5000), tolerance = 1e-10)
  testthat::expect_equal(med[["b1"]], 5 * correction_factor(-3.5, 100, 300, 1, 5000), tolerance = 1e-10)
})

testthat::test_that("particle-rebuild basis scales QC rows by the aliquot factor and drops unmeasured samples", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))
  fit <- list(a_psd = -2.5, se_a_psd = 0.01, lower_lod_um = 100, upper_lod_um = 300)
  # s1: count 10 (summary), extrap 20 (aliquot 2), C_reported 4 /L -> 0.4 /L per QC row
  mon <- data.frame(sample_id = c("s1", "s2", "s3"), C_reported_pL = c(4, 6, 1), count = c(10, 3, 5),
                    Lmin_reported_um = 50, Lmax_reported_um = 500)
  raw <- data.frame(Client_ID_MSSupdate = c(rep("s1", 6), "s2"), length_um = c(75, 150, 150, 250, 350, 600, 75), count = 1)
  out <- suppressMessages(lod_scope_monitoring_concentrations(mon, raw, "C_reported_pL", "C_measured_pL", cpsd_fit = fit))
  testthat::expect_equal(out$sample_id, c("s1", "s2"))
  testthat::expect_equal(attr(out, "excluded_no_particle_data"), "s3")
  testthat::expect_equal(out$C_measured_pL, c(3 * 0.4, 0))           # 3 rows in [100, 300)
  testthat::expect_equal(out$C_rebuilt_reported_window, c(5 * 0.4, 2)) # rows in [50, 500]
  legacy <- lod_scope_monitoring_concentrations(mon, raw, "C_reported_pL", "C_measured_pL", cpsd_fit = fit,
                                                concentration_basis = "reported_fraction")
  testthat::expect_equal(legacy$C_measured_pL, c(4 * 3 / 5, 0, 0))
  d <- data.frame(acc_analy_conf = c("confident", "confident", "possible"), bad_spectra = c(FALSE, TRUE, TRUE))
  testthat::expect_equal(spectra_qc_pass(d), c(TRUE, FALSE, FALSE))
})
