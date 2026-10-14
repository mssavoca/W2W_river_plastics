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

testthat::test_that("river-specific (grouped) rescaling applies each group's own CF", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))
  fit <- function(a, lo, hi) list(a_psd = a, se_a_psd = 0.01, lower_lod_um = lo, upper_lod_um = hi)
  fits <- list(A = fit(-2.5, 100, 300), B = fit(-3.5, 100, 300))
  draws <- list(A = rep(-2.5, 50), B = rep(-3.5, 50))
  mon <- data.frame(sample_id = c("a1", "a2", "a3", "b1", "b2", "b3"), river = rep(c("A", "B"), each = 3), C_measured_pL = 1,
                    Lmin_measured_um = 50, Lmax_measured_um = 500)
  set.seed(1)
  out <- correct_and_bootstrap_eed(mon, "C_measured_pL", cpsd_fit = fits$A, alpha_draws = draws$A,
                                   n_draws = 10, n_boot = 10, group_col = "river",
                                   cpsd_fit_by_group = fits, alpha_draws_by_group = draws)
  med <- stats::setNames(out$C_sample_median$C_corr_med, out$C_sample_median$sample_id)
  testthat::expect_equal(med[["a1"]], correction_factor(-2.5, 100, 300, 1, 5000), tolerance = 1e-10)
  testthat::expect_equal(med[["b1"]], correction_factor(-3.5, 100, 300, 1, 5000), tolerance = 1e-10)
})
