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
