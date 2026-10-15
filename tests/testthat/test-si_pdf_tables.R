testthat::test_that("SI rescaling tables retain sample values and matrix summaries", {
  source(testthat::test_path("../../R/si_pdf_tables.R"))

  monitoring <- tibble::tibble(
    sample_id = c("sample-1", "sample-2"),
    C_reported_pL = c(20, 40),
    C_measured_pL = c(10, 20),
    Lmin_reported_um = c(50, 50),
    Lmax_reported_um = c(500, 500),
    Lmin_measured_um = c(100, 100),
    Lmax_measured_um = c(400, 400),
    Lmin_biascorr_um = c(100, 100),
    Lmax_biascorr_um = c(400, 400),
    lod_particle_fraction = c(0.5, 0.5),
    a_psd_default = c(-2.1, -2.1),
    river = c("Carmel", "Pajaro")
  )
  corrected_draws <- tibble::tibble(
    sample_id = rep(c("sample-1", "sample-2"), each = 3),
    cf = c(2, 3, 4, 5, 6, 7),
    C_corrected = c(20, 30, 40, 100, 120, 140)
  )

  samples <- si_rescaling_sample_rows(
    monitoring, corrected_draws, "C_measured_pL", "River water", "particles/L",
    psd_model = "River-specific", location_col = "river"
  )
  pooled <- si_rescaling_sample_rows(
    dplyr::mutate(monitoring, a_psd_default = -1.8),
    dplyr::mutate(corrected_draws, cf = cf + 1, C_corrected = C_corrected * 1.2),
    "C_measured_pL", "River water", "particles/L",
    psd_model = "Pooled", location_col = "river"
  )
  summary <- si_rescaling_matrix_summary(dplyr::bind_rows(samples, pooled))
  location_summary <- si_rescaling_matrix_summary(
    dplyr::bind_rows(samples, pooled),
    group_cols = c("Matrix", "Location", "PSD model", "Concentration unit")
  )

  testthat::expect_equal(nrow(samples), 2)
  testthat::expect_equal(samples$`Reported mesh size limit (µm)`, c("50–500", "50–500"))
  testthat::expect_equal(samples$Location, c("Carmel", "Pajaro"))
  testthat::expect_true(all(samples$`PSD model` == "River-specific"))
  testthat::expect_equal(samples$`Reported concentration (median)`, c(20, 40))
  testthat::expect_equal(samples$`C-PSD-valid concentration (median)`, c(10, 20))
  testthat::expect_equal(samples$`CF (median)`, c(3, 6))
  testthat::expect_equal(samples$`Rescaled concentration (median)`, c(30, 120))
  testthat::expect_equal(nrow(summary), 2)
  testthat::expect_equal(summary$n, c(2, 2))
  testthat::expect_equal(summary$`C-PSD-valid mean`, c(15, 15))
  testthat::expect_equal(summary$`C-PSD-valid IQR`, c(5, 5))
  testthat::expect_equal(nrow(location_summary), 4)
  testthat::expect_true(all(location_summary$n == 1))
})
