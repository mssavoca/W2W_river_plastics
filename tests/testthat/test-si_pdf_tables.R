testthat::test_that("SI rescaling tables retain sample values and matrix summaries", {
  source(testthat::test_path("../../R/si_pdf_tables.R"))

  monitoring <- tibble::tibble(
    sample_id = c("sample-1", "sample-2"),
    C_measured_pL = c(10, 20),
    Lmin_measured_um = c(50, 50),
    Lmax_measured_um = c(500, 500),
    Lmin_biascorr_um = c(100, 100),
    Lmax_biascorr_um = c(400, 400),
    a_psd_default = c(-2.1, -2.1)
  )
  corrected_draws <- tibble::tibble(
    sample_id = rep(c("sample-1", "sample-2"), each = 3),
    cf = c(2, 3, 4, 5, 6, 7),
    C_corrected = c(20, 30, 40, 100, 120, 140)
  )

  samples <- si_rescaling_sample_rows(
    monitoring, corrected_draws, "C_measured_pL", "River water", "particles/L"
  )
  summary <- si_rescaling_matrix_summary(samples)

  testthat::expect_equal(nrow(samples), 2)
  testthat::expect_equal(samples$`Reported mesh size limit (µm)`, c("50–500", "50–500"))
  testthat::expect_equal(samples$`Measured concentration (median)`, c(10, 20))
  testthat::expect_equal(samples$`CF (median)`, c(3, 6))
  testthat::expect_equal(samples$`Rescaled concentration (median)`, c(30, 120))
  testthat::expect_equal(summary$n, 2)
  testthat::expect_equal(summary$`Measured mean`, 15)
  testthat::expect_equal(summary$`Measured IQR`, 5)
  testthat::expect_equal(summary$`Rescaled median`, 75)
  testthat::expect_equal(summary$`Rescaled IQR`, 45)
})
