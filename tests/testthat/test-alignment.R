source(testthat::test_path("../../R/mp_risk_utils.R"))

testthat::test_that("monitoring collapses to one row per sample with summed concentrations", {
  df <- data.frame(sample_id = c("a", "a", "b"), material_class = c("PE", "PP", "PE"),
                   river = c("X", "X", "Y"), count = c(1, 2, 3), extrap_count = c(2, 4, 3),
                   C_measured_pL = c(0.01, 0.02, 0.015))
  out <- collapse_to_sample_totals(df, sum_cols = c("count", "extrap_count", "C_measured_pL"))
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_equal(out$C_measured_pL[out$sample_id == "a"], 0.03)
  df$river[2] <- "Z"
  testthat::expect_error(collapse_to_sample_totals(df, sum_cols = c("count", "extrap_count", "C_measured_pL")), "vary within a sample")
})

testthat::test_that("draw_rq_aligned refuses mismatched exposure/hazard psd_keys", {
  haz <- data.frame(ERM = "Food Dilution", HCx = 5, PNEC = rlnorm(20))
  attr(haz, "psd_key") <- "river:Carmel"
  ok  <- with_psd_key(rlnorm(20), "river:Carmel")
  bad <- with_psd_key(rlnorm(20), "river:pooled")
  testthat::expect_s3_class(draw_rq_aligned(list(Carmel = haz), list(Carmel = ok), n_risk = 10), "data.frame")
  testthat::expect_error(draw_rq_aligned(list(Carmel = haz), list(Carmel = bad), n_risk = 10), "ALIGNMENT RULE")
  testthat::expect_error(draw_rq_aligned(list(Carmel = haz), list(Carmel = rlnorm(20)), n_risk = 10), "ALIGNMENT RULE")
})

testthat::test_that("mc2d_risk requires group-aligned hazards for group-specific CFs", {
  mon <- data.frame(C_measured_pL = rlnorm(10), river = rep(c("A", "B"), 5))
  haz <- data.frame(ERM = "Food Dilution", HCx = 5, PNEC = rlnorm(20))
  cfs <- list(A = rlnorm(50), B = rlnorm(50))
  testthat::expect_error(mc2d_risk(mon, NULL, haz, 5, 10, cf_by_group = cfs), "group-aligned hazards")
  out <- mc2d_risk(mon, NULL, haz, 5, 10, cf_by_group = cfs, haz_by_group = list(A = haz, B = haz))
  testthat::expect_equal(nrow(out), 5)
})
