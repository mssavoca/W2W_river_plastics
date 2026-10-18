testthat::test_that("ingestion coverage reports records excluded from Food Dilution", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))
  tox <- data.frame(Species = factor(c("a", "b", "c", "d")), Group = c("Crustacea", "Fish", "Algae", "Mollusca"),
                    max.size.ingest.um = c(100, NA, NA, NA), body.length.cm = c(NA, 0.1, NA, NA))
  cov <- ingestion_coverage(tox)
  testthat::expect_equal(cov$n_excluded_fd, 1L)            # d: no mouth size, no body length (algae c not counted)
  testthat::expect_equal(cov$n_predicted_from_body, 1L)    # b: predicted from body length by PSSDplusplus
  testthat::expect_identical(cov$species_excluded_fd, "d")
})

testthat::test_that("installed PSSDplusplus excludes missing mouth sizes from Food Dilution", {
  testthat::skip_if(utils::packageVersion("PSSDplusplus") < "0.3.6")
  testthat::expect_true(all(c("missing_mouth", "mouth_size_source") %in% names(formals(PSSDplusplus::MC_sim_align_parallel))))
})
