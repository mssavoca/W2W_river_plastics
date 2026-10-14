source(testthat::test_path("../../R/mp_risk_utils.R"))
source(testthat::test_path("../../R/rivm_utils.R"))

rivm_test_paths <- function() rivm_paths(testthat::test_path("../.."))

testthat::test_that("rivm_cf matches RIVM F.Correctionfactor() reference values", {
  testthat::expect_equal(rivm_cf(10, 1, 20, 2, 2.5), 0.35355339, tolerance = 1e-7)
  testthat::expect_equal(rivm_cf(2000, 30, 5000, 1, 1.6), 8.319172, tolerance = 1e-6)
})

testthat::test_that("RIVM alpha maps to a_psd = -alpha in correction_factor()", {
  cases <- list(c(10, 1, 20, 2, 2.5), c(2000, 30, 5000, 1, 1.6),
                c(5000, 20, 500, 50, 2.29316), c(500, 10, 5000, 1, 3.76312))
  for (x in cases) {
    ULm <- x[1]; LLm <- x[2]; ULd <- x[3]; LLd <- x[4]; alpha <- x[5]
    testthat::expect_equal(
      rivm_cf(ULm, LLm, ULd, LLd, alpha),
      correction_factor(a = -alpha, L_meas_min = LLm, L_meas_max = ULm,
                        L_tar_min = LLd, L_tar_max = ULd, slope_convention = "differential"),
      tolerance = 1e-10)
  }
  # Existing Koelmans et al. (2020) guard, through the RIVM form.
  testthat::expect_equal(rivm_cf(2000, 30, 5000, 1, 1.6), 8.32, tolerance = 0.05 / 8.32)
})

testthat::test_that("rtriangular stays within bounds and centres on the mode", {
  x <- withr::with_seed(1, rtriangular(2e4, 2, 4, 2.5))
  testthat::expect_true(all(x >= 2 & x <= 4))
  testthat::expect_equal(mean(x), (2 + 4 + 2.5) / 3, tolerance = 0.01)
})

testthat::test_that("RIVM alphas read from CSV match the prototype table", {
  p <- rivm_test_paths()
  alphas <- if (rivm_repo_available(p$repo_dir)) read_rivm_alphas(p$repo_dir) else
    utils::read.csv(file.path(p$derived_dir, "rivm_alphas_longest.csv"))
  testthat::skip_if(is.null(alphas) || !nrow(alphas), "RIVM inputs unavailable")
  expected <- tibble::tribble(
    ~technique, ~river,               ~alpha,
    "FTIR",     "Meuse",              2.29316,
    "FTIR",     "Dommel",             2.35304,
    "FTIR",     "All",                2.30356,
    "LDIR",     "Meuse",              3.55762,
    "LDIR",     "Lek canal",          3.76312,
    "LDIR",     "Overijsselse Vecht", 3.31444,
    "LDIR",     "WWTP Werverschoof effluent canal", 2.44021,
    "LDIR",     "Rhine",              2.97751,
    "LDIR",     "All",                2.71976)
  got <- dplyr::inner_join(expected, alphas, by = c("technique", "river"), suffix = c("_exp", "_got"))
  testthat::expect_equal(nrow(got), nrow(expected))
  testthat::expect_equal(got$alpha_got, got$alpha_exp, tolerance = 1e-5)
  testthat::expect_true(all(alphas$a_psd == -alphas$alpha))
})

testthat::test_that("Dutch sample audit: exclusions and row counts", {
  p <- rivm_test_paths()
  testthat::skip_if_not(rivm_repo_available(p$repo_dir) ||
                          file.exists(file.path(p$derived_dir, "rivm_sample_audit.csv")),
                        "RIVM inputs unavailable")
  r <- rivm_load_inputs(p)
  s <- r$samples
  testthat::expect_false(any(s$water_type == "Effluent WWTP"))
  testthat::expect_false(any(s$LLm >= 300))
  testthat::expect_false(any(grepl("B[aä]uerlein|Leslie", s$study)))
  cnt <- dplyr::count(s, .data$study, .data$river)
  n_of <- function(st, rv) cnt$n[cnt$study == st & cnt$river == rv]
  testthat::expect_equal(n_of("Mintenig et al. 2020", "Meuse"), 11)
  testthat::expect_equal(n_of("Mintenig et al. 2020", "Dommel"), 18)
  testthat::expect_equal(n_of("Mughini-Gras et al. 2021", "Rhine"), 2)
  testthat::expect_equal(n_of("Mughini-Gras et al. 2021", "Lek canal"), 2)
  testthat::expect_equal(sum(grepl("LLm = 300", r$audit$exclusion_reason)), 9)
  testthat::expect_true(all(r$rubber_match$matched))
})

testthat::test_that("prototype regression targets (+/-10%) and published-vs-reconstructed (<2x)", {
  p <- rivm_test_paths()
  testthat::skip_if_not(rivm_repo_available(p$repo_dir), "RIVM clone unavailable")
  r <- rivm_load_inputs(p)
  dutch <- rivm_join_alphas(r$samples, r$alphas)
  tbl <- rescale_rivm(dutch, "published", n = 5000) |>
    dplyr::mutate(dataset = "Dutch rivers", scenario = "primary",
                  group = rivm_group_label(.data$river, .data$technique))
  chk <- rivm_regression_check(tbl)
  testthat::expect_true(all(chk$pass), info = paste(capture.output(print(chk)), collapse = "\n"))
  w1 <- dplyr::filter(tbl, .data$window == "W1")
  testthat::expect_true(min(w1$cf_p50) > 40 && max(w1$cf_p50) < 480)
})
