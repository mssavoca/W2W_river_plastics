testthat::test_that("tpl_mle recovers a known differential slope on a fixed window", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))
  r_tpl <- function(n, a, lo, hi) { al <- -a; u <- stats::runif(n); (lo^(1 - al) + u * (hi^(1 - al) - lo^(1 - al)))^(1 / (1 - al)) }
  set.seed(42)
  est <- replicate(50, tpl_mle(r_tpl(3000, -2.4, 50, 5000), 100, 500)[["a_psd"]])
  testthat::expect_lt(abs(mean(est) + 2.4), 0.03)
  # SE from the observed information is close to the sampling SD
  set.seed(7)
  x <- r_tpl(3000, -2.4, 50, 5000)
  m <- tpl_mle(x, 100, 500)
  testthat::expect_true(m[["se"]] > 0.5 * stats::sd(est) && m[["se"]] < 2 * stats::sd(est))
  testthat::expect_equal(unname(m[["n"]]), sum(x >= 100 & x < 500))
})

testthat::test_that("tpl_mle is unaffected by a deficit of particles above the window", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))
  r_tpl <- function(n, a, lo, hi) { al <- -a; u <- stats::runif(n); (lo^(1 - al) + u * (hi^(1 - al) - lo^(1 - al)))^(1 / (1 - al)) }
  set.seed(3)
  x <- r_tpl(20000, -2.4, 50, 5000)
  x_def <- x[x < 500 | stats::runif(length(x)) < 0.3]
  testthat::expect_equal(tpl_mle(x, 100, 500)[["a_psd"]], tpl_mle(x_def, 100, 500)[["a_psd"]])
})

testthat::test_that("as_headline_fit replaces slope and window and keeps the C-PSD values", {
  source(testthat::test_path("../../R/mp_risk_utils.R"))
  r_tpl <- function(n, a, lo, hi) { al <- -a; u <- stats::runif(n); (lo^(1 - al) + u * (hi^(1 - al) - lo^(1 - al)))^(1 / (1 - al)) }
  set.seed(11)
  x <- r_tpl(4000, -2.5, 50, 5000)
  f <- fit_cpsd_segur_r(x, bin_um = 5)
  h <- as_headline_fit(f, x, c(100, 500))
  testthat::expect_identical(h$estimator, "tpl_mle")
  testthat::expect_equal(c(h$lower_lod_um, h$upper_lod_um), c(100, 500))
  testthat::expect_equal(h$a_cpsd, h$a_psd + 1)
  testthat::expect_equal(h$cpsd$a_psd, f$a_psd)
  testthat::expect_equal(h$a_psd, tpl_mle(x, 100, 500)[["a_psd"]])
  # Plotting curve passes through the observed count at the window top
  cv <- fit_curve_logN(h, c(100, 499.999))
  testthat::expect_equal(10^cv$logN_fit[2], sum(x >= 500), tolerance = 0.01)
  # Too few particles in the window: the C-PSD fit is returned, flagged as a fallback
  h2 <- as_headline_fit(f, x[1:20], c(100, 500))
  testthat::expect_identical(h2$estimator, "cpsd (fallback)")
  testthat::expect_equal(h2$a_psd, f$a_psd)
})
