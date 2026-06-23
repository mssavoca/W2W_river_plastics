# Standalone CDF/PDF fitting method (Weibull-like CDF) with a test data frame.
# Mirrors the approach in the referenced notebook while staying in base R.

cdf_function <- function(x, alpha, lambda) {
  1 - exp(-lambda * x^alpha)
}

parse_numeric_list <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_real_)
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  if (is.list(x)) {
    return(as.numeric(unlist(x)))
  }
  if (!is.character(x)) {
    return(NA_real_)
  }

  s <- trimws(x)
  if (nchar(s) == 0) {
    return(NA_real_)
  }

  s <- gsub("\\[|\\]|\\(|\\)", "", s)
  if (nchar(s) == 0) {
    return(NA_real_)
  }

  parts <- strsplit(s, "[,\\s]+")[[1]]
  parts <- parts[parts != ""]
  vals <- suppressWarnings(as.numeric(parts))
  if (all(is.na(vals))) {
    return(NA_real_)
  }
  vals
}

convert_pdf_to_cdf <- function(pdf, normalize = FALSE) {
  if (!is.numeric(pdf) || any(!is.finite(pdf))) {
    return(NA_real_)
  }
  if (normalize) {
    s <- sum(pdf)
    if (s > 0) pdf <- pdf / s
  }
  cumsum(pdf)
}

fit_cdf_curve <- function(x, y, start_alpha = 1, start_lambda = 1) {
  if (length(x) < 3 || length(y) < 3) {
    return(NULL)
  }
  if (any(!is.finite(x)) || any(!is.finite(y))) {
    return(NULL)
  }

  df <- data.frame(x = x, y = y)

  # Try bounded NLS (port algorithm) first.
  fit <- try(
    nls(
      y ~ 1 - exp(-lambda * x^alpha),
      data = df,
      start = list(alpha = start_alpha, lambda = start_lambda),
      algorithm = "port",
      lower = c(alpha = 1e-6, lambda = 1e-6),
      control = nls.control(warnOnly = TRUE, maxiter = 200)
    ),
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    # Fallback: optimize on log-params for positivity.
    obj <- function(par) {
      alpha <- exp(par[1])
      lambda <- exp(par[2])
      yhat <- cdf_function(x, alpha, lambda)
      sum((y - yhat)^2)
    }
    opt <- optim(par = log(c(start_alpha, start_lambda)), fn = obj)
    alpha <- exp(opt$par[1])
    lambda <- exp(opt$par[2])
  } else {
    coefs <- coef(fit)
    alpha <- unname(coefs["alpha"])
    lambda <- unname(coefs["lambda"])
  }

  yhat <- cdf_function(x, alpha, lambda)
  r <- suppressWarnings(cor(y, yhat, use = "pairwise.complete.obs"))
  r2 <- if (is.finite(r)) r^2 else NA_real_
  p_value <- tryCatch(cor.test(y, yhat)$p.value, error = function(e) NA_real_)

  list(alpha = alpha, lambda = lambda, r2 = r2, p_value = p_value)
}

merge_and_fit_cdf <- function(
  data_df,
  cdf_col = "CDF",
  pdf_col = "PDF",
  x_col = "Converted Size Mid-point",
  normalize_pdf = FALSE,
  id_col = "ID"
) {
  resolve_col <- function(df, name) {
    if (name %in% names(df)) return(name)
    alt <- make.names(name)
    if (alt %in% names(df)) return(alt)
    name
  }

  x_col <- resolve_col(data_df, x_col)
  cdf_col <- resolve_col(data_df, cdf_col)
  pdf_col <- resolve_col(data_df, pdf_col)
  id_col <- resolve_col(data_df, id_col)

  results <- vector("list", nrow(data_df))

  for (i in seq_len(nrow(data_df))) {
    row <- data_df[i, ]

    x_vals <- parse_numeric_list(row[[x_col]])
    if (length(x_vals) == 1 && is.na(x_vals)) {
      next
    }

    y_vals <- parse_numeric_list(row[[cdf_col]])
    if (length(y_vals) == 1 && is.na(y_vals)) {
      pdf_vals <- parse_numeric_list(row[[pdf_col]])
      if (length(pdf_vals) == 1 && is.na(pdf_vals)) {
        next
      }
      y_vals <- convert_pdf_to_cdf(pdf_vals, normalize = normalize_pdf)
    }

    if (!is.numeric(y_vals) || length(y_vals) != length(x_vals)) {
      next
    }

    fit <- fit_cdf_curve(x_vals, y_vals)
    if (is.null(fit)) {
      next
    }

    results[[i]] <- data.frame(
      ID = row[[id_col]],
      Alpha_CFD = fit$alpha,
      Lambda_CFD = fit$lambda,
      R2_CFD = fit$r2,
      p_value_CFD = fit$p_value
    )
  }

  out <- do.call(rbind, results)
  if (is.null(out)) {
    out <- data.frame(
      ID = character(0),
      Alpha_CFD = numeric(0),
      Lambda_CFD = numeric(0),
      R2_CFD = numeric(0),
      p_value_CFD = numeric(0)
    )
  }
  out
}

# ---- Test data frame ----
test_df <- data.frame(
  ID = c("A", "B", "C"),
  `Converted Size Mid-point` = c(
    "[0.1, 0.2, 0.5, 1, 2, 5]",
    "[0.1, 0.25, 0.5, 1, 2, 5]",
    "[0.1, 0.2, 0.4, 0.8, 1.6, 3.2]"
  ),
  PDF = c(
    "[0.02, 0.05, 0.12, 0.25, 0.28, 0.28]",
    NA,
    "[0.05, 0.08, 0.14, 0.22, 0.25, 0.26]"
  ),
  CDF = c(
    NA,
    "[0.02, 0.10, 0.22, 0.48, 0.74, 0.95]",
    NA
  ),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

fit_results <- merge_and_fit_cdf(test_df, normalize_pdf = FALSE)
print(fit_results)
