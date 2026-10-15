# Fast path: regenerate docs/SI_Tables_S5-S11_PDF_fits_and_rescaling.docx (Section 16.1)
# without the pSSD++ / risk / sensitivity compute.
#
# Runs, outside knitr and in document order, only the chunks of
# R/probabilistic_risk_characterization.qmd that the SI-table chunk depends on
# (data loads, C-PSD fits, rescaling for river/ocean/sediment, piecewise and MLE
# comparator fits). Cached C-PSD fit chunks are loaded from the knitr cache when
# available. Uses the production budget unless W2W_BUDGET=quick is set.
#
# Usage (from the project root):  Rscript tools/run_si_tables_only.R

qmd <- "R/probabilistic_risk_characterization.qmd"
stopifnot(file.exists(qmd))
proj_root <- normalizePath(".")
lines <- readLines(qmd, encoding = "UTF-8")

# Parse chunks: header line index, label, body.
hdr <- grep("^```\\{r", lines)
chunks <- lapply(hdr, function(h) {
  end <- h + which(grepl("^```\\s*$", lines[(h + 1):length(lines)]))[1]
  opts <- sub("^```\\{r\\s*", "", sub("\\}\\s*$", "", lines[h]))
  label <- trimws(strsplit(opts, ",")[[1]][1])
  if (is.na(label) || grepl("=", label)) label <- ""
  list(line = h, label = label, opts = opts, body = lines[(h + 1):(end - 1)])
})
labels <- vapply(chunks, `[[`, "", "label")
line_of <- function(lbl) chunks[[match(lbl, labels)]]$line

# Chunks required by si-pdf-fit-tables (named) plus the unnamed data-preparation
# chunks between them (selected by position relative to named anchors).
needed_named <- c(
  "setup", "polymer-density-table", "river-sample-manifest",
  "cpsd-fits-by-shape", "cpsd-fits-by-location", "cpsd-location-heterogeneity",
  "cpsd-fits-surface-area", "volume-lod-bounds", "cpsd-fits-volume", "cpsd-per-river-fits",
  "alpha-distribution", "aspect-ratio-distribution", "koelmans-convention-check", "rescaling-cf",
  "cpsd-fits-sed-length", "cpsd-fits-sed-area", "sed-volume-lod-bounds", "cpsd-fits-sed-volume",
  "spec1d-sediment-provenance-audit", "spec1d-sediment-quantitative-flag",
  "ocean-volume", "cpsd-fits-ocean-length", "cpsd-fits-ocean-by-location",
  "cpsd-ocean-location-heterogeneity", "ocean-length-fits-continue", "cpsd-fits-ocean-area",
  "ocean-volume-lod-bounds", "cpsd-fits-ocean-volume", "ocean-monitoring",
  "hoffman-critical-size-piecewise", "m13-model-fits", "si-pdf-fit-tables")
# Unnamed chunks needed: everything unnamed before cpsd-fits-by-shape (packages,
# particle loading/geometry, monitoring), plus the unnamed sediment and ocean
# preparation chunks (they hold the matrix monitoring/rescaling steps).
unnamed_idx <- which(labels == "")
in_range <- function(i, from, to) chunks[[i]]$line > line_of(from) & chunks[[i]]$line < line_of(to)
keep_unnamed <- unnamed_idx[vapply(unnamed_idx, function(i)
  chunks[[i]]$line < line_of("cpsd-fits-by-shape") ||
  in_range(i, "cpsd-fits-surface-area", "cpsd-per-river-fits") ||
  in_range(i, "lmeas-inline", "sed-eed-bootstrap-plot") ||
  in_range(i, "cpsd-fits-ocean-by-location", "cpsd-river-ocean-faceted"), logical(1))]
run_idx <- sort(c(match(needed_named, labels), keep_unnamed))
stopifnot(!anyNA(run_idx))

cache_dir <- file.path("R", "probabilistic_risk_characterization_cache", "html")
try_cache <- function(lbl) {
  f <- list.files(cache_dir, paste0("^", gsub("([.|()^{}+$*?\\[\\]\\\\-])", "\\\\\\1", lbl), "_[0-9a-f]{32}[.]rdb$"), full.names = TRUE)
  if (!length(f)) return(FALSE)
  lazyLoad(sub("[.]rdb$", "", f[which.max(file.mtime(f))]), envir = globalenv())
  TRUE
}

# Depth-stratified length fits (the fit part of hoffman-depth-stratified-sensitivity,
# without its pSSD++/risk part), injected before the SI chunk.
depth_chunk <- chunks[[match("hoffman-depth-stratified-sensitivity", labels)]]$body
depth_fit_code <- depth_chunk[seq_len(grep("^depth_slope_plot <-", depth_chunk)[1] - 1)]

t0 <- Sys.time()
for (i in run_idx) {
  ch <- chunks[[i]]
  name <- if (nzchar(ch$label)) ch$label else paste0("unnamed@", ch$line)
  if (identical(ch$label, "si-pdf-fit-tables")) {
    cat(">> injected: depth_shape_fits\n")
    eval(parse(text = depth_fit_code), envir = globalenv())
  }
  if (grepl("cache\\s*=\\s*TRUE", ch$opts) && try_cache(ch$label)) {
    cat(sprintf(">> %-42s (from cache)\n", name)); next
  }
  cat(sprintf(">> %-42s ", name))
  t1 <- Sys.time()
  withr::with_options(list(warn = 1), eval(parse(text = ch$body), envir = globalenv()))
  cat(sprintf("%.1fs\n", as.numeric(difftime(Sys.time(), t1, units = "secs"))))
}
cat("Done in", format(Sys.time() - t0), "->", si_docx_path, "\n")
