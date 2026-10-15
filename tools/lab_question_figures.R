# Figures for docs/lab_questions/lab_data_questions.md (questions for the
# analytical lab). Reads only data_input/ and writes docs/lab_questions/figs/.
# Run from the project root:  Rscript tools/lab_question_figures.R

suppressMessages({ library(dplyr); library(ggplot2) })
source("R/mp_risk_utils.R")
out <- "docs/lab_questions/figs"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
theme_set(theme_minimal(base_size = 13) +
            theme(panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.4),
                  plot.title.position = "plot"))
save <- function(p, f, w = 9, h = 5.5) ggsave(file.path(out, f), p, width = w, height = h, dpi = 200)

summ <- readRDS("data_input/Part_dets_summ_final.rds") |>
  mutate(sid = recode(Client_ID_MSSupdate, "CR020250611S" = "CR020250611SS", "SRR20240205SS_2" = "SRR20240205SS"))
parts <- readRDS("data_input/Part_dets_comb.rds") |>
  mutate(sid = recode(sub("_[0-9]+$", "", Client_ID_MSSupdate), "SRR20240205SS_2" = "SRR20240205SS"))
plastic <- parts |> filter(!material_class %in% c("mineral", "organic matter"), !is.na(material_class))
qc_new <- spectra_qc_pass(plastic)
qc_old <- plastic$acc_analy_conf %in% "confident" & plastic$bad_spectra %in% TRUE
matrices <- c("river water" = "River water", "ocean water" = "Ocean water", "beach sand" = "Beach sand")
summ_tot <- summ |>
  filter(material_simple == "plastic", sample_or_blank == "sample", sample_type %in% names(matrices)) |>
  group_by(sid, sample_type) |>
  summarise(summary_count = sum(count), conc_m3 = sum(extrap_conc_PPL) * 1000, .groups = "drop")

# 1. bad_spectra is a threshold on the library-match correlation
f1 <- parts |> filter(!is.na(bad_spectra)) |>
  mutate(flag = ifelse(bad_spectra, "bad_spectra = TRUE", "bad_spectra = FALSE"))
p1 <- ggplot(f1, aes(max_cor_val, fill = flag)) +
  geom_histogram(binwidth = 0.01, boundary = 0, color = NA) +
  geom_vline(xintercept = 0.66, linetype = "dashed") + geom_vline(xintercept = 0.60, linetype = "dotted") +
  annotate("text", x = 0.665, y = Inf, label = "0.66: bad_spectra cut", hjust = 0, vjust = 1.5, size = 3.6) +
  annotate("text", x = 0.595, y = Inf, label = "0.60: 'confident' cut", hjust = 1, vjust = 1.5, size = 3.6) +
  scale_fill_manual(values = c("bad_spectra = FALSE" = "#1f6f5c", "bad_spectra = TRUE" = "#c0622b"), name = NULL) +
  labs(x = "Library-match correlation (max_cor_val)", y = "Particles (all rows)",
       title = "bad_spectra is TRUE exactly when the match correlation is 0.66 or lower",
       subtitle = "Particles between 0.60 and 0.66 are rated 'confident' yet flagged bad_spectra = TRUE") +
  theme(legend.position = "top")
save(p1, "fig1_bad_spectra_threshold.png")

# 2. Particle rows vs summary counts, per sample
rows <- plastic |> filter(sample_or_blank == "sample") |>
  mutate(new = qc_new[plastic$sample_or_blank == "sample"], old = qc_old[plastic$sample_or_blank == "sample"]) |>
  group_by(sid) |> summarise(`Corrected filter (bad_spectra = FALSE)` = sum(new), `Old filter (bad_spectra = TRUE)` = sum(old), .groups = "drop")
f2 <- summ_tot |> inner_join(rows, by = "sid") |>
  tidyr::pivot_longer(c(`Corrected filter (bad_spectra = FALSE)`, `Old filter (bad_spectra = TRUE)`), names_to = "filter", values_to = "rows") |>
  filter(rows > 0) |> mutate(Matrix = matrices[sample_type])
p2 <- ggplot(f2, aes(summary_count, rows, color = Matrix)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(alpha = 0.75, size = 2) +
  facet_wrap(~ filter) +
  scale_x_log10() + scale_y_log10() +
  scale_color_manual(values = c("River water" = "#0072B2", "Ocean water" = "#009E73", "Beach sand" = "#E69F00")) +
  labs(x = "Summary count per sample (Part_dets_summ_final)", y = "QC-passing particle rows per sample",
       title = "Particle-level rows do not reproduce the summary counts under either filter",
       subtitle = "Dashed line = 1:1. Each point is one sample.") +
  theme(legend.position = "top")
save(p2, "fig2_rows_vs_summary.png", w = 10, h = 5.5)

# 3. One sample, polymer by polymer
ex <- "CRR20231207SD"
f3 <- full_join(
  summ |> filter(sid == ex, material_simple == "plastic") |> group_by(material_class) |> summarise(`Summary count` = sum(count)),
  plastic |> filter(sid == ex) |> mutate(q = spectra_qc_pass(pick(everything()))) |> group_by(material_class) |>
    summarise(`QC-passing particle rows` = sum(q), `All plastic rows` = n()),
  by = "material_class") |>
  tidyr::pivot_longer(-material_class, names_to = "source", values_to = "n") |>
  mutate(n = coalesce(n, 0), material_class = stringr::str_trunc(material_class, 38))
p3 <- ggplot(f3, aes(reorder(material_class, n), n, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  coord_flip() + scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), breaks = c(0, 1, 3, 10, 30, 100, 300)) +
  scale_fill_manual(values = c("Summary count" = "#c0622b", "QC-passing particle rows" = "#1f6f5c", "All plastic rows" = "#9db8ae"), name = NULL) +
  labs(x = NULL, y = "Particles (log scale)", title = paste0("Sample ", ex, ": summary vs particle table, by polymer"),
       subtitle = "The summary lists 3 particles in 3 classes; the particle table has hundreds of polyethylene and polystyrene rows") +
  theme(legend.position = "top")
save(p3, "fig3_example_sample_by_polymer.png", w = 10, h = 6)

# 4. Which samples have particle-level data (ocean and sand)
f4 <- summ_tot |> filter(sample_type %in% c("ocean water", "beach sand")) |>
  mutate(Matrix = matrices[sample_type], year = paste0("20", substr(gsub("\\D", "", sid), 3, 4)),
         `Particle-level data` = ifelse(sid %in% parts$sid, "present", "missing"))
p4 <- ggplot(f4, aes(year, conc_m3, color = `Particle-level data`)) +
  geom_jitter(width = 0.15, height = 0, size = 2.4, alpha = 0.85) +
  facet_wrap(~ Matrix, scales = "free_y") + scale_y_log10(labels = scales::label_comma()) +
  scale_color_manual(values = c(present = "#1f6f5c", missing = "#c0622b")) +
  labs(x = "Sampling year", y = "Reported concentration x 1000
(water: per m3; sand: unit unconfirmed, see Q9)",
       title = "Samples without particle-level records are the 2025 samples",
       subtitle = "For ocean water these are also the low-concentration samples, so analyses needing particle data see only the high ones") +
  theme(legend.position = "top")
save(p4, "fig4_missing_particle_data.png", w = 10, h = 5)

# 5. Ocean 1-5000 um decomposition (fitted slope, point estimate)
oc <- plastic |> filter(sample_type == "ocean water", sample_or_blank == "sample")
oc <- oc[spectra_qc_pass(oc), ]
fit <- fit_cpsd_segur_r(oc$max_length_um, bin_um = 5)
a <- fit$a_psd
cf <- function(lo, hi) correction_factor(a, lo, hi, 1, 5000)
oc_summ <- summ |> filter(sample_type == "ocean water", material_simple == "plastic", sample_or_blank == "sample") |>
  group_by(sid) |> summarise(count = sum(count), C = sum(extrap_conc_PPL) * 1000, .groups = "drop")
oc_rows <- oc |> group_by(sid) |> summarise(n50 = sum(max_length_um >= 50 & max_length_um <= 500),
                                             nlod = sum(max_length_um >= fit$lower_lod_um & max_length_um < fit$upper_lod_um))
o6 <- inner_join(oc_summ, oc_rows, by = "sid")
steps <- tibble::tibble(
  step = factor(c("A. All 15 samples,\nreported 50-500 um,\nsieve-anchored", "B. Only the 6 samples\nwith particle data",
                  "C. Rebuilt from\nQC-passing rows", "D. Anchored on the\nLOD window\n(production)"),
                levels = c("A. All 15 samples,\nreported 50-500 um,\nsieve-anchored", "B. Only the 6 samples\nwith particle data",
                           "C. Rebuilt from\nQC-passing rows", "D. Anchored on the\nLOD window\n(production)")),
  value = c(median(oc_summ$C) * cf(50, 500), median(o6$C) * cf(50, 500),
            median(o6$n50 * o6$C / o6$count) * cf(50, 500),
            median(o6$nlod * o6$C / o6$count) * cf(fit$lower_lod_um, fit$upper_lod_um)))
mpsb <- read.csv("data_output/mpsizebase_ocean_sample_windows_v1.csv")
p5 <- ggplot(steps, aes(step, value)) +
  geom_col(fill = "#1f6f5c", width = 0.6) +
  geom_text(aes(label = format(signif(value, 2), big.mark = ",", scientific = FALSE)), vjust = -0.4, size = 3.8) +
  geom_hline(yintercept = median(mpsb$fit_1_5000, na.rm = TRUE), linetype = "dashed", color = "#c0622b") +
  annotate("text", x = 0.55, y = median(mpsb$fit_1_5000, na.rm = TRUE), label = "MPsizeBase ocean sample median",
           hjust = 0, vjust = -0.5, color = "#c0622b", size = 3.6) +
  scale_y_log10(labels = scales::label_comma(), expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Ocean 1-5000 um concentration (MP per m3, log)",
       title = "How the ocean estimate climbs from comparable-to-literature to ~40x higher",
       subtitle = sprintf("Median over samples; fitted ocean slope a = %.2f; LOD window %.0f-%.0f um", a, fit$lower_lod_um, fit$upper_lod_um))
save(p5, "fig5_ocean_decomposition.png", w = 10, h = 5.5)

# 6. Blank particle counts vs sample counts
bl <- plastic |> filter(sample_type %in% c("field blank", "lab blank", "river water", "ocean water")) |>
  mutate(q = spectra_qc_pass(pick(everything()))) |> filter(q) |>
  mutate(group = ifelse(sample_or_blank == "blank", sample_type, paste(sample_type, "sample"))) |>
  count(group, sid)
p6 <- ggplot(bl, aes(group, n)) +
  geom_boxplot(outlier.shape = NA, width = 0.55) + geom_jitter(width = 0.15, height = 0, alpha = 0.6, color = "#1f6f5c") +
  scale_y_log10() +
  labs(x = NULL, y = "QC-passing plastic rows (log)",
       title = "Blanks carry particles on the same order as low-concentration samples",
       subtitle = "Particle-level rows are not blank-corrected; the summary counts appear to be")
save(p6, "fig6_blanks_vs_samples.png", w = 9, h = 5)

# 7. Sieve size class vs measured length
f7 <- plastic[qc_new, ] |> filter(sample_or_blank == "sample") |>
  mutate(size_class = factor(case_when(grepl("^<212", size_class, useBytes = TRUE) ~ "<212", grepl("^212", size_class, useBytes = TRUE) ~ "212-500", grepl("^>500", size_class, useBytes = TRUE) ~ ">500"), levels = c("<212", "212-500", ">500")))
p7 <- ggplot(f7, aes(size_class, max_length_um)) +
  geom_boxplot(outlier.alpha = 0.3) + scale_y_log10() +
  geom_hline(yintercept = c(50, 500), linetype = "dashed", color = "#c0622b") +
  labs(x = "Sieve size fraction (size_class)", y = "Measured particle length, max_length_um (log)",
       title = "Sieve fraction is not measured length; the >500 um fraction is in the µFTIR table",
       subtitle = "Dashed lines = reported 50-500 um range")
save(p7, "fig7_size_class_vs_length.png", w = 8, h = 5)

# 8. River 1-5000 um decomposition by river, with the RIVM Dutch-river benchmark
rv <- plastic |> filter(sample_type == "river water", sample_or_blank == "sample")
rv <- rv[spectra_qc_pass(rv), ] |> filter(sid %in% summ$sid)
rfits <- lapply(split(rv$max_length_um, rv$sample_location), fit_cpsd_segur_r, bin_um = 5)
rmon <- summ |> filter(sample_type == "river water", material_simple == "plastic", sample_or_blank == "sample") |>
  group_by(sid) |> summarise(river = first(sample_location), count = sum(count), C = sum(extrap_conc_PPL) * 1000, .groups = "drop")
rv$in_lod <- mapply(function(L, r) L >= rfits[[r]]$lower_lod_um & L < rfits[[r]]$upper_lod_um, rv$max_length_um, rv$sample_location)
rrows <- rv |> group_by(sid) |> summarise(n50 = sum(max_length_um >= 50 & max_length_um <= 500), nlod = sum(in_lod), .groups = "drop")
rx <- rmon |> left_join(rrows, by = "sid") |>
  mutate(a = vapply(river, function(r) rfits[[r]]$a_psd, 1),
         cf_sieve = correction_factor(a, 50, 500, 1, 5000),
         cf_lod = mapply(function(a, r) correction_factor(a, rfits[[r]]$lower_lod_um, rfits[[r]]$upper_lod_um, 1, 5000), a, river),
         A = C * cf_sieve, Cr = n50 * C / count * cf_sieve, D = nlod * C / count * cf_lod)
step_levels <- c("A. Reported 50-500 um,\nsieve-anchored (= RIVM W1)", "B. Rebuilt from\nQC-passing rows", "C. Anchored on the\nLOD window (production)")
rsteps <- dplyr::bind_rows(lapply(c("All rivers", sort(unique(rx$river))), function(r) {
  d <- if (r == "All rivers") rx else rx[rx$river == r, ]
  tibble::tibble(river = r, step = step_levels,
                 value = c(median(d$A), median(d$Cr, na.rm = TRUE), median(d$D, na.rm = TRUE)))
})) |> mutate(river = factor(river, levels = c("All rivers", sort(unique(rx$river)))), step = factor(step, levels = step_levels))
tv <- read.csv("data_output/rivm_benchmark_text_values_v1.csv")
dutch <- tibble::tibble(label = c("Meuse (FTIR)", "Dommel (FTIR)"),
                        value = tv$value[match(c("dutch_Meuse_published_W1", "dutch_Dommel_published_W1"), tv$key)])
p8 <- ggplot(rsteps, aes(river, value, fill = step)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.8) +
  geom_hline(data = dutch, aes(yintercept = value, linetype = label), color = "#c0622b") +
  scale_y_log10(labels = scales::label_comma()) +
  scale_fill_manual(values = c("#9db8ae", "#4f9a85", "#1f6f5c"), name = NULL) +
  scale_linetype_manual(values = c("Meuse (FTIR)" = "dashed", "Dommel (FTIR)" = "dotted"), name = "Dutch rivers, RIVM W1 median") +
  labs(x = NULL, y = "River 1-5000 um concentration (MP per m3, log)",
       title = "River water: the same rebuild and anchor steps, plus steep slopes in Pajaro and Salinas",
       subtitle = sprintf("Median over samples. Fitted river slopes: %s",
                          paste(sprintf("%s %.2f", names(rfits), vapply(rfits, function(f) f$a_psd, 1)), collapse = "; "))) +
  theme(legend.position = "bottom", legend.box = "vertical")
save(p8, "fig8_river_decomposition.png", w = 11, h = 6.5)

# 9. Large-particle deficit: observed cumulative counts vs the fixed-window (100-500 um) MLE power law
tpl_mle <- function(x, lo, hi) {
  x <- x[x >= lo & x < hi]
  nll <- function(al) { k <- (1 - al) / (hi^(1 - al) - lo^(1 - al)); -sum(log(k) - al * log(x)) }
  stats::optimize(nll, c(1.01, 8))$minimum
}
f9 <- dplyr::bind_rows(lapply(names(rfits), function(r) {
  x <- rv$max_length_um[rv$sample_location == r]; f <- rfits[[r]]; lo <- f$lower_lod_um; hi <- f$upper_lod_um
  al <- tpl_mle(x, 100, 500); n_win <- sum(x >= lo & x < hi)
  # Expected N(>=L) for a power law matching the in-window count, extended to 5000 um
  k <- n_win / ((lo^(1 - al) - hi^(1 - al)) / (al - 1))
  L <- exp(seq(log(50), log(3000), length.out = 120))
  ratio <- sum(x >= hi) / (k * (hi^(1 - al) - 5000^(1 - al)) / (al - 1))
  lab <- sprintf("%s\nC-PSD a = %.2f; MLE (100-500 um) a = %.2f\nabove window: %.0f%% of expected", r, f$a_psd, -al, 100 * ratio)
  dplyr::bind_rows(
    tibble::tibble(river = lab, L = L, N = vapply(L, function(l) sum(x >= l), 1), what = "Observed N(>= L)", lo = lo, hi = hi),
    tibble::tibble(river = lab, L = L, N = sum(x >= hi) + 0 * L + (k * (pmax(L, lo)^(1 - al) - 5000^(1 - al)) / (al - 1)) - k * (hi^(1 - al) - 5000^(1 - al)) / (al - 1) +
                     k * (hi^(1 - al) - 5000^(1 - al)) / (al - 1) - sum(x >= hi), what = "Expected from 100-500 um MLE slope", lo = lo, hi = hi) |>
      dplyr::filter(L >= lo))
}))
p9 <- ggplot(f9, aes(L, N, color = what, linetype = what)) +
  geom_rect(data = dplyr::distinct(f9, river, lo, hi), aes(xmin = lo, xmax = hi, ymin = 0, ymax = Inf), inherit.aes = FALSE, fill = "#1f6f5c", alpha = 0.07) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 500, linetype = "dotted", color = "gray40") +
  facet_wrap(~ river, nrow = 1) +
  scale_x_log10() + scale_y_log10() +
  scale_color_manual(values = c("Observed N(>= L)" = "#1f6f5c", "Expected from 100-500 um MLE slope" = "#c0622b"), name = NULL) +
  scale_linetype_manual(values = c("Observed N(>= L)" = "solid", "Expected from 100-500 um MLE slope" = "dashed"), name = NULL) +
  labs(x = "Particle length L (um, log)", y = "Particles at or above L (log)",
       title = "Too few large particles: the cumulative curve falls below the power law above each fit window",
       subtitle = "Shaded = Segur LOD window; dotted line = 500 um sieve boundary. The shortfall bends the C-PSD fit steeper.") +
  theme(legend.position = "bottom", strip.text = element_text(size = 9))
save(p9, "fig9_large_particle_deficit.png", w = 13, h = 5.5)

cat("Wrote", length(list.files(out)), "figures to", out, "\n")
