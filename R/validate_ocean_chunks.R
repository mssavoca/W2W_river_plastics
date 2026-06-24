# Chunk-by-chunk validation for the ocean water section (Section 12)
# Run from R_new/W2W_river_plastics/R directory.
# Each block corresponds to a chunk label in the Rmd.

# Rmd uses knitr root.dir=".." so all paths are relative to project root (W2W_river_plastics/)
setwd("C:/Users/Scott.Coffin/OneDrive - California OEHHA/R_new/W2W_river_plastics")

# ── Packages ────────────────────────────────────────────────────────────────
library(tidyverse)
library(truncnorm)
library(PSSDplusplus)
library(parallel)
library(rlang)

# ── Helper functions ─────────────────────────────────────────────────────────
source("R/mp_risk_utils.R")
source("R/cpsd_plotting.R")

# ── Global params ─────────────────────────────────────────────────────────────
n_boot <- 1000
n_mc   <- 20000

# ── Load and prepare raw ocean particles ─────────────────────────────────────
cat("\n== Loading raw particles ==\n")
raw_particles <- readRDS("data_input/Part_dets_comb.rds") |>
  dplyr::filter(
    !material_class %in% c("mineral", "organic matter"),
    acc_analy_conf == "confident",
    bad_spectra
  ) |>
  mutate(
    date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"),
    date     = ymd(date_raw),
    shape    = case_when(aspect_ratio >= 3 ~ "fiber", TRUE ~ "fragment"),
    length_um = max_length_um,
    width_um  = min_length_um
  )

raw_particles_ocean <- raw_particles |> filter(sample_type == "ocean water")
cat("Ocean particles: n =", nrow(raw_particles_ocean), "\n")
cat("Shapes:", paste(sort(unique(raw_particles_ocean$shape)), collapse=", "), "\n")

# ── ocean-volume ──────────────────────────────────────────────────────────────
cat("\n== ocean-volume chunk ==\n")
ratio_tbl_ocean <- raw_particles_ocean |>
  dplyr::mutate(WL = 1 / aspect_ratio) |>
  dplyr::group_by(shape) |>
  dplyr::summarise(n=dplyr::n(), r_med=median(WL,na.rm=TRUE), .groups="drop")

raw_particles_ocean <- raw_particles_ocean |>
  dplyr::left_join(ratio_tbl_ocean |> dplyr::select(shape, r_med), by="shape") |>
  dplyr::mutate(
    height_um = r_med * width_um,
    V_um3 = dplyr::case_when(
      shape == "fragment" ~ (pi/6) * length_um * width_um * height_um,
      shape == "fiber"    ~ pi * (width_um/2)^2 * length_um,
      TRUE ~ NA_real_
    )
  )
cat("Ocean volume computed. NAs in V_um3:", sum(is.na(raw_particles_ocean$V_um3)), "\n")

# ── cpsd-fits-ocean-length ────────────────────────────────────────────────────
cat("\n== cpsd-fits-ocean-length chunk ==\n")
cpsd_fit_ocean_frag  <- c(fit_cpsd_segur_r(raw_particles_ocean |> dplyr::filter(shape=="fragment") |> dplyr::pull(length_um), bin_um=5), list(shape="fragment"))
cpsd_fit_ocean_fiber <- c(fit_cpsd_segur_r(raw_particles_ocean |> dplyr::filter(shape=="fiber")    |> dplyr::pull(length_um), bin_um=5), list(shape="fiber"))
cpsd_fit_ocean_all   <- c(fit_cpsd_segur_r(raw_particles_ocean |>                                       dplyr::pull(length_um), bin_um=5), list(shape="all"))

alpha_ocean    <- alpha_dist(mu=cpsd_fit_ocean_all$a_cpsd, sd=cpsd_fit_ocean_all$se_a_cpsd, n=n_mc)
alpha_ocean_mu <- mean(alpha_ocean)
alpha_ocean_se <- sd(alpha_ocean)
cat("Ocean All length: a_cpsd =", round(cpsd_fit_ocean_all$a_cpsd,3), "LOD =",
    round(cpsd_fit_ocean_all$lower_lod_used_um), "-", round(cpsd_fit_ocean_all$upper_lod_um), "µm\n")

# ── cpsd-fits-ocean-area ──────────────────────────────────────────────────────
cat("\n== cpsd-fits-ocean-area chunk ==\n")
cpsd_fit_area_ocean_frag  <- c(fit_cpsd_segur_r(raw_particles_ocean |> dplyr::filter(shape=="fragment") |> dplyr::pull(area_um2), bin_um=500, fit_range_um=c(2000,50000)),  list(shape="fragment"))
cpsd_fit_area_ocean_fiber <- c(fit_cpsd_segur_r(raw_particles_ocean |> dplyr::filter(shape=="fiber")    |> dplyr::pull(area_um2), bin_um=500, fit_range_um=c(2000,500000)), list(shape="fiber"))
cpsd_fit_area_ocean       <- c(fit_cpsd_segur_r(raw_particles_ocean |>                                       dplyr::pull(area_um2), bin_um=500, fit_range_um=c(2000,500000)), list(shape="all"))
cat("Ocean All area: a_cpsd =", round(cpsd_fit_area_ocean$a_cpsd,3), "\n")

# ── cpsd-fits-ocean-volume ────────────────────────────────────────────────────
cat("\n== cpsd-fits-ocean-volume chunk ==\n")
cpsd_fit_volume_ocean_frag  <- c(fit_cpsd_segur_r(raw_particles_ocean |> dplyr::filter(shape=="fragment") |> dplyr::pull(V_um3), bin_um=1e4), list(shape="fragment"))
cpsd_fit_volume_ocean_fiber <- c(fit_cpsd_segur_r(raw_particles_ocean |> dplyr::filter(shape=="fiber")    |> dplyr::pull(V_um3), bin_um=1e4), list(shape="fiber"))
cpsd_fit_volume_ocean       <- c(fit_cpsd_segur_r(raw_particles_ocean |>                                       dplyr::pull(V_um3), bin_um=1e4), list(shape="all"))
cat("Ocean All volume: a_cpsd =", round(cpsd_fit_volume_ocean$a_cpsd,3), "\n")

# ── ocean-monitoring ──────────────────────────────────────────────────────────
cat("\n== ocean-monitoring chunk ==\n")
monitoring_ocean <- readRDS("data_input/Part_dets_summ.rds") |>
  dplyr::filter(sample_type=="ocean water", material_simple=="plastic", sample_or_blank=="sample") |>
  dplyr::mutate(
    date_raw      = stringr::str_extract(Client_ID_MSSupdate, "\\d{8}"),
    date          = lubridate::ymd(date_raw),
    sample_id     = Client_ID_MSSupdate,
    C_measured_pL = extrap_conc_PPL,
    Lmin_measured_um = 50,
    Lmax_measured_um = 500
  )
cat("Ocean monitoring samples:", nrow(monitoring_ocean), "\n")
cat("Valid C_measured_pL:", sum(is.finite(monitoring_ocean$C_measured_pL) & monitoring_ocean$C_measured_pL > 0), "\n")

monitoring_ocean <- monitoring_ocean |>
  dplyr::mutate(
    Lmin_biascorr_um = pmax(Lmin_measured_um, cpsd_fit_ocean_all$lower_lod_um, na.rm=TRUE),
    Lmax_biascorr_um = pmin(Lmax_measured_um, cpsd_fit_ocean_all$upper_lod_um,  na.rm=TRUE)
  )

L_meas_min_ocean <- median(monitoring_ocean$Lmin_biascorr_um, na.rm=TRUE)
L_meas_max_ocean <- median(monitoring_ocean$Lmax_biascorr_um, na.rm=TRUE)
cat("LOD window for ocean CF:", L_meas_min_ocean, "-", L_meas_max_ocean, "µm\n")

cf_rescale_ocean  <- correction_factor(a=alpha_ocean, L_meas_min=L_meas_min_ocean,
                                       L_meas_max=L_meas_max_ocean, L_tar_min=1, L_tar_max=5000)
combined_cf_ocean <- cf_rescale_ocean
cat("Ocean CF median:", round(median(combined_cf_ocean),1), "x\n")

C_corrected_draws_ocean <- monitoring_ocean |>
  dplyr::mutate(idx=dplyr::row_number()) |>
  tidyr::crossing(draw=1:3000) |>
  dplyr::mutate(cf=sample(combined_cf_ocean, size=dplyr::n(), replace=TRUE),
                C_corrected_pL=C_measured_pL*cf)
C_sample_median_ocean <- C_corrected_draws_ocean |>
  dplyr::group_by(sample_id) |>
  dplyr::summarise(C_corr_med=median(C_corrected_pL), .groups="drop")
eed_boot_ocean <- bootstrap_eed(C_sample_median_ocean$C_corr_med, n_boot=n_boot, probs=c(0.5,0.95))
cat("EED q50:", round(eed_boot_ocean$q50, 2), "\n")

# ── ocean-tox-data ────────────────────────────────────────────────────────────
cat("\n== ocean-tox-data chunk ==\n")
tomex2 <- PSSDplusplus::tomex2
tox_data_ocean <- tomex2 |>
  dplyr::filter(env_f=="Marine", Group!="Bacterium", Group!="Plant",
                effect.metric!="HONEC", tier_zero_tech_f=="Red Criteria Passed",
                tier_zero_risk_f=="Red Criteria Passed", risk.13!=0)
cat("Marine tox endpoints:", nrow(tox_data_ocean), "records,",
    length(unique(tox_data_ocean$Group)), "taxonomic groups\n")

# ── ocean-aspect-ratio (includes param_values_ocean) ─────────────────────────
cat("\n== ocean-aspect-ratio chunk ==\n")
R.ave_ocean_result <- bootstrap_aspect_ratio(raw_particles_ocean$aspect_ratio, n_boot=n_boot)
R.ave_ocean_vals   <- R.ave_ocean_result$vals
R.ave_ocean_mean   <- R.ave_ocean_result$boot_mean
R.ave_ocean_sd     <- R.ave_ocean_result$boot_sd

param_values_ocean <- PSSDplusplus::param_default_values
param_values_ocean[["alpha.marine"]]          <- -alpha_ocean_mu
param_values_ocean[["alpha.marine.sd"]]       <- alpha_ocean_se
param_values_ocean[["R.ave.water.marine"]]    <- R.ave_ocean_mean
param_values_ocean[["R.ave.water.marine.sd"]] <- R.ave_ocean_sd
param_values_ocean[["a.sa.marine"]]           <- -cpsd_fit_area_ocean$a_cpsd
param_values_ocean[["a.sa.marine.sd"]]        <- cpsd_fit_area_ocean$se_a_cpsd
param_values_ocean[["a.v.marine"]]            <- -cpsd_fit_volume_ocean$a_cpsd
param_values_ocean[["a.v.marine.sd"]]         <- cpsd_fit_volume_ocean$se_a_cpsd
param_values_ocean[["a.m.marine"]]            <- -cpsd_fit_volume_ocean$a_cpsd
param_values_ocean[["a.m.marine.sd"]]         <- cpsd_fit_volume_ocean$se_a_cpsd

# Keep all defaults — only override derived values (same pattern as river section)
cat("param_values_ocean: a.m.marine =", param_values_ocean$a.m.marine,
    "| a.ssa.marine =", param_values_ocean$a.ssa.marine, "\n")

# ── ocean-param-matrix ────────────────────────────────────────────────────────
cat("\n== ocean-param-matrix chunk ==\n")
param_matrix_ocean <- matrix_function(
  n      = n_boot,
  params = param_values_ocean,
  upper.tissue.truncation.limit = 500,
  x1M_set = 1,
  x2D_set = 5000,
  include_marine_surface_water     = TRUE,
  include_freshwater_surface_water = FALSE,
  include_marine_sediment          = FALSE,
  include_freshwater_sediment      = FALSE
)
cat("param_matrix_ocean: nrow =", nrow(param_matrix_ocean), "ncol =", ncol(param_matrix_ocean), "\n")

ocean_R_col  <- rlang::sym("R.ave.water.marine")
ocean_HW_col <- rlang::sym("H_W_ratio.water.marine")
param_matrix_ocean <- param_matrix_ocean |>
  dplyr::mutate(
    !!ocean_R_col  := sample(R.ave_ocean_vals, size=dplyr::n(), replace=TRUE),
    !!ocean_HW_col := sample(R.ave_ocean_vals, size=dplyr::n(), replace=TRUE)
  )
cat("R.ave.water.marine overwritten. Summary:", round(summary(param_matrix_ocean$R.ave.water.marine), 3), "\n")
cat("SUCCESS: ocean-param-matrix\n")

# ── ocean-mc-sim ──────────────────────────────────────────────────────────────
cat("\n== ocean-mc-sim chunk (MC_sim_align_parallel) ==\n")
cat("This may take several minutes...\n")
MC_sim_df_ocean <- MC_sim_align_parallel(
  tox_data     = tox_data_ocean,
  param_matrix = param_matrix_ocean,
  n_sim        = n_boot,
  x1D_set      = 1,
  x2D_set      = 5000,
  num_cores    = parallel::detectCores() - 2
)
cat("MC_sim_df_ocean: nrow =", nrow(MC_sim_df_ocean), "\n")

results_df_food_ocean <- dplyr::filter(MC_sim_df_ocean,
  ingestible != "not ingestible", particles_L_food_dilution > 0, Group != "Algae") |>
  dplyr::mutate(dose_new_particles_L = particles_L_food_dilution) |>
  tidyr::drop_na(particles_L_food_dilution)

results_df_food_ocean_t3_t4 <- dplyr::filter(results_df_food_ocean,
  risk.13 != 1, bio_f %in% c("Organism","Population"))

results_df_tissue_ocean <- dplyr::filter(MC_sim_df_ocean,
  translocatable != "not translocatable", particles_L_ox_stress > 0) |>
  dplyr::mutate(dose_new_particles_L = particles_L_ox_stress) |>
  tidyr::drop_na(particles_L_ox_stress)

results_df_tissue_ocean_t3_t4 <- dplyr::filter(results_df_tissue_ocean,
  risk.13 != 1, bio_f %in% c("Organism","Population"))

erm_registry_ocean <- list(
  "Food Dilution"        = list(base=results_df_food_ocean,   t3_t4=results_df_food_ocean_t3_t4),
  "Tissue Translocation" = list(base=results_df_tissue_ocean, t3_t4=results_df_tissue_ocean_t3_t4)
)
cat("food n =", nrow(results_df_food_ocean), "| tissue n =", nrow(results_df_tissue_ocean), "\n")
cat("SUCCESS: ocean-mc-sim\n")

# ── ocean-pssds ───────────────────────────────────────────────────────────────
cat("\n== ocean-pssds chunk (make_all_pSSDs) ==\n")
cat("This may take several minutes...\n")
pSSDs_ocean <- make_all_pSSDs(
  MC_sim_df    = MC_sim_df_ocean,
  environments = c("Marine"),
  erm_registry = erm_registry_ocean,
  sim          = 30,
  cv_uf        = 0.5,
  rmore_method = "lognormal",
  parallel     = TRUE,
  workers      = parallel::detectCores() - 2,
  base_cache_dir   = file.path(tempdir(), "pssd_cache_ocean_test"),
  base_output_path = file.path(tempdir(), "pssd_figures_ocean_test"),
  overwrite_cache  = TRUE
)
cat("pSSDs_ocean keys:", paste(names(pSSDs_ocean), collapse=", "), "\n")
cat("SUCCESS: ocean-pssds\n")

# ── Hazard extraction ─────────────────────────────────────────────────────────
cat("\n== hazard extraction chunk ==\n")
food_key   <- "Tier3_Marine_Food Dilution"
tissue_key <- "Tier3_Marine_Tissue Translocation"

if (!food_key %in% names(pSSDs_ocean)) {
  cat("WARNING: expected key '", food_key, "' not found.\n", sep="")
  cat("Available keys:", paste(names(pSSDs_ocean), collapse=", "), "\n")
} else {
  haz_ocean_HC5_food    <- pSSDs_ocean[[food_key]]$summary_05$df   |> dplyr::mutate(HCx=5,  ERM="Food Dilution")
  haz_ocean_HC10_food   <- pSSDs_ocean[[food_key]]$summary_10$df   |> dplyr::mutate(HCx=10, ERM="Food Dilution")
  haz_ocean_HC5_tissue  <- pSSDs_ocean[[tissue_key]]$summary_05$df |> dplyr::mutate(HCx=5,  ERM="Tissue Translocation")
  haz_ocean_HC10_tissue <- pSSDs_ocean[[tissue_key]]$summary_10$df |> dplyr::mutate(HCx=10, ERM="Tissue Translocation")
  haz_ocean <- dplyr::bind_rows(haz_ocean_HC5_food, haz_ocean_HC10_food, haz_ocean_HC5_tissue, haz_ocean_HC10_tissue)
  cat("haz_ocean nrow:", nrow(haz_ocean), "\n")

  # ── mc2d_ocean ──────────────────────────────────────────────────────────────
  cat("\n== mc2d_ocean chunk ==\n")
  mc2d_ocean <- mc2d_risk(
    monitoring_df = monitoring_ocean,
    combined_cf   = combined_cf_ocean,
    haz_df        = haz_ocean,
    n_uncertainty = 50,   # smaller for validation
    n_variability = 200,
    seed          = 1
  )
  cat("mc2d_ocean nrow:", nrow(mc2d_ocean), "\n")
  cat("Columns:", paste(names(mc2d_ocean), collapse=", "), "\n")

  mc2d_ocean_summary <- mc2d_ocean |>
    dplyr::group_by(ERM, HCx) |>
    dplyr::summarise(P_exceed_med=median(P_exceed), RQ_p50_med=median(RQ_p50), .groups="drop")
  print(mc2d_ocean_summary)
  cat("SUCCESS: full ocean section validated\n")
}
