knitr::opts_chunk$set(
  echo    = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.width  = 9,
  fig.height = 6
)

set.seed(1)

# During an actual render (quarto render / rmarkdown::render()), knitr's working
# directory for THIS chunk is the .qmd's own directory (R/), but that can vary by
# how the render is invoked. Resolve the project root from the file's own known
# location via knitr::current_input() rather than assuming a starting cwd, so this
# works the same whether rendered from the project root, from R/, or via Quarto's
# CLI. Falls back to the current directory when run interactively (chunk-by-chunk),
# where cwd is normally already the project root.
proj_root <- tryCatch({
  doc_path <- knitr::current_input(dir = TRUE)
  if (is.null(doc_path) || !nzchar(doc_path)) stop("not knitting")
  normalizePath(file.path(dirname(doc_path), ".."))
}, error = function(e) normalizePath("."))

knitr::opts_knit$set(root.dir = proj_root)
getwd()

# ── Global Monte Carlo settings (change here to scale up/down) ──
n_boot <- 100   # bootstrap replicates (increase to ≥ 1000 for publication)
n_mc   <- 20000 # alpha Monte Carlo draws

# Uniform C-PSD bin width for volume fits (µm³), applied identically across every
# matrix and shape (river/sediment/ocean; fragment/fiber/all) — the same convention
# already used for length (bin_um = 5 µm, fixed) and area (bin_um = 500 µm², fixed)
# fits below. Segur et al. (2026) demonstrate the C-PSD method's slope estimate
# should be ~bin-size-independent (RSD < 1.8%) *once the fit window is held fixed*;
# empirical testing here confirms that holds for volume too (RSD ~5-10% across a
# 10x range of bin_um) as long as the window itself isn't also re-optimized per bin
# size — see the volume-lod-bounds chunks below for why the window is fixed via
# length/width-consistent bounds rather than left to auto-detection for volume.
vol_bin_um <- 2e4

# ── Utility functions (sourced from companion scripts in R/) ──
# mp_risk_utils.R – C-PSD fitting, exposure/hazard/risk functions
# cpsd_plotting.R – plot_cpsd_multi(), MP_PALETTE
source(file.path(proj_root, "R", "mp_risk_utils.R"))
source(file.path(proj_root, "R", "cpsd_plotting.R"))
source(file.path(proj_root, "R", "risk_plotting.R"))    # matrix-aware risk plotting helpers

# ── Additional packages used in plots ──
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
if (!requireNamespace("scales",  quietly = TRUE)) install.packages("scales")
library(ggrepel)
library(scales)


pkgs <- c("tidyverse", "truncnorm", "fitdistrplus", "ggpubr", "sensitivity")
to_install <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(to_install) > 0) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

# pSSD++ package (GitHub)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
# install from github unless library(PSSDplusplus) is not already installed
if (!requireNamespace("PSSDplusplus", quietly = TRUE)) {
  devtools::install_github("ScottCoffin/ToMEx2.0_EcoToxRisk", upgrade = "never", subdir = "package", build_vignettes = FALSE)
}
library(PSSDplusplus)

# import river particle data (includes plastics and non-plastics)
raw_particles <- readRDS("data_input/Part_dets_comb.rds") |> 
  # filter to only QC-passing particles
  dplyr::filter(!material_class %in% c("mineral", "organic matter"), #filter non-plastic
                acc_analy_conf == "confident", # only use confident spectra
                bad_spectra) %>% #filter out bad spectra
  # extract the first 8-digit block from the ID
  mutate(date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"), date = ymd(date_raw), # convert YYYYMMDD → Date
         shape = case_when(aspect_ratio >= 3 ~ "fiber", # common definition of fiber - check!
                           aspect_ratio <= 3 ~ "fragment"),
         length_um = max_length_um,
         width_um = min_length_um
  )

# subset by matrix for downstream analysis
raw_particles_river <- raw_particles |> 
  filter(sample_type == "river water") #river water, ocean water, or beach sand are options

raw_particles_ocean <- raw_particles |> 
  filter(sample_type == "ocean water")

raw_particles_river |> 
  group_by(shape) |> 
  summarize(count = n(),
            mean_length = mean(length_um, na.rm = TRUE),
            min_length = min(length_um, na.rm = TRUE),
            max_length = max(length_um, na.rm = TRUE),
            sd_length = sd(length_um, na.rm = TRUE),
            mean_width = mean(width_um, na.rm = TRUE),
            min_width = min(width_um, na.rm = TRUE),
            max_width = max(width_um, na.rm = TRUE)
            )

# ---- Thickness (height) factor from measured raw data ----
# Kooi et al. (2022) H = r * W, where r is median(W/L). 
ratio_tbl <- raw_particles_river |>
  mutate(
    WL = 1/aspect_ratio,
    # area-derived effective width (rectangle proxy); protects against noisy min_length
    width_eff_um = case_when(
      shape == "fragment" & !is.na(area_um2) & !is.na(length_um) & length_um > 0 &
        !is.na(circularity) & circularity < 0.7 ~ area_um2 / length_um,
      TRUE ~ width_um
    )) |> 
  group_by(shape) |>
  summarise(
    n = n(),
    r_med  = median(WL, na.rm = TRUE),            # central estimate
    r_low  = quantile(WL, 0.25, na.rm = TRUE),    # conservative low
    r_high = quantile(WL, 0.75, na.rm = TRUE),    # conservative high
    .groups = "drop"
  )

# quick sanity check: ratio table + summary of volumes
ratio_tbl

# ---- Volumes ----
# Ellipsoid (Kooi-compatible): V = (pi/6) * L * W * H  with H = r * W
# => V = (pi/6) * r * L * W^2
# Cylinder assumption for fibers: V = pi * (W/2)^2 * L

raw_particles_river <- raw_particles_river |>
  left_join(ratio_tbl |> dplyr::select(shape, r_med), by = "shape") |>  
  mutate(# thickness in um (assume L/W = H/W)
    height_um = r_med * width_um) |> 

    mutate(# ellipsoid volume in um^3 (primary; Kooi-consistent)
      V_um3  = case_when(shape == "fragment" ~ (pi/6) * length_um * width_um * height_um,
    # fiber cylinder sensitivity in um^3 (only meaningful for fibers)
                        shape == "fiber"    ~ pi * (width_um/2)^2 * length_um,
                        TRUE ~ NA_real_)
                        ) |> 
    # estimate particle mass based on volume and assumed density
    mutate(mass_ug = V_um3 * 1.1) # assume density of 1.1 g/cm^3 for plastics

# export CSV of cleaned up raw particles
write.csv(raw_particles_river, "data_output/Part_dets_cleaned.csv", row.names = FALSE)

raw_particles_river |>
  group_by(shape) |>
  summarise(
    n = n(),
    V_um3_median = median(V_um3, na.rm = TRUE),
    V_um3_mean = mean(V_um3, na.rm = TRUE),
    V_um3_sd = sd(V_um3, na.rm = TRUE),
    mass_ug_median = median(mass_ug, na.rm = TRUE),
    mass_ug_mean = mean(mass_ug, na.rm = TRUE),
    mass_ug_sd = sd(mass_ug, na.rm = TRUE),
    .groups = "drop"
  )

monitoring <- readRDS("data_input/Part_dets_summ.rds") |> 
  filter(sample_type == "river water",
         material_simple == "plastic",
         sample_or_blank == "sample"
  ) %>% 
  mutate(# extract the first 8-digit block from the ID
    date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"),  
    # convert YYYYMMDD → Date
    date = ymd(date_raw),    
    # correct the specific sample date
    date = case_when(
      Client_ID_MSSupdate == "SRR20250303SS" ~ ymd("20250303"),
      TRUE ~ date
    ),
    sample_dets = case_when(Client_ID_MSSupdate %in% c("CRR20231207SS", "CRR20231207SD") ~ "lagoon",
                            Client_ID_MSSupdate %in% c("SRR20231207SS", "SRR20231207SD") ~ "lagoon"),
    # recode depth to subsurface
    sample_depth_general = recode(sample_depth_general,
                                  "depth" = "subsurface"),
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("subsurface", "surface")
    ) 
  ) %>%
  rename(river = sample_location,
         C_measured_pL = extrap_conc_PPL) |> 
          # should adjust based on power law LLOD determined below
  mutate(Lmin_measured_um = 50, 
         Lmax_measured_um = 500,
         sample_id = Client_ID_MSSupdate)

head(monitoring)

# Pooled C-PSD fits across all river locations, bin_um = 5 µm
cpsd_fits_river_length <- fit_cpsd_by_shape(
  raw_particles_river, value_col = "length_um",
  config = list(
    fragment = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_)),
    fiber    = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_)),
    all      = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_))
  )
)
cpsd_fit_frag  <- cpsd_fits_river_length$fragment
cpsd_fit_fiber <- cpsd_fits_river_length$fiber
cpsd_fit_all   <- cpsd_fits_river_length$all

# Per-location fits (Carmel, Pajaro, Salinas, San Lorenzo) — all particles pooled by site
locations <- sort(unique(raw_particles_river$sample_location))

cpsd_fits_by_loc <- lapply(setNames(locations, locations), function(loc) {
  dat <- raw_particles_river |> dplyr::filter(sample_location == loc) |> dplyr::pull(length_um)
  c(fit_cpsd_segur_r(dat, bin_um = 5), list(shape = loc))
})

# Summary table: slope and LOD window by location
loc_summary <- dplyr::bind_rows(lapply(names(cpsd_fits_by_loc), function(loc) {
  fo <- cpsd_fits_by_loc[[loc]]
  tibble::tibble(
    location     = loc,
    n_particles  = sum(raw_particles_river$sample_location == loc),
    a_cpsd       = round(fo$a_cpsd,   3),
    se_a_cpsd    = round(fo$se_a_cpsd, 3),
    a_psd        = round(fo$a_psd,    3),
    lod_low_um   = round(fo$lower_lod_used_um, 0),
    lod_high_um  = round(fo$upper_lod_um,       0),
    r2           = round(fo$r2,       3),
    n_bins       = fo$n_bins,
    fit_mode     = fo$fit_mode
  )
}))

knitr::kable(loc_summary, caption = "C-PSD length fits by river location (Segur 2026 algorithm)")

cpsd_location_slope_plot <- loc_summary |>
  ggplot2::ggplot(ggplot2::aes(x = location, y = a_psd)) +
  ggplot2::geom_hline(yintercept = -2.66, linetype = "dashed", color = "gray50", linewidth = 0.7) +
  ggplot2::geom_point(size = 4, color = "#56B4E9") +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = a_psd - se_a_cpsd, ymax = a_psd + se_a_cpsd),
    width = 0.2, color = "#56B4E9", linewidth = 0.8
  ) +
  ggplot2::annotate("text", x = Inf, y = -2.66,
                    label = "Segur 2026 fragment mean (−2.66)",
                    size = 3.2, hjust = 1.05, vjust = -0.5, color = "gray40") +
  ggplot2::coord_cartesian(ylim = c(-4, -1)) +
  ggplot2::scale_y_continuous(
    breaks = seq(-4, -1, by = 0.5),
    labels = function(x) formatC(x, digits = 1, format = "f")
  ) +
  ggplot2::labs(
    x        = "River location",
    y        = expression(italic(a)[psd] ~ "=" ~ italic(a)[cpsd] - 1),
    title    = "Power-law slope by river location",
    subtitle = expression("Error bars = ±1 SE  |  Dashed = Segur 2026 reference")
  ) +
  ggplot2::theme_minimal(base_size = 20) +
  ggplot2::theme(panel.border = ggplot2::element_rect(color = "gray85", fill = NA, linewidth = 0.4))

ggsave(cpsd_location_slope_plot, filename = "figures/cpsd_location_slope_plot.png", width = 9, height = 6, dpi = 300)
cpsd_location_slope_plot

fits_by_shape <- list(fragment = cpsd_fit_frag, fiber = cpsd_fit_fiber, all = cpsd_fit_all)
fits_by_shape <- fits_by_shape[names(MP_PALETTE)[names(MP_PALETTE) %in% names(fits_by_shape)]]

alpha_cpsh_plot <- plot_cpsd_multi(
  fits_by_shape,
  title     = "C-PSD length fits by shape",
  attribute = "Length"
)

ggsave(alpha_cpsh_plot, filename = "figures/alpha_cpsh_plot.png", width =11.5, height = 7, dpi = 300)
alpha_cpsh_plot

# Kooi et al. (2021) length-slope (alpha) reference values, as embedded in
# PSSDplusplus::param_default_values (alpha.freshwater, alpha.marine) — the same
# source used elsewhere in this document for surface-area/volume/mass defaults.
kooi_alpha_freshwater <- PSSDplusplus::param_default_values$alpha.freshwater
kooi_alpha_marine     <- PSSDplusplus::param_default_values$alpha.marine

lit_alpha_df <- data.frame(
  Source = c(
    paste0("This study — fragments (µFTIR, Segur LOD, ", round(cpsd_fit_frag$lower_lod_used_um), "–", round(cpsd_fit_frag$upper_lod_um), " µm)"),
    paste0("This study — fibers (µFTIR, Segur LOD, ", round(cpsd_fit_fiber$lower_lod_used_um), "–", round(cpsd_fit_fiber$upper_lod_um), " µm)"),
    paste0("This study — all shapes (µFTIR, Segur LOD, ", round(cpsd_fit_all$lower_lod_used_um), "–", round(cpsd_fit_all$upper_lod_um), " µm)"),
    "Wang et al. 2026 — Rhine/Meuse combined suspended solids (µFTIR)",
    "Wang et al. 2026 — Rhine Lobith samples (µFTIR, mean)",
    "Wang et al. 2026 — Meuse Eijsden samples (µFTIR, mean)",
    "Segur et al. 2026 — ocean reference (C-PSD)",
    "Segur et al. 2026 — atmosphere reference (C-PSD)",
    "Zhao et al. 2026 — global surface water meta-analysis (45 studies)",
    "Zhao et al. 2026 — mesh-adjusted global surface water",
    "Kooi et al. 2021 — freshwater surface water default",
    "Kooi et al. 2021 — marine surface water default"
  ),
  Convention = c(
    rep("C-PSD a_cpsd (negative)", 3),
    rep("BN-PSD +α (positive)", 3),
    rep("C-PSD a_cpsd (negative)", 2),
    rep("BN-PSD +α (positive)", 4)
  ),
  Reported_alpha = c(
    round(cpsd_fit_frag$a_cpsd, 3),
    round(cpsd_fit_fiber$a_cpsd, 3),
    round(cpsd_fit_all$a_cpsd, 3),
    3.65, 3.89, 3.19,
    -1.66, -1.55,
    1.50, 1.53, kooi_alpha_freshwater, kooi_alpha_marine
  ),
  Equivalent_a_psd = c(
    round(cpsd_fit_frag$a_cpsd - 1, 3),
    round(cpsd_fit_fiber$a_cpsd - 1, 3),
    round(cpsd_fit_all$a_cpsd - 1, 3),
    -3.65, -3.89, -3.19,
    -2.66, -2.55,
    -1.50, -1.53, -kooi_alpha_freshwater, -kooi_alpha_marine
  ),
  check.names = FALSE
)
colnames(lit_alpha_df) <- c("Source", "Convention", "Reported value", "Equivalent a_psd")
knitr::kable(lit_alpha_df,
  caption = "Power-law size distribution slopes: this study vs. recent literature. All values converted to the BN-PSD differential slope a_psd for comparability.",
  digits = 3, align = "llrr"
)

# Individual per-site/location values (box + jitter)
alpha_indiv_df <- dplyr::bind_rows(list(
  dplyr::transmute(loc_summary,
    Study     = "This study\n(CA rivers, µFTIR)",
    alpha_abs = abs(a_psd),
    Matrix    = "Freshwater river"
  ),
  data.frame(
    Study     = "Wang 2026\n(NL rivers, µFTIR)",
    alpha_abs = c(3.89, 3.19),   # Lobith and Eijsden site means
    Matrix    = "Freshwater river"
  ),
  data.frame(
    Study     = c("Segur 2026\n(ocean)", "Segur 2026\n(atmosphere)"),
    alpha_abs = c(2.66, 2.55),
    Matrix    = c("Ocean", "Atmosphere")
  )
))

# Summary statistics only — plotted as diamond + errorbar
# Kooi et al. (2021) freshwater/marine length-slope defaults from
# PSSDplusplus::param_default_values (kooi_alpha_freshwater/marine computed in
# Section 4.2.1.2 above), shown separately rather than as one blended "global" value.
alpha_summ_df <- data.frame(
  Study    = c("Zhao 2026\n(global meta\nn = 45)", "Kooi 2021\n(freshwater)", "Kooi 2021\n(marine)"),
  mean_abs = c(1.50, kooi_alpha_freshwater, kooi_alpha_marine),
  sd_abs   = c(0.53, PSSDplusplus::param_default_values$alpha.freshwater.sd, PSSDplusplus::param_default_values$alpha.marine.sd),
  Matrix   = c("Surface water (global)", "Freshwater river", "Ocean")
)

study_order <- c(
  "This study\n(CA rivers, µFTIR)",
  "Wang 2026\n(NL rivers, µFTIR)",
  "Segur 2026\n(ocean)",
  "Segur 2026\n(atmosphere)",
  "Zhao 2026\n(global meta\nn = 45)",
  "Kooi 2021\n(freshwater)",
  "Kooi 2021\n(marine)"
)
alpha_indiv_df$Study <- factor(alpha_indiv_df$Study, levels = study_order)
alpha_summ_df$Study  <- factor(alpha_summ_df$Study,  levels = study_order)

matrix_pal_comp <- c(
  "Freshwater river"       = "#56B4E9",
  "Ocean"                  = "#0072B2",
  "Atmosphere"             = "#D55E00",
  "Surface water (global)" = "#009E73"
)

alpha_comparison_plot <- ggplot2::ggplot() +
  ggplot2::geom_boxplot(
    data = alpha_indiv_df,
    ggplot2::aes(x = Study, y = alpha_abs, color = Matrix, fill = Matrix),
    alpha = 0.25, width = 0.45, outlier.shape = NA
  ) +
  ggplot2::geom_jitter(
    data = alpha_indiv_df,
    ggplot2::aes(x = Study, y = alpha_abs, color = Matrix),
    width = 0.10, size = 2.8, alpha = 0.9
  ) +
  ggplot2::geom_point(
    data = alpha_summ_df,
    ggplot2::aes(x = Study, y = mean_abs, color = Matrix),
    size = 5, shape = 18
  ) +
  ggplot2::geom_errorbar(
    data = alpha_summ_df |> dplyr::filter(!is.na(sd_abs)),
    ggplot2::aes(x = Study, ymin = mean_abs - sd_abs, ymax = mean_abs + sd_abs, color = Matrix),
    width = 0.25, linewidth = 0.9
  ) +
  ggplot2::scale_color_manual(values = matrix_pal_comp, name = "Sample matrix") +
  ggplot2::scale_fill_manual(values  = matrix_pal_comp, name = "Sample matrix", guide = "none") +
  ggplot2::coord_cartesian(ylim = c(0.5, 5.2)) +
  ggplot2::labs(
    x        = NULL,
    y        = expression("|"*italic(a)[psd]*"|"),
    title    = "Power-law size distribution slope: this study (CA rivers) vs. literature",
    subtitle = "Boxes = IQR (per-site values). Diamond = mean; error bar = ±1 SD.",
    caption  = paste0(
      "This study: per-river all-shapes slopes (n = 4). ",
      "Wang 2026: Lobith and Eijsden site means. ",
      "Segur 2026: separate ocean and atmosphere reference slopes. ",
      "Zhao 2026 (diamond ± bar): global meta-analysis mean ± 1 SD (n = 45 studies). ",
      "Kooi et al. 2021 (diamond ± bar): freshwater/marine surface-water defaults from PSSDplusplus::param_default_values. ",
      "All values: |a_psd| = |a_cpsd − 1|."
    )
  ) +
  ggplot2::theme_minimal(base_size = 20) +
  ggplot2::theme(
    axis.text.x     = ggplot2::element_text(angle = 20, hjust = 1, size = 15),
    legend.position = "right",
    panel.border    = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.4),
    plot.caption    = ggplot2::element_text(size = 8, color = "gray50", hjust = 0)
  )

ggsave(alpha_comparison_plot, filename = "figures/alpha_comparison_plot.png", width = 12, height = 6.5, dpi = 300)

alpha_comparison_plot

cpsd_fits_river_area <- fit_cpsd_by_shape(
  raw_particles_river, value_col = "area_um2",
  config = list(
    fragment = list(bin_um = 500, fit_range_um = c(2000, 50000)),
    fiber    = list(bin_um = 500, fit_range_um = c(2000, 500000)),
    all      = list(bin_um = 500, fit_range_um = c(2000, 500000))
  )
)
cpsd_fit_area_fragment <- cpsd_fits_river_area$fragment
cpsd_fit_area_fiber    <- cpsd_fits_river_area$fiber
cpsd_fit_area          <- cpsd_fits_river_area$all

cat("Fragment Surface Area: alpha =", signif(cpsd_fit_area_fragment$a_cpsd, 2), "+-", signif(cpsd_fit_area_fragment$se_a_cpsd, 2),
    ", LOD =", signif(cpsd_fit_area_fragment$lower_lod_used_um, 2), "–", signif(cpsd_fit_area_fragment$upper_lod_um, 2), "µm²\n")
cat("Fiber Surface Area:    alpha =", signif(cpsd_fit_area_fiber$a_cpsd,    2), "+-", signif(cpsd_fit_area_fiber$se_a_cpsd,    2),
    ", LOD =", signif(cpsd_fit_area_fiber$lower_lod_used_um,    2), "–", signif(cpsd_fit_area_fiber$upper_lod_um,    2), "µm²\n")
cat("All Shape Surface Area: alpha =", signif(cpsd_fit_area$a_cpsd,         2), "+-", signif(cpsd_fit_area$se_a_cpsd,         2),
    ", LOD =", signif(cpsd_fit_area$lower_lod_used_um,          2), "–", signif(cpsd_fit_area$upper_lod_um,          2), "µm²\n")

# Full Kooi et al. (2021) length/area/volume/mass slope defaults, as embedded in
# PSSDplusplus::param_default_values — the single canonical literature-reference
# source used throughout this document (replacing the earlier, partial Kooi &
# Koelmans 2019 comparison values in Section 4.2.1.2).
kooi_pv <- PSSDplusplus::param_default_values
kooi_ref_df <- data.frame(
  Compartment = c("Freshwater surface water", "Marine surface water",
                  "Freshwater sediment", "Marine sediment"),
  alpha_length = c(kooi_pv$alpha.freshwater, kooi_pv$alpha.marine,
                   kooi_pv$alpha.sediment.freshwater, kooi_pv$alpha.sediment.marine),
  a_sa_area    = c(kooi_pv$a.sa.freshwater, kooi_pv$a.sa.marine,
                   kooi_pv$a.sa.sediment.freshwater, kooi_pv$a.sa.sediment.marine),
  a_v_volume   = c(kooi_pv$a.v.freshwater, kooi_pv$a.v.marine,
                   kooi_pv$a.v.sediment.freshwater, kooi_pv$a.v.sediment.marine),
  a_m_mass     = c(kooi_pv$a.m.freshwater, kooi_pv$a.m.marine,
                   kooi_pv$a.m.sediment.freshwater, kooi_pv$a.m.sediment.marine)
)
knitr::kable(kooi_ref_df,
  caption = "Kooi et al. (2021) power-law slope defaults by compartment and size metric (PSSDplusplus::param_default_values). All values in the positive-α convention.",
  col.names = c("Compartment", "Length (α)", "Area (a.sa)", "Volume (a.v)", "Mass (a.m)"),
  digits = 2
)

surface_area_fits_by_shape <- list(
  fragment = cpsd_fit_area_fragment,
  fiber    = cpsd_fit_area_fiber,
  all      = cpsd_fit_area
  # film  = cpsd_fit_film,
  # foam  = cpsd_fit_foam,
  # etc.
)

surfacearea_cpsd_plot <- plot_cpsd_multi(surface_area_fits_by_shape, title = "C-PSD fits by shape", attribute = "Area")

ggsave(surfacearea_cpsd_plot,
       filename = "figures/surfacearea_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

surfacearea_cpsd_plot

# volume_lod_bounds() (sourced from R/mp_risk_utils.R) converts each shape's length-LOD
# window into a volume-LOD window using the *measured* width/length ratio (r_med) for
# that shape, rather than assuming an isotropic (L=W=H) particle — so the fit range stays
# consistent with the actual length/width measurements. The "all" (pooled shapes) range is
# the union of the fragment and fiber ranges.
r_med_frag_river  <- ratio_tbl$r_med[ratio_tbl$shape == "fragment"]
r_med_fiber_river <- ratio_tbl$r_med[ratio_tbl$shape == "fiber"]
vlb_river <- volume_lod_bounds(cpsd_fit_frag, cpsd_fit_fiber, r_med_frag_river, r_med_fiber_river)
cat("Fragment volume LOD: [", signif(vlb_river$fragment[1], 2), ",", signif(vlb_river$fragment[2], 2), "] µm³\n")
cat("Fiber    volume LOD: [", signif(vlb_river$fiber[1],    2), ",", signif(vlb_river$fiber[2],    2), "] µm³\n")
cat("All      volume LOD: [", signif(vlb_river$all[1],      2), ",", signif(vlb_river$all[2],      2), "] µm³\n")

cpsd_fits_river_volume <- fit_cpsd_by_shape(
  raw_particles_river, value_col = "V_um3",
  config = list(
    fragment = list(bin_um = vol_bin_um, fit_range_um = vlb_river$fragment),
    fiber    = list(bin_um = vol_bin_um, fit_range_um = vlb_river$fiber),
    all      = list(bin_um = vol_bin_um, fit_range_um = vlb_river$all)
  )
)
cpsd_fit_volume_fragment <- cpsd_fits_river_volume$fragment
cpsd_fit_volume_fiber    <- cpsd_fits_river_volume$fiber
cpsd_fit_volume          <- cpsd_fits_river_volume$all

cat("Fragment Volume: alpha =", signif(cpsd_fit_volume_fragment$a_cpsd, 2), "+-", signif(cpsd_fit_volume_fragment$se_a_cpsd, 2),
    ", LOD =", signif(cpsd_fit_volume_fragment$lower_lod_used_um, 2), "–", signif(cpsd_fit_volume_fragment$upper_lod_um, 2), "µm³\n")
cat("Fiber Volume:    alpha =", signif(cpsd_fit_volume_fiber$a_cpsd,    2), "+-", signif(cpsd_fit_volume_fiber$se_a_cpsd,    2),
    ", LOD =", signif(cpsd_fit_volume_fiber$lower_lod_used_um,    2), "–", signif(cpsd_fit_volume_fiber$upper_lod_um,    2), "µm³\n")
cat("All Shape Volume: alpha =", signif(cpsd_fit_volume$a_cpsd,         2), "+-", signif(cpsd_fit_volume$se_a_cpsd,         2),
    ", LOD =", signif(cpsd_fit_volume$lower_lod_used_um,          2), "–", signif(cpsd_fit_volume$upper_lod_um,          2), "µm³\n")

volume_fits_by_shape <- list(
  fragment = cpsd_fit_volume_fragment,
  fiber    = cpsd_fit_volume_fiber,
  all      = cpsd_fit_volume
  # film  = cpsd_fit_film,
  # foam  = cpsd_fit_foam,
  # etc.
)

volume_cpsd_plot <- plot_cpsd_multi(volume_fits_by_shape, title = "C-PSD fits by shape", attribute = "Volume")

ggsave(volume_cpsd_plot,
       filename = "figures/volume_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

volume_cpsd_plot

# Fit C-PSD per river × shape for all three metrics.
# Uses tryCatch to handle sparse data combinations gracefully.
safe_cpsd <- function(data, ...) {
  if (length(data) < 10) return(list(valid = FALSE))
  tryCatch(
    c(fit_cpsd_segur_r(data, ...), list(valid = TRUE)),
    error = function(e) list(valid = FALSE)
  )
}

cpsd_loc_shape_length <- lapply(setNames(locations, locations), function(loc) {
  sub <- raw_particles_river |> dplyr::filter(sample_location == loc)
  list(
    fragment = safe_cpsd(sub |> dplyr::filter(shape == "fragment") |> dplyr::pull(length_um), bin_um = 5),
    fiber    = safe_cpsd(sub |> dplyr::filter(shape == "fiber")    |> dplyr::pull(length_um), bin_um = 5),
    all      = safe_cpsd(sub |> dplyr::pull(length_um), bin_um = 5)
  )
})

cpsd_loc_shape_area <- lapply(setNames(locations, locations), function(loc) {
  sub <- raw_particles_river |> dplyr::filter(sample_location == loc)
  list(
    fragment = safe_cpsd(sub |> dplyr::filter(shape == "fragment") |> dplyr::pull(area_um2),
                         bin_um = 500, fit_range_um = c(2000, 50000)),
    fiber    = safe_cpsd(sub |> dplyr::filter(shape == "fiber")    |> dplyr::pull(area_um2),
                         bin_um = 500, fit_range_um = c(2000, 500000)),
    all      = safe_cpsd(sub |> dplyr::pull(area_um2),
                         bin_um = 500, fit_range_um = c(2000, 500000))
  )
})

cpsd_loc_shape_vol <- lapply(setNames(locations, locations), function(loc) {
  sub <- raw_particles_river |> dplyr::filter(sample_location == loc)
  list(
    fragment = safe_cpsd(sub |> dplyr::filter(shape == "fragment") |> dplyr::pull(V_um3),
                         bin_um = 1e3, fit_range_um = c(fragment_volume_lod_lower, fragment_volume_lod_upper)),
    fiber    = safe_cpsd(sub |> dplyr::filter(shape == "fiber")    |> dplyr::pull(V_um3),
                         bin_um = 1e4, fit_range_um = c(fiber_volume_lod_lower, fiber_volume_lod_upper)),
    all      = safe_cpsd(sub |> dplyr::pull(V_um3),
                         bin_um = 1e2, fit_range_um = c(1e7, 1e8))
  )
})

# Helper: extract LOD bounds from a fit object
.lod_bounds <- function(fo) {
  lod_lo <- if (!is.null(fo$lower_lod_used_um) && is.finite(fo$lower_lod_used_um))
               fo$lower_lod_used_um else fo$lower_lod_um
  list(lo = lod_lo, hi = fo$upper_lod_um)
}

# Extract predicted lines + 95% CI ribbon within each fit's valid LOD window
extract_fit_lines <- function(fits_by_loc, metric_name) {
  dplyr::bind_rows(lapply(names(fits_by_loc), function(loc) {
    dplyr::bind_rows(lapply(names(fits_by_loc[[loc]]), function(shp) {
      fo <- fits_by_loc[[loc]][[shp]]
      if (!isTRUE(fo$valid) || is.null(fo$fit)) return(NULL)
      b <- .lod_bounds(fo)
      if (!is.finite(b$lo) || !is.finite(b$hi) || b$lo >= b$hi) return(NULL)
      grid <- tibble::tibble(L_low = seq(b$lo, b$hi, length.out = 80))
      pred <- tryCatch(predict(fo$fit, newdata = grid, se.fit = TRUE), error = function(e) NULL)
      if (is.null(pred)) return(NULL)
      tibble::tibble(metric = metric_name, shape = shp, location = loc,
                     L_low    = grid$L_low,
                     logN_fit = pred$fit,
                     logN_lo  = pred$fit - 1.96 * pred$se.fit,
                     logN_hi  = pred$fit + 1.96 * pred$se.fit)
    }))
  }))
}

# Extract raw binned data points with an in_lod flag for opacity mapping
extract_fit_points <- function(fits_by_loc, metric_name) {
  dplyr::bind_rows(lapply(names(fits_by_loc), function(loc) {
    dplyr::bind_rows(lapply(names(fits_by_loc[[loc]]), function(shp) {
      fo <- fits_by_loc[[loc]][[shp]]
      if (!isTRUE(fo$valid) || is.null(fo$bins)) return(NULL)
      b <- .lod_bounds(fo)
      fo$bins |>
        dplyr::arrange(L_low) |>
        dplyr::mutate(
          N_ge   = rev(cumsum(rev(n))),
          in_lod = is.finite(b$lo) & is.finite(b$hi) & L_low >= b$lo & L_high <= b$hi
        ) |>
        dplyr::filter(L_low > 0, N_ge > 0) |>
        dplyr::transmute(metric = metric_name, shape = shp, location = loc,
                         L_low, logN = log10(N_ge), in_lod)
    }))
  }))
}

# River location colour palette (defined once, reused in both chunks)
loc_palette <- c(
  "Carmel"      = "#E69F00",
  "Pajaro"      = "#56B4E9",
  "Salinas"     = "#009E73",
  "San Lorenzo" = "#CC79A7"
)

factor_levels <- list(
  metric = c("Length (µm)", "Surface Area (µm²)", "Volume (µm³)"),
  shape  = c("fragment", "fiber", "all"),
  slabel = c("Fragment",  "Fiber",  "All shapes")
)

cpsd_loc_lines <- dplyr::bind_rows(
  extract_fit_lines(cpsd_loc_shape_length, "Length (µm)"),
  extract_fit_lines(cpsd_loc_shape_area,   "Surface Area (µm²)"),
  extract_fit_lines(cpsd_loc_shape_vol,    "Volume (µm³)")
) |>
  dplyr::mutate(
    metric   = factor(metric, levels = factor_levels$metric),
    shape    = factor(shape,  levels = factor_levels$shape, labels = factor_levels$slabel),
    location = factor(location, levels = locations)
  ) |>
  dplyr::filter(!is.na(logN_fit))

cpsd_loc_points <- dplyr::bind_rows(
  extract_fit_points(cpsd_loc_shape_length, "Length (µm)"),
  extract_fit_points(cpsd_loc_shape_area,   "Surface Area (µm²)"),
  extract_fit_points(cpsd_loc_shape_vol,    "Volume (µm³)")
) |>
  dplyr::mutate(
    metric   = factor(metric, levels = factor_levels$metric),
    shape    = factor(shape,  levels = factor_levels$shape, labels = factor_levels$slabel),
    location = factor(location, levels = locations)
  ) |>
  dplyr::filter(!is.na(logN))

# Subset palette to locations that are present in data
loc_palette_used <- loc_palette[names(loc_palette) %in% as.character(unique(cpsd_loc_lines$location))]

# Points subset: ensure levels match lines
cpsd_loc_points2 <- cpsd_loc_points |>
  dplyr::filter(location %in% names(loc_palette_used)) |>
  dplyr::mutate(
    metric   = factor(metric,   levels = factor_levels$metric),
    shape    = factor(shape,    levels = factor_levels$slabel),
    location = factor(location, levels = locations)
  )

cpsd_river_facet_plot <- ggplot2::ggplot() +
  # --- Raw data points: outside LOD at low alpha ---
  ggplot2::geom_point(
    data = dplyr::filter(cpsd_loc_points2, !in_lod),
    ggplot2::aes(x = log10(L_low), y = logN, color = location),
    size = 0.7, alpha = 0.05, shape = 16
  ) +
  # --- Raw data points: inside LOD at full alpha ---
  ggplot2::geom_point(
    data = dplyr::filter(cpsd_loc_points2, in_lod),
    ggplot2::aes(x = log10(L_low), y = logN, color = location),
    size = 1.3, alpha = 0.75, shape = 16
  ) +
  # --- 95% CI ribbon (within LOD window) ---
  ggplot2::geom_ribbon(
    data = cpsd_loc_lines,
    ggplot2::aes(x = log10(L_low), ymin = logN_lo, ymax = logN_hi, fill = location),
    alpha = 0.12, color = NA
  ) +
  # --- Fitted regression line (within LOD window) ---
  ggplot2::geom_line(
    data = cpsd_loc_lines,
    ggplot2::aes(x = log10(L_low), y = logN_fit, color = location),
    linewidth = 0.9, alpha = 0.9
  ) +
  ggplot2::facet_grid(
    shape ~ metric,
    scales   = "free",
    labeller = ggplot2::label_value
  ) +
  ggplot2::scale_color_manual(values = loc_palette_used, name = "River location") +
  ggplot2::scale_fill_manual( values = loc_palette_used, name = "River location") +
  ggplot2::guides(
    color = ggplot2::guide_legend(override.aes = list(alpha = 1, size = 3)),
    fill  = "none"
  ) +
  ggplot2::labs(
    x        = expression(log[10] * "(size)"),
    y        = expression(log[10] * (N("">=L))),
    title    = "C-PSD power-law fits by river, shape, and size metric",
    subtitle = "Solid lines + ribbon = fit ± 95% CI within Segur LOD window. Faded points = outside LOD.",
    caption  = "Rows: shape. Columns: size metric. Fits per river via Segur (2026) LOD algorithm."
  ) +
  ggplot2::theme_minimal(base_size = 18) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.border    = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.4),
    strip.text      = ggplot2::element_text(face = "bold", size = 10),
    plot.caption    = ggplot2::element_text(size = 8, color = "gray50", hjust = 0)
  )

ggsave(cpsd_river_facet_plot, filename = "figures/cpsd_river_facet_plot.png",
       width = 12, height = 8, dpi = 300)
cpsd_river_facet_plot

# Extract per-location a_psd for surface area and volume from the per-river fits
extract_loc_slopes <- function(fits_by_loc, metric_name) {
  dplyr::bind_rows(lapply(names(fits_by_loc), function(loc) {
    fo <- fits_by_loc[[loc]][["all"]]
    if (!isTRUE(fo$valid) || is.null(fo$fit)) return(NULL)
    tibble::tibble(location = loc, a_psd = fo$a_psd, se_a_cpsd = fo$se_a_cpsd, metric = metric_name)
  }))
}

loc_slopes_all <- dplyr::bind_rows(
  dplyr::transmute(loc_summary, location, a_psd, se_a_cpsd, metric = "Length (µm)"),
  extract_loc_slopes(cpsd_loc_shape_area, "Surface Area (µm²)"),
  extract_loc_slopes(cpsd_loc_shape_vol,  "Volume (µm³)")
) |>
  dplyr::mutate(
    metric   = factor(metric, levels = c("Length (µm)", "Surface Area (µm²)", "Volume (µm³)")),
    location = factor(location, levels = locations)
  )

# Reference slopes (Segur 2026 ocean mean, length metric)
ref_slopes <- data.frame(
  metric    = factor("Length (µm)", levels = levels(loc_slopes_all$metric)),
  ref_slope = -2.66,
  ref_label = "Segur 2026\nfragment mean"
)

cpsd_slopes_all_metrics_plot <- ggplot2::ggplot(loc_slopes_all,
    ggplot2::aes(x = location, y = a_psd, color = location)) +
  ggplot2::geom_hline(
    data = ref_slopes,
    ggplot2::aes(yintercept = ref_slope),
    linetype = "dashed", color = "gray50", linewidth = 0.7, inherit.aes = FALSE
  ) +
  ggplot2::geom_point(size = 4) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = a_psd - se_a_cpsd, ymax = a_psd + se_a_cpsd),
    width = 0.2, linewidth = 0.8
  ) +
  ggplot2::facet_wrap(~ metric, nrow = 1, scales = "free_y") +
  ggplot2::scale_color_manual(values = loc_palette, guide = "none") +
  ggplot2::labs(
    x        = "River location",
    y        = expression(italic(a)[psd]),
    title    = "BN-PSD slope by river location and size metric",
    subtitle = "Error bars = ±1 SE  |  Dashed = Segur 2026 fragment reference (length only)"
  ) +
  ggplot2::theme_minimal(base_size = 20) +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(color = "gray85", fill = NA, linewidth = 0.4),
    strip.text   = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

ggsave(cpsd_slopes_all_metrics_plot, filename = "figures/cpsd_slopes_all_metrics_plot.png", width = 11, height = 7, dpi = 300)
cpsd_slopes_all_metrics_plot

alpha    <- alpha_dist(mu = cpsd_fit_all$a_cpsd, sd = cpsd_fit_all$se_a_cpsd, n = n_mc)
alpha_mu <- mean(alpha)
alpha_se <- sd(alpha)
summary(alpha)

aspect_ratio_hist_plot <- raw_particles_river |>
  ggplot2::ggplot(ggplot2::aes(x = 1 / aspect_ratio, fill = shape)) +
  ggplot2::geom_histogram(bins = 40) +
  ggplot2::labs(x = "Width : Length ratio (W/L)", y = "Count",
                title = "Distribution of particle width-to-length ratios by shape") +
  ggplot2::theme_minimal(base_size = 20)

ggsave(aspect_ratio_hist_plot, filename = "figures/aspect_ratio_hist_plot.png", width = 9, height = 6, dpi = 300)
aspect_ratio_hist_plot

R.ave_result <- bootstrap_aspect_ratio(raw_particles_river$aspect_ratio, n_boot = n_boot)
R.ave_vals   <- R.ave_result$vals
R.ave_mean   <- R.ave_result$boot_mean
R.ave_sd     <- R.ave_result$boot_sd
print(R.ave_result$summary)

# correct_and_bootstrap_eed() (sourced from R/mp_risk_utils.R) bundles the LOD bias
# correction, the power-law correction_factor() rescale, the Monte Carlo corrected-
# concentration draws, and the EED bootstrap into one shared step used identically
# across river/sediment/ocean.
river_corr <- correct_and_bootstrap_eed(
  monitoring  = monitoring,
  conc_col    = "C_measured_pL",
  cpsd_fit    = cpsd_fit_all,
  alpha_draws = alpha,
  L_tar_min   = 1,
  L_tar_max   = 5000,
  n_draws     = 3000,
  n_boot      = n_boot,
  probs       = c(0.5, 0.95)
)
monitoring         <- river_corr$monitoring
L_meas_min_use     <- river_corr$L_meas_min_use
L_meas_max_use     <- river_corr$L_meas_max_use
combined_cf        <- river_corr$combined_cf
C_corrected_draws  <- river_corr$C_corrected_draws
C_sample_median    <- river_corr$C_sample_median
eed_boot           <- river_corr$eed_boot

quantile(combined_cf, c(0.05, 0.5, 0.95))

# Optional multiplicative factors — not applied for this µFTIR membrane-filter dataset.
# Retain code for reference and potential future use with mesh-sampled datasets.
fiber_cf   <- rlnorm(n_mc, meanlog = log(2.0), sdlog = 0.5)    # illustrative; not applied
plastic_pf <- pmin(rbeta(n_mc, shape1 = 20, shape2 = 3), 1.0)  # illustrative; not applied

# combined_cf was already computed above (Section 5.1, via correct_and_bootstrap_eed());
# fiber_cf and plastic_pf are not multiplied in — see rationale above.
quantile(combined_cf, c(0.05, 0.5, 0.95))

# For simplicity, assume the same CF distribution applies to each sample.
# If CF depends on site/method/shape, build CF distributions per strata and apply accordingly.
# C_corrected_draws was already computed above (Section 5.1, via correct_and_bootstrap_eed()).

C_corrected_draws |>
  summarise(p05 = quantile(C_corrected, 0.05),
            p50 = quantile(C_corrected, 0.50),
            p95 = quantile(C_corrected, 0.95))

# Collapse each sample to its median corrected concentration, then bootstrap across samples.
# This isolates site-level variability from correction-factor uncertainty (handled in MC2D below).
# C_sample_median / eed_boot were already computed above (Section 5.1, via
# correct_and_bootstrap_eed(), which internally calls bootstrap_eed()).
summary(eed_boot)

eed_bootstrap_plot <- plot_eed_bootstrap(eed_boot, n_boot, "River water")

ggsave(eed_bootstrap_plot, filename = "figures/eed_bootstrap_plot.png", width = 9, height = 6, dpi = 300)
eed_bootstrap_plot

# filter species by environment
tox_data <- tomex2 |> 
   dplyr::filter(
    env_f == "Freshwater",
    Group != "Bacterium", # Drop studies with Bacterium group
    Group != "Plant", # Drop studies with Plant group
    effect.metric != "HONEC", # Drop studies with HONEC effect metric
    tier_zero_tech_f == "Red Criteria Passed", #minimum QC for technical criteria
    tier_zero_risk_f == "Red Criteria Passed", #minimum QC for risk criteria
    risk.13 != 0 #Drop studies that received a score of 0 for endpoints criteria (this also drops studies that have not yet been)
  )

# summarize by group, species, and polymer type to understand tox data included in model
tox_data |> 
  group_by(Group, Species, poly_f, shape_f) |> 
  summarise(n = n(), .groups = "drop")

# replace the default parameter matrix distribution with values derived using the C-PSD method here
param_values <- PSSDplusplus::param_default_values |> 
  # update parameters from C-PSD method
  mutate(alpha.freshwater = -alpha_mu, # use cPSD - 1 since dN/dL = k * L^a
         alpha.freshwater.sd = alpha_se,
         # additional values need updating:
         ### Length to width ratio
         R.ave.water.freshwater = R.ave_mean,
         R.ave.water.freshwater.sd = R.ave_sd,

         ### polymer density
         #p.ave.freshwater = ,
         #p.ave.freshwater.sd = ,

         ### volume
         a.v.freshwater = -cpsd_fit_volume$a_cpsd, 
         a.v.freshwater.sd = cpsd_fit_volume$se_a_cpsd,

         ### mass
         #a.m.freshwater = ,
         #a.m.freshwater.sd = ,

         ### surface area
         a.sa.freshwater = -cpsd_fit_area$a_cpsd,# derived value for all shapes
         a.sa.freshwater.sd = cpsd_fit_area$se_a_cpsd
         )

# generate parameter matrix using bespoke environmental parameter values
param_matrix <- matrix_function(
  n = n_boot, #size of matrix (10,000 is recommended)
  params = param_values, #parameter distribution values 
  #params = PSSDplusplus::param_default_values, #beta-test with defaults
  upper.tissue.truncation.limit = 500, # maximum particle size (microns) to truncate tissue tranlocation size limit distribution to based on biological plausibility
  x1M_set = 1, #minimum particle size to generate distributions to in microns
  x2D_set = 5000 #maximum particle size to generate distributions to in microns
) |> 
  # replace R.ave.water.freshwater with bootstrapped values
   mutate(R.ave.water.freshwater = sample(R.ave_vals, size = n(), replace = TRUE),
          H_W_ratio.freshwater = sample(R.ave_vals, size = n(), replace = TRUE)) #H_W ratio is assumed same as length-width ratio

# visualize parameter distributions
param_plots <- parameter_histograms_function(param_matrix) # generate parameter distribution plots
#visualize plots
alpha_combined_plot_river <- param_plots$alpha_combined_plot # distribution of alpha parameters
alpha_combined_plot_river <- label_matrix(alpha_combined_plot_river, "River water")

ggsave(alpha_combined_plot_river, filename = "figures/alpha_combined_plot_river.png", width = 9, height = 6, dpi = 300)
alpha_combined_plot_river

# run_pssd_pipeline() (sourced from R/mp_risk_utils.R) bundles MC_sim_align_parallel(),
# the ERM-specific results_df filtering, erm_registry construction, and make_all_pSSDs()
# into one step used identically across river/sediment/ocean.
river_pssd <- run_pssd_pipeline(
  tox_data     = tox_data,
  param_matrix = param_matrix,
  environments = c("Freshwater"),
  cache_suffix = "river",
  dose_unit    = "L",
  n_sim        = n_boot,
  num_cores    = parallel::detectCores() - 2,
  sim          = 30,
  cv_uf        = 0.5,
  rmore_method = "lognormal"
)
MC_sim_df    <- river_pssd$MC_sim_df
erm_registry <- river_pssd$erm_registry

pSSDs <- river_pssd$pSSDs

PNEC_summary <- summarize_PNECs(pSSDs)
head(PNEC_summary)

PNEC_plot_05_river <- pSSDs$`Tier3_Freshwater_Food Dilution`$PNEC_plot_05 +
  theme_minimal(base_size = 20)
PNEC_plot_05_river <- label_matrix(PNEC_plot_05_river, "River water")

ggsave(PNEC_plot_05_river, filename = "figures/PNEC_plot_05_river.png", width = 9, height = 6, dpi = 300)
PNEC_plot_05_river

# NOTE: this plot uses ggrepel::geom_label_repel internally (via PSSDplusplus::pSSD_plot_fnx).
# Re-printing a ggrepel layer multiple times in one session can hit a "Viewport has zero
# dimension(s)" grid bug, so we save once via ggsave() and display the rendered PNG rather
# than re-drawing the live ggplot object a second time.
pSSD_plot_river <- pSSDs$`Tier3_Freshwater_Food Dilution`$pSSD_plot + theme_minimal(base_size = 20)
pSSD_plot_river <- label_matrix(pSSD_plot_river, "River water")

ggsave(pSSD_plot_river, filename = "figures/pSSD_plot_river.png", width = 9, height = 6, dpi = 300)
knitr::include_graphics("figures/pSSD_plot_river.png")

# Also contains the ggrepel-based pSSD_plot (via ggpubr::ggarrange) — same rationale as above.
arranged_plot_river <- pSSDs$`Tier3_Freshwater_Food Dilution`$arranged_plot + theme_minimal(base_size = 20)
arranged_plot_river <- label_matrix(arranged_plot_river, "River water")

ggsave(arranged_plot_river, filename = "figures/arranged_plot_river.png", width = 11, height = 8, dpi = 300)
knitr::include_graphics("figures/arranged_plot_river.png")

# build_haz_df() (sourced from R/mp_risk_utils.R) extracts and row-binds the HC5/HC10 x
# Food Dilution/Tissue Translocation hazard summaries — used identically per matrix.
haz <- build_haz_df(pSSDs, "Freshwater")

hazard_threshold_plot <- plot_hazard_threshold(haz, "River water")

ggsave(hazard_threshold_plot, filename = "figures/hazard_threshold_plot.png", width = 9, height = 6, dpi = 300)
hazard_threshold_plot

# draw_rq_mc1d() and summarize_rq() are sourced from R/mp_risk_utils.R.
n_risk     <- 1000
risk_draws <- draw_rq_mc1d(haz, eed_boot$q50, n_risk = n_risk)
risk_summary <- summarize_rq(risk_draws)
risk_summary

rq_plot <- plot_rq_hist(risk_draws, "River water")

ggsave(rq_plot, filename = "figures/rq_plot.png", width = 9, height = 6, dpi = 300)
rq_plot

# Extract key risk metrics for inline reporting
rs_food_hc5    <- dplyr::filter(risk_summary, ERM == "Food Dilution",       HCx == 5)
rs_tissue_hc5  <- dplyr::filter(risk_summary, ERM == "Tissue Translocation", HCx == 5)

# plot_ecdf_overlap() (sourced from R/risk_plotting.R) wraps the ecdf_bands() calls
# (from R/mp_risk_utils.R) for the exposure and hazard distributions and combines
# them into one CDF overlap plot — used identically across river/sediment/ocean.
ecdf_overlap_plot <- plot_ecdf_overlap(
  eed_vals     = C_sample_median$C_corr_med,
  haz_df       = haz,
  matrix_label = "River water",
  n_boot       = n_boot
)

ggsave(ecdf_overlap_plot, filename = "figures/ecdf_overlap_plot.png", width = 10, height = 6, dpi = 300)
ecdf_overlap_plot

# mc2d_risk() is sourced from R/mp_risk_utils.R.
# Outer loop: n_uncertainty = 300 CF + hazard draws (uncertainty).
# Inner loop: n_variability = 1000 exposure draws per outer iteration (variability).
mc2d_results <- mc2d_risk(
  monitoring_df = monitoring,
  combined_cf   = combined_cf,
  haz_df        = haz,
  n_uncertainty = 300,
  n_variability = 1000,
  seed          = 1
)

mc2d_summary <- mc2d_results |>
  dplyr::group_by(ERM, HCx) |>
  summarise(
    P_exceed_med = median(P_exceed),
    P_exceed_p95 = quantile(P_exceed, 0.95),
    P_exceed_p05 = quantile(P_exceed, 0.05),
    RQ_p50_med = median(RQ_p50),
    RQ_p95_med = median(RQ_p95),
    .groups = "drop"
  )

mc2d_summary

# plot_mc2d_diagnostic() (sourced from R/risk_plotting.R) replaces the 4 near-identical
# diagnostic plots below with one shared, matrix-labeled function. MC2D stays river-only.
mc2d_pexceed_hist_plot <- plot_mc2d_diagnostic(mc2d_results, "River water", type = "pexceed_hist")

ggsave(mc2d_pexceed_hist_plot, filename = "figures/mc2d_pexceed_hist_plot.png", width = 9, height = 6, dpi = 300)
mc2d_pexceed_hist_plot

mc2d_pexceed_boxplot <- plot_mc2d_diagnostic(mc2d_results, "River water", type = "pexceed_boxplot")

ggsave(mc2d_pexceed_boxplot, filename = "figures/mc2d_pexceed_boxplot.png", width = 9, height = 6, dpi = 300)
mc2d_pexceed_boxplot

mc2d_cf_scatter_plot <- plot_mc2d_diagnostic(mc2d_results, "River water", type = "cf_scatter")

ggsave(mc2d_cf_scatter_plot, filename = "figures/mc2d_cf_scatter_plot.png", width = 9, height = 6, dpi = 300)
mc2d_cf_scatter_plot

mc2d_rq_density_plot <- plot_mc2d_diagnostic(mc2d_results, "River water", type = "rq_density")

ggsave(mc2d_rq_density_plot, filename = "figures/mc2d_rq_density_plot.png", width = 9, height = 7, dpi = 300)
mc2d_rq_density_plot

# param_bounds() is sourced from R/mp_risk_utils.R.
morris_params  <- param_bounds(param_values, k = 2)
morris_subset  <- c("alpha.freshwater", "R.ave.water.freshwater", "a.sa.freshwater")
morris_params  <- if (all(morris_subset %in% morris_params$param)) {
  dplyr::filter(morris_params, param %in% morris_subset)
} else {
  dplyr::slice(morris_params, 1:min(4, nrow(morris_params)))
}
morris_params

# Helper: build hazard distributions from a custom param_values dataframe (small budgets for beta test)
build_haz_from_params <- function(pv = param_values,
                                  n_matrix = 2,
                                  n_sim = 2,
                                  sim = 2,
                                  num_cores = 1) {
  param_matrix_i <- matrix_function(
    n = n_matrix,
    params = pv,
    upper.tissue.truncation.limit = 500,
    x1M_set = 1,
    x2D_set = 5000
  )

  MC_sim_df_i <- MC_sim_align_parallel(
    tox_data = tox_data,
    param_matrix = param_matrix_i,
    n_sim = n_matrix,
    x1D_set = 1,
    x2D_set = 5000
  )

  results_df_food_i <- dplyr::filter(
    MC_sim_df_i,
    ingestible != "not ingestible",
    particles_L_food_dilution > 0,
    Group != "Algae"
  ) |>
    dplyr::mutate(dose_new_particles_L = particles_L_food_dilution) |>
    tidyr::drop_na(particles_L_food_dilution)
  results_df_food_t3_t4_i <- dplyr::filter(
    results_df_food_i,
    risk.13 != 1,
    bio_f %in% c("Organism", "Population")
  )

  results_df_tissue_i <- dplyr::filter(
    MC_sim_df_i,
    translocatable != "not translocatable",
    particles_L_ox_stress > 0
  ) |>
    dplyr::mutate(dose_new_particles_L = particles_L_ox_stress) |>
    tidyr::drop_na(particles_L_ox_stress)
  results_df_tissue_t3_t4_i <- dplyr::filter(
    results_df_tissue_i,
    risk.13 != 1,
    bio_f %in% c("Organism", "Population")
  )

  erm_registry_i <- list(
    "Food Dilution" = list(base = results_df_food_i, t3_t4 = results_df_food_t3_t4_i),
    "Tissue Translocation" = list(base = results_df_tissue_i, t3_t4 = results_df_tissue_t3_t4_i)
  )

  pSSDs_i <- make_all_pSSDs(
    MC_sim_df = MC_sim_df_i,
    environments = c("Freshwater"),
    erm_registry = erm_registry_i,
    sim = sim,
    cv_uf = 0.5,
    rmore_method = "lognormal",    
    parallel = T,
    workers = parallel::detectCores() - 1,
    base_cache_dir = file.path(tempdir(), "morris_pssd_cache"),
    base_output_path = file.path(tempdir(), "morris_pssd_figures"),
    overwrite_cache = TRUE
  )

  haz_HC5_food_i <- pSSDs_i$`Tier3_Freshwater_Food Dilution`$summary_05$df |>
    mutate(HCx = 5, ERM = "Food Dilution")
  haz_HC10_food_i <- pSSDs_i$`Tier3_Freshwater_Food Dilution`$summary_10$df |>
    mutate(HCx = 10, ERM = "Food Dilution")
  haz_HC5_tissue_i <- pSSDs_i$`Tier3_Freshwater_Tissue Translocation`$summary_05$df |>
    mutate(HCx = 5, ERM = "Tissue Translocation")
  haz_HC10_tissue_i <- pSSDs_i$`Tier3_Freshwater_Tissue Translocation`$summary_10$df |>
    mutate(HCx = 10, ERM = "Tissue Translocation")

  bind_rows(haz_HC5_food_i, haz_HC10_food_i, haz_HC5_tissue_i, haz_HC10_tissue_i)
}

# Morris model: returns a scalar RQ_p50 (median across ERM/HCx) for each input set.
# tryCatch handles PSSDplusplus failures at small budget sizes.
morris_model <- function(X) {
  apply(X, 1, function(x_row) {
    pv <- param_values
    for (j in seq_along(x_row)) {
      pv[[colnames(X)[j]]] <- x_row[j]
    }
    tryCatch({
      haz_i  <- build_haz_from_params(pv, n_matrix = 3, n_sim = 3, sim = 15, num_cores = 5)
      mc2d_i <- mc2d_risk(
        monitoring_df = monitoring,
        combined_cf   = combined_cf,
        haz_df        = haz_i,
        n_uncertainty = 50,
        n_variability = 200,
        seed          = 1
      )
      result <- median(mc2d_i$RQ_p50, na.rm = TRUE)
      if (is.nan(result) || is.na(result)) NA_real_ else result
    }, error = function(e) NA_real_)
  })
}

# Small Morris budget for beta test
morris_levels <- 3
morris_r      <- 3

morris_design <- sensitivity::morris(
  model  = NULL,
  factors = morris_params$param,
  r      = morris_r,
  design = list(type = "oat", levels = morris_levels, grid.jump = 1),
  binf   = morris_params$min,
  bsup   = morris_params$max
)

morris_design$y <- morris_model(morris_design$X)

# Convert NaN → NA so tell() handles them as missing rather than producing NaN EEs
morris_design$y[is.nan(morris_design$y)] <- NA_real_
message("Morris y values: ", paste(round(morris_design$y, 4), collapse = ", "))
morris_res <- sensitivity::tell(morris_design)
print(morris_res)

# Defensive extraction: handles NULL, wrong-length, or all-NaN returns from tell()
.pull_morris <- function(x, n) {
  if (is.null(x)) return(rep(NA_real_, n))
  v <- as.numeric(x)
  if (length(v) != n) {
    v <- c(x)
    if (length(v) != n) return(rep(NA_real_, n))
  }
  # Coerce NaN → NA for consistent downstream handling
  v[is.nan(v)] <- NA_real_
  v
}

n_factors <- nrow(morris_params)

morris_df <- data.frame(
  parameter = morris_params$param,
  mu_star   = .pull_morris(morris_res$mu.star, n_factors),
  sigma     = .pull_morris(morris_res$sigma,   n_factors),
  mu        = .pull_morris(morris_res$mu,       n_factors)
) |>
  dplyr::arrange(dplyr::desc(dplyr::coalesce(mu_star, -Inf))) |>
  dplyr::mutate(
    rank  = seq_len(dplyr::n()),
    label = gsub("\\.(freshwater|marine)$", "", parameter, ignore.case = TRUE)
  )

morris_df$has_interaction <- morris_df$sigma / pmax(morris_df$mu_star, 1e-9) > 0.5

# Rows with valid (finite) data for plotting
morris_df_valid <- morris_df |> dplyr::filter(is.finite(mu_star) & is.finite(sigma))
morris_has_data <- nrow(morris_df_valid) > 0

# Axis limits: use valid data range or fallback
xlim_m <- if (morris_has_data) c(0, max(morris_df_valid$mu_star, 0.1) * 1.15) else c(0, 1)
ylim_m <- if (morris_has_data) c(0, max(morris_df_valid$sigma,   0.1) * 1.15) else c(0, 1)

morris_gg <- ggplot2::ggplot(
    morris_df_valid,
    ggplot2::aes(x = mu_star, y = sigma, color = has_interaction)
  ) +
  ggplot2::geom_abline(slope = 1,   intercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.6) +
  ggplot2::geom_abline(slope = 0.5, intercept = 0, linetype = "dotted", color = "gray60", linewidth = 0.5) +
  (if (morris_has_data) {
    list(
      ggplot2::geom_point(size = 4),
      ggrepel::geom_text_repel(
        ggplot2::aes(label = label),
        size               = 3.5,
        box.padding        = 0.5,
        point.padding      = 0.4,
        max.overlaps       = 20,
        segment.color      = "gray60",
        segment.size       = 0.35,
        min.segment.length = 0.2
      )
    )
  } else {
    ggplot2::annotate(
      "text", x = 0.5, y = 0.5,
      label = paste0(
        "No valid model outputs (budget too small).\n",
        "Increase morris_r (≥ 10) and morris_levels (≥ 4) for production results."
      ),
      size = 4, color = "gray40", hjust = 0.5, vjust = 0.5
    )
  }) +
  ggplot2::scale_color_manual(
    values = c(`TRUE` = "#D55E00", `FALSE` = "#0072B2"),
    labels = c(`TRUE` = "σ/µ* > 0.5 (interactions/non-linearity)",
               `FALSE` = "σ/µ* ≤ 0.5 (approximately linear)"),
    name   = NULL,
    na.value = "gray70"
  ) +
  ggplot2::coord_cartesian(xlim = xlim_m, ylim = ylim_m) +
  ggplot2::labs(
    x        = expression(mu^"*" ~ "(mean absolute elementary effect — overall importance)"),
    y        = expression(sigma ~ "(SD of elementary effects — non-linearity / interactions)"),
    title    = "Morris sensitivity analysis: parameter importance",
    subtitle = "River water — dashed: σ = µ*; dotted: σ = 0.5 µ*. Points above dashed line indicate interactions.",
    caption  = paste0(
      "n = ", morris_r, " trajectories, ", morris_levels, " levels. ",
      "Output metric: median RQ_p50 across MC2D iterations."
    )
  ) +
  ggplot2::theme_minimal(base_size = 20) +
  ggplot2::theme(
    legend.position = "bottom",
    panel.border    = ggplot2::element_rect(color = "gray80", fill = NA, linewidth = 0.4),
    plot.caption    = ggplot2::element_text(size = 9, color = "gray50", hjust = 0)
  )

ggsave(morris_gg, filename = "figures/morris_gg.png", width = 9, height = 7, dpi = 300)
# Uses ggrepel::geom_text_repel when morris_has_data — save once, display the rendered PNG
# rather than re-drawing the live ggplot object (see pSSD_plot note in Section 7.4.2).
knitr::include_graphics("figures/morris_gg.png")

# Safely extract Morris results; all values may be NA for small-budget runs
morris_valid  <- !all(is.na(morris_df$mu_star))
morris_top    <- if (morris_valid) morris_df$label[1]               else NA_character_
morris_top_mu <- if (morris_valid) round(morris_df$mu_star[1], 3)   else NA_real_
morris_2nd    <- if (morris_valid && nrow(morris_df) > 1) morris_df$label[2]             else NA_character_
morris_2nd_mu <- if (morris_valid && nrow(morris_df) > 1) round(morris_df$mu_star[2], 3) else NA_real_
n_interacting <- sum(morris_df$has_interaction, na.rm = TRUE)

# Compare CF and risk outcomes under two L_meas_min assumptions:
# 50 µm (current, includes marginal zone) vs 100 µm (excludes marginal zone — conservative floor)
lmeas_scenarios <- list(
  "L_meas_min = 50 µm (current)" = list(L_lo = 50,  L_hi = 500),
  "L_meas_min = 100 µm (conservative)" = list(L_lo = 100, L_hi = 500)
)

lmeas_cf_summary <- dplyr::bind_rows(lapply(names(lmeas_scenarios), function(scen) {
  s   <- lmeas_scenarios[[scen]]
  cf_s <- correction_factor(
    a           = alpha,                  # same alpha draws (n_mc)
    L_meas_min  = s$L_lo,
    L_meas_max  = s$L_hi,
    L_tar_min   = 1,
    L_tar_max   = 5000
  )
  data.frame(
    scenario = scen,
    CF_p05   = round(quantile(cf_s, 0.05),  1),
    CF_med   = round(median(cf_s),           1),
    CF_p95   = round(quantile(cf_s, 0.95),  1)
  )
}))

knitr::kable(lmeas_cf_summary,
  caption = paste0(
    "Correction factor (50–500 µm → 1–5000 µm) under alternative lower detection limits. ",
    "Using α_cpsd = ", round(cpsd_fit_all$a_cpsd, 3), " (all shapes, Segur LOD). ",
    "n_mc = ", n_mc, " Monte Carlo draws."
  ),
  col.names = c("Scenario", "CF 5th pct", "CF median", "CF 95th pct")
)

cf_50  <- lmeas_cf_summary$CF_med[lmeas_cf_summary$scenario == "L_meas_min = 50 µm (current)"]
cf_100 <- lmeas_cf_summary$CF_med[lmeas_cf_summary$scenario == "L_meas_min = 100 µm (conservative)"]

# import sediment particle data (beach sand as sediment proxy)
raw_particles_sed <- raw_particles |> 
  dplyr::filter(sample_type == "beach sand")

# ---- Thickness (height) factor from sediment data ----
ratio_tbl_sed <- raw_particles_sed |>
  mutate(
    WL = 1 / aspect_ratio,
    width_eff_um = case_when(
      shape == "fragment" & !is.na(area_um2) & !is.na(length_um) & length_um > 0 &
        !is.na(circularity) & circularity < 0.7 ~ area_um2 / length_um,
      TRUE ~ width_um
    )
  ) |> 
  group_by(shape) |>
  summarise(
    n = n(),
    r_med  = median(WL, na.rm = TRUE),
    r_low  = quantile(WL, 0.25, na.rm = TRUE),
    r_high = quantile(WL, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Volumes ----
raw_particles_sed <- raw_particles_sed |>
  left_join(ratio_tbl_sed |> dplyr::select(shape, r_med), by = "shape") |>  
  mutate(
    height_um = r_med * width_um,
    V_um3 = case_when(
      shape == "fragment" ~ (pi / 6) * length_um * width_um * height_um,
      shape == "fiber"    ~ pi * (width_um / 2)^2 * length_um,
      TRUE ~ NA_real_
    )
  )

cpsd_fits_sed_length <- fit_cpsd_by_shape(
  raw_particles_sed, value_col = "length_um",
  config = list(
    fragment = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_)),
    fiber    = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_)),
    all      = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_))
  )
)
cpsd_fit_sed_frag  <- cpsd_fits_sed_length$fragment
cpsd_fit_sed_fiber <- cpsd_fits_sed_length$fiber
cpsd_fit_sed_all   <- cpsd_fits_sed_length$all

alpha_sed <- alpha_dist(
  mu = cpsd_fit_sed_all$a_cpsd,
  sd = cpsd_fit_sed_all$se_a_cpsd,
  n = 20000
)

alpha_sed_mu <- mean(alpha_sed)
alpha_sed_se <- sd(alpha_sed)

cat("Sediment All: alpha =", signif(cpsd_fit_sed_all$a_cpsd, 2), "+-", signif(cpsd_fit_sed_all$se_a_cpsd, 2),
    ", LOD =", signif(cpsd_fit_sed_all$lower_lod_used_um, 2), "–", signif(cpsd_fit_sed_all$upper_lod_um, 2), "µm\n")

sed_length_fits_by_shape <- list(
  fragment = cpsd_fit_sed_frag,
  fiber    = cpsd_fit_sed_fiber,
  all      = cpsd_fit_sed_all
)
sed_length_fits_by_shape <- sed_length_fits_by_shape[names(MP_PALETTE)[names(MP_PALETTE) %in% names(sed_length_fits_by_shape)]]

sed_length_cpsd_plot <- plot_cpsd_multi(sed_length_fits_by_shape, title = "Sediment C-PSD fits by shape", attribute = "Length")

ggsave(sed_length_cpsd_plot,
       filename = "figures/sediment_length_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

sed_length_cpsd_plot

cpsd_fits_sed_area <- fit_cpsd_by_shape(
  raw_particles_sed, value_col = "area_um2",
  config = list(
    fragment = list(bin_um = 500, fit_range_um = c(2000, 50000)),
    fiber    = list(bin_um = 500, fit_range_um = c(2000, 500000)),
    all      = list(bin_um = 500, fit_range_um = c(2000, 500000))
  )
)
cpsd_fit_area_sed_fragment <- cpsd_fits_sed_area$fragment
cpsd_fit_area_sed_fiber    <- cpsd_fits_sed_area$fiber
cpsd_fit_area_sed          <- cpsd_fits_sed_area$all

cat("All Shape Sediment Area: alpha =", signif(cpsd_fit_area_sed$a_cpsd, 2), "+-", signif(cpsd_fit_area_sed$se_a_cpsd, 2),
    ", LOD =", signif(cpsd_fit_area_sed$lower_lod_used_um, 2), "–", signif(cpsd_fit_area_sed$upper_lod_um, 2), "µm²\n")

sed_area_fits_by_shape <- list(
  fragment = cpsd_fit_area_sed_fragment,
  fiber    = cpsd_fit_area_sed_fiber,
  all      = cpsd_fit_area_sed
)

sed_area_cpsd_plot <- plot_cpsd_multi(sed_area_fits_by_shape, title = "Sediment C-PSD fits by shape", attribute = "Area")

ggsave(sed_area_cpsd_plot,
       filename = "figures/sediment_surfacearea_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

sed_area_cpsd_plot

# volume_lod_bounds() (sourced from R/mp_risk_utils.R) — see the river volume-lod-bounds
# chunk (Section 4.2.3) for the rationale: length/width-consistent bounds via the measured
# r_med ratio, rather than an isotropic (L=W=H) assumption.
r_med_frag_sed  <- ratio_tbl_sed$r_med[ratio_tbl_sed$shape == "fragment"]
r_med_fiber_sed <- ratio_tbl_sed$r_med[ratio_tbl_sed$shape == "fiber"]
vlb_sed <- volume_lod_bounds(cpsd_fit_sed_frag, cpsd_fit_sed_fiber, r_med_frag_sed, r_med_fiber_sed)
cat("Sediment fragment volume LOD: [", signif(vlb_sed$fragment[1], 2), ",", signif(vlb_sed$fragment[2], 2), "] µm³\n")
cat("Sediment fiber    volume LOD: [", signif(vlb_sed$fiber[1],    2), ",", signif(vlb_sed$fiber[2],    2), "] µm³\n")
cat("Sediment all      volume LOD: [", signif(vlb_sed$all[1],      2), ",", signif(vlb_sed$all[2],      2), "] µm³\n")

cpsd_fits_sed_volume <- fit_cpsd_by_shape(
  raw_particles_sed, value_col = "V_um3",
  config = list(
    fragment = list(bin_um = vol_bin_um, fit_range_um = vlb_sed$fragment),
    fiber    = list(bin_um = vol_bin_um, fit_range_um = vlb_sed$fiber),
    all      = list(bin_um = vol_bin_um, fit_range_um = vlb_sed$all)
  )
)
cpsd_fit_volume_sed_fragment <- cpsd_fits_sed_volume$fragment
cpsd_fit_volume_sed_fiber    <- cpsd_fits_sed_volume$fiber
cpsd_fit_volume_sed          <- cpsd_fits_sed_volume$all

cat("Sediment Fragment Volume: alpha =", signif(-cpsd_fit_volume_sed_fragment$a_cpsd, 2), "+-", signif(cpsd_fit_volume_sed_fragment$se_a_cpsd, 2),
    ", LOD =", signif(cpsd_fit_volume_sed_fragment$lower_lod_used_um, 2), "–", signif(cpsd_fit_volume_sed_fragment$upper_lod_um, 2), "µm³\n")
cat("Sediment Fiber Volume:    alpha =", signif(-cpsd_fit_volume_sed_fiber$a_cpsd,    2), "+-", signif(cpsd_fit_volume_sed_fiber$se_a_cpsd,    2),
    ", LOD =", signif(cpsd_fit_volume_sed_fiber$lower_lod_used_um,    2), "–", signif(cpsd_fit_volume_sed_fiber$upper_lod_um,    2), "µm³\n")
cat("Sediment All Shape Volume: alpha =", signif(-cpsd_fit_volume_sed$a_cpsd,         2), "+-", signif(cpsd_fit_volume_sed$se_a_cpsd,         2),
    ", LOD =", signif(cpsd_fit_volume_sed$lower_lod_used_um,          2), "–", signif(cpsd_fit_volume_sed$upper_lod_um,          2), "µm³\n")

sed_volume_fits_by_shape <- list(
  fragment = cpsd_fit_volume_sed_fragment,
  fiber    = cpsd_fit_volume_sed_fiber,
  all      = cpsd_fit_volume_sed
)

sed_volume_cpsd_plot <- plot_cpsd_multi(sed_volume_fits_by_shape, title = "Sediment C-PSD fits by shape", attribute = "Volume")

ggsave(sed_volume_cpsd_plot,
       filename = "figures/sediment_volume_cpsd_plot.png",
       width = 8, height = 6, dpi = 300)

sed_volume_cpsd_plot

monitoring_sed <- readRDS("data_input/Part_dets_summ.rds") |> 
  filter(
    sample_type == "beach sand",
    material_simple == "plastic",
    sample_or_blank == "sample"
  ) |>
  mutate(
    date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"),
    date = ymd(date_raw),
    sample_id = Client_ID_MSSupdate,
    # placeholder: update if a sediment-specific concentration column is available
    C_measured_pkg = extrap_conc_PPL,
    Lmin_measured_um = 50,
    Lmax_measured_um = 500
  )

sed_corr <- correct_and_bootstrap_eed(
  monitoring  = monitoring_sed,
  conc_col    = "C_measured_pkg",
  cpsd_fit    = cpsd_fit_sed_all,
  alpha_draws = alpha_sed,
  L_tar_min   = 1,
  L_tar_max   = 5000,
  n_draws     = 3000,
  n_boot      = n_boot,
  probs       = c(0.5, 0.95)
)
monitoring_sed        <- sed_corr$monitoring
L_meas_min_use_sed    <- sed_corr$L_meas_min_use
L_meas_max_use_sed    <- sed_corr$L_meas_max_use
combined_cf_sed       <- sed_corr$combined_cf
C_corrected_draws_sed <- sed_corr$C_corrected_draws
C_sample_median_sed   <- sed_corr$C_sample_median
eed_boot_sed          <- sed_corr$eed_boot
summary(eed_boot_sed)

sed_eed_bootstrap_plot <- plot_eed_bootstrap(eed_boot_sed, n_boot, "Sediment")

ggsave(sed_eed_bootstrap_plot, filename = "figures/sed_eed_bootstrap_plot.png", width = 9, height = 6, dpi = 300)
sed_eed_bootstrap_plot

tox_data_sed <- tomex2 |> 
   dplyr::filter(
    env_f %in% c("Marine", "Freshwater"),
    exposure.route == "sediment",
    Group != "Bacterium",
    effect.metric != "HONEC",
    tier_zero_tech_f == "Red Criteria Passed",
    tier_zero_risk_f == "Red Criteria Passed",
    risk.13 != 0
  ) |> 
    # since limited data available, and working with estuarine system - combine marine and freshwater species by overwriting environment to be freshwater for all
    mutate(env_f = "Freshwater")

R.ave_sed_result <- bootstrap_aspect_ratio(raw_particles_sed$aspect_ratio, n_boot = n_boot)
R.ave_sed_vals   <- R.ave_sed_result$vals
R.ave_sed_mean   <- R.ave_sed_result$boot_mean
R.ave_sed_sd     <- R.ave_sed_result$boot_sd

param_values_sed <- PSSDplusplus::param_default_values

param_values_sed[[paste0("alpha.sediment.freshwater")]] <- -alpha_sed_mu
param_values_sed[[paste0("alpha.sediment.freshwater.sd")]] <- alpha_sed_se
param_values_sed[[paste0("R.ave.sediment.freshwater")]] <- R.ave_sed_mean
param_values_sed[[paste0("R.ave.sediment.freshwater.sd")]] <- R.ave_sed_sd
param_values_sed[[paste0("a.v.sediment.freshwater")]] <- -cpsd_fit_volume_sed$a_cpsd
param_values_sed[[paste0("a.v.sediment.freshwater.sd")]] <- cpsd_fit_volume_sed$se_a_cpsd
param_values_sed[[paste0("a.sa.sediment.freshwater")]] <- -cpsd_fit_area_sed$a_cpsd
param_values_sed[[paste0("a.sa.sediment.freshwater.sd")]] <- cpsd_fit_area_sed$se_a_cpsd

# pair down to only used parameters
param_values_sed <- param_values_sed |> dplyr::select(contains("sediment.freshwater"), contains("beta"), contains("body"))
param_values_sed

param_matrix_sed <- matrix_function(
  n = n_boot,
  params = param_values_sed,
  upper.tissue.truncation.limit = 500,
  x1M_set = 1,
  x2D_set = 5000,
  include_marine_surface_water = F,
  include_freshwater_surface_water = F,
  include_marine_sediment = F,
  include_freshwater_sediment = TRUE
)

sed_R_col <- rlang::sym(paste0("R.ave.sediment.freshwater"))
sed_HW_col <- rlang::sym(paste0("H_W_ratio.sediment.freshwater"))

param_matrix_sed <- param_matrix_sed |>
  mutate(
    !!sed_R_col := sample(R.ave_sed_vals, size = n(), replace = TRUE),
    !!sed_HW_col := sample(R.ave_sed_vals, size = n(), replace = TRUE)
  )

# display distributions
sediment_param_plots <- parameter_histograms_function(
  param_matrix_sed,
  compartments = c("Freshwater Sediment")
)
alpha_combined_plot_sed <- sediment_param_plots$alpha_combined_plot
alpha_combined_plot_sed <- label_matrix(alpha_combined_plot_sed, "Sediment")

ggsave(alpha_combined_plot_sed, filename = "figures/alpha_combined_plot_sed.png", width = 9, height = 6, dpi = 300)
alpha_combined_plot_sed

# run_pssd_pipeline() (sourced from R/mp_risk_utils.R) bundles MC_sim_align_parallel(),
# the ERM-specific results_df filtering, erm_registry construction, and make_all_pSSDs()
# into one step used identically across river/sediment/ocean. Sediment is mass-based
# (particles/kg), hence dose_unit = "kg".
sed_pssd <- run_pssd_pipeline(
  tox_data     = tox_data_sed,
  param_matrix = param_matrix_sed,
  environments = c("Freshwater Sediment"),
  cache_suffix = "sediment",
  dose_unit    = "kg",
  n_sim        = n_boot,
  num_cores    = parallel::detectCores() - 2,
  sim          = 30,
  cv_uf        = 0.5,
  rmore_method = "lognormal"
)
MC_sim_df_sed    <- sed_pssd$MC_sim_df
erm_registry_sed <- sed_pssd$erm_registry

pSSDs_sed <- sed_pssd$pSSDs

# Uses ggrepel internally — save once, display the rendered PNG (see river pSSD_plot note above).
pSSD_plot_sed <- pSSDs_sed$`Tier3_Freshwater Sediment_Food Dilution`$pSSD_plot
pSSD_plot_sed <- label_matrix(pSSD_plot_sed, "Sediment")

ggsave(pSSD_plot_sed, filename = "figures/pSSD_plot_sed.png", width = 9, height = 6, dpi = 300)
knitr::include_graphics("figures/pSSD_plot_sed.png")

haz_sed <- build_haz_df(pSSDs_sed, "Freshwater Sediment")

n_risk_sed <- 1000
risk_draws_sed <- draw_rq_mc1d(haz_sed, eed_boot_sed$q50, n_risk = n_risk_sed)
risk_summary_sed <- summarize_rq(risk_draws_sed)
risk_summary_sed

sed_hazard_threshold_plot <- plot_hazard_threshold(haz_sed, "Sediment")

ggsave(sed_hazard_threshold_plot, filename = "figures/sed_hazard_threshold_plot.png", width = 9, height = 6, dpi = 300)
sed_hazard_threshold_plot

sed_rq_plot <- plot_rq_hist(risk_draws_sed, "Sediment")

ggsave(sed_rq_plot, filename = "figures/sed_rq_plot.png", width = 9, height = 6, dpi = 300)
sed_rq_plot

rs_sed_food_hc5   <- dplyr::filter(risk_summary_sed, ERM == "Food Dilution",        HCx == 5)
rs_sed_tissue_hc5 <- dplyr::filter(risk_summary_sed, ERM == "Tissue Translocation", HCx == 5)

sed_ecdf_overlap_plot <- plot_ecdf_overlap(
  eed_vals     = C_sample_median_sed$C_corr_med,
  haz_df       = haz_sed,
  matrix_label = "Sediment",
  n_boot       = n_boot
)

ggsave(sed_ecdf_overlap_plot, filename = "figures/sed_ecdf_overlap_plot.png", width = 10, height = 6, dpi = 300)
sed_ecdf_overlap_plot

# Compute W/L thickness ratio from ocean particles (same instrument as river)
ratio_tbl_ocean <- raw_particles_ocean |>
  dplyr::mutate(WL = 1 / aspect_ratio) |>
  dplyr::group_by(shape) |>
  dplyr::summarise(
    n      = dplyr::n(),
    r_med  = median(WL, na.rm = TRUE),
    r_low  = quantile(WL, 0.25, na.rm = TRUE),
    r_high = quantile(WL, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

raw_particles_ocean <- raw_particles_ocean |>
  dplyr::left_join(ratio_tbl_ocean |> dplyr::select(shape, r_med), by = "shape") |>
  dplyr::mutate(
    height_um = r_med * width_um,
    V_um3 = dplyr::case_when(
      shape == "fragment" ~ (pi / 6) * length_um * width_um * height_um,
      shape == "fiber"    ~ pi * (width_um / 2)^2 * length_um,
      TRUE ~ NA_real_
    )
  )

cat("Ocean particles: n =", nrow(raw_particles_ocean),
    "| shapes:", paste(sort(unique(raw_particles_ocean$shape)), collapse = ", "), "\n")

cpsd_fits_ocean_length <- fit_cpsd_by_shape(
  raw_particles_ocean, value_col = "length_um",
  config = list(
    fragment = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_)),
    fiber    = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_)),
    all      = list(bin_um = 5, fit_range_um = c(NA_real_, NA_real_))
  )
)
cpsd_fit_ocean_frag  <- cpsd_fits_ocean_length$fragment
cpsd_fit_ocean_fiber <- cpsd_fits_ocean_length$fiber
cpsd_fit_ocean_all   <- cpsd_fits_ocean_length$all

alpha_ocean    <- alpha_dist(mu = cpsd_fit_ocean_all$a_cpsd, sd = cpsd_fit_ocean_all$se_a_cpsd, n = n_mc)
alpha_ocean_mu <- mean(alpha_ocean)
alpha_ocean_se <- sd(alpha_ocean)

cat("Ocean All: alpha =", signif(cpsd_fit_ocean_all$a_cpsd, 2), "±",
    signif(cpsd_fit_ocean_all$se_a_cpsd, 2),
    "| LOD =", signif(cpsd_fit_ocean_all$lower_lod_used_um, 2), "–",
    signif(cpsd_fit_ocean_all$upper_lod_um, 2), "µm\n")

ocean_length_fits <- list(fragment = cpsd_fit_ocean_frag, fiber = cpsd_fit_ocean_fiber, all = cpsd_fit_ocean_all)
ocean_length_fits <- ocean_length_fits[names(MP_PALETTE)[names(MP_PALETTE) %in% names(ocean_length_fits)]]

ocean_length_cpsd_plot <- plot_cpsd_multi(ocean_length_fits, title = "Ocean water C-PSD fits by shape", attribute = "Length")
ggsave(ocean_length_cpsd_plot, filename = "figures/ocean_length_cpsd_plot.png", width = 8, height = 6, dpi = 300)
ocean_length_cpsd_plot

cpsd_fits_ocean_area <- fit_cpsd_by_shape(
  raw_particles_ocean, value_col = "area_um2",
  config = list(
    fragment = list(bin_um = 500, fit_range_um = c(2000, 50000)),
    fiber    = list(bin_um = 500, fit_range_um = c(2000, 500000)),
    all      = list(bin_um = 500, fit_range_um = c(2000, 500000))
  )
)
cpsd_fit_area_ocean_frag  <- cpsd_fits_ocean_area$fragment
cpsd_fit_area_ocean_fiber <- cpsd_fits_ocean_area$fiber
cpsd_fit_area_ocean       <- cpsd_fits_ocean_area$all

cat("Ocean All SA: alpha =", signif(cpsd_fit_area_ocean$a_cpsd, 2), "±",
    signif(cpsd_fit_area_ocean$se_a_cpsd, 2),
    "| LOD =", signif(cpsd_fit_area_ocean$lower_lod_used_um, 2), "–",
    signif(cpsd_fit_area_ocean$upper_lod_um, 2), "µm²\n")

ocean_area_fits <- list(fragment = cpsd_fit_area_ocean_frag, fiber = cpsd_fit_area_ocean_fiber, all = cpsd_fit_area_ocean)
ocean_area_cpsd_plot <- plot_cpsd_multi(ocean_area_fits, title = "Ocean water C-PSD fits by shape", attribute = "Area")
ggsave(ocean_area_cpsd_plot, filename = "figures/ocean_surfacearea_cpsd_plot.png", width = 8, height = 6, dpi = 300)
ocean_area_cpsd_plot

r_med_frag_ocean  <- ratio_tbl_ocean$r_med[ratio_tbl_ocean$shape == "fragment"]
r_med_fiber_ocean <- ratio_tbl_ocean$r_med[ratio_tbl_ocean$shape == "fiber"]
vlb_ocean <- volume_lod_bounds(cpsd_fit_ocean_frag, cpsd_fit_ocean_fiber, r_med_frag_ocean, r_med_fiber_ocean)
cat("Ocean fragment volume LOD: [", signif(vlb_ocean$fragment[1], 2), ",", signif(vlb_ocean$fragment[2], 2), "] µm³\n")
cat("Ocean fiber    volume LOD (informational only — too few particles to use as a fit constraint): [",
    signif(vlb_ocean$fiber[1], 2), ",", signif(vlb_ocean$fiber[2], 2), "] µm³\n")

# fragment and all use the same uniform vol_bin_um as river/sediment. fiber is the one
# documented exception (see note above): with only 4 particles inside its physically-
# derived window, no fixed window is viable, so it falls back to auto-detection
# (fit_range_um = NA) with a coarser bin width appropriate to its much wider absolute
# volume range (up to ~3e7 µm³, vs ~7e5 µm³ for fragment) — fine bins there would be
# almost entirely empty regardless of methodology, per the small-sample diagnosis above.
ocean_fiber_vol_bin_um <- 1e6
cpsd_fits_ocean_volume <- fit_cpsd_by_shape(
  raw_particles_ocean, value_col = "V_um3",
  config = list(
    fragment = list(bin_um = vol_bin_um,          fit_range_um = vlb_ocean$fragment),
    fiber    = list(bin_um = ocean_fiber_vol_bin_um, fit_range_um = c(NA_real_, NA_real_)),
    all      = list(bin_um = vol_bin_um,          fit_range_um = vlb_ocean$fragment)
  )
)
cpsd_fit_volume_ocean_frag  <- cpsd_fits_ocean_volume$fragment
cpsd_fit_volume_ocean_fiber <- cpsd_fits_ocean_volume$fiber
cpsd_fit_volume_ocean       <- cpsd_fits_ocean_volume$all

cat("Ocean All Volume: alpha =", signif(cpsd_fit_volume_ocean$a_cpsd, 2), "±",
    signif(cpsd_fit_volume_ocean$se_a_cpsd, 2),
    "| LOD =", signif(cpsd_fit_volume_ocean$lower_lod_used_um, 2), "–",
    signif(cpsd_fit_volume_ocean$upper_lod_um, 2), "µm³\n")

ocean_volume_fits <- list(fragment = cpsd_fit_volume_ocean_frag, fiber = cpsd_fit_volume_ocean_fiber, all = cpsd_fit_volume_ocean)
ocean_volume_cpsd_plot <- plot_cpsd_multi(ocean_volume_fits, title = "Ocean water C-PSD fits by shape", attribute = "Volume")
ggsave(ocean_volume_cpsd_plot, filename = "figures/ocean_volume_cpsd_plot.png", width = 8, height = 6, dpi = 300)
ocean_volume_cpsd_plot

monitoring_ocean <- readRDS("data_input/Part_dets_summ.rds") |>
  dplyr::filter(
    sample_type    == "ocean water",
    material_simple == "plastic",
    sample_or_blank == "sample"
  ) |>
  dplyr::mutate(
    date_raw      = stringr::str_extract(Client_ID_MSSupdate, "\\d{8}"),
    date          = lubridate::ymd(date_raw),
    sample_id     = Client_ID_MSSupdate,
    C_measured_pL = extrap_conc_PPL,
    Lmin_measured_um = 50,
    Lmax_measured_um = 500
  )

cat("Ocean monitoring samples:", nrow(monitoring_ocean), "\n")

ocean_corr <- correct_and_bootstrap_eed(
  monitoring  = monitoring_ocean,
  conc_col    = "C_measured_pL",
  cpsd_fit    = cpsd_fit_ocean_all,
  alpha_draws = alpha_ocean,
  L_tar_min   = 1,
  L_tar_max   = 5000,
  n_draws     = 3000,
  n_boot      = n_boot,
  probs       = c(0.5, 0.95)
)
monitoring_ocean        <- ocean_corr$monitoring
L_meas_min_ocean        <- ocean_corr$L_meas_min_use
L_meas_max_ocean        <- ocean_corr$L_meas_max_use
combined_cf_ocean       <- ocean_corr$combined_cf
C_corrected_draws_ocean <- ocean_corr$C_corrected_draws
C_sample_median_ocean   <- ocean_corr$C_sample_median
eed_boot_ocean           <- ocean_corr$eed_boot

cat("Ocean CF: median =", round(median(combined_cf_ocean), 1), "× (90% CI",
    round(quantile(combined_cf_ocean, 0.05), 1), "–",
    round(quantile(combined_cf_ocean, 0.95), 1), "×)\n")

summary(eed_boot_ocean)

ocean_eed_bootstrap_plot <- plot_eed_bootstrap(eed_boot_ocean, n_boot, "Ocean water")

ggsave(ocean_eed_bootstrap_plot, filename = "figures/ocean_eed_bootstrap_plot.png", width = 9, height = 6, dpi = 300)
ocean_eed_bootstrap_plot

tox_data_ocean <- tomex2 |>
  dplyr::filter(
    env_f            == "Marine",
    Group            != "Bacterium",
    Group            != "Plant",
    effect.metric    != "HONEC",
    tier_zero_tech_f == "Red Criteria Passed",
    tier_zero_risk_f == "Red Criteria Passed",
    risk.13          != 0
  )

cat("Marine tox endpoints:", nrow(tox_data_ocean), "records,",
    length(unique(tox_data_ocean$Group)), "taxonomic groups\n")

R.ave_ocean_result <- bootstrap_aspect_ratio(raw_particles_ocean$aspect_ratio, n_boot = n_boot)
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
# a.m.marine: use volume slope as proxy (mass ∝ density × volume; constant density → same slope)
param_values_ocean[["a.m.marine"]]            <- -cpsd_fit_volume_ocean$a_cpsd
param_values_ocean[["a.m.marine.sd"]]         <- cpsd_fit_volume_ocean$se_a_cpsd
# Keep all remaining params from param_default_values (same pattern as river section)
# a.ssa.marine, p.ave.marine etc. retain their validated defaults
param_values_ocean

param_matrix_ocean <- matrix_function(
  n    = n_boot,
  params = param_values_ocean,
  upper.tissue.truncation.limit = 500,
  x1M_set = 1,
  x2D_set = 5000,
  include_marine_surface_water     = TRUE,
  include_freshwater_surface_water = FALSE,
  include_marine_sediment          = FALSE,
  include_freshwater_sediment      = FALSE
)

ocean_R_col  <- rlang::sym("R.ave.water.marine")
ocean_HW_col <- rlang::sym("H_W_ratio.water.marine")

param_matrix_ocean <- param_matrix_ocean |>
  dplyr::mutate(
    !!ocean_R_col  := sample(R.ave_ocean_vals, size = dplyr::n(), replace = TRUE),
    !!ocean_HW_col := sample(R.ave_ocean_vals, size = dplyr::n(), replace = TRUE)
  )

tryCatch(
  {
    alpha_combined_plot_ocean <- parameter_histograms_function(param_matrix_ocean, compartments = c("Marine Surface Water"))$alpha_combined_plot
    alpha_combined_plot_ocean <- label_matrix(alpha_combined_plot_ocean, "Ocean water")
    ggsave(alpha_combined_plot_ocean, filename = "figures/alpha_combined_plot_ocean.png", width = 9, height = 6, dpi = 300)
    alpha_combined_plot_ocean
  },
  error = function(e) message("Parameter histogram skipped: ", conditionMessage(e))
)

# run_pssd_pipeline() (sourced from R/mp_risk_utils.R) bundles MC_sim_align_parallel(),
# the ERM-specific results_df filtering, erm_registry construction, and make_all_pSSDs()
# into one step used identically across river/sediment/ocean.
ocean_pssd <- run_pssd_pipeline(
  tox_data     = tox_data_ocean,
  param_matrix = param_matrix_ocean,
  environments = c("Marine"),
  cache_suffix = "ocean",
  dose_unit    = "L",
  n_sim        = n_boot,
  num_cores    = parallel::detectCores() - 2,
  sim          = 30,
  cv_uf        = 0.5,
  rmore_method = "lognormal"
)
MC_sim_df_ocean    <- ocean_pssd$MC_sim_df
erm_registry_ocean <- ocean_pssd$erm_registry

pSSDs_ocean <- ocean_pssd$pSSDs

# Uses ggrepel internally — save once, display the rendered PNG (see river pSSD_plot note above).
pSSD_plot_ocean <- pSSDs_ocean$`Tier3_Marine_Food Dilution`$pSSD_plot
pSSD_plot_ocean <- label_matrix(pSSD_plot_ocean, "Ocean water")

ggsave(pSSD_plot_ocean, filename = "figures/pSSD_plot_ocean.png", width = 9, height = 6, dpi = 300)
knitr::include_graphics("figures/pSSD_plot_ocean.png")

haz_ocean <- build_haz_df(pSSDs_ocean, "Marine")

ocean_hazard_threshold_plot <- plot_hazard_threshold(haz_ocean, "Ocean water")

ggsave(ocean_hazard_threshold_plot, filename = "figures/ocean_hazard_threshold_plot.png", width = 9, height = 6, dpi = 300)
ocean_hazard_threshold_plot

# 1D Monte Carlo risk quotient — added for parity with river/sediment (both already have
# this step); ocean previously had only the 2D MC2D characterization below.
n_risk_ocean      <- 1000
risk_draws_ocean  <- draw_rq_mc1d(haz_ocean, eed_boot_ocean$q50, n_risk = n_risk_ocean)
risk_summary_ocean <- summarize_rq(risk_draws_ocean)
risk_summary_ocean

ocean_rq_plot_1d <- plot_rq_hist(risk_draws_ocean, "Ocean water")

ggsave(ocean_rq_plot_1d, filename = "figures/ocean_rq_plot_1d.png", width = 9, height = 6, dpi = 300)
ocean_rq_plot_1d

rs_ocean_1d_food_hc5   <- dplyr::filter(risk_summary_ocean, ERM == "Food Dilution",        HCx == 5)
rs_ocean_1d_tissue_hc5 <- dplyr::filter(risk_summary_ocean, ERM == "Tissue Translocation", HCx == 5)

ocean_ecdf_overlap_plot <- plot_ecdf_overlap(
  eed_vals     = C_sample_median_ocean$C_corr_med,
  haz_df       = haz_ocean,
  matrix_label = "Ocean water",
  n_boot       = n_boot
)

ggsave(ocean_ecdf_overlap_plot, filename = "figures/ocean_ecdf_overlap_plot.png", width = 10, height = 6, dpi = 300)
ocean_ecdf_overlap_plot

mc2d_ocean <- mc2d_risk(
  monitoring_df = monitoring_ocean,
  combined_cf   = combined_cf_ocean,
  haz_df        = haz_ocean,
  n_uncertainty = 300,
  n_variability = 1000,
  seed          = 1
)

mc2d_ocean_summary <- mc2d_ocean |>
  dplyr::group_by(ERM, HCx) |>
  dplyr::summarise(
    P_exceed_med = median(P_exceed),
    P_exceed_p95 = quantile(P_exceed, 0.95),
    P_exceed_p05 = quantile(P_exceed, 0.05),
    RQ_p50_med   = median(RQ_p50),
    RQ_p95_med   = median(RQ_p95),
    .groups = "drop"
  )
mc2d_ocean_summary

ocean_mc2d_pexceed_hist_plot <- plot_mc2d_diagnostic(mc2d_ocean, "Ocean water", type = "pexceed_hist")

ggsave(ocean_mc2d_pexceed_hist_plot, filename = "figures/ocean_mc2d_pexceed_hist_plot.png", width = 9, height = 6, dpi = 300)
ocean_mc2d_pexceed_hist_plot

rs_ocean_food_hc5   <- dplyr::filter(mc2d_ocean_summary, ERM == "Food Dilution",        HCx == 5)
rs_ocean_tissue_hc5 <- dplyr::filter(mc2d_ocean_summary, ERM == "Tissue Translocation", HCx == 5)

rq_comparison_all_matrices_plot <- plot_rq_hist_combined(list(
  "River water" = risk_draws,
  "Sediment"    = risk_draws_sed,
  "Ocean water" = risk_draws_ocean
))

ggsave(rq_comparison_all_matrices_plot, filename = "figures/rq_comparison_all_matrices.png",
       width = 10, height = 9, dpi = 300)
rq_comparison_all_matrices_plot

knitr::kable(
  dplyr::bind_rows(
    tibble::tibble(
      Matrix  = "River water",
      Shape   = "All",
      alpha_cpsd = round(cpsd_fit_all$a_cpsd, 3),
      se         = round(cpsd_fit_all$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_all$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_all$upper_lod_um),
      R2         = round(cpsd_fit_all$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "River water",
      Shape   = "Fragment",
      alpha_cpsd = round(cpsd_fit_frag$a_cpsd, 3),
      se         = round(cpsd_fit_frag$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_frag$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_frag$upper_lod_um),
      R2         = round(cpsd_fit_frag$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "River water",
      Shape   = "Fiber",
      alpha_cpsd = round(cpsd_fit_fiber$a_cpsd, 3),
      se         = round(cpsd_fit_fiber$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_fiber$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_fiber$upper_lod_um),
      R2         = round(cpsd_fit_fiber$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "Sediment",
      Shape   = "All",
      alpha_cpsd = round(cpsd_fit_sed_all$a_cpsd, 3),
      se         = round(cpsd_fit_sed_all$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_sed_all$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_sed_all$upper_lod_um),
      R2         = round(cpsd_fit_sed_all$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "Sediment",
      Shape   = "Fragment",
      alpha_cpsd = round(cpsd_fit_sed_frag$a_cpsd, 3),
      se         = round(cpsd_fit_sed_frag$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_sed_frag$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_sed_frag$upper_lod_um),
      R2         = round(cpsd_fit_sed_frag$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "Sediment",
      Shape   = "Fiber",
      alpha_cpsd = round(cpsd_fit_sed_fiber$a_cpsd, 3),
      se         = round(cpsd_fit_sed_fiber$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_sed_fiber$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_sed_fiber$upper_lod_um),
      R2         = round(cpsd_fit_sed_fiber$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "Ocean water",
      Shape   = "All",
      alpha_cpsd = round(cpsd_fit_ocean_all$a_cpsd, 3),
      se         = round(cpsd_fit_ocean_all$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_ocean_all$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_ocean_all$upper_lod_um),
      R2         = round(cpsd_fit_ocean_all$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "Ocean water",
      Shape   = "Fragment",
      alpha_cpsd = round(cpsd_fit_ocean_frag$a_cpsd, 3),
      se         = round(cpsd_fit_ocean_frag$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_ocean_frag$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_ocean_frag$upper_lod_um),
      R2         = round(cpsd_fit_ocean_frag$r2, 3)
    ),
    tibble::tibble(
      Matrix  = "Ocean water",
      Shape   = "Fiber",
      alpha_cpsd = round(cpsd_fit_ocean_fiber$a_cpsd, 3),
      se         = round(cpsd_fit_ocean_fiber$se_a_cpsd, 3),
      LOD_low_um = round(cpsd_fit_ocean_fiber$lower_lod_used_um),
      LOD_hi_um  = round(cpsd_fit_ocean_fiber$upper_lod_um),
      R2         = round(cpsd_fit_ocean_fiber$r2, 3)
    )
  ),
  caption = "C-PSD power-law fits (Segur et al. 2026 algorithm). alpha_cpsd is the C-PSD slope; a_psd = alpha_cpsd − 1 is the differential BN-PSD slope used in PSSDplusplus.",
  col.names = c("Matrix", "Shape", "α_CPSD", "SE", "LOD low (µm)", "LOD high (µm)", "R²")
)

sessionInfo()
