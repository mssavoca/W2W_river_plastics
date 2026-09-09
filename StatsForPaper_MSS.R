
#Stats for Fig. 3----

# Create plastic-only dataset and exclude specified blanks
plastic_stats <- Part_dets_summ_river2 %>%
  filter(
    material_simple == "plastic",
    !Client_ID_MSSupdate %in% c(
      "CRR20240124_WaterBlank",
      "SRR20250403_blank"
    )
  ) %>%
  mutate(
    river = factor(river),
    river = relevel(
      river,
      ref = "San Lorenzo"
    )
  )

#linear model
drivers_of_MP_conc <- lm(
  log10(particles_per_L) ~ river + sampling_season + sample_depth_general,
  data = plastic_stats
)

# Overall effects of river and sampling season
car::Anova(
  drivers_of_MP_conc,
  type = 2
)


# All pairwise river comparisons
library(emmeans)

emmeans(
  drivers_of_MP_conc,
  pairwise ~ river,
  adjust = "tukey"
)

# Model coefficients
summary(drivers_of_MP_conc)


# Diagnostics
par(mfrow = c(1, 2))
plot(drivers_of_MP_conc, which = 1)
plot(drivers_of_MP_conc, which = 2)
par(mfrow = c(1, 1))



#Stats for Fig. 4----


library(dplyr)
library(tidyr)

plastic_comp <- Part_dets_summ %>%
  filter(
    material_simple == "plastic",
    sample_type %in% c(
      "river water",
      "field blank",
      "lab blank"
    )
  )


polymer_by_sample <- plastic_comp %>%
  group_by(
    Client_ID_MSSupdate,
    sample_type,
    material_class
  ) %>%
  summarise(
    polymer_count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    Client_ID_MSSupdate,
    sample_type
  ) %>%
  mutate(
    total_plastic = sum(polymer_count),
    polymer_prop = polymer_count / total_plastic
  ) %>%
  ungroup()

polymer_by_sample <- polymer_by_sample %>%
  complete(
    Client_ID_MSSupdate,
    material_class,
    fill = list(polymer_count = 0)
  ) %>%
  group_by(Client_ID_MSSupdate) %>%
  fill(sample_type, .direction = "downup") %>%
  mutate(
    total_plastic = sum(polymer_count),
    polymer_prop = polymer_count / total_plastic
  ) %>%
  ungroup()


sample_info <- plastic_comp %>%
  distinct(
    Client_ID_MSSupdate,
    sample_type
  )

polymer_levels <- plastic_comp %>%
  distinct(material_class)

polymer_by_sample <- sample_info %>%
  crossing(polymer_levels) %>%
  left_join(
    plastic_comp %>%
      group_by(
        Client_ID_MSSupdate,
        sample_type,
        material_class
      ) %>%
      summarise(
        polymer_count = sum(count, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c(
      "Client_ID_MSSupdate",
      "sample_type",
      "material_class"
    )
  ) %>%
  mutate(
    polymer_count = replace_na(polymer_count, 0)
  ) %>%
  group_by(
    Client_ID_MSSupdate,
    sample_type
  ) %>%
  mutate(
    total_plastic = sum(polymer_count),
    polymer_prop = polymer_count / total_plastic
  ) %>%
  ungroup()


polymer_comp_summary <- polymer_by_sample %>%
  group_by(
    sample_type,
    material_class
  ) %>%
  summarise(
    mean_prop = mean(polymer_prop, na.rm = TRUE),
    median_prop = median(polymer_prop, na.rm = TRUE),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  mutate(
    mean_percent = 100 * mean_prop,
    median_percent = 100 * median_prop
  ) %>%
  arrange(
    sample_type,
    desc(mean_percent)
  )

polymer_comp_summary





library(vegan)

polymer_wide <- polymer_by_sample %>%
  select(
    Client_ID_MSSupdate,
    sample_type,
    material_class,
    polymer_prop
  ) %>%
  pivot_wider(
    names_from = material_class,
    values_from = polymer_prop,
    values_fill = 0
  )

metadata <- polymer_wide %>%
  select(
    Client_ID_MSSupdate,
    sample_type
  )

polymer_matrix <- polymer_wide %>%
  select(
    -Client_ID_MSSupdate,
    -sample_type
  )

permanova_polymer <- adonis2(
  polymer_matrix ~ sample_type,
  data = metadata,
  method = "bray",
  permutations = 9999
)

permanova_polymer



polymer_dist <- vegdist(
  polymer_matrix,
  method = "bray"
)

polymer_disp <- betadisper(
  polymer_dist,
  metadata$sample_type
)

anova(polymer_disp)
permutest(
  polymer_disp,
  permutations = 9999
)


library(dplyr)
library(tidyr)
library(purrr)

set.seed(123)

# ------------------------------------------------------------
# polymer_by_sample should have:
# Client_ID_MSSupdate
# sample_type
# material_class
# polymer_prop
#
# including zeros for polymers absent from a sample
# ------------------------------------------------------------


# Function to bootstrap whole samples within one sample type
bootstrap_polymer_composition <- function(dat, B = 10000) {
  
  sample_ids <- unique(dat$Client_ID_MSSupdate)
  
  # Observed mean polymer percentages
  observed <- dat %>%
    group_by(material_class) %>%
    summarise(
      mean_prop = mean(polymer_prop),
      .groups = "drop"
    )
  
  # Bootstrap whole samples
  boot_results <- map_dfr(
    1:B,
    function(i) {
      
      sampled_ids <- sample(
        sample_ids,
        size = length(sample_ids),
        replace = TRUE
      )
      
      # Important: preserve duplicated bootstrap samples
      boot_dat <- map_dfr(
        seq_along(sampled_ids),
        function(j) {
          
          dat %>%
            filter(
              Client_ID_MSSupdate == sampled_ids[j]
            ) %>%
            mutate(
              bootstrap_sample = j
            )
        }
      )
      
      boot_dat %>%
        group_by(material_class) %>%
        summarise(
          boot_mean = mean(polymer_prop),
          .groups = "drop"
        ) %>%
        mutate(iteration = i)
    }
  )
  
  # Bootstrap 95% CIs
  boot_ci <- boot_results %>%
    group_by(material_class) %>%
    summarise(
      ci_low = quantile(
        boot_mean,
        0.025
      ),
      ci_high = quantile(
        boot_mean,
        0.975
      ),
      .groups = "drop"
    )
  
  observed %>%
    left_join(
      boot_ci,
      by = "material_class"
    )
}


# Run independently for each sample type
polymer_boot <- polymer_by_sample %>%
  group_split(sample_type) %>%
  map_dfr(
    function(dat) {
      
      type <- unique(dat$sample_type)
      
      bootstrap_polymer_composition(
        dat,
        B = 10000
      ) %>%
        mutate(sample_type = type)
    }
  ) %>%
  mutate(
    mean_percent = 100 * mean_prop,
    ci_low_percent = 100 * ci_low,
    ci_high_percent = 100 * ci_high
  )


polymer_boot_table <- polymer_boot %>%
  select(
    material_class,
    sample_type,
    mean_percent,
    ci_low_percent,
    ci_high_percent
  ) %>%
  mutate(
    composition = sprintf(
      "%.1f%% (%.1f–%.1f%%)",
      mean_percent,
      ci_low_percent,
      ci_high_percent
    )
  ) %>%
  select(
    material_class,
    sample_type,
    composition,
    mean_percent
  ) %>%
  pivot_wider(
    names_from = sample_type,
    values_from = c(composition, mean_percent),
    names_sep = "_"
  ) %>%
  arrange(desc(`mean_percent_river water`)) %>%
  transmute(
    material_class,
    `River water` = `composition_river water`,
    `Field blank` = `composition_field blank`,
    `Lab blank` = `composition_lab blank`
  )

View(polymer_boot_table)








library(dplyr)
library(tidyr)
library(purrr)

set.seed(123)

# ------------------------------------------------------------
# Function: bootstrap difference in mean polymer proportions
# between TWO sample types
# ------------------------------------------------------------

bootstrap_polymer_difference <- function(
    dat,
    group1,
    group2,
    B = 10000
) {
  
  # Get polymer-by-sample matrices for each group
  mat1 <- dat %>%
    filter(sample_type == group1) %>%
    select(
      Client_ID_MSSupdate,
      material_class,
      polymer_prop
    ) %>%
    pivot_wider(
      names_from = material_class,
      values_from = polymer_prop,
      values_fill = 0
    )
  
  mat2 <- dat %>%
    filter(sample_type == group2) %>%
    select(
      Client_ID_MSSupdate,
      material_class,
      polymer_prop
    ) %>%
    pivot_wider(
      names_from = material_class,
      values_from = polymer_prop,
      values_fill = 0
    )
  
  # Save sample IDs, then convert polymer columns to matrices
  x1 <- as.matrix(
    mat1 %>%
      select(-Client_ID_MSSupdate)
  )
  
  x2 <- as.matrix(
    mat2 %>%
      select(-Client_ID_MSSupdate)
  )
  
  polymers <- colnames(x1)
  
  # Make sure polymer columns occur in identical order
  x2 <- x2[, polymers, drop = FALSE]
  
  
  # ----------------------------------------------------------
  # Observed difference in mean proportion
  # group1 - group2
  # ----------------------------------------------------------
  
  observed_diff <- colMeans(x1) - colMeans(x2)
  
  
  # ----------------------------------------------------------
  # Bootstrap WHOLE SAMPLES within each group
  # ----------------------------------------------------------
  
  boot_diff <- replicate(
    B,
    {
      
      i1 <- sample(
        seq_len(nrow(x1)),
        size = nrow(x1),
        replace = TRUE
      )
      
      i2 <- sample(
        seq_len(nrow(x2)),
        size = nrow(x2),
        replace = TRUE
      )
      
      colMeans(x1[i1, , drop = FALSE]) -
        colMeans(x2[i2, , drop = FALSE])
    }
  )
  
  
  # ----------------------------------------------------------
  # Summarize each polymer
  # ----------------------------------------------------------
  
  results <- map_dfr(
    seq_along(polymers),
    function(j) {
      
      d <- boot_diff[j, ]
      
      # Two-sided bootstrap-tail probability
      p_boot <- 2 * min(
        mean(d <= 0),
        mean(d >= 0)
      )
      
      # Avoid reporting p = 0 with finite bootstrap iterations
      p_boot <- max(
        p_boot,
        1 / B
      )
      
      tibble(
        material_class = polymers[j],
        
        mean_difference =
          observed_diff[j] * 100,
        
        ci_low =
          quantile(d, 0.025) * 100,
        
        ci_high =
          quantile(d, 0.975) * 100,
        
        p_boot = p_boot
      )
    }
  )
  
  results %>%
    mutate(
      comparison = paste(
        group1,
        "vs",
        group2
      )
    )
}


# River water vs field blanks
river_vs_field <- bootstrap_polymer_difference(
  polymer_by_sample,
  group1 = "river water",
  group2 = "field blank",
  B = 10000
)

# River water vs laboratory blanks
river_vs_lab <- bootstrap_polymer_difference(
  polymer_by_sample,
  group1 = "river water",
  group2 = "lab blank",
  B = 10000
)




river_vs_field <- river_vs_field %>%
  mutate(
    p_adj = p.adjust(
      p_boot,
      method = "BH"
    )
  )

river_vs_lab <- river_vs_lab %>%
  mutate(
    p_adj = p.adjust(
      p_boot,
      method = "BH"
    )
  )


polymer_difference_results <- bind_rows(
  river_vs_field,
  river_vs_lab
)




polymer_difference_table <- polymer_difference_results %>%
  mutate(
    
    difference_CI = sprintf(
      "%.1f%% (%.1f to %.1f%%)",
      mean_difference,
      ci_low,
      ci_high
    ),
    
    significant = if_else(
      p_adj < 0.05,
      "YES",
      "no"
    ),
    
    direction = case_when(
      p_adj < 0.05 &
        mean_difference > 0 ~
        "Higher in river water",
      
      p_adj < 0.05 &
        mean_difference < 0 ~
        "Lower in river water",
      
      TRUE ~
        "No significant difference"
    )
  ) %>%
  select(
    material_class,
    comparison,
    difference_CI,
    p_boot,
    p_adj,
    significant,
    direction
  ) %>%
  arrange(
    comparison,
    p_adj
  )

View(polymer_difference_table)

#Stats for Fig.6----
### PSD extrapolation: 50–500 µm -> 1–5000 µm ----

# Empirically measured concentration in the
# operational 50–500 µm size fraction
C_measured <- 0.33  # particles/L


# ------------------------------------------------------------
# Fitted pooled river-water PSD
# ------------------------------------------------------------

# Fitted cumulative C-PSD slope
a_cpsd <- -1.920

# Standard error of fitted slope
a_cpsd_se <- 0.035

# Convert cumulative slope to differential PSD slope
# following Segur et al. (2026):
# a_differential = a_cpsd - 1

a <- a_cpsd - 1

a
# -2.920


# ------------------------------------------------------------
# 95% CI for differential slope
# ------------------------------------------------------------

a_lower <- a - 1.96 * a_cpsd_se
a_upper <- a + 1.96 * a_cpsd_se

a_lower
a_upper


# ------------------------------------------------------------
# Function to integrate the differential power law
#
# dN/dL = k * L^a
#
# k is omitted because it cancels when calculating
# the correction factor.
# ------------------------------------------------------------

powerlaw_integral <- function(L1, L2, a) {
  
  (L2^(a + 1) - L1^(a + 1)) /
    (a + 1)
  
}


# ------------------------------------------------------------
# Function to calculate correction factor
#
# target range   = 1–5000 µm
# measured range = 50–500 µm
# ------------------------------------------------------------

get_CF <- function(a) {
  
  N_target <- powerlaw_integral(
    L1 = 1,
    L2 = 5000,
    a = a
  )
  
  N_measured <- powerlaw_integral(
    L1 = 50,
    L2 = 500,
    a = a
  )
  
  N_target / N_measured
}


# ------------------------------------------------------------
# Calculate correction factors
# ------------------------------------------------------------

CF_best <- get_CF(a)

# Shallower slope -> fewer extrapolated small particles
CF_low <- get_CF(a_upper)

# Steeper slope -> more extrapolated small particles
CF_high <- get_CF(a_lower)

CF_best
CF_low
CF_high


# ------------------------------------------------------------
# Apply correction factors to empirical concentration
# ------------------------------------------------------------

C_best <- C_measured * CF_best
C_low  <- C_measured * CF_low
C_high <- C_measured * CF_high


# ------------------------------------------------------------
# Final results
# ------------------------------------------------------------

PSD_concentration_estimate <- data.frame(
  
  empirical_50_500_MP_L = C_measured,
  
  differential_slope = a,
  
  slope_lower_95 = a_lower,
  slope_upper_95 = a_upper,
  
  correction_factor = CF_best,
  correction_factor_low = CF_low,
  correction_factor_high = CF_high,
  
  estimated_1_5000_MP_L = C_best,
  estimated_1_5000_low_95 = C_low,
  estimated_1_5000_high_95 = C_high
)

PSD_concentration_estimate


