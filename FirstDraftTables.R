# Prelim Tables for paper


library(dplyr)
library(readr)
library(openxlsx)

# Set river and depth order
river_order <- c("San Lorenzo", "Pajaro", "Salinas", "Carmel")
depth_order <- c("surface", "subsurface")

# ---------------------------
# Small MPs summary----
# ---------------------------



# Overall, not by sample location, depth, etc.

Overall_summary_table <- Part_dets_summ %>%
  filter(sample_type %in% c("river water", "lab blank", "field blank")) %>% 
  mutate(
    sample_type = str_trim(tolower(sample_type)),
    material_simple = str_trim(tolower(material_simple))
  ) %>%
  
  # Sum extrapolated counts per sample first
  group_by(sample_type, material_simple, Client_ID_MSSupdate) %>%
  summarize(
    total_extrap_count = sum(extrap_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Summary statistics across samples
  group_by(sample_type, material_simple) %>%
  summarize(
    median_count = median(total_extrap_count, na.rm = TRUE),
    MAD_count    = mad(total_extrap_count, constant = 1, na.rm = TRUE),
    p25_count    = quantile(total_extrap_count, 0.25, na.rm = TRUE),
    p75_count    = quantile(total_extrap_count, 0.75, na.rm = TRUE),
    n_samples    = n_distinct(Client_ID_MSSupdate),
    .groups = "drop"
  ) %>%
  arrange(sample_type, material_simple)

Overall_summary_table





# ---- Readable summary table for Word / screenshot ----


Overall_summary_table <- Part_dets_summ %>%
  filter(sample_type %in% c("river water", "lab blank", "field blank")) %>% 
  mutate(
    sample_type = str_trim(tolower(sample_type)),
    material_simple = str_trim(tolower(material_simple))
  ) %>%
  
  # Sum extrapolated counts per sample first
  group_by(sample_type, material_simple, Client_ID_MSSupdate) %>%
  summarize(
    total_extrap_count = sum(extrap_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Summary statistics across samples
  group_by(sample_type, material_simple) %>%
  summarize(
    median = median(total_extrap_count, na.rm = TRUE),
    p25    = quantile(total_extrap_count, 0.25, na.rm = TRUE),
    p75    = quantile(total_extrap_count, 0.75, na.rm = TRUE),
    n      = n_distinct(Client_ID_MSSupdate),
    .groups = "drop"
  ) %>%
  
  # Combine into a single readable column
  mutate(
    `Median (25th–75th)` = paste0(
      round(median, 1), " (",
      round(p25, 1), "–",
      round(p75, 1), ")"
    )
  ) %>%
  
  select(
    Sample_type = sample_type,
    Material    = material_simple,
    `Median (25th–75th)`,
    `n samples` = n
  ) %>%
  arrange(Sample_type, Material)

# Print clean table for RStudio viewer / screenshot
kable(
  Overall_summary_table,
  align = c("l", "l", "c", "c"),
  caption = "Summary of extrapolated microplastic counts by sample type and material. Values shown as median (25th–75th percentile)."
)





small_mp_summary <- Part_dets_summ_river2 %>%
  filter(material_simple == "plastic") %>% 
  mutate(
    river = factor(river, levels = river_order),
    sample_depth_general = tolower(sample_depth_general),
    `Sample depth` = factor(sample_depth_general, levels = depth_order)
  ) %>%
  group_by(river, `Sample depth`) %>%
  summarise(
    median_val = median(particles_per_L, na.rm = TRUE),
    p25 = quantile(particles_per_L, 0.25, na.rm = TRUE),
    p75 = quantile(particles_per_L, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    `MPs per L (50–500 µm)` = sprintf("%.3f (%.3f–%.3f)", median_val, p25, p75)
  ) %>%
  select(river, `Sample depth`, `MPs per L (50–500 µm)`, n)











# ---------------------------
# Large MPs summary----
# ---------------------------
large_mp_summary <- river_MPs_summ %>%
  mutate(
    river = factor(sample_location, levels = river_order),
    sample_depth_general = ifelse(substr(Sample_ID, nchar(Sample_ID), nchar(Sample_ID)) == "S",
                                  "surface", "subsurface"),
    `Sample depth` = factor(sample_depth_general, levels = depth_order)
  ) %>%
  group_by(river, `Sample depth`) %>%
  summarise(
    median_val = median(MPs_L, na.rm = TRUE),
    p25 = quantile(MPs_L, 0.25, na.rm = TRUE),
    p75 = quantile(MPs_L, 0.75, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    `MPs per L (500–5000 µm)` = sprintf("%.3f (%.3f–%.3f)", median_val, p25, p75)
  ) %>%
  select(river, `Sample depth`, `MPs per L (500–5000 µm)`, n)

# ---------------------------
# Combine small + large----
# ---------------------------
combined_summary <- full_join(
  small_mp_summary,
  large_mp_summary,
  by = c("river", "Sample depth"),
  suffix = c("_small", "_large")
)

# ---------------------------
# Export to CSV / XLSX----
# ---------------------------
write_csv(combined_summary, "MPs_summary_table.csv")
write.xlsx(combined_summary, "MPs_summary_table.xlsx", overwrite = TRUE)

combined_summary








# ---------------------------
# Small MPs summary (by river only)
# ---------------------------
small_mp_summary_river_only <- Part_dets_summ_river2_sum %>%
  mutate(
    river = factor(river, levels = river_order)
  ) %>%
  group_by(river) %>%
  summarise(
    median_val = median(particles_per_L, na.rm = TRUE),
    p25 = quantile(particles_per_L, 0.25, na.rm = TRUE),
    p75 = quantile(particles_per_L, 0.75, na.rm = TRUE),
    n_small = n(),
    .groups = "drop"
  ) %>%
  mutate(
    `MPs per L (50–500 µm)` =
      sprintf("%.3f (%.3f–%.3f)", median_val, p25, p75)
  ) %>%
  select(river, `MPs per L (50–500 µm)`, n_small)

View(small_mp_summary_river_only)



small_mp_summary_yr1_only <- Part_dets_summ_river2_sum %>%
  filter(date < ymd("2024-05-30")) %>%   # <-- Year 1 sampling only for final fig
  mutate(
    river = factor(river, levels = river_order)
  ) %>%
  group_by(river) %>%
  summarise(
    median_val = median(particles_per_L, na.rm = TRUE),
    p25 = quantile(particles_per_L, 0.25, na.rm = TRUE),
    p75 = quantile(particles_per_L, 0.75, na.rm = TRUE),
    n_small = n(),
    .groups = "drop"
  ) %>%
  mutate(
    `MPs per L (50–500 µm)` =
      sprintf("%.3f (%.3f–%.3f)", median_val, p25, p75)
  ) %>%
  select(river, `MPs per L (50–500 µm)`, n_small)

View(small_mp_summary_yr1_only)








# Table of polymer types ----

library(dplyr)
library(readr)
library(openxlsx)

# ---------------------------
# Summarize plastic counts by material_class
# ---------------------------
plastic_material_summary <- Part_dets_summ %>%
  filter(sample_type == "river water", material_simple == "plastic") %>%
  group_by(material_class) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_count)) %>%
  mutate(
    percent_of_total = total_count / sum(total_count) * 100
  ) %>%
  # Round for presentation
  mutate(
    total_count = round(total_count, 0),
    percent_of_total = round(percent_of_total, 1)
  ) %>%
  # Rename columns for paper/presentation
  rename(
    `Polymer type` = material_class,
    `Total count` = total_count,
    `Percent of total` = percent_of_total
  )

# ---------------------------
# Export table
# ---------------------------
write_csv(plastic_material_summary, "plastic_material_summary.csv")
write.xlsx(plastic_material_summary, "plastic_material_summary.xlsx", overwrite = TRUE)

sum(plastic_material_summary$`Total count`)

