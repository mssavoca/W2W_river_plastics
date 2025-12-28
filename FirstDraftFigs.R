# Figure drafts ----


library(dplyr)
library(lubridate)
library(patchwork)





summary_table_smallMPs <- Part_dets_summ_river %>%
  group_by(river, date) %>%
  summarise(
    n_unique_Client_ID_MSSupdate = n_distinct(Client_ID_MSSupdate),
    .groups = "drop"
  ) %>%
  arrange(date)

#View(summary_table_smallMPs)



summary_table_largeMPs <- river_MPs_summ %>%
  group_by(sample_location, date) %>%
  summarise(
    n_unique_Sample_ID = n_distinct(Sample_ID),
    .groups = "drop"
  ) %>%
  arrange(date)

#View(summary_table_largeMPs)


# Small MPs over river flow data----
# Part_dets_summ_river2 <- Part_dets_summ_river %>%
#   mutate(
#     particles_per_L = ifelse(
#       Client_ID_MSSupdate == "CRR20240116SS",
#       extrap_count / 200,
#       extrap_count / 400
#     )
#   )


Part_dets_summ_river2 <- Part_dets_summ_river %>%
  group_by(Client_ID_MSSupdate, river, date, 
           material_simple, sample_depth_general) %>% 
  summarise(
    particles_per_L = sum(extrap_conc_PPL, na.rm = TRUE),
    .groups = "drop"
  )

#View(Part_dets_summ_river2)

Part_dets_summ_river2_sum <- Part_dets_summ_river %>%
  mutate(
    particles_per_L = ifelse(
      Client_ID_MSSupdate == "CRR20240116SS",
      extrap_count / 200,
      extrap_count / 400
    )
  ) %>%
  group_by(Client_ID_MSSupdate, material_simple) %>%
  summarise(
    particles_per_L = sum(particles_per_L, na.rm = TRUE),
    across(-particles_per_L, first),
    .groups = "drop"
  )



flow_scaled <- all_rivers_flow %>%
  group_by(river) %>%
  mutate(
    scale_factor =
      2 * max(                                   # 👈 increase multiplier (try 1.5–3)
        Part_dets_summ_river2_sum$particles_per_L[
          Part_dets_summ_river2_sum$river == first(river)
        ],
        na.rm = TRUE
      ) / max(Flow_m3s, na.rm = TRUE),
    
    flow_scaled = Flow_m3s * scale_factor
  ) %>%
  ungroup()



# Relevel river to control facet order
flow_scaled$river <- factor(
  flow_scaled$river,
  levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel" )
)

Part_dets_summ_river2_sum$river <- factor(
  Part_dets_summ_river2_sum$river,
  levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel")
)



p_small = ggplot() +
  geom_line(
    data = flow_scaled,
    aes(x = date, y = flow_scaled),
    color = "steelblue2",
    linewidth = 0.6,
    #linetype = "dotted"
  ) +
  
  geom_col(
    data = Part_dets_summ_river2_sum,
    aes(x = date, y = particles_per_L, fill = material_simple),
    position = "stack",
    alpha = 0.7
  ) +
  
  facet_wrap(~ river, scales = "free_y", ncol = 1) +
  
  scale_y_continuous(
    name = "Particle count per L (50–500 µm)",
    sec.axis = sec_axis(
      ~ . / unique(flow_scaled$scale_factor)[1],
            name = expression(paste("River flow (m"^3, " s"^-1, ")"))
    )
  ) +
  
  scale_x_date(
    limits = c(ymd("2023-10-01"), ymd("2025-06-01")),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  scale_fill_manual(
    name = "Material",
    values = c(
      "plastic" = "mediumorchid",
      "organic matter" = "forestgreen",
      "mineral" = "darkorange2"
    ),
    labels = c(
      "plastic" = "plastic",
      "organic matter" = "organic\nmatter",
      "mineral" = "mineral"
    )
  ) +
  
  labs(x = "Date") +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.y.right = element_text(color = "steelblue3"),
    axis.text.y.right = element_text(color = "steelblue3"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "right"
  )

p_small


# ggsave(
#   filename = "river_small particle_flow_dual_axis_vert.pdf",
#   plot = p_small,
#   device = "pdf",
#   width = 7,
#   height = 8,
#   units = "in"
# )
# 
# 
ggsave(
  filename = "river_small particle_flow_dual_axis_vert.png",
  plot = p_small,
  width = 6,
  height = 8,
  units = "in",
  dpi = 600
)




# Large MPs over river flow data----
# ------------------------------------
# Scale river flow to large MP concentrations
# ------------------------------------

flow_clean <- all_rivers_flow %>%
  group_by(river, date) %>%
  summarize(
    Flow_m3s = mean(Flow_m3s, na.rm = TRUE),
    .groups = "drop"
  )


flow_clean <- flow_clean %>%
  rename(sample_location = river)


scale_df <- river_MPs_summ %>%
  group_by(sample_location) %>%
  summarize(
    max_particles = max(MPs_L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    flow_clean %>%
      group_by(sample_location) %>%
      summarize(
        max_flow = max(Flow_m3s, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "sample_location"
  ) %>%
  mutate(scale_factor = max_particles / max_flow)



flow_scaled <- flow_clean %>%
  left_join(scale_df, by = "sample_location") %>%
  transmute(
    sample_location,
    date,
    flow_scaled = Flow_m3s * scale_factor
  )



# ------------------------------------
# Plot: Large MPs on top of river flow
# ------------------------------------

# Re-set factor levels in desired order
# river_order <- c("Carmel", "Pajaro", "Salinas", "San Lorenzo")
# river_MPs_summ$sample_location <- factor(river_MPs_summ$sample_location, levels = river_order)
# flow_scaled$sample_location <- factor(flow_scaled$sample_location, levels = river_order)
# 
# flow_scaled <- flow_scaled %>%
#   mutate(sample_location = ifelse(is.na(sample_location), "San Lorenzo", sample_location))


# Relevel river to control facet order
flow_scaled$sample_location <- factor(
  flow_scaled$sample_location,
  levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel" )
)

river_MPs_summ$sample_location <- factor(
  river_MPs_summ$sample_location,
  levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel" )
)


ref_scale <- median(scale_df$scale_factor, na.rm = TRUE)

# -----------------------------
# Large particle + flow plot
# -----------------------------
p_large_vert <- ggplot() +
  
  # River flow line
  geom_line(
    data = flow_scaled,
    aes(x = date, y = flow_scaled),
    color = "steelblue2",
    linewidth = 0.6
  ) +
  
  # Large plastic particle bars
  geom_col(
    data = river_MPs_summ,
    aes(x = date, y = MPs_L),
    fill = "orchid",
    alpha = 0.7
  ) +
  
  # Vertical facets by river in desired order
  facet_wrap(
    ~ sample_location,
    scales = "free_y",
    ncol = 1
  ) +
  
  # y-axis and secondary axis for real flow
  scale_y_continuous(
    name = "Particle count per L (500–5000 µm)",
    sec.axis = sec_axis(
      ~ . / ref_scale,
      name = expression(paste("River flow (m"^3, " s"^-1, ")"))
    )
  ) +
  
  # x-axis formatting
  scale_x_date(
    limits = c(ymd("2023-10-01"), ymd("2025-06-01")),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  labs(x = "Date") +
  
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.y.right = element_text(color = "steelblue3"),
    axis.text.y.right  = element_text(color = "steelblue3"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )


p_large_vert




# ggsave(
#   filename = "river_large particle_flow_dual_axis_vert.pdf",
#   plot = p_large_vert,
#   device = "pdf",
#   width = 5,
#   height = 8,
#   units = "in"
# )
# 
# 
ggsave(
  filename = "river_large particle_flow_dual_axis_vert.png",
  plot = p_large_vert,
  width = 5,
  height = 8,
  units = "in",
  dpi = 600
)



# boxplot by river and material_simple----
bp_river_material <- ggplot(
  Part_dets_summ_river2,
  aes(
    x = river,
    y = particles_per_L
  )
) +
  geom_boxplot(
    aes(fill = material_simple),
    outlier.shape = NA,
    color = "black",                    # keep black outlines
    linewidth = 0.6,
    position = position_dodge(width = 0.8),
    alpha = 0.7
  ) +
  geom_jitter(
    aes(color = material_simple),
    position = position_jitterdodge(
      jitter.width = 0.15,
      dodge.width  = 0.8
    ),
    alpha = 0.7,
    size = 2
  ) +
  scale_y_log10() +
  
  #facet_grid(.~sample_depth_general) +
  
  scale_fill_manual(
    name = "Material",
    values = c(
      "plastic" = "mediumorchid",
      "organic matter" = "forestgreen",
      "mineral" = "darkorange2"
    )
  ) +
  scale_color_manual(
    name = "Material",
    values = c(
      "plastic" = "mediumorchid",
      "organic matter" = "forestgreen",
      "mineral" = "darkorange2"
    )
  ) +
  labs(
    x = "River",
    y = "Particles per L (50–500 µm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "right"
  )
bp_river_material


ggsave(
  filename = "boxplot by river and material.pdf",
  plot = bp_river_material,
  device = "pdf",
  width = 7,
  height = 5,
  units = "in"
)







#River water MPs by depth, probably a supplemental ----

df_plot <- Part_dets_summ_river %>%
  group_by(material_simple, Client_ID_MSSupdate, sample_depth_general) %>%
  summarize(particles_per_L = sum(extrap_conc_PPL)) %>%
  mutate(
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("surface", "subsurface")
     )
    ) %>% 
  ungroup()


df_medians <- df_plot %>%
  group_by(sample_depth_general, material_simple) %>%
  summarize(
    median_count_L = median(particles_per_L),
    .groups = "drop"
  )



t = ggplot(df_plot, aes(x = particles_per_L, color = material_simple)) +
  geom_density(alpha = 0.4, linewidth = 0.5) +
  geom_rug(alpha = 0.8) +
  
  # ---- medians by material within each facet ----
geom_vline(
  data = df_medians,
  aes(xintercept = median_count_L, color = material_simple),
  linetype = "dashed",
  linewidth = 0.6,
  show.legend = FALSE
) +
  
  facet_wrap(
    ~ sample_depth_general,
    scales = "free",
    ncol = 1
  ) +
  scale_x_log10() +
  scale_color_manual(
    name = "Material",
    values = c(
      "plastic" = "mediumorchid",
      "organic matter" = "forestgreen",
      "mineral" = "darkorange2"
    )
  ) +
  labs(
    x = "Particle count per L (50–500 µm)",
    y = "Density"
  ) +
  theme_minimal(base_size = 16) +
    theme(
      strip.background = element_blank(),
      strip.placement = "outside",
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      
      legend.position = "top",
      legend.justification = "center",
      legend.margin = margin(t = 0, b = 0),
      legend.box.margin = margin(b = -6),
      legend.spacing.x = unit(0.3, "cm"),
      
      legend.title = element_text(size = 12.5),
      legend.text  = element_text(size = 12),
      legend.key.size = unit(0.6, "lines"),
      
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
    )

t


ggsave(
  filename = "all small particles by depth.pdf",
  plot = t,
  device = "pdf",
  width = 7,
  height = 6,
  units = "in"
)






# Filter for plastic only and summarize counts by Client_ID_MSSupdate and material_class, sample_depth_general
df_plastic <- Part_dets_summ_river %>%
  filter(material_simple == "plastic") %>%
  group_by(material_class, Client_ID_MSSupdate, sample_depth_general) %>%
  summarize(particles_per_L = sum(extrap_conc_PPL)) %>%
  mutate(
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("surface", "subsurface")
    )
  ) %>% 
  ungroup()

# Determine top 5 most common material_class by total count
top_materials <- df_plastic %>%
  group_by(material_class) %>%
  summarize(sum_total_extrap_count = sum(particles_per_L)) %>%
  arrange(desc(sum_total_extrap_count)) %>%
  slice_head(n = 5) %>%
  pull(material_class)

# Filter to top 5 material classes for plotting
df_top_plastic <- df_plastic %>%
  filter(material_class %in% top_materials & !is.na(sample_depth_general)) %>% 
  mutate(
    material_short = case_when(
      material_class == "poly(propylene)" ~ "polypropylene",
      material_class == "poly(ethylene)" ~ "polyethylene",
      material_class == "polystyrenes (polyphenylethylenes, -methylstyrene)" ~ "polystyrene",
      material_class == "poly(acrylamide/amid)s" ~ "polyacrylamide",
      material_class == "poly(esters/ethers/diglycidylethers/terephthalates)s" ~ "polyester / PET",
      TRUE ~ material_class   # fallback: keep original if not matched
    )
  )

# Reorder sample_depth_general for vertical facet stacking: surface on top
df_top_plastic <- df_top_plastic %>%
  mutate(
    sample_depth_general = recode(
      sample_depth_general,
      "depth" = "subsurface"
    ),
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("surface", "subsurface")
    )
  )



median_df <- df_top_plastic %>%
  group_by(sample_depth_general, material_short) %>%
  summarize(
    median_total_count_L = median(particles_per_L),
    .groups = "drop"
  )



# Get brewer palette and overwrite polypropylene
poly_levels <- levels(factor(df_top_plastic$material_short))
pal <- RColorBrewer::brewer.pal(
  n = max(3, length(poly_levels)),
  name = "Set1"
)
names(pal) <- poly_levels
pal["polypropylene"] <- "gray40"

u <- ggplot(df_top_plastic, aes(x = particles_per_L, color = material_short)) +
  geom_density(alpha = 0.4, linewidth = 0.5) +
  geom_rug(alpha = 0.8) +
  
  # ---- facet-specific median lines 
geom_vline(
  data = median_df,
  aes(xintercept = median_total_count_L, color = material_short),
  linetype = "dashed",
  linewidth = 0.6,
  show.legend = FALSE
) +
  
  facet_wrap(~ sample_depth_general, scales = "free", ncol = 1) +
  scale_x_log10() +
  
  # ---- Set1 palette with PP overridden 
scale_color_manual(
  name = "Polymer Type",
  values = pal
) +
  
  # ---- two-row legend
guides(
  color = guide_legend(nrow = 2, byrow = TRUE)
) +
  
  labs(
    x = "Microplastic count per L (50–500 µm)",
    y = "Density"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    
    legend.position = "top",
    legend.justification = "center",
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(b = -6),
    legend.spacing.x = unit(0.3, "cm"),
    
    legend.title = element_text(size = 12.5),
    legend.text  = element_text(size = 12),
    legend.key.size = unit(0.6, "lines"),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )


u

ggsave(
  filename = "small plastics by type and depth.pdf",
  plot = u,
  device = "pdf",
  width = 7,
  height = 6,
  units = "in"
)








# Stacked proportional plots; all rivers, and blanks----


# -------------------------------
# Proportional stacked bar plot by river: top 5 polymers + "other"
# -------------------------------

# Define top 5 polymers (short names)
top_materials <- c(
  "polypropylene",
  "polyethylene",
  "polystyrene",
  "polyacrylamide",
  "polyester / PET"
)

# Prepare plotting dataframe
df_bar <- Part_dets_summ_river %>%
  filter(material_simple == "plastic") %>%
  mutate(
    # standardize polymer names
    polymer_short = case_when(
      material_class == "poly(propylene)" ~ "polypropylene",
      material_class == "poly(ethylene)" ~ "polyethylene",
      material_class == "polystyrenes (polyphenylethylenes, -methylstyrene)" ~ "polystyrene",
      material_class == "poly(acrylamide/amid)s" ~ "polyacrylamide",
      material_class == "poly(esters/ethers/diglycidylethers/terephthalates)s" ~ "polyester / PET",
      TRUE ~ material_class
    ),
    # lump others
    polymer_plot = if_else(
      polymer_short %in% top_materials,
      polymer_short,
      "other"
    )
  ) %>%
  group_by(river, polymer_plot) %>%
  summarize(
    total_particles = sum(extrap_count, na.rm = TRUE),
    .groups = "drop"
  )

# Build color palette
pal <- RColorBrewer::brewer.pal(
  n = length(top_materials),
  name = "Set1"
)
names(pal) <- top_materials
pal["polypropylene"] <- "gray40"  # override polypropylene
pal["other"] <- "grey80"          # lumped category

# Ensure factor order
df_bar$polymer_plot <- factor(
  df_bar$polymer_plot,
  levels = c(top_materials, "other")
)

# Plot
v = ggplot(df_bar, aes(x = river, y = total_particles, fill = polymer_plot)) +
  geom_col(position = "fill", width = 0.7, color = "black", linewidth = 0.2) +
  scale_fill_manual(
    name = "Polymer type",
    values = pal
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "River",
    y = "Proportion of plastic particles (50–500 µm)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "right",
    legend.justification = "center",
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(b = -6),
    legend.spacing.x = unit(0.4, "cm"),
    
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )
v




ggsave(
  filename = "small plastics by river.pdf",
  plot = v,
  device = "pdf",
  width = 7,
  height = 6,
  units = "in"
)






# -------------------------------
# Proportional stacked bar plot for BLANKS by sample_type
# Top 5 polymers + "other"
# -------------------------------

# Define top 5 polymers (short names)
top_materials <- c(
  "polypropylene",
  "polyethylene",
  "polystyrene",
  "polyacrylamide",
  "polyester / PET"
)

# Prepare plotting dataframe
df_bar_blank <- Part_dets_summ %>%
  filter(
    material_simple == "plastic",
    sample_or_blank == "blank"
  ) %>%
  mutate(
    # standardize polymer names
    polymer_short = case_when(
      material_class == "poly(propylene)" ~ "polypropylene",
      material_class == "poly(ethylene)" ~ "polyethylene",
      material_class == "polystyrenes (polyphenylethylenes, -methylstyrene)" ~ "polystyrene",
      material_class == "poly(acrylamide/amid)s" ~ "polyacrylamide",
      material_class == "poly(esters/ethers/diglycidylethers/terephthalates)s" ~ "polyester / PET",
      TRUE ~ material_class
    ),
    # lump others
    polymer_plot = if_else(
      polymer_short %in% top_materials,
      polymer_short,
      "other"
    )
  ) %>%
  group_by(sample_type, polymer_plot) %>%
  summarize(
    total_particles = sum(extrap_count, na.rm = TRUE),
    .groups = "drop"
  )

# Build color palette
pal <- RColorBrewer::brewer.pal(
  n = length(top_materials),
  name = "Set1"
)
names(pal) <- top_materials
pal["polypropylene"] <- "gray40"
pal["other"] <- "grey80"

# Ensure factor order
df_bar_blank$polymer_plot <- factor(
  df_bar_blank$polymer_plot,
  levels = c(top_materials, "other")
)

# Plot
w = ggplot(df_bar_blank, aes(x = sample_type, y = total_particles, fill = polymer_plot)) +
  geom_col(
    position = "fill",
    width = 0.7,
    color = "black",
    linewidth = 0.2
  ) +
  scale_fill_manual(
    name = "Polymer type",
    values = pal
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Blank type",
    y = "Proportion of plastic particles (50–500 µm)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "none",
    legend.justification = "center",
    legend.margin = margin(t = 0, b = 0),
    legend.box.margin = margin(b = -6),
    legend.spacing.x = unit(0.4, "cm"),
    
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11),
    
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

w


ggsave(
  filename = "small plastics by blanks.pdf",
  plot = w,
  device = "pdf",
  width = 3.35,
  height = 6,
  units = "in"
)







library(dplyr)

# Add "Other" category to all rows not in top 5
df_top_plastic <- df_plastic %>%
  mutate(
    material_short = case_when(
      material_class == "poly(propylene)" ~ "polypropylene",
      material_class == "poly(ethylene)" ~ "polyethylene",
      material_class == "polystyrenes (polyphenylethylenes, -methylstyrene)" ~ "polystyrene",
      material_class == "poly(acrylamide/amid)s" ~ "polyacrylamide",
      material_class == "poly(esters/ethers/diglycidylethers/terephthalates)s" ~ "polyester / PET",
      !material_class %in% top_materials ~ "Other",  # <-- anything not in top 5
      TRUE ~ material_class
    )
  )

# Reorder factor levels so "Other" appears last
df_top_plastic <- df_top_plastic %>%
  mutate(
    material_short = factor(
      material_short,
      levels = c("polypropylene", "polyethylene", "polystyrene", "polyacrylamide", "polyester / PET", "Other")
    )
  )




# Define top polymers and include "Other"
poly_order <- c(
  "polypropylene",
  "polystyrene",
  "polyethylene",
  "polyester / PET",
  "polyacrylamide",
  "other"
)

# Add "other" category to any material not in top 5
df_top_plastic <- df_plastic %>%
  mutate(
    material_short = case_when(
      material_class == "poly(propylene)" ~ "polypropylene",
      material_class == "poly(ethylene)" ~ "polyethylene",
      material_class == "polystyrenes (polyphenylethylenes, -methylstyrene)" ~ "polystyrene",
      material_class == "poly(acrylamide/amid)s" ~ "polyacrylamide",
      material_class == "poly(esters/ethers/diglycidylethers/terephthalates)s" ~ "polyester / PET",
      TRUE ~ "other"  # everything else
    ),
    material_short = factor(material_short, levels = poly_order)
  )

# Summarize for plotting: median ± MAD
df_poly_summary <- df_top_plastic %>%
  group_by(sample_depth_general, material_short) %>%
  summarise(
    med_count_L = median(particles_per_L, na.rm = TRUE),
    mad_count_L = mad(particles_per_L, na.rm = TRUE),  # median absolute deviation
    n = n(),
    .groups = "drop"
  )

# Define base palette for top 5 polymers
pal <- RColorBrewer::brewer.pal(n = 5, name = "Set1")
names(pal) <- poly_order[1:5]

# Override polypropylene and add color for Other
pal["polypropylene"] <- "gray40"
pal <- c(pal, other = "gray60")  # add Other


p_poly_bar <- ggplot(
  df_poly_summary,
  aes(
    x = material_short,
    y = med_count_L,
    fill = material_short
  )
) +
  geom_col(width = 0.65, alpha = 0.85) +
  
  # ---- upper-only MAD line (no perpendicular hatch)
  geom_segment(
    aes(
      x = material_short,
      xend = material_short,
      y = med_count_L,
      yend = med_count_L + mad_count_L
    ),
    linewidth = 0.6
  ) +
  
  facet_wrap(~ sample_depth_general, ncol = 2) +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "Median microplastic count per L\n(50–500 µm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_poly_bar





ggsave(
  filename = "small polymers by depth_bar.pdf",
  plot = p_poly_bar,
  device = "pdf",
  width = 9.5,
  height = 3.5,
  units = "in"
)











# Correlation of large and small MPs in same sample----
small_MPs <- Part_dets_summ_river2_sum %>%
  filter(material_simple == "plastic") %>%
  mutate(
    sample_key = str_replace(Client_ID_MSSupdate, "(SD|SS)$", "")
  ) %>%
  group_by(sample_key) %>%
  summarise(
    small_particles_L = sum(particles_per_L, na.rm = TRUE),
    .groups = "drop"
  )


large_MPs <- river_MPs_summ %>%
  mutate(
    sample_key = str_replace(Sample_ID, "(LD|LS)$", "")
  ) %>%
  group_by(sample_key) %>%
  summarise(
    large_particles_L = sum(MPs_L, na.rm = TRUE),
    .groups = "drop"
  )


MPs_joined <- small_MPs %>%
  inner_join(large_MPs, by = "sample_key")



p_corr <- ggplot(
  MPs_joined,
  aes(x = small_particles_L, y = large_particles_L) 
) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    linewidth = 0.8,
    color = "black"
  ) +
 # scale_x_log10() +
  #scale_y_log10() +
  labs(
    x = "Small microplastics (50–500 µm)\nparticles per L",
    y = "Large microplastics (500–5000 µm)\nparticles per L",
  ) +
  theme_minimal(base_size = 12)

p_corr



# Add marginal densities
p_corr_marginal <- ggMarginal(
  p_corr,
  type = "density",          # marginal density plots
  fill = "gray80",        # fill color for the marginal densities
  alpha = 0.5,               # transparency
  size = 5                   # thickness of marginal plots
)

p_corr_marginal



ggsave(
  filename = "Corr of large and small MPs.pdf",
  plot = p_corr_marginal,
  device = "pdf",
  width = 7.5,
  height = 4.5,
  units = "in"
)


cor.test(
  MPs_joined$small_particles_L,
  MPs_joined$large_particles_L,
  method = "spearman"
)




#Microplastic flux from first flush----


flow_scaled <- all_rivers_flow %>%
  group_by(river) %>%
  mutate(
    scale_factor =
      2 * max(                                   # 👈 increase multiplier (try 1.5–3)
        Part_dets_summ_river2_sum$particles_per_L[
          Part_dets_summ_river2_sum$river == first(river)
        ],
        na.rm = TRUE
      ) / max(Flow_m3s, na.rm = TRUE),
    
    flow_scaled = Flow_m3s * scale_factor
  ) %>%
  ungroup()



# Relevel river to control facet order
flow_scaled$river <- factor(
  flow_scaled$river,
  levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel" )
)

Part_dets_summ_river2_sum$river <- factor(
  Part_dets_summ_river2_sum$river,
  levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel")
)


library(dplyr)
library(lubridate)

start_date <- ymd("2023-11-15")
end_date   <- ymd("2024-05-15")

# Flow data restricted to window
flow_win <- flow_scaled %>%
  filter(date >= start_date, date <= end_date)

library(dplyr)
library(lubridate)

storm_windows <- tibble(
  river = factor(
    c("San Lorenzo", "Pajaro", "Salinas", "Carmel"),
    levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel")
  ),
  start = ymd(c(
    "2023-12-24",  # San Lorenzo
    "2024-01-16",  # Pajaro
    "2024-01-30",  # Salinas
    "2024-01-26"   # Carmel
  )),
  end = ymd(c(
    "2024-01-09",  # San Lorenzo
    "2024-01-30",  # Pajaro
    "2024-02-15",  # Salinas
    "2024-02-15"   # Carmel
  ))
)




p_MP_flow_yr1 <- ggplot() +
  
  # ---- Storm / peak-flow window shading (behind everything)
geom_rect(
  data = storm_windows,
  aes(
    xmin = start,
    xmax = end,
    ymin = -Inf,
    ymax = Inf
  ),
  inherit.aes = FALSE,
  fill = "gray70",
  alpha = 0.35
) +
  
  # ---- Plastic bars only 
geom_col(
  data = Part_dets_summ_river2_sum %>%
    filter(
      material_simple == "plastic",
      date >= ymd("2023-11-15"),
      date <= ymd("2024-05-15")
    ),
  aes(x = date, y = particles_per_L),
  fill = "mediumorchid",
  alpha = 0.7
) +
  
  # ---- River flow (secondary axis) 
geom_line(
  data = flow_scaled %>%
    filter(
      date >= ymd("2023-11-15"),
      date <= ymd("2024-05-15")
    ),
  aes(x = date, y = flow_scaled),
  color = "steelblue2",
  linewidth = 0.6
) +
  
  facet_wrap(~ river, scales = "free_y", ncol = 1) +
  
  scale_y_continuous(
    name = "Plastic particle count per L (50–500 µm)",
    sec.axis = sec_axis(
      ~ . / unique(flow_scaled$scale_factor)[1],
      name = expression(paste("River flow (m"^3, " s"^-1, ")"))
    )
  ) +
  
  scale_x_date(
    limits = c(ymd("2023-11-15"), ymd("2024-05-15")),
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  
  labs(x = "Date") +
  
  theme_minimal(base_size = 8) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 8),
    axis.title.y.right = element_text(color = "steelblue3"),
    axis.text.y.right  = element_text(color = "steelblue3"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

p_MP_flow_yr1


ggsave(
  filename = "p_MP_flow_yr1.pdf",
  plot = p_MP_flow_yr1,
  device = "pdf",
  width = 4,
  height = 5,
  units = "in"
)









# scale factor
scale_factor <- max(all_FF_flow_hourly$MP_flux_cumulative_med, na.rm = TRUE) /
  max(all_FF_flow_hourly$Flow_m3s, na.rm = TRUE)


First_flush_flux <- ggplot(all_FF_flow_hourly, aes(x = date_hour)) +
  
  # ---- Cumulative MP flux (median)
geom_line(
  aes(y = MP_flux_cumulative_med),
  color = "mediumorchid",
  linewidth = 0.6
) +
  
  # ---- Cumulative MP flux (p25 & p75; dotted) 
geom_line(
  aes(y = MP_flux_cumulative_p25),
  color = "mediumorchid",
  linewidth = 0.4,
  linetype = "dashed"
) +
  
  geom_line(
    aes(y = MP_flux_cumulative_p75),
    color = "mediumorchid",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  # ---- Flow (secondary axis; scaled)
geom_line(
  aes(y = Flow_m3s * scale_factor),
  color = "steelblue",
  linewidth = 0.5
) +
  
  scale_y_continuous(
    name = "Cumulative microplastic flux (50–500 µm)",
    labels = comma,
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = expression(River~flow~(m^3~s^-1))
    )
  ) +
  
  facet_wrap(~ river, scales = "free", ncol = 1) +
  
  labs(x = NULL) +
  
  theme_minimal(base_size = 9) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8, face = "bold"),
    
    axis.title.y.left = element_text(color = "mediumorchid"),
    axis.text.y.left  = element_text(color = "mediumorchid"),
    
    axis.title.y.right = element_text(color = "steelblue"),
    axis.text.y.right  = element_text(color = "steelblue"),
    
    panel.grid.minor = element_blank()
  )
First_flush_flux


ggsave(
  filename = "First flush flux.pdf",
  plot = First_flush_flux,
  device = "pdf",
  width = 4,
  height = 5,
  units = "in"
)















# Extra code below----

# Now for the individual plots, MPs only for particles


q = ggplot() +
  
  
  geom_line(
    data = flow_scaled,
    aes(x = date, y = flow_scaled),
    color = "steelblue2",
    linewidth = 0.6
  ) +
  
  
  facet_wrap(~ river, nrow = 1)+
  
  scale_y_continuous(name = "River flow rate (m³/s)"
  ) +
  
  scale_x_date(
    limits = c(ymd("2023-10-01"), ymd("2025-06-01")),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  labs(x = "Date") +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.y.right = element_text(color = "steelblue3"),
    axis.text.y.right = element_text(color = "steelblue3"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "bottom"
  )

q


r = ggplot() +
  geom_col(
    data = Part_dets_summ_river2 %>% 
      filter(material_simple == "plastic"),
    aes(x = date, y = particles_per_L),
    fill = "mediumorchid",
    alpha = 0.85
  ) +
  facet_wrap(~ river, nrow = 1) +
  scale_y_continuous(
    name = "Particle count per L (50–500 µm)"
  ) +
  scale_x_date(
    limits = c(ymd("2023-10-01"), ymd("2025-06-01")),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  labs(x = "Date") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

r







s = ggplot(
  river_MPs_summ,
  aes(x = date, y = MPs_L)
) +
  geom_col(
    fill = "orchid",
    alpha = 0.85
  ) +
  facet_wrap(
    ~ sample_location,
    nrow = 1,
    scales = "free_y"
  ) +
  scale_x_date(
    limits = c(ymd("2023-10-01"), ymd("2025-06-01")),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  labs(
    x = "Date",
    y = "Particle count per L (500–5000 µm)",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

s


# Now plot together:

q <- q +
  labs(x = NULL) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

r <- r +
  labs(x = NULL) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

combined_plot <- q / r / s +
  plot_layout(
    heights = c(1, 1, 1.1)   # slightly more space for bottom axis labels
  )
combined_plot


all_rivers_flow_MPs <- all_rivers_flow %>%
  mutate(
    MP_Flow_m3s = case_when(
      river == "San Lorenzo" ~ Flow_m3s * 1000 * 0.615,
      river == "Pajaro"      ~ Flow_m3s * 1000 * 0.402,
      river == "Salinas"     ~ Flow_m3s * 1000 * 0.315,
      river == "Carmel"      ~ Flow_m3s * 1000 * 0.355,
      TRUE ~ NA_real_
    )
  )




p_MP_flux <- ggplot() +
  geom_line(
    data = all_rivers_flow_MPs,
    aes(x = date, y = MP_Flow_m3s * 86400),
    color = "mediumorchid",
    linewidth = 0.6
  ) +
  
  facet_wrap(~ river, scales = "free_y", ncol = 1) +
  
  scale_y_continuous(
    name   = "Total estimated MP flux (50–5000 µm)\nper day",
    labels = scales::comma
  ) +
  
  scale_x_date(
    limits = c(ymd("2023-10-01"), ymd("2025-06-01")),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  labs(x = "Date") +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

p_MP_flux























