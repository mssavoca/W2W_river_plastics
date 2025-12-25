# River flow and MPs figure ----


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
Part_dets_summ_river2 <- Part_dets_summ_river %>%
  mutate(
    particles_per_L = ifelse(
      Client_ID_MSSupdate == "CRR20240116SS",
      extrap_count / 200,
      extrap_count / 400
    )
  )

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
      max(
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









#River water MPs by depth ----

df_plot <- Part_dets_summ_river %>%
  group_by(material_simple, Client_ID_MSSupdate, sample_depth_general) %>%
  summarize(total_extrap_count = sum(extrap_count, na.rm = TRUE),
            particles_per_L = total_extrap_count/200) %>%
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
  summarize(total_extrap_count = sum(extrap_count, na.rm = TRUE),
            particles_per_L = total_extrap_count/200) %>%
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
  summarize(sum_total_extrap_count = sum(total_extrap_count)) %>%
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
  
  # ---- facet-specific median lines ----
geom_vline(
  data = median_df,
  aes(xintercept = median_total_count_L, color = material_short),
  linetype = "dashed",
  linewidth = 0.6,
  show.legend = FALSE
) +
  
  facet_wrap(~ sample_depth_general, scales = "free", ncol = 1) +
  scale_x_log10() +
  
  # ---- Set1 palette with PP overridden ----
scale_color_manual(
  name = "Polymer Type",
  values = pal
) +
  
  # ---- two-row legend ----
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


