# Supplemental Figure drafts ----


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



#Large MPs supplemental plots----

# ---- Large MPs supplemental plot: median ± MAD by sample type


desired_order <- c("river water", "field blank", "lab blank")

# Convert to long format and summarize by Sample_ID first
long_data <- large_MPs_summ %>% 
  pivot_longer(
    cols = Total_Fibers:Total_Foams,
    names_to = "Plastic_Type",
    values_to = "Value"
  ) %>%
  mutate(
    Plastic_Type = case_when(
      Plastic_Type == "Total_Fibers" ~ "fiber",
      Plastic_Type == "Total_Fragments" ~ "fragment",
      Plastic_Type == "Total_Films" ~ "film",
      Plastic_Type == "Total_Nurdles" ~ "nurdle",
      Plastic_Type == "Total_Foams" ~ "foam",
      TRUE ~ Plastic_Type
    ),
    sample_type = str_trim(tolower(sample_type))
  ) %>%
  filter(
    sample_type %in% desired_order,
    !is.na(Value)
  )

# Summarize total MPs per sample (across plastic types)
sample_level_summary <- long_data %>%
  group_by(sample_type, Sample_ID) %>%
  summarize(
    Total_MP = sum(Value, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate median, MAD, and sample size
plot_summary <- sample_level_summary %>%
  group_by(sample_type) %>%
  summarize(
    MP_median = median(Total_MP, na.rm = TRUE),
    MP_MAD = mad(Total_MP, constant = 1, na.rm = TRUE),
    n = n_distinct(Sample_ID),
    .groups = "drop"
  ) %>%
  mutate(
    sample_type = factor(sample_type, levels = desired_order)
  )

# Plot
ggplot(plot_summary, aes(x = sample_type, y = MP_median)) +
  geom_col(width = 0.7, fill = "gray40") +
  geom_segment(
    aes(
      x = sample_type,
      xend = sample_type,
      y = MP_median,
      yend = MP_median + MP_MAD
    ),
    linewidth = 0.8
  ) +
  labs(
    x = "Sample type",
    y = "Num. large microplastics (500–5000 µm) \nper sample"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave(
  filename = "large MPs by sample type.pdf",
  device = "pdf",
  width = 4,
  height = 5,
  units = "in"
)





# Create a stacked bar plot with the custom palette
Opt_summ_stacked_bar_plot <- ggplot(long_river_data, aes(x = sample_type, y = Value, 
                                                   fill = Plastic_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  scale_fill_manual(values = custom_palette) +  # Apply custom color palette
  labs(x = "Sample Type", y = "Plastic Count", fill = "Plastic Type") +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Display the plot
Opt_summ_stacked_bar_plot





# Calculate proportions
long_data_prop <- long_river_data %>%
  group_by(sample_type) %>%
  mutate(Total = sum(Value)) %>%
  ungroup() %>%
  mutate(Proportion = Value / Total)

# Create the plot
Opt_summ_prop_bar_plot <- ggplot(long_data_prop, aes(x = sample_type, y = Proportion, fill = Plastic_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  scale_fill_manual(values = custom_palette) +  # Apply custom color palette
  labs(x = "Sample Type", y = "Proportion of Plastic Types", fill = "Plastic Type") +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +  # Rotate x-axis labels
  scale_y_continuous(labels = scales::percent_format())  # Format y-axis as percentages

# Display the plot
Opt_summ_prop_bar_plot


ggsave(
  filename = "Prop large MPs by sample type and morphology.png",
  width = 5.5,
  height = 5,
  units = "in",
  dpi = 600
)




Opt_summ_AgvsUrban <- long_river_data %>%
  filter(!is.na(Site_type)) %>%  # Remove rows where Site_type is NA
  ggplot(aes(x = Site_type, y = Value, fill = Plastic_Type)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bars
  scale_fill_manual(values = custom_palette) +  # Apply custom color palette
  labs(x = "Site Type", y = "Plastic Count", fill = "Plastic Type") +
  theme_classic(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Display the plot
Opt_summ_AgvsUrban






library(dplyr)
library(ggplot2)
library(forcats)

# ---- Ensure facet order
large_MPs_river_dets_master <- large_MPs_river_dets_master %>%
  mutate(
    sample_type = factor(
      sample_type,
      levels = c("river water", "field blank", "lab blank")
    )
  )

# ---- Determine color order from river water counts only
color_order <- large_MPs_river_dets_master %>%
  filter(
    sample_type == "river water",
    !is.na(Color)
  ) %>%
  count(Color, sort = TRUE) %>%
  pull(Color)

# ---- Apply color order
large_MPs_river_dets_master <- large_MPs_river_dets_master %>%
  mutate(Color = factor(Color, levels = color_order))

# ---- Explicit color palette (gold folded into yellow)
color_pal <- c(
  blue   = "steelblue3",
  clear  = "gray90",
  black  = "black",
  red    = "firebrick2",
  brown  = "sienna3",
  white  = "white",
  green  = "forestgreen",
  orange = "darkorange",
  yellow = "goldenrod2",
  purple = "mediumpurple3"
)

# ---- Plot counts
large_MPs_river_dets_master %>%
  filter(!is.na(Color)) %>%
  count(sample_type, Color, name = "n") %>%
  ggplot(
    aes(x = Color, y = n, fill = Color)
  ) +
  geom_col(
    width = 0.7,
    color = "black",
    linewidth = 0.3
  ) +
  facet_wrap(
    ~ sample_type,
    scales = "free_y"
  ) +
  scale_fill_manual(values = color_pal, drop = FALSE) +
  labs(
    x = "Particle color",
    y = "Total count of putative \nmicroplastics (500–5000 µm)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )



ggsave(
  filename = "large MPs by color.pdf",
  device = "pdf",
  width = 8,
  height = 4,
  units = "in"
)




