# Main Figure drafts ----


source("Util.R")

library(dplyr)
library(lubridate)
library(patchwork)



# Figure 2 ----

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


# Small MPs over river flow data
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
           material_simple, sample_depth_general,
           sampling_season) %>% 
  summarise(
    particles_per_L = sum(extrap_conc_PPL, na.rm = TRUE), 
    .groups = "drop"
  ) 

Part_dets_summ_river2$river <- factor(
    Part_dets_summ_river2$river,
    levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel")
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
      1.25 * max(                                   # 👈 increase multiplier (try 1.5–3)
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


### Fig 2A ----
library(dplyr)
library(grid)

# ------------------------------------------------------------
# Plastic data: one averaged value per river x date
# ------------------------------------------------------------

plastic_data <- Part_dets_summ_river2 %>%
  filter(
    material_simple == "plastic",
    Client_ID_MSSupdate != "CRR20240124_WaterBlank"
  ) %>%
  mutate(
    date = as.Date(date)
  ) %>%
  group_by(river, date) %>%
  summarise(
    particles_per_L = mean(particles_per_L, na.rm = TRUE),
    n_measurements = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Check that there is now exactly one value per river/date
# ------------------------------------------------------------

plastic_data %>%
  count(river, date) %>%
  filter(n > 1)

# This should return 0 rows


# ------------------------------------------------------------
# Calculate arrow positions from the SAME averaged data
# ------------------------------------------------------------

arrow_data <- plastic_data %>%
  group_by(river) %>%
  mutate(
    arrow_start =
      particles_per_L +
      max(particles_per_L, na.rm = TRUE) * 0.15,
    
    arrow_end =
      particles_per_L +
      max(particles_per_L, na.rm = TRUE) * 0.05
  ) %>%
  ungroup()


# ------------------------------------------------------------
# Plot
# ------------------------------------------------------------

p_small_w_arrows <- ggplot() +
  
  # River flow
  geom_line(
    data = flow_scaled,
    aes(
      x = date,
      y = flow_scaled
    ),
    color = "steelblue2",
    linewidth = 0.6
  ) +
  
  # One averaged plastic bar per river x date
  geom_col(
    data = plastic_data,
    aes(
      x = date,
      y = particles_per_L
    ),
    fill = "maroon",
    alpha = 0.7
  ) +
  
  # One arrow per averaged sampling event
  geom_segment(
    data = arrow_data,
    aes(
      x = date,
      xend = date,
      y = arrow_start,
      yend = arrow_end
    ),
    arrow = arrow(
      length = unit(0.2, "cm"),
      type = "closed"
    ),
    color = "gray40",
    linewidth = 0.6
  ) +
  
  facet_wrap(
    ~ river,
    scales = "free_y",
    ncol = 1
  ) +
  
  scale_y_continuous(
    name = "Microplastics per L (50–500 µm)",
    sec.axis = sec_axis(
      ~ . / unique(flow_scaled$scale_factor)[1],
      name = expression(
        paste("River flow (m"^3, " s"^-1, ")")
      )
    )
  ) +
  
  scale_x_date(
    limits = c(
      ymd("2023-10-01"),
      ymd("2025-06-01")
    ),
    date_breaks = "2 months",
    date_labels = "%b %Y"
  ) +
  
  labs(x = "Date") +
  
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    strip.text = element_text(
      face = "bold",
      size = 12
    ),
    axis.title.y.right = element_text(
      color = "steelblue3"
    ),
    axis.text.y.right = element_text(
      color = "steelblue3"
    ),
    axis.title.y.left = element_text(
      color = "maroon"
    ),
    axis.text.y.left = element_text(
      color = "maroon"
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    legend.position = "none"
  )

p_small_w_arrows



ggsave(
  filename = "river_small plast_flow_dual_axis_vert_w_arrows_v2.pdf",
  plot = p_small_w_arrows,
  device = "pdf",
  width = 5.5,
  height = 8,
  units = "in"
)
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




# Large MPs over river flow data

flow_clean <- all_rivers_flow %>%
  group_by(river, date) %>%
  summarise(
    Flow_m3s = mean(Flow_m3s, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sample_location = factor(
      river,
      levels = c(
        "San Lorenzo",
        "Pajaro",
        "Salinas",
        "Carmel"
      )
    )
  )



# ------------------------------------------------------------
# Calculate river-specific scaling factors
# Using the AVERAGED large-MP data
# ------------------------------------------------------------

scale_df <- large_MPs_plot %>%
  group_by(sample_location) %>%
  summarise(
    max_particles = max(MPs_L, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    flow_clean %>%
      group_by(sample_location) %>%
      summarise(
        max_flow = max(Flow_m3s, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "sample_location"
  ) %>%
  mutate(
    scale_factor = max_particles / max_flow
  )



# Scale flow within each river
flow_scaled <- flow_clean %>%
  left_join(
    scale_df,
    by = "sample_location"
  ) %>%
  transmute(
    sample_location,
    date,
    flow_scaled = Flow_m3s * scale_factor
  )




# Plot: Large MPs on top of river flow


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



### Fig 2B----
library(dplyr)
library(grid) # Needed for the arrow() function

# 1. Calculate the top of the bars to position the arrows for the large particles
arrow_data_large <- river_MPs_summ %>%
  group_by(sample_location, date) %>%
  summarise(total_MPs = sum(MPs_L, na.rm = TRUE), .groups = "drop") %>%
  group_by(sample_location) %>%
  mutate(
    # Set the start and end heights of the arrow based on the bar height
    # Proportional margin (15% to 5% of the facet's max height)
    arrow_start = total_MPs + max(total_MPs) * 0.15,
    arrow_end   = total_MPs + max(total_MPs) * 0.05
  )

# 2. Your Plot Code
p_large_vert_w_arrows <- ggplot() +
  
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
    fill = "maroon",
    alpha = 0.7
  ) +
  
  # ---> NEW: Add the arrows pointing down to the sampling dates <---
  geom_segment(
    data = arrow_data_large,
    aes(x = date, xend = date, y = arrow_start, yend = arrow_end),
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "gray40",
    linewidth = 0.6
  ) +
  
  # Vertical facets by river in desired order
  facet_wrap(
    ~ sample_location,
    scales = "free_y",
    ncol = 1
  ) +
  
  # y-axis and secondary axis for real flow
  scale_y_continuous(
    name = "Putative microplastics per L (500–5000 µm)",
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
    axis.title.y.left = element_text(color = "maroon"),
    axis.text.y.left = element_text(color = "maroon"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_large_vert_w_arrows



ggsave(
  filename = "river_large particle_flow_dual_axis_vert_w_arrows_v2.pdf",
  plot = p_large_vert_w_arrows,
  device = "pdf",
  width = 5.5,
  height = 8,
  units = "in"
)
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



# Figure 3----


# Boxplot by river and sampling season - plastic only

plastic_data <- Part_dets_summ_river2 %>%
  filter(
    material_simple == "plastic",
    !is.na(sample_depth_general)
  ) %>%
  mutate(
    river = factor(
      river,
      levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel")
    ),
    sampling_season = factor(
      sampling_season,
      levels = c("Season 1", "Season 2")
    )
  )

# Panel A: River
bp_river_MPs <- ggplot(
  plastic_data,
  aes(x = river, y = particles_per_L)
) +
  geom_boxplot(
    fill = "maroon",
    color = "black",
    outlier.shape = NA,
    linewidth = 0.6,
    alpha = 0.5
  ) +
  geom_jitter(
    color = "maroon",
    width = 0.2,
    alpha = 0.7,
    size = 2
  ) +
  scale_y_log10() +
  labs(
    x = NULL,
    y = "Microplastics per L (50–500 µm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

# Panel B: Sampling season
bp_season_MPs <- ggplot(
  plastic_data,
  aes(x = sampling_season, y = particles_per_L)
) +
  geom_boxplot(
    fill = "maroon",
    color = "black",
    outlier.shape = NA,
    linewidth = 0.6,
    alpha = 0.5
  ) +
  geom_jitter(
    color = "maroon",
    width = 0.15,
    alpha = 0.7,
    size = 2
  ) +
  scale_y_log10() +
  labs(
    x = NULL,
    y = "Microplastics per L (50–500 µm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

# Combine panels
(bp_river_MPs | bp_season_MPs) +
  plot_annotation(tag_levels = "A")



ggsave(
  filename = "boxplot by river and sampling season_v2.pdf",
  device = "pdf",
  width = 7,
  height = 4.5,
  units = "in"
)






#Figure 4 ----


# Stacked proportional plots; all rivers, and blanks

# Proportional stacked bar plot by river: top 5 polymers + "other"



# Desired river display order (north -> south)
river_order <- c(
  "San Lorenzo",
  "Pajaro",
  "Salinas",
  "Carmel"
)


# Define top 5 polymers (short names)
top_materials <- c(
  "polypropylene",
  "polyethylene",
  "polystyrene",
  "polyamide",
  "polyester / PET"
)

# Prepare plotting dataframe
df_bar <- Part_dets_summ_river %>%
  filter(material_simple == "plastic") %>%
  mutate(
    river = factor(river, levels = river_order), 
    # standardize polymer names
    polymer_short = case_when(
      material_class == "poly(propylene)" ~ "polypropylene",
      material_class == "poly(ethylene)" ~ "polyethylene",
      material_class == "polystyrenes (polyphenylethylenes, -methylstyrene)" ~ "polystyrene",
      material_class == "poly(acrylamide/amid)s" ~ "polyamide",
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

### Fig 4A----
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
  filename = "small plastics by river_v2.pdf",
  plot = v,
  device = "pdf",
  width = 7,
  height = 6,
  units = "in"
)




# Proportional stacked bar plot for BLANKS by sample_type
# Top 5 polymers + "other"


# Define top 5 polymers (short names)
top_materials <- c(
  "polypropylene",
  "polyethylene",
  "polystyrene",
  "polyamide",
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

### Fig 4B ----
w = ggplot(df_bar_blank, 
           aes(x = sample_type, y = total_particles, fill = polymer_plot)) +
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
  filename = "small plastics by blanks_v2.pdf",
  plot = w,
  device = "pdf",
  width = 3.35,
  height = 6,
  units = "in"
)





# Filter for plastic only and summarize counts by Client_ID_MSSupdate and material_class, sample_depth_general
df_plastic <- Part_dets_summ_river %>%
  filter(material_simple == "plastic",
         sample_or_blank == "sample",
         ) %>%
  group_by(material_class, Client_ID_MSSupdate, sample_depth_general) %>%
  summarize(particles_per_L = sum(extrap_conc_PPL)) %>%
  mutate(
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("surface", "subsurface")
    )
  ) %>% 
  ungroup()




# Define top polymers and include "other"
poly_order <- c(
  "polypropylene",
  "polyethylene",
  "polystyrene",
  "polyamide",
  "polyester / PET",
  "other"
)

# Add "other" category to any material not in top 5
df_top_plastic <- df_plastic %>%
  mutate(
    material_short = case_when(
      material_class == "poly(propylene)" ~ "polypropylene",
      material_class == "poly(ethylene)" ~ "polyethylene",
      material_class == "polystyrenes (polyphenylethylenes, -methylstyrene)" ~ "polystyrene",
      material_class == "poly(acrylamide/amid)s" ~ "polyamide",
      material_class == "poly(esters/ethers/diglycidylethers/terephthalates)s" ~ "polyester / PET",
      TRUE ~ "other"
    ),
    material_short = factor(
      material_short,
      levels = poly_order
    )
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

# Build color palette
pal <- RColorBrewer::brewer.pal(
  n = length(top_materials),
  name = "Set1"
)
names(pal) <- top_materials
pal["polypropylene"] <- "gray40"  # override polypropylene
pal["other"] <- "grey80"          # lumped category



### Fig 4C ----
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
    y = "Microplastics per L (50–500 µm)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

p_poly_bar





ggsave(
  filename = "small polymers by depth_bar_v2.pdf",
  plot = p_poly_bar,
  device = "pdf",
  width = 9.5,
  height = 4,
  units = "in"
)









#Figure 5----
#Microplastic flux from first flush


flow_scaled <- all_rivers_flow %>%
  group_by(river) %>%
  mutate(
    scale_factor =
      0.75 * max(                              
        Part_dets_summ_river2$particles_per_L[
          Part_dets_summ_river2$river == first(river)
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

Part_dets_summ_river2$river <- factor(
  Part_dets_summ_river2$river,
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



### Fig 5A----
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
geom_point(
  data = Part_dets_summ_river2 %>%
    filter(
      material_simple == "plastic",
      Client_ID_MSSupdate != "CRR20240124_WaterBlank",
      date >= ymd("2023-11-15"),
      date <= ymd("2024-05-15")
    ),
  aes(x = date, y = particles_per_L,
      shape = sample_depth_general),
  color = "maroon",
  alpha = 0.8
) +
  
  # ---- River flow (secondary axis) 
geom_line(
  data = flow_scaled %>%
    filter(
      date >= ymd("2023-11-15"),
      date <= ymd("2024-05-15")
    ),
  aes(x = date, y = flow_scaled),
  color = "steelblue",
  linewidth = 0.6
) +
  
  facet_wrap(~ river, scales = "free_y", ncol = 1) +
  
  scale_y_continuous(
    name = "Microplastics per L (50–500 µm)",
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
  
  theme_minimal(base_size = 12) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(color = "maroon"),
    axis.text.y.left  = element_text(color = "maroon"),
    axis.title.y.right = element_text(color = "steelblue"),
    axis.text.y.right  = element_text(color = "steelblue"),
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





### Fig 5B----
### Season 1 microplastic transport estimates ----

library(dplyr)
library(lubridate)

# ------------------------------------------------------------
# 1. Get Season 1 plastic concentrations by river
#    Use ALL measurements within each river
# ------------------------------------------------------------

season1_conc_stats <- Part_dets_summ_river2 %>%
  filter(
    material_simple == "plastic",
    sampling_season == "Season 1",
    Client_ID_MSSupdate != "CRR20240124_WaterBlank"
  ) %>%
  group_by(river) %>%
  summarise(
    conc_med = median(particles_per_L, na.rm = TRUE),
    conc_p25 = quantile(particles_per_L, 0.25, na.rm = TRUE),
    conc_p75 = quantile(particles_per_L, 0.75, na.rm = TRUE),
    n_measurements = sum(!is.na(particles_per_L)),
    .groups = "drop"
  ) %>%
  mutate(
    river = factor(
      river,
      levels = c(
        "San Lorenzo",
        "Pajaro",
        "Salinas",
        "Carmel"
      )
    )
  )

season1_conc_stats


# ------------------------------------------------------------
# 2. Build hourly flow dataset
# ------------------------------------------------------------

all_FF_flow_hourly <- bind_rows(
  
  flow_hourly_SanLorenzo_clean %>%
    mutate(river = "San Lorenzo"),
  
  flow_hourly_Pajaro_clean %>%
    mutate(river = "Pajaro"),
  
  flow_hourly_Salinas_clean %>%
    mutate(river = "Salinas"),
  
  flow_hourly_Carmel_clean %>%
    mutate(river = "Carmel")
  
) %>%
  mutate(
    date_hour = floor_date(date_time, unit = "hour"),
    river = factor(
      river,
      levels = c(
        "San Lorenzo",
        "Pajaro",
        "Salinas",
        "Carmel"
      )
    )
  ) %>%
  group_by(river, date_hour) %>%
  summarise(
    Flow_m3s = mean(Flow_m3s, na.rm = TRUE),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 3. Add Season 1 concentration estimates and calculate flux
# ------------------------------------------------------------

all_FF_flow_hourly <- all_FF_flow_hourly %>%
  left_join(
    season1_conc_stats,
    by = "river"
  ) %>%
  mutate(
    MP_flux_per_hr_med =
      Flow_m3s * conc_med * 1000 * 3600,
    
    MP_flux_per_hr_p25 =
      Flow_m3s * conc_p25 * 1000 * 3600,
    
    MP_flux_per_hr_p75 =
      Flow_m3s * conc_p75 * 1000 * 3600
  )


# ------------------------------------------------------------
# 4. Calculate cumulative transported microplastics
# ------------------------------------------------------------

all_FF_flow_hourly <- all_FF_flow_hourly %>%
  arrange(river, date_hour) %>%
  group_by(river) %>%
  mutate(
    MP_flux_cumulative_med =
      lag(
        cumsum(MP_flux_per_hr_med),
        default = 0
      ),
    
    MP_flux_cumulative_p25 =
      lag(
        cumsum(MP_flux_per_hr_p25),
        default = 0
      ),
    
    MP_flux_cumulative_p75 =
      lag(
        cumsum(MP_flux_per_hr_p75),
        default = 0
      )
  ) %>%
  ungroup()



final_MP_flux <- all_FF_flow_hourly %>%
  group_by(river) %>%
  slice_max(
    order_by = date_hour,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    river,
    MP_flux_cumulative_med,
    MP_flux_cumulative_p25,
    MP_flux_cumulative_p75
  )

final_MP_flux


total_MP_flux <- final_MP_flux %>%
  summarise(
    total_med = sum(MP_flux_cumulative_med, na.rm = TRUE),
    total_p25 = sum(MP_flux_cumulative_p25, na.rm = TRUE),
    total_p75 = sum(MP_flux_cumulative_p75, na.rm = TRUE)
  )

total_MP_flux

# ------------------------------------------------------------
# Calculate ONE common scaling factor for river flow
# ------------------------------------------------------------

scale_factor <- max(
  all_FF_flow_hourly$MP_flux_cumulative_med,
  na.rm = TRUE
) /
  max(
    all_FF_flow_hourly$Flow_m3s,
    na.rm = TRUE
  )


# Scale flow using the common factor
plot_flux <- all_FF_flow_hourly %>%
  mutate(
    Flow_scaled = Flow_m3s * scale_factor
  )


### Cumulative microplastic flux - Season 1

First_flush_flux <- ggplot(
  plot_flux,
  aes(x = date_hour)
) +
  
  # Median cumulative MP flux
  geom_line(
    aes(y = MP_flux_cumulative_med),
    color = "maroon",
    linewidth = 0.6
  ) +
  
  # P25 cumulative MP flux
  geom_line(
    aes(y = MP_flux_cumulative_p25),
    color = "maroon",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  # P75 cumulative MP flux
  geom_line(
    aes(y = MP_flux_cumulative_p75),
    color = "maroon",
    linewidth = 0.4,
    linetype = "dashed"
  ) +
  
  # River flow
  geom_line(
    aes(y = Flow_scaled),
    color = "steelblue",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ river,
    scales = "free",
    ncol = 1
  ) +
  
  scale_y_continuous(
    name = "Cumulative microplastic flux (50–500 µm)",
    labels = scales::comma,
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = expression(
        River~flow~(m^3~s^-1)
      ),
      labels = scales::comma
    )
  ) +
  
  
  theme_minimal(base_size = 12) +
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    axis.title.x = element_blank(),
    axis.title.y.left = element_text(color = "maroon"),
    axis.text.y.left  = element_text(color = "maroon"),
    axis.title.y.right = element_text(color = "steelblue"),
    axis.text.y.right  = element_text(color = "steelblue"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )
First_flush_flux




# Combine panels
(p_MP_flow_yr1 | First_flush_flux) +
  plot_annotation(tag_levels = "A")




ggsave(
  filename = "First flush flux_comb.pdf",
  device = "pdf",
  width = 12,
  height = 7,
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























