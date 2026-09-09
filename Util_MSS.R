# Utilities file to pre-load for all analysis of W2W river plastics project----

#Load packages----
library(tidyverse)
library(dataRetrieval)
library(janitor)
library(ggExtra)
library(scales)
library(RColorBrewer)
library(broom)
library(readxl)
library(tidyr)
library(knitr)
library(grid)
library(glmmTMB)
library(lme4)
library(lmerTest)
library(car)
library(patchwork)




# Define a custom color palette----
custom_palette <- c(
  "fiber" = "#E69F00",      # Orange
  "fragment" = "#56B4E9",   # Light Blue
  "film" = "#009E73",       # Green
  "nurdle" = "#F0E442",     # Yellow
  "foam" = "#0072B2",       # Dark Blue
  "other" = "#D55E00"       # Red (for any unexpected categories)
)


#####Bring in data from other proj----

#large microplastics----

large_MPs_summ <- readRDS("../W2W_MBNMS/Opt_micro_all_cut.rds")

river_MPs_summ <- large_MPs_summ %>% 
  filter(sample_type == "river water",
         !(
           Sample_ID == "CRR20240124LS" &
             Total_Fibers == 0 &
             Total_Fragments == 0 &
             Total_Films == 0 &
             Total_Nurdles == 0
         )
  ) %>% 
  mutate(
    # Correct Pajaro sampling date: 2024-02-09 -> 2024-02-08
    Sample_ID = str_replace(
      Sample_ID,
      "PRR20240209",
      "PRR20240208"
    ),
    # extract the first 8-digit block from the ID
    date_raw = str_extract(Sample_ID, "\\d{8}"),
    
    # convert YYYYMMDD → Date
    date = ymd(date_raw), 
    MPs_L = Total_Count/200
  )


# ------------------------------------------------------------
# Combine duplicate large-MP sample rows
# ------------------------------------------------------------

combine_large_sample <- function(df, ids_to_combine) {
  
  combined <- df %>%
    filter(Sample_ID %in% ids_to_combine) %>%
    summarise(
      across(
        -starts_with("Total_"),
        ~ first(.x)
      ),
      across(
        starts_with("Total_"),
        ~ sum(.x, na.rm = TRUE)
      )
    )
  
  df %>%
    filter(!Sample_ID %in% ids_to_combine) %>%
    bind_rows(combined)
}

# Combine SLR20240122LS
river_MPs_summ <- combine_large_sample(
  river_MPs_summ,
  c("SLR20240122LS")
)

# Combine SRR20240205LD and SRR20240205LD_2
river_MPs_summ <- river_MPs_summ %>%
  mutate(
    Sample_ID = if_else(
      Sample_ID == "SRR20240205LD_2",
      "SRR20240205LD",
      Sample_ID
    )
  ) %>%
  group_by(Sample_ID) %>%
  summarise(
    across(
      -starts_with("Total_"),
      ~ first(.x)
    ),
    across(
      starts_with("Total_"),
      ~ sum(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )







large_MPs_river_dets_master <- bind_rows(
  
  # ---- River + field blank detections
  readRDS("../W2W_MBNMS/Opt_micro_river_dets_cut.rds") %>%
    mutate(
      # Correct Pajaro sampling date: 2024-02-09 -> 2024-02-08
      Sample_ID = str_replace(
        Sample_ID,
        "PRR20240209",
        "PRR20240208"
      ),
      Morphology = case_when(
        Morphology %in% c("Fiber", "Fiber Clump", "fiber", "fiber clump", "Fibers", "fibers") ~ "fiber",
        Morphology %in% c("Fragment", "fragment") ~ "fragment",
        tolower(Morphology) == "film" ~ "film",
        Morphology %in% c("Foams", "Foam") ~ "foam",
        TRUE ~ as.character(Morphology)
      ),
      
      Color = case_when(
        Color %in% c("Navy blue", "Blue", "Light Blue", "Navy Blue", "Teal", "blue", "Light blue") ~ "blue",
        tolower(Color) == "clear" ~ "clear",
        Color %in% c("Black", "Gray", "Gold/Black") ~ "black",
        Color %in% c("Red", "Maroon", "Pink", "Red/Black/Clear", "Red/Black", "red") ~ "red",
        Color %in% c("Burgendy", "Tan", "brown", "Brown") ~ "brown",
        Color %in% c("White", "White/Blue", "Silver", "white") ~ "white",
        Color %in% c("Green", "Dark Green") ~ "green",
        Color %in% c("Orange") ~ "orange",
        Color %in% c("Yellow", "Gold") ~ "yellow",
        Color %in% c("Purple") ~ "purple",
        TRUE ~ as.character(Color)
      ),
      
      sample_type = case_when(
        str_detect(Sample_ID, regex("blank", ignore_case = TRUE)) ~ "field blank",
        TRUE ~ "river water"
      )
    ),
  
  # ---- Lab blanks
  read_xlsx("LabBlanks_10.30.25.xlsx", sheet = 2) %>%
    rename(
      Sample_ID    = Blank_ID,
      Date_Sampled = Date_Picked
    ) %>%
    mutate(
      Morphology = case_when(
        Morphology %in% c("Fiber", "Fiber Clump", "fiber", "fiber clump", "Fibers", "fibers") ~ "fiber",
        Morphology %in% c("Fragment", "fragment") ~ "fragment",
        tolower(Morphology) == "film" ~ "film",
        Morphology %in% c("Foams", "Foam") ~ "foam",
        TRUE ~ as.character(Morphology)
      ),
      
      Color = case_when(
        Color %in% c("Navy blue", "Blue", "Light Blue", "Navy Blue", "Teal", "blue", "Light blue") ~ "blue",
        tolower(Color) == "clear" ~ "clear",
        Color %in% c("Black", "Gray", "Gold/Black") ~ "black",
        Color %in% c("Red", "Maroon", "Pink", "Red/Black/Clear", "Red/Black", "red") ~ "red",
        Color %in% c("Burgendy", "Tan", "brown", "Brown") ~ "brown",
        Color %in% c("White", "White/Blue", "Silver", "white") ~ "white",
        Color %in% c("Green", "Dark Green") ~ "green",
        Color %in% c("Orange") ~ "orange",
        Color %in% c("Yellow", "Gold") ~ "yellow",
        Color %in% c("Purple") ~ "purple",
        TRUE ~ as.character(Color)
      ),
      
      sample_type = "lab blank"
    )
)






# Small microplastics----
Part_dets_comb <- readRDS("../W2W_MBNMS/Part_dets_final.rds")

Part_dets_summ <- readRDS("../W2W_MBNMS/Part_dets_summ_final.rds") %>%
  # Correct sample IDs
  mutate(
    Client_ID_MSSupdate = dplyr::recode(
      Client_ID_MSSupdate,
      "CR020250611S"     = "CR020250611SS",
      "SRR20240205SS_2" = "SRR20240205SS"
    )
  )

# Pull out and combine the SRR20240205SS duplicate records
SRR20240205_combined <- Part_dets_summ %>%
  filter(Client_ID_MSSupdate == "SRR20240205SS") %>%
  group_by(material_class) %>%
  summarise(
    # Sum quantitative particle measurements
    count = sum(count, na.rm = TRUE),
    extrap_count = sum(extrap_count, na.rm = TRUE),
    extrap_conc_PPL = sum(extrap_conc_PPL, na.rm = TRUE),
    
    # Keep the first value for the remaining columns
    across(
      -c(count, extrap_count, extrap_conc_PPL),
      ~ first(.x)
    ),
    
    .groups = "drop"
  )

# Remove the original SRR20240205SS rows and replace with combined rows
Part_dets_summ <- Part_dets_summ %>%
  filter(Client_ID_MSSupdate != "SRR20240205SS") %>%
  bind_rows(SRR20240205_combined)





#refine dataset for what's needed here

Part_dets_comb_river <- Part_dets_comb %>%
  dplyr::filter(
    sample_type == "river water",
  ) %>% 
mutate(
    # extract the first 8-digit block from the ID
    date_raw = str_extract(Client_ID_MSSupdate, "\\d{8}"),
    
    # convert YYYYMMDD → Date
    date = ymd(date_raw)
  )

saveRDS(
  Part_dets_comb_river,
  file = "Part_dets_comb_river.rds"
)


Part_dets_summ_river <- Part_dets_summ %>%
  filter(sample_type == "river water") %>% 
  mutate(
    # extract the first 8-digit block from the ID
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
    sample_depth_general = dplyr::recode(
      sample_depth_general,
      "depth" = "subsurface"
    ),
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("subsurface", "surface")
    ),
    sampling_season = factor(
      if_else(
        date < as.Date("2024-08-01"),
        "Season 1",
        "Season 2"
      ),
      levels = c("Season 1", "Season 2")
    )
  ) %>%
  rename(river = sample_location)





###### River flow data---- 
#from: https://waterdata.usgs.gov/

# Define site and dates

#Carmel river USGS flow data
site <- "11143250"
startDate <- "2023-09-01"
endDate   <- "2025-09-01"

# Parameter code for discharge (cubic feet per second)
pCode <- "00060"

#Salinas river USGS flow data
carmel_flow <- readNWISdv(siteNumbers = site,
                          parameterCd = pCode,
                          startDate = startDate,
                          endDate = endDate)

# Clean up column names
carmel_flow <- renameNWISColumns(carmel_flow)

# View the first rows
head(carmel_flow)


#Salinas river USGS flow data
site <- "11152500"        # Salinas River at Spreckels, CA
pCode <- "00060"          # Discharge (cfs)

startDate <- "2023-09-01"
endDate   <- "2025-09-01"

salinas_flow <- readNWISdv(
  siteNumbers = site,
  parameterCd = pCode,
  startDate = startDate,
  endDate = endDate
)

head(salinas_flow)



#Pajaro river USGS flow data
site <- "11159500"        # Pajaro River at Chittenden, CA
pCode <- "00060"          # Discharge (cfs)

startDate <- "2023-09-01"
endDate   <- "2025-09-01"

pajaro_flow <- readNWISdv(
  siteNumbers = site,
  parameterCd = pCode,
  startDate = startDate,
  endDate = endDate
)

head(pajaro_flow)



#San Lorenzo river USGS flow data
site <- "11161000"        # San Lorenzo River
pCode <- "00060"          # Discharge (cfs)

startDate <- "2023-09-01"
endDate   <- "2025-09-01"

sanlorenzo_flow <- readNWISdv(
  siteNumbers = site,
  parameterCd = pCode,
  startDate = startDate,
  endDate = endDate
)

head(sanlorenzo_flow)






# Add river name to each dataset
carmel_flow <- carmel_flow %>% 
  mutate(river = "Carmel")

salinas_flow <- salinas_flow %>% 
  mutate(river = "Salinas")

pajaro_flow <- pajaro_flow %>% 
  mutate(river = "Pajaro")

sanlorenzo_flow <- sanlorenzo_flow %>% 
  mutate(river = "San Lorenzo")

# Combine into one dataframe
all_rivers_flow <- bind_rows(
  carmel_flow,
  salinas_flow,
  pajaro_flow,
  sanlorenzo_flow
) %>%
  janitor::clean_names() %>%
  # Combine columns: keep the value that exists (non-NA)
  mutate(
    Flow_cfps = coalesce(flow, x_00060_00003),
    Flow_cd   = coalesce(flow_cd, x_00060_00003_cd)
  ) %>%
  select(date, Flow_cfps, Flow_cd, river)

# Add a new column for cubic meters per second
all_rivers_flow <- all_rivers_flow %>%
  mutate(
    Flow_m3s = Flow_cfps * 0.0283168, 
    sampling_year = case_when(
      date < as.Date("2024-10-01")  ~ "Year 1",
      date >= as.Date("2024-10-01") ~ "Year 2"
    )
  ) %>%
  arrange(river, sampling_year, date) %>%
  group_by(river, sampling_year) %>%
  mutate(
    Flow_m3s_cumsum = cumsum(Flow_m3s * 86400) - first(Flow_m3s)
  ) %>%
  ungroup()







#combine the data----


Part_dets_river_full <- Part_dets_summ_river %>%
  right_join(
    all_rivers_flow,
    by = c("date", "river")
  )





# Hourly flow data for "first flush" estimates-----


flow_quantiles <- all_rivers_flow %>%
  group_by(river) %>%
  summarise(
    p25_flow   = quantile(Flow_m3s, 0.25, na.rm = TRUE),
    median_flow = median(Flow_m3s, na.rm = TRUE),
    p75_flow   = quantile(Flow_m3s, 0.75, na.rm = TRUE),
    p90_flow = quantile(Flow_m3s, 0.90, na.rm = TRUE),
    n          = sum(!is.na(Flow_m3s)),
    .groups = "drop"
  )

#View(flow_quantiles)

#select first time in season when it crosses the 90% percentile of flow


flow_hourly_SanLorenzo <- readNWISuv(
  siteNumbers = "11161000",
  parameterCd = "00060",   # Discharge
  startDate   = "2023-12-24",
  endDate     = "2024-01-09"
)


flow_hourly_SanLorenzo_clean <- flow_hourly_SanLorenzo %>%
  rename(
    Flow_cfs = X_00060_00000
  ) %>%
  mutate(
    date_time = as.POSIXct(dateTime, tz = "UTC"),
    Flow_m3s  = Flow_cfs * 0.0283168
  ) %>%
  select(site_no, date_time, Flow_cfs, Flow_m3s)




flow_hourly_Pajaro <- readNWISuv(
  siteNumbers = "11159500",
  parameterCd = "00060",   # Discharge
  startDate   = "2024-01-16",
  endDate     = "2024-01-30"
)

flow_hourly_Pajaro_clean <- flow_hourly_Pajaro %>%
  rename(
    Flow_cfs = X_00060_00000
  ) %>%
  mutate(
    date_time = as.POSIXct(dateTime, tz = "UTC"),
    Flow_m3s  = Flow_cfs * 0.0283168
  ) %>%
  select(site_no, date_time, Flow_cfs, Flow_m3s)



flow_hourly_Salinas <- readNWISuv(
  siteNumbers = "11152500",
  parameterCd = "00060",   # Discharge
  startDate   = "2024-01-30",
  endDate     = "2024-02-15"
)


flow_hourly_Salinas_clean <- flow_hourly_Salinas %>%
  rename(
    Flow_cfs = X_00060_00000
  ) %>%
  mutate(
    date_time = as.POSIXct(dateTime, tz = "UTC"),
    Flow_m3s  = Flow_cfs * 0.0283168
  ) %>%
  select(site_no, date_time, Flow_cfs, Flow_m3s)



flow_hourly_Carmel <- readNWISuv(
  siteNumbers = "11143250",
  parameterCd = "00060",   # Discharge
  startDate   = "2024-01-26",
  endDate     = "2024-02-15"
)


flow_hourly_Carmel_clean <- flow_hourly_Carmel %>%
  rename(
    Flow_cfs = X_00060_00000
  ) %>%
  mutate(
    date_time = as.POSIXct(dateTime, tz = "UTC"),
    Flow_m3s  = Flow_cfs * 0.0283168
  ) %>%
  select(site_no, date_time, Flow_cfs, Flow_m3s)


# Data in super high res, every 15 minutes
all_FF_flow <- bind_rows(
  flow_hourly_SanLorenzo_clean %>%
    mutate(river = "San Lorenzo"),
  
  flow_hourly_Pajaro_clean %>%
    mutate(river = "Pajaro"),
  
  flow_hourly_Salinas_clean %>%
    mutate(river = "Salinas"),
  
  flow_hourly_Carmel_clean %>%
    mutate(river = "Carmel")
) 




