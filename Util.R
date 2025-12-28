# Utilies file to pre-load for all analysis of W2W river plastics project----

#Load packages----
library(tidyverse)
library(dataRetrieval)
library(janitor)
library(ggExtra)
library(scales)
library(RColorBrewer)



#####Bring in data from other proj----

#large microplastics----

large_MPs_summ <- readRDS("../W2W_MBNMS/Opt_micro_all_cut.rds")

river_MPs_summ <- large_MPs_summ %>% 
  filter(sample_type == "river water") %>% 
  mutate(
    # extract the first 8-digit block from the ID
    date_raw = str_extract(Sample_ID, "\\d{8}"),
    
    # convert YYYYMMDD → Date
    date = ymd(date_raw), 
    MPs_L = Total_Count/200
  )




# Small microplastics----
Part_dets_comb <- readRDS("../W2W_MBNMS/Part_dets_comb.rds")

Part_dets_summ <- readRDS("../W2W_MBNMS/Part_dets_summ.rds")



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
    sample_depth_general = recode(sample_depth_general,
                                  "depth" = "subsurface"),
    sample_depth_general = factor(
      sample_depth_general,
      levels = c("subsurface", "surface")
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
    Flow_m3s = Flow_cfps * 0.0283168
  )





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

View(flow_quantiles)

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


# averaging out data to every hour
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
      levels = c("San Lorenzo", "Pajaro", "Salinas", "Carmel")
    )
  ) %>%
  group_by(river, date_hour) %>%
  summarise(
    Flow_m3s = mean(Flow_m3s, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    MP_flux_per_hr_yr1 = case_when(
      river == "San Lorenzo" ~ Flow_m3s * 1000 * 0.771 * 3600,
      river == "Pajaro"      ~ Flow_m3s * 1000 * 0.390 * 3600,
      river == "Salinas"     ~ Flow_m3s * 1000 * 0.198 * 3600,
      river == "Carmel"      ~ Flow_m3s * 1000 * 0.210 * 3600,
      TRUE ~ NA_real_
    ),
    MP_flux_per_hr_p25_yr1 = case_when(
      river == "San Lorenzo" ~ Flow_m3s * 1000 * 0.203 * 3600,
      river == "Pajaro"      ~ Flow_m3s * 1000 * 0.083 * 3600,
      river == "Salinas"     ~ Flow_m3s * 1000 * 0.080 * 3600,
      river == "Carmel"      ~ Flow_m3s * 1000 * 0.129 * 3600,
      TRUE ~ NA_real_
    ),
    MP_flux_per_hr_p75_yr1 = case_when(
      river == "San Lorenzo" ~ Flow_m3s * 1000 * 1.525 * 3600,
      river == "Pajaro"      ~ Flow_m3s * 1000 * 1.333 * 3600,
      river == "Salinas"     ~ Flow_m3s * 1000 * 0.458 * 3600,
      river == "Carmel"      ~ Flow_m3s * 1000 * 0.414 * 3600,
      TRUE ~ NA_real_
    )
  ) %>% 
  arrange(river, date_hour) %>%
  group_by(river) %>%
  mutate(
    MP_flux_cumulative_med = lag(cumsum(MP_flux_per_hr_yr1), default = 0),
    MP_flux_cumulative_p25 = lag(cumsum(MP_flux_per_hr_p25_yr1), default = 0),
    MP_flux_cumulative_p75 = lag(cumsum(MP_flux_per_hr_p75_yr1), default = 0)
  ) %>%
  ungroup()

