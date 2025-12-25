# Utilies file to pre-load for all analysis of W2W river plastics project----

#Load packages----
library(tidyverse)
library(dataRetrieval)
library(janitor)



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

