library(knitr)
library(tidyverse)
library(odbc)
library(DBI)
library(glue)
library(dplyr)
library(tidyr)
library(dbplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(openxlsx)
library(readxl)
library(rmarkdown)

# -------------------------------------------------------- Load Data -------------------------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# Load Baseline Data
baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
  filter(FACILITY_MSX != 'MSSM') %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
    LOC_NAME = case_when(
      LOC_NAME == 'THE MOUNT SINAI HOSPITAL' ~ 'MSH',
      LOC_NAME == 'MOUNT SINAI QUEENS'       ~ 'MSQ',
      LOC_NAME == 'MOUNT SINAI BROOKLYN'     ~ 'MSB',
      LOC_NAME == 'MOUNT SINAI BETH ISRAEL'  ~ 'MSBI',
      LOC_NAME == 'MOUNT SINAI MORNINGSIDE'  ~ 'MSM',
      LOC_NAME == 'MOUNT SINAI WEST'         ~ 'MSW',
      TRUE ~ LOC_NAME),
    FACILITY_MSX = case_when(
      FACILITY_MSX == "BIB" ~ "MSB",
      FACILITY_MSX == "BIP" ~ "MSBI",
      FACILITY_MSX == "RVT" ~ "MSW",
      FACILITY_MSX == "STL" ~ "MSM",
      TRUE ~ FACILITY_MSX)) %>%
  filter(
    (SERVICE_DATE >= as.Date("2025-07-01") & SERVICE_DATE <= as.Date("2025-12-31")) | (SERVICE_DATE >= as.Date("2026-03-01") & SERVICE_DATE <= as.Date("2026-06-30")),
    FACILITY_MSX != "MSSN"
  )



#pool NA SERVICE_GROUP vals as "Other"
baseline <- baseline %>%
  mutate(
    SERVICE_GROUP = if_else(
      is.na(SERVICE_GROUP),
      "Other",
      SERVICE_GROUP
    )
  )

baseline <- baseline %>%
  mutate(
    LOC_NAME = case_when(
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSH" ~ "MSH",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSQ" ~ "MSQ",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSBI" ~ "MSBI",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSB" ~ "MSB",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSM" ~ "MSM",
      SERVICE_GROUP == "Other" & FACILITY_MSX == "MSW" ~ "MSW",
      TRUE ~ LOC_NAME
    )
  )
#  ---------------------------------------------------------------- Render Models ----------------------------------------------------------------

# load all functions
source("functions/location_swap.R")
source("functions/emergency_exclusion.R")
source("functions/los_adjustment.R")
source("functions/unit_capacity.R")
source("functions/excel_add_to_wb.R")
source("functions/save_parameters.R")
source("functions/volume_projections.R")
source("functions/dow_service_group.R")
source("functions/dow_unit.R")
source("functions/excel_add_to_wb_dow.R")
source("functions/NA_cleanup.R")
source("functions/daily_demand.R")
source("model/model-ip_daily_demand.R")


# ---------------------------------------------------------- Scenario Parameters ----------------------------------------------------------

# calculate # of weekdays and # of all days in dataset
all_dates <- seq(min(baseline$SERVICE_DATE),max(baseline$SERVICE_DATE), by = "day")
num_days <- length(all_dates)
num_weekdays <- sum(!wday(all_dates) %in% c(1, 7))
num_weekend_days <- sum(wday(all_dates) %in% c(1, 7))
dow_counts <- table(
  factor(
    toupper(weekdays(all_dates)),
    levels = c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
  )
)



# AGG X AGG
# vol_projections <- list(
#   "2026" = "BCG_2026_AGG.csv",
#   "2027" = "BCG_2027_AGG.csv" ,
#   "2028" = "BCG_2028_AGG.csv",
#   "2029" = "BCG_2029_AGG.csv",
#   "2030" = "BCG_2030_AGG.csv"
# )
# 
# los_projections <- list(
#   "2026" = "BCG_2026_AGG.csv",
#   "2027" = "BCG_2027_AGG.csv",
#   "2028" = "BCG_2028_AGG.csv",
#   "2029" = "BCG_2029_AGG.csv",
#   "2030" = "BCG_2030_AGG.csv"
# )


# # MEDIUM 1 (AGG X CON)
# vol_projections <- list(
#   "2026" = "BCG_2026_AGG.csv",
#   "2027" = "BCG_2027_AGG.csv" ,
#   "2028" = "BCG_2028_AGG.csv",
#   "2029" = "BCG_2029_AGG.csv",
#   "2030" = "BCG_2030_AGG.csv"
# )  
# 
# los_projections <- list(
#   "2026" = "BCG_2026_CON.csv",
#   "2027" = "BCG_2027_CON.csv",
#   "2028" = "BCG_2028_CON.csv",
#   "2029" = "BCG_2029_CON.csv",
#   "2030" = "BCG_2030_CON.csv"
# )

# 
#   
# 
# # MEDIUM 2  (CON X AGG)
# vol_projections <- list(
#   "2026" = "BCG_2026_CON.csv",
#   "2027" = "BCG_2027_CON.csv" ,
#   "2028" = "BCG_2028_CON.csv",
#   "2029" = "BCG_2029_CON.csv",
#   "2030" = "BCG_2030_CON.csv"
# )
# 
# los_projections <- list(
#   "2026" = "BCG_2026_AGG.csv",
#   "2027" = "BCG_2027_AGG.csv",
#   "2028" = "BCG_2028_AGG.csv",
#   "2029" = "BCG_2029_AGG.csv",
#   "2030" = "BCG_2030_AGG.csv"
# )
# 
# 
# 
# 
# # # CON X CON
# vol_projections <- list(
#   "2026" = "BCG_2026_CON.csv",
#   "2027" = "BCG_2027_CON.csv" ,
#   "2028" = "BCG_2028_CON.csv",
#   "2029" = "BCG_2029_CON.csv",
#   "2030" = "BCG_2030_CON.csv"
# )
# 
# los_projections <- list(
#   "2026" = "BCG_2026_CON.csv",
#   "2027" = "BCG_2027_CON.csv",
#   "2028" = "BCG_2028_CON.csv",
#   "2029" = "BCG_2029_CON.csv",
#   "2030" = "BCG_2030_CON.csv"
# )

# final
vol_projections <- list(
  "2027" = "BCG_2026_AGG.csv",
  "2028" = "BCG_2027_AGG.csv" ,
  "2029" = "BCG_2028_AGG.csv",
  "2030" = "BCG_2029_AGG.csv",
  "2031" = "BCG_2030_AGG.csv"
)


los_projections <- list(
  "2027" = "BCG_2026_AGGX2.csv",
  "2028" = "BCG_2027_AGGX2.csv" ,
  "2029" = "BCG_2028_AGGX2.csv",
  "2030" = "BCG_2029_AGGX2.csv",
  "2031" = "BCG_2030_AGGX2.csv"
)


los_validation <- list()
n_simulations <- 1

model_results  <- ip_daily_demand_model(
    n_simulations = n_simulations,
    vol_projections,
    los_projections,
    baseline
  )


results <- model_results$average_daily_demand

# OR
encounter_scenario_outputs <-
  model_results$encounter_scenario_outputs


averaged_results <- results %>%
  group_by(
    LOC_NAME,
    SERVICE_GROUP,
    PROJECTION_YEAR
  ) %>%
  summarise(
    AVERAGE_DAILY_DEMAND = round(
      mean(AVG_DAILY_DEMAND_SCENARIO, na.rm = TRUE),
      2
    ),
    .groups = "drop"
  ) %>%
  mutate(
    PROJECTION_YEAR = paste0(
      PROJECTION_YEAR,
      "_average_daily_demand"
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = PROJECTION_YEAR,
    values_from = AVERAGE_DAILY_DEMAND,
    names_sort = TRUE
  ) %>%
  rename(
    SERVICE_LINE = SERVICE_GROUP
  ) %>%
  arrange(
    LOC_NAME,
    SERVICE_LINE
  )
  
