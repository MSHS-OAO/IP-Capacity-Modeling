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


# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")
# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"
# get days in baseline
num_days <- as.numeric(difftime(as.Date("2025-06-30"), as.Date("2024-06-01"), units = "days")) + 1

# get baseline data
baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
  filter(!is.na(EXTERNAL_NAME)) %>%
  mutate(
    EXTERNAL_NAME = trimws(EXTERNAL_NAME),
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"))

# 
daily_bed_charges <- baseline %>%
  filter(!is.na(EXTERNAL_NAME),
         ADMIT_DT_SRC >= as.Date("2024-06-01"),
         DSCH_DT_SRC <= as.Date("2025-06-30")) %>%
  group_by(ENCOUNTER_NO, FACILITY_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(BED_CHARGES = case_when(
    BED_CHARGES > 1 ~ 1,
    TRUE ~ BED_CHARGES))

unit_los <- daily_bed_charges %>%
  arrange(ENCOUNTER_NO, SERVICE_DATE) %>%   
  group_by(ENCOUNTER_NO) %>%
  mutate(
    UNIT_CHANGE = EXTERNAL_NAME != lag(EXTERNAL_NAME),
    DATE_GAP    = SERVICE_DATE != lag(SERVICE_DATE) + days(1),
    NEW_STAY    = is.na(lag(SERVICE_DATE)) | UNIT_CHANGE | DATE_GAP,
    STAY_ID     = cumsum(NEW_STAY)
  ) %>%
  group_by(ENCOUNTER_NO, EXTERNAL_NAME, STAY_ID) %>%
  summarise(
    STAY_LENGTH = as.integer(max(SERVICE_DATE) - min(SERVICE_DATE)) + 1,
    STAY_START  = min(SERVICE_DATE),
    STAY_END    = max(SERVICE_DATE),
    MONTH_YEAR  = floor_date(min(SERVICE_DATE), "month"),
    .groups = "drop"
  ) %>%
  arrange(ENCOUNTER_NO, STAY_ID)

avg_unit_los <- unit_los %>%
  group_by(EXTERNAL_NAME, MONTH_YEAR) %>%
  summarise(AVG_UNIT_LOS = mean(STAY_LENGTH))

avg_unit_los_wide <- avg_unit_los %>%
  pivot_wider(id_cols = EXTERNAL_NAME, 
              names_from = MONTH_YEAR,
              values_from = AVG_UNIT_LOS)

# filter for saving
daily_bed_charges <- daily_bed_charges %>%
  filter(ENCOUNTER_NO %in% c("40000022006", "10000243149"))
unit_los <- unit_los %>%
  filter(ENCOUNTER_NO %in% c("40000022006", "10000243149"))
# Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# create sheet name
addWorksheet(wb, "Daily Bed Charges")
writeData(wb, x = head(daily_bed_charges, 100000), sheet = "Daily Bed Charges")
setColWidths(wb, "Daily Bed Charges", cols = 1:ncol(daily_bed_charges), widths = "auto")

addWorksheet(wb, "Encounter Unit LOS")
writeData(wb, x = head(unit_los, 100000), sheet = "Encounter Unit LOS")
setColWidths(wb, "Encounter Unit LOS", cols = 1:ncol(unit_los), widths = "auto")

addWorksheet(wb, "Summary")
writeData(wb, x = avg_unit_los_wide, sheet = "Summary")
setColWidths(wb, "Summary", cols = 1:ncol(avg_unit_los_wide), widths = "auto")

saveWorkbook(wb, 
             file = paste0(cap_dir, "Model Outputs/Baseline Metrics/",
                           "AVG_UNIT_LOS2_", Sys.Date(), ".xlsx"),
             overwrite = TRUE )

