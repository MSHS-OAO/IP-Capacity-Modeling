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

# get bed capacity data
bed_cap_csv <- list.files(paste0(cap_dir, "Tableau Data/Bed Capacity/"),
                          pattern = "\\.csv$", full.names = TRUE)
bed_cap <- bed_cap_csv %>%
  map_dfr(~ read_csv(.x, show_col_types = FALSE) %>% mutate(source_file = basename(.x))) %>%
  rename(HOSPITAL = Location,
         SERVICE_GROUP = `Service Group`,
         MEASURE = `Measure Names`,
         SERVICE_MONTH = `Day of Census Start`) %>%
  filter(MEASURE == 'Avg Total Beds') %>%
  mutate(
    HOSPITAL = case_when(
      HOSPITAL == "MOUNT SINAI BETH ISRAEL" ~ "MSBI",
      HOSPITAL == "MOUNT SINAI BROOKLYN" ~ "MSB",
      HOSPITAL == "MOUNT SINAI MORNINGSIDE" ~ "MSM",
      HOSPITAL == "MOUNT SINAI QUEENS" ~ "MSQ",
      HOSPITAL == "MOUNT SINAI WEST" ~ "MSW",
      HOSPITAL == "THE MOUNT SINAI HOSPITAL" ~ "MSH"),
    SERVICE_MONTH = mdy(SERVICE_MONTH),
    Unit = trimws(Unit)) %>%
  group_by(HOSPITAL, SERVICE_GROUP, Unit, SERVICE_MONTH) %>%
  summarise(AVG_BED_CAPACITY = sum(`Measure Values`, na.rm = TRUE))

# compute baseline daily averages
baseline_daily_avg <- baseline %>%
  filter(!is.na(EXTERNAL_NAME)) %>%
  group_by(ENCOUNTER_NO, FACILITY_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(BED_CHARGES = case_when(
    BED_CHARGES > 1 ~ 1,
    TRUE ~ BED_CHARGES)) %>%
  group_by(FACILITY_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop") %>%
  collect() %>%
  left_join(bed_cap, by = c("SERVICE_GROUP" = "SERVICE_GROUP",
                            "SERVICE_MONTH" = "SERVICE_MONTH",
                            "EXTERNAL_NAME" = "Unit")) %>%
  relocate(HOSPITAL) %>%
  mutate(UTILIZATION = DAILY_DEMAND/AVG_BED_CAPACITY,
         UTILIZATION_85 = case_when(
           UTILIZATION > .85 ~ TRUE,
           TRUE ~ FALSE),
         UTILIZATION_95 = case_when(
           UTILIZATION > .95 ~ TRUE,
           TRUE ~ FALSE))

# compute baseline total averages
baseline_avg <- baseline_daily_avg %>%
  group_by(HOSPITAL, EXTERNAL_NAME, SERVICE_GROUP) %>%
  summarise(AVG_DAILY_DEMAND = sum(DAILY_DEMAND)/num_days,
            AVG_BED_CAPACITY = sum(AVG_BED_CAPACITY)/num_days,
            AVG_UTILIZATION = AVG_DAILY_DEMAND/AVG_BED_CAPACITY,
            AVG_PERCENT_85 = sum(UTILIZATION_85)/num_days,
            AVG_PERCENT_95 = sum(UTILIZATION_95)/num_days)

baseline_unit_los <- baseline %>%
  filter(ADMIT_DT_SRC >= as.Date("2024-06-01"),
         DSCH_DT_SRC <= as.Date("2025-06-30")) %>%
  group_by(ENCOUNTER_NO, FACILITY_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
  mutate(BED_CHARGES = case_when(
    BED_CHARGES > 1 ~ 1,
    TRUE ~ BED_CHARGES))%>%
  arrange(ENCOUNTER_NO, SERVICE_DATE) %>%   
  group_by(ENCOUNTER_NO) %>%
  mutate(UNIT_CHANGE = EXTERNAL_NAME != lag(EXTERNAL_NAME),
         DATE_GAP    = SERVICE_DATE != lag(SERVICE_DATE) + days(1),
         NEW_STAY    = is.na(lag(SERVICE_DATE)) | UNIT_CHANGE | DATE_GAP,
         STAY_ID     = cumsum(NEW_STAY)) %>%
  group_by(ENCOUNTER_NO, EXTERNAL_NAME, STAY_ID) %>%
  summarise(STAY_LENGTH = as.integer(max(SERVICE_DATE) - min(SERVICE_DATE)) + 1) %>%
  group_by(EXTERNAL_NAME) %>%
  summarise(UNIT_ALOS = mean(STAY_LENGTH))

# get avg total los and expected LOS
baseline_total_los <- baseline %>%
  filter(SERVICE_DESC_MSX %in% c("REHABILITATION", "PSYCHIATRY", "CHEMICAL DEPENDENCY"),
         !is.na(VIZ_EX_LOS),
         LOS_NO_SRC < 100) %>% # align with Poppy's AVG LOS metric definition
  group_by(EXTERNAL_NAME) %>%
  summarise(ALOS = mean(LOS_NO_SRC, na.rm = TRUE),
            ELOS = mean(VIZ_EX_LOS, na.rm = TRUE),
            LOS_RATIO = ALOS/ELOS)

# join all LOS metrics
baseline_avg_los <- baseline_avg %>%
  left_join(baseline_unit_los, by = c("EXTERNAL_NAME" = "EXTERNAL_NAME")) %>%
  left_join(baseline_total_los, by = c("EXTERNAL_NAME" = "EXTERNAL_NAME"))

# get distribution of admission types
admission_dist <- baseline %>%
  filter(!is.na(ADMIT_TYPE_DESC_SRC)) %>%
  group_by(EXTERNAL_NAME, ADMIT_TYPE_DESC_SRC) %>%
  summarise(ADMIT_COUNT = n()) %>%
  pivot_wider(
    id_cols = EXTERNAL_NAME,
    names_from = ADMIT_TYPE_DESC_SRC,
    values_from = ADMIT_COUNT,
    values_fill = list(ADMIT_COUNT = 0)) %>%
  rowwise() %>%
  mutate(across(
    where(is.numeric),
    ~ round(. / sum(c_across(where(is.numeric))), 4),
    .names = "{.col}_PCT")) %>%
  select(EXTERNAL_NAME, ends_with("_PCT"))

# join in admission distributions
baseline_los_admissions <- baseline_avg_los %>%
  left_join(admission_dist, by = c("EXTERNAL_NAME" = "EXTERNAL_NAME"))

write.xlsx(baseline_los_admissions,
           paste0(cap_dir, "Model Outputs/Baseline Metrics/",
                  "BASELINE_METRICS_", Sys.Date(), ".xlsx"))
