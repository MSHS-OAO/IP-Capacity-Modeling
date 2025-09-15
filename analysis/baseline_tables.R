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
cap_dir <- "/SharedDrive/deans/Presidents/SixSigma/Project Based/System/Capacity Modeling/"
# get days in baseline
num_days <- as.numeric(difftime(as.Date("2024-12-31"), as.Date("2024-06-01"), units = "days")) + 1

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
  group_by(EPIC_DEPT_ID, EXTERNAL_NAME, SERVICE_GROUP,  SERVICE_MONTH, SERVICE_DATE) %>%
  summarise(DAILY_DEMAND = sum(QUANTITY, na.rm = TRUE)) %>%
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
  group_by(HOSPITAL, EPIC_DEPT_ID, EXTERNAL_NAME, SERVICE_GROUP) %>%
  summarise(AVG_DAILY_DEMAND = sum(DAILY_DEMAND)/num_days,
            AVG_BED_CAPACITY = sum(AVG_BED_CAPACITY)/num_days,
            AVG_UTILIZATION = AVG_DAILY_DEMAND/AVG_BED_CAPACITY,
            AVG_PERCENT_85 = sum(UTILIZATION_85)/num_days,
            AVG_PERCENT_95 = sum(UTILIZATION_95)/num_days)

# get avg unit los
avg_unit_los <- baseline %>%
  group_by(ENCOUNTER_NO, EXTERNAL_NAME) %>%
  summarise(UNIT_LOS = sum(QUANTITY, na.rm = TRUE)) %>%
  group_by(EXTERNAL_NAME) %>%
  summarise(AVG_UNIT_LOS = mean(UNIT_LOS))

# get avg total los and expected LOS
avg_los <- baseline %>%
  group_by(EXTERNAL_NAME) %>%
  summarise(AVG_TOTAL_LOS = mean(LOS_NO_SRC),
            AVG_TOTAL_EXPECTED_LOS = mean(P_AVG_LOS_MSDRG, na.rm = TRUE))

# join all LOS metrics
baseline_avg_los <- baseline_avg %>%
  left_join(avg_unit_los, by = c("EXTERNAL_NAME" = "EXTERNAL_NAME")) %>%
  left_join(avg_los, by = c("EXTERNAL_NAME" = "EXTERNAL_NAME"))

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

# get linear regression slope of bed charges over time
baseline_regression <- baseline_daily_avg %>%
  mutate(DATE_NUM = as.numeric(SERVICE_DATE)) %>%
  group_by(EXTERNAL_NAME) %>%
  summarise(AVG_BED_CAPACITY = sum(AVG_BED_CAPACITY)/num_days,
            REGRESSION = coef(lm(DAILY_DEMAND ~ DATE_NUM))[2]) %>%
  mutate(NORMALIZED_REGRESSION = round(REGRESSION/AVG_BED_CAPACITY, digits = 4),
         REGRESSION = round(REGRESSION, digits = 4)) %>%
  select(EXTERNAL_NAME, contains("REGRESSION"))

# join in regression metrics
baseline_los_admissions_regression <- baseline_los_admissions %>%
  left_join(baseline_regression, by = c("EXTERNAL_NAME" = "EXTERNAL_NAME")) %>%
  arrange(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME)

write.xlsx(baseline_los_admissions_regression,
           paste0(cap_dir, "Model Outputs/Baseline Metrics/",
                  "BASELINE_METRICS_", Sys.Date(), ".xlsx"))




check <- baseline %>%
  mutate(LOS_VAR = LOS_NO_SRC - P_AVG_LOS_MSDRG) %>%
  summarise(avg_var = mean(LOS_VAR, na.rm = TRUE))
