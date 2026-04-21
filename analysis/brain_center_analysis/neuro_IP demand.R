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
library(tsibble)
library(fable)

# ----------------------------------- Constants --------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"


# ---------------------------------- DRG Patient Identification --------------------------------
# read in neuro codes and isolate DRGs
neuro_drg <- read_csv("Neurosurgery DRG.csv") 
neuro_drg_codes <- neuro_drg$`MS-DRG`

neuro_cpt <- read_csv("Neurosurgery CPT.csv")
neuro_cpt_codes <- as.character(neuro_cpt$`CPT Code`)

# read in all bedcharges for neuro DRGs for MSH, MSM, MSW
neuro_ip_encounters <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>% 
  filter(FACILITY_MSX %in% c("MSH", "RVT", "STL"),
         MSDRG_CD_SRC %in% neuro_drg_codes) %>%
  collect() 

# isolate the neuro encounters
neuro_encounters_drg <- unique(neuro_ip_encounters$ENCOUNTER_NO)

# ---------------------------------- CPT Patient Identification --------------------------------
neuro_or_cases <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>%
  filter(FACILITY_MSX %in% c("MSH", "RVT", "STL"),
         PRIMARY_PROC_CODE %in% neuro_cpt_codes) %>%
  collect()

# isolate the neuro encounters
neuro_encounters_cpt <- unique(neuro_or_cases$ENCOUNTER_NO)

# ------------------------------- compare encounter lists ---------------------
# encounters found with cpt but not with drg
cpt_not_drg <- setdiff(neuro_encounters_cpt, neuro_encounters_drg)
# encounters found with drg but not with cpt
drg_not_cpt <- setdiff(neuro_encounters_drg, neuro_encounters_cpt)

# encounters found with both drg and cpt
cpt_and_drg <- intersect(neuro_encounters_cpt, neuro_encounters_drg)

# all unique encounters identified with drg or cpt
all_neuro_encounters <- unique(union(neuro_encounters_cpt, neuro_encounters_drg))

# ------------------------ Bed Demand -----------------------------------------

### ASSUMPTION: use all encounters identified with either cpt or drg ##########

neuro_bed_charges <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% 
  filter(ENCOUNTER_NO %in% all_neuro_encounters) %>%
  collect() %>%
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
      TRUE ~ FACILITY_MSX),
    SERVICE_GROUP = ifelse(is.na(SERVICE_GROUP), "OTHER", SERVICE_GROUP)) %>%
  group_by(
        FACILITY_MSX, ENCOUNTER_NO, MSDRG_CD_SRC, MSDRG_DESC_MSX, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE,
        DSCH_UNIT_DESC_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH,
        SERVICE_DATE, LOS_NO_SRC
      ) %>%
  summarise(
    BED_CHARGES = sum(QUANTITY, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    BED_CHARGES = case_when(
      BED_CHARGES > 1 ~ 1,
      TRUE ~ BED_CHARGES
    )
  ) %>%
  filter(BED_CHARGES != 0)

# Encounter check for encounters identified as neuro but no bed charge
no_bedcharge <- unique(setdiff(all_neuro_encounters, neuro_bed_charges$ENCOUNTER_NO))

# calc encounter count at DRG level
drg_counts <- neuro_bed_charges %>%
  group_by(MSDRG_CD_SRC) %>%
  summarise(TOTAL_ENCOUNTERS = n_distinct(ENCOUNTER_NO), .groups = "drop")

# get bed demand metrics at service group level
bed_demand_drg <- neuro_bed_charges %>%
  group_by(MSDRG_CD_SRC, MSDRG_DESC_MSX, SERVICE_GROUP) %>%
  summarise(
    TOTAL_BEDCHARGES = sum(BED_CHARGES, na.rm = TRUE),
    BEDCHARGES_PER_DAY = sum(BED_CHARGES, na.rm = TRUE) / 365,
    .groups = "drop") %>%
  left_join(drg_counts, by = "MSDRG_CD_SRC") %>%
  mutate(BEDCHARGES_PER_ENCOUNTER = TOTAL_BEDCHARGES / TOTAL_ENCOUNTERS)

# summarise at service group level
bed_demand <- neuro_bed_charges %>%
  group_by(SERVICE_GROUP) %>%
  summarise(
    TOTAL_BEDCHARGES = sum(BED_CHARGES, na.rm = TRUE),
    BEDCHARGES_PER_DAY = sum(BED_CHARGES, na.rm = TRUE) / 365,
    .groups = "drop") %>%
  mutate(TOTAL_ENCOUNTERS = length(unique(neuro_bed_charges$ENCOUNTER_NO)),
         BEDCHARGES_PER_ENCOUNTER = TOTAL_BEDCHARGES / TOTAL_ENCOUNTERS) %>%
  arrange(desc(TOTAL_BEDCHARGES)) %>%
  bind_rows(
    summarise(., 
              SERVICE_GROUP = "Total",
              across(where(is.numeric), sum))
  ) %>%
  mutate(TOTAL_ENCOUNTERS = if_else(SERVICE_GROUP == "Total",
                                    length(unique(neuro_bed_charges$ENCOUNTER_NO)),
                                    TOTAL_ENCOUNTERS))

### qc #######################################################################

#study population with cpt match and drg mismatch
cpt_match_drg_mismatch <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>%
  collect() %>%
  filter(
    PRIMARY_PROC_CODE %in% neuro_cpt_codes &
      (
        is.na(MSDRG_CD_SRC) | 
          MSDRG_CD_SRC == "" | 
          !(MSDRG_CD_SRC %in% neuro_drg_codes)
      ) &
      FACILITY_MSX %in% c("MSH", "RVT", "STL")
  ) %>%
  count(
    MATCHED_CPT = PRIMARY_PROC_CODE, 
    MISMATCHED_DRG = MSDRG_CD_SRC, 
    MSDRG_DESC_MSX,
    sort = TRUE, 
    name = "NUM_OF_PROCEDURES"
  ) %>%
  filter(!is.na(MISMATCHED_DRG))

#study population with drg match and cpt mismatch
drg_match_cpt_mismatch <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>%
  collect() %>%
  filter(
    MSDRG_CD_SRC %in% neuro_drg_codes &
      (
        is.na(PRIMARY_PROC_CODE) |
          PRIMARY_PROC_CODE == "" |
          !(PRIMARY_PROC_CODE %in% neuro_cpt_codes)
      ) &
      FACILITY_MSX %in% c("MSH", "RVT", "STL")
  ) %>%
  select(MSDRG_CD_SRC, MSDRG_DESC_MSX, PRIMARY_PROC_CODE, PRIMARY_PROCEDURE) %>%
  count(MSDRG_CD_SRC, MSDRG_DESC_MSX, PRIMARY_PROC_CODE, PRIMARY_PROCEDURE, sort = TRUE, name = "NUM_OF_PROCEDURES") %>%
  rename(
    MATCHED_DRG = MSDRG_CD_SRC,
    MISMATCHED_CPT = PRIMARY_PROC_CODE
  ) %>%
  filter(!is.na(MISMATCHED_CPT))        
