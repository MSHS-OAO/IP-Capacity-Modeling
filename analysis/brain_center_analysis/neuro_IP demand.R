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

#  Constants -----------------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# read in neuro codes and isolate DRGs
neuro_drg <- read_csv(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Neurosurgery DRG.csv")) 
neuro_drg_codes <- neuro_drg$`MS-DRG`

neuro_cpt <- read_csv(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Neurosurgery CPT.csv"))
neuro_cpt_codes <- as.character(neuro_cpt$`CPT Code`)

neuro_npi <- read_xlsx(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Faculty and NPI.xlsx")) 
neuro_npi_codes <- trimws(neuro_npi$NPI)

#  DRG Patient Identification -------------------------------------------------
# read in all bedcharges for neuro DRGs for MSH, MSM, MSW
neuro_ip_encounters <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>% 
  filter(FACILITY_MSX %in% c("MSH", "RVT", "STL"),
         MSDRG_CD_SRC %in% neuro_drg_codes)  %>%
  collect() 

# isolate the neuro encounters
neuro_encounters_drg <- unique(neuro_ip_encounters$ENCOUNTER_NO)

# CPT Patient Identification --------------------------------------------------
neuro_or_cases <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>%
  filter(FACILITY_MSX %in% c("MSH", "RVT", "STL"),
         PRIMARY_PROC_CODE %in% neuro_cpt_codes,
         trimws(CASE_STATUS) == "Completed") %>%
  collect()

# isolate the neuro encounters
neuro_encounters_cpt <- unique(neuro_or_cases$ENCOUNTER_NO)

# CPT Patient Identification --------------------------------------------------
neuro_surgeon_cases <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>%
  filter(FACILITY_MSX %in% c("MSH", "RVT", "STL"),
         SURGEON_NPI %in% neuro_npi_codes,
         trimws(CASE_STATUS) == "Completed") %>%
  collect()

# isolate the neuro encounters
neuro_encounters_npi <- unique(neuro_surgeon_cases$ENCOUNTER_NO)

# compare encounter lists -----------------------------------------------------
# # encounters found with cpt but not with drg
# cpt_not_drg <- setdiff(neuro_encounters_cpt, neuro_encounters_drg)
# # encounters found with drg but not with cpt
# drg_not_cpt <- setdiff(neuro_encounters_drg, neuro_encounters_cpt)
# 
# # encounters found with both drg and cpt
# cpt_and_drg <- intersect(neuro_encounters_cpt, neuro_encounters_drg)

# all unique encounters identified with drg or cpt
all_neuro_encounters <- unique(c(
  neuro_encounters_cpt, 
  neuro_encounters_drg, 
  neuro_encounters_npi))

# Encounter ------------------------------------------
surgeons <- tbl(con_prod, "MSX_PROVIDER_V") %>%
  select(NPI, LAST_NAME, FIRST_NAME, DEPT_SPEC1_DESC) %>%
  collect()

neuro_case_encounters <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>%
  filter(ENCOUNTER_NO %in% all_neuro_encounters) %>%
  collect() %>%
  left_join(surgeons, by = c("SURGEON_NPI" = "NPI"))

nuero_proc_check <- neuro_case_encounters %>%
  filter(trimws(CASE_STATUS) == "Completed") %>%
  group_by(MSDRG_CD_SRC, MSDRG_DESC_MSX) %>%
  summarise(
    TOTAL_PROCEDURES = n(),
    TOTAL_NEURO = sum(PRIMARY_PROC_CODE %in% neuro_cpt_codes),
    TOTAL_NON_NEURO = TOTAL_PROCEDURES - TOTAL_NEURO,
    NEURO_PCT = TOTAL_NEURO/TOTAL_PROCEDURES * 100) %>%
  arrange(desc(TOTAL_PROCEDURES))

no_case_check <- neuro_case_encounters %>%
  filter(is.na(OR_CASE_ID))
## Surgeon ----------------------------------------------------------- 
neuro_encounter_surgeon <- neuro_case_encounters %>%
  group_by(DEPT_SPEC1_DESC, SURGEON_NPI, LAST_NAME, FIRST_NAME, FACILITY_MSX) %>%
  summarise(ENCOUNTERS = n_distinct(ENCOUNTER_NO), .groups = "drop") %>%
  pivot_wider(id_cols = c(DEPT_SPEC1_DESC, SURGEON_NPI, LAST_NAME, FIRST_NAME),
              names_from = FACILITY_MSX,
              values_from = ENCOUNTERS) %>%
  mutate(TOTAL_ENCOUNTERS = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
  arrange(desc(TOTAL_ENCOUNTERS)) %>%
  mutate(
    PCT = TOTAL_ENCOUNTERS / sum(TOTAL_ENCOUNTERS) * 100,
    CUM_PCT = cumsum(PCT))
### majority of NAs coming from encounters that hit DRG inclusion but did not get procedure
### some of these patients had organ procurement procedure without surgeon listed

## DRG -------------------------------------------------------------------------
neuro_encounter_drg <- neuro_case_encounters %>%
  group_by(MSDRG_CD_SRC, MSDRG_DESC_MSX, FACILITY_MSX) %>%
  summarise(ENCOUNTERS = n_distinct(ENCOUNTER_NO), .groups = "drop") %>%
  pivot_wider(id_cols = c(MSDRG_CD_SRC, MSDRG_DESC_MSX),
              names_from = FACILITY_MSX,
              values_from = ENCOUNTERS) %>%
  mutate(TOTAL_ENCOUNTERS = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
  arrange(desc(TOTAL_ENCOUNTERS)) %>%
  mutate(
    PCT = TOTAL_ENCOUNTERS / sum(TOTAL_ENCOUNTERS) * 100,
    CUM_PCT = cumsum(PCT))

# Bed Demand -----------------------------------------

# get bedcharges for entire neuro population
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
        FACILITY_MSX, ENCOUNTER_NO, MSDRG_CD_SRC, MSDRG_DESC_MSX, LOC_NAME, 
        ATTENDING_VERITY_REPORT_SERVICE, PRINCIPAL_SURGEON_CD_SRC, PRINCIPAL_SURGEON_NAME_MSX,
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

## Service Group --------------------------------------------------------------------
# summarise at service group level
bed_demand_service_group <- neuro_bed_charges %>%
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

## DRG -----------------------------------------------------------
# calc encounter count at DRG level
drg_counts <- neuro_bed_charges %>%
  group_by(MSDRG_CD_SRC) %>%
  summarise(TOTAL_ENCOUNTERS = n_distinct(ENCOUNTER_NO), .groups = "drop")

# get bed demand metrics at service group level
bed_demand_drg <- neuro_bed_charges %>%
  filter(SERVICE_GROUP %in% c("Med Surg", "Medicine", "Critical Care", "Pediatric Critical Care", "Rehab")) %>%
  group_by(MSDRG_CD_SRC, MSDRG_DESC_MSX, SERVICE_GROUP) %>%
  summarise(
    TOTAL_BEDCHARGES = sum(BED_CHARGES, na.rm = TRUE),
    BEDCHARGES_PER_DAY = sum(BED_CHARGES, na.rm = TRUE) / 365,
    .groups = "drop") %>%
  left_join(drg_counts,
            by = "MSDRG_CD_SRC") %>%
  arrange(desc(TOTAL_ENCOUNTERS), desc(TOTAL_BEDCHARGES))

## Hospital --------------------------------------------------------------------
bed_demand_hosp <- neuro_bed_charges %>%
  group_by(FACILITY_MSX, SERVICE_GROUP) %>%
  summarise(
    BEDCHARGES_PER_DAY = sum(BED_CHARGES, na.rm = TRUE) / 365,
    .groups = "drop") %>% 
  pivot_wider(
    names_from = FACILITY_MSX, 
    values_from = BEDCHARGES_PER_DAY
  )

## Surgeon --------------------------------------------------------------------
surgeons <- tbl(con_prod, "MSX_PROVIDER_V") %>% collect() %>%
  filter(MSH_PROV_CD %in% neuro_bed_charges$PRINCIPAL_SURGEON_CD_SRC,
         !is.na(MSH_PROV_CD)) %>%
  select(NPI, MSH_PROV_CD, DEPT_SPEC1_DESC)

bed_demand_surgeon <- neuro_bed_charges %>%
  filter(SERVICE_GROUP %in% c("Med Surg", "Medicine", "Critical Care", "Pediatric Critical Care", "Rehab")) %>%
  left_join(surgeons,
            by = c("PRINCIPAL_SURGEON_CD_SRC" = "MSH_PROV_CD")) %>%
  group_by(NPI, PRINCIPAL_SURGEON_NAME_MSX, DEPT_SPEC1_DESC, SERVICE_GROUP) %>%
  summarise(
    TOTAL_BEDCHARGES = sum(BED_CHARGES, na.rm = TRUE),
    BEDCHARGES_PER_DAY = sum(BED_CHARGES, na.rm = TRUE) / 365,
    .groups = "drop")

# Extracts --------------------------------------------------------------------
## IP OR Cases ------------------------------------------
neuro_ip_cases <- tbl(con_prod, "IPCAP_OR_CASE_DATA") %>%
  filter(ENCOUNTER_NO %in% all_neuro_encounters) %>%
  arrange(ENCOUNTER_NO) %>%
  collect()

writexl::write_xlsx(neuro_ip_cases, "2025 IP Neuro Cases.xlsx")
