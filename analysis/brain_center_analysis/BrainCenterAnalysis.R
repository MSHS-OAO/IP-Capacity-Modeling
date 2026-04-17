rm(list = ls())
library(timeDate)
library(readxl)
library(bizdays)
library(dplyr)
library(lubridate)
library(reshape2)
library(knitr)
# library(gdtools)
# library(kableExtra)
library(kableExtra, "~/R/x86_64-pc-linux-gnu-library/4.2")
library(formattable)
library(rmarkdown)
library(stringr)
library(writexl)
library(gsubfn)
library(tidyr)
library(pool)
library(DBI)
library(odbc)
library(dbplyr)
library(glue)
library(assertr)
library(doParallel)
# library(blastula)
library(readr)
library(zip)
library(here)
library(hms)
library(ggplot2)
library(patchwork)
library(grid)
# library(ggnewscale)
library(ggtext)
library(tidyverse)
library(shadowtext)

# library(magick)

# library(ggh4x) # for x axis

# source functions ----
# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"


# ---------------------------------- Data Pull --------------------------------

# read in all bedcharges for neuro DRGs for MSH, MSM, MSW
# neuro_bed_charges <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% 
#   filter(FACILITY_MSX %in% c("MSH", "BIB", "BIP"),
#          MSDRG_CD_SRC %in% neuro_drg) %>%
#   collect() %>%
#   mutate(
#     SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
#     SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
#     LOC_NAME = case_when(
#       LOC_NAME == 'THE MOUNT SINAI HOSPITAL' ~ 'MSH',
#       LOC_NAME == 'MOUNT SINAI QUEENS'       ~ 'MSQ',
#       LOC_NAME == 'MOUNT SINAI BROOKLYN'     ~ 'MSB',
#       LOC_NAME == 'MOUNT SINAI BETH ISRAEL'  ~ 'MSBI',
#       LOC_NAME == 'MOUNT SINAI MORNINGSIDE'  ~ 'MSM',
#       LOC_NAME == 'MOUNT SINAI WEST'         ~ 'MSW',
#       TRUE ~ LOC_NAME),
#     FACILITY_MSX = case_when(
#       FACILITY_MSX == "BIB" ~ "MSB",
#       FACILITY_MSX == "BIP" ~ "MSBI",
#       FACILITY_MSX == "RVT" ~ "MSW",
#       FACILITY_MSX == "STL" ~ "MSM",
#       TRUE ~ FACILITY_MSX))

# isolate the neuro encounters
# neuro_encounters <- unique(neuro_bed_charges$ENCOUNTER_NO)

# --------------------- Table Names ---------------------
or_encounter_data_table_name <- 'MS_INSIGHT.OR_QUALITY_DASHBOARD_CASE_DETAILS'
or_room_master_data_table_name <- 'MS_INSIGHT.OR_QUALITY_DASHBOARD_ROOM_MASTER'
or_room_schedules_data_table_name <- 'MS_INSIGHT.OR_QUALITY_ROOM_UTIL_V'
ip_encounter_table_name <- 'MSX_IP_OUTPUT'

# --------------------- Filters ---------------------
# current_date <- Sys.Date()
# sched_date <- '2024-09-01'
sched_start_date <- '2025-01-01'
# sched_end_date <- '2026-03-31'
status <- 'Completed'
facilities <- "('MSH','RVT','STL')"
# room_exclusion_list <- "('MSW OR 23','MSM OR 08','MSM OR 15')"
# read in neuro codes and isolate DRGs
neuro_codes <- read_csv(paste0(cap_dir,"Adhoc/MS Brain Health/","NeuroSurgeryCodes.csv")) 
neuro_drg <- neuro_codes$`Ms Drg`
neuro_drg_query <- paste0("('", paste(neuro_drg, collapse = "', '"), "')")



# query to capture combining IP and OR data ----
ip_or_query <- glue("select d.pat_mrn_id               as or_pat_mrn_id,
                            d.patient_in_room_dttm     as or_patient_in_room_dttm,
                            d.patient_out_room_dttm    as or_patient_out_room_dttm,
                            d.or_case_id               as or_case_id,
                            d.case_status              as or_case_status,
                            d.turnover_from_prior_case as or_turnover_from_prior_case,
                            d.PRIMARY_PROC_CODE        as or_primary_procedure_code
                            rm.room_id                 as or_rm_room_id,
                            rm.location_nm             as or_rm_location_name,
                            rm.cluster_name            as or_rm_cluster_name,
                            i.encounter_no             as msxipoutput_encounter_no,
                            i.admit_dt_src             as msxipoutput_admit_dt_src,
                            i.dsch_dt_src              as msxipoutput_dsch_dt_src,
                            i.msdrg_cd_src             as msxipoutput_msdrg_cd_src,
                            i.msdrg_desc_msx           as msxipoutput_msdrg_desc_msx,
                            i.facility_msx             as msxipoutput_facility_msx
                    FROM {or_encounter_data_table_name} d 
                    LEFT JOIN {room_master_data_table_name} rm
                        ON d.or_id = rm.room_id
                    LEFT JOIN {ip_encounter_table_name} i 
                        ON i.MRN_MSX = d.PAT_MRN_ID and d.PATIENT_IN_ROOM_DTTM between i.ADMIT_DT_SRC and i.DSCH_DT_SRC
                    where d.PATIENT_IN_ROOM_DTTM is not null and
                          d.SURGERY_DATE >= TO_DATE('{sched_start_date}','YYYY-MM-DD') AND
                          i.MSDRG_CD_SRC IN {neuro_drg_query} AND
                          i.FACILITY_MSX IN {facilities};")


# Establish DB Connection and Get data ----
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)
dbExecute(conn, "ALTER SESSION SET TIME_ZONE = 'America/New_York'")
ip_or_data <- dbGetQuery(conn,"SELECT * FROM IPCAP_OR_CASE_DATA")
dbDisconnect(conn)


# Procedure Minutes ----
procedure_minutes <- ip_or_data%>%
  distinct(OR_CASE_ID,PAT_MRN_ID,ENCOUNTER_NO, .keep_all = TRUE) %>%
  mutate(procedure_minutes = PATIENT_OUT_ROOM_DTTM-PATIENT_OUT_ROOM_DTTM)
