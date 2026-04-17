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
library(readr)
library(zip)
library(here)
library(hms)
library(ggplot2)
library(patchwork)
library(grid)
library(ggtext)
library(tidyverse)
library(shadowtext)



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
ip_or_master_table_name <- 'IPCAP_OR_CASE_DATA'

# --------------------- Filters ---------------------
# current_date <- Sys.Date()
# sched_date <- '2024-09-01'
# sched_start_date <- '2025-01-01'
# sched_end_date <- '2026-03-31'
status <- 'Completed'
# facilities <- "('MSH','RVT','STL')"
# room_exclusion_list <- "('MSW OR 23','MSM OR 08','MSM OR 15')"


# read in neuro codes and isolate DRGs/CPTs
neuro_codes_drg <- read_csv(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Neurosurgery DRG.csv")) 
neuro_drg <- neuro_codes_drg$`MS-DRG`
neuro_drg_query <- paste0("('", paste(neuro_drg, collapse = "', '"), "')")

neuro_codes_cpt <- read_csv(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Neurosurgery CPT.csv")) 
neuro_cpt <- neuro_codes_cpt$`CPT Code`
neuro_cpt_query <- paste0("('", paste(neuro_cpt, collapse = "', '"), "')")



# query to capture combining IP and OR data ----
ip_or_query_drg_cpt <- glue("SELECT *
                              FROM {ip_or_master_table_name} d 
                              WHERE d.CASE_STATUS = '{status}'and
                                    d.MSDRG_CD_SRC IN {neuro_drg_query} AND
                                    d.PRIMARY_PROC_CODE IN {neuro_cpt_query};")
ip_or_query_drg <- glue("SELECT *
                              FROM {ip_or_master_table_name} d 
                              WHERE d.CASE_STATUS = '{status}' AND
                                    d.MSDRG_CD_SRC IN {neuro_drg_query};")
ip_or_query_cpt <- glue("SELECT *
                              FROM {ip_or_master_table_name} d 
                              WHERE d.CASE_STATUS = '{status}' AND
                                    d.PRIMARY_PROC_CODE IN {neuro_cpt_query};")


ip_or_query_null_cpt_drg_neuro_speciality <- glue("SELECT *
                                                  FROM {ip_or_master_table_name} d 
                                                  WHERE REGEXP_LIKE(SURGEON_SPECIALTY, 'NEURO') AND
                                                        MSDRG_CD_SRC IS NULL AND
                                                        PRIMARY_PROC_CODE IS NULL;")


# Establish DB Connection and Get data ----
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)
dbExecute(conn, "ALTER SESSION SET TIME_ZONE = 'America/New_York'")
ip_or_data_drg_cpt <- dbGetQuery(conn,ip_or_query_drg_cpt)
ip_or_data_drg <- dbGetQuery(conn,ip_or_query_drg)
ip_or_data_cpt <- dbGetQuery(conn,ip_or_query_cpt)
ip_or_data_null_cpt_drg_neuro_speciality <- dbGetQuery(conn,ip_or_query_null_cpt_drg_neuro_speciality)
dbDisconnect(conn)


# Procedure Minutes ----
procedure_minutes <- ip_or_data%>%
  distinct(OR_CASE_ID,PAT_MRN_ID,ENCOUNTER_NO, .keep_all = TRUE) %>%
  mutate(procedure_minutes = PATIENT_OUT_ROOM_DTTM-PATIENT_OUT_ROOM_DTTM)
