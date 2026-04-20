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
library(ggVennDiagram)



# source functions ----
# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"


# ---------------------------------- Data Pull --------------------------------

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
                                                        MSDRG_CD_SRC IS NULL AND PRIMARY_PROC_CODE IS NULL;")


# Establish DB Connection and Get data ----
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)
dbExecute(conn, "ALTER SESSION SET TIME_ZONE = 'America/New_York'")
ip_or_data_drg <- dbGetQuery(conn,ip_or_query_drg)
ip_or_data_cpt <- dbGetQuery(conn,ip_or_query_cpt)
ip_or_data_null_cpt_drg_neuro_speciality <- dbGetQuery(conn,ip_or_query_null_cpt_drg_neuro_speciality)
dbDisconnect(conn)


# Group Classification: DRG-only, CPT-only, Both ----

encounters_drg <- unique(ip_or_data_drg$ENCOUNTER_NO)
encounters_cpt <- unique(ip_or_data_cpt$ENCOUNTER_NO)


enc_both     <- intersect(encounters_drg, encounters_cpt)
enc_drg_only <- setdiff(encounters_drg, encounters_cpt)
enc_cpt_only <- setdiff(encounters_cpt, encounters_drg)
enc_universe <- union(encounters_drg, encounters_cpt)

cat(glue(
  "DRG only Encounters : {length(enc_drg_only)},{length(enc_drg_only)*100/length(enc_universe)}\n",
  "CPT only Encounters : {length(enc_cpt_only)},{length(enc_cpt_only)*100/length(enc_universe)}\n",
  "Encounters Common in DRG and CPT : {length(enc_both)},{length(enc_both)*100/length(enc_universe)}\n",
  "Unique Encounters across DRG and CPT : {length(enc_universe)}\n",
  "Total DRG: {length(encounters_drg)},{length(encounters_drg)*100/length(enc_universe)}\n",
  "Total CPT: {length(encounters_cpt)},{length(encounters_cpt)*100/length(enc_universe)}\n"
))


# ip_or_data_drg <- ip_or_data_drg %>%
#   mutate(cohort = if_else(ENCOUNTER_NO %in% enc_both, "Both", "DRG Only"))
# 
# ip_or_data_cpt <- ip_or_data_cpt %>%
#   mutate(cohort = if_else(ENCOUNTER_NO %in% enc_both, "Both", "CPT Only"))
# 
# 
# # Combined dataset (union, one row per OR case)
# ip_or_data_all <- bind_rows(
#   ip_or_data_drg,
#   ip_or_data_cpt %>% filter(ENCOUNTER_NO %in% enc_cpt_only)
# ) %>%
#   distinct(ENCOUNTER_NO, .keep_all = TRUE)
# 
# 
# # Venn Diagram: DRG vs CPT encounter overlap ----
# 
# venn_list <- list(DRG = encounters_drg, CPT = encounters_cpt)
# 
# venn_plot <- ggVennDiagram(venn_list, label_alpha = 0) +
#   scale_fill_gradient(low = "#F4FAFE", high = "#06ABEB") +
#   labs(title = "Neurosurgery OR Cases: DRG vs CPT Encounter Overlap") +
#   theme(legend.position = "none")
# 
# 
# ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/venn_drg_cpt_overlap.png"),
#        venn_plot, width = 8, height = 6, dpi = 150)


# OR Volume Analysis by Cohort ----

or_cases_drg <- unique(ip_or_data_drg$OR_CASE_ID)
or_cases_cpt <- unique(ip_or_data_cpt$OR_CASE_ID)


or_cases_both     <- intersect(or_cases_drg, or_cases_cpt)
or_cases_drg_only <- setdiff(or_cases_drg, or_cases_cpt)
or_cases_cpt_only <- setdiff(or_cases_cpt, or_cases_drg)
or_cases_universe <- union(or_cases_drg, or_cases_cpt)

cat(glue(
  "DRG only OR Cases : {length(or_cases_drg_only)},{length(or_cases_drg_only)*100/length(or_cases_universe)}\n",
  "CPT only OR Cases : {length(or_cases_cpt_only)},{length(or_cases_cpt_only)*100/length(or_cases_universe)}\n",
  "OR Cases Common in DRG and CPT : {length(or_cases_both)},{length(or_cases_both)*100/length(or_cases_universe)}\n",
  "Unique OR Cases across DRG and CPT : {length(or_cases_universe)}\n",
  "Total DRG: {length(or_cases_drg)},{length(or_cases_drg)*100/length(or_cases_universe)}\n",
  "Total CPT: {length(or_cases_cpt)},{length(or_cases_cpt)*100/length(or_cases_universe)}\n"
))

ip_or_data_drg <- ip_or_data_drg %>%
  mutate(cohort_or = if_else(OR_CASE_ID %in% or_cases_both, "Both", "DRG Only"))

ip_or_data_cpt <- ip_or_data_cpt %>%
  mutate(cohort_or = if_else(OR_CASE_ID %in% or_cases_both, "Both", "CPT Only"))


# Combined dataset (union, one row per OR case)
ip_or_data_all_or_cases <- bind_rows(
  ip_or_data_drg,
  ip_or_data_cpt %>% filter(OR_CASE_ID %in% or_cases_cpt_only)
) %>%
  distinct(OR_CASE_ID, .keep_all = TRUE)


# Venn Diagram: DRG vs CPT encounter Volume overlap ----

venn_list <- list(DRG = or_cases_drg, CPT = or_cases_cpt)

venn_plot <- ggVennDiagram(venn_list, label_alpha = 0) +
  scale_fill_gradient(low = "#F4FAFE", high = "#06ABEB") +
  labs(title = "Neurosurgery OR Cases: DRG vs CPT OR Case Overlap") +
  theme(legend.position = "none")
  


ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/venn_drg_cpt_or_overlap.png"),
       venn_plot, width = 8, height = 6, dpi = 150)

# OR Case Minutes and TAT ----

or_procedure_and_tat_minutes_across_cohorts <- ip_or_data_all_or_cases %>%
  select(SURGERY_DATE,FACILITY_MSX,OR_CASE_ID,cohort_or,MINUTES_IN_ROOM_TO_OUT_ROOM,TURNOVER_FROM_PRIOR_CASE) %>%
  mutate(TURNOVER_FROM_PRIOR_CASE = if_else(is.na(TURNOVER_FROM_PRIOR_CASE),0,
                                            as.numeric(TURNOVER_FROM_PRIOR_CASE)),
         MINUTES_IN_ROOM_TO_OUT_ROOM = if_else(is.na(MINUTES_IN_ROOM_TO_OUT_ROOM),0,
                                               as.numeric(MINUTES_IN_ROOM_TO_OUT_ROOM)),
         procedure_and_tat =  MINUTES_IN_ROOM_TO_OUT_ROOM+TURNOVER_FROM_PRIOR_CASE)
  
  
cat(glue(
  "DRG only OR Cases : {length(or_cases_drg_only)},{length(or_cases_drg_only)*100/length(or_cases_universe)}\n",
  "CPT only OR Cases : {length(or_cases_cpt_only)},{length(or_cases_cpt_only)*100/length(or_cases_universe)}\n",
  "OR Cases Common in DRG and CPT : {length(or_cases_both)},{length(or_cases_both)*100/length(or_cases_universe)}\n",
  "Unique OR Cases across DRG and CPT : {length(or_cases_universe)}\n",
  "Total DRG: {length(or_cases_drg)},{length(or_cases_drg)*100/length(or_cases_universe)}\n",
  "Total CPT: {length(or_cases_cpt)},{length(or_cases_cpt)*100/length(or_cases_universe)}\n"
))

