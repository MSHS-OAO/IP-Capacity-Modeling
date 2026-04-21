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
or_case_details_table_name <- 'MS_INSIGHT.OR_QUALITY_DASHBOARD_CASE_DETAILS'

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


neuro_codes_npi <- read_xlsx(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Faculty and NPI.xlsx")) 
neuro_codes_npi <- trimws(neuro_codes_npi$NPI)
neuro_npi_query <- paste0("('", paste(neuro_codes_npi, collapse = "', '"), "')")



# query to capture combining IP and OR data ----
ip_or_query_drg <- glue("SELECT *
                              FROM {ip_or_master_table_name} d 
                              WHERE d.MSDRG_CD_SRC IN {neuro_drg_query} AND
                                    d.FACILITY_MSX  in ('RVT', 'MSH', 'STL');")
ip_or_query_cpt <- glue("SELECT *
                              FROM {ip_or_master_table_name} d 
                              WHERE d.PRIMARY_PROC_CODE IN {neuro_cpt_query} AND
                                    d.FACILITY_MSX  in ('RVT', 'MSH', 'STL');")


ip_or_query_npi <- glue("SELECT *
                              FROM {ip_or_master_table_name} d 
                              WHERE d.SURGEON_NPI IN {neuro_npi_query} AND
                                    d.FACILITY_MSX  in ('RVT', 'MSH', 'STL');")



or_case_details_cpt <- glue("SELECT *
                              FROM {or_case_details_table_name} d 
                              WHERE d.PRIMARY_PROC_CODE IN {neuro_cpt_query} AND
                                    d.HOSPITAL  in ('MSW', 'MSH', 'MSM');")

or_case_details_npi <- glue("SELECT *
                              FROM {or_case_details_table_name} d 
                              WHERE d.SURGEON_NPI IN {neuro_npi_query} AND
                                    d.HOSPITAL  in ('MSW', 'MSH', 'MSM');")


# ip_or_query_null_cpt_drg_neuro_speciality <- glue("SELECT *
#                                                   FROM {ip_or_master_table_name} d 
#                                                   WHERE REGEXP_LIKE(SURGEON_SPECIALTY, 'NEURO') AND
#                                                         MSDRG_CD_SRC IS NULL AND PRIMARY_PROC_CODE IS NULL AND
#                                                         d.FACILITY_MSX  in ('RVT', 'MSH', 'STL');")






# Establish DB Connection and Get data ----
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)
dbExecute(conn, "ALTER SESSION SET TIME_ZONE = 'America/New_York'")
ip_or_data_drg <- dbGetQuery(conn,ip_or_query_drg)
ip_or_data_cpt <- dbGetQuery(conn,ip_or_query_cpt)
ip_or_data_npi <- dbGetQuery(conn,ip_or_query_npi)
or_case_details_cpt_data <- dbGetQuery(conn,or_case_details_cpt)
or_case_details_npi_data <- dbGetQuery(conn,or_case_details_npi)
# ip_or_data_null_cpt_drg_neuro_speciality <- dbGetQuery(conn,ip_or_query_null_cpt_drg_neuro_speciality)
dbDisconnect(conn)

# ip_or_data_null_cpt_drg_neuro_speciality_filtered <- ip_or_data_null_cpt_drg_neuro_speciality %>%
#   select(MSMRN,
#          ENCOUNTER_NO, 
#          OR_CASE_ID, 
#          SURGERY_DATE, 
#          PRINCIPAL_SURGEON_NAME_MSX, 
#          FACILITY_MSX,
#          MSDRG_CD_SRC,PRIMARY_PROC_CODE)

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


ip_or_data_drg <- ip_or_data_drg %>%
  mutate(cohort = if_else(ENCOUNTER_NO %in% enc_both, "Both", "DRG Only"))

ip_or_data_cpt <- ip_or_data_cpt %>%
  mutate(cohort = if_else(ENCOUNTER_NO %in% enc_both, "Both", "CPT Only"))


# Combined dataset (union, one row per OR case)
ip_or_data_all <- bind_rows(
  ip_or_data_drg,
  ip_or_data_cpt %>% filter(ENCOUNTER_NO %in% enc_cpt_only)
) %>%
  distinct(ENCOUNTER_NO, .keep_all = TRUE)


# Venn Diagram: DRG vs CPT encounter overlap ----

venn_list <- list(DRG = encounters_drg, CPT = encounters_cpt)

venn_plot <- ggVennDiagram(venn_list, label_alpha = 0) +
  scale_fill_gradient(low = "#F4FAFE", high = "#06ABEB") +
  labs(title = "Neurosurgery OR Cases: DRG vs CPT Encounter Overlap") +
  theme(legend.position = "none")


ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/venn_drg_cpt_overlap.png"),
       venn_plot, width = 8, height = 6, dpi = 150)


# OR Volume Analysis by Cohort ----

or_cases_drg <- unique(ip_or_data_drg$OR_CASE_ID)
or_cases_cpt <- unique(ip_or_data_cpt$OR_CASE_ID)
or_cases_npi <- unique(ip_or_data_cpt$OR_CASE_ID)


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

# OR Demand Analysis by Cohort ----

procedure_minutes <- ip_or_data_all_or_cases %>%
  filter(ENCOUNTER_NO %in% enc_universe) %>%
  distinct(OR_CASE_ID, MSMRN, ENCOUNTER_NO, .keep_all = TRUE) %>%
  mutate(
    PATIENT_IN_ROOM_DTTM  = as.POSIXct(PATIENT_IN_ROOM_DTTM),
    PATIENT_OUT_ROOM_DTTM = as.POSIXct(PATIENT_OUT_ROOM_DTTM),
    TURNOVER_FROM_PRIOR_CASE =  if_else(is.na(TURNOVER_FROM_PRIOR_CASE),0,as.numeric(TURNOVER_FROM_PRIOR_CASE)),
    procedure_minutes     = as.numeric(difftime(PATIENT_OUT_ROOM_DTTM,
                                                PATIENT_IN_ROOM_DTTM,
                                                units = "mins")),
    procedure_and_tat = procedure_minutes+TURNOVER_FROM_PRIOR_CASE,
    surgery_month = lubridate::floor_date(as.Date(PATIENT_IN_ROOM_DTTM), "month")
  ) %>%
  filter(procedure_minutes > 0,
         as.Date(PATIENT_IN_ROOM_DTTM) >= as.Date('2025-01-01'),
         as.Date(PATIENT_IN_ROOM_DTTM) <= as.Date('2025-12-31'))


daily_or_demand <- procedure_minutes %>%
  group_by(SURGERY_DATE) %>%
  summarise(
    case_count       = n(),
    total_or_minutes = sum(procedure_and_tat, na.rm = TRUE),
    avg_or_minutes   = mean(procedure_and_tat, na.rm = TRUE),
    .groups          = "drop"
  )

monthly_or_demand <- procedure_minutes %>%
  group_by(surgery_month) %>%
  summarise(
    case_count       = n(),
    total_or_minutes = sum(procedure_and_tat, na.rm = TRUE),
    avg_or_minutes   = mean(procedure_and_tat, na.rm = TRUE),
    .groups          = "drop"
  ) 

# Plots ----

mshs_theme <- theme_minimal() +
  theme(
    plot.title       = element_text(color = "#221F72", face = "bold", size = 13),
    axis.title       = element_text(color = "#221F72"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.title     = element_text(color = "#221F72", face = "bold"),
    strip.text       = element_text(color = "#FFFFFF", face = "bold"),
    strip.background = element_rect(fill = "#221F72"),
    legend.position = "right",
    panel.grid = element_blank()
  )

scale_factor <- max(monthly_or_demand$case_count, na.rm = TRUE) /
  max(monthly_or_demand$avg_or_minutes, na.rm = TRUE)

p_combined <- ggplot(monthly_or_demand, aes(x = surgery_month)) +
  geom_col(aes(y = avg_or_minutes * scale_factor, fill = "Avg OR Minutes"),
           alpha = 0.6) +
  geom_line(aes(y = case_count, color = "Case Volume"),
            linewidth = 1) +
  geom_point(aes(y = case_count, color = "Case Volume"),
             size = 2) +
  scale_color_manual(name = "", values = c("Case Volume"    = "#221F72")) +
  scale_fill_manual(name  = "", values = c("Avg OR Minutes" = "#97CBE7")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    name     = "Case Volume",
    sec.axis = sec_axis(~ . / scale_factor, name = "Avg OR Minutes (Procedure + TAT)")
  ) +
  labs(
    title = "Monthly Neurosurgery OR Case Volume & Avg OR Minutes",
    x     = "Month"
  ) +
  guides(color = guide_legend(order = 1),
         fill  = guide_legend(order = 2)) +
  mshs_theme 


print(p_combined)

ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/or_demand_monthly_combined.png"),
       p_combined, width = 12, height = 6, dpi = 150)


# Daily Plot
scale_factor_daily <- max(daily_or_demand$case_count, na.rm = TRUE) /
  max(daily_or_demand$avg_or_minutes, na.rm = TRUE)

p_combined_daily <- ggplot(daily_or_demand, aes(x = SURGERY_DATE)) +
  # geom_area(aes(y = avg_or_minutes * scale_factor_daily, fill = "Avg OR Minutes"),
  #          alpha = 0.6) +
  geom_line(aes(y = total_or_minutes, color = "Total OR Minutes"),
            linewidth = 0.6) +
  # geom_point(aes(y = case_count, color = "Case Volume"),
  #            size = 2) +
  # geom_line(aes(y = avg_or_minutes),
  #            linetype = "dashed", color = "#D80B8C", linewidth = 0.5)+
  scale_color_manual(name = "", values = c("Total OR Minutes"    = "#D80B8C")) +
  # scale_fill_manual(name  = "", values = c("Avg OR Minutes" = "#97CBE7")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    name     = "Total OR Minutes (Procedure + TAT)"
    # sec.axis = sec_axis(~ . / scale_factor_daily, name = "Avg OR Minutes (Procedure + TAT)")
  ) +
  labs(
    title = "Daily Neurosurgery OR Minutes (Procedure + TAT)",
    x     = "Day"
  ) +
  guides(color = guide_legend(order = 1),
         fill  = guide_legend(order = 2)) +
  mshs_theme +
  theme(legend.position = "none")


print(p_combined_daily)

ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/or_demand_daily_combined.png"),
       p_combined_daily, width = 12, height = 6, dpi = 150)

# p_case_volume <- ggplot(monthly_or_demand,
#                         aes(x = surgery_month, y = case_count,
#                             color = cohort_or, group = cohort_or)) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   scale_color_manual(values = c("DRG Only" = "#221F72",
#                                 "CPT Only" = "#00AEEF",
#                                 "Both"     = "#D80B8C")) +
#   facet_wrap(~ FACILITY_MSX, scales = "free_y") +
#   labs(
#     title = "Monthly Neurosurgery OR Case Volume by Cohort",
#     x     = "Month", y = "Number of Cases", color = "Cohort"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# p_proc_minutes <- ggplot(monthly_or_demand,
#                          aes(x = surgery_month, y = avg_or_minutes,
#                              color = cohort_or, group = cohort_or)) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(~ FACILITY_MSX, scales = "free_y") +
#   scale_color_manual(values = c("DRG Only" = "#221F72",
#                                 "CPT Only" = "#00AEEF",
#                                 "Both"     = "#D80B8C")) +
#   labs(
#     title = "Average Procedure+TAT Minutes by Cohort",
#     x     = "Month", y = "Avg OR Minutes", color = "Cohort"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# p_proc_minutes_total <- ggplot(monthly_or_demand,
#                          aes(x = surgery_month, y = total_or_minutes,
#                              color = cohort_or, group = cohort_or)) +
#   geom_line(linewidth = 0.8) +
#   geom_point(size = 1.5) +
#   facet_wrap(~ FACILITY_MSX, scales = "free_y") +
#   scale_color_manual(values = c("DRG Only" = "#221F72",
#                                 "CPT Only" = "#00AEEF",
#                                 "Both"     = "#D80B8C")) +
#   labs(
#     title = "Total Procedure+TAT Hours by Cohort",
#     x     = "Month", y = "Avg OR Minutes", color = "Cohort"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# print(p_case_volume)
# print(p_proc_minutes)
# print(p_proc_minutes_total)
# ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/CaseVolume.png"),
#        p_case_volume)
# ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/AvgProcedureTatHours.png"),
#        p_proc_minutes)
# ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/ProcedureTatMinutes.png"),
#        p_proc_minutes)
# 
# cohort_demand_summary <- procedure_minutes %>%
#   group_by(cohort_or, FACILITY_MSX) %>%
#   summarise(
#     total_cases      = n(),
#     total_or_minutes = sum(procedure_and_tat, na.rm = TRUE),
#     avg_or_minutes   = round(mean(procedure_and_tat, na.rm = TRUE), 1),
#     median_or_min    = round(median(procedure_and_tat, na.rm = TRUE), 1),
#     .groups          = "drop"
#   ) %>%
#   arrange(FACILITY_MSX, cohort_or)