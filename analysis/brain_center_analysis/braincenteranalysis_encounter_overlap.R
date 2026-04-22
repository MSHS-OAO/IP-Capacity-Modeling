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
library(VennDiagram)




# --------------------- Source functions and Constants ---------------------
# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"
mshs_colors <- c("#221F72", "#00AEFF", "#D80B8C", "#7F7F7F", "#000000", 
                 "#800080", "#FFFF00", "#CC0000", "#38761D", "#F39C12")
mshs_theme <- theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.title = element_text(size = 12),
                    legend.text = element_text(size = 8),
                    strip.text = element_text(size = 12, face = "bold"),
                    legend.margin = margin(l = 50,r = 50),
                    panel.grid = element_blank())


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


# --------------------- Read in mapping files ---------------------
neuro_codes_drg <- read_csv(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Neurosurgery DRG.csv")) 
neuro_drg <- neuro_codes_drg$`MS-DRG`
neuro_drg_query <- paste0("('", paste(neuro_drg, collapse = "', '"), "')")

neuro_codes_cpt <- read_csv(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Neurosurgery CPT.csv")) 
neuro_cpt <- neuro_codes_cpt$`CPT Code`
neuro_cpt_query <- paste0("('", paste(neuro_cpt, collapse = "', '"), "')")


neuro_codes_npi <- read_xlsx(paste0(cap_dir,"Adhoc/MS Brain Health/Mappings/","Faculty and NPI.xlsx")) 
neuro_codes_npi <- trimws(neuro_codes_npi$NPI)
neuro_npi_query <- paste0("('", paste(neuro_codes_npi, collapse = "', '"), "')")



# --------------------- Queries ---------------------
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






# --------------------- Connect to database and query the data ---------------------
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)
dbExecute(conn, "ALTER SESSION SET TIME_ZONE = 'America/New_York'")
ip_or_data_drg <- dbGetQuery(conn,ip_or_query_drg) %>%
  filter(!is.na(OR_CASE_ID))
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

# --------------------- Group Classification: DRG-only, CPT-only, Both ---------------------

# encounters_drg <- unique(ip_or_data_drg$ENCOUNTER_NO)
# encounters_cpt <- unique(ip_or_data_cpt$ENCOUNTER_NO)
# 
# 
# enc_both     <- intersect(encounters_drg, encounters_cpt)
# enc_drg_only <- setdiff(encounters_drg, encounters_cpt)
# enc_cpt_only <- setdiff(encounters_cpt, encounters_drg)
# enc_universe <- union(encounters_drg, encounters_cpt)
# 
# cat(glue(
#   "DRG only Encounters : {length(enc_drg_only)},{length(enc_drg_only)*100/length(enc_universe)}\n",
#   "CPT only Encounters : {length(enc_cpt_only)},{length(enc_cpt_only)*100/length(enc_universe)}\n",
#   "Encounters Common in DRG and CPT : {length(enc_both)},{length(enc_both)*100/length(enc_universe)}\n",
#   "Unique Encounters across DRG and CPT : {length(enc_universe)}\n",
#   "Total DRG: {length(encounters_drg)},{length(encounters_drg)*100/length(enc_universe)}\n",
#   "Total CPT: {length(encounters_cpt)},{length(encounters_cpt)*100/length(enc_universe)}\n"
# ))
# 
# 
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


# ---- OR Volume Analysis ----

or_cases_drg_ip <- unique(ip_or_data_drg$OR_CASE_ID)
or_cases_cpt_ip <- unique(ip_or_data_cpt$OR_CASE_ID)
or_cases_npi_ip <- unique(ip_or_data_npi$OR_CASE_ID)
or_cases_npi_ordata <- unique(or_case_details_npi_data$OR_CASE_ID)
or_cases_cpt_ordata <- unique(or_case_details_cpt_data$OR_CASE_ID)


# or_cases_both     <- intersect(or_cases_drg, or_cases_cpt)
# or_cases_drg_only <- setdiff(or_cases_drg, or_cases_cpt)
# or_cases_cpt_only <- setdiff(or_cases_cpt, or_cases_drg)
# or_cases_universe <- union(or_cases_drg, or_cases_cpt)

# or_cases_from_ordata_npi <- setdiff(or_cases_npi_ordata)
# 
# cat(glue(
#   "DRG only OR Cases : {length(or_cases_drg_only)},{length(or_cases_drg_only)*100/length(or_cases_universe)}\n",
#   "CPT only OR Cases : {length(or_cases_cpt_only)},{length(or_cases_cpt_only)*100/length(or_cases_universe)}\n",
#   "OR Cases Common in DRG and CPT : {length(or_cases_both)},{length(or_cases_both)*100/length(or_cases_universe)}\n",
#   "Unique OR Cases across DRG and CPT : {length(or_cases_universe)}\n",
#   "Total DRG: {length(or_cases_drg)},{length(or_cases_drg)*100/length(or_cases_universe)}\n",
#   "Total CPT: {length(or_cases_cpt)},{length(or_cases_cpt)*100/length(or_cases_universe)}\n"
# ))
# 
# ip_or_data_drg <- ip_or_data_drg %>%
#   mutate(cohort_or = if_else(OR_CASE_ID %in% or_cases_both, "Both", "DRG Only"))
# 
# ip_or_data_cpt <- ip_or_data_cpt %>%
#   mutate(cohort_or = if_else(OR_CASE_ID %in% or_cases_both, "Both", "CPT Only"))
# 
# 
# # Combined dataset (union, one row per OR case)
# ip_or_data_all_or_cases <- bind_rows(
#   ip_or_data_drg,
#   ip_or_data_cpt %>% filter(OR_CASE_ID %in% or_cases_cpt_only)
# ) %>%
#   distinct(OR_CASE_ID, .keep_all = TRUE)
# 

# ---- Venn Diagram: DRG vs CPT vs NPI from IP and OR data volume overlap ----


venn_list <- list(
  orcases_using_drg_ip = or_cases_drg_ip,
  orcases_using_cpt_ip = or_cases_cpt_ip,
  orcases_using_npi_ip = or_cases_npi_ip,
  orcases_using_npi_or = or_cases_npi_ordata,
  orcases_using_cpt_or = or_cases_cpt_ordata
)

myCol <- mshs_colors[6:10]

venn.diagram(
  x = venn_list,
  category.names = names(venn_list),
  filename = paste0(cap_dir, "Adhoc/MS Brain Health/Output/venn_drg_cpt_npi_or_overlap.png"),
  output = TRUE,
  
  # Output features — bump size up; 480x480 @ 300dpi is tiny for a 5-set diagram
  imagetype = "png",
  height = 2400,
  width = 2400,
  resolution = 300,
  compression = "lzw",
  
  # Circles / ellipses
  lwd = 2,
  lty = "blank",
  fill = myCol,
  
  # Numbers
  cex = 0.7,
  fontface = "bold",
  fontfamily = "sans",
  
  # Set names — must have length 5 for a 5-set Venn
  cat.cex = 0.8,
  cat.fontface = "bold",
  cat.default.pos = "outer",
  cat.pos = c(0, -40, -150, 150, 40),
  cat.dist = c(0.22, 0.22, 0.22, 0.22, 0.22),
  cat.fontfamily = "sans",
  
  # rotation = 1 is only valid for 3-set diagrams; drop it or use rotation.degree
  rotation.degree = 0,
  
  margin = 0.15
)

# print(venn.diagram)

or_cases_universe <- union(
  union(
    union(
      union(or_cases_drg_ip,
            or_cases_cpt_ip),
      or_cases_npi_ip),
    or_cases_npi_ordata),
  or_cases_cpt_ordata)



# ----- Select relevant columns and combine all datasets -----

ip_or_data_cpt <- ip_or_data_cpt %>%
  select(MSMRN,
         OR_CASE_ID,
         SURGERY_DATE,
         PATIENT_IN_ROOM_DTTM,
         PATIENT_OUT_ROOM_DTTM,
         TURNOVER_FROM_PRIOR_CASE)


ip_or_data_drg <- ip_or_data_drg %>%
  select(MSMRN,
         OR_CASE_ID,
         SURGERY_DATE,
         PATIENT_IN_ROOM_DTTM,
         PATIENT_OUT_ROOM_DTTM,
         TURNOVER_FROM_PRIOR_CASE)

ip_or_data_npi <- ip_or_data_npi %>%
  select(MSMRN,
         OR_CASE_ID,
         SURGERY_DATE,
         PATIENT_IN_ROOM_DTTM,
         PATIENT_OUT_ROOM_DTTM,
         TURNOVER_FROM_PRIOR_CASE)

or_case_details_cpt_data <- or_case_details_cpt_data %>%
  select(PAT_MRN_ID,
         OR_CASE_ID,
         SURGERY_DATE,
         PATIENT_IN_ROOM_DTTM,
         PATIENT_OUT_ROOM_DTTM,
         TURNOVER_FROM_PRIOR_CASE) %>%
  rename(MSMRN = PAT_MRN_ID) 

or_case_details_npi_data <- or_case_details_npi_data %>%
  select(PAT_MRN_ID,
         OR_CASE_ID,
         SURGERY_DATE,
         PATIENT_IN_ROOM_DTTM,
         PATIENT_OUT_ROOM_DTTM,
         TURNOVER_FROM_PRIOR_CASE) %>%
  rename(MSMRN = PAT_MRN_ID)

ip_or_data_all_or_cases <- rbind(ip_or_data_cpt,
                                 ip_or_data_drg,
                                 ip_or_data_npi,
                                 or_case_details_cpt_data,
                                 or_case_details_npi_data)


# ---- OR Demand Analysis Data ----

procedure_minutes <- ip_or_data_all_or_cases %>%
  filter(OR_CASE_ID %in% or_cases_universe) %>%
  distinct(OR_CASE_ID, MSMRN, .keep_all = TRUE) %>%
  mutate(
    PATIENT_IN_ROOM_DTTM  = as.POSIXct(PATIENT_IN_ROOM_DTTM),
    PATIENT_OUT_ROOM_DTTM = as.POSIXct(PATIENT_OUT_ROOM_DTTM),
    TURNOVER_FROM_PRIOR_CASE =  if_else(is.na(TURNOVER_FROM_PRIOR_CASE),0,as.numeric(TURNOVER_FROM_PRIOR_CASE)),
    procedure_minutes     = as.numeric(difftime(PATIENT_OUT_ROOM_DTTM,
                                                PATIENT_IN_ROOM_DTTM,
                                                units = "mins")),
    procedure_and_tat = procedure_minutes+TURNOVER_FROM_PRIOR_CASE,
    surgery_month = lubridate::floor_date(as.Date(PATIENT_IN_ROOM_DTTM), "month"),
    surgery_hour = hour(lubridate::ymd_hms(as_datetime(PATIENT_IN_ROOM_DTTM)))
  ) %>%
  filter(procedure_minutes > 0,
         as.Date(PATIENT_IN_ROOM_DTTM) >= as.Date('2025-01-01'),
         as.Date(PATIENT_IN_ROOM_DTTM) <= as.Date('2025-12-31'))

hourly_or_demand <- procedure_minutes %>%
  group_by(surgery_hour) %>%
  summarise(
    case_count       = n(),
    avg_case_count_hour   = case_count/352,
    # total_or_minutes = sum(procedure_and_tat, na.rm = TRUE),
    # avg_or_minutes   = mean(procedure_and_tat, na.rm = TRUE),
    .groups          = "drop"
  ) 

hourly <- ip_or_data_all_or_cases %>%
  filter(!is.na(PATIENT_IN_ROOM_DTTM), !is.na(PATIENT_OUT_ROOM_DTTM)) %>%
  mutate( TURNOVER_FROM_PRIOR_CASE =  if_else(is.na(TURNOVER_FROM_PRIOR_CASE),0,as.numeric(TURNOVER_FROM_PRIOR_CASE)),
          PATIENT_OUT_ROOM_DTTM1 = PATIENT_OUT_ROOM_DTTM + minutes(TURNOVER_FROM_PRIOR_CASE)) %>%
  rowwise() %>%
  mutate(
    hour_bucket = list(
      seq(floor_date(PATIENT_IN_ROOM_DTTM,  "hour"),
          floor_date(PATIENT_OUT_ROOM_DTTM1, "hour"),
          by = "1 hour")
    )
  ) %>%
  ungroup() %>% 
  unnest(hour_bucket) %>%
  mutate(
    hour_end        = hour_bucket + hours(1),
    minutes_in_hour = as.numeric(
      difftime(
        pmin(PATIENT_OUT_ROOM_DTTM1, hour_end),
        pmax(PATIENT_IN_ROOM_DTTM,  hour_bucket),
        units = "mins"
      )
    ),
    surgery_hour = hour(hour_bucket)
  ) %>%
  select(MSMRN, OR_CASE_ID, SURGERY_DATE, surgery_hour, hour_bucket, minutes_in_hour)


hourly_mean_minutes <- hourly %>%
  filter(as.Date(SURGERY_DATE) >= as.Date('2025-01-01'),
         as.Date(SURGERY_DATE) <= as.Date('2025-12-31')) %>%
  group_by(surgery_hour) %>%
  summarise(avg_or_minutes = mean(minutes_in_hour))

hourly_mean_volume <- hourly %>%
  filter(as.Date(SURGERY_DATE) >= as.Date('2025-01-01'),
         as.Date(SURGERY_DATE) <= as.Date('2025-12-31')) %>%
  distinct(SURGERY_DATE,OR_CASE_ID,surgery_hour) %>%
  group_by(surgery_hour) %>%
  summarise(avg_or_cases = n_distinct(OR_CASE_ID)/352)


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


# ---- Plots ----

# monthly demand ----
scale_factor <- max(monthly_or_demand$case_count, na.rm = TRUE) /
  max(monthly_or_demand$avg_or_minutes, na.rm = TRUE)

p_combined <- ggplot(monthly_or_demand, aes(x = surgery_month)) +
  geom_col(aes(y = avg_or_minutes * scale_factor, fill = "Avg OR Minutes/Case (Procedure + TAT)"),
           alpha = 0.6) +
  geom_line(aes(y = case_count, color = "Total Case Volume"),
            linewidth = 1) +
  geom_point(aes(y = case_count, color = "Total Case Volume"),
             size = 2) +
  # scale_color_manual(values = mshs_colors)+
  scale_color_manual(name = "", values = c("Total Case Volume"    = "#221F72")) +
  scale_fill_manual(name  = "", values = c("Avg OR Minutes/Case (Procedure + TAT)" = "#00AEEF")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  scale_y_continuous(
    name     = "Total Case Volume",
    sec.axis = sec_axis(~ . / scale_factor, name = "Avg OR Minutes/Case (Procedure + TAT)")
  ) +
  labs(
    title = "Monthly Neurosurgery OR Demand - Case Volume and Avg OR Minutes/Case",
    x     = "Month"
  ) +
  guides(color = guide_legend(order = 1),
         fill  = guide_legend(order = 2)) +
  mshs_theme 


print(p_combined)

ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/or_demand_monthly_combined.png"),
       p_combined, width = 12, height = 6, dpi = 150)





# hourly demand ----



p1 <- ggplot(hourly_or_demand, aes(x = surgery_hour, y = case_count)) +
  geom_col(fill = mshs_colors[1], width = 0.8) +
  labs(title = "Total Case Count by Hour", x = "Surgery Hour", y = "Cases") +
  scale_x_continuous(breaks = seq(0:23)-1,
                     expand = expansion(mult = 0.01) )+
  mshs_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

print(p1)


p2 <- ggplot(hourly_or_demand, aes(x = surgery_hour, y = avg_case_count_hour)) +
  geom_col(fill = mshs_colors[2], width = 0.8) +
  labs(title = "Avg Number of Cases/Hour", x = "Surgery Hour", y = "Cases") +
  geom_text(aes(label = round(avg_case_count_hour,2), vjust = -0.5)) +
  scale_x_continuous(breaks = c(0,6,seq(0:23)+6),
                     expand = expansion(mult = 0.01) )+
  mshs_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

print(p2)


p3 <- ggplot(hourly_mean_minutes, aes(x = surgery_hour, y = avg_or_minutes)) +
  geom_col(fill = mshs_colors[2], width = 0.7) +
  labs(title = "Avg OR Minutes per Case by Hour", x = "Surgery Hour", y = "Avg Min / Case") +
  scale_x_continuous(breaks = seq(0:23)-1,
                     expand = expansion(mult = 0.01) )+
  mshs_theme +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1)
  )


print(p3)


# Daily Plot
# scale_factor_daily <- max(daily_or_demand$case_count, na.rm = TRUE) /
#   max(daily_or_demand$avg_or_minutes, na.rm = TRUE)
# 
# p_combined_daily <- ggplot(daily_or_demand, aes(x = SURGERY_DATE)) +
#   # geom_area(aes(y = avg_or_minutes * scale_factor_daily, fill = "Avg OR Minutes"),
#   #          alpha = 0.6) +
#   geom_line(aes(y = total_or_minutes, color = "Total OR Minutes"),
#             linewidth = 0.6) +
#   # geom_point(aes(y = case_count, color = "Case Volume"),
#   #            size = 2) +
#   # geom_line(aes(y = avg_or_minutes),
#   #            linetype = "dashed", color = "#D80B8C", linewidth = 0.5)+
#   scale_color_manual(name = "", values = c("Total OR Minutes"    = "#D80B8C")) +
#   # scale_fill_manual(name  = "", values = c("Avg OR Minutes" = "#97CBE7")) +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
#   scale_y_continuous(
#     name     = "Total OR Minutes (Procedure + TAT)"
#     # sec.axis = sec_axis(~ . / scale_factor_daily, name = "Avg OR Minutes (Procedure + TAT)")
#   ) +
#   labs(
#     title = "Daily Neurosurgery OR Minutes (Procedure + TAT)",
#     x     = "Day"
#   ) +
#   guides(color = guide_legend(order = 1),
#          fill  = guide_legend(order = 2)) +
#   mshs_theme +
#   theme(legend.position = "none")
# 
# 
# print(p_combined_daily)
# 
# ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/or_demand_daily_combined.png"),
#        p_combined_daily, width = 12, height = 6, dpi = 150)

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