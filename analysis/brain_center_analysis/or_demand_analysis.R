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
sched_start_date <- '2025-01-01'
sched_end_date <- '2025-12-31'
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

msm_msw_non_nuero_or_cpt_with_neuro_drg <- glue("SELECT *
                                                FROM {ip_or_master_table_name} d 
                                                WHERE (d.MSDRG_CD_SRC IN {neuro_drg_query} AND d.PRIMARY_PROC_CODE NOT IN {neuro_cpt_query}) AND
                                                d.HOSPITAL  in ('MSM', 'MSW') AND
                                                d.SURGERY_DATE BETWEEN TO_DATE('{sched_start_date}','YYYY-MM-DD') AND TO_DATE('{sched_end_date}','YYYY-MM-DD');")


msh_non_nuero_or <- glue("SELECT *
                          FROM {or_case_details_table_name} d 
                          WHERE (d.SURGEON_NPI NOT IN {neuro_npi_query} AND d.PRIMARY_PROC_CODE NOT IN {neuro_cpt_query}) AND
                          d.HOSPITAL  IN ('MSH') AND
                          d.SURGERY_DATE BETWEEN TO_DATE('{sched_start_date}','YYYY-MM-DD') AND TO_DATE('{sched_end_date}','YYYY-MM-DD');")



# --------------------- Connect to database and query the data ---------------------
dsn <- "OAO Cloud DB Production"
conn <- dbConnect(odbc(), dsn)
dbExecute(conn, "ALTER SESSION SET TIME_ZONE = 'America/New_York'")
msm_msw_non_nuero_or_cpt_with_neuro_drg_data <- dbGetQuery(conn,msm_msw_non_nuero_or_cpt_with_neuro_drg)
msh_non_nuero_or_data <- dbGetQuery(conn,msh_non_nuero_or)
dbDisconnect(conn)


msh_non_nuero_or_data <- msh_non_nuero_or_data %>%
  rename(MSMRN = PAT_MRN_ID)


# ----- MSM MSW  non neuro -----
procedure_minutes_msm_msw <- msm_msw_non_nuero_or_cpt_with_neuro_drg_data %>%
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
  filter(procedure_minutes > 0)


hourly_msm_msw <- msm_msw_non_nuero_or_cpt_with_neuro_drg_data %>%
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
  select(MSMRN, OR_CASE_ID, SURGERY_DATE, surgery_hour, hour_bucket, minutes_in_hour) %>%
  distinct()


hourly_mean_rooms_msm_msw <- hourly_msm_msw %>%
  filter(as.Date(SURGERY_DATE) >= as.Date('2025-01-01'),
         as.Date(SURGERY_DATE) <= as.Date('2025-12-31')) %>%
  distinct(SURGERY_DATE,OR_CASE_ID,surgery_hour) %>%
  group_by(surgery_hour) %>%
  summarise(avg_or_cases = n_distinct(OR_CASE_ID)/352) %>%
  ungroup() %>%
  mutate(
    hour_bins = cut(
      surgery_hour,
      breaks = c(-1, 6, 7:20, 23),
      labels = c("0-6", as.character(7:20), "21-23"),
      right  = TRUE
    )
  ) %>%
  group_by(hour_bins) %>%
  summarise(avg_or_rooms = sum(avg_or_cases))%>%
  mutate(HOSPITAL = 'MSMW')


# ----- MSH non neuro -----
procedure_minutes_msh <- msh_non_nuero_or_data %>%
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
  filter(procedure_minutes > 0)


hourly_msh <- msh_non_nuero_or_data %>%
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
  select(MSMRN, OR_CASE_ID, SURGERY_DATE, surgery_hour, hour_bucket, minutes_in_hour) %>%
  distinct()


hourly_mean_rooms_msh <- hourly_msh %>%
  filter(as.Date(SURGERY_DATE) >= as.Date('2025-01-01'),
         as.Date(SURGERY_DATE) <= as.Date('2025-12-31')) %>%
  distinct(SURGERY_DATE,OR_CASE_ID,surgery_hour) %>%
  group_by(surgery_hour) %>%
  summarise(avg_or_cases = n_distinct(OR_CASE_ID)/352) %>%
  ungroup() %>%
  mutate(
    hour_bins = cut(
      surgery_hour,
      breaks = c(-1, 6, 7:20, 23),
      labels = c("0-6", as.character(7:20), "21-23"),
      right  = TRUE
    )
  ) %>%
  group_by(hour_bins) %>%
  summarise(avg_or_rooms = sum(avg_or_cases)) %>%
  mutate(HOSPITAL = 'MSH')


# ----- Combined OR Demand MSH, MSM. MSW -----

hourly_mean_rooms <- rbind(hourly_mean_rooms_msh,hourly_mean_rooms_msm_msw)


# ---- Plots ----



# hourly demand ----


# We use a factor of 150 because MSH max is ~38 and MSMW max is ~0.2
# 0.2 * 150 = 30 (which fits on the same scale as 38)
scaling_factor <- 9

p2 <-ggplot(hourly_mean_rooms, aes(x = hour_bins)) +
      # Large group bars
      geom_col(data = filter(hourly_mean_rooms, HOSPITAL == "MSH"),
               aes(y = avg_or_rooms, fill = "MSH")) +
      # Small group bars (scaled up)
      geom_col(data = filter(hourly_mean_rooms, HOSPITAL == "MSMW"), 
               aes(y = avg_or_rooms * scaling_factor, fill = "MSMW")) +
      # Primary Axis (Left) and Secondary Axis (Right)
      scale_y_continuous(
        name = "Rooms"
        # sec.axis = sec_axis(~ . / scaling_factor, name = "MSMW Avg Rooms")
      ) +
      # geom_text(aes(y = avg_or_rooms,label = round(avg_or_rooms,1), vjust = -0.5)) +
      labs(title = "Avg Room Demand/Hour", x = "Hour", y = "Rooms") +
      scale_fill_manual(name = "",values = c("MSH" = mshs_colors[1], 'MSMW'= mshs_colors[2]))+
      mshs_theme +
      theme(
        axis.text.x = element_text(angle = 0,hjust = 0.5),
        legend.position = "bottom"
      )

print(p2)
ggsave(paste0(cap_dir, "Adhoc/MS Brain Health/Output/hourly_mean_rooms_msm_msw_msh_0423.png"),
       p2, width = 12, height = 6, dpi = 150)