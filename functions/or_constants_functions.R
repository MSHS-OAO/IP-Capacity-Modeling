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

# get prime_time ---
prime_time_location <- function(data) {
  data <- data %>% mutate(
            Weekday = weekdays(SURGERY_DATE),
            site = case_when(
              str_detect(LOCATION_NAME, "MSDUS") ~ "MSDUS",   # check before MSDC/MSD
              str_detect(LOCATION_NAME, "MSDC")  ~ "MSDC",
              str_detect(LOCATION_NAME, "MSW")   ~ "MSW",
              str_detect(LOCATION_NAME, "MSH")   ~ "MSH",
              str_detect(LOCATION_NAME, "5 E 98TH ST AMB SUITE")   ~ "MSH",
              str_detect(LOCATION_NAME, "MSM")   ~ "MSM",
              str_detect(LOCATION_NAME, "MSQ")   ~ "MSQ",
              str_detect(LOCATION_NAME, "MSB")   ~ "MSB"
            ),
            is_wed = Weekday == "Wednesday",
            start_time = case_when(
              site == "MSH"  & !is_wed ~ "08:00:00",
              site == "MSH"  &  is_wed ~ "09:00:00",
              site == "MSM"  &  is_wed ~ "07:30:00",
              site == "MSM"  & !is_wed ~ "08:30:00",
              site %in% c("MSDC", "MSDUS", "MSW") &  is_wed ~ "08:30:00",
              site %in% c("MSDC", "MSDUS", "MSW") & !is_wed ~ "07:30:00",
              site %in% c("MSQ", "MSB") ~ "08:00:00"
            ),
            end_time = case_when(
              site %in% c("MSH", "MSQ") ~ "18:00:00",
              site %in% c("MSM", "MSDC", "MSDUS", "MSW") ~ "17:30:00",
              site == "MSB" ~ "15:00:00"
            ))
  
  data <- data %>%
    mutate(
      PRIME_TIME_START = as.POSIXct(paste(SURGERY_DATE, start_time),format='%Y-%m-%d %H:%M:%S', tz = "America/New_York"),
      PRIME_TIME_END   = as.POSIXct(paste(SURGERY_DATE, end_time),format='%Y-%m-%d %H:%M:%S',   tz = "America/New_York")
    ) %>%
    select(-site, -is_wed, -start_time, -end_time)
  
  
}

# Function to fetch data ----
get_or_data <- function(sched_start_date = '2025-01-01', sched_end_date = '2025-12-31',status = 'Completed', mrn_list = NULL){
  
  # DSN and Database Connections
  dsn <- "OAO Cloud DB Production"
  conn <- dbConnect(odbc(), dsn)
  encounter_data_table_name <- 'MS_INSIGHT.OR_QUALITY_DASHBOARD_CASE_DETAILS'
  utlization_calculation_table <- 'MS_INSIGHT.OR_QUALITY_ROOM_UTIL_V'
  
  
  encounters <- tbl(conn, in_schema("MS_INSIGHT", "OR_QUALITY_DASHBOARD_CASE_DETAILS"))   # your actual names
  utlization_calculation_table   <- tbl(conn, in_schema("MS_INSIGHT", "OR_QUALITY_ROOM_UTIL_V"))
  
  
  or_cases<- encounters %>%
    left_join(utlization_calculation_table, by = c("OR_CASE_ID" = "LOG_ID")) %>%
    filter(
      SURGERY_DATE >= to_date(sched_start_date, "YYYY-MM-DD"),
      SURGERY_DATE <= to_date(sched_end_date,   "YYYY-MM-DD"),
      # PAT_MRN_ID %in% mrn_list,            
      WEEKEND_YN == "N",
      HOLIDAY_YN == "N",
      # sql("OR_LOCATION NOT LIKE 'L&D%'"),
      CASE_STATUS == status
    ) %>%
    select(
      OR_CASE_ID,
      ENCOUNTER_NO_SRC, ENCOUNTER_ID,
      PAYOR_GROUP_DESC_MSX_OP, CCM_PAYOR_DESC_MSX_OP, PAYOR_GROUP_OP,
      CLINIC_GROUP_DESC_MSX, REG_AREA_DESC_SRC,
      ATTENDING_MD = ATTENDING_MD_NAME_MSX,
      ATTENDING_MD_SPECIALIZATION = ATTENDING_MD_SPEC_SRC,
      PRIMARY_SURGEON,
      PRIMARY_SURGEON_SPECIALTY = SURGEON_SPECIALTY,
      # PATIENT_CLASS = PAT_CLASS_NAME,
      PATIENT_MRN = PAT_MRN_ID,
      PATIENT_DOB = PAT_DOB,
      ADMIT_CSN_ID, TOTAL_TIME_NEEDED,
      PRIMARY_PROCEDURE_CODE = PRIMARY_PROC_CODE,
      PRIMARY_PROCEDURE_DESC = PRIMARY_PROCEDURE,
      ANESTHESIA_TYPE, PRIME_TIME_PROC,
      SCHED_IN_ROOM_DTTM, SCHED_START_TIME,
      PATIENT_IN_ROOM_DTTM, PATIENT_OUT_ROOM_DTTM,
      MINUTES_IN_ROOM_TO_OUT_ROOM, TURNOVER_FROM_PRIOR_CASE,
      SURGERY_DATE,
      ROOM_ID = OR_ID,
      LOCATION_NAME = OR_LOCATION,
      CLUSTER_NAME, ROBOTIC_SURGERY_DAVINCI_YN,
      HOLIDAY_YN, WEEKEND_YN
    )
  
  # or_cases_lazy %>% show_query()   # inspect the Oracle SQL it generates
  schedule_data <- or_cases %>% collect()   
  
  
  dbDisconnect(conn)
  
  schedule_data <- prime_time_location(schedule_data) %>%
    distinct(OR_CASE_ID, .keep_all = TRUE) 
  
  schedule_data_ip <- schedule_data %>%
    left_join(
      mrn_list,
      join_by(
        PATIENT_MRN == MSMRN,
        between(SURGERY_DATE, ADMIT_DT_SRC, DSCH_DT_SRC)
    )) 
    
  schedule_data_ip <- schedule_data_ip %>%
    mutate(across(c(PATIENT_IN_ROOM_DTTM, PATIENT_OUT_ROOM_DTTM),
                  ~ force_tz(.x, tzone = "America/New_York"))) %>%
    group_by(ROOM_ID) %>%
    arrange(PATIENT_OUT_ROOM_DTTM, .by_group = TRUE) %>%
    mutate(setup_and_cleanup_time = coalesce(lead(TURNOVER_FROM_PRIOR_CASE), 0)) %>%
    ungroup() %>%
    mutate(
      PATIENT_OUT_AND_SETUP_CLEANUP_END = PATIENT_OUT_ROOM_DTTM + minutes(as.integer(setup_and_cleanup_time)),
      PrimeTimeInterval  = interval(PRIME_TIME_START, PRIME_TIME_END, tzone = "America/New_York"),
      ProcedureInterval  = interval(PATIENT_IN_ROOM_DTTM, PATIENT_OUT_ROOM_DTTM, tzone = "America/New_York"),
      SetupTimeInterval  = interval(PATIENT_OUT_ROOM_DTTM, PATIENT_OUT_AND_SETUP_CLEANUP_END, tzone = "America/New_York"),
      overlap_primetime_procedure = intersect(PrimeTimeInterval, ProcedureInterval),
      overlap_primetime_setup     = intersect(PrimeTimeInterval, SetupTimeInterval)
    ) %>%
    # --- calendar fields ---
    mutate(
      month        = month(SURGERY_DATE),
      month_date   = floor_date(SURGERY_DATE, unit = 'month'),
      day_of_month = day(SURGERY_DATE),
      week_of_year = week(SURGERY_DATE),
      year         = year(SURGERY_DATE)
    )
  
}

summary_metrics <- function(processed_or_cases){
  
  RECOVERABLE_THRESHOLD <- 180  # minutes
  
  
  baseline_data <- processed_or_cases %>%
    filter(!is.na(overlap_primetime_procedure) | !is.na(overlap_primetime_setup)) %>% # get only primetime overlaping cases
    mutate(
      proc_pt_min = coalesce(as.numeric(as.duration(overlap_primetime_procedure), "minutes"), 0),
      tat_pt_min  = coalesce(as.numeric(as.duration(overlap_primetime_setup), "minutes"), 0),
      used_pt_min = proc_pt_min + tat_pt_min
    )
  
  # ---------------------------------------------------------------
  # 2. ROOM-DAY level: available time + gap analysis for
  #    recoverable vs non-recoverable
  # ---------------------------------------------------------------
  
  
  room_day <- baseline_data %>%
    group_by(CLUSTER_NAME, LOCATION_NAME, ROOM_ID, SURGERY_DATE,
             PRIME_TIME_START, PRIME_TIME_END) %>%
    arrange(PATIENT_IN_ROOM_DTTM, .by_group = TRUE) %>%
    summarise(
      n_cases     = n(),
      proc_pt_min = sum(proc_pt_min),
      tat_pt_min  = sum(tat_pt_min),
      used_pt_min = sum(used_pt_min),
      gaps = list({
        pt_start <- first(PRIME_TIME_START)
        pt_end   <- first(PRIME_TIME_END)
        busy_start <- pmax(PATIENT_IN_ROOM_DTTM, pt_start)
        busy_end   <- pmin(PATIENT_OUT_AND_SETUP_CLEANUP_END, pt_end)
        keep <- busy_end > busy_start
        bs <- busy_start[keep]; be <- busy_end[keep]
        if (length(bs) == 0) { 
          as.numeric(difftime(pt_end, pt_start, units = "mins"))
        } else {
          pmax(c(
            as.numeric(difftime(bs[1], pt_start, units = "mins")),
            if (length(bs) > 1)
              as.numeric(difftime(bs[-1], be[-length(be)], units = "mins")),
            as.numeric(difftime(pt_end, be[length(be)], units = "mins"))
          ), 0)
        }
      }),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      available_pt_min   = as.numeric(difftime(PRIME_TIME_END, PRIME_TIME_START, units = "mins")),
      recoverable_min    = sum(gaps[gaps >= RECOVERABLE_THRESHOLD]),
      nonrecoverable_min = sum(gaps[gaps <  RECOVERABLE_THRESHOLD])
    ) %>%
    ungroup() %>%
    select(-gaps) %>%
    mutate(month = month(SURGERY_DATE),
           day   = day(SURGERY_DATE))
  
  # =================================================================
  # 4. Baseline summary table
  # =================================================================
  baseline_table <- room_day %>%
    group_by(Cluster = CLUSTER_NAME,
             # Department = PRIMARY_SURGEON_SPECIALTY,
             LOCATION_NAME,
             `OR Room` = ROOM_ID,
             SURGERY_DATE) %>%
    summarise(
      `# Cases`                         = sum(n_cases),
      `Prime Time Used Time`            = sum(used_pt_min),
      `Prime Time Available Time`       = sum(available_pt_min),
      `Prime Time Utilization`          = `Prime Time Used Time` / `Prime Time Available Time`,
      `Prime Time Recoverable Time`     = sum(recoverable_min),
      `Prime Time Non Recoverable Time` = sum(nonrecoverable_min),
      `Prime Time Procedure Time`       = sum(proc_pt_min),
      `Prime Time TAT`                  = sum(tat_pt_min),
      .groups = "drop"
    ) %>%
    relocate(`Prime Time Utilization`, .after = `# Cases`)
}