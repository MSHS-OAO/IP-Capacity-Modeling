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
library(openxlsx)


# MSHS brand palette
mshs_cyan    <- "#06ABEB"
mshs_magenta <- "#DC298D"
mshs_violet  <- "#212070"
mshs_gray    <- "#63666A"
mshs_theme <- theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                    axis.title = element_text(size = 12),
                    axis.text = element_text(size = 10),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.title = element_text(size = 12),
                    legend.text = element_text(size = 8),
                    strip.text = element_text(size = 12, face = "bold"),
                    legend.margin = margin(l = 50,r = 50),
                    panel.grid = element_blank())


file_location <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"


# #################################################################
# CASCADE FACTOR CURVES  ----
# Effective capacity per location x minute-of-day, from ORSchedules.
# factor(minute) = rooms_open_in_band / peak_rooms_for_location.
# Peak = max rooms that location ever staffs (the 1.0 reference).
# e.g. MSH 51 rooms daytime -> 30 after 16:00 gives factor 30/51 = 0.588.
# #################################################################

# Location crosswalk: OR LOCATION_NAME -> cascade Location code (confirmed)
loc_xwalk <- tribble(
  ~location_pattern,         ~casc_loc,
  "5 E 98TH ST AMB SUITE",   "MSH",
  "MSDC|MSDUS",              "MSBI",
  "MSH",                     "MSH",
  "MSW",                     "MSW",
  "MSM",                     "MSM",
  "MSQ",                     "MSQ",
  "MSB",                     "MSB"
)
xwalk_loc <- function(loc) {
  out <- rep(NA_character_, length(loc))
  for (i in seq_len(nrow(loc_xwalk))) {
    hit <- is.na(out) & str_detect(loc, loc_xwalk$location_pattern[i])
    out[hit] <- loc_xwalk$casc_loc[i]
  }
  out
}


to_sec <- function(x) {
  as.numeric(as_hms(format(
    lubridate::parse_date_time(x, orders = "I:M:S p"), "%H:%M:%S")))
}


# Build per-location stepwise factor table (seconds-of-day band bounds)
cascade_factor <- read_xlsx(paste0(file_location, paste0("OR Modeling/SupplementData/","ORSchedules.xlsx")), sheet = "10-2025") %>%
  filter(!is.na(Location), Location != "") %>%
  # transmute(
  #   casc_loc = Location,
  #   b_start  = to_sec(`Time Start`),
  #   b_end    = to_sec(`Time End`),
  #   n_ors    = as.integer(`# ORs`)
  # ) %>%
  group_by(Location) %>%
  mutate( peak = max(`# ORs`), 
          factor = `# ORs` / peak,
          b_start  = to_sec(`Time Start`),
          b_end    = to_sec(`Time End`),
          band_min = (b_end - b_start) / 60, capacity_min =  `# ORs` * band_min
  ) %>%
  ungroup() %>%
  select(Location, `Time Start`,`Time End`, `# ORs`,factor,capacity_min)


# cascade_bands <- cascade_factor %>%
#   mutate(band_min = (b_end - b_start) / 60, capacity_min = n_ors * band_min)

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
    ))%>%
    group_by(OR_CASE_ID) %>%
    slice_max(ADMIT_DT_SRC, n = 1, with_ties = FALSE, na_rm = FALSE) %>%
    ungroup()
    
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




# #################################################################
# METRICS ENGINE  ----
# Output grain: Hospital / Year. Raw metrics are mean-per-case; penalized
# metrics are mean-per-room-day.
# ONLY two things are penalized by the cascade staffing curve:
#   - Prime Time Available Time  (the utilization denominator)
#   - the gaps -> Recoverable / Non Recoverable
# Everything else (# Cases, Used, Procedure, TAT) is RAW clock time.
# Utilization = RAW used / PENALIZED available, so running cases in
# de-staffed hours pushes utilization up (can exceed 100% by design).
# Recoverable threshold (>=180) applies to the penalized gap value.
# #################################################################
summary_metrics_weighted <- function(processed_or_cases, scenario_label) {
  
  RECOVERABLE_THRESHOLD <- 180  # minutes (on the cascade-penalized gap value)


  base <- processed_or_cases %>%
    filter(!is.na(overlap_primetime_procedure) | !is.na(overlap_primetime_setup)) %>%
    mutate(Location = xwalk_loc(LOCATION_NAME))
  
  # --- RAW metrics (all records, not penalized), mean per case -> Hospital x Year ---
  # Prime-time minutes come straight from the pre-built overlap intervals:
  # procedure = case in-room to out-room; TAT = the following setup/cleanup.
  raw_all <- base %>%
    mutate(
      proc_pt_min = int_length(overlap_primetime_procedure) / 60,
      tat_pt_min  = int_length(overlap_primetime_setup) / 60,
      used_pt_min = coalesce(proc_pt_min, 0) + coalesce(tat_pt_min, 0),
      Year = year(SURGERY_DATE)
    ) %>%
    # 1) total per location-day
    group_by(Location, Year, SURGERY_DATE) %>%
    summarise(cases_day = n(),
              used_day   = sum(used_pt_min, na.rm = TRUE),
              proc_day   = sum(proc_pt_min, na.rm = TRUE),
              tat_day    = sum(tat_pt_min,  na.rm = TRUE),
              .groups = "drop") %>%
    # 2) mean across days
    group_by(Location, Year) %>%
    summarise(`# Cases`                   = sum(cases_day),          # total cases (count)
              `Prime Time Used Time`      = mean(used_day),          # mean daily used
              `Prime Time Procedure Time` = mean(proc_day),
              `Prime Time TAT`            = mean(tat_day),
              .groups = "drop")
  
  # --- PENALIZED gaps + available (room-assigned records only) ---
  base_rooms <- base %>% filter(!is.na(ROOM_ID))

  # busy intervals per room-day, clipped to prime time, ordered by in-room time
  ordered_cases <- base_rooms %>%
    group_by(ROOM_ID, SURGERY_DATE, Location) %>%
    arrange(PATIENT_IN_ROOM_DTTM, .by_group = TRUE) %>%
    mutate(  # clip each case to the prime-time window
      case_order = row_number(),
      first_case_start  = min(PATIENT_IN_ROOM_DTTM, na.rm = TRUE),
      last_case_end_tat_incl  = max(PATIENT_OUT_AND_SETUP_CLEANUP_END, na.rm = TRUE),
      previous_case_end = lag(PATIENT_OUT_AND_SETUP_CLEANUP_END, n = 1) 
      
    ) %>%
    select(ROOM_ID, 
           SURGERY_DATE, 
           Location, 
           PATIENT_IN_ROOM_DTTM, 
           PATIENT_OUT_AND_SETUP_CLEANUP_END,
           PRIME_TIME_START, 
           PRIME_TIME_END,
           case_order,
           first_case_start,
           last_case_end_tat_incl,
           previous_case_end) %>%
    ungroup()

  # All gaps in one pass: the complement of the busy intervals inside the
  # prime-time window. For n busy intervals this yields n+1 candidate gaps
  # (open -> first case, between cases, last case -> close); empty ones drop.
  gaps <- ordered_cases %>%
    mutate(gap_at_start = if_else(difftime(first_case_start,PRIME_TIME_START,units = "min")>0,
                                  as.numeric(difftime(first_case_start,PRIME_TIME_START,units = "min")),0),
           gap_between = if_else(difftime(PATIENT_IN_ROOM_DTTM,previous_case_end,units = "min")>0,
                                 as.numeric(difftime(PATIENT_IN_ROOM_DTTM,previous_case_end,units = "min")),0),
           gap_end = if_else(difftime(PRIME_TIME_END,last_case_end_tat_incl,units = "min")>0,
                             as.numeric(difftime(PRIME_TIME_END,last_case_end_tat_incl,units = "min")),0))
  
  gaps_start <- gaps %>%
    select(ROOM_ID,
           SURGERY_DATE,
           Location,
           PRIME_TIME_START,
           first_case_start,gap_at_start) %>%
    unique() %>%
    mutate(Recoverable = if_else(gap_at_start >=RECOVERABLE_THRESHOLD,
                                 'Recoverable Start',
                                 'Non Recoverable Start')) %>%
    group_by(ROOM_ID,
             SURGERY_DATE,
             Location) %>%
    pivot_wider(
      id_cols = c("ROOM_ID",
                  "SURGERY_DATE",
                  "Location"),
      names_from = Recoverable,
      values_from = gap_at_start,
      values_fn = sum
    )%>%
    replace_na(list(`Recoverable Start` = 0, `Non Recoverable Start` = 0))
  
  gaps_mid <- gaps %>%
    filter(case_order > 1) %>%                       # a between-gap exists only from case 2 on
    select(ROOM_ID,
           SURGERY_DATE, Location, case_order, gap_between) %>%
    unique() %>%
    drop_na()%>%
    mutate(Recoverable = if_else(gap_between >=RECOVERABLE_THRESHOLD,
                                 'Recoverable Mid',
                                 'Non Recoverable Mid'))%>%
    group_by(ROOM_ID,
             SURGERY_DATE,
             Location) %>%
    pivot_wider(
      id_cols = c("ROOM_ID",
                  "SURGERY_DATE",
                  "Location"),
      names_from = Recoverable,
      values_from = gap_between,
      values_fn = sum
    ) %>%
    replace_na(list(`Recoverable Mid` = 0, `Non Recoverable Mid` = 0))
  
  gaps_end <- gaps %>%
    select(ROOM_ID,
           SURGERY_DATE,
           Location,
           PRIME_TIME_END,
           last_case_end_tat_incl,gap_end) %>%
    unique() %>%
    left_join(cascade_factor) %>%
    select(ROOM_ID,
           SURGERY_DATE,
           Location,
           PRIME_TIME_END,
           last_case_end_tat_incl,
           gap_end,
           `Time Start`,`Time End`,factor) %>%
    mutate(gap_end_interval = interval(last_case_end_tat_incl, PRIME_TIME_END, tzone = "America/New_York"),
           cascade_start = as.POSIXct(paste0(SURGERY_DATE," ",`Time Start`),format="%Y-%m-%d %I:%M:%S %p"),
           cascade_end =  as.POSIXct(paste0(SURGERY_DATE," ",`Time End`),format="%Y-%m-%d %I:%M:%S %p"),
           cascade_interval = interval(cascade_start, cascade_end, tzone = "America/New_York")) %>%
    select(ROOM_ID,
           SURGERY_DATE,
           Location,
           factor,
           cascade_interval,
           gap_end_interval) %>%
    mutate(overlap_primetime_gap     = intersect(cascade_interval, gap_end_interval),
           gap_length = int_length(overlap_primetime_gap) / 60,
           gap_length_normalized = gap_length*factor) %>%
    filter(!is.na(overlap_primetime_gap))%>%
    mutate(Recoverable = if_else(gap_length_normalized >=RECOVERABLE_THRESHOLD,
                                 'Recoverable End',
                                 'Non Recoverable End'))%>%
    group_by(ROOM_ID,
             SURGERY_DATE,
             Location) %>%
    pivot_wider(
      id_cols = c("ROOM_ID",
                  "SURGERY_DATE",
                  "Location"),
      names_from = Recoverable,
      values_from = gap_length_normalized,
      values_fn = sum
    )%>%
    replace_na(list(`Recoverable End` = 0, `Non Recoverable End` = 0))
  
  
  gaps_summary_room <- gaps_mid %>%
    left_join(gaps_start) %>%
    left_join(gaps_end) %>%
    mutate(across(where(is.numeric), ~ coalesce(.x, 0)),
           Recoverable = `Recoverable End` + `Recoverable Mid` +`Recoverable Start`,
           `Non Recoverable` = `Non Recoverable End` + `Non Recoverable Mid` +`Non Recoverable Start`,
           Year = year(SURGERY_DATE)) %>%
    select(ROOM_ID, SURGERY_DATE, Year, Location, Recoverable, `Non Recoverable`) %>%
    group_by(Location,Year) %>%
    summarise(`Prime Time Recoverable Time` = mean(Recoverable),
              `Prime Time Non Recoverable Time` = mean(`Non Recoverable`),
              .groups = "drop")
    

  # penalized available time = the prime-time window itself, one per room-day
  rd_avail <- base_rooms %>%
    distinct(SURGERY_DATE, Location,PRIME_TIME_START, PRIME_TIME_END) %>%
    left_join(cascade_factor) %>%
    mutate(cascade_start = as.POSIXct(paste0(SURGERY_DATE," ",`Time Start`),format="%Y-%m-%d %I:%M:%S %p"),
           cascade_end =  as.POSIXct(paste0(SURGERY_DATE," ",`Time End`),format="%Y-%m-%d %I:%M:%S %p"),
           cascade_interval = interval(cascade_start, cascade_end, tzone = "America/New_York"),
           primetime_interval = interval(PRIME_TIME_START, PRIME_TIME_END, tzone = "America/New_York"),
           cascade_primetime_interval = intersect(cascade_interval, primetime_interval)) %>%
    select(Location, SURGERY_DATE,cascade_interval,primetime_interval,cascade_primetime_interval,`# ORs`) %>%
    filter(!is.na(cascade_primetime_interval)) %>%
    mutate(minutes = int_length(cascade_primetime_interval)/60,
           `Prime Time Available Time` = minutes*`# ORs`,
           Year = year(SURGERY_DATE)) %>%
    group_by(Location, SURGERY_DATE, Year) %>%
    summarise(avail_day = sum(`Prime Time Available Time`), .groups = "drop") %>%
    group_by(Location, Year) %>%
    summarise(`Prime Time Available Time` = mean(avail_day), .groups = "drop")  
  
  
  # combine everything
  out <- raw_all %>%
    left_join(rd_avail,          by = c("Location","Year")) %>%
    left_join(gaps_summary_room, by = c("Location","Year")) %>%
    mutate(across(c(`# Cases`, `Prime Time Used Time`, `Prime Time Procedure Time`,
                    `Prime Time TAT`, `Prime Time Available Time`,
                    `Prime Time Recoverable Time`, `Prime Time Non Recoverable Time`),
                  ~ coalesce(.x, 0)),
           `Prime Time Utilization` = if_else(`Prime Time Available Time` > 0,
                                              `Prime Time Used Time` / `Prime Time Available Time`, NA_real_),
           scenario = scenario_label) %>%
    rename(Hospital = Location) %>%
    relocate(`Prime Time Utilization`, .after = `# Cases`)
  
  out
}




# #################################################################
# PROJECTED band ----  baseline metrics adjusted by the dummy pool.
# New-volume minutes (per Hospital x Month x Day) EAT recoverable first:
#   recoverable_proj = max(0, recoverable_base - dummy_min)
#   used_proj        = used_base + dummy_min ;  available unchanged
# Overflow beyond recoverable is NOT shown here (lives in collision sheet).
# #################################################################
project_with_volume <- function(baseline_out, dummy_cases, scenario_label) {
  
  dummy_pool <- prime_time_location(dummy_cases) %>%
    mutate(
      PrimeTimeInterval  = interval(PRIME_TIME_START, PRIME_TIME_END, tzone = "America/New_York"),
      ProcedureInterval  = interval(PATIENT_IN_ROOM_DTTM, PATIENT_OUT_ROOM_DTTM, tzone = "America/New_York"),
      SetupTimeInterval  = interval(PATIENT_OUT_ROOM_DTTM, PATIENT_OUT_AND_SETUP_CLEANUP_END, tzone = "America/New_York"),
      overlap_primetime_procedure = intersect(PrimeTimeInterval, ProcedureInterval),
      overlap_primetime_setup     = intersect(PrimeTimeInterval, SetupTimeInterval),
      proc_pt_min = int_length(overlap_primetime_procedure) / 60,
      tat_pt_min  = int_length(overlap_primetime_setup) / 60,
      used_pt_min = coalesce(proc_pt_min, 0) + coalesce(tat_pt_min, 0),
      Year = year(SURGERY_DATE),
      Hospital = Location, Month = month(SURGERY_DATE)
    ) %>%
    group_by(Hospital, Year, SURGERY_DATE) %>%
    summarise(cases_day = n(),
              used_day   = sum(used_pt_min, na.rm = TRUE),
              proc_day   = sum(proc_pt_min, na.rm = TRUE),
              tat_day    = sum(tat_pt_min,  na.rm = TRUE),
              .groups = "drop") %>%
    # 2) mean across days
    group_by(Hospital, Year) %>%
    summarise(dummy_cases    = sum(cases_day),          # total cases (count)
              dummy_min      = mean(used_day),          # mean daily used
              dummy_min_proc_time = mean(proc_day),
              dummy_min_tat_time            = mean(tat_day),
              .groups = "drop")
  
  baseline_out %>%
    select(-scenario) %>%
    full_join(dummy_pool, by = c("Hospital", "Year")) %>%
    mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>%
    mutate(
      `# Cases`              = `# Cases` + dummy_cases,
      `Prime Time Used Time` = `Prime Time Used Time` + dummy_min,
      # new volume eats recoverable first
      `Prime Time Recoverable Time` = pmax(0, `Prime Time Recoverable Time` - dummy_min),
      `Prime Time Procedure Time` = `Prime Time Procedure Time` + dummy_min_proc_time,
      `Prime Time TAT` = `Prime Time TAT` + dummy_min_tat_time,
      `Prime Time Utilization` = if_else(`Prime Time Available Time` > 0,
                                         `Prime Time Used Time` / `Prime Time Available Time`, NA_real_),
      scenario = scenario_label
    ) %>%
    select(-dummy_cases, -dummy_min,-dummy_min_proc_time,-dummy_min_tat_time) %>%
    relocate(`Prime Time Utilization`, .after = `# Cases`)
}


# =================================================================
# Demand Engine
# =================================================================
demand_function <- function(cases, baseline_dates) {
  
  demand <- cases %>%
    mutate(Location = xwalk_loc(LOCATION_NAME)) %>%
    distinct(OR_CASE_ID,SURGERY_DATE, Location,
             PATIENT_IN_ROOM_DTTM,
             PATIENT_OUT_ROOM_DTTM,
             PATIENT_OUT_AND_SETUP_CLEANUP_END,
             PRIME_TIME_START,
             PRIME_TIME_END,
             ProcedureInterval,
             SetupTimeInterval,
             PrimeTimeInterval) %>%
    mutate(
      primetime_procedure_overlap= intersect(ProcedureInterval,PrimeTimeInterval),
      primetime_setup_overlap= intersect(SetupTimeInterval,PrimeTimeInterval)) %>%
    filter(!is.na(primetime_procedure_overlap)& !is.na(primetime_setup_overlap))  %>%
    mutate(.rid = row_number())%>%
    # select(.rid, all_of(keep_cols), seg_start, seg_end) %>%
    # one row per clock-hour the interval touches
    rowwise() %>%
    mutate(
      hour_bucket = list(
        seq(floor_date(max(PATIENT_IN_ROOM_DTTM),  "hour"),
            floor_date(max(PATIENT_OUT_ROOM_DTTM), "hour"),
            by = "1 hour")
      )
    ) %>%
    ungroup() %>% 
    unnest(hour_bucket)%>%
    mutate(
      hour_end        = hour_bucket + hours(1),
      minutes_in_hour = as.numeric(
        difftime(
          pmin(PATIENT_OUT_ROOM_DTTM, hour_end),
          pmax(PATIENT_IN_ROOM_DTTM,  hour_bucket),
          units = "mins"
        )
      ),
      surgery_hour = hour(hour_bucket)
    ) %>%
    select(Location,OR_CASE_ID, SURGERY_DATE, surgery_hour, hour_bucket, minutes_in_hour) %>%
    distinct()

  

  demand_plot_df <- demand %>%
    distinct(SURGERY_DATE,OR_CASE_ID,surgery_hour,Location) %>%
    group_by(surgery_hour, Location) %>%
    summarise(avg_or_cases = n_distinct(OR_CASE_ID)/baseline_dates) %>%
    ungroup()
    # mutate(
    #   hour_bins = cut(
    #     surgery_hour,
    #     breaks = c(-1, 6, 7:20, 23),
    #     labels = c("0-6", as.character(7:20), "21-23"),
    #     right  = TRUE
    #   )
    # ) %>%
    # group_by(hour_bins) %>%
    # summarise(avg_or_rooms = sum(avg_or_cases)) 
  
}