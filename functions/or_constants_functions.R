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


# MSHS brand palette
mshs_cyan    <- "#06ABEB"
mshs_magenta <- "#DC298D"
mshs_violet  <- "#212070"
mshs_gray    <- "#63666A"
scenario_cols <- c("Baseline" = mshs_violet, "Volume Projections" = mshs_cyan)

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


# Build per-location stepwise factor table (seconds-of-day band bounds).
# ORSchedules times read as character like "7:45:00 AM" -> parse to seconds.
to_sec <- function(x) {
  as.numeric(as_hms(format(
    lubridate::parse_date_time(x, orders = "I:M:S p"), "%H:%M:%S")))
}


# Build per-location stepwise factor table (seconds-of-day band bounds)
cascade_factor <- read_xlsx(paste0(file_location, paste0("OR Modeling/SupplementData/","ORSchedules.xlsx")), sheet = "10-2025") %>%
  filter(!is.na(Location), Location != "") %>%
  transmute(
    casc_loc = Location,
    b_start  = to_sec(`Time Start`),
    b_end    = to_sec(`Time End`),
    n_ors    = as.integer(`# ORs`)
  ) %>%
  group_by(casc_loc) %>%
  mutate(peak = max(n_ors), factor = n_ors / peak) %>%
  ungroup()


cascade_bands <- cascade_factor %>%
  mutate(band_min = (b_end - b_start) / 60, capacity_min = n_ors * band_min)

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
# Output grain: Hospital / Department / Surgeon / Month / Day.
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
    mutate(casc_loc = xwalk_loc(LOCATION_NAME))
  
  # --- RAW metrics (all records, not penalized) -> Hospital x Month x Day ---
  raw_all <- base %>%
    mutate(
      used_pt_min = pmax(0, as.numeric(difftime(
        pmin(PATIENT_OUT_AND_SETUP_CLEANUP_END, PRIME_TIME_END),
        pmax(PATIENT_IN_ROOM_DTTM, PRIME_TIME_START), units = "mins"))),
      proc_pt_min = pmax(0, as.numeric(difftime(
        pmin(PATIENT_OUT_ROOM_DTTM, PRIME_TIME_END),
        pmax(PATIENT_IN_ROOM_DTTM, PRIME_TIME_START), units = "mins"))),
      tat_pt_min  = used_pt_min - proc_pt_min,
      Hospital = casc_loc, Month = month(SURGERY_DATE), Day = day(SURGERY_DATE)
    ) %>%
    group_by(Hospital, Month, Day) %>%
    summarise(`# Cases`                   = n(),
              `Prime Time Used Time`      = sum(used_pt_min),
              `Prime Time Procedure Time` = sum(proc_pt_min),
              `Prime Time TAT`            = sum(tat_pt_min),
              .groups = "drop")
  
  # --- PENALIZED gaps + available (room-assigned records only) ---
  base_rooms <- base %>% filter(!is.na(ROOM_ID))
  
  ordered_cases <- base_rooms %>%
    group_by(ROOM_ID, SURGERY_DATE) %>%
    arrange(PATIENT_IN_ROOM_DTTM, .by_group = TRUE) %>%
    mutate(busy_start = pmax(PATIENT_IN_ROOM_DTTM, PRIME_TIME_START),
           busy_end   = pmin(PATIENT_OUT_AND_SETUP_CLEANUP_END, PRIME_TIME_END),
           seq_in_day = row_number(), n_in_day = n()) %>%
    ungroup() %>%
    filter(busy_end > busy_start)
  
  gap_opening <- ordered_cases %>%
    filter(seq_in_day == 1) %>%
    transmute(ROOM_ID, SURGERY_DATE, casc_loc,
              gap_start = PRIME_TIME_START, gap_end = busy_start) %>%
    filter(gap_end > gap_start)
  
  gap_between <- ordered_cases %>%
    group_by(ROOM_ID, SURGERY_DATE) %>%
    arrange(seq_in_day, .by_group = TRUE) %>%
    mutate(next_start = lead(busy_start)) %>%
    ungroup() %>%
    filter(!is.na(next_start)) %>%
    transmute(ROOM_ID, SURGERY_DATE, casc_loc,
              gap_start = busy_end, gap_end = next_start) %>%
    filter(gap_end > gap_start)
  
  gap_closing <- ordered_cases %>%
    filter(seq_in_day == n_in_day) %>%
    transmute(ROOM_ID, SURGERY_DATE, casc_loc,
              gap_start = busy_end, gap_end = PRIME_TIME_END) %>%
    filter(gap_end > gap_start)
  
  gaps_pen <- bind_rows(gap_opening, gap_between, gap_closing) %>%
    mutate(.rid = row_number()) %>%
    inner_join(cascade_factor, by = "casc_loc", relationship = "many-to-many") %>%
    mutate(s = as.numeric(as_hms(format(gap_start, "%H:%M:%S"))),
           e = as.numeric(as_hms(format(gap_end,   "%H:%M:%S"))),
           seg = pmax(0, pmin(e, b_end) - pmax(s, b_start)) / 60 * factor) %>%
    group_by(.rid, ROOM_ID, SURGERY_DATE) %>%
    summarise(gap_min = sum(seg), .groups = "drop") %>%
    mutate(recoverable_min    = if_else(gap_min >= RECOVERABLE_THRESHOLD, gap_min, 0),
           nonrecoverable_min = if_else(gap_min <  RECOVERABLE_THRESHOLD, gap_min, 0)) %>%
    group_by(ROOM_ID, SURGERY_DATE) %>%
    summarise(recoverable_min    = sum(recoverable_min),
              nonrecoverable_min = sum(nonrecoverable_min), .groups = "drop")
  
  rd_avail <- base_rooms %>%
    distinct(ROOM_ID, SURGERY_DATE, casc_loc, PRIME_TIME_START, PRIME_TIME_END) %>%
    inner_join(cascade_factor, by = "casc_loc", relationship = "many-to-many") %>%
    mutate(s = as.numeric(as_hms(format(PRIME_TIME_START, "%H:%M:%S"))),
           e = as.numeric(as_hms(format(PRIME_TIME_END,   "%H:%M:%S"))),
           seg = pmax(0, pmin(e, b_end) - pmax(s, b_start)) / 60 * factor) %>%
    group_by(ROOM_ID, SURGERY_DATE) %>%
    summarise(available_pt_min = sum(seg), .groups = "drop")
  
  pen_by_grain <- base_rooms %>%
    mutate(Hospital = casc_loc, Month = month(SURGERY_DATE), Day = day(SURGERY_DATE)) %>%
    distinct(Hospital, Month, Day, ROOM_ID, SURGERY_DATE) %>%
    left_join(rd_avail,  by = c("ROOM_ID", "SURGERY_DATE")) %>%
    left_join(gaps_pen,  by = c("ROOM_ID", "SURGERY_DATE")) %>%
    mutate(available_pt_min   = coalesce(available_pt_min, 0),
           recoverable_min    = coalesce(recoverable_min, 0),
           nonrecoverable_min = coalesce(nonrecoverable_min, 0)) %>%
    group_by(Hospital, Month, Day) %>%
    summarise(
      `Prime Time Available Time`       = sum(available_pt_min[!duplicated(paste(ROOM_ID, SURGERY_DATE))]),
      `Prime Time Recoverable Time`     = sum(recoverable_min[!duplicated(paste(ROOM_ID, SURGERY_DATE))]),
      `Prime Time Non Recoverable Time` = sum(nonrecoverable_min[!duplicated(paste(ROOM_ID, SURGERY_DATE))]),
      .groups = "drop"
    )
  
  raw_all %>%
    full_join(pen_by_grain, by = c("Hospital", "Month", "Day")) %>%
    mutate(across(c(`# Cases`, `Prime Time Used Time`, `Prime Time Procedure Time`,
                    `Prime Time TAT`, `Prime Time Available Time`,
                    `Prime Time Recoverable Time`, `Prime Time Non Recoverable Time`),
                  ~ coalesce(.x, 0)),
           `Prime Time Utilization` = if_else(`Prime Time Available Time` > 0,
                                              `Prime Time Used Time` / `Prime Time Available Time`, NA_real_),
           scenario = scenario_label) %>%
    relocate(`Prime Time Utilization`, .after = `# Cases`)
}




# #################################################################
# PROJECTED band ----  baseline metrics adjusted by the dummy pool.
# New-volume minutes (per Hospital x Month x Day) EAT recoverable first:
#   recoverable_proj = max(0, recoverable_base - dummy_min)
#   used_proj        = used_base + dummy_min ;  available unchanged
# Overflow beyond recoverable is NOT shown here (lives in collision sheet).
# #################################################################
project_with_volume <- function(baseline_out, dummy_cases, scenario_label) {
  
  dummy_pool <- dummy_cases %>%
    prime_time_location() %>%
    mutate(
      used_pt_min = pmax(0, as.numeric(difftime(
        pmin(PATIENT_OUT_ROOM_DTTM, PRIME_TIME_END),
        pmax(PATIENT_IN_ROOM_DTTM, PRIME_TIME_START), units = "mins"))),
      Hospital = casc_loc, Month = month(SURGERY_DATE), Day = day(SURGERY_DATE)
    ) %>%
    group_by(Hospital, Month, Day) %>%
    summarise(dummy_cases = n(), dummy_min = sum(used_pt_min), .groups = "drop")
  
  baseline_out %>%
    select(-scenario) %>%
    full_join(dummy_pool, by = c("Hospital", "Month", "Day")) %>%
    mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>%
    mutate(
      `# Cases`              = `# Cases` + dummy_cases,
      `Prime Time Used Time` = `Prime Time Used Time` + dummy_min,
      # new volume eats recoverable first
      `Prime Time Recoverable Time` = pmax(0, `Prime Time Recoverable Time` - dummy_min),
      `Prime Time Utilization` = if_else(`Prime Time Available Time` > 0,
                                         `Prime Time Used Time` / `Prime Time Available Time`, NA_real_),
      scenario = scenario_label
    ) %>%
    select(-dummy_cases, -dummy_min) %>%
    relocate(`Prime Time Utilization`, .after = `# Cases`)
}


# =================================================================
# Cascade collision rate (capacity model) ----
# Demand = case room-minutes (INCLUDING setup/cleanup) within each band.
# Each band is clipped to the location-day's PRIME-TIME window, so only
# prime-time capacity and prime-time demand are counted.
# capacity(band) = rooms_open * (clipped band minutes)
# =================================================================
collision_by_band <- function(cases, scenario_label) {
  cases %>%
    mutate(casc_loc = xwalk_loc(LOCATION_NAME),
           SURGERY_DATE = as.Date(SURGERY_DATE),
           start_s = as.numeric(as_hms(format(PATIENT_IN_ROOM_DTTM, "%H:%M:%S"))),
           end_s   = as.numeric(as_hms(format(OCC_END,             "%H:%M:%S"))),  # incl. setup
           pt_s    = as.numeric(as_hms(format(PRIME_TIME_START,    "%H:%M:%S"))),
           pt_e    = as.numeric(as_hms(format(PRIME_TIME_END,      "%H:%M:%S")))) %>%
    filter(!is.na(casc_loc)) %>%
    inner_join(cascade_factor, by = "casc_loc", relationship = "many-to-many") %>%
    # clip each band to the prime-time window for this case's location-day
    mutate(cb_start = pmax(b_start, pt_s),
           cb_end   = pmin(b_end,   pt_e)) %>%
    filter(cb_end > cb_start) %>%                       # drop bands outside prime time
    mutate(overlap_min = pmax(0, pmin(end_s, cb_end) - pmax(start_s, cb_start)) / 60) %>%
    group_by(casc_loc, SURGERY_DATE, cb_start, cb_end, n_ors) %>%
    summarise(demand_min = sum(overlap_min), .groups = "drop") %>%
    mutate(capacity_min = n_ors * (cb_end - cb_start) / 60,
           collision  = demand_min > capacity_min,
           excess_min = pmax(0, demand_min - capacity_min),
           band_start = format(as_hms(cb_start), "%H:%M"),
           band_end   = format(as_hms(cb_end),   "%H:%M"),
           band       = paste0(band_start, "\u2013", band_end),
           scenario   = scenario_label) %>%
    relocate(band, band_start, band_end, .after = SURGERY_DATE)
}



# =================================================================
# Graph: room demand (ORs/hour) vs staffed capacity ----
# =================================================================
expand_hours <- function(cases, scenario_label) {
  cases %>%
    mutate(casc_loc = xwalk_loc(LOCATION_NAME),
           h0 = floor(as.numeric(as_hms(format(PATIENT_IN_ROOM_DTTM, "%H:%M:%S"))) / 3600),
           h1 = floor(as.numeric(as_hms(format(OCC_END,             "%H:%M:%S"))) / 3600)) %>%  # incl. setup
    filter(!is.na(casc_loc)) %>%
    rowwise() %>% mutate(hr = list(seq(h0, h1))) %>% unnest(hr) %>% ungroup() %>%
    count(casc_loc, SURGERY_DATE = as.Date(SURGERY_DATE), hr, name = "concurrent") %>%
    group_by(casc_loc, hr) %>%
    summarise(ors_needed = mean(concurrent), .groups = "drop") %>%
    mutate(scenario = scenario_label)
}