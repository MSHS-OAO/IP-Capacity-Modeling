source('functions/or_constants_functions.R')


# Get volume projection data ----
file_location <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/archive/"
file_name <- "daily_demand_encounter.xlsx"


data_baseline_ip <- read_xlsx(paste0(file_location,file_name),sheet = 'baseline')
data_volume_projections_ip <- read_xlsx(paste0(file_location,file_name),sheet = 'scenario')


# max admin date, mrn list and min discharge date
min_admin_date <- format(min(data_baseline_ip$ADMIT_DT_SRC),'%Y-%m-%d')
max_discharge_date <- format(max(data_baseline_ip$DSCH_DT_SRC),'%Y-%m-%d')
mrn_list_ip <- data_baseline_ip %>%
  mutate( MSMRN = trimws(MSMRN)) %>%
  select(MSMRN,ADMIT_DT_SRC,DSCH_DT_SRC) %>%
  distinct()


# Get OR data based on ipdata ----
or_cases_baseline <- get_or_data(sched_start_date = min_admin_date, sched_end_date = max_discharge_date,status = 'Completed', mrn_list = mrn_list_ip)



# Cohort flag: stay columns attached = case belongs to a baseline IP stay
or_cases_baseline <- or_cases_baseline %>%
  mutate(is_cohort = !is.na(ADMIT_DT_SRC))


or_cases_baseline %>% count(is_cohort)   

# Calculate baseline metrics ----
summary_metrics_baseline <- summary_metrics(or_cases_baseline) %>%
  mutate(scenario = "Baseline")


# =================================================================
# Parent templates: index surgery per cohort stay ----
# offset = day-of-stay the surgery happened; time_of_day = clock time
# =================================================================
parent_templates <- or_cases_baseline %>%
  filter(is_cohort) %>%
  mutate(
    admit_date     = as.Date(ADMIT_DT_SRC),
    surgery_offset = as.numeric(as.Date(PATIENT_IN_ROOM_DTTM, tz = "America/New_York") - admit_date),
    time_of_day    = format(PATIENT_IN_ROOM_DTTM, "%H:%M:%S")
  ) %>%
  group_by(PATIENT_MRN, admit_date) %>%
  slice_min(PATIENT_IN_ROOM_DTTM, n = 1, with_ties = FALSE) %>%   # index case per stay
  ungroup() %>%
  select(parent_mrn = PATIENT_MRN, surgery_offset, time_of_day,
         MINUTES_IN_ROOM_TO_OUT_ROOM, TURNOVER_FROM_PRIOR_CASE,
         PRIMARY_SURGEON, PRIMARY_SURGEON_SPECIALTY,
         PRIMARY_PROCEDURE_CODE, PRIMARY_PROCEDURE_DESC,
         ANESTHESIA_TYPE, LOCATION_NAME, CLUSTER_NAME, ROOM_ID) %>%
  # one template per parent MRN (if a patient had multiple cohort stays,
  # keep the first encountered; change rule here if needed)
  distinct(parent_mrn, .keep_all = TRUE)



# =================================================================
# Dummy cases: encounter grain, offset-clone from parent ----
# =================================================================
dummy_cases <- data_volume_projections_ip %>%
  filter(str_detect(MSMRN, "_")) %>%
  mutate(across(c(NEW_ADMIT_DT_SRC, NEW_DSCH_DT_SRC),
                ~ as.Date(force_tz(.x, "America/New_York"))),
         parent_mrn = str_remove(trimws(MSMRN), "_\\d+$")) %>%   # strips "_12" style suffixes
  distinct(ENCOUNTER_NO, MSMRN, parent_mrn, NEW_ADMIT_DT_SRC, NEW_DSCH_DT_SRC) %>%
  inner_join(parent_templates, by = "parent_mrn") %>%
  mutate(
    SURGERY_DATE          = NEW_ADMIT_DT_SRC + surgery_offset,
    PATIENT_IN_ROOM_DTTM  = as.POSIXct(paste(SURGERY_DATE, time_of_day),
                                       tz = "America/New_York"),
    PATIENT_OUT_ROOM_DTTM = PATIENT_IN_ROOM_DTTM +
      minutes(as.integer(MINUTES_IN_ROOM_TO_OUT_ROOM)),
    OR_CASE_ID  = paste0("DUMMY_", ENCOUNTER_NO),
    PATIENT_MRN = MSMRN
  )

# =================================================================
# Combine: dummies layered onto the full schedule ----
# =================================================================
combine_cols <- c("OR_CASE_ID", "PATIENT_MRN",
                  "PATIENT_IN_ROOM_DTTM", "PATIENT_OUT_ROOM_DTTM",
                  "MINUTES_IN_ROOM_TO_OUT_ROOM", "TURNOVER_FROM_PRIOR_CASE",
                  "SURGERY_DATE",
                  "PRIMARY_SURGEON", "PRIMARY_SURGEON_SPECIALTY",
                  "PRIMARY_PROCEDURE_CODE", "PRIMARY_PROCEDURE_DESC",
                  "ANESTHESIA_TYPE", "LOCATION_NAME", "CLUSTER_NAME", "ROOM_ID")

all_cases <- bind_rows(
  or_cases_baseline %>%
    mutate(SURGERY_DATE = as.Date(SURGERY_DATE)) %>%
    select(all_of(combine_cols)) %>%
    mutate(is_dummy = FALSE),
  dummy_cases %>%
    select(all_of(combine_cols)) %>%
    mutate(is_dummy = TRUE)
)


# =================================================================
# Collision gate: dummies vs ALL real occupancy ----
# =================================================================
conflicts <- all_cases %>%
  group_by(ROOM_ID, SURGERY_DATE) %>%
  arrange(PATIENT_IN_ROOM_DTTM, .by_group = TRUE) %>%
  mutate(overlap_min = as.numeric(difftime(lag(PATIENT_OUT_ROOM_DTTM),
                                           PATIENT_IN_ROOM_DTTM, units = "mins")),
         prev_case   = lag(OR_CASE_ID)) %>%
  ungroup() %>%
  filter(overlap_min > 0)


conflicts %>% count(is_dummy)
cat("Dummy collision rate:",
    round(100 * sum(conflicts$is_dummy) / max(sum(all_cases$is_dummy), 1), 1), "%\n")


# =================================================================
# Re-prep combined set ----
# Mirrors get_or_data's post-collect steps (lines 146-168 of the
# functions file); turnover attribution must be recomputed with
# dummies inserted into each room's sequence.
# =================================================================
prepped_projected <- prime_time_location(all_cases) %>%
  distinct(OR_CASE_ID, .keep_all = TRUE) %>%
  group_by(ROOM_ID) %>%
  arrange(PATIENT_OUT_ROOM_DTTM, .by_group = TRUE) %>%
  mutate(setup_and_cleanup_time = coalesce(lead(TURNOVER_FROM_PRIOR_CASE), 0)) %>%
  ungroup() %>%
  mutate(
    PATIENT_OUT_AND_SETUP_CLEANUP_END = PATIENT_OUT_ROOM_DTTM +
      minutes(as.integer(setup_and_cleanup_time)),
    PrimeTimeInterval  = interval(PRIME_TIME_START, PRIME_TIME_END, tzone = "America/New_York"),
    ProcedureInterval  = interval(PATIENT_IN_ROOM_DTTM, PATIENT_OUT_ROOM_DTTM, tzone = "America/New_York"),
    SetupTimeInterval  = interval(PATIENT_OUT_ROOM_DTTM, PATIENT_OUT_AND_SETUP_CLEANUP_END, tzone = "America/New_York"),
    overlap_primetime_procedure = intersect(PrimeTimeInterval, ProcedureInterval),
    overlap_primetime_setup     = intersect(PrimeTimeInterval, SetupTimeInterval),
    month        = month(SURGERY_DATE),
    month_date   = floor_date(SURGERY_DATE, unit = 'month'),
    day_of_month = day(SURGERY_DATE),
    week_of_year = week(SURGERY_DATE),
    year         = year(SURGERY_DATE)
  )
# Note: no force_tz here — baseline timestamps were already fixed inside
# get_or_data, and dummy timestamps were built directly in America/New_York.


# Calculate projected metrics ----
summary_metrics_projected <- summary_metrics(prepped_projected) %>%
  mutate(scenario = "Baseline + New Volume")


# =================================================================
# Comparison table: Baseline vs Baseline + New Volume ----
# =================================================================
summary_all <- bind_rows(summary_metrics_baseline, summary_metrics_projected)

summary_wide <- summary_all %>%
  pivot_wider(names_from = scenario,
              values_from = `# Cases`:`Prime Time TAT`,
              names_glue = "{scenario} {.value}") %>%
  # room-days that exist only in the projected scenario (dummy opened an
  # empty day): Baseline truth = 0 cases, full window recoverable
  mutate(
    `Baseline # Cases` = coalesce(`Baseline # Cases`, 0L),
    `Baseline Prime Time Available Time` =
      coalesce(`Baseline Prime Time Available Time`,
               `Baseline + New Volume Prime Time Available Time`),
    across(c(`Baseline Prime Time Used Time`, `Baseline Prime Time Procedure Time`,
             `Baseline Prime Time TAT`, `Baseline Prime Time Non Recoverable Time`),
           ~ coalesce(.x, 0)),
    `Baseline Prime Time Recoverable Time` =
      coalesce(`Baseline Prime Time Recoverable Time`,
               `Baseline Prime Time Available Time`),
    `Baseline Prime Time Utilization` = coalesce(`Baseline Prime Time Utilization`, 0),
    `Δ Cases`       = `Baseline + New Volume # Cases` - `Baseline # Cases`,
    `Δ Utilization` = `Baseline + New Volume Prime Time Utilization` - `Baseline Prime Time Utilization`,
    `Δ Recoverable` = `Baseline + New Volume Prime Time Recoverable Time` - `Baseline Prime Time Recoverable Time`
  )


# =============
# Validation ---------
# =============

neg_days <- summary_wide %>% filter(`Δ Cases` < 0) %>%
  select(`OR Room`, SURGERY_DATE)


# pick one and compare the room's sequence across scenarios:
prepped_projected %>%
  semi_join(neg_days, by = c("ROOM_ID" = "OR Room", "SURGERY_DATE")) %>%
  arrange(PATIENT_IN_ROOM_DTTM) %>%
  select(OR_CASE_ID, PATIENT_IN_ROOM_DTTM, PATIENT_OUT_ROOM_DTTM,
         TURNOVER_FROM_PRIOR_CASE, setup_and_cleanup_time)