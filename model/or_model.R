source('functions/or_constants_functions.R')


# Get volume projection data ----
file_name <- "new_daily_demand_encounter.xlsx"


data_baseline_ip <- read_xlsx(paste0(file_location,paste0("archive/",file_name)),sheet = 'baseline')
data_volume_projections_ip <- read_xlsx(paste0(file_location,paste0("archive/",file_name)),sheet = 'scenario')


# max admin date, mrn list and min discharge date
min_admin_date <- format(min(data_baseline_ip$SERVICE_DATE),'%Y-%m-%d')
max_discharge_date <- format(max(data_baseline_ip$SERVICE_DATE),'%Y-%m-%d')
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
summary_metrics_baseline <- summary_metrics_weighted(or_cases_baseline,scenario_label="Baseline")

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
         ANESTHESIA_TYPE, LOCATION_NAME) %>%
  # one template per parent MRN (if a patient had multiple cohort stays,
  # keep the first encountered; change rule here if needed)
  distinct(parent_mrn, .keep_all = TRUE)



# =================================================================
# Dummy cases: encounter grain, offset-clone from parent ----
# =================================================================
new_volume_cases <- data_volume_projections_ip %>%
  filter(str_detect(MSMRN, "_")) %>%
  mutate(across(c(NEW_ADMIT_DT_SRC, NEW_DSCH_DT_SRC),
                ~ as.Date(force_tz(.x, "America/New_York"))),
         parent_mrn = str_remove(trimws(MSMRN), "_\\d+$")) %>%
  distinct(ENCOUNTER_NO, MSMRN, parent_mrn, NEW_ADMIT_DT_SRC, NEW_DSCH_DT_SRC) %>%
  inner_join(parent_templates, by = "parent_mrn") %>%
  mutate(
    SURGERY_DATE_OFFSET          = NEW_ADMIT_DT_SRC + surgery_offset,
    SURGERY_DATE = get_valid_date(SURGERY_DATE_OFFSET, holidays_vec = mshs_holiday),
    PATIENT_IN_ROOM_DTTM  = as.POSIXct(paste(SURGERY_DATE, time_of_day), tz = "America/New_York"),
    PATIENT_OUT_ROOM_DTTM = PATIENT_IN_ROOM_DTTM + minutes(as.integer(MINUTES_IN_ROOM_TO_OUT_ROOM)),
    OR_CASE_ID  = paste0("NEW_", ENCOUNTER_NO),
    PATIENT_MRN = MSMRN
  )


# new_volume_cases_surgery_date_diff <- new_volume_cases %>%
#   filter(SURGERY_DATE_OFFSET!=SURGERY_DATE)%>%
#   select(PATIENT_MRN,OR_CASE_ID,SURGERY_DATE_OFFSET,SURGERY_DATE)


# =================================================================
# Projected metrics ----
# Dummies have NO room (new volume can land anywhere at a location), so
# they DON'T enter the room/gap engine. They contribute to the RAW
# columns only, via extra_raw_cases; penalized columns come from the
# baseline rooms. Dummy capacity impact is shown in the collision view.
# Dummies need CLUSTER_NAME for the Hospital grain; derive from LOCATION.
# =================================================================
new_volume_cases_raw <- prime_time_location(new_volume_cases) %>%
  mutate(
    PATIENT_OUT_AND_SETUP_CLEANUP_END = PATIENT_OUT_ROOM_DTTM + minutes(as.integer(TURNOVER_FROM_PRIOR_CASE)),
    PATIENT_OUT_AND_SETUP_CLEANUP_END = if_else(is.na(PATIENT_OUT_AND_SETUP_CLEANUP_END),
                                                PATIENT_OUT_ROOM_DTTM,PATIENT_OUT_AND_SETUP_CLEANUP_END),
    overlap_primetime_procedure = 1,                            # non-NA so the base filter keeps them
    overlap_primetime_setup     = 1,
    PrimeTimeInterval  = interval(PRIME_TIME_START, PRIME_TIME_END, tzone = "America/New_York"),
    ProcedureInterval  = interval(PATIENT_IN_ROOM_DTTM, PATIENT_OUT_ROOM_DTTM, tzone = "America/New_York"),
    SetupTimeInterval  = interval(PATIENT_OUT_ROOM_DTTM, PATIENT_OUT_AND_SETUP_CLEANUP_END, tzone = "America/New_York"),
    
  ) %>%
  mutate(Location = xwalk_loc(LOCATION_NAME))




# =================================================================
# Projected band ----
# Baseline metrics adjusted by the new-volume pool at Hospital x Month x Day.
# Dummy minutes are added to Used and EAT recoverable first; available is
# unchanged. Dummies need no room. Overflow (dummy beyond recoverable) is
# shown only in the collision view, not here.
# =================================================================
output_projected <- project_with_volume(summary_metrics_baseline, new_volume_cases_raw, "Volume Projections")

output_all <- bind_rows(summary_metrics_baseline, output_projected)


# =================================================================
# Delta table (batched: all Baseline cols, then Projected, then deltas) ----
# =================================================================
output_wide <- output_all %>%
  pivot_wider(names_from = scenario,
              values_from = `# Cases`:`Prime Time Non Recoverable Time`,
              names_glue = "{scenario} {.value}",
              names_vary = "slowest") %>%
  mutate(across(starts_with("Baseline "), ~ coalesce(.x, 0))) %>%
  mutate(
    `Delta Cases`        = `Volume Projections # Cases` - `Baseline # Cases`,
    `Delta Used`         = `Volume Projections Prime Time Used Time` - `Baseline Prime Time Used Time`,
    `Delta Procedure Time` = `Volume Projections Prime Time Procedure Time` - `Baseline Prime Time Procedure Time`,
    `Delta TAT`         = `Volume Projections Prime Time TAT` - `Volume Projections Prime Time TAT`,
    `Delta Utilization`  = `Volume Projections Prime Time Utilization` - `Baseline Prime Time Utilization`,
    `Delta Recoverable`  = `Volume Projections Prime Time Recoverable Time` - `Baseline Prime Time Recoverable Time`
  )



# ==============================================
# Collision - Is demand on par with capacity?
# Capacity comes from cascade data
# ==============================================

# or_cases_baseline_demand <- or_cases_baseline%>%
#   select(LOCATION_NAME,OR_CASE_ID,SURGERY_DATE,
#          PATIENT_IN_ROOM_DTTM,
#          PATIENT_OUT_ROOM_DTTM, ROOM_ID, Weekday) %>%
#   filter(!is.na(PATIENT_OUT_ROOM_DTTM)) %>%
#   filter(PATIENT_IN_ROOM_DTTM<=PATIENT_OUT_ROOM_DTTM)
# 
# collision_data_baseline <- capacity_and_utlization_data(or_cases_baseline_demand,Scenario="Baseline")
# 
# baseline_and_new_volume_data_raw <- new_volume_cases_raw %>%
#   select(LOCATION_NAME,OR_CASE_ID,SURGERY_DATE,
#          PATIENT_IN_ROOM_DTTM,
#          PATIENT_OUT_ROOM_DTTM)%>%
#   filter(!is.na(PATIENT_OUT_ROOM_DTTM)) 
# 
# 
# baseline_and_new_volume_data_raw <- bind_rows(or_cases_baseline_demand,
#                                               baseline_and_new_volume_data_raw)
# 
# collision_data_baseline_and_new_volume <- capacity_and_utlization_data(baseline_and_new_volume_data_raw,Scenario="Volume Projections",denominator=n_distinct(or_cases_baseline_demand$SURGERY_DATE))
# 
# 
# volume_base<-collision_data_baseline[["Volume"]]
# volume_new<-collision_data_baseline_and_new_volume[["Volume"]]
# collision_data_baseline <- collision_data_baseline[["demand_capacity"]] %>%
#   mutate(Scenario =  "Baseline")
# collision_data_baseline_and_new_volume <- collision_data_baseline_and_new_volume[["demand_capacity"]] %>%
#   mutate(Scenario =  "Volume Projections")
# 
# demand_capacity_aggregated <- bind_rows(collision_data_baseline,
#                                         collision_data_baseline_and_new_volume) %>%
#   filter(!is.na(Location))
# 
# 
# # ==============================================
# # Collision Rate - How often are we exceeding capacity
# # Capacity comes from cascade data
# # ==============================================
# collision_rate_baseline <- collision_rate(or_cases_baseline)
# collision_rate_baseline_and_new_volume <- collision_rate(baseline_and_new_volume_data_raw)
# 
# collision_rate_baseline <- collision_rate_baseline %>%
#   mutate(Scenario =  "Baseline")
# collision_rate_baseline_and_new_volume <- collision_rate_baseline_and_new_volume %>%
#   mutate(Scenario =  "Volume Projections")
# 
# collision_rate_aggregated <- bind_rows(collision_rate_baseline,
#                                        collision_rate_baseline_and_new_volume) %>%
#   filter(!is.na(Location))
# 
# 
# 
# # =================================================================
# # Plots - Collision and Demand
# # =================================================================
# 
# 
# 
# 
# collision_rate_plot <-ggplot(collision_rate_aggregated_plot, aes(x = time_interval, y = CollisionRate, fill = Scenario)) +
#   geom_col(position = "dodge", width = 0.7) +
#   
#   # 'axes = "all"' guarantees labels appear on all wrapped facet charts
#   facet_wrap(~Location, ncol = 1, scales = "free_x", axes = "all") + 
#   
#   scale_fill_manual(values = c("Baseline" = mshs_violet, "Volume Projections" = mshs_cyan)) +
#   labs(
#     title = "Collision Rate - How often does demand exceed capacity?",
#     x     = "Interval",
#     y     = "Collision Rate",
#     fill  = "Scenario"
#   ) +
#   theme_minimal() + # Sets structural panel default rules
#   mshs_theme       # Applies custom MSHS visual rules
# 
# print(collision_rate_plot)


new_volume_cases_raw_subset <- new_volume_cases_raw %>%
  select(OR_CASE_ID,
         SURGERY_DATE,
         ProcedureInterval,
         PATIENT_IN_ROOM_DTTM, 
         PATIENT_OUT_ROOM_DTTM,
         LOCATION_NAME)



# =================================================================
# Baseline Demand
# =================================================================
baseline_demand <- demand_baseline(or_cases_baseline)

# =================================================================
# Baseline and New Volume Demand 
# =================================================================
new_volume_demand <- demand_new_volume(new_volume_cases_raw_subset, scenario = "Volume Projections")

# =================================================================
# Aggregated demamd
# =================================================================
demand_capacity_aggregated <- bind_rows(baseline_demand,
                                        new_volume_demand)
demand_capacity_aggregated <- demand_capacity_aggregated %>%
  filter(YearReporting!=2026)



# ================= OR Capacity vs Demand (stacked) =======================
BAR_W <- 0.7

for (site in unique(demand_capacity_aggregated$Location)) {
  
  site_data <- demand_capacity_aggregated %>%
    filter(Location == site, hour_surgery %in% 7:19) %>%
    group_by(hour_surgery, Scenario) %>%
    summarise(RoomsInUse = sum(RoomsInUse, na.rm = TRUE),
              Capacity   = max(Capacity,   na.rm = TRUE),
              .groups    = "drop") %>%
    mutate(Scenario = factor(Scenario,
                             levels = c("Volume Projections", "Baseline")))
  
  cap_data <- site_data %>%
    group_by(hour_surgery) %>%
    summarise(Capacity = max(Capacity, na.rm = TRUE), .groups = "drop") %>%
    mutate(xmin = hour_surgery - 0.5, xmax = hour_surgery + 0.5)
  
  p <- ggplot() +
    geom_col(data = site_data,
             aes(x = hour_surgery, y = RoomsInUse, fill = Scenario),
             position = position_stack(reverse = FALSE), width = BAR_W) +
    
    geom_text(data = site_data,
              aes(x = hour_surgery, y = RoomsInUse,
                  label = round(RoomsInUse, 1), group = Scenario),
              position = position_stack(vjust = 0.5, reverse = FALSE),
              size = 2.4, fontface = "bold", color = "white") +
    
    geom_segment(data = cap_data,
                 aes(x = xmin, xend = xmax, y = Capacity, yend = Capacity,
                     color = "Staffed Capacity"),
                 linewidth = 0.7) +
    
    scale_fill_manual(name   = "Demand Scenario",
                      values = c("Baseline"           = mshs_violet,
                                 "Volume Projections" = mshs_cyan),
                      breaks = c("Baseline", "Volume Projections")) +
    scale_color_manual(name = "Threshold",
                       values = c("Staffed Capacity" = mshs_magenta)) +
    scale_x_continuous(breaks = seq(7, 19, 1)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(title = paste0("OR Staffed Capacity Vs Demand (Location: ", site, ")"),
         x = "Time of Day (24h Format)", y = "Room Demand") +
    theme_minimal() +
    mshs_theme +
    theme(axis.text.x = element_text(angle = 0))
  
  print(p)
  
  ggsave(paste0(file_location, "OR Modeling/Outputs/DemandPlots/",
                "demand_vs_staffed_v5_", site, ".png"), p,
         width = 12, height = 6, dpi = 150)
}
# =================================================================
# Write outputs (openxlsx, with merged band headers) ----
# Output by Grain gets a two-row header: top row merges "Baseline" /
# "Volume Projections" / "Delta" across their column groups; second row
# holds the detail column names. Other sheets write plainly.
# =================================================================

# strip the band prefix so detail headers read "# Cases", "Used Time", etc.
strip_band <- function(nm) nm %>%
  str_remove("^Baseline ") %>%
  str_remove("^Volume Projections ") %>%
  str_remove("^Delta ?")

grain_cols  <- c("Hospital", "Year")
base_cols   <- names(output_wide)[str_starts(names(output_wide), "Baseline ")]
proj_cols   <- names(output_wide)[str_starts(names(output_wide), "Volume Projections ")]
delta_cols  <- names(output_wide)[str_starts(names(output_wide), "Delta")]
ordered_cols <- c(grain_cols, base_cols, proj_cols, delta_cols)
output_wide  <- output_wide %>% select(all_of(ordered_cols))

wb <- createWorkbook()

# ---- Output by Grain sheet, two-row header ----
addWorksheet(wb, "Output by Grain")

n_grain <- length(grain_cols); n_base <- length(base_cols)
n_proj  <- length(proj_cols);  n_delta <- length(delta_cols)

# Row 1: band labels (merged); Row 2: detail headers; data from row 3
band_row <- c(rep("", n_grain), "Baseline", rep("", n_base - 1),
              "Volume Projections", rep("", n_proj - 1),
              if (n_delta > 0) "Delta", rep("", max(0, n_delta - 1)))
writeData(wb, "Output by Grain", t(matrix(band_row)), startRow = 1, colNames = FALSE)
writeData(wb, "Output by Grain", t(matrix(c(grain_cols, strip_band(c(base_cols, proj_cols, delta_cols))))),
          startRow = 2, colNames = FALSE)
writeData(wb, "Output by Grain", output_wide, startRow = 3, colNames = FALSE)

# merge the band cells across their groups
if (n_base  > 0) mergeCells(wb, "Output by Grain", cols = (n_grain + 1):(n_grain + n_base), rows = 1)
if (n_proj  > 0) mergeCells(wb, "Output by Grain", cols = (n_grain + n_base + 1):(n_grain + n_base + n_proj), rows = 1)
if (n_delta > 0) mergeCells(wb, "Output by Grain", cols = (n_grain + n_base + n_proj + 1):(n_grain + n_base + n_proj + n_delta), rows = 1)

band_style <- createStyle(textDecoration = "bold", halign = "center",
                          fgFill = "#D9E1F2", border = "TopBottomLeftRight")
hdr_style  <- createStyle(textDecoration = "bold", halign = "center", wrapText = TRUE)
addStyle(wb, "Output by Grain", band_style, rows = 1, cols = 1:length(ordered_cols), gridExpand = TRUE)
addStyle(wb, "Output by Grain", hdr_style,  rows = 2, cols = 1:length(ordered_cols), gridExpand = TRUE)
freezePane(wb, "Output by Grain", firstActiveRow = 3)
setColWidths(wb, "Output by Grain", cols = 1:length(ordered_cols), widths = "auto")


saveWorkbook(wb, paste0(file_location,"OR Modeling/Outputs/","or_capacity_scenario_results ",Sys.time(),".xlsx"),
             overwrite = TRUE)


###############
# Validation  #
###############
nrow(baseline_collision)   # baseline case count
nrow(all_cases)            # should be baseline + dummies
all_cases %>% count(is_new)


# Per-hour, both scenarios: mean (current) vs total vs n_dates
test <- bind_rows(
  baseline_collision %>% mutate(scenario="Baseline"),
  all_cases %>% mutate(scenario="Volume Projections")
) %>%
  mutate(casc_loc = xwalk_loc(LOCATION_NAME),
         h0 = floor(as.numeric(as_hms(format(PATIENT_IN_ROOM_DTTM,"%H:%M:%S")))/3600),
         h1 = floor(as.numeric(as_hms(format(OCC_END,"%H:%M:%S")))/3600)) %>%
  filter(casc_loc=="MSH") %>%
  rowwise() %>% mutate(hr=list(seq(h0,h1))) %>% unnest(hr) %>% ungroup() %>%
  count(scenario, SURGERY_DATE, hr, name="concurrent") %>%
  group_by(scenario, hr) %>%
  summarise(mean_c = mean(concurrent),
            total_c = sum(concurrent),
            n_dates = n_distinct(SURGERY_DATE), .groups="drop") %>%
  filter(hr == 11) %>% print()



or_cases_baseline %>%
  mutate(casc_loc = xwalk_loc(LOCATION_NAME),
         OCC_END = PATIENT_OUT_ROOM_DTTM + minutes(as.integer(coalesce(TURNOVER_FROM_PRIOR_CASE, 0))),
         in_s  = as.numeric(as_hms(format(PATIENT_IN_ROOM_DTTM,"%H:%M:%S"))),
         out_s = as.numeric(as_hms(format(OCC_END,"%H:%M:%S")))) %>%
  filter(casc_loc == "MSH") %>%
  group_by(SURGERY_DATE) %>%
  summarise(
    touching_hr11          = sum(in_s < 12*3600 & out_s > 11*3600),
    occupied_at_1100       = sum(in_s <= 11*3600 & out_s > 11*3600),
    distinct_rooms_at_1100 = n_distinct(ROOM_ID[in_s <= 11*3600 & out_s > 11*3600]),
    .groups = "drop"
  ) %>%
  summarise(across(c(touching_hr11, occupied_at_1100, distinct_rooms_at_1100),
                   ~ mean(.x, na.rm = TRUE)))