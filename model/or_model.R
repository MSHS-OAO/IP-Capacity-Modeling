source('functions/or_constants_functions.R')


# Get volume projection data ----
file_name <- "new_daily_demand_encounter.xlsx"


data_baseline_ip <- read_xlsx(paste0(file_location,paste0("archive/",file_name)),sheet = 'baseline')
data_volume_projections_ip <- read_xlsx(paste0(file_location,paste0("archive/",file_name)),sheet = 'scenario')


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
    SURGERY_DATE          = NEW_ADMIT_DT_SRC + surgery_offset,
    PATIENT_IN_ROOM_DTTM  = as.POSIXct(paste(SURGERY_DATE, time_of_day), tz = "America/New_York"),
    PATIENT_OUT_ROOM_DTTM = PATIENT_IN_ROOM_DTTM + minutes(as.integer(MINUTES_IN_ROOM_TO_OUT_ROOM)),
    OR_CASE_ID  = paste0("NEW_", ENCOUNTER_NO),
    PATIENT_MRN = MSMRN
  )





# =================================================================
# Projected metrics ----
# Dummies have NO room (new volume can land anywhere at a location), so
# they DON'T enter the room/gap engine. They contribute to the RAW
# columns only, via extra_raw_cases; penalized columns come from the
# baseline rooms. Dummy capacity impact is shown in the collision view.
# Dummies need CLUSTER_NAME for the Hospital grain; derive from LOCATION.
# =================================================================
new_volume_cases_raw <- new_volume_cases %>%
  mutate(
    PATIENT_OUT_AND_SETUP_CLEANUP_END = PATIENT_OUT_ROOM_DTTM + minutes(as.integer(TURNOVER_FROM_PRIOR_CASE)),
    overlap_primetime_procedure = 1,                            # non-NA so the base filter keeps them
    overlap_primetime_setup     = 1
  ) %>%
  mutate(casc_loc = xwalk_loc(LOCATION_NAME))




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
    `Delta Utilization`  = `Volume Projections Prime Time Utilization` - `Baseline Prime Time Utilization`,
    `Delta Recoverable`  = `Volume Projections Prime Time Recoverable Time` - `Baseline Prime Time Recoverable Time`
  )



# Lightweight combined case set for the COLLISION / demand views only.
# Collision needs LOCATION_NAME + timestamps (no ROOM_ID). Occupancy now
# INCLUDES setup/cleanup (PATIENT_OUT + turnover), and bands are clipped
# to each location-day's prime-time window.
collision_cols <- c("OR_CASE_ID", "PATIENT_IN_ROOM_DTTM", "PATIENT_OUT_ROOM_DTTM",
                    "OCC_END", "PRIME_TIME_START", "PRIME_TIME_END",
                    "SURGERY_DATE", "LOCATION_NAME")

baseline_collision <- or_cases_baseline %>%
  mutate(SURGERY_DATE = as.Date(SURGERY_DATE),
         OCC_END = PATIENT_OUT_ROOM_DTTM +
           minutes(as.integer(coalesce(TURNOVER_FROM_PRIOR_CASE, 0)))) %>%
  select(all_of(collision_cols)) %>% mutate(is_new = FALSE)

new_volume_cases_collision <- new_volume_cases %>%
  prime_time_location() %>%
  mutate(OCC_END = PATIENT_OUT_ROOM_DTTM +
           minutes(as.integer(coalesce(TURNOVER_FROM_PRIOR_CASE, 0)))) %>%
  select(all_of(collision_cols)) %>% mutate(is_new = TRUE)

all_cases <- bind_rows(baseline_collision, new_volume_cases_collision)

# =================================================================
# Cascade collision rate (capacity model) ----
# capacity(band) = rooms_open * band_minutes; demand = case room-minutes in band
# =================================================================

collision_all <- bind_rows(
  collision_by_band(baseline_collision, "Baseline"),
  collision_by_band(all_cases,         "Volume Projections")
)
# scenario-level summary (overall)
collision_rate <- collision_all %>%
  group_by(scenario,casc_loc,SURGERY_DATE,band) %>%
  summarise(n_bands = n(), colliding = sum(collision),
            collision_rate = colliding / n_bands,
            total_excess_hrs = sum(excess_min) / 60, .groups = "drop")
print(collision_rate)

# by Hospital x Day: a day "collides" if any band that day exceeds capacity;
# rate = fraction of that day's bands over capacity. Add Month/Day for joins.
collision_by_hosp_day <- collision_all %>%
  mutate(Hospital = casc_loc, Month = month(SURGERY_DATE), Day = day(SURGERY_DATE)) %>%
  group_by(scenario, Hospital, SURGERY_DATE, Month, Day) %>%
  summarise(n_bands          = n(),
            colliding_bands  = sum(collision),
            band_collision_rate = colliding_bands / n_bands,   # share of bands over capacity
            any_collision    = as.integer(colliding_bands > 0), # day flagged at all
            total_excess_hrs = sum(excess_min) / 60,
            peak_excess_min  = max(excess_min),
            .groups = "drop") %>%
  arrange(scenario, Hospital, SURGERY_DATE)

# wide view: baseline vs projected collision side by side per Hospital-day.
# Key on full SURGERY_DATE (Month/Day alone collide across years -> list-cols).
collision_by_hosp_day_wide <- collision_by_hosp_day %>%
  select(scenario, Hospital, SURGERY_DATE, band_collision_rate, total_excess_hrs, any_collision) %>%
  pivot_wider(id_cols = c(Hospital, SURGERY_DATE),
              names_from = scenario,
              values_from = c(band_collision_rate, total_excess_hrs, any_collision),
              names_glue = "{scenario} {.value}",
              names_vary = "slowest") %>%
  arrange(Hospital, SURGERY_DATE)


# =================================================================
# Graph: room demand (ORs/hour) vs staffed capacity ----
# =================================================================

demand_plot_df <- bind_rows(
  expand_hours(baseline_collision, "Baseline"),
  expand_hours(all_cases,          "Volume Projections")
)
capacity_hourly <- cascade_factor %>%
  rowwise() %>% mutate(hr = list(seq(floor(b_start/3600), floor(b_end/3600)))) %>%
  unnest(hr) %>% ungroup() %>%
  group_by(casc_loc, hr) %>% summarise(capacity_ors = max(n_ors), .groups = "drop")


# One graph per site, saved separately ----
sites <- sort(unique(demand_plot_df$casc_loc))
for (site in sites) {
  dplot <- demand_plot_df %>% filter(casc_loc == site)
  cplot <- capacity_hourly %>% filter(casc_loc == site)
  
  p_site <- ggplot(dplot, aes(hr, ors_needed, color = scenario)) +
    geom_step(data = cplot, aes(hr, capacity_ors),
              inherit.aes = FALSE, linetype = "dashed",
              color = mshs_gray, linewidth = 0.8) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = scenario_cols) +
    scale_x_continuous(breaks = seq(0, 24, 2)) +
    labs(title = paste0(site, ": OR demand (rooms needed / hour) vs staffed capacity"),
         subtitle = "Dashed grey = staffed rooms (cascade)  |  solid = mean case demand (incl. setup)",
         x = "Hour of day", y = "ORs in use", color = NULL) +
    theme_gray(base_size = 13) +
    theme(legend.position = "top",
          plot.title = element_text(face = "bold", color = mshs_violet),
          panel.grid.minor = element_blank())
  
  ggsave(paste0(file_location,"OR Modeling/Outputs/DemandPlots/", "or_demand_", site, ".png"),
         p_site, width = 10, height = 6, dpi = 150)
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

grain_cols  <- c("Hospital", "Month", "Day")
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

# ---- other sheets (plain) ----
for (nm in c("Collision Rate", "Collision by Hosp-Day", "Collision HospDay Wide",
             "Collision Detail")) {
  addWorksheet(wb, nm)
}
writeData(wb, "Collision Rate",         collision_rate)
writeData(wb, "Collision by Hosp-Day",  collision_by_hosp_day)
writeData(wb, "Collision HospDay Wide", collision_by_hosp_day_wide)
writeData(wb, "Collision Detail",       collision_all)

saveWorkbook(wb, paste0(file_location,"OR Modeling/Outputs/","or_capacity_scenario_results.xlsx"),
             overwrite = TRUE)


###############
# Validation  #
###############
nrow(baseline_collision)   # baseline case count
nrow(all_cases)            # should be baseline + dummies
all_cases %>% count(is_new)


# Per-hour, both scenarios: mean (current) vs total vs n_dates
bind_rows(
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