los_reduction_sim <- function(encounter_days_df) {
  # read in los projections
  los_projections <- read_csv(paste0(cap_dir, "Mapping Info/los adjustments/", los_projections_file),
                              show_col_types = FALSE) %>%
    filter(!is.na(TARGET_LOS)) %>%
    mutate(UNIQUE_ID = paste0(Hospital, VERITY_REPORT_SERVICE_MSX))
  
  # calc baseline los
  baseline_los <- encounter_days_df %>%
    filter(LOS_NO_SRC <= 100,
           !is.na(ATTENDING_VERITY_REPORT_SERVICE),
           !is.na(MSDRG_CD_SRC)) %>%
    distinct(ENCOUNTER_NO, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, LOS_NO_SRC) %>% ###
    group_by(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE) %>%
    summarise(ALOS = mean(LOS_NO_SRC, na.rm=TRUE),
              .groups = "drop") %>%
    mutate(UNIQUE_ID = paste0(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE)) ###FACILITY_MSX
  
  # join basline los with projections 
  baseline_projections <- baseline_los %>%
    left_join(los_projections, by = "UNIQUE_ID") %>%
    filter(!is.na(TARGET_LOS)) %>%
    mutate(REDUCTION_NEEDED = ALOS - TARGET_LOS,
           PCT_REDUCTION = 1 - (REDUCTION_NEEDED / ALOS))
  
  # join daily encounter data with los projections to bring in ALOS and target LOS
  encounter_daily <- encounter_days_df %>%
    mutate(UNIQUE_ID = paste0(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE)) %>%
    left_join(baseline_projections %>% select(UNIQUE_ID, TARGET_LOS, ALOS,
                                              REDUCTION_NEEDED, PCT_REDUCTION),
              by = "UNIQUE_ID") %>%
    group_by(ENCOUNTER_NO) %>%
    mutate(LOS = sum(BED_CHARGES))
  
  encounter_days_adjusted <- encounter_daily %>%
    arrange(ENCOUNTER_NO, SERVICE_DATE) %>%
    group_by(ENCOUNTER_NO, MSDRG_CD_SRC) %>%
    mutate(
      DAY_NUMBER = row_number(),
      DAYS_TO_KEEP = ceiling(n() * PCT_REDUCTION)
    ) %>%
    filter(
      is.na(TARGET_LOS) |
        LOS < TARGET_LOS |
        DAY_NUMBER <= DAYS_TO_KEEP |
        n() == 1
    ) %>%
    group_by(ENCOUNTER_NO) %>%
    mutate(
      NEW_ADMIT_DT_SRC = min (SERVICE_DATE),
      NEW_DSCH_DT_SRC = max(SERVICE_DATE, na.rm = TRUE) + days(1)
    ) %>%
    ungroup()
  
  return(encounter_days_adjusted)
}