los_reduction_sim <- function(encounter_days_df) {
  # read in los projections
  los_projections <- read_csv(paste0(cap_dir, "Mapping Info/los adjustments/", los_projections_file),
                              show_col_types = FALSE) %>%
    # remove combos with no addressable days or addressable days = 0
    filter(!is.na(PERCENT_ADDRESSABLE),   
           PERCENT_ADDRESSABLE != 0) %>%
    mutate(UNIQUE_ID = paste0(Unit, VERITY_REPORT_SERVICE_MSX))
  
  # calc baseline los
  baseline_los <- encounter_days_df %>%
    filter(LOS_NO_SRC <= 100,
           !is.na(ATTENDING_VERITY_REPORT_SERVICE),
           !is.na(MSDRG_CD_SRC),
           !(MSDRG_CD_SRC %in% c('77','78','79','222','223','224','225','226','227','246','247','248','249',
                               '294','338','339','340','341','342','343','453','454','455','459','460','509'))) %>%
    distinct(ENCOUNTER_NO, UNIT_DESC_MSX, ATTENDING_VERITY_REPORT_SERVICE, LOS_NO_SRC) %>%
    group_by(UNIT_DESC_MSX, ATTENDING_VERITY_REPORT_SERVICE) %>%
    summarise(ALOS = mean(LOS_NO_SRC, na.rm=TRUE),
              .groups = "drop") %>%
    mutate(UNIQUE_ID = paste0(UNIT_DESC_MSX, ATTENDING_VERITY_REPORT_SERVICE))
  
  # join basline los with projections 
  baseline_projections <- baseline_los %>%
    left_join(los_projections, by = "UNIQUE_ID") %>%
    filter(!is.na(TARGET_LOS)) %>%
    mutate(REDUCTION_NEEDED = ALOS - TARGET_LOS,
           PCT_REDUCTION = 1 - (REDUCTION_NEEDED / ALOS))
  
  # join daily encounter data with los projections to bring in ALOS and target LOS
  encounter_daily <- encounter_days_df %>%
    mutate(UNIQUE_ID = paste0(UNIT_DESC_MSX, ATTENDING_VERITY_REPORT_SERVICE)) %>%
    left_join(baseline_projections %>% select(UNIQUE_ID, TARGET_LOS, ALOS,
                                              REDUCTION_NEEDED, PCT_REDUCTION),
              by = "UNIQUE_ID") %>%
    group_by(ENCOUNTER_NO) %>%
    mutate(LOS = sum(BED_CHARGES))
  
  # calculate what days to remove and filter them out from dataset
  encounter_days_adjusted <- encounter_daily %>%
    arrange(ENCOUNTER_NO, SERVICE_DATE) %>%
    group_by(ENCOUNTER_NO, MSDRG_CD_SRC) %>%
    mutate(DAY_NUMBER = row_number(),
           DAYS_TO_KEEP = ceiling(n() * PCT_REDUCTION)) %>%
    # keep days for encounters with only 1 day or where LOS is less than target
    filter(is.na(TARGET_LOS) | LOS < TARGET_LOS | DAY_NUMBER <= DAYS_TO_KEEP | n() == 1) 
  
  return(encounter_days_adjusted)
}