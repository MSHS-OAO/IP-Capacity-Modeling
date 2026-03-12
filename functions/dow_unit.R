ip_comparison_dow_unit_generator <- function(datasets_processed, unit_capacity_adjustments = NULL) {
  
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(readr)
  library(DBI)
  library(dbplyr)
  
  stopifnot(is.list(datasets_processed))
  stopifnot(all(c("baseline", "scenario") %in% names(datasets_processed)))
  
  dow_order <- c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
  
  bed_cap_unit_wide <- unit_capacity(
    unit_capacity_adjustments = unit_capacity_adjustments,
    level = "EXTERNAL_NAME"
  )

  daily_demand_unit <- bind_rows(daily_demand_generator(datasets_processed, level = "EXTERNAL_NAME"))
  
  # assign(
  #   "daily_demand_unit",
  #   bind_rows(daily_demand_generator(datasets_processed, level = "EXTERNAL_NAME")),
  #   envir = .GlobalEnv
  # )
  
  
  # # unit daily demand from baseline/scenario
  # daily_demand_unit <- lapply(names(datasets_processed), function(dataset) {
  #   
  #   df <- datasets_processed[[dataset]]
  #   
  #   df <- df %>%
  #     filter(!is.na(EXTERNAL_NAME)) %>%
  #     group_by(
  #       ENCOUNTER_NO, MSDRG_CD_SRC, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE,
  #       DSCH_UNIT_DESC_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH,
  #       SERVICE_DATE, LOS_NO_SRC
  #     ) %>%
  #     summarise(BED_CHARGES = sum(QUANTITY, na.rm = TRUE), .groups = "drop") %>%
  #     mutate(BED_CHARGES = if_else(BED_CHARGES > 1, 1, BED_CHARGES))
  #   
  #   if (dataset == "scenario" && exists("vol_projections_file") && !is.null(vol_projections_file)) {
  #     df <- volume_projections(df, vol_projections_file)
  #   }
  #   if (dataset == "scenario" && exists("los_projections_file") && !is.null(los_projections_file)) {
  #     df <- los_reduction_sim(df)
  #   }
  #   
  #   df %>%
  #     group_by(LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_MONTH, SERVICE_DATE) %>%
  #     summarise(DAILY_DEMAND = sum(BED_CHARGES, na.rm = TRUE), .groups = "drop") %>%
  #     mutate(PERIOD = toupper(dataset))
  # })
  
  
  # join capacity + compute utilization
  util_df <- daily_demand_unit %>%
    left_join(bed_cap_unit_wide, by = c("LOC_NAME","SERVICE_GROUP","EXTERNAL_NAME","SERVICE_DATE")) %>%
    mutate(
      BED_CAPACITY = if_else(PERIOD == "BASELINE", BASELINE, SCENARIO),
      BED_CAPACITY = if_else(is.na(BED_CAPACITY) | BED_CAPACITY <= 0, NA_real_, BED_CAPACITY),
      UTILIZATION  = DAILY_DEMAND / BED_CAPACITY,
      DAY_OF_WEEK  = toupper(weekdays(SERVICE_DATE))
    ) %>%
    select(
      LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_DATE, SERVICE_MONTH, DAY_OF_WEEK, PERIOD,
      DAILY_DEMAND, BED_CAPACITY, UTILIZATION
    )
  
  # Capacity/Demand summaries ----
  cap_dem_wide <- util_df %>%
    group_by(LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, PERIOD) %>%
    summarise(
      AVG_BED_CAPACITY = if (all(is.na(BED_CAPACITY))) NA_real_ else mean(BED_CAPACITY, na.rm = TRUE),
      AVG_DAILY_DEMAND = mean(DAILY_DEMAND, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = PERIOD,
      values_from = c(AVG_BED_CAPACITY, AVG_DAILY_DEMAND),
      names_glue = "{.value}_{PERIOD}",
      names_repair = "check_unique"
    )
  
  # Overall utilization stats 
  overall <- util_df %>%
    group_by(LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, PERIOD) %>%
    summarise(
      AVG_BED_UTILIZATION     = round(mean(UTILIZATION, na.rm = TRUE) * 100, 2),
      WEEKDAY_AVG_UTILIZATION = round(mean(UTILIZATION[!DAY_OF_WEEK %in% c("SATURDAY","SUNDAY")], na.rm = TRUE) * 100, 2),
      WEEKEND_AVG_UTILIZATION = round(mean(UTILIZATION[ DAY_OF_WEEK %in% c("SATURDAY","SUNDAY")], na.rm = TRUE) * 100, 2),
      OVERALL_MIN_UTILIZATION = if (all(is.na(UTILIZATION))) NA_real_ else round(min(UTILIZATION, na.rm = TRUE) * 100, 2),
      OVERALL_MAX_UTILIZATION = if (all(is.na(UTILIZATION))) NA_real_ else round(max(UTILIZATION, na.rm = TRUE) * 100, 2),
      UTILIZATION_SD          = if (sum(!is.na(UTILIZATION)) < 2) NA_real_ else round(sd(UTILIZATION, na.rm = TRUE) * 100, 2),
      .groups = "drop"
    ) %>%
    mutate(WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE = round(WEEKEND_AVG_UTILIZATION - WEEKDAY_AVG_UTILIZATION, 2))
  
  # DOW averages wide
  dow_wide <- util_df %>%
    mutate(DAY_OF_WEEK = toupper(weekdays(SERVICE_DATE))) %>%
    filter(DAY_OF_WEEK %in% dow_order) %>%
    group_by(LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, PERIOD, DAY_OF_WEEK) %>%
    summarise(.v = round(mean(UTILIZATION, na.rm = TRUE) * 100, 2), .groups = "drop") %>%
    mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, levels = dow_order)) %>%
    pivot_wider(
      names_from = DAY_OF_WEEK,
      values_from = .v,
      names_repair = "check_unique"
    )
  
  # Combine + DOW_MIN/MAX/DIFF + pivot
  util_all <- overall %>%
    left_join(dow_wide, by = c("LOC_NAME","SERVICE_GROUP","EXTERNAL_NAME","PERIOD")) %>%
    rowwise() %>%
    mutate(
      .all_na  = all(is.na(c_across(all_of(dow_order)))),
      .min_val = if (.all_na) NA_real_ else min(c_across(all_of(dow_order)), na.rm = TRUE),
      .max_val = if (.all_na) NA_real_ else max(c_across(all_of(dow_order)), na.rm = TRUE),
      
      DOW_MIN  = if (.all_na) NA_character_ else paste0(
        paste(dow_order[which(c_across(all_of(dow_order)) == .min_val)], collapse = ", "),
        ": ", sprintf("%.2f", .min_val), "%"
      ),
      DOW_MAX  = if (.all_na) NA_character_ else paste0(
        paste(dow_order[which(c_across(all_of(dow_order)) == .max_val)], collapse = ", "),
        ": ", sprintf("%.2f", .max_val), "%"
      ),
      DOW_DIFF = if (.all_na) NA_real_ else round(.max_val - .min_val, 2)
    ) %>%
    ungroup() %>%
    select(-.all_na, -.min_val, -.max_val) %>%
    pivot_wider(
      names_from  = PERIOD,
      values_from = c(
        AVG_BED_UTILIZATION, UTILIZATION_SD,
        WEEKEND_AVG_UTILIZATION, WEEKDAY_AVG_UTILIZATION,
        WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE,
        OVERALL_MIN_UTILIZATION, OVERALL_MAX_UTILIZATION,
        DOW_MIN, DOW_MAX, DOW_DIFF,
        all_of(dow_order)
      ),
      names_glue  = "{.value}_{PERIOD}",
      names_repair = "check_unique"
    )
  
  out <- cap_dem_wide %>%
    left_join(util_all, by = c("LOC_NAME","SERVICE_GROUP","EXTERNAL_NAME"))
  
  # convert percent-scale numeric columns to proportions (0-1)
  pct_cols <- grep(
    paste0(
      "^(AVG_BED_UTILIZATION|UTILIZATION_SD|WEEKEND_AVG_UTILIZATION|WEEKDAY_AVG_UTILIZATION|",
      "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE|OVERALL_MIN_UTILIZATION|OVERALL_MAX_UTILIZATION|DOW_DIFF|",
      paste(dow_order, collapse="|"),
      ")_(BASELINE|SCENARIO)$"
    ),
    names(out), value = TRUE
  )
  
  out <- out %>%
    mutate(across(all_of(pct_cols), ~ round(.x / 100, 4))) %>%
    filter(!is.na(AVG_BED_CAPACITY_BASELINE))
  
  out
}
