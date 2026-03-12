library(dplyr)

na_cleanup <- function(df) {
  
  baseline_cols <- names(df)[grepl("_BASELINE$", names(df))]
  scenario_cols <- names(df)[grepl("_SCENARIO$", names(df))]
  
  dow_baseline_dash <- c("DOW_MIN_BASELINE", "DOW_MAX_BASELINE")
  dow_scenario_dash <- c("DOW_MIN_SCENARIO", "DOW_MAX_SCENARIO")
  
  exclude_from_zero <- c(
    "AVG_DAILY_DEMAND_BASELINE",
    "AVG_DAILY_DEMAND_SCENARIO"
  )
  
  baseline_blank_cols <- setdiff(baseline_cols,
                                 c(dow_baseline_dash, exclude_from_zero))
  
  scenario_blank_cols <- setdiff(scenario_cols,
                                 c(dow_scenario_dash, exclude_from_zero))
  
  df %>%
    mutate(
      across(
        all_of(dow_baseline_dash),
        ~ ifelse((AVG_BED_CAPACITY_BASELINE == 0 | is.na(AVG_BED_CAPACITY_BASELINE)), "-", as.character(.))
      )
    ) %>%
    mutate(
      across(
        all_of(dow_scenario_dash),
        ~ ifelse((AVG_BED_CAPACITY_SCENARIO == 0 | is.na(AVG_BED_CAPACITY_BASELINE)), "-", as.character(.))
      )
    ) %>%
    mutate(
      across(
        all_of(baseline_blank_cols),
        ~ ifelse((AVG_BED_CAPACITY_BASELINE == 0 | is.na(AVG_BED_CAPACITY_BASELINE)), NA, .)
      )
    ) %>%
    mutate(
      across(
        all_of(scenario_blank_cols),
        ~ ifelse((AVG_BED_CAPACITY_SCENARIO == 0 | is.na(AVG_BED_CAPACITY_SCENARIO)), NA, .)
      )
    )
}