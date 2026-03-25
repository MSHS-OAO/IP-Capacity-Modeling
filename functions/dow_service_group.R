library(dplyr)
library(tidyr)

ip_comparison_dow_service_group_function <- function(ip_comparison_daily) {
  
  dow_order <- c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
  
  ip_comparison_daily %>%
    mutate(DAY_OF_WEEK = toupper(weekdays(SERVICE_DATE))) %>%
    { df <- .
    
    cap_dem <- df %>%
      group_by(LOC_NAME, SERVICE_GROUP) %>%
      summarise(
        AVG_BED_CAPACITY_BASELINE = mean(AVG_BED_CAPACITY_BASELINE, na.rm = TRUE),
        AVG_BED_CAPACITY_SCENARIO = mean(AVG_BED_CAPACITY_SCENARIO, na.rm = TRUE),
        AVG_DAILY_DEMAND_BASELINE = mean(DAILY_DEMAND_BASELINE, na.rm = TRUE),
        AVG_DAILY_DEMAND_SCENARIO = mean(DAILY_DEMAND_SCENARIO, na.rm = TRUE),
        .groups = "drop"
      )
    
    util_long <- df %>%
      select(
        LOC_NAME, SERVICE_GROUP, SERVICE_DATE, DAY_OF_WEEK,
        UTILIZATION_BASELINE, UTILIZATION_SCENARIO
      ) %>%
      pivot_longer(
        cols = starts_with("UTILIZATION_"),
        names_to = "metric",
        values_to = "UTILIZATION"
      ) %>%
      mutate(
        PERIOD = case_when(
          metric == "UTILIZATION_BASELINE" ~ "BASELINE",
          metric == "UTILIZATION_SCENARIO" ~ "SCENARIO",
          TRUE ~ NA_character_
        )
      ) %>%
      select(-metric)
    
    overall <- util_long %>%
      group_by(LOC_NAME, SERVICE_GROUP, PERIOD) %>%
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
    dow_wide <- util_long %>%
      group_by(LOC_NAME, SERVICE_GROUP, PERIOD, DAY_OF_WEEK) %>%
      summarise(.v = round(mean(UTILIZATION, na.rm = TRUE) * 100, 2), .groups = "drop") %>%
      mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, levels = dow_order)) %>%
      pivot_wider(names_from = DAY_OF_WEEK, values_from = .v)
    
    # Combine + pivot
    util_all <- overall %>%
      left_join(dow_wide, by = c("LOC_NAME","SERVICE_GROUP","PERIOD")) %>%
      pivot_wider(
        names_from  = PERIOD,
        values_from = c(
          AVG_BED_UTILIZATION, UTILIZATION_SD,
          WEEKEND_AVG_UTILIZATION, WEEKDAY_AVG_UTILIZATION,
          WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE,
          OVERALL_MIN_UTILIZATION, OVERALL_MAX_UTILIZATION,
          all_of(dow_order)
        ),
        names_glue  = "{.value}_{PERIOD}"
      )
    
    out <- cap_dem %>% left_join(util_all, by = c("LOC_NAME","SERVICE_GROUP"))
    
    pct_cols <- grep(
      paste0("^(AVG_BED_UTILIZATION|UTILIZATION_SD|WEEKEND_AVG_UTILIZATION|WEEKDAY_AVG_UTILIZATION|",
             "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE|OVERALL_MIN_UTILIZATION|OVERALL_MAX_UTILIZATION|",
             paste(dow_order, collapse="|"), ")_(BASELINE|SCENARIO)$"),
      names(out), value = TRUE
    )
    
    out %>% 
      mutate(across(all_of(pct_cols), ~ round(.x / 100, 4))) %>%
      filter(AVG_DAILY_DEMAND_BASELINE >= 1)
    }
}

add_dow_diff_summary_columns <- function(df) {
  
  dow_order <- c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
  dow_baseline_cols <- paste0(dow_order, "_BASELINE")
  dow_scenario_cols <- paste0(dow_order, "_SCENARIO")
  
  df %>%
    rowwise() %>%
    mutate(
      .min_val_baseline = if (all(is.na(c_across(all_of(dow_baseline_cols))))) NA_real_ else min(c_across(all_of(dow_baseline_cols)), na.rm = TRUE),
      .max_val_baseline = if (all(is.na(c_across(all_of(dow_baseline_cols))))) NA_real_ else max(c_across(all_of(dow_baseline_cols)), na.rm = TRUE),
      .min_val_scenario = if (all(is.na(c_across(all_of(dow_scenario_cols))))) NA_real_ else min(c_across(all_of(dow_scenario_cols)), na.rm = TRUE),
      .max_val_scenario = if (all(is.na(c_across(all_of(dow_scenario_cols))))) NA_real_ else max(c_across(all_of(dow_scenario_cols)), na.rm = TRUE),
      
      DOW_MIN_BASELINE = if (is.na(.min_val_baseline)) NA_character_ else paste0(
        paste(dow_order[which(c_across(all_of(dow_baseline_cols)) == .min_val_baseline)], collapse = ", "),
        ": ", sprintf("%.2f", .min_val_baseline * 100), "%"
      ),
      DOW_MAX_BASELINE = if (is.na(.max_val_baseline)) NA_character_ else paste0(
        paste(dow_order[which(c_across(all_of(dow_baseline_cols)) == .max_val_baseline)], collapse = ", "),
        ": ", sprintf("%.2f", .max_val_baseline * 100), "%"
      ),
      DOW_DIFF_BASELINE = if (is.na(.min_val_baseline) | is.na(.max_val_baseline)) NA_real_ else round(.max_val_baseline - .min_val_baseline, 4),
      
      DOW_MIN_SCENARIO = if (is.na(.min_val_scenario)) NA_character_ else paste0(
        paste(dow_order[which(c_across(all_of(dow_scenario_cols)) == .min_val_scenario)], collapse = ", "),
        ": ", sprintf("%.2f", .min_val_scenario * 100), "%"
      ),
      DOW_MAX_SCENARIO = if (is.na(.max_val_scenario)) NA_character_ else paste0(
        paste(dow_order[which(c_across(all_of(dow_scenario_cols)) == .max_val_scenario)], collapse = ", "),
        ": ", sprintf("%.2f", .max_val_scenario * 100), "%"
      ),
      DOW_DIFF_SCENARIO = if (is.na(.min_val_scenario) | is.na(.max_val_scenario)) NA_real_ else round(.max_val_scenario - .min_val_scenario, 4)
    ) %>%
    ungroup() %>%
    select(-.min_val_baseline, -.max_val_baseline, -.min_val_scenario, -.max_val_scenario)
}