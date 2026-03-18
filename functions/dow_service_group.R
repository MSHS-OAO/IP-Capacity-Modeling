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
    
    dow_wide <- util_long %>%
      group_by(LOC_NAME, SERVICE_GROUP, PERIOD, DAY_OF_WEEK) %>%
      summarise(.v = round(mean(UTILIZATION, na.rm = TRUE) * 100, 2), .groups = "drop") %>%
      mutate(DAY_OF_WEEK = factor(DAY_OF_WEEK, levels = dow_order)) %>%
      pivot_wider(names_from = DAY_OF_WEEK, values_from = .v)
    
    util_all <- overall %>%
      left_join(dow_wide, by = c("LOC_NAME","SERVICE_GROUP","PERIOD")) %>%
      rowwise() %>%
      mutate(
        .min_val = min(c_across(all_of(dow_order)), na.rm = TRUE),
        .max_val = max(c_across(all_of(dow_order)), na.rm = TRUE),
        DOW_MIN  = paste0(paste(dow_order[which(c_across(all_of(dow_order)) == .min_val)], collapse = ", "),
                          ": ", sprintf("%.2f", .min_val), "%"),
        DOW_MAX  = paste0(paste(dow_order[which(c_across(all_of(dow_order)) == .max_val)], collapse = ", "),
                          ": ", sprintf("%.2f", .max_val), "%"),
        DOW_DIFF = round(.max_val - .min_val, 2)
      ) %>%
      ungroup() %>%
      select(-.min_val, -.max_val) %>%
      pivot_wider(
        names_from  = PERIOD,
        values_from = c(AVG_BED_UTILIZATION, UTILIZATION_SD, WEEKEND_AVG_UTILIZATION, WEEKDAY_AVG_UTILIZATION,
                        WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE, OVERALL_MIN_UTILIZATION, OVERALL_MAX_UTILIZATION,
                        DOW_MIN, DOW_MAX, DOW_DIFF, all_of(dow_order)),
        names_glue  = "{.value}_{PERIOD}"
      )
    
    out <- cap_dem %>% left_join(util_all, by = c("LOC_NAME","SERVICE_GROUP"))
    
    pct_cols <- grep(
      paste0("^(AVG_BED_UTILIZATION|UTILIZATION_SD|WEEKEND_AVG_UTILIZATION|WEEKDAY_AVG_UTILIZATION|",
             "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE|OVERALL_MIN_UTILIZATION|OVERALL_MAX_UTILIZATION|DOW_DIFF|",
             paste(dow_order, collapse="|"), ")_(BASELINE|SCENARIO)$"),
      names(out), value = TRUE
    )
    
    out %>% mutate(across(all_of(pct_cols), ~ round(.x / 100, 4)))
    }
}


mode_chr <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (!length(x)) return(NA_character_)
  tab <- sort(table(x), decreasing = TRUE)
  names(tab)[1]
}


