ip_utilization <- function(daily_demand_list, bed_cap) {
  
  ip_utilization_list <- list()
  
  for (dataset in names(daily_demand_list)) {
    # load dataset based on name of list element
    df <- daily_demand_list[[dataset]]
    
    # calculat daily averages of bed demand and join bed capacity data
    df <- df %>%
      group_by(LOC_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
      summarise(DAILY_DEMAND = sum(DAILY_DEMAND, na.rm = TRUE), .groups = "drop") %>%
      collect() %>%
      left_join(bed_cap, by = c("LOC_NAME",
                                "SERVICE_GROUP",
                                "SERVICE_DATE")) 
    
    # calculate utilization based on baseline/scenario bed capacity
    if (dataset == "baseline") {
      df <- df %>%
        mutate(UTILIZATION = DAILY_DEMAND/BASELINE) %>%
        select(-SCENARIO) %>%
        rename(AVG_BED_CAPACITY = BASELINE)
    } else {
      df <- df %>%
        mutate(UTILIZATION = DAILY_DEMAND/SCENARIO)%>%
        select(-BASELINE) %>%
        rename(AVG_BED_CAPACITY = SCENARIO)
    }
    
    # add boolean columns to show if day was above 85 and 95 % utilization
    df <- df %>%
      mutate(
        UTILIZATION_85 = case_when(
          UTILIZATION > .85 ~ TRUE,
          TRUE ~ FALSE),
        UTILIZATION_95 = case_when(
          UTILIZATION > .95 ~ TRUE,
          TRUE ~ FALSE))
    
    ip_utilization_list[[dataset]] <- df
  }
  
  # Scenario Outputs
  ## IP Demand & Utilization Comparison
  # compare utilization of baseline and scenario at daily level
  ip_comparison_daily <- ip_utilization_list[["baseline"]] %>%
    full_join(ip_utilization_list[["scenario"]],
              by = c("LOC_NAME"="LOC_NAME",
                     "SERVICE_GROUP"="SERVICE_GROUP",
                     "SERVICE_MONTH"="SERVICE_MONTH",
                     "SERVICE_DATE"="SERVICE_DATE"),
              suffix = c("_BASELINE", "_SCENARIO")) %>%
    filter(LOC_NAME != "MSBI")
  
  # aggregate comparison at total level
  ip_comparison_total <- ip_comparison_daily %>%
    group_by(LOC_NAME, SERVICE_GROUP) %>% 
    summarise(AVG_BED_CAPACITY_BASELINE = mean(AVG_BED_CAPACITY_BASELINE, na.rm = TRUE),
              AVG_BED_CAPACITY_SCENARIO = mean(AVG_BED_CAPACITY_SCENARIO, na.rm = TRUE),
              AVG_DAILY_DEMAND_BASELINE = mean(DAILY_DEMAND_BASELINE, na.rm = TRUE),
              AVG_DAILY_DEMAND_SCENARIO = mean(DAILY_DEMAND_SCENARIO, na.rm = TRUE),
              AVG_PERCENT_85_BASELINE = mean(UTILIZATION_85_BASELINE, na.rm = TRUE),
              AVG_PERCENT_85_SCENARIO = mean(UTILIZATION_85_SCENARIO, na.rm = TRUE),
              AVG_PERCENT_95_BASELINE = mean(UTILIZATION_95_BASELINE, na.rm = TRUE),
              AVG_PERCENT_95_SCENARIO = mean(UTILIZATION_95_SCENARIO, na.rm = TRUE),
              AVG_UTILIZATION_BASELINE = mean(UTILIZATION_BASELINE, na.rm = TRUE),
              AVG_UTILIZATION_SCENARIO = mean(UTILIZATION_SCENARIO, na.rm = TRUE)) %>%
    mutate(across(where(is.numeric), \(x) coalesce(x, 0))) %>%
    mutate(AVG_UTILIZATION_SCENARIO = if_else(AVG_UTILIZATION_SCENARIO == 0, Inf, AVG_UTILIZATION_SCENARIO))
  
  # IP Utilization Output
  ip_utilization_output <- ip_comparison_total %>%
    select(LOC_NAME, 
           SERVICE_GROUP, 
           AVG_BED_CAPACITY_BASELINE, 
           AVG_DAILY_DEMAND_BASELINE,
           AVG_UTILIZATION_BASELINE, 
           AVG_PERCENT_85_BASELINE, 
           AVG_BED_CAPACITY_SCENARIO, 
           AVG_DAILY_DEMAND_SCENARIO, 
           AVG_UTILIZATION_SCENARIO, 
           AVG_PERCENT_85_SCENARIO) %>%
    filter(AVG_DAILY_DEMAND_BASELINE > 1)
  
  return(list(
    ip_comparison_daily = ip_comparison_daily,
    ip_comparison_total = ip_comparison_total,
    ip_utilization_output = ip_utilization_output
  ))
}