ip_utilization_model <- function(generator = "", n_simulations = 1, level = "service_group") {
  
  # load bed capacity for baseline and scenario
  bed_cap <- unit_capacity(unit_capacity_adjustments, level = level)
  
  # loop through each iteration
  outputs_list <- lapply(1:n_simulations, function(i) {
    
    
    print(paste("Running simulation #", i))
    
    # read in processed data from data refresh script
    if (generator == "location_swap"){
      generator <- location_swap
      datasets_processed <- list(
        "baseline" = baseline,
        "scenario" = generator(hospitals, services, percentage_to_hosp1, percentage_to_hosp2))
    } else {
      datasets_processed <- list(
        "baseline" = baseline,
        "scenario" = baseline)
    }
    
    # create daily demand list at desired level all scenario modifiers are applied here
    daily_demand_list <- lapply(names(datasets_processed), daily_demand, df = datasets_processed, level = level)
    names(daily_demand_list) <- names(datasets_processed)
    
    # compute utilization for this simulation iteration
    ip_utilization_list <- ip_utilization(daily_demand_list = daily_demand_list,
                                          bed_cap = bed_cap)
    })
    
  # --- ip_utilization_output ---
  ip_utilization_output <- outputs_list %>%
    map("ip_utilization_output") %>%
    list_rbind() %>%
    group_by(LOC_NAME, SERVICE_GROUP) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # --- ip_comparison_daily ---
  ip_comparison_daily <- outputs_list %>%
    map("ip_comparison_daily") %>%
    list_rbind() %>%
    group_by(LOC_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE,
             AVG_BED_CAPACITY_BASELINE, AVG_BED_CAPACITY_SCENARIO) %>%
    summarise(across(ends_with("_BASELINE") | ends_with("_SCENARIO"),
                     ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # --- ip_comparison_total ---
  ip_comparison_total <- outputs_list %>%
    map("ip_comparison_total") %>%
    list_rbind() %>%
    group_by(LOC_NAME, SERVICE_GROUP) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  rm(outputs_list)
  return(list(
    ip_utilization_output = ip_utilization_output ,
    ip_comparison_total = ip_comparison_total,
    ip_comparison_daily = ip_comparison_daily))
    
}