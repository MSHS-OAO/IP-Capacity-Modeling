ip_utilization_model <- function(generator = "", n_simulations = 1) 
{
  
  # loop through each iteration
  outputs_list <- lapply(1:n_simulations, function(i) {
    
    
    print(paste("Running simulation #", i))
    
    # load bed capacity for baseline and scenario
    bed_cap <- unit_capacity( unit_capacity_adjustments = unit_capacity_adjustments,
                                 level = "SERVICE_GROUP")
    
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

    #call daily_demand_generator function
    daily_demand <- daily_demand_generator(
      datasets_processed,
      level = "SERVICE_GROUP"
    )
    
    
    # assign(
    #   "daily_demand",
    #   daily_demand_generator(datasets_processed, level = "SERVICE_GROUP"),
    #   envir = .GlobalEnv
    # )
    
    
    names(daily_demand) <- names(datasets_processed)
    
    # compute daily average utilization and days over 85 and 95 %
    ip_utilization <- lapply(names(daily_demand), function(dataset) {
      
      # load dataset based on name of list element
      df <- daily_demand[[dataset]]
      
      # calculat daily averages of bed demand and join bed capacity data
      df <- df %>%
        group_by(LOC_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
        summarise(DAILY_DEMAND = sum(DAILY_DEMAND, na.rm = TRUE), .groups = "drop") %>%
        collect() %>%
        left_join(bed_cap, by = c("LOC_NAME" = "LOC_NAME", 
                                  "SERVICE_GROUP" = "SERVICE_GROUP",
                                  "SERVICE_DATE" = "SERVICE_DATE")) 
      
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
    })
    # reasign original names to list
    names(ip_utilization) <- names(datasets_processed)
    
    # Scenario Outputs
    ## IP Demand & Utilization Comparison
    # compare utilization of baseline and scenario at daily level
    ip_comparison_daily <- ip_utilization[["baseline"]] %>%
      full_join(ip_utilization[["scenario"]],
                by = c("LOC_NAME"="LOC_NAME",
                       "SERVICE_GROUP"="SERVICE_GROUP",
                       "SERVICE_MONTH"="SERVICE_MONTH",
                       "SERVICE_DATE"="SERVICE_DATE"),
                suffix = c("_BASELINE", "_SCENARIO")) %>%
      filter(LOC_NAME != "MSBI") %>%
      mutate(DOW = wday(SERVICE_DATE),
             WEEKDAY = 
               case_when(DOW %in% c(1, 7) ~ FALSE,
                         TRUE ~ TRUE))
    
    ip_comparison_weekday <- ip_comparison_daily %>%
      filter(WEEKDAY == TRUE)
    
    # aggregate comparison at monthly level
    ip_comparison_monthly <- ip_comparison_daily %>%
      group_by(LOC_NAME, SERVICE_GROUP, SERVICE_MONTH) %>%
      summarise(AVG_BED_CAPACITY_BASELINE = mean(AVG_BED_CAPACITY_BASELINE, na.rm = TRUE),
                AVG_BED_CAPACITY_SCENARIO = mean(AVG_BED_CAPACITY_SCENARIO, na.rm = TRUE),
                AVG_DAILY_DEMAND_BASELINE = mean(DAILY_DEMAND_BASELINE, na.rm = TRUE),
                AVG_DAILY_DEMAND_SCENARIO = mean(DAILY_DEMAND_SCENARIO, na.rm = TRUE),
                AVG_PERCENT_85_BASELINE = mean(UTILIZATION_85_BASELINE, na.rm = TRUE),
                AVG_PERCENT_85_SCENARIO = mean(UTILIZATION_85_SCENARIO, na.rm = TRUE),
                AVG_PERCENT_95_BASELINE = mean(UTILIZATION_95_BASELINE, na.rm = TRUE),
                AVG_PERCENT_95_SCENARIO = mean(UTILIZATION_95_SCENARIO, na.rm = TRUE),
                AVG_UTILIZATION_BASELINE = mean(UTILIZATION_BASELINE, na.rm = TRUE),
                AVG_UTILIZATION_SCENARIO = mean(UTILIZATION_SCENARIO, na.rm = TRUE),
                AVG_SD_BASELINE = sd(UTILIZATION_BASELINE, na.rm = TRUE),
                AVG_SD_SCENARIO = sd(UTILIZATION_SCENARIO, na.rm = TRUE)) %>%
      mutate(across(where(is.numeric), \(x) coalesce(x, 0))) %>%
      mutate(AVG_UTILIZATION_SCENARIO = if_else(AVG_UTILIZATION_SCENARIO == 0, Inf, AVG_UTILIZATION_SCENARIO))
    
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
                AVG_UTILIZATION_SCENARIO = mean(UTILIZATION_SCENARIO, na.rm = TRUE),
                AVG_SD_BASELINE = sd(UTILIZATION_BASELINE, na.rm = TRUE),
                AVG_SD_SCENARIO = sd(UTILIZATION_SCENARIO, na.rm = TRUE)) %>%
      mutate(across(where(is.numeric), \(x) coalesce(x, 0))) %>%
      mutate(AVG_UTILIZATION_SCENARIO = if_else(AVG_UTILIZATION_SCENARIO == 0, Inf, AVG_UTILIZATION_SCENARIO))
    
    # aggregate weekday comparison at total level
    ip_comparison_weekday_total <- ip_comparison_weekday %>%
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
      mutate(AVG_UTILIZATION_SCENARIO = if_else(AVG_UTILIZATION_SCENARIO == 0, Inf, AVG_UTILIZATION_SCENARIO)) %>%
      rename(AVG_WEEKDAY_UTILIZATION_BASELINE = AVG_UTILIZATION_BASELINE,
             AVG_WEEKDAY_UTILIZATION_SCENARIO = AVG_UTILIZATION_SCENARIO) %>%
      select(LOC_NAME, SERVICE_GROUP, AVG_WEEKDAY_UTILIZATION_BASELINE, AVG_WEEKDAY_UTILIZATION_SCENARIO)
    
    # IP Utilization Output
    ip_utilization_output <- ip_comparison_total %>%
      select(LOC_NAME, SERVICE_GROUP, AVG_BED_CAPACITY_BASELINE, 
             AVG_DAILY_DEMAND_BASELINE,AVG_UTILIZATION_BASELINE, 
             AVG_PERCENT_85_BASELINE, AVG_BED_CAPACITY_SCENARIO, 
             AVG_DAILY_DEMAND_SCENARIO, AVG_UTILIZATION_SCENARIO, 
             AVG_PERCENT_85_SCENARIO,AVG_SD_BASELINE,AVG_SD_SCENARIO) %>%
      filter(AVG_DAILY_DEMAND_BASELINE > 1) %>%
      left_join(ip_comparison_weekday_total,
                by = c("LOC_NAME" = "LOC_NAME",
                       "SERVICE_GROUP" = "SERVICE_GROUP")) %>%
      relocate(AVG_WEEKDAY_UTILIZATION_BASELINE, .after = AVG_UTILIZATION_BASELINE) %>%
      relocate(AVG_WEEKDAY_UTILIZATION_SCENARIO, .after = AVG_UTILIZATION_SCENARIO)
    
    
    ip_comparison_dow_service_group <- ip_comparison_dow_service_group_generator(ip_comparison_daily)
    ip_comparison_dow_unit <- ip_comparison_dow_unit_generator(datasets_processed, unit_capacity_adjustments)
    # omitting shut down units (bed capacity = 0 in Tisch capacity projections)
    ip_comparison_dow_unit <- na.omit(ip_comparison_dow_unit)
    return(list(
      ip_comparison_daily = ip_comparison_daily,
      ip_comparison_monthly = ip_comparison_monthly,
      ip_comparison_total = ip_comparison_total,
      ip_utilization_output = ip_utilization_output,
      ip_comparison_dow_service_group = ip_comparison_dow_service_group
      ,ip_comparison_dow_unit=ip_comparison_dow_unit
    ))
    
  })
  
  # --- 1. ip_utilization_output ---
  ip_utilization_output <- outputs_list %>%
    map("ip_utilization_output") %>%
    list_rbind() %>%
    group_by(LOC_NAME, SERVICE_GROUP) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # --- 2. ip_comparison_daily ---
  ip_comparison_daily <- outputs_list %>%
    map("ip_comparison_daily") %>%
    list_rbind() %>%
    group_by(LOC_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE,
             AVG_BED_CAPACITY_BASELINE, AVG_BED_CAPACITY_SCENARIO) %>%
    summarise(across(ends_with("_BASELINE") | ends_with("_SCENARIO"),
                     ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # --- 3. ip_comparison_monthly ---
  ip_comparison_monthly <- outputs_list %>%
    map("ip_comparison_monthly") %>%
    list_rbind() %>%
    group_by(LOC_NAME, SERVICE_GROUP, SERVICE_MONTH,
             AVG_BED_CAPACITY_BASELINE, AVG_BED_CAPACITY_SCENARIO) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # --- 3. ip_comparison_total ---
  ip_comparison_total <- outputs_list %>%
    map("ip_comparison_total") %>%
    list_rbind() %>%
    group_by(LOC_NAME, SERVICE_GROUP) %>%
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # --- 4. ip_comparison_dow_service_group
  ip_comparison_dow_service_group <- outputs_list %>%
    purrr::map("ip_comparison_dow_service_group") %>%
    dplyr::bind_rows() %>%
    group_by(LOC_NAME, SERVICE_GROUP) %>%
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      across(any_of(c("DOW_MIN_BASELINE","DOW_MAX_BASELINE","DOW_MIN_SCENARIO","DOW_MAX_SCENARIO")),
             mode_chr),
      .groups = "drop"
    )

  #--- 5. ip_comparison_dow_unit
  ip_comparison_dow_unit <- outputs_list %>%
    purrr::map("ip_comparison_dow_unit") %>%
    dplyr::bind_rows() %>%
    group_by(LOC_NAME, SERVICE_GROUP,EXTERNAL_NAME) %>%
    summarise(
      across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      across(any_of(c("DOW_MIN_BASELINE","DOW_MAX_BASELINE","DOW_MIN_SCENARIO","DOW_MAX_SCENARIO")),
             mode_chr),
      .groups = "drop"
    )
  
  ip_utilization_output <- ip_utilization_output %>%
    left_join(
      ip_comparison_dow_service_group %>%
        select(any_of(c(
          "LOC_NAME","SERVICE_GROUP",
          "DOW_DIFF_BASELINE","DOW_DIFF_SCENARIO",
          "DOW_MIN_BASELINE","DOW_MAX_BASELINE",
          "DOW_MIN_SCENARIO","DOW_MAX_SCENARIO"
        ))),
      by = c("LOC_NAME", "SERVICE_GROUP")
    )
  
  rm(outputs_list)
  return(list(
    ip_utilization_output = ip_utilization_output ,
    ip_comparison_total = ip_comparison_total,
    ip_comparison_daily = ip_comparison_daily,
    ip_comparison_monthly = ip_comparison_monthly,
    ip_comparison_dow_service_group     = ip_comparison_dow_service_group
    ,ip_comparison_dow_unit     = ip_comparison_dow_unit
    
    
  ))
}