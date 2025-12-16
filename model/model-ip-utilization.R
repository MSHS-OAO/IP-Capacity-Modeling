ip_utilization_model <- function(generator = "", n_simulations = 1) 
{
  
  # loop through each iteration
  outputs_list <- lapply(1:n_simulations, function(i) {
    
    
    print(paste("Running simulation #", i))
    
    # load bed capacity for baseline and scenario
    bed_cap <- unit_capacity(unit_capacity_adjustments)
    
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

    # adjust scenario demand based on volume and LOS projections
    daily_demand <- lapply(names(datasets_processed), function(dataset) {
      
      # load dataset based on name of list element
      df <- datasets_processed[[dataset]]
      
      # get daily demand by service line and service group
      df <- df %>%
        filter(!is.na(EXTERNAL_NAME)) %>%
        group_by(ENCOUNTER_NO, MSDRG_CD_SRC, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, 
                 UNIT_DESC_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, 
                 SERVICE_DATE, LOS_NO_SRC) %>%
        summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
        mutate(BED_CHARGES = case_when(
          BED_CHARGES > 1 ~ 1,
          TRUE ~ BED_CHARGES)) 
      
      # project changes in LOS
      if(dataset == "scenario" & !is.null(los_projections_file)) {
        df <- los_reduction_sim(df)
      } else {
        df <- df
      }
      
      # get total daily volume for each service line and unit type
      df <- df %>%
        group_by(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
        summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop")
      
      # project volumes for scenario dataset
      if (dataset == "scenario" & !is.null(vol_projections_file)) {
        df <- df %>%
          mutate(UNIQUE_ID = paste0(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE)) %>%
          left_join(read_csv(paste0(cap_dir, "Mapping Info/volume projections/", vol_projections_file),
                             show_col_types = FALSE),
                    by = c("UNIQUE_ID" = "UNIQUE_ID")) %>%
          mutate(DAILY_DEMAND = case_when(
            is.na(PERCENT) ~ DAILY_DEMAND,
            TRUE ~ DAILY_DEMAND + DAILY_DEMAND*PERCENT))
      } else {
        df <- df
      }
      
    })
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
                AVG_UTILIZATION_SCENARIO = mean(UTILIZATION_SCENARIO, na.rm = TRUE)) %>%
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
                AVG_UTILIZATION_SCENARIO = mean(UTILIZATION_SCENARIO, na.rm = TRUE)) %>%
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
             AVG_PERCENT_85_SCENARIO) %>%
      filter(AVG_DAILY_DEMAND_BASELINE > 1) %>%
      left_join(ip_comparison_weekday_total,
                by = c("LOC_NAME" = "LOC_NAME",
                       "SERVICE_GROUP" = "SERVICE_GROUP")) %>%
      relocate(AVG_WEEKDAY_UTILIZATION_BASELINE, .after = AVG_UTILIZATION_BASELINE) %>%
      relocate(AVG_WEEKDAY_UTILIZATION_SCENARIO, .after = AVG_UTILIZATION_SCENARIO)
    
    return(list(
      ip_comparison_daily = ip_comparison_daily,
      ip_comparison_monthly = ip_comparison_monthly,
      ip_comparison_total = ip_comparison_total,
      ip_utilization_output = ip_utilization_output
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
  
  rm(outputs_list)
  return(list(
    ip_utilization_output = ip_utilization_output ,
    ip_comparison_total = ip_comparison_total,
    ip_comparison_daily = ip_comparison_daily,
    ip_comparison_monthly = ip_comparison_monthly
  ))
}