ip_utilization_model <- function(generator,n_simulations, hospitals, services, percentage_to_hosp1, percentage_to_hosp2)
{
  
  if (percentage_to_hosp1 %in% c(0, 1) &
      percentage_to_hosp2 %in% c(0, 1)) {
    n_simulations <- 1
  }
  
  
  # loop through each iteration
  outputs_list <- lapply(1:n_simulations, function(i) {
    
    
    print(paste("Running simulation #", i))
    
    # load bed capacity for baseline and scenario
    bed_cap <- unit_capacity(unit_capacity_adjustments)
    
    # read in processed data from data refresh script
    datasets_processed <- list(
      "baseline" = baseline,
      "scenario" = generator(hospitals, services, percentage_to_hosp1, percentage_to_hosp2)
    )
    
    # adjust scenario demand based on volume projections
    daily_demand <- lapply(names(datasets_processed), function(dataset) {
      
      # load dataset based on name of list element
      df <- datasets_processed[[dataset]]
      
      # get daily demand by service line and service group
      df <- df %>%
        filter(!is.na(EXTERNAL_NAME)) %>%
        group_by(ENCOUNTER_NO, FACILITY_MSX, VERITY_REPORT_SERVICE_MSX, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
        summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
        mutate(BED_CHARGES = case_when(
          BED_CHARGES > 1 ~ 1,
          TRUE ~ BED_CHARGES)) %>%
        group_by(FACILITY_MSX, VERITY_REPORT_SERVICE_MSX, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
        summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop")
      
      # project volumes for scenario dataset
      if (dataset == "scenario" & !is.na(vol_projections_file)) {
        df <- df %>%
          mutate(UNIQUE_ID = paste0(FACILITY_MSX, VERITY_REPORT_SERVICE_MSX)) %>%
          left_join(read_csv(paste0(cap_dir, "Mapping Info/volume projections/", vol_projections_file),
                             show_col_types = FALSE),
                    by = c("UNIQUE_ID" = "UNIQUE_ID")) %>%
          mutate(DAILY_DEMAND = DAILY_DEMAND + DAILY_DEMAND*PERCENT)
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
        group_by(FACILITY_MSX, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
        summarise(DAILY_DEMAND = sum(DAILY_DEMAND), .groups = "drop") %>%
        collect() %>%
      left_join(bed_cap, by = c("FACILITY_MSX" = "HOSPITAL", 
                                "SERVICE_GROUP" = "SERVICE_GROUP",
                                "SERVICE_MONTH" = "SERVICE_MONTH")) 
      
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
                by = c("FACILITY_MSX"="FACILITY_MSX",
                       "SERVICE_GROUP"="SERVICE_GROUP",
                       "SERVICE_MONTH"="SERVICE_MONTH",
                       "SERVICE_DATE"="SERVICE_DATE"),
                suffix = c("_BASELINE", "_SCENARIO")) %>%
      filter(FACILITY_MSX != "MSBI")
    
    # aggregate comparison at monthly level
    ip_comparison_monthly <- ip_comparison_daily %>%
      group_by(FACILITY_MSX, SERVICE_GROUP, SERVICE_MONTH) %>%
      summarise(TOTAL_DEMAND_BASELINE = sum(DAILY_DEMAND_BASELINE, na.rm = TRUE),
                TOTAL_DEMAND_SCENARIO = sum(DAILY_DEMAND_SCENARIO, na.rm = TRUE),
                TOTAL_85_BASELINE = sum(UTILIZATION_85_BASELINE),
                TOTAL_85_SCENARIO = sum(UTILIZATION_85_SCENARIO),
                TOTAL_95_BASELINE = sum(UTILIZATION_95_BASELINE),
                TOTAL_95_SCENARIO = sum(UTILIZATION_95_SCENARIO),
                AVG_BED_CAPACITY_BASELINE = mean(AVG_BED_CAPACITY_BASELINE),
                AVG_BED_CAPACITY_SCENARIO = mean(AVG_BED_CAPACITY_SCENARIO)) %>%
      mutate(AVG_DAILY_DEMAND_BASELINE = TOTAL_DEMAND_BASELINE/days_in_month(SERVICE_MONTH),
             AVG_DAILY_DEMAND_SCENARIO = TOTAL_DEMAND_SCENARIO/days_in_month(SERVICE_MONTH),
             AVG_PERCENT_85_BASELINE = TOTAL_85_BASELINE/days_in_month(SERVICE_MONTH),
             AVG_PERCENT_85_SCENARIO = TOTAL_85_SCENARIO/days_in_month(SERVICE_MONTH),
             AVG_PERCENT_95_BASELINE = TOTAL_95_BASELINE/days_in_month(SERVICE_MONTH),
             AVG_PERCENT_95_SCENARIO = TOTAL_95_SCENARIO/days_in_month(SERVICE_MONTH),
             AVG_UTILIZATION_BASELINE = AVG_DAILY_DEMAND_BASELINE/AVG_BED_CAPACITY_BASELINE,
             AVG_UTILIZATION_SCENARIO = AVG_DAILY_DEMAND_SCENARIO/AVG_BED_CAPACITY_SCENARIO) %>%
      mutate(across(where(is.numeric), \(x) coalesce(x, 0))) %>%
      mutate(AVG_UTILIZATION_SCENARIO = if_else(AVG_UTILIZATION_SCENARIO == 0, Inf, AVG_UTILIZATION_SCENARIO))
    
    # aggregate comparison at total level
    ip_comparison_total <- ip_comparison_daily %>%
      group_by(FACILITY_MSX, SERVICE_GROUP) %>% 
      summarise(TOTAL_DEMAND_BASELINE = sum(DAILY_DEMAND_BASELINE, na.rm = TRUE),
                TOTAL_DEMAND_SCENARIO = sum(DAILY_DEMAND_SCENARIO, na.rm = TRUE),
                TOTAL_85_BASELINE = sum(UTILIZATION_85_BASELINE),
                TOTAL_85_SCENARIO = sum(UTILIZATION_85_SCENARIO),
                TOTAL_95_BASELINE = sum(UTILIZATION_95_BASELINE),
                TOTAL_95_SCENARIO = sum(UTILIZATION_95_SCENARIO),
                AVG_BED_CAPACITY_BASELINE = mean(AVG_BED_CAPACITY_BASELINE),
                AVG_BED_CAPACITY_SCENARIO = mean(AVG_BED_CAPACITY_SCENARIO)) %>%
      mutate(AVG_DAILY_DEMAND_BASELINE = round(TOTAL_DEMAND_BASELINE/num_days,2),
             AVG_DAILY_DEMAND_SCENARIO = round(TOTAL_DEMAND_SCENARIO/num_days,2),
             AVG_PERCENT_85_BASELINE = TOTAL_85_BASELINE/num_days,
             AVG_PERCENT_85_SCENARIO = TOTAL_85_SCENARIO/num_days,
             AVG_PERCENT_95_BASELINE = TOTAL_95_BASELINE/num_days,
             AVG_PERCENT_95_SCENARIO = TOTAL_95_SCENARIO/num_days,
             AVG_UTILIZATION_BASELINE = AVG_DAILY_DEMAND_BASELINE/AVG_BED_CAPACITY_BASELINE,
             AVG_UTILIZATION_SCENARIO = AVG_DAILY_DEMAND_SCENARIO/AVG_BED_CAPACITY_SCENARIO) %>%
      mutate(across(where(is.numeric), \(x) coalesce(x, 0))) %>%
      mutate(AVG_UTILIZATION_SCENARIO = if_else(AVG_UTILIZATION_SCENARIO == 0, Inf, AVG_UTILIZATION_SCENARIO))
    
    # IP Utilization Output
    ip_utilization_output <- ip_comparison_total %>%
      select(FACILITY_MSX, SERVICE_GROUP, AVG_BED_CAPACITY_BASELINE, 
             AVG_DAILY_DEMAND_BASELINE,AVG_UTILIZATION_BASELINE, 
             AVG_PERCENT_85_BASELINE, AVG_BED_CAPACITY_SCENARIO, 
             AVG_DAILY_DEMAND_SCENARIO, AVG_UTILIZATION_SCENARIO, 
             AVG_PERCENT_85_SCENARIO)
    
    
    return(list(
      ip_comparison_daily = ip_comparison_daily,
      ip_comparison_monthly = ip_comparison_monthly,
      ip_comparison_total = ip_comparison_total,
      ip_utilization_output = ip_utilization_output
    ))
    
  })
  
  
  ip_utilization_output <- bind_rows(lapply(outputs_list, `[[`, "ip_utilization_output"))  %>%
    group_by(SERVICE_GROUP) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  ip_comparison_daily <- bind_rows(lapply(outputs_list, `[[`, "ip_comparison_daily")) %>%
    group_by(FACILITY_MSX, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE, AVG_BED_CAPACITY_BASELINE, AVG_BED_CAPACITY_SCENARIO) %>%
    summarise(across(ends_with("_BASELINE") | ends_with("_SCENARIO"), mean, na.rm = TRUE), .groups = "drop")
  
  ip_comparison_monthly <- bind_rows(lapply(outputs_list, `[[`, "ip_comparison_monthly")) %>%
    group_by(FACILITY_MSX, SERVICE_GROUP, SERVICE_MONTH, AVG_BED_CAPACITY_BASELINE, AVG_BED_CAPACITY_SCENARIO) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  ip_comparison_total <- bind_rows(lapply(outputs_list, `[[`, "ip_comparison_total")) %>%
    group_by(FACILITY_MSX, SERVICE_GROUP) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop")
  
  
  rm(outputs_list)
  return(list(
    ip_utilization_output = ip_utilization_output ,
    ip_comparison_total = ip_comparison_total,
    ip_comparison_daily = ip_comparison_daily,
    ip_comparison_monthly = ip_comparison_monthly
  ))
}