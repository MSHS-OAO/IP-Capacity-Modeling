ip_daily_demand_model <- function(
    n_simulations = 1,
    vol_projections,
    los_projections,
    initial_baseline = baseline
) {
  
  #count number of projection years
  n_projection_years <- length(vol_projections)
  
  # Store all simulation and projection year results
  all_projection_outputs <- vector(
    mode = "list",
    length = n_simulations * n_projection_years
  )
  
  # Store encounter-level scenarios by simulation and year
  encounter_scenario_outputs <- vector(
    mode = "list",
    length = n_simulations
  )
  
  names(encounter_scenario_outputs) <- paste0(
    "simulation_",
    seq_len(n_simulations)
  )
  
  output_index <- 1
  
  # calculate daily demand for baseline (2025)
  
  initial_baseline_year <- as.character(
    lubridate::year(
      min(initial_baseline$SERVICE_DATE, na.rm = TRUE)
    )
  )
  
  initial_baseline_daily <- daily_demand(
    list(
      baseline = initial_baseline
    )
  )
  
  initial_baseline_daily <- daily_demand_grouper(
    initial_baseline_daily,
    level = "SERVICE_GROUP"
  )[["baseline"]]
  
  initial_baseline_average <- initial_baseline_daily %>%
    group_by(
      LOC_NAME,
      SERVICE_GROUP,
      SERVICE_DATE
    ) %>%
    summarise(
      AVG_DAILY_DEMAND_BASELINE = sum(
        DAILY_DEMAND,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      AVG_DAILY_DEMAND_SCENARIO = AVG_DAILY_DEMAND_BASELINE,
      PROJECTION_YEAR_INDEX = 0,
      PROJECTION_YEAR = initial_baseline_year,
      N_SIMULATIONS = 1
    )
  
  #outer loop: simulations
  for (simulation_index in seq_len(n_simulations)) {
    
    print(
      paste(
        "Running simulation",
        simulation_index,
        "of",
        n_simulations
      )
    )
    
    #name the years for the first simulation in the OR outputs
    encounter_scenario_outputs[[simulation_index]] <- vector(
      mode = "list",
      length = n_projection_years
    )
    
    names(encounter_scenario_outputs[[simulation_index]]) <-
      names(vol_projections)
    
    # Each simulation starts from the original baseline
    current_baseline <- initial_baseline
    
    #inner loop: projection years
    for (year_index in seq_len(n_projection_years)) {
      
      projection_year <- as.integer(
        names(vol_projections)[year_index]
      )
      
      print(
        paste(
          "Simulation",
          simulation_index,
          "- projection year",
          projection_year,
          "- year",
          year_index,
          "of",
          n_projection_years
        )
      )
      
      # Change dates to the current projection year
      current_baseline <- current_baseline %>%
        mutate(
          ORIGINAL_MONTH = lubridate::month(SERVICE_DATE),
          ORIGINAL_DAY = lubridate::day(SERVICE_DATE),
          
          TARGET_MONTH_START = lubridate::make_date(
            year = projection_year,
            month = ORIGINAL_MONTH,
            day = 1
          ),
          
          SERVICE_DATE = lubridate::make_date(
            year = projection_year,
            month = ORIGINAL_MONTH,
            day = pmin(
              ORIGINAL_DAY,
              lubridate::days_in_month(TARGET_MONTH_START)
            )
          ),
          
          SERVICE_MONTH = lubridate::floor_date(
            SERVICE_DATE,
            unit = "month"
          )
        ) %>%
        select(
          -ORIGINAL_MONTH,
          -ORIGINAL_DAY,
          -TARGET_MONTH_START
        )
      
      # Globally assign the current year's projection files
      
      assign(
        "vol_projections_file",
        vol_projections[[year_index]],
        envir = .GlobalEnv
      )
      
      assign(
        "los_projections_file",
        los_projections[[year_index]],
        envir = .GlobalEnv
      )
      
      # Create baseline and scenario datasets
      scenario_dataset <- current_baseline
      
      datasets_processed <- list(
        baseline = current_baseline,
        scenario = scenario_dataset
      )
      
      # Calculate daily demand
      
      daily_demand_data <- daily_demand(
        datasets_processed
      )
      
      # Group daily demand by site and service group
      daily_demand_service_group <- daily_demand_grouper(
        daily_demand_data,
        level = "SERVICE_GROUP"
      )
      
      # OR
      
      daily_demand_encounter <- daily_demand_grouper(
        daily_demand_data,
        level = "ENCOUNTER"
      )
      
      #FINAL OUTCOME OF THIS LIST SHOULD BE N_SIMULATION SCENARIO LISTS FOR EACH YEAR

      encounter_scenario_outputs[[simulation_index]][[as.character(projection_year)]] <- daily_demand_encounter[["scenario"]] %>%
        collect() %>%
        mutate(
          SIMULATION = simulation_index,
          PROJECTION_YEAR_INDEX = year_index,
          PROJECTION_YEAR = as.character(projection_year)
        )
      
      #
      
      # Store daily demand for the current simulation and year
      all_projection_outputs[[output_index]] <-
        purrr::imap_dfr(
          daily_demand_service_group,
          function(df, dataset_name) {
            
            df %>%
              collect() %>%
              group_by(
                LOC_NAME,
                SERVICE_GROUP,
                SERVICE_DATE
              ) %>%
              summarise(
                DAILY_DEMAND = sum(
                  DAILY_DEMAND,
                  na.rm = TRUE
                ),
                .groups = "drop"
              ) %>%
              mutate(
                DATASET = dataset_name,
                SIMULATION = simulation_index,
                PROJECTION_YEAR_INDEX = year_index,
                PROJECTION_YEAR = as.character(projection_year)
              )
          }
        )
      
      output_index <- output_index + 1
      
      #carry the scenario into the next year
      current_baseline <-
        daily_demand_data[["scenario"]] %>%
        ungroup() %>%
        mutate(
          QUANTITY = BED_CHARGES
        ) %>%
        select(
          -BED_CHARGES
        )
    }
  }
  
  #merge all simulations and projection years
  all_simulations <- dplyr::bind_rows(
    all_projection_outputs
  )
  
  daily_demand_keys <- all_simulations %>%
    distinct(
      DATASET,
      LOC_NAME,
      SERVICE_GROUP,
      SERVICE_DATE,
      PROJECTION_YEAR_INDEX,
      PROJECTION_YEAR
    )
  
  #assign 0 to missing demand through the year
  all_simulations_complete <- daily_demand_keys %>%
    tidyr::crossing(
      SIMULATION = seq_len(n_simulations)
    ) %>%
    left_join(
      all_simulations,
      by = c(
        "DATASET",
        "LOC_NAME",
        "SERVICE_GROUP",
        "SERVICE_DATE",
        "PROJECTION_YEAR_INDEX",
        "PROJECTION_YEAR",
        "SIMULATION"
      )
    ) %>%
    mutate(
      DAILY_DEMAND = coalesce(
        DAILY_DEMAND,
        0
      )
    )
  
  # Average each projection year across # of simulations
  projected_average_daily_demand <-
    all_simulations_complete %>%
    group_by(
      DATASET,
      LOC_NAME,
      SERVICE_GROUP,
      SERVICE_DATE,
      PROJECTION_YEAR_INDEX,
      PROJECTION_YEAR
    ) %>%
    summarise(
      AVG_DAILY_DEMAND = mean(
        DAILY_DEMAND,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(
      DATASET = toupper(DATASET)
    ) %>%
    tidyr::pivot_wider(
      names_from = DATASET,
      values_from = AVG_DAILY_DEMAND,
      names_prefix = "AVG_DAILY_DEMAND_"
    ) %>%
    mutate(
      N_SIMULATIONS = n_simulations
    )
  
  # Append all projection years
  
  average_daily_demand <- bind_rows(
    initial_baseline_average,
    projected_average_daily_demand
  ) %>%
    arrange(
      PROJECTION_YEAR_INDEX,
      LOC_NAME,
      SERVICE_GROUP,
      SERVICE_DATE
    ) %>%
    select(
      #PROJECTION_YEAR_INDEX,
      LOC_NAME,
      SERVICE_GROUP,
      PROJECTION_YEAR,
      SERVICE_DATE,
      AVG_DAILY_DEMAND_BASELINE,
      AVG_DAILY_DEMAND_SCENARIO
      #,N_SIMULATIONS
    )
  
  return(
    list(
      average_daily_demand = average_daily_demand,
      encounter_scenario_outputs = encounter_scenario_outputs
    )
  )
}