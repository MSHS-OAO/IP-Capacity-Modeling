multi_year_model <- function(n_years, n_simulations = 1) {
  
  outputs_list <- lapply(1:n_simulations, function(i) {
    
    print(paste("Running simulation #", i))
    
    # read in processed data from data refresh script
    datasets_processed <- list(
      "baseline" = baseline,
      "scenario" = baseline)
    
    # call daily_demand function
    daily_demand_forecast <- daily_demand(
      datasets_processed,
      level = "SERVICE_GROUP")
    names(daily_demand_forecast) <- names(datasets_processed)
    
    # baseline forecast on unaltered 2025 data
    baseline_df <- daily_demand_forecast[["baseline"]] %>%
      filter(LOC_NAME != "MSBI",
             SERVICE_DATE <= as.Date("2025-12-28"),
             ) %>%
      group_by(LOC_NAME, SERVICE_DATE) %>%
      summarise(DAILY_DEMAND = sum(DAILY_DEMAND)) %>%
      as_tsibble(key = c(LOC_NAME), index = SERVICE_DATE) %>%
      fill_gaps(DAILY_DEMAND = 0)
    
    baseline_fit <- baseline_df %>%
      model(
        linear = TSLM(DAILY_DEMAND ~ trend() + season("week") + fourier(period = "year", K = 4)),
        ets = ETS(DAILY_DEMAND ~ error("A") + trend("A") + season("A")))
    
    baseline_fit_accuracy <- accuracy(baseline_fit) %>%
      arrange(LOC_NAME, RMSE)
    
    baseline_fc <- baseline_fit %>% forecast(h = n_years)
    
    #################QC####################
    ggplot() +
      geom_line(data = daily_demand_forecast[["baseline"]] %>% group_by(LOC_NAME, SERVICE_DATE) %>% summarise(DAILY_DEMAND = sum(DAILY_DEMAND)),
                aes(x = SERVICE_DATE, y = DAILY_DEMAND, color = LOC_NAME)) +
      geom_line(data = baseline_fc %>% filter(.model == "linear"),
                aes(x = SERVICE_DATE, y = .mean, color = LOC_NAME))
    #######################################
    
    
    # convert scenario dataset into 2026 projection
    daily_demand_forecast[["scenario"]] <- daily_demand_forecast[["scenario"]] %>%
      mutate(SERVICE_DATE = SERVICE_DATE + years(1))
    
    # combine baseline and scenario as 2025-2026 daily demand
    daily_demand_forecast <- daily_demand_forecast %>%
      bind_rows()
    
    #################QC####################
    ggplot(daily_demand_forecast %>% group_by(LOC_NAME, SERVICE_DATE) %>% summarise(DAILY_DEMAND = sum(DAILY_DEMAND)),
           aes(x = SERVICE_DATE, y = DAILY_DEMAND, color = LOC_NAME)) +
      geom_line()
    #######################################
    
    
    
    df <- daily_demand_forecast %>%
      filter(LOC_NAME != "MSBI") %>%
      group_by(LOC_NAME, SERVICE_DATE) %>%
      summarise(DAILY_DEMAND = sum(DAILY_DEMAND)) %>%
      as_tsibble(key = c(LOC_NAME), index = SERVICE_DATE) %>%
      fill_gaps(DAILY_DEMAND = 0)
    
    fit <- df %>%
      model(
        linear = TSLM(DAILY_DEMAND ~ trend() + season("week") + fourier(period = "year", K = 4)),
        ets = ETS(DAILY_DEMAND ~ error("A") + trend("A") + season("A")))
    
    fit_accuracy <- accuracy(fit) %>%
      arrange(LOC_NAME, RMSE)
    
    fc <- fit %>% forecast(h = n_years)
    
    #################QC####################
    ggplot() +
      geom_line(data = daily_demand_processed %>% group_by(LOC_NAME, SERVICE_DATE) %>% summarise(DAILY_DEMAND = sum(DAILY_DEMAND)),
                aes(x = SERVICE_DATE, y = DAILY_DEMAND, color = LOC_NAME)) +
      geom_line(data = fc %>% filter(.model == "linear"),
                aes(x = SERVICE_DATE, y = .mean, color = LOC_NAME))
    #######################################
    
    for (hosp in unique(df$LOC_NAME)) {
      p <- ggplot() +
        # Plot Historical Data (The Actuals)
        geom_line(data = df %>% filter(LOC_NAME == hosp), aes(x = SERVICE_DATE, y = DAILY_DEMAND), color = "black") +
        # Plot Forecasted Data (The .mean values)
        geom_line(data = fc %>% filter(LOC_NAME == hosp), aes(x = SERVICE_DATE, y = .mean, color = .model), size = 1) +
        labs(title = paste("Location:", hosp),
             subtitle = "Black = History, Colored = Forecast Models",
             y = "Daily Demand", x = "Date", color = "Model Type") +
        theme_minimal()
      
      print(p)
    }
    # Scenario Outputs
    ## IP Demand Comparison
    # compare demand of baseline and scenario at daily level
    demand_comparison_daily <- daily_demand[["baseline"]] %>%
      full_join(daily_demand[["scenario"]],
                by = c("LOC_NAME"="LOC_NAME",
                       "ATTENDING_VERITY_REPORT_SERVICE"="ATTENDING_VERITY_REPORT_SERVICE",
                       "SERVICE_GROUP"="SERVICE_GROUP",
                       "SERVICE_MONTH"="SERVICE_MONTH",
                       "SERVICE_DATE"="SERVICE_DATE"),
                suffix = c("_BASELINE", "_SCENARIO")) %>%
      filter(LOC_NAME != "MSBI")
    
    return(list(
      demand_comparison_daily = demand_comparison_daily
    ))
  })
  }
