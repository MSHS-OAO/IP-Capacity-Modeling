daily_demand <- function(datasets_processed) {
  
  volume_daily_demand <- NULL
  
  daily_demand <- lapply(names(datasets_processed), function(dataset) {
    
    # load dataset based on name of list element
    df <- datasets_processed[[dataset]]
    
    # get daily demand by encounter first
    df <- df %>%
      #filter(!is.na(EXTERNAL_NAME)) %>%
      group_by(
        FACILITY_MSX, ENCOUNTER_NO, MSMRN, DSCH_DT_SRC, ADMIT_DT_SRC, MSDRG_CD_SRC, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, ATTENDING_VERITY_DIV_DESC,
        DSCH_UNIT_DESC_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH,
        SERVICE_DATE, LOS_NO_SRC
      ) %>%
      summarise(
        BED_CHARGES = sum(QUANTITY, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        BED_CHARGES = case_when(
          BED_CHARGES > 1 ~ 1,
          TRUE ~ BED_CHARGES
        )
      )
    
    # execute volume projections
    if (dataset == "scenario" && exists("vol_projections_file") && !is.null(vol_projections_file)) {
      df <- volume_projections(df, vol_projections_file)
    }
    
    if (dataset == "scenario") {
      volume_daily_demand <<- df
    }
    
    # project changes in LOS
    if (dataset == "scenario" && exists("los_projections_file") && !is.null(los_projections_file)) {
      df <- los_reduction_sim(df)
    }
    
    
    df

  })
  
  names(daily_demand) <- names(datasets_processed)
  
  if (exists("los_projections_file") && !is.null(los_projections_file)) 
    {
    
    daily_demand_los_validation(
      daily_demand = daily_demand,
      volume_daily_demand = volume_daily_demand,
      los_projections_file = los_projections_file)
    }
  
  return(daily_demand)
}



daily_demand_grouper <- function(
    daily_demand,
    level = c("SERVICE_GROUP", "UNIT", "ENCOUNTER")
) {
  
  level <- match.arg(level)
  
  lapply(daily_demand, function(df) {
    
    if (level == "SERVICE_GROUP") {
      
      df %>%
        group_by(
          LOC_NAME,
          SERVICE_GROUP,
          SERVICE_MONTH,
          SERVICE_DATE
        ) %>%
        summarise(
          DAILY_DEMAND = sum(BED_CHARGES, na.rm = TRUE),
          .groups = "drop"
        )
      
    } else if (level == "UNIT") {
      
      df %>%
        group_by(
          LOC_NAME,
          SERVICE_GROUP,
          EXTERNAL_NAME,
          SERVICE_MONTH,
          SERVICE_DATE
        ) %>%
        summarise(
          DAILY_DEMAND = sum(BED_CHARGES, na.rm = TRUE),
          .groups = "drop"
        )
      
    } else if (level == "ENCOUNTER") {
      
      df %>%
        mutate(
          ADMIT_DOW = toupper(
            lubridate::wday(ADMIT_DT_SRC, label = TRUE)
          )
        ) %>%
        select(any_of(c(
          "ENCOUNTER_NO",
          "MSMRN",
          "LOC_NAME",
          "SERVICE_GROUP",
          "EXTERNAL_NAME",
          "ADMIT_DT_SRC",
          "DSCH_DT_SRC",
          "NEW_ADMIT_DT_SRC",
          "NEW_DSCH_DT_SRC",
          "SERVICE_DATE",
          "SERVICE_MONTH",
          "DSCH_UNIT_DESC_MSX",
          "ATTENDING_VERITY_REPORT_SERVICE",
          "MSDRG_CD_SRC",
          "ADMIT_DOW"
        ))) %>%
        distinct()
    }
    
  })
}


daily_demand_los_validation <- function(
    daily_demand,
    volume_daily_demand,
    los_projections_file
) {
  
  los_targets <- read_csv(
    paste0(
      cap_dir,
      "Mapping Info/los adjustments/",
      los_projections_file
    ),
    show_col_types = FALSE
  ) %>%
    filter(!is.na(TARGET_LOS)) %>%
    transmute(
      SITE = Hospital,
      SL = VERITY_REPORT_SERVICE_MSX,
      TARGET_LOS
    ) %>%
    distinct()
  
  
  # Reusable validation summary
  calculate_alos <- function(
    df,
    encounter_column,
    days_column,
    alos_column
  ) {
    
    df %>%
      filter(
        !is.na(LOC_NAME),
        !is.na(ATTENDING_VERITY_REPORT_SERVICE),
        !is.na(ENCOUNTER_NO)
      ) %>%
      group_by(
        LOC_NAME,
        ATTENDING_VERITY_REPORT_SERVICE
      ) %>%
      summarise(
        ENCOUNTERS = n_distinct(ENCOUNTER_NO),
        TOTAL_DAYS = sum(BED_CHARGES, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        ALOS = if_else(
          ENCOUNTERS > 0,
          TOTAL_DAYS / ENCOUNTERS,
          NA_real_
        )
      ) %>%
      rename(
        SITE = LOC_NAME,
        SL = ATTENDING_VERITY_REPORT_SERVICE,
        !!encounter_column := ENCOUNTERS,
        !!days_column := TOTAL_DAYS,
        !!alos_column := ALOS
      )
  }
  
  
  # Original baseline
  baseline_validation <- calculate_alos(
    df = daily_demand$baseline,
    encounter_column = "BASELINE_ENCOUNTERS",
    days_column = "BASELINE_TOTAL_DAYS",
    alos_column = "BASELINE_ALOS"
  )
  
  
  # Scenario after volume projection only
  volume_validation <- calculate_alos(
    df = volume_daily_demand,
    encounter_column = "VOLUME_ENCOUNTERS",
    days_column = "VOLUME_TOTAL_DAYS",
    alos_column = "VOLUME_ALOS"
  )
  
  
  # Scenario after both volume and LOS projections
  volume_los_validation <- calculate_alos(
    df = daily_demand$scenario,
    encounter_column = "VOLUME_LOS_ENCOUNTERS",
    days_column = "VOLUME_LOS_TOTAL_DAYS",
    alos_column = "VOLUME_LOS_ALOS"
  )
  
  
  validation <- los_targets %>%
    left_join(
      baseline_validation,
      by = c("SITE", "SL")
    ) %>%
    left_join(
      volume_validation,
      by = c("SITE", "SL")
    ) %>%
    left_join(
      volume_los_validation,
      by = c("SITE", "SL")
    ) %>%
    arrange(
      SITE,
      SL
    )
  
  los_validation[[los_projections_file]] <<- validation
  
  return(validation)
}