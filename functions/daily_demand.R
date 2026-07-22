daily_demand <- function(datasets_processed) {
  
  daily_demand <- lapply(names(datasets_processed), function(dataset) {
    
    # load dataset based on name of list element
    df <- datasets_processed[[dataset]]
    
    # get daily demand by encounter first
    df <- df %>%
      #filter(!is.na(EXTERNAL_NAME)) %>%
      group_by(
        FACILITY_MSX, ENCOUNTER_NO, MSMRN, DSCH_DT_SRC, ADMIT_DT_SRC, MSDRG_CD_SRC, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE,
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
    
    # project changes in LOS
    if (dataset == "scenario" && exists("los_projections_file") && !is.null(los_projections_file)) {
      df <- los_reduction_sim(df)
    }
    
    
    df

  })
  
  names(daily_demand) <- names(datasets_processed)
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