daily_demand <- function(dataset, df, level) {
  
  # load dataset based on name of list element
  df <- df[[dataset]]
  
  # get daily demand by service line and service group
  df <- df %>%
    filter(!is.na(EXTERNAL_NAME)) %>%
    group_by(ENCOUNTER_NO, MSDRG_CD_SRC, FACILITY_MSX, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, 
             DSCH_UNIT_DESC_MSX, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, 
             SERVICE_DATE, LOS_NO_SRC) %>%
    summarise(BED_CHARGES = sum(QUANTITY), .groups = "drop") %>%
    mutate(BED_CHARGES = case_when(
      BED_CHARGES > 1 ~ 1,
      TRUE ~ BED_CHARGES)) 
  
  #execute volume projections
  if (dataset == "scenario" & !is.null(vol_projections_file)) {
    
    df <- volume_projections(df, vol_projections_file)
    
  } else {
    df <- df
  }
  
  # project changes in LOS
  if(dataset == "scenario" & !is.null(los_projections_file)) {
    
    df <- los_reduction_sim(df)
    
  } else {
    df <- df
  }
  
  # get daily volume by encounter, unit and service group
  if(level == "encounter") {
    df <- df %>%
      group_by(ENCOUNTER_NO, LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
      summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop")
  } else if (level == "unit") {
    df <- df %>%
      group_by(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, EXTERNAL_NAME, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
      summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop")
  } else if (level == "service_group") {
    df_service_group <- df %>%
      group_by(LOC_NAME, ATTENDING_VERITY_REPORT_SERVICE, SERVICE_GROUP, SERVICE_MONTH, SERVICE_DATE) %>%
      summarise(DAILY_DEMAND = sum(BED_CHARGES), .groups = "drop")
  }
  
}
