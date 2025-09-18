# exclusion of emergency admits with primary procedure within x days of admit
emergency_exclusion <- function(indexes, exclusion = TRUE, proc_lag = 1) {
  
  if (exclusion == TRUE) {
    
    # get encounters of patients in hosp1 indexes
    encounters <- baseline[indexes,]$ENCOUNTER_NO
    
    # get encounters admitted through ED with procedure within 1 day of admit
    exclusion_encounters <- tbl(con_prod, "IPCAP_PROCEDURE_DATA") %>%
      filter(ADMIT_TYPE_DESC_SRC == 'EMERGENCY',
             PROC_TYPE == 'P') %>%
      mutate(admit_to_proc = PROC_DT - ADMIT_DT_SRC) %>%
      filter(admit_to_proc < proc_lag) %>%
      select(ENCOUNTER_NO) %>%
      collect() %>%
      filter(ENCOUNTER_NO %in% encounters) %>%
      pull()
    
    # get indexes in scenario df of exclusion encounters
    exclusion_indexes <- which(baseline$ENCOUNTER_NO %in% exclusion_encounters)
    
    # remove exclusion indexes from cohort
    indexes <- setdiff(indexes, exclusion_indexes)
    
  }
  
  return(indexes)
  
}