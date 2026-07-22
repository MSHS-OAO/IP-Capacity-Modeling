location_swap <- function(hospitals, services,percentage_to_hosp1,percentage_to_hosp2) {
  
  # scenario data staged as replica of baseline
  scenario <- baseline 
  
  if(!is.null(hospitals)) {
    # identify row indexes where patient is at hospital 1 and is in service line 2
    hosp_1_indexes <- emergency_exclusion(
      indexes = which(baseline$LOC_NAME == hospitals[[1]]&
                        baseline$ATTENDING_VERITY_DIV_DESC %in% services[[2]]),
      exclusion = exclusion_hosp1)
    
    # identify row indexes where patient is at hospital 2 and is in service line 1
    hosp_2_indexes <- emergency_exclusion(
      indexes = which(baseline$LOC_NAME == hospitals[[2]]&
                        baseline$ATTENDING_VERITY_DIV_DESC %in% services[[1]]),
      exclusion = exclusion_hosp2)
    
    # get unique encounter_no with provided combinations of LOC_NAME and ATTENDING_VERITY_DIV_DESC
    hosp_1_encounters <- unique(baseline$ENCOUNTER_NO[hosp_1_indexes])
    hosp_2_encounters <- unique(baseline$ENCOUNTER_NO[hosp_2_indexes])
    
    # sample from both comboos
    hosp_1_sampled_encounters <- sample(
      hosp_1_encounters,
      size = floor(length(hosp_1_encounters) * percentage_to_hosp2)
    )
    
    hosp_2_sampled_encounters <- sample(
      hosp_2_encounters,
      size = floor(length(hosp_2_encounters) * percentage_to_hosp1)
    )
    
    # swap sampled encounters in hosp_1_sampled_encounters and hosp_2_sampled_encounters
    scenario <- scenario %>%
      mutate(LOC_NAME = case_when(
        ENCOUNTER_NO %in% hosp_1_sampled_encounters ~ hospitals[[2]], # MSH -> MSM
        ENCOUNTER_NO %in% hosp_2_sampled_encounters ~ hospitals[[1]], # MSM -> MSH
        TRUE ~ LOC_NAME
      ))
    
    #construct list of swapped encounters for SERVICE_GROUP swaps
    sampled_encounters <- list(
      "hosp_1_sampled_encounters" = hosp_1_sampled_encounters,
      "hosp_2_sampled_encounters" = hosp_2_sampled_encounters
    )
    
    # create a df for new demand that has been rerouted
    new_demand <- data.frame()
    
    
    # loop through each hospital and reroute the demand
    for (hosp in 1:length(hospitals)){
      
      #get full encounter set for hosp 
      enc_set <- sampled_encounters[[hosp]]
      
      # get sample rows of hospital i that are also getting rerouted
      reroute_data <- scenario %>% filter(ENCOUNTER_NO %in% enc_set)

      #assign service groups while keeping the overall percentages as close to exact as possible
      pct <- reroute_service_group_percent[[hosp]]
      pct <- pct / sum(pct)

      encounter_days <- reroute_data %>%
        distinct(ENCOUNTER_NO, SERVICE_DATE)

      number_of_days <- nrow(encounter_days)

      if (number_of_days > 0) {

        exact_counts <- number_of_days * pct
        counts <- floor(exact_counts)
        remainder <- number_of_days - sum(counts)

        if (remainder > 0) {
          remainder_order <- order(
            exact_counts - counts,
            decreasing = TRUE
          )

          counts[remainder_order[seq_len(remainder)]] <-counts[remainder_order[seq_len(remainder)]] + 1
        }

        new_service_groups <- rep(
          names(counts),
          times = counts
        )

        encounter_days$NEW_SERVICE_GROUP <- sample(new_service_groups,size = number_of_days,replace = FALSE)

        reroute_data <- reroute_data %>%
          left_join(
            encounter_days,
            by = c("ENCOUNTER_NO", "SERVICE_DATE")
          ) %>%
          mutate(
            SERVICE_GROUP = NEW_SERVICE_GROUP
          ) %>%
          select(-NEW_SERVICE_GROUP)
      }
      
      new_demand <- bind_rows(new_demand, reroute_data)
    }
    
    scenario <- scenario %>%
      filter(!(ENCOUNTER_NO %in% c(hosp_1_sampled_encounters, hosp_2_sampled_encounters))) %>%
      rbind(new_demand)  
  }
  
  return(scenario)
}