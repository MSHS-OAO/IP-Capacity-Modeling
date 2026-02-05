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
      
      # normalize rerouting percentages to 100 if needed
      reroute_service_group_percent[[hosp]] <- 
        reroute_service_group_percent[[hosp]]/sum(reroute_service_group_percent[[hosp]])
      
      # determine how many rows will be edited for each service group
      counts <- round(nrow(reroute_data) * reroute_service_group_percent[[hosp]])
      remainder <- nrow(reroute_data) - sum(counts)
      max_service <- names(which.max(counts))
      counts[max_service] <- counts[max_service] + remainder
      
      # randomly apply new service groupings at desired percentages
      new_service <- sample(unlist(mapply(rep, names(counts), counts)))
      
      # overwrite rerouted df
      reroute_data$SERVICE_GROUP <- new_service
      
      # add rerouted data to new demand df
      new_demand <- rbind(new_demand, reroute_data)
    }
    
    # remove original routing of patients and replace with rerouted data
    scenario <- scenario %>%
      filter(!(ENCOUNTER_NO %in% c(hosp_1_sampled_encounters, hosp_2_sampled_encounters))) %>%
      rbind(new_demand)  
  }
  
  return(scenario)
}