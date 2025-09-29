location_swap <- function(hospitals, services, percentage_to_hosp1 = 0.9, percentage_to_hosp2 = 0.9) {
  
  # scenario data staged as replica of baseline
  scenario <- baseline 
  
  # identify row indexes where patient is at hospial 1 and is in service line 2
  hosp_1_indexes <- emergency_exclusion(
    indexes = which(baseline$FACILITY_MSX == hospitals[[1]]&
                      baseline$VERITY_DIV_DESC_SRC %in% services[[2]]),
    exclusion = exclusion_hosp1)
  
  # identify row indexes where patient is at hospial 2 and is in service line 1
  hosp_2_indexes <- emergency_exclusion(
    index = which(baseline$FACILITY_MSX == hospitals[[2]]&
                    baseline$VERITY_DIV_DESC_SRC %in% services[[1]]),
    exclusion = exclusion_hosp2)
  
  # take percent sample of full list of scenario indedxes and place index samples in list to prep for loop
  sample_rows <- list(
    "hosp_1_sampled_indexes" = sample(hosp_1_indexes, size = floor(length(hosp_1_indexes) * percentage_to_hosp2)),
    "hosp_2_sampled_indexes" = sample(hosp_2_indexes, size = floor(length(hosp_2_indexes) * percentage_to_hosp1)))
  
  # reset the FACILITY of the scenario patient encounters to their new FACILITY
  scenario <- scenario %>%
    mutate(FACILITY_MSX = case_when(
      row_number() %in% sample_rows$hosp_1_sampled_indexes ~ hospitals[[2]],  # MSH → MSM
      row_number() %in% sample_rows$hosp_2_sampled_indexes ~ hospitals[[1]],  # MSM → MSH
      TRUE ~ FACILITY_MSX))
  
  # create a df for new demand that has been rerouted
  new_demand <- data.frame()
  # loop through each hospital and reroute the demand
  for (hosp in 1:length(hospitals)){
    
    # get sample rows of hospital i that are also getting rerouted
    reroute_data <- scenario[sample_rows[[hosp]],] 
    
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
    filter(!(row_number() %in% unlist(sample_rows))) %>%
    rbind(new_demand)
  
  return(scenario)
}