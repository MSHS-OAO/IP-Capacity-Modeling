unit_capacity <- function(unit_capacity_adjustments = NULL) {
  
  # read each CSV and list average bed capacity for each unit monthly
  bed_cap <- read_csv(paste0(cap_dir, "Tableau Data/Detail_data.csv"),
                      show_col_types = FALSE) %>%
    rename(HOSPITAL = Location,
           SERVICE_GROUP = `Service Group`,
           EXTERNAL_NAME = Unit) %>%
    mutate(
      HOSPITAL = case_when(
        HOSPITAL == "MOUNT SINAI BETH ISRAEL" ~ "MSBI",
        HOSPITAL == "MOUNT SINAI BROOKLYN" ~ "MSB",
        HOSPITAL == "MOUNT SINAI MORNINGSIDE" ~ "MSM",
        HOSPITAL == "MOUNT SINAI QUEENS" ~ "MSQ",
        HOSPITAL == "MOUNT SINAI WEST" ~ "MSW",
        HOSPITAL == "THE MOUNT SINAI HOSPITAL" ~ "MSH"),
      SERVICE_GROUP = case_when(
        EXTERNAL_NAME == "MSH KCC 2 South" ~ "Rehab",
        TRUE ~ SERVICE_GROUP),
      SERVICE_DATE = mdy(`Day of Census Day`)) %>%
    group_by(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_DATE) %>%
    summarise(DATASET = "BASELINE",
              BED_CAPACITY = sum(`Count of Custom SQL Query`, na.rm = TRUE)) %>%
    filter(SERVICE_DATE >= min(baseline$SERVICE_DATE),
           SERVICE_DATE <= max(baseline$SERVICE_DATE))
  
  # create duplicate df for scenario and bind it to the basline bed cap
  bed_cap_scenario <- bed_cap %>% mutate(DATASET = "SCENARIO")
  bed_cap <- bed_cap %>% rbind(bed_cap_scenario)
  
  # if there is a unit capacity adjustment for the sim update scenario bed cap
  if (!is.null(unit_capacity_adjustments)) {
    # read in file for unit capacity changes to be applied to scenario output
    scenario_capacity <- read_csv(paste0(cap_dir, "Mapping Info/unit capacity/",
                                         unit_capacity_adjustments), show_col_types = FALSE)
    
    # get list of all months in bed capacity data
    unique_days <- unique(bed_cap$SERVICE_DATE)
    # get list of all unique units with changes
    unique_units <- unique(scenario_capacity$EXTERNAL_NAME)
    
    # expand the scenario capacity file for each day in the simulation
    scenario_capacity <- bind_rows(replicate(length(unique_days), 
                                             scenario_capacity, 
                                             simplify = FALSE)) %>%
      mutate(SERVICE_DATE = rep(unique_days, each = length(unique_units)),
             DATASET = "SCENARIO") %>%
      select(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_DATE, DATASET, BED_CAPACITY)
    
    bed_cap <- bed_cap %>%
      filter(!(EXTERNAL_NAME %in% unique_units & DATASET == "SCENARIO")) %>%
      rbind(scenario_capacity)
  }
  
  bed_cap <- bed_cap %>%
    group_by(HOSPITAL, SERVICE_GROUP, SERVICE_DATE, DATASET) %>%
    summarise(BED_CAPACITY = sum(BED_CAPACITY)) %>%
    pivot_wider(id_cols = c(HOSPITAL, SERVICE_GROUP, SERVICE_DATE),
                names_from = DATASET,
                values_from = BED_CAPACITY)
  
  return(bed_cap) 
}