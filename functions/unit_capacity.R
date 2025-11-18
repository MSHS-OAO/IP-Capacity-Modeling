unit_capacity <- function(unit_capacity_adjustments = NULL) {
  
  # List all CSV files in the directory
  bed_cap_csv <- list.files(paste0(cap_dir, "Tableau Data/Bed Capacity/"),
                            pattern = "\\.csv$", full.names = TRUE)
  # read each CSV and list average bed capacity for each unit monthly
  bed_cap <- bed_cap_csv %>%
    map_dfr(~ read_csv(.x, show_col_types = FALSE) %>% mutate(source_file = basename(.x))) %>%
    rename(HOSPITAL = Location,
           SERVICE_GROUP = `Service Group`,
           MEASURE = `Measure Names`,
           SERVICE_MONTH = `Day of Census Start`) %>%
    filter(MEASURE == 'Avg Total Beds') %>%
    mutate(
      HOSPITAL = case_when(
        HOSPITAL == "MOUNT SINAI BETH ISRAEL" ~ "MSBI",
        HOSPITAL == "MOUNT SINAI BROOKLYN" ~ "MSB",
        HOSPITAL == "MOUNT SINAI MORNINGSIDE" ~ "MSM",
        HOSPITAL == "MOUNT SINAI QUEENS" ~ "MSQ",
        HOSPITAL == "MOUNT SINAI WEST" ~ "MSW",
        HOSPITAL == "THE MOUNT SINAI HOSPITAL" ~ "MSH"),
      SERVICE_MONTH = mdy(SERVICE_MONTH),
      EXTERNAL_NAME = Unit) %>%
    group_by(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_MONTH) %>%
    summarise(DATASET = "BASELINE",
              BED_CAPACITY = sum(`Measure Values`, na.rm = TRUE)) %>%
    mutate(SERVICE_GROUP = 
             case_when(EXTERNAL_NAME == "MSH CSDU KCC 6 North" ~ "Heart",
                       TRUE ~ SERVICE_GROUP))
  
  # create duplicate df for scenario and bind it to the basline bed cap
  bed_cap_scenario <- bed_cap %>% mutate(DATASET = "SCENARIO")
  bed_cap <- bed_cap %>% rbind(bed_cap_scenario)
  
  # if there is a unit capacity adjustment for the sim update scenario bed cap
  if (!is.null(unit_capacity_adjustments)) {
    # read in file for unit capacity changes to be applied to scenario output
    scenario_capacity <- read_csv(paste0(cap_dir, "Mapping Info/unit capacity/",
                                         unit_capacity_adjustments), show_col_types = FALSE)

    # get list of all months in bed capacity data
    unique_months <- unique(bed_cap$SERVICE_MONTH)
    # get list of all unique units with changes
    unique_units <- unique(scenario_capacity$EXTERNAL_NAME)
    
    # expand the scenario capacity file for each month in the simulation
    scenario_capacity <- bind_rows(replicate(length(unique_months), 
                                             scenario_capacity, 
                                             simplify = FALSE)) %>%
      mutate(SERVICE_MONTH = rep(unique_months, each = length(unique_units)),
             DATASET = "SCENARIO") %>%
      select(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME, SERVICE_MONTH, DATASET, BED_CAPACITY)
    
    bed_cap <- bed_cap %>%
      filter(!(EXTERNAL_NAME %in% unique_units & DATASET == "SCENARIO")) %>%
      rbind(scenario_capacity)
  }
  
  bed_cap <- bed_cap %>%
    group_by(HOSPITAL, SERVICE_GROUP, SERVICE_MONTH, DATASET) %>%
    summarise(BED_CAPACITY = sum(BED_CAPACITY)) %>%
    pivot_wider(id_cols = c(HOSPITAL, SERVICE_GROUP, SERVICE_MONTH),
                names_from = DATASET,
                values_from = BED_CAPACITY)
   
  return(bed_cap) 
}