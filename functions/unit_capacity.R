unit_capacity <- function(unit_capacity_adjustments = NULL) {
  
  # load mapping file for all Epic IDs
  epic_mapping <- tbl(con_prod, "IPCAP_SERVICE_GROUPS") %>%
    collect() %>%
    mutate(VALID_TO = case_when(
      is.na(VALID_TO) ~ Sys.Date(),
      TRUE ~ VALID_TO),
      VALID_FROM = as.Date(VALID_FROM),
      VALID_TO = as.Date(VALID_TO))
  
  # read each CSV and list average bed capacity for each unit monthly
  bed_cap <- read_csv(paste0(cap_dir, "Tableau Data/Detail_data.csv"),
                      show_col_types = FALSE) %>%
    rename(HOSPITAL = Location,
           SERVICE_GROUP = `Service Group`,
           EXTERNAL_NAME = Unit) %>%
    mutate(SERVICE_DATE = mdy(`Day of Census Day`)) %>%
    group_by(HOSPITAL,EXTERNAL_NAME, SERVICE_DATE) %>%
    summarise(DATASET = "BASELINE",
              BED_CAPACITY = sum(`Count of Custom SQL Query`, na.rm = TRUE)) %>%
    mutate(BED_CAPACITY = case_when(
      EXTERNAL_NAME == "MSH KP2 L&D" ~ 20, # hard code bed cap based on Victoria's input
      TRUE ~ BED_CAPACITY)) %>%
    filter(HOSPITAL != "MOUNT SINAI BETH ISRAEL",
           SERVICE_DATE >= min(baseline$SERVICE_DATE),
           SERVICE_DATE <= max(baseline$SERVICE_DATE)) %>%
    left_join(epic_mapping, 
              by = join_by(EXTERNAL_NAME == EXTERNAL_NAME,
                           SERVICE_DATE >= VALID_FROM,
                           SERVICE_DATE <= VALID_TO)) %>%
    mutate(
      LOC_NAME = case_when(
        LOC_NAME == "MOUNT SINAI BETH ISRAEL" ~ "MSBI",
        LOC_NAME == "MOUNT SINAI BROOKLYN" ~ "MSB",
        LOC_NAME == "MOUNT SINAI MORNINGSIDE" ~ "MSM",
        LOC_NAME == "MOUNT SINAI QUEENS" ~ "MSQ",
        LOC_NAME == "MOUNT SINAI WEST" ~ "MSW",
        LOC_NAME == "THE MOUNT SINAI HOSPITAL" ~ "MSH")) %>%
    ungroup() %>%
    select(SERVICE_DATE, LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, EPIC_DEPT_ID, BED_CAPACITY, DATASET)
  
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
    # get list of all epic IDs in projections
    unique_id <- unique(scenario_capacity$EPIC_DEPT_ID)
    
    # create scenario bed capacity for full scenario dataset
    scenario_capacity <- expand_grid(
      SERVICE_DATE = unique_days,
      scenario_capacity) %>%
      mutate(DATASET = "SCENARIO")
    
    bed_cap <- bed_cap %>%
      filter(!(EPIC_DEPT_ID %in% unique_id & DATASET == "SCENARIO")) %>%
      rbind(scenario_capacity)
  }
  
  bed_cap <- bed_cap %>%
    group_by(LOC_NAME, SERVICE_GROUP, SERVICE_DATE, DATASET) %>%
    summarise(BED_CAPACITY = sum(BED_CAPACITY, na.rm = TRUE)) %>%
    pivot_wider(id_cols = c(LOC_NAME, SERVICE_GROUP, SERVICE_DATE),
                names_from = DATASET,
                values_from = BED_CAPACITY)
  
  return(bed_cap) 
}