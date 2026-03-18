unit_capacity <- function(unit_capacity_adjustments = NULL,
                          level = c("SERVICE_GROUP", "EXTERNAL_NAME")) {
  
  level <- match.arg(level)
  
  # load mapping file for all Epic IDs
  epic_mapping <- tbl(con_prod, "IPCAP_SERVICE_GROUPS") %>%
    collect() %>%
    mutate(
      VALID_TO = case_when(
        is.na(VALID_TO) ~ Sys.Date(),
        TRUE ~ VALID_TO
      ),
      VALID_FROM = as.Date(VALID_FROM),
      VALID_TO   = as.Date(VALID_TO)
    )
  
  # read each CSV and list average bed capacity for each unit monthly
  bed_cap <- read_csv(
    paste0(cap_dir, "Tableau Data/Detail_Full Data_data.csv"),
    show_col_types = FALSE
  ) %>%
    rename(
      HOSPITAL      = Location,
      SERVICE_GROUP = `Service Group`,
      EXTERNAL_NAME = Unit
    ) %>%
    mutate(SERVICE_DATE = mdy(`Day of Census Day`)) %>%
    group_by(HOSPITAL, EXTERNAL_NAME, BED_ID, SERVICE_DATE) %>%
    summarise(
      DATASET = "BASELINE",
      BED_CAPACITY = sum(`Valid Room Keep`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      BED_CAPACITY = case_when(
        BED_CAPACITY > 1 ~ 1,
        TRUE ~ BED_CAPACITY
      )
    ) %>%
    filter(
      HOSPITAL != "MOUNT SINAI BETH ISRAEL",
      HOSPITAL != "MOUNT SINAI SOUTH NASSAU",
      SERVICE_DATE >= min(baseline$SERVICE_DATE),
      SERVICE_DATE <= max(baseline$SERVICE_DATE)
    ) %>%
    left_join(
      epic_mapping,
      by = join_by(
        EXTERNAL_NAME == EXTERNAL_NAME,
        SERVICE_DATE >= VALID_FROM,
        SERVICE_DATE <= VALID_TO
      )
    ) %>%
    mutate(
      LOC_NAME = case_when(
        LOC_NAME == "MOUNT SINAI BETH ISRAEL"      ~ "MSBI",
        LOC_NAME == "MOUNT SINAI BROOKLYN"         ~ "MSB",
        LOC_NAME == "MOUNT SINAI MORNINGSIDE"      ~ "MSM",
        LOC_NAME == "MOUNT SINAI QUEENS"           ~ "MSQ",
        LOC_NAME == "MOUNT SINAI WEST"             ~ "MSW",
        LOC_NAME == "THE MOUNT SINAI HOSPITAL"     ~ "MSH",
        TRUE ~ LOC_NAME
      )
    ) %>%
    ungroup() %>%
    select(
      SERVICE_DATE,
      LOC_NAME,
      SERVICE_GROUP,
      EXTERNAL_NAME,
      EPIC_DEPT_ID,
      BED_CAPACITY,
      DATASET
    )
  
  # create duplicate df for scenario and bind it to the baseline bed cap
  bed_cap_scenario <- bed_cap %>%
    mutate(DATASET = "SCENARIO")
  
  bed_cap <- bind_rows(bed_cap, bed_cap_scenario)
  
  # if there is a unit capacity adjustment for the sim update scenario bed cap
  if (!is.null(unit_capacity_adjustments)) {
    
    # read in file for unit capacity changes to be applied to scenario output
    scenario_capacity <- read_csv(
      paste0(cap_dir, "Mapping Info/unit capacity/", unit_capacity_adjustments),
      show_col_types = FALSE
    )
    
    # get list of all days in bed capacity data
    unique_days <- unique(bed_cap$SERVICE_DATE)
    
    # get list of all epic IDs in projections
    unique_id <- unique(scenario_capacity$EPIC_DEPT_ID)
    
    # create scenario bed capacity for full scenario dataset
    scenario_capacity <- expand_grid(
      SERVICE_DATE = unique_days,
      scenario_capacity
    ) %>%
      mutate(DATASET = "SCENARIO")
    
    bed_cap <- bed_cap %>%
      filter(!(EPIC_DEPT_ID %in% unique_id & DATASET == "SCENARIO")) %>%
      bind_rows(scenario_capacity)
  }
  
  # First aggregate to UNIT level
  bed_cap_unit <- bed_cap %>%
    group_by(SERVICE_DATE, LOC_NAME, SERVICE_GROUP, EXTERNAL_NAME, DATASET) %>%
    summarise(
      BED_CAPACITY = sum(BED_CAPACITY, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      BED_CAPACITY = if_else(
        EXTERNAL_NAME == "MSH KP2 L&D",
        20,
        BED_CAPACITY
      )
    )
  
  # return based on requested level
  if (level == "EXTERNAL_NAME") {
    
    bed_cap <- bed_cap_unit %>%
      pivot_wider(
        id_cols = c("LOC_NAME", "SERVICE_GROUP", "EXTERNAL_NAME", "SERVICE_DATE"),
        names_from = DATASET,
        values_from = BED_CAPACITY
      )
    
  } else if (level == "SERVICE_GROUP") {
    
    bed_cap <- bed_cap_unit %>%
      group_by(SERVICE_DATE, LOC_NAME, SERVICE_GROUP, DATASET) %>%
      summarise(
        BED_CAPACITY = sum(BED_CAPACITY, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_wider(
        id_cols = c("LOC_NAME", "SERVICE_GROUP", "SERVICE_DATE"),
        names_from = DATASET,
        values_from = BED_CAPACITY
      )
  }
  

  return(bed_cap)
}