unit_capacity <- function(unit_capacity_adjustments = NULL,
                          level = c("SERVICE_GROUP", "EXTERNAL_NAME")) {
  
  level <- match.arg(level)
  
  unique_dates_baseline <- unique(baseline$SERVICE_DATE)
  
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
  
  Sys.setenv(VROOM_CONNECTION_SIZE = 10 * 1024 * 1024)  # 10 MB
  
  # read each CSV and list average bed capacity for each unit monthly
  bed_cap <- read_csv(
    paste0(cap_dir, "Tableau Data/Detail_Full Data_data_2026-10-06.csv"),
    show_col_types = FALSE
  ) %>%
    rename(
      HOSPITAL      = Location,
      SERVICE_GROUP = `Service Group`,
      EXTERNAL_NAME = Unit
    ) %>%
    mutate(SERVICE_DATE = mdy(`Day of Census Day`),
           trimws(EXTERNAL_NAME)) %>%
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
      SERVICE_DATE %in% unique_dates_baseline
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
    group_by(
      SERVICE_DATE,
      LOC_NAME,
      SERVICE_GROUP,
      EXTERNAL_NAME,
      EPIC_DEPT_ID,
      DATASET
    ) %>%
    summarise(BED_CAPACITY = sum(BED_CAPACITY)) %>%
    mutate(BED_CAPACITY = if_else(EXTERNAL_NAME == "MSH KP2 L&D",
                                  20,
                                  BED_CAPACITY))
  
  # if there is a unit capacity adjustment for the sim then process scenario bed cap
  if (!is.null(unit_capacity_adjustments)) {
    # read in file for unit capacity changes to be applied to scenario output
    scenario_capacity <- read_csv(
      paste0(cap_dir, "Mapping Info/unit capacity/", unit_capacity_adjustments),
      show_col_types = FALSE
    )
    
    scenario_capacity <- expand_grid(
      SERVICE_DATE = unique_dates_baseline,
      scenario_capacity
    ) %>%
      mutate(DATASET = "SCENARIO", .before = BED_CAPACITY) %>%
      bind_rows(bed_cap)
    
    # return based on requested level
    if (level == "EXTERNAL_NAME") {
      
      bed_cap <- scenario_capacity %>%
        pivot_wider(
          id_cols = c("LOC_NAME", "SERVICE_GROUP", "EXTERNAL_NAME", "SERVICE_DATE"),
          names_from = DATASET,
          values_from = BED_CAPACITY) %>% 
        replace_na(list(SCENARIO = 0,
                        BASELINE = 0))
      
    } else if (level == "SERVICE_GROUP") {
      
      bed_cap <- scenario_capacity %>%
        group_by(SERVICE_DATE, LOC_NAME, SERVICE_GROUP, DATASET) %>%
        summarise(
          BED_CAPACITY = sum(BED_CAPACITY, na.rm = TRUE),
          .groups = "drop") %>%
        pivot_wider(
          id_cols = c("LOC_NAME", "SERVICE_GROUP", "SERVICE_DATE"),
          names_from = DATASET,
          values_from = BED_CAPACITY) %>% 
        replace_na(list(SCENARIO = 0,
                        BASELINE = 0))
    }
    
  } else {
    
    bed_cap_scenario <- bed_cap %>%
      mutate(DATASET = "SCENARIO")
    
    scenario_capacity <- bind_rows(
      bed_cap %>% mutate(DATASET = "BASELINE"),
      bed_cap_scenario %>% mutate(DATASET = "SCENARIO"))
    
    if (level == "EXTERNAL_NAME") {
      
      bed_cap <- scenario_capacity %>%
        pivot_wider(
          id_cols = c("LOC_NAME", "SERVICE_GROUP", "EXTERNAL_NAME", "SERVICE_DATE"),
          names_from = DATASET,
          values_from = BED_CAPACITY) %>% 
        replace_na(list(SCENARIO = 0,
                        BASELINE = 0))
    } else if (level == "SERVICE_GROUP") {
      
      bed_cap <- scenario_capacity %>%
        group_by(SERVICE_DATE, LOC_NAME, SERVICE_GROUP, DATASET) %>%
        summarise(
          BED_CAPACITY = sum(BED_CAPACITY, na.rm = TRUE),
          .groups = "drop") %>%
        pivot_wider(
          id_cols = c("LOC_NAME", "SERVICE_GROUP", "SERVICE_DATE"),
          names_from = DATASET,
          values_from = BED_CAPACITY) %>% 
        replace_na(list(SCENARIO = 0,
                        BASELINE = 0))
    }
  }
  return(bed_cap)
}
