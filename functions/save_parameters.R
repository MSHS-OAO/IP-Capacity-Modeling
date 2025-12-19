save_parameters <- function(generator = "") {
  
  # check if sim was a location swap
  if (generator == "location_swap") {
    parameters <- data.frame(
      "Hospital" = c(hospitals[[2]],
                     hospitals[[1]]),
      "Service Line" = c(services[[1]],
                         services[[2]]),
      "Emergency Exclusion" = c(exclusion_hosp2,
                                exclusion_hosp1), check.names = FALSE)
    parameters$`Routing Logic` <- reroute_service_group_percent
    
    # sheetname
    sheetname <- "Parameters"
    # create sheet name
    sheet <- addWorksheet(wb, sheetname)
    # write data to sheet
    writeData(wb, x = parameters, sheet = sheetname)
    # add data
    setColWidths(wb, sheetname, cols = 1:ncol(parameters), widths = "auto")
  }
  
  # check if sim had unit capacity changes
  if (!is.null(unit_capacity_adjustments)) {
    
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
      group_by(LOC_NAME, EXTERNAL_NAME, EPIC_DEPT_ID, SERVICE_GROUP, SERVICE_DATE, DATASET) %>%
      summarise(BED_CAPACITY = sum(BED_CAPACITY, na.rm = TRUE)) %>%
      pivot_wider(id_cols = c(LOC_NAME, EXTERNAL_NAME, EPIC_DEPT_ID, SERVICE_GROUP, SERVICE_DATE),
                  names_from = DATASET,
                  values_from = BED_CAPACITY)
    
    sheetname = "Bed Capacity Projections"
    # create sheet name
    sheet <- addWorksheet(wb, sheetname)
    # write data to sheet
    writeData(wb, x = bed_cap, sheet = sheetname)
    # add data
    setColWidths(wb, sheetname, cols = 1:ncol(bed_cap), widths = "auto")
    
  }
  
  if (!is.null(vol_projections_file)) {
    vol_projections <- read_csv(paste0(cap_dir, "Mapping Info/volume projections/", vol_projections_file),
                                show_col_types = FALSE) %>%
      rename(SERVICE_LINE = VERITY_REPORT_SERVICE_MSX,
             PERCENT_CHANGE = PERCENT) %>%
      select(HOSPITAL, SERVICE_LINE, PERCENT_CHANGE)
    
    # sheetname
    sheetname <- "Volume Projections"
    # create sheet name
    sheet <- addWorksheet(wb, sheetname)
    # write data to sheet
    writeData(wb, x = vol_projections, sheet = sheetname)
    # add data
    setColWidths(wb, sheetname, cols = 1:ncol(vol_projections), widths = "auto")
  }
  
  if (!is.null(los_projections_file)) {
    # read in los projections
    los_projections <- read_csv(paste0(cap_dir, "Mapping Info/los adjustments/", los_projections_file),
                                show_col_types = FALSE) %>%
      # remove combos with no addressable days or addressable days = 0
      filter(!is.na(PERCENT_ADDRESSABLE),   
             PERCENT_ADDRESSABLE != 0) %>%
      rename(HOSPITAL = Hospital) %>%
      select(HOSPITAL, EXTERNAL_NAME, VERITY_REPORT_SERVICE_MSX, PERCENT_ADDRESSABLE, TARGET_LOS) %>%
      arrange(HOSPITAL, EXTERNAL_NAME, VERITY_REPORT_SERVICE_MSX)
      
    # sheetname
    sheetname <- "LOS Projections"
    # create sheet name
    sheet <- addWorksheet(wb, sheetname)
    # write data to sheet
    writeData(wb, x = los_projections, sheet = sheetname)
    # add data
    setColWidths(wb, sheetname, cols = 1:ncol(los_projections), widths = "auto")
  }
  
}