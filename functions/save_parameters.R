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
    
    scenario_capacity <- read_csv(paste0(cap_dir, "Mapping Info/unit capacity/",
                                         unit_capacity_adjustments), show_col_types = FALSE) %>%
      select(SERVICE_GROUP, EXTERNAL_NAME, BED_CAPACITY) %>%
      rename(SCENARIO_BED_CAPACITY = BED_CAPACITY,
             SCENARIO_UNIT_TYPE = SERVICE_GROUP)
    
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
      group_by(HOSPITAL, SERVICE_GROUP, EXTERNAL_NAME) %>%
      summarise(BASELINE_BED_CAPACITY = mean(`Measure Values`, na.rm = TRUE)) %>%
      filter(EXTERNAL_NAME %in% scenario_capacity$EXTERNAL_NAME) %>%
      rename(UNIT = EXTERNAL_NAME,
             BASELINE_UNIT_TYPE = SERVICE_GROUP) %>%
      full_join(scenario_capacity, by = c("UNIT" = "EXTERNAL_NAME")) %>%
      mutate(BASELINE_BED_CAPACITY = format(round(BASELINE_BED_CAPACITY, 2), nsmall = 2),
             SCENARIO_BED_CAPACITY = format(round(SCENARIO_BED_CAPACITY, 2), nsmall =2)) %>%
      select(HOSPITAL, UNIT, BASELINE_UNIT_TYPE, BASELINE_BED_CAPACITY, SCENARIO_UNIT_TYPE, SCENARIO_BED_CAPACITY)
    
    sheetname = sub("\\..*", "", unit_capacity_adjustments)
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
  
}