add_to_wb <- function(df, sheetname) {
  
  
  desired_cols <- c(
    "LOC_NAME", "SERVICE_GROUP",
    
    #BASELINE
    "AVG_BED_CAPACITY_BASELINE",
    "AVG_DAILY_DEMAND_BASELINE",
    "AVG_UTILIZATION_BASELINE",
    "AVG_SD_BASELINE",                    
    "WEEKDAY_AVG_UTILIZATION_BASELINE",
    "AVG_PERCENT_85_BASELINE",
    "DOW_MIN_BASELINE",
    "DOW_MAX_BASELINE",
    "DOW_DIFF_BASELINE",
    
    #SCENARIO
    "AVG_BED_CAPACITY_SCENARIO",
    "AVG_DAILY_DEMAND_SCENARIO",
    "AVG_UTILIZATION_SCENARIO",
    "AVG_SD_SCENARIO",                    
    "WEEKDAY_AVG_UTILIZATION_SCENARIO",
    "AVG_PERCENT_85_SCENARIO",
    "DOW_MIN_SCENARIO",
    "DOW_MAX_SCENARIO",
    "DOW_DIFF_SCENARIO"
  )
  
  
  present <- desired_cols[desired_cols %in% names(df)]
  df <- df %>% select(all_of(present)) 
  
  # create sheet
  addWorksheet(wb, sheetname)
  
  # write data (no column names)
  writeData(wb,
            sheet = sheetname,
            x = df,
            startRow = 4,
            startCol = 1,
            colNames = FALSE,
            keepNA = FALSE)
  
  # Title row ------------------------------------------------------------------
  # Baseline title
  writeData(wb,
            sheetname,
            x = paste0("Baseline (", 
                       format(min(baseline$SERVICE_DATE), "%B%Y"), " - ",
                       format(max(baseline$SERVICE_DATE), "%B%Y"),
                       ")"),
            startRow = 1,
            startCol = 3)
  mergeCells(wb, sheetname, rows = 1, cols = 3:11)
  
  # Scenario title
  writeData(wb,
            sheetname,
            x = "Projections",
            startRow = 1,
            startCol = 12)
  mergeCells(wb, sheetname, rows = 1, cols = 12:20)

  # Metric row -----------------------------------------------------------------
  # Hospital and Unit Type headers
  writeData(wb, sheetname, 
            x = t(as.data.frame(c("Hospital", "Unit Type"))), 
            startRow = 2, 
            startCol = 1,
            colNames = FALSE)
  # Baseline and scenario metrics
  metrics <- c("Avg. Bed Capacity", "Avg.Daily Demand", "Avg. Bed Utilization","Utilization SD", "Avg. Weekday Utilization", "Days over 85%", "DOW Min", "DOW Max", "DOW Diff")
  writeData(wb,
            sheetname,
            x = t(as.data.frame(rep(metrics, 2))),
            startRow = 2,
            startCol = 3,
            colNames = FALSE)
  for (i in 1:(2 + length(metrics) * 2)) {
    mergeCells(wb, sheetname, rows = 2:3, cols = i)
  }
  mergeCells(wb, sheetname, rows = 1, cols = 1:2)
  
  # Styles ---------------------------------------------------------------------
  # Create styles
  hosp_style <- createStyle(fgFill = "#221F72", halign = "left", textDecoration = "bold", fontColour = "#FFFFFF")
  baseline_style <- createStyle(fgFill = "#bcdfeb", halign = "center", textDecoration = "bold", fontColour = "#010101", wrapText = TRUE)
  scenario_style <- createStyle(fgFill = "#ffcad2", halign = "center", textDecoration = "bold", fontColour = "#010101", wrapText = TRUE)
  dow_text_style <- createStyle(halign = "center", wrapText = FALSE)
  body_style <- createStyle(border = "TopBottomLeftRight")
  numeric_style <- createStyle(numFmt = "0.00")
  percent_style <- createStyle(numFmt = "0%")
  sd_percent_style <- createStyle(numFmt = "0.00%")  
  # Apply data styles
  addStyle(wb, sheetname, numeric_style, rows = 4:(nrow(df) + 3), cols = c(3,4, 12,13), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, percent_style, rows = 4:(nrow(df) + 3), cols = c(5,7,8, 11,14,16,17,20), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, sd_percent_style,rows = 4:(nrow(df) + 3),cols = c(6,15),gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, dow_text_style,rows = 4:(nrow(df) + 3),cols = c(9,10, 18,19),gridExpand = TRUE, stack = TRUE)
  # Apply header styles
  addStyle(wb, sheetname, hosp_style, rows = 2:3, cols = 1:2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, baseline_style, rows = 1:3, cols = 3:11, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, scenario_style, rows = 1:3, cols = 12:20, gridExpand = TRUE, stack = TRUE)
  
  # Apply body borders
  addStyle(wb, sheetname, body_style, rows = 1:(nrow(df) + 3), cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  
  # auto column widths
  setColWidths(wb, sheetname, cols = 1:ncol(df), widths = "auto")
  
  # DOW Min/Max are long strings -> keep them reasonable and let wrap handle it
  setColWidths(wb, sheetname, cols = c(9,10,18,19), widths = 24)
  
}
