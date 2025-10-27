add_to_wb <- function(df, sheetname) {
  
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
  mergeCells(wb, sheetname, rows = 1, cols = 3:7)
  
  # Scenario title
  writeData(wb,
            sheetname,
            x = "Projections",
            startRow = 1,
            startCol = 7)
  mergeCells(wb, sheetname, rows = 1, cols = 8:12)

  # Metric row -----------------------------------------------------------------
  # Hospital and Unit Type headers
  writeData(wb, sheetname, 
            x = t(as.data.frame(c("Hospital", "Unit Type"))), 
            startRow = 2, 
            startCol = 1,
            colNames = FALSE)
  # Baseline and scenario metrics
  metrics <- c("Avg. Bed Capacity", "Avg.Daily Demand", "Avg. Bed Utilization", "Avg. Weekday Utilization", "Days over 85%")
  writeData(wb,
            sheetname,
            x = t(as.data.frame(rep(metrics, 2))),
            startRow = 2,
            startCol = 3,
            colNames = FALSE)
  for (i in 1:ncol(df)) {
    mergeCells(wb, sheetname, rows = 2:3, cols = i)
  }
  mergeCells(wb, sheetname, rows = 1, cols = 1:2)
  
  # Styles ---------------------------------------------------------------------
  # Create styles
  hosp_style <- createStyle(fgFill = "#221F72", halign = "left", textDecoration = "bold", fontColour = "#FFFFFF")
  baseline_style <- createStyle(fgFill = "#bcdfeb", halign = "center", textDecoration = "bold", fontColour = "#010101", wrapText = TRUE)
  scenario_style <- createStyle(fgFill = "#ffcad2", halign = "center", textDecoration = "bold", fontColour = "#010101", wrapText = TRUE)
  body_style <- createStyle(border = "TopBottomLeftRight")
  numeric_style <- createStyle(numFmt = "0.00")
  percent_style <- createStyle(numFmt = "0%")
  
  # Apply data styles
  addStyle(wb, sheetname, numeric_style, rows = 4:(nrow(df) + 3), cols = c(3:4, 8:9), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, percent_style, rows = 4:(nrow(df) + 3), cols = c(5:7, 10:12), gridExpand = TRUE, stack = TRUE)
  
  # Apply header styles
  addStyle(wb, sheetname, hosp_style, rows = 2:3, cols = 1:2, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, baseline_style, rows = 1:3, cols = 3:7, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, scenario_style, rows = 1:3, cols = 8:12, gridExpand = TRUE, stack = TRUE)
  
  # Apply body borders
  addStyle(wb, sheetname, body_style, rows = 1:(nrow(df) + 3), cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  
  # auto column widths
  setColWidths(wb, sheetname, cols = 1:ncol(df), widths = "auto")
  
}
