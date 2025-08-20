# excel wb function
add_to_wb <- function(df, sheetname, add_filter = FALSE) {
  # create sheet name
  sheet <- addWorksheet(wb, sheetname)
  
  setColWidths(wb, sheetname, cols = 1:ncol(df), widths = "auto")
  
  # write data to sheet
  writeData(wb,
            x = df,
            sheet = sheetname,
            startRow = 4,
            startCol = 1,
            colNames = FALSE,
            keepNA = FALSE,
            withFilter = add_filter)
  
  # add headers
  # Hospital row ---------------------------------------------------------------
  for (i in 1:length(hospitals)) {
    writeData(wb,
              sheetname,
              x = hospitals[[i]],
              startRow = 1,
              startCol = 2 + ((i-1)*9))
    
    mergeCells(wb, sheetname, rows = 1,
               cols = (2 + ((i-1)*9)):(1 + ((i)*9)))
  }
  
  # bed capacity ---------------------------------------------------------------
  for (i in 1:length(hospitals)) {
    writeData(wb, sheetname, x = "Bed Capacity", startRow = 2, startCol = 2 + ((i-1)*9))
    mergeCells(wb, sheetname, rows = 2:3, cols = 2 + ((i-1)*9))
  }
  
  # metrics row ----------------------------------------------------------------
  for (i in 1:length(hospitals)) {
    writeData(wb, sheetname, x = "Daily Bed Demand", startRow = 2, startCol = 3 + ((i-1)*9))
    mergeCells(wb, sheetname, rows = 2, cols = (3 + ((i-1)*9)):(3 + ((i-1)*9) + 1))
    
    writeData(wb, sheetname, x = "Avg. Bed Utilization", startRow = 2, startCol = 5 + ((i-1)*9))
    mergeCells(wb, sheetname, rows = 2, cols = (5 + ((i-1)*9)):(5 + ((i-1)*9) + 1))
    
    writeData(wb, sheetname, x = "Days over 85%", startRow = 2, startCol = 7 + ((i-1)*9))
    mergeCells(wb, sheetname, rows = 2, cols = (7 + ((i-1)*9)):(7 + ((i-1)*9) + 1))
    
    writeData(wb, sheetname, x = "Days over 95%", startRow = 2, startCol = 9 + ((i-1)*9))
    mergeCells(wb, sheetname, rows = 2, cols = (9 + ((i-1)*9)):(9 + ((i-1)*9) + 1))
  }
  
  # baseline scenario ----------------------------------------------------------
  subheader <- rep(c("Baseline", "Scenario"), 4)
  for (i in 1:length(hospitals)) {
    writeData(wb, sheetname, x = matrix(subheader, nrow = 1, byrow = TRUE), startRow = 3, startCol = 3 + ((i-1)*9), colNames = FALSE)
  }
  
  # unit type ------------------------------------------------------------------
  writeData(wb, sheetname, x = "Unit Type", startRow = 3, startCol = 1)
  
  #create cell styles
  header_style <- createStyle(fgFill = "grey", halign = "center", textDecoration = "bold", fontColour = "#010101")
  body_style <- createStyle(border = "TopBottomLeftRight")
  numeric_style <- createStyle(numFmt = "0.00")
  percent_style <- createStyle(numFmt = "0%")
  
  # apply numeric formatting
  for (i in 1:length(hospitals)) {
    addStyle(wb, sheetname, numeric_style, rows = 4:(nrow(df) + 4), cols = (2 + ((i-1)*9)):(4 + ((i-1)*9)), gridExpand = TRUE, stack = TRUE)
  }
  
  # apply percentage formatting
  for (i in 1:length(hospitals)) {
    addStyle(wb, sheetname, percent_style, rows = 4:(nrow(df) + 4), cols = (5 + ((i-1)*9)):(10 + ((i-1)*9)), gridExpand = TRUE, stack = TRUE)
  }
  
  # apply cell styles
  addStyle(wb, sheetname, header_style, rows = 1:3, 
           cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheetname, body_style, rows = 1:(nrow(df) + 3),
           cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  
  freezePane(wb, sheetname, firstCol = TRUE)
  
}
