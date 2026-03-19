add_to_wb_dow <- function(df, sheetname) {
  
  # ---- helper: Excel sheetname max length = 31 ----
  safe_sheetname <- function(x) {
    x <- gsub("[\\[\\]\\*\\?\\:/\\\\]", "-", x)  # replace invalid chars
    if (nchar(x) > 31) substr(x, 1, 31) else x
  }
  
  sheetname <- safe_sheetname(sheetname)
  
  # ---- Decide if this is UNIT-level or SERVICE_GROUP-level (based on EXTERNAL_NAME) ----
  has_unit <- "EXTERNAL_NAME" %in% names(df)
  
  id_cols <- if (has_unit) {
    c("LOC_NAME", "SERVICE_GROUP", "EXTERNAL_NAME")
  } else {
    c("LOC_NAME", "SERVICE_GROUP")
  }
  
  dow_order <- c("MONDAY","TUESDAY","WEDNESDAY","THURSDAY","FRIDAY","SATURDAY","SUNDAY")
  
  # ---- Metrics we expect from the DOW generators ----
  base_metrics <- c(
    "AVG_BED_CAPACITY",
    "AVG_DAILY_DEMAND",
    "AVG_BED_UTILIZATION",
    "UTILIZATION_SD",
    "WEEKDAY_AVG_UTILIZATION",
    "WEEKEND_AVG_UTILIZATION",
    "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE",
    "OVERALL_MIN_UTILIZATION",
    "OVERALL_MAX_UTILIZATION",
    "DOW_MIN",
    "DOW_MAX",
    "DOW_DIFF",
    dow_order
  )
  
  # Build desired columns in the exact order we want to show
  desired_cols <- c(
    id_cols,
    paste0(base_metrics, "_BASELINE"),
    paste0(base_metrics, "_SCENARIO")
  )
  
  present <- desired_cols[desired_cols %in% names(df)]
  df_out  <- df %>% dplyr::select(dplyr::all_of(present))
  
  # ---- Create sheet ----
  addWorksheet(wb_dow, sheetname)
  
  # ---- Write data (no column names) ----
  writeData(
    wb_dow,
    sheet = sheetname,
    x = df_out,
    startRow = 4,
    startCol = 1,
    colNames = FALSE,
    keepNA = FALSE
  )
  
  # -----------------------------
  # Header construction
  # -----------------------------
  # Row 1 titles
  baseline_title <- paste0(
    "Baseline (",
    format(min(baseline$SERVICE_DATE), "%B%Y"), " - ",
    format(max(baseline$SERVICE_DATE), "%B%Y"),
    ")"
  )
  scenario_title <- "Projections"
  
  # where each section starts
  n_id <- length(id_cols)
  baseline_start <- n_id + 1
  baseline_end   <- baseline_start + length(base_metrics) - 1
  scenario_start <- baseline_end + 1
  scenario_end   <- scenario_start + length(base_metrics) - 1
  
  writeData(wb_dow, sheetname, x = baseline_title, startRow = 1, startCol = baseline_start, colNames = FALSE)
  mergeCells(wb_dow, sheetname, rows = 1, cols = baseline_start:baseline_end)
  
  writeData(wb_dow, sheetname, x = scenario_title, startRow = 1, startCol = scenario_start, colNames = FALSE)
  mergeCells(wb_dow, sheetname, rows = 1, cols = scenario_start:scenario_end)
  
  # Row 2–3: ID headers + metrics headers
  # ID headers
  id_header <- if (has_unit) c("Hospital", "Unit Type", "Unit") else c("Hospital", "Unit Type")
  writeData(wb_dow, sheetname, x = t(as.data.frame(id_header)), startRow = 2, startCol = 1, colNames = FALSE)
  
  # Metric labels (same list repeated for baseline + scenario)
  metrics_labels <- c(
    "Avg. Bed Capacity",
    "Avg. Daily Demand",
    "Avg. Bed Utilization",
    "Utilization SD",
    "Avg. Weekday Utilization",
    "Avg. Weekend Utilization",
    "Weekend-Weekday Diff",
    "Overall Min",
    "Overall Max",
    "DOW Min",
    "DOW Max",
    "DOW Diff",
    dow_order
  )
  
  writeData(
    wb_dow,
    sheetname,
    x = t(as.data.frame(rep(metrics_labels, 2))),
    startRow = 2,
    startCol = baseline_start,
    colNames = FALSE
  )
  
  # Merge each header cell over rows 2–3 for every column in the header area
  for (i in 1:(n_id + length(base_metrics) * 2)) {
    mergeCells(wb_dow, sheetname, rows = 2:3, cols = i)
  }
  # Merge the top-left title area
  mergeCells(wb_dow, sheetname, rows = 1, cols = 1:n_id)
  
  # -----------------------------
  # Styles (match add_to_wb theme)
  # -----------------------------
  hosp_style     <- createStyle(fgFill = "#221F72", halign = "left", textDecoration = "bold", fontColour = "#FFFFFF")
  baseline_style <- createStyle(fgFill = "#bcdfeb", halign = "center", textDecoration = "bold", fontColour = "#010101", wrapText = FALSE)
  scenario_style <- createStyle(fgFill = "#ffcad2", halign = "center", textDecoration = "bold", fontColour = "#010101", wrapText = FALSE)
  
  body_style       <- createStyle(border = "TopBottomLeftRight")
  numeric_style    <- createStyle(numFmt = "0.00")
  percent_style    <- createStyle(numFmt = "0.00%") 
  sd_percent_style <- createStyle(numFmt = "0.00%")
  dow_text_style   <- createStyle(halign = "center", wrapText = FALSE)
  
  # Apply header styles
  addStyle(wb_dow, sheetname, hosp_style,     rows = 2:3, cols = 1:n_id, gridExpand = TRUE, stack = TRUE)
  addStyle(wb_dow, sheetname, baseline_style, rows = 1:3, cols = baseline_start:baseline_end, gridExpand = TRUE, stack = TRUE)
  addStyle(wb_dow, sheetname, scenario_style, rows = 1:3, cols = scenario_start:scenario_end, gridExpand = TRUE, stack = TRUE)
  
  # Apply body borders to the area we wrote (headers + data)
  addStyle(
    wb_dow,
    sheetname,
    body_style,
    rows = 1:(nrow(df_out) + 3),
    cols = 1:ncol(df_out),
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # -----------------------------
  # Apply number formats by column name (robust to column order)
  # -----------------------------
  get_col_idx <- function(nms) which(names(df_out) %in% nms)
  
  # numeric columns (capacity + demand)
  num_cols <- c(
    "AVG_BED_CAPACITY_BASELINE", "AVG_DAILY_DEMAND_BASELINE",
    "AVG_BED_CAPACITY_SCENARIO", "AVG_DAILY_DEMAND_SCENARIO"
  )
  
  # percent-like numeric columns (already 0–1 proportions in your generator output)
  pct_cols <- c(
    paste0(c("AVG_BED_UTILIZATION","WEEKDAY_AVG_UTILIZATION","WEEKEND_AVG_UTILIZATION",
             "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE","OVERALL_MIN_UTILIZATION","OVERALL_MAX_UTILIZATION",
             "DOW_DIFF", dow_order), "_BASELINE"),
    paste0(c("AVG_BED_UTILIZATION","WEEKDAY_AVG_UTILIZATION","WEEKEND_AVG_UTILIZATION",
             "WEEKEND_TO_WEEKDAY_AVG_DIFFERENCE","OVERALL_MIN_UTILIZATION","OVERALL_MAX_UTILIZATION",
             "DOW_DIFF", dow_order), "_SCENARIO")
  )
  
  sd_cols <- c("UTILIZATION_SD_BASELINE", "UTILIZATION_SD_SCENARIO")
  
  dow_text_cols <- c("DOW_MIN_BASELINE","DOW_MAX_BASELINE","DOW_MIN_SCENARIO","DOW_MAX_SCENARIO")
  
  data_rows <- 4:(nrow(df_out) + 3)
  
  if (length(get_col_idx(num_cols)) > 0) {
    addStyle(wb_dow, sheetname, numeric_style, rows = data_rows, cols = get_col_idx(num_cols), gridExpand = TRUE, stack = TRUE)
  }
  if (length(get_col_idx(pct_cols)) > 0) {
    addStyle(wb_dow, sheetname, percent_style, rows = data_rows, cols = get_col_idx(pct_cols), gridExpand = TRUE, stack = TRUE)
  }
  if (length(get_col_idx(sd_cols)) > 0) {
    addStyle(wb_dow, sheetname, sd_percent_style, rows = data_rows, cols = get_col_idx(sd_cols), gridExpand = TRUE, stack = TRUE)
  }
  if (length(get_col_idx(dow_text_cols)) > 0) {
    addStyle(wb_dow, sheetname, dow_text_style, rows = data_rows, cols = get_col_idx(dow_text_cols), gridExpand = TRUE, stack = TRUE)
  }
  
  # auto widths + make DOW min/max columns readable
  setColWidths(wb_dow, sheetname, cols = 1:ncol(df_out), widths = "auto")
  if (length(get_col_idx(dow_text_cols)) > 0) {
    setColWidths(wb_dow, sheetname, cols = get_col_idx(dow_text_cols), widths = 24)
  }
  # then force DOW columns to a readable width (simple + consistent)
  dow_cols <- get_col_idx(c(paste0(dow_order, "_BASELINE"), paste0(dow_order, "_SCENARIO")))
  if (length(dow_cols) > 0) {
    setColWidths(wb_dow, sheetname, cols = dow_cols, widths = "11")  
  }
}
