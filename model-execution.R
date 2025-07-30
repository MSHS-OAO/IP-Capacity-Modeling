library(knitr)
library(tidyverse)
library(odbc)
library(DBI)
library(glue)
library(dplyr)
library(tidyr)
library(dbplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(scales)
library(openxlsx)
library(readxl)
library(rmarkdown)

# Functions & Constants --------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")
# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/SixSigma/Project Based/System/Capacity Modeling/"

# excel wb function
add_to_wb <- function(df, sheetname, add_filter = FALSE) {
  # create sheet name
  sheet <- addWorksheet(wb, sheetname)
  
  setColWidths(wb, sheet, cols = 1:ncol(df), widths = "auto")
  
  # write data to sheet
  writeDataTable(wb,
                 x = df,
                 sheet = sheet,
                 keepNA = FALSE,
                 withFilter = add_filter)
  
  #create cell styles
  header_style <- createStyle(fgFill = "grey", halign = "left", 
                              textDecoration = "bold", fontColour = "#010101")
  body_style <- createStyle(border = "TopBottomLeftRight")
  
  # apply cell styles
  addStyle(wb, sheet, header_style, rows = 1, 
           cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, body_style, rows = 1:(nrow(df) + 1),
           cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  
}

# Sinai color scheme
mshs_colors <- c("#221F72", "#00AEFF", "#D80B8C", "#7F7F7F", "#000000", 
                 "#800080", "#FFFF00", "#CC0000", "#38761D", "#F39C12")

# set knitr options
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, 
                      message = FALSE,
                      fig.width = 17, fig.height = 10)

# Scenario Parameters ----------------------------------------------------------
num_days <- as.numeric(difftime(as.Date("2024-12-31"), as.Date("2024-06-01"), units = "days"))

# set hospitals in scenario
hospitals <- list(
  "MSH",
  "MSM"
)

# set of services to be swapped in scenario
services <- list(
  "CARDIOLOGY",
  "CARDIOVASCULAR SURGERY"
)

# Render Models ----------------------------------------------------------------
# execute lab and radiology script
render("model-lab-rad.Rmd")

#execute script for scenario generator
render("scenario_generator_location_swap.Rmd")

# execute % simulation script
# execute ip utilziation script
render("model-ip-utilization.Rmd")

# run code for IP_Utilization
results <- ip_utilization_model (
  generator = scenario_generator_location_swap,
  n_simulations = 100,
  hospitals = hospitals, 
  services = services, 
  percentage = 0.5
)

# Unpack values from IP result list
ip_utilization_output = results$ip_utilization_output
ip_comparison_total = results$ip_comparison_total
ip_comparison_daily = results$ip_comparison_daily
ip_comparison_monthly = results$ip_comparison_monthly

# run code for lab_rad
lab_results <- lab_rad_model (
  generator = scenario_generator_location_swap,
  n_simulations = 1,
  hospitals = hospitals, 
  services = services, 
  percentage = 0.5
)

# unpack lab from lab_results
lab_rad_baseline_comp = lab_results$lab_rad_baseline_comp
lab_rad_demand = lab_results$lab_rad_demand
lab_rad_output = lab_results$lab_rad_output

# execute visualization script
render("model-visualizations.Rmd")

# Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# add sheet with IP Utilization Output
add_to_wb(df = ip_utilization_output,
          sheetname = "IP Utilization Comparison")
# add sheet with daily IP Stats
add_to_wb(df = ip_comparison_daily,
          sheetname = "IP Stats Daily",
          add_filter = TRUE)
# add sheet with daily Lab & Rad Stats
add_to_wb(df = lab_rad_output,
          sheetname = "LAB & RAD Demand")
# add sheet with avg charges per patient
add_to_wb(df = lab_rad_baseline_comp,
          sheetname = "LAB & RAD Patient Charges",
          add_filter = TRUE)
# add sheet with daily Lab & Rad Stats
add_to_wb(df = lab_rad_demand,
          sheetname = "LAB & RAD Daily",
          add_filter = TRUE)

saveWorkbook(wb, 
             file = paste0(cap_dir, "Model Outputs/Workbooks/OUTPUT_",
                           hospitals[[1]], services[[1]], "-",
                           hospitals[[2]], services[[2]],"_", 
                           Sys.Date(), "_50_percent.xlsx"),
             overwrite = TRUE )


rm(ip_utilization_output,ip_comparison_total,ip_comparison_daily, ip_comparison_monthly,
   lab_rad_output,lab_rad_baseline_comp,lab_rad_demand,results,lab_results)