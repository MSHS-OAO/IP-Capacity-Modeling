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
con_prod <- dbConnect(odbc(), "OAO_PRODUCTION")
# capacity modeling path
cap_dir <- "J:/deans/Presidents/SixSigma/Project Based/System/Capacity Modeling/"

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
# execute ip utilziation script
render("model-ip-utilization.Rmd")

# execute lab and radiology script
render("model-lab-rad.Rmd")

# execute % simulation script
#CANT USE VARIABLE NAMES SAVED IN WORKBOOKS BELOW#
# render()

# execute visualization script


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
                           Sys.Date(), ".xlsx"),
             overwrite = TRUE )
