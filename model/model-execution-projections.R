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

# -------------------------------------------------------- Functions & Constants --------------------------------------------------------

# OAO_PRODUCTION DB connection
con_prod <- dbConnect(odbc(), "OAO Cloud DB Production")

# capacity modeling path
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# Load Baseline Data
baseline <- tbl(con_prod, "IPCAP_BEDCHARGES") %>% collect() %>%
  mutate(
    SERVICE_DATE = as.Date(SERVICE_DATE, format = "%Y%m%d"),
    SERVICE_MONTH = lubridate::floor_date(SERVICE_DATE, "month"),
    FACILITY_MSX = case_when(
      FACILITY_MSX == 'STL' ~ 'MSM',
      FACILITY_MSX == 'RVT' ~ 'MSW',
      FACILITY_MSX == 'BIB' ~ 'MSB',
      FACILITY_MSX == 'BIP' ~ 'MSBI',
      TRUE ~ FACILITY_MSX
    )
  )

#  ---------------------------------------------------------------- Render Models ----------------------------------------------------------------

# load all functions
source("functions/los_adjustment.R")
source("functions/unit_capacity.R")
source("functions/excel_add_to_wb.R")
source("functions/save_parameters.R")

# execute ip utiliziation script
source("model/model-ip-utilization.R")

# ---------------------------------------------------------- Scenario Parameters ----------------------------------------------------------

# file with unit capacity adjustments
unit_capacity_adjustments <- "tisch_cancer_center.csv"

# file with volume projections
vol_projections_file <- "2026_budget_volume.csv"

# file with los adjustments
los_projections_file <- "los_adjustments_2025Q4.csv"

# calculate # of weekdays and # of all days in dataset
num_days <- as.numeric(difftime(max(baseline$SERVICE_DATE),
                                min(baseline$SERVICE_DATE), 
                                units = "days")) + 1
weekdays <- seq(min(baseline$SERVICE_DATE), max(baseline$SERVICE_DATE), by = "day")
num_weekdays <- sum(!wday(weekdays) %in% c(1, 7))

# run code for IP_Utilization
utilizations <- list()

# -------------------------------------------------------- Execute model --------------------------------------------------------
results <- ip_utilization_model()

# Unpack values from IP result list
ip_utilization_output = results$ip_utilization_output
ip_comparison_total = results$ip_comparison_total
ip_comparison_monthly = results$ip_comparison_monthly
ip_comparison_daily = results$ip_comparison_daily

utilizations[["MSHS IP Utilization"]] <- ip_utilization_output

render(input = "model/model-visualizations.Rmd",
       output_file = paste0(cap_dir, "Model Outputs/Visualizations/",
                            "MSHS_IP_Utilization_", Sys.Date(), ".html"))

# -------------------------------------------------------- Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# save parameters and unit capacity changes as necessary
save_parameters()

add_to_wb(df = utilizations[["MSHS IP Utilization"]],
          sheetname = "MSHS IP Utilization")

saveWorkbook(wb,
             file = paste0(cap_dir, "Model Outputs/Workbooks/",
                           "MSHS_IP Utilization_", Sys.Date(), ".xlsx"),
             overwrite = TRUE)
