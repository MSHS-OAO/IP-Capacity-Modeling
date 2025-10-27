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
cap_dir <- "/SharedDrive/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Capacity Modeling/"

# Scenario Parameters ----------------------------------------------------------
# set hospitals in scenario
hospitals <- list(
  "MSH",
  "MSM"
)
#hospitals <- NULL

# set of services to be swapped in scenario
services <- list(
  "CARDIOVASCULAR SURGERY",
  "VASCULAR SURGERY")
#services <- NULL 

# how the rerouted service group should be distributed at destination hospital
reroute_service_group_percent <- list(
  c("Med Surg" = 0.80,
    "Critical Care" = 0.20),
  c("Heart" = 0.65,
    "Critical Care" = 0.35))
#reroute_service_group_percent <- NULL 

# file with unit capacity adjustments
unit_capacity_adjustments <- "tisch_cancer_center.csv"

# file with volume projections
vol_projections_file <- "2026_budget_volume.csv"

# emergency exclusions
exclusion_hosp1 <- TRUE
exclusion_hosp2 <- TRUE

# percentage of service line moving from hospital n
percentage_to_hosp1_list <- c(0, 1, 1, 1, 1, 0, 0, 0, 0)
percentage_to_hosp2_list <- c(0, 1, .9, .8, .7, 1, .9, .8, .7)
#percentage_to_hosp1_list <- NULL
#percentage_to_hosp2_list <- NULL

# specify num of simulations
n_simulations = 2

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
num_days <- as.numeric(difftime(max(baseline$SERVICE_DATE),
                                min(baseline$SERVICE_DATE), 
                                units = "days")) + 1
weekdays <- seq(min(baseline$SERVICE_DATE), max(baseline$SERVICE_DATE), by = "day")
num_weekdays <- sum(!wday(weekdays) %in% c(1, 7))

# Render Models ----------------------------------------------------------------

# load all functions
source("functions/emergency_exclusion.R")
source("functions/location_swap.R")
source("functions/unit_capacity.R")
source("functions/excel_add_to_wb.R")
source("functions/save_parameters.R")

# execute ip utiliziation script
source("model/model-ip-utilization.R")

# run code for IP_Utilization
utilizations <- list()

# determine if scenario is a location_swap
n <- ifelse(is.null(hospitals), 1, length(hospitals))
p <- ifelse(is.null(percentage_to_hosp1_list), 1, length(percentage_to_hosp1_list))

for (i in 1:p) {
  print(paste0("Running scenario ", i, "/", p))
  
  results <- ip_utilization_model (
    generator = location_swap,
    n_simulations = n_simulations,
    hospitals = hospitals, 
    services = services, 
    percentage_to_hosp1 = ifelse(is.null(percentage_to_hosp1_list), 1, percentage_to_hosp1_list[i]),
    percentage_to_hosp2 = ifelse(is.null(percentage_to_hosp2_list), 1, percentage_to_hosp2_list[i])
  )
  
  # Unpack values from IP result list
  ip_utilization_output = results$ip_utilization_output
  ip_comparison_total = results$ip_comparison_total
  ip_comparison_monthly = results$ip_comparison_monthly
  ip_comparison_daily = results$ip_comparison_daily
  
  # save utilizations outputs in list to loop through for workbook saving
  if (n == 1) {
    utilizations[["MSHS IP Utilization"]] <- ip_utilization_output
  } else {
    list_name <- paste0(hospitals[[1]], percentage_to_hosp2_list[i] * 100, " - ",
                        hospitals[[2]], percentage_to_hosp1_list[i] * 100)
    utilizations[[list_name]] <- ip_utilization_output
  }
  
  # visualization script
  if (n == 1) {
    render(input = "model/model-visualizations.Rmd",
           output_file = paste0(cap_dir, "Model Outputs/Visualizations/",
                                "MSHS_IP_Utilization_", Sys.Date(), ".html"))
  } else {
    html_output_path <- paste0(cap_dir, "Model Outputs/Visualizations/",
                               "model-visualizations-", 
                               hospitals[[1]], services[[1]], "-",
                               hospitals[[2]], services[[2]], "_",
                               percentage_to_hosp1_list[i] * 100, "-",
                               percentage_to_hosp2_list[i] * 100, "_",
                               Sys.Date(), ".html")
    render(input = "model/model-visualizations.Rmd",
           output_file = html_output_path)
  }
  html_output_path <- file.path(cap_dir, "Model Outputs/Workbooks",
                                paste0("model-visualizations-",
                                       hospitals[[1]], services[[1]], "-",
                                       hospitals[[2]], services[[2]], "_",
                                       percentage_to_hosp1 * 100, "-",
                                       percentage_to_hosp2 * 100, "_",
                                       Sys.Date(), ".html"))
  render(input = "model-visualizations.Rmd",
         output_file = html_output_path)
}

# Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# save parameters and unit capacity changes as necessary
save_parameters()

# create a sheet for each percentage pair
for (i in 1:length(utilizations)) {
  add_to_wb(df = utilizations[[i]],
            sheetname = names(utilizations[i]))
}

if (n == 1) {
  saveWorkbook(wb,
               file = paste0(cap_dir, "Model Outputs/Workbooks/",
                             "MSHS_IP Utilization_", Sys.Date(), ".xlsx"),
               overwrite = TRUE)
} else {
  saveWorkbook(wb, 
               file = paste0(cap_dir, "Model Outputs/Workbooks/",
                             hospitals[[1]], services[[1]], "-",
                             hospitals[[2]], services[[2]],"_",
                             Sys.Date(), ".xlsx"),
               overwrite = TRUE )
}
