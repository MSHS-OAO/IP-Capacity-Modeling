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

# list for all utilizations
utilizations <- list()
# load excel workbook function
source("functions/excel_add_to_wb.R")

# Sinai color scheme
mshs_colors <- c("#221F72", "#00AEFF", "#D80B8C", "#7F7F7F", "#000000", 
                 "#800080", "#FFFF00", "#CC0000", "#38761D", "#F39C12")

# Scenario Parameters ----------------------------------------------------------
num_days <- as.numeric(difftime(as.Date("2024-12-31"), as.Date("2024-06-01"), units = "days")) + 1

# set hospitals in scenario
hospitals <- list(
  "MSH",
  "MSM"
)

# set of services to be swapped in scenario
services <- list(
  "CARDIOVASCULAR SURGERY",
  "VASCULAR SURGERY"
)

# how the rerouted service group should be distributed at destination hospital
reroute_service_group_percent <- list(
  c("Med Surg" = 0.80,
    "Critical Care" = 0.20),
  c("Heart" = 0.65,
    "Critical Care" = 0.35)
)

# emergency exclusions
exclusion_hosp1 <- TRUE
exclusion_hosp2 <- FALSE

# percentage of service line moving from hospital n
percentage_to_hosp1_list <- c(1, 1, 1, 1, 0, 0, 0, 0)
percentage_to_hosp2_list <- c(1, .9, .8, .7, 1, .9, .8, .7)

# specify num of simulations
n_simulations = 5

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

# Render Models ----------------------------------------------------------------

#execute script for scenario generator and exclusion criteria
source("functions/emergency_exclusion.R")
source("functions/location_swap.R")

# execute ip utilziation script
source("model/model-ip-utilization.R")

# run code for IP_Utilization
utilizations <- list()

for (i in seq_along(percentage_to_hosp1_list)) {
  print(paste0("Running scenario ", i, "/", length(percentage_to_hosp1_list)))
  percentage_to_hosp1 <- percentage_to_hosp1_list[i]
  percentage_to_hosp2 <- percentage_to_hosp2_list[i]
  
  results <- ip_utilization_model (
    generator = location_swap,
    n_simulations = n_simulations,
    hospitals = hospitals, 
    services = services, 
    percentage_to_hosp1 = percentage_to_hosp1,
    percentage_to_hosp2 = percentage_to_hosp2
  )
  
  # Unpack values from IP result list
  ip_utilization_output = results$ip_utilization_output
  ip_comparison_total = results$ip_comparison_total
  ip_comparison_daily = results$ip_comparison_daily
  ip_comparison_monthly = results$ip_comparison_monthly
  
  # save utilizations outputs in list to loop through for workbook saving
  list_name <- paste0(hospitals[[1]], percentage_to_hosp2 * 100, " - ",
                      hospitals[[2]], percentage_to_hosp1 * 100)
  utilizations[[list_name]] <- ip_utilization_output
  
  # visualization script
  html_output_path <- paste0(cap_dir, "Model Outputs/Visualizations/",
                              "model-visualizations-", 
                              hospitals[[1]], services[[1]], "-",
                              hospitals[[2]], services[[2]], "_",
                              percentage_to_hosp1 * 100, "-",
                              percentage_to_hosp2 * 100, "_",
                              Sys.Date(), ".html")
  render(input = "model/model-visualizations.Rmd",
         output_file = html_output_path)
}

# Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# create a sheet defining the scenario parameters
parameters <- data.frame(
  "Hospital" = c(hospitals[[2]],
                 hospitals[[1]]),
  "Service Line" = c(services[[1]],
                     services[[2]]),
  "Emergency Exclusion" = c(exclusion_hosp2,
                            exclusion_hosp1),
  check.names = FALSE)
parameters$`Routing Logic` <- reroute_service_group_percent

# create sheet name
sheet <- addWorksheet(wb, "Parameters")

setColWidths(wb, "Parameters", cols = 1:ncol(parameters), widths = "auto")

# write data to sheet
writeData(wb, x = parameters, sheet = "Parameters")

# create a sheet for each percentage pair
for (i in 1:length(utilizations)) {
  add_to_wb(df = utilizations[[i]],
            sheetname = names(utilizations[i]))
}

saveWorkbook(wb, 
             file = paste0(cap_dir, "Model Outputs/Workbooks/",
                           hospitals[[1]], services[[1]], "-",
                           hospitals[[2]], services[[2]],"_",
                           Sys.Date(), ".xlsx"),
             overwrite = TRUE )

rm(ip_utilization_output,ip_comparison_total,ip_comparison_daily, ip_comparison_monthly,
   lab_rad_output,lab_rad_baseline_comp,lab_rad_demand,results,lab_results)