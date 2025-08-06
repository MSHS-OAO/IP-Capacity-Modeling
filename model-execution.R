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
source("excel_add_to_wb.R")

# Sinai color scheme
mshs_colors <- c("#221F72", "#00AEFF", "#D80B8C", "#7F7F7F", "#000000", 
                 "#800080", "#FFFF00", "#CC0000", "#38761D", "#F39C12")

# set knitr options
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, 
                      message = FALSE,
                      fig.width = 17, fig.height = 10)

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

# percentage of service line moving from hospital n
percentage_to_hosp1 = 1
percentage_to_hosp2 = 1

# rerouting logic for unit type demand 
reroute_hosp <- list(
  "MSH", 
  "MSM"
  )

# service group(s) from hospital i that need to be rerouted
reroute_service_group <- list(
  "Heart",
  c("Critical Care", "Med Surg")
  )

# how the rerouted service group should be distributed at destination hospital
reroute_service_group_percent <- list(
  c("Med Surg" = 0.80,
    "Critical Care" = 0.20),
  c("Heart" = 0.33,
    "Critical Care" = 0.23,
    "Medicine" = 0.17,
    "Med Surg" = 0.16)
  )

# Render Models ----------------------------------------------------------------
# # execute lab and radiology script
# render("model-lab-rad.Rmd")

#execute script for scenario generator
render("scenario_generator_location_swap.Rmd")

# execute ip utilziation script
render("model-ip-utilization.Rmd")

# run code for IP_Utilization
results <- ip_utilization_model (
  generator = scenario_generator_location_swap,
  n_simulations = 1,
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

# # run code for lab_rad
# lab_results <- lab_rad_model (
#   generator = scenario_generator_location_swap,
#   n_simulations = 1,
#   hospitals = hospitals, 
#   services = services, 
#   percentage = 0.5
# )
# 
# # unpack lab from lab_results
# lab_rad_baseline_comp = lab_results$lab_rad_baseline_comp
# lab_rad_demand = lab_results$lab_rad_demand
# lab_rad_output = lab_results$lab_rad_output

# # execute visualization script
# render("model-visualizations.Rmd")

# Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

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