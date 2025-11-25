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
source("functions/location_swap.R")
source("functions/emergency_exclusion.R")
source("functions/los_adjustment.R")
source("functions/unit_capacity.R")
source("functions/excel_add_to_wb.R")
source("functions/save_parameters.R")

# execute ip utiliziation script
source("model/model-ip-utilization.R")

# ---------------------------------------------------------- Scenario Parameters ----------------------------------------------------------

# set hospitals in scenario
hospitals <- list("MSH", "MSM")

services <- list("CARDIOVASCULAR SURGERY", 
                 "VASCULAR SURGERY")

# how the rerouted service group should be distributed at destination hospital
reroute_service_group_percent <- list(
 c("Med Surg" = 0.80,
   "Critical Care" = 0.20),
 c("Heart" = 0.65,
   "Critical Care" = 0.35))


# file with unit capacity adjustments
unit_capacity_adjustments <- "tisch_cancer_center.csv"

# file with volume projections
vol_projections_file <- "2026_budget_volume.csv"

# file with los adjustments
los_projections_file <- "los_adjustments_2025Q4.csv"

# emergency exclusions
exclusion_hosp1 <- FALSE
exclusion_hosp2 <- FALSE

# percentage of service line moving from hospital n
percentage_to_hosp1_list <- c(0.4, 0.9)
percentage_to_hosp2_list <- c(0.9,0.4)



# calculate # of weekdays and # of all days in dataset
num_days <- as.numeric(difftime(max(baseline$SERVICE_DATE),
                                min(baseline$SERVICE_DATE), 
                                units = "days")) + 1
weekdays <- seq(min(baseline$SERVICE_DATE), max(baseline$SERVICE_DATE), by = "day")
num_weekdays <- sum(!wday(weekdays) %in% c(1, 7))



# run code for IP_Utilization
utilizations <- list()

# -------------------------------------------------------- Execute model --------------------------------------------------------
for (i in 1:length(percentage_to_hosp1_list)) {
  print(paste0("Running scenario ", i, "/", length(percentage_to_hosp1_list)))
  
  # specify num of simulations
  n_simulations = 2

  # define global percentage_to_hosp1 and percentage_to_hosp2 here
  percentage_to_hosp1 <- ifelse(is.null(percentage_to_hosp1_list), 1, percentage_to_hosp1_list[i])
  percentage_to_hosp2 <- ifelse(is.null(percentage_to_hosp2_list), 1, percentage_to_hosp2_list[i])
  
  # adjust n_simulations according to percentages
  if (percentage_to_hosp1 %in% c(0, 1) & percentage_to_hosp2 %in% c(0, 1)) {
    n_simulations <- 1
  }
  
  results <- ip_utilization_model (
    generator = "location_swap",
    n_simulations = n_simulations
  )
  
  # Unpack values from IP result list
  ip_utilization_output = results$ip_utilization_output
  ip_comparison_total = results$ip_comparison_total
  ip_comparison_monthly = results$ip_comparison_monthly
  ip_comparison_daily = results$ip_comparison_daily
  
  # save utilizations outputs in list to loop through for workbook saving
    list_name <- paste0(hospitals[[1]], percentage_to_hosp2_list[i] * 100, " - ",
                        hospitals[[2]], percentage_to_hosp1_list[i] * 100)
    utilizations[[list_name]] <- ip_utilization_output
  
  # visualization script
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


# -------------------------------------------------------- Save Workbook ----------------------------------------------------------------
# create excel workbook for model outputs
wb <- createWorkbook()

# save parameters and unit capacity changes as necessary
save_parameters(generator = "location_swap")

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

